/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gsimage.c */
/* Image setup procedures for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gspaint.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate.h */
#include "gzcolor.h"			/* requires gxdevice.h */
#include "gzpath.h"
#include "gxcolor.h"
#include "gxcpath.h"
#include "gxdevmem.h"
#include "gximage.h"

/* Exported size of enumerator */
const uint gs_image_enum_sizeof = sizeof(gs_image_enum);

/* Forward declarations */
private void image_init_map(P3(byte *, int, const float *));
private int image_init(P9(gs_image_enum *, int, int, int,
  int, int, gs_matrix *, gs_state *, fixed));
/* Procedures for unpacking the input data into 8 bits/sample. */
extern iunpack_proc(image_unpack_1);
extern iunpack_proc(image_unpack_1_spread);
extern iunpack_proc(image_unpack_2);
extern iunpack_proc(image_unpack_2_spread);
extern iunpack_proc(image_unpack_4);
extern iunpack_proc(image_unpack_8);
extern iunpack_proc(image_unpack_8_spread);
extern iunpack_proc(image_unpack_12);
/* The image_render procedures work on fully expanded, complete rows. */
/* These take a height argument, which is an integer > 0; */
/* they return a negative code, or the number of */
/* rows actually processed (which may be less than the height). */
extern irender_proc(image_render_skip);
extern irender_proc(image_render_simple);
extern irender_proc(image_render_mono);
extern irender_proc(image_render_color);

/* Standard mask tables for spreading input data. */
/* Note that the mask tables depend on the end-orientation of the CPU. */
/* We can't simply define them as byte arrays, because */
/* they might not wind up properly long- or short-aligned. */
#define map4tox(z,a,b,c,d)\
	z, z^a, z^b, z^(a+b),\
	z^c, z^(a+c), z^(b+c), z^(a+b+c),\
	z^d, z^(a+d), z^(b+d), z^(a+b+d),\
	z^(c+d), z^(a+c+d), z^(b+c+d), z^(a+b+c+d)
#if arch_is_big_endian
private const unsigned long map_4x1_to_32[16] =
   {	map4tox(0L, 0xffL, 0xff00L, 0xff0000L, 0xff000000L)	};
private const unsigned long map_4x1_to_32_invert[16] =
   {	map4tox(0xffffffffL, 0xffL, 0xff00L, 0xff0000L, 0xff000000L)	};
#else					/* !arch_is_big_endian */
private const unsigned long map_4x1_to_32[16] =
   {	map4tox(0L, 0xff000000L, 0xff0000L, 0xff00L, 0xffL)	};
private const unsigned long map_4x1_to_32_invert[16] =
   {	map4tox(0xffffffffL, 0xff000000L, 0xff0000L, 0xff00L, 0xffL)	};
#endif

/* Start processing an image */
int
gs_image_init(gs_image_enum *penum, gs_state *pgs,
  int width, int height, int bps,
  int spread, const gs_color_space *pcs, const float *decode /* [spp*2] */,
  gs_matrix *pmat)
{	const gs_color_space_type *pcst = pcs->type;
	int spp = pcst->num_components;
	int ci;
	int device_color = 0;
	static const float default_decode[8] =
		{ 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 };
	if ( pgs->in_cachedevice )
		return_error(gs_error_undefined);
	if ( spp < 0 )			/* Pattern not allowed */
		return_error(gs_error_rangecheck);
	if ( decode == 0 )
		decode = default_decode;
	switch ( pcst->index )
	{
	case gs_color_space_index_DeviceGray:
	case gs_color_space_index_DeviceRGB:
	case gs_color_space_index_DeviceCMYK:
		device_color = 1;
	}
	spread = (spread ? spp : 1);

	if ( spp == 1 )
	{	/* Initialize the color table */
#define chtl(i)\
  penum->dev_colors[i].halftone_level
		switch ( bps )
		   {
		default:
		   {	register gx_device_color *pcht =
			   &penum->dev_colors[0];
			register int n = 64;
			do
			   {	pcht[0].halftone_level =
				  pcht[1].halftone_level =
				  pcht[2].halftone_level =
				  pcht[3].halftone_level = -1;
				pcht += 4;
			   }
			while ( --n > 0 );
			break;
		   }
		case 4:
			chtl(17) = chtl(2*17) = chtl(3*17) =
			  chtl(4*17) = chtl(6*17) = chtl(7*17) =
			  chtl(8*17) = chtl(9*17) = chtl(11*17) =
			  chtl(12*17) = chtl(13*17) = chtl(14*17) = -1;
			/* falls through */
		case 2:
			chtl(5*17) = chtl(10*17) = -1;
		case 1:
			;
		   }
#undef chtl
	}

	/* Initialize the maps from samples to intensities. */

	for ( ci = 0; ci < spp; ci++ )
	{	sample_map *pmap = &penum->map[ci];

		/* If the decoding is [0 1] or [1 0], we can fold it */
		/* into the expansion of the sample values; */
		/* otherwise, we have to use the floating point method. */

		const float *this_decode = &decode[ci * 2];
		const float *map_decode;	/* decoding used to */
				/* construct the expansion map */

		const float *real_decode;	/* decoding for */
				/* expanded samples */

		int no_decode;

		map_decode = real_decode = this_decode;
		if ( map_decode[0] == 0.0 && map_decode[1] == 1.0 )
			no_decode = 1;
		else if ( map_decode[0] == 1.0 && map_decode[1] == 0.0 )
			no_decode = 1,
			real_decode = default_decode;
		else
			no_decode = 0,
			device_color = 0,
			map_decode = default_decode;
		if ( bps > 2 || spread != 1 )
			image_init_map(&pmap->table.lookup8[0], 1 << bps,
				       map_decode);
		else
		{	/* The map index encompasses more than one pixel. */
			byte map[4];
			register int i;
			image_init_map(&map[0], 1 << bps, map_decode);
			switch ( bps )
			{
			case 1:
			{	register ulong *p = &pmap->table.lookup4x1to32[0];
				if ( map[0] == 0 && map[1] == 0xff )
					memcpy((byte *)p, map_4x1_to_32, 16 * 4);
				else if ( map[0] == 0xff && map[1] == 0 )
					memcpy((byte *)p, map_4x1_to_32_invert, 16 * 4);
				else
				  for ( i = 0; i < 16; i++, p++ )
					((byte *)p)[0] = map[i >> 3],
					((byte *)p)[1] = map[(i >> 2) & 1],
					((byte *)p)[2] = map[(i >> 1) & 1],
					((byte *)p)[3] = map[i & 1];
			}	break;
			case 2:
			{	register ushort *p = &pmap->table.lookup2x2to16[0];
				for ( i = 0; i < 16; i++, p++ )
					((byte *)p)[0] = map[i >> 2],
					((byte *)p)[1] = map[i & 3];
			}	break;
			}
		}
		pmap->decode_base /* = decode_lookup[0] */ = real_decode[0];
		pmap->decode_factor = (real_decode[1] - real_decode[0]) / 255.0;
		pmap->decode_max /* = decode_lookup[15] */ = real_decode[1];
		if ( no_decode )
			pmap->decoding = sd_none;
		else if ( bps <= 4 )
		{	static const int steps[] = { 0, 15, 5, 0, 1 };
			int step = steps[bps];
			int i;
			pmap->decoding = sd_lookup;
			for ( i = 15 - step; i > 0; i -= step )
			  pmap->decode_lookup[i] = pmap->decode_base +
			    i * (255.0 / 15) * pmap->decode_factor;
		}
		else
			pmap->decoding = sd_compute;
		if ( spp == 1 )		/* and ci == 0 */
		{	/* Pre-map entries 0 and 255. */
			gs_client_color cc;
			cc.paint.values[0] = real_decode[0];
			(*pcst->remap_color)(&cc, pcs, &penum->icolor0, pgs);
			cc.paint.values[0] = real_decode[1];
			(*pcst->remap_color)(&cc, pcs, &penum->icolor1, pgs);
		}
	}

	penum->masked = 0;
	penum->device_color = device_color;
	return image_init(penum, width, height, bps, spread, spp,
			  pmat, pgs, (fixed)0);
}
/* Construct a mapping table for sample values. */
/* map_size is 2, 4, 16, or 256.  Note that 255 % (map_size - 1) == 0. */
private void
image_init_map(byte *map, int map_size, const float *decode)
{	float min_v = decode[0], max_v = decode[1];
	byte *limit = map + map_size;
	uint value = min_v * 0xffffL;
	/* The division in the next statement is exact, */
	/* see the comment above. */
	uint diff = (max_v - min_v) * (0xffffL / (map_size - 1));
	for ( ; map != limit; map++, value += diff )
		*map = value >> 8;
}

/* Start processing a masked image */
int
gs_imagemask_init(gs_image_enum *penum, gs_state *pgs,
  int width, int height, int invert, gs_matrix *pmat, int adjust)
{	/* Initialize color entries 0 and 255. */
	penum->icolor0.halftone_level = 0;
	penum->icolor0.color1 = penum->icolor0.color2 = gx_no_color_index;
	penum->icolor1 = *pgs->dev_color;
	penum->masked = 1;
	memcpy(&penum->map[0].table.lookup4x1to32[0],
	       (invert ? map_4x1_to_32_invert : map_4x1_to_32),
	       16 * 4);
	penum->map[0].decoding = sd_none;
	return image_init(penum, width, height, 1, 1, 1, pmat, pgs,
			  (adjust && pgs->in_cachedevice ?
			   float2fixed(0.25) : (fixed)0));
}

/* Common setup for image and imagemask. */
/* Caller has set penum->masked, map, dev_colors[]. */
private int
image_init(register gs_image_enum *penum, int width, int height, int bps,
  int spread, int spp, gs_matrix *pmat, gs_state *pgs, fixed adjust)
{	int code;
	int index_bps;
	gs_matrix mat;
	gs_fixed_rect clip_box;
	uint bsize = (width + 8) * spp;	/* round up, +1 for end-of-run byte */
	byte *buffer;
	fixed mtx, mty;
	if ( width < 0 || height < 0 )
		return_error(gs_error_rangecheck);
	switch ( bps )
	   {
	case 1: index_bps = 0; break;
	case 2: index_bps = 1; break;
	case 4: index_bps = 2; break;
	case 8: index_bps = 3; break;
	case 12: index_bps = 4; break;
	default: return_error(gs_error_rangecheck);
	   }
	if ( width == 0 || height == 0 )
		return 1;	/* empty image */
	if (	(code = gs_matrix_invert(pmat, &mat)) < 0 ||
		(code = gs_matrix_multiply(&mat, &ctm_only(pgs), &mat)) < 0
	   )	return code;
	buffer = (byte *)gs_malloc(1, bsize, "image buffer");
	if ( buffer == 0 )
		return_error(gs_error_VMerror);
	penum->width = width;
	penum->height = height;
	penum->bps = bps;
	penum->spp = spp;
	penum->spread = spread;
	penum->fxx = float2fixed(mat.xx);
	penum->fxy = float2fixed(mat.xy);
	penum->fyx = float2fixed(mat.yx);
	penum->fyy = float2fixed(mat.yy);
	penum->skewed = (penum->fxy | penum->fyx) != 0;
	penum->xcur = mtx = float2fixed(mat.tx);
	penum->ycur = mty = float2fixed(mat.ty);
	penum->pgs = pgs;
	clip_box = pgs->clip_path->path.bbox;	/* box is known to be up to date */
	penum->clip_box = clip_box;
	penum->buffer = buffer;
	penum->buffer_size = bsize;
	penum->line = 0;
	penum->line_size = 0;
	penum->bytes_per_row =
		(uint)(((ulong)width * (bps * spp) / spread + 7) >> 3);
	penum->slow_loop = penum->skewed;
	/* If all four extrema of the image fall within the clipping */
	/* rectangle, clipping is never required. */
	   {	gs_fixed_rect cbox;
		fixed edx = float2fixed(mat.xx * width);
		fixed edy = float2fixed(mat.yy * height);
		fixed epx, epy, eqx, eqy;
		if ( edx < 0 ) epx = edx, eqx = 0;
		else epx = 0, eqx = edx;
		if ( edy < 0 ) epy = edy, eqy = 0;
		else epy = 0, eqy = edy;
		if ( penum->skewed )
		   {	edx = float2fixed(mat.yx * height);
			edy = float2fixed(mat.xy * width);
			if ( edx < 0 ) epx += edx; else eqx += edx;
			if ( edy < 0 ) epy += edy; else eqy += edy;
		   }
		gx_cpath_box_for_check(pgs->clip_path, &cbox);
		penum->never_clip =
			mtx + epx >= cbox.p.x && mtx + eqx <= cbox.q.x &&
			mty + epy >= cbox.p.y && mty + eqy <= cbox.q.y;
		if_debug7('b',
			  "[b]Image: cbox=(%g,%g),(%g,%g)\n     mt=(%g,%g) never_clip=%d\n",
			  fixed2float(cbox.p.x), fixed2float(cbox.p.y),
			  fixed2float(cbox.q.x), fixed2float(cbox.q.y),
			  fixed2float(mtx), fixed2float(mty),
			  penum->never_clip);
	   }
	   {	static iunpack_proc((*procs[5])) = {
			image_unpack_1, image_unpack_2,
			image_unpack_4, image_unpack_8, image_unpack_12
		   };
		static iunpack_proc((*spread_procs[5])) = {
			image_unpack_1_spread, image_unpack_2_spread,
			image_unpack_4, image_unpack_8_spread,
			image_unpack_12
		   };
		long dev_width;
		if ( spread != 1 )
		  penum->unpack = spread_procs[index_bps];
		else
		  penum->unpack = procs[index_bps];
		penum->slow_loop |=
			/* Use slow loop for imagemask with a halftone */
			(penum->masked &&
			 !color_is_pure(pgs->dev_color));
		if ( pgs->in_charpath )
			penum->render = image_render_skip;
		else if ( spp == 1 && bps == 1 && !penum->slow_loop &&
			  (penum->masked ||
			   (color_is_pure(&penum->icolor0) &&
			    color_is_pure(&penum->icolor1))) &&
			  ((dev_width =
			    fixed2long_rounded(mtx + width * penum->fxx) -
			    fixed2long_rounded(mtx)) == width ||
			   adjust == 0)
		   )
		{	penum->render = image_render_simple;
			if ( dev_width != width )
			{	/* Must buffer a scan line */
				dev_width = any_abs(dev_width);
				if ( dev_width + 7 > max_ushort )
				{	gs_image_cleanup(penum);
					return_error(gs_error_limitcheck);
				}
				penum->line_width = dev_width;
				penum->line_size = (dev_width + 7) >> 3;
				penum->line = (byte *)gs_malloc(1,
					penum->line_size, "image line");
				if ( penum->line == 0 )
				{	gs_image_cleanup(penum);
					return_error(gs_error_VMerror);
				}
			}
			/* The image is 1-for-1 with the device: */
			/* we don't want to spread the samples, */
			/* but we have to reset bps to prevent the buffer */
			/* pointer from being incremented by 8 bytes */
			/* per input byte. */
			penum->unpack = image_unpack_8;
			penum->bps = 8;
		}
		else
			penum->render =
			  (spp == 1 ? image_render_mono : image_render_color);
	   }
	if ( !penum->never_clip )
	   {	/* Set up the clipping device. */
		gx_device *dev = (gx_device *)&penum->clip_dev;
		penum->clip_dev = gs_clip_device;
		penum->clip_dev.target = gs_currentdevice(pgs);
		penum->clip_dev.list = pgs->clip_path->list;
		(*dev->procs->open_device)(dev);
	   }
	penum->adjust = adjust;
	penum->plane_index = 0;
	penum->byte_in_row = 0;
	penum->y = 0;
	if_debug9('b', "[b]Image: w=%d h=%d %s\n   [%f %f %f %f %f %f]\n",
		 width, height,
		 (penum->never_clip ? "no clip" : "must clip"),
		 mat.xx, mat.xy, mat.yx, mat.yy, mat.tx, mat.ty);
	return 0;
}
