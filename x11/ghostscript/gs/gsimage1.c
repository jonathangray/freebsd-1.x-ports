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

/* gsimage1.c */
/* Image procedures for Ghostscript library */
/* This file is logically part of gsimage.c; we have split it out */
/* to reduce the code working set. */
#include "gx.h"
#include "memory_.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gscspace.h"
#include "gspaint.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate.h */
#include "gzcolor.h"			/* requires gxdevice.h */
#include "gzpath.h"
#include "gxcolor.h"
#include "gxcpath.h"
#include "gxdevmem.h"
#include "gximage.h"

/* Process the next piece of an image */
int
gs_image_next(register gs_image_enum *penum, byte *dbytes, uint dsize)
{	uint rsize = penum->bytes_per_row;
	uint pos = penum->byte_in_row;
	int width = penum->width;
	uint dleft = dsize;
	uint dpos = 0;
	gs_state *pgs = penum->pgs;
	gx_device *save_dev = 0;
	fixed xcur_save, ycur_save;
	int y_save;
	int code;
	/* Accumulate separated colors, if needed */
	if ( penum->plane_index == 0 )
		penum->plane_size = dsize;
	else if ( dsize != penum->plane_size )
		return_error(gs_error_undefinedresult);
	penum->planes[penum->plane_index] = dbytes;
	if ( ++(penum->plane_index) != penum->spread )
		return 0;
	penum->plane_index = 0;
	/* Save the dynamic state components in case of an error. */
	xcur_save = penum->xcur;
	ycur_save = penum->ycur;
	y_save = penum->y;
	/* We've accumulated an entire set of planes. */
	if ( !penum->never_clip )
	   {	/* Install the clipping device if needed. */
		gx_device *dev = (gx_device *)&penum->clip_dev;
		save_dev = gs_currentdevice(pgs);
		penum->clip_dev.target = save_dev;
		gx_set_device_only(pgs, dev);
	   }
	while ( dleft )
	   {	/* Fill up a row, then display it. */
		uint bcount = min(dleft, rsize - pos);
		byte *bptr =
		  penum->buffer + (pos << 3) / penum->bps * penum->spread;
		int px;
		for ( px = 0; px < penum->spread; px++ )
		  (*penum->unpack)(penum, &penum->map[px], bptr + px, penum->planes[px] + dpos, bcount, pos);
		pos += bcount;
		dpos += bcount;
		dleft -= bcount;
		if ( pos == rsize )	/* filled an entire row */
		   {
#ifdef DEBUG
if ( gs_debug['B'] )
   {			int i, n = width * penum->spp;
			dputs("[B]row:");
			for ( i = 0; i < n; i++ )
				dprintf1(" %02x", penum->buffer[i]);
			dputs("\n");
   }
#endif
			if ( !penum->skewed )
			  { /* Precompute integer y and height, */
			    /* and check for clipping. */
			    fixed yc = penum->ycur, yn;
			    fixed dyy = penum->fyy;
			    fixed adjust = penum->adjust;
			    if ( dyy > 0 )
			      dyy += adjust << 1,
			      yc -= adjust;
			    else
			      dyy = (adjust << 1) - dyy,
			      yc -= dyy - adjust;
			    if ( yc >= penum->clip_box.q.y ) goto mt;
			    yn = yc + dyy;
			    if ( yn <= penum->clip_box.p.y ) goto mt;
			    penum->yci = fixed2int_var_rounded(yc);
			    penum->hci =
			      fixed2int_var_rounded(yn) - penum->yci;
			    if ( penum->hci == 0 ) goto mt;
			  }
			code = (*penum->render)(penum, penum->buffer, width * penum->spp, 1);
			if ( code < 0 ) goto err;
mt:			if ( ++(penum->y) == penum->height ) goto end;
			pos = 0;
			penum->xcur += penum->fyx;
			penum->ycur += penum->fyy;
		   }
	   }
	penum->byte_in_row = pos;
	code = 0;
	goto out;
end:	/* End of data */
	code = 1;
	goto out;
err:	/* Error or interrupt, restore original state. */
	penum->plane_index = penum->spread - 1;
	penum->xcur = xcur_save;
	penum->ycur = ycur_save;
	penum->y = y_save;
	/* Note that caller must call gs_image_cleanup */
	/* for both error and normal termination. */
out:	if ( save_dev != 0 ) gx_set_device_only(pgs, save_dev);
	return code;
}

/* Clean up by releasing the buffers. */
void
gs_image_cleanup(register gs_image_enum *penum)
{	if ( penum->buffer )
	{	gs_free((char *)penum->buffer, 1, penum->buffer_size,
			"image buffer");
		penum->buffer = 0;
	}
	if ( penum->line )
	{	gs_free((char *)penum->line, 1, penum->line_size,
			"image line");
		penum->line = 0;
	}
}

/* ------ Unpacking procedures ------ */

void
image_unpack_1(const gs_image_enum *penum, const sample_map *pmap,
  byte *bptr, register const byte *data, uint dsize, uint inpos)
{	register ulong *bufp = (unsigned long *)bptr;
	int left = dsize;
	register const ulong *map = &pmap->table.lookup4x1to32[0];
	register uint b;
	if ( left & 1 )
	   {	b = data[0];
		bufp[0] = map[b >> 4];
		bufp[1] = map[b & 0xf];
		data++, bufp += 2;
	   }
	left >>= 1;
	while ( left-- )
	   {	b = data[0];
		bufp[0] = map[b >> 4];
		bufp[1] = map[b & 0xf];
		b = data[1];
		bufp[2] = map[b >> 4];
		bufp[3] = map[b & 0xf];
		data += 2, bufp += 4;
	   }
}

void
image_unpack_2(const gs_image_enum *penum, const sample_map *pmap,
  byte *bptr, register const byte *data, uint dsize, uint inpos)
{	register ushort *bufp = (unsigned short *)bptr;
	int left = dsize;
	register const ushort *map = &pmap->table.lookup2x2to16[0];
	while ( left-- )
	   {	register unsigned b = *data++;
		*bufp++ = map[b >> 4];
		*bufp++ = map[b & 0xf];
	   }
}

void
image_unpack_4(const gs_image_enum *penum, const sample_map *pmap,
  register byte *bufp, register const byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	int left = dsize;
	register const byte *map = &pmap->table.lookup8[0];
	while ( left-- )
	   {	register unsigned b = *data++;
		*bufp = map[b >> 4]; bufp += spread;
		*bufp = map[b & 0xf]; bufp += spread;
	   }
}

void
image_unpack_8(const gs_image_enum *penum, const sample_map *ignore_pmap,
  byte *bufp, const byte *data, uint dsize, uint inpos)
{	if ( data != bufp ) memcpy(bufp, data, dsize);
}

/* ------ Rendering procedures ------ */

/* Rendering procedure for ignoring an image.  We still need to iterate */
/* over the samples, because the procedure might have side effects. */
int
image_render_skip(gs_image_enum *penum, byte *data, uint w, int h)
{	return h;
}

/* Rendering procedure for a monobit image with no */
/* skew or rotation and pure colors. */
int
image_render_simple(gs_image_enum *penum, byte *buffer, uint w, int h)
{	byte *line = penum->line;
	uint line_size, line_width;
	gx_device *dev = penum->pgs->device->info;
	dev_proc_copy_mono((*copy_mono)) = dev->procs->copy_mono;
	int ix = fixed2int_rounded(penum->xcur);
	const int iy = penum->yci, ih = penum->hci;
	gx_color_index
		zero = penum->icolor0.color1,
		one = penum->icolor1.color1;
	int dy;

	if ( penum->map[0].table.lookup4x1to32[0] != 0 )
		zero = penum->icolor1.color1,
		one = penum->icolor0.color1;

	if ( line == 0 )
	{	/* A direct BitBlt is possible. */
		line = buffer;
		line_size = (w + 7) >> 3;
		line_width = w;
	}
	else
	{	fixed xl = penum->xcur + fixed_half - int2fixed(ix);
		const fixed dxx = penum->fxx;
		const fixed dxx_4 = dxx << 2;
		const fixed dxx_8 = dxx_4 << 1;
		register const byte *psrc = buffer;
		byte sbit = 0x80;
		byte *endp = buffer + (w >> 3);
		const byte endbit = 1 << (~w & 7);
		byte data;

		line_size = penum->line_size;
		line_width = penum->line_width;
		if ( dxx < 0 )
			ix -= line_width,
			xl += int2fixed(line_width);

		/* Invert the bit following the last valid data bit. */
		if ( endbit == 0x80 ) *endp = ~endp[-1] << 7;
		else if ( *endp & (endbit << 1) ) *endp &= ~endbit;
		else *endp |= endbit;
		data = *psrc;

		memset(line, 0, line_size);
		/*
		 * Loop invariants:
		 *	data = *psrc;
		 *	sbit = 1 << n, 0<=n<=7.
		 */
		do
		{	int x0, n, bit;
			byte *bp;
			static const byte lmasks[9] =
			 { 0xff, 0x7f, 0x3f, 0x1f, 0xf, 7, 3, 1, 0 };
			static const byte rmasks[8] =
			 { 0, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe };

			/* Scan a run of zeros. */
			while ( ~data & sbit )
			{	xl += dxx;
				sbit >>= 1;
			}
			if ( !sbit )
			{	while ( (data = *++psrc) == 0 )
					xl += dxx_8;
				if ( data > 0xf )
					sbit = 0x80;
				else
					xl += dxx_4,
					sbit = 0x08;
				while ( ~data & sbit )
				{	xl += dxx;
					sbit >>= 1;
				}
			}
			if ( !(psrc < endp || sbit > endbit) )
				break;
			x0 = fixed2int_var(xl);

			/* Scan a run of ones. */
			while ( data & sbit )
			{	xl += dxx;
				sbit >>= 1;
			}
			if ( !sbit )
			{	while ( (data = *++psrc) == 0xff )
					xl += dxx_8;
				if ( data < 0xf0 )
					sbit = 0x80;
				else
					xl += dxx_4,
					sbit = 0x08;
				while ( data & sbit )
				{	xl += dxx;
					sbit >>= 1;
				}
			}

			/* Fill the run in the scan line. */
			n = fixed2int_var(xl) - x0;
			if ( n < 0 ) x0 += n, n = -n;
			bp = line + (x0 >> 3);
			bit = x0 & 7;
			if ( (n += bit) <= 8 )
				*bp |= lmasks[bit] - lmasks[n];
			else
			{	*bp++ |= lmasks[bit];
				if ( n > 64 )
				{	int nb = (n >> 3) - 1;
					memset(bp, 0xff, nb);
					bp += nb;
					n &= 7;
				}
				else
				{	n -= 8;
					while ( (n -= 8) >= 0 )
						*bp++ = 0xff;
				}
				*bp |= rmasks[n & 7];
			}

		} while ( psrc < endp || sbit > endbit );
	}

	/* Finally, transfer the scan line to the device. */
	for ( dy = 0; dy < ih; dy++ )
		(*copy_mono)(dev, line, 0, line_size, gx_no_bitmap_id,
			ix, iy + dy, line_width, 1, zero, one);

	return_check_interrupt(1);
}

/* Rendering procedure for the general case of displaying a */
/* monochrome image, dealing with multiple bit-per-sample images, */
/* general transformations, and arbitrary single-component */
/* color spaces (DeviceGray, CIEBasedA, Separation, Indexed). */
/* This procedure handles a single scan line. */
int
image_render_mono(gs_image_enum *penum, byte *buffer, uint w, int h)
{	gs_state *pgs = penum->pgs;
	const int masked = penum->masked;
	const fixed dxx = penum->fxx;
	fixed xt = penum->xcur;
	gs_color_space *pcs = pgs->color_space;
	cs_proc_remap_color((*remap_color)) = pcs->type->remap_color;
	gs_client_color cc;
	cmap_proc_gray((*map_gray)) = pgs->cmap_procs->map_gray;
	gx_device_color *pdevc = pgs->dev_color;
	/* Make sure the cache setup matches the graphics state. */
	/* Also determine whether all tiles fit in the cache. */
	int tiles_fit = gx_check_tile_cache(pgs);
#define image_set_gray(sample_value)\
   { pdevc = &penum->dev_colors[sample_value];\
     switch ( pdevc->halftone_level )\
      { default:		/* halftone */\
	  if ( !tiles_fit ) gx_color_load(pdevc, pgs); break;\
        case -1:		/* not computed yet */\
	  if ( penum->device_color )\
	    (*map_gray)(byte2frac(sample_value), pdevc, pgs);\
	  else\
	  { decode_sample(sample_value, cc, 0);\
	    (*remap_color)(&cc, pcs, pdevc, pgs);\
	  }\
	case 0: ;		/* pure color */\
      }\
   }
	fixed xl = xt;
	register const byte *psrc = buffer;
	byte *endp = buffer + w;
	fixed xrun = xt;		/* x at start of run */
	register byte run = *psrc;	/* run value */
	int htrun =			/* halftone run value */
	  (masked ? 255 : -2);

	*endp = ~endp[-1];	/* force end of run */
	if ( penum->slow_loop )
	  { /* Skewed, or imagemask with a halftone. */
	    const fixed
	      dxy = penum->fxy, dyx = penum->fyx,
	      dyy = penum->fyy;
	    fixed ytf = penum->ycur;
	    fixed yrun = ytf;
	    int code;
	    for ( ; ; )
	      { if ( *psrc++ != run )
		  { /* Fill the region between xrun and xl. */
		    if ( run != htrun )
		      { if ( run == 0 )
			  { if ( masked ) goto trans;
			  }
			htrun = run;
			image_set_gray(run);
		      }
		    code = gz_fill_pgram_fixed(xrun, yrun, xl - xrun,
					       ytf - yrun, dyx, dyy,
					       pdevc, pgs);
		    if ( code < 0 ) return code;
trans:		    if ( psrc > endp ) break;
		    yrun = ytf;
		    xrun = xl;
		    run = psrc[-1];
		  }
		xl += dxx;
		ytf += dxy;		/* harmless if no skew */
	      }
	  }
	else			/* fast loop */
	  { /* No skew, and not imagemask with a halftone. */
	    const fixed adjust = penum->adjust;
	    fixed xa = (dxx >= 0 ? adjust : -adjust);
	    const int yt = penum->yci, iht = penum->hci;
	    gx_device *dev = pgs->device->info;
	    dev_proc_fill_rectangle((*fill_proc)) = dev->procs->fill_rectangle;
	    dev_proc_tile_rectangle((*tile_proc)) = dev->procs->tile_rectangle;
	    dev_proc_copy_mono((*copy_mono_proc)) = dev->procs->copy_mono;
	    dev_proc_copy_color((*copy_color_proc)) = dev->procs->copy_color;
	    /* If each pixel is likely to fit in a single halftone tile, */
	    /* determine that now (tile_offset = offset of row within tile). */
	    int tile_offset =
	      gx_check_tile_size(pgs,
				 fixed2int_rounded(any_abs(dxx) + (xa << 1)),
				 yt, iht);
	    /* Fold the adjustment into xrun and xl, */
	    /* including the +0.5 for rounding. */
	    xrun = xrun - xa + fixed_half;
	    xl = xl + xa + fixed_half;
	    xa <<= 1;
	    for ( ; ; )
	      { /* Skip large constant regions quickly, */
	        /* but don't slow down transitions too much. */
	        while ( psrc[0] == run )
		{ if ( psrc[1] == run )
		  { if ( psrc[2] == run )
		    { if ( psrc[3] == run )
		      { psrc += 4, xl += dxx << 2;
			continue;
		      }
		      else
		        psrc += 3, xl += (dxx << 1) + dxx;
		    }
		    else
		      psrc += 2, xl += dxx << 1;
		  }
		  else
		    psrc++, xl += dxx;
		  break;
		}
		psrc++;
		  { /* Fill the region between xrun and xl. */
		    int xi = fixed2int_var(xrun);
		    int wi = fixed2int_var(xl) - xi;
		    int tsx, code;
		    gx_bitmap *tile;
		    if ( wi <= 0 )
		      { if ( wi == 0 ) goto mt;
			xi += wi, wi = -wi;
		      }
		    switch ( run )
		      {
		      case 0:
			if ( masked ) goto mt;
			if ( !color_is_pure(&penum->icolor0) ) goto ht;
			code = (*fill_proc)(dev, xi, yt, wi, iht, penum->icolor0.color1);
			break;
		      case 255:		/* just for speed */
			if ( !color_is_pure(&penum->icolor1) ) goto ht;
			code = (*fill_proc)(dev, xi, yt, wi, iht, penum->icolor1.color1);
			break;
		      default:
ht:			/* Use halftone if needed */
			if ( run != htrun )
			  { image_set_gray(run);
			    htrun = run;
			  }
			/* We open-code gz_fill_rectangle_open, */
			/* because we've done some of the work for */
			/* halftone tiles in advance. */
			if ( color_is_pure(pdevc) )
			  { code = (*fill_proc)(dev, xi, yt, wi, iht, pdevc->color1);
			  }
			else if ( tile_offset >= 0 &&
				  (tile = pdevc->tile,
				   (tsx = (xi + pgs->phase_mod.x) % tile->rep_width) + wi <= tile->size.x)
				)
			  { /* The pixel(s) fit(s) in a single tile. */
			    byte *row = tile->data + tile_offset;
			    code = (color_is_color_halftone(pdevc) ?
				    (*copy_color_proc)
				      (dev, row, tsx, tile->raster, gx_no_bitmap_id,
				       xi, yt, wi, iht) :
				    (*copy_mono_proc)
				      (dev, row, tsx, tile->raster, gx_no_bitmap_id,
				       xi, yt, wi, iht,
				       pdevc->color1, pdevc->color2)
				    );
			    return_if_interrupt();
			  }
			else
			  { code = (*tile_proc)(dev, pdevc->tile, xi, yt, wi, iht,
					     pdevc->color1, pdevc->color2,
					     pgs->phase_mod.x, pgs->phase_mod.y);
			  }
		      }
		    if ( code < 0 ) return code;
mt:		    if ( psrc > endp ) break;
		    xrun = xl - xa;	/* original xa << 1 */
		    run = psrc[-1];
		  }
		xl += dxx;
	      }
	  }
	return 1;
}
