/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gxccache.c */
/* Fast case character cache routines for Ghostscript library */
#include "gx.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate.h */
#include "gzcolor.h"
#include "gzpath.h"
#include "gxcpath.h"
#include "gxdevmem.h"
#include "gxchar.h"
#include "gxcache.h"
#include "gxfont.h"
#include "gxfdir.h"
#include "gxxfont.h"
#include "gscspace.h"			/* for gsimage.h */
#include "gsimage.h"

/* Import one routine from gxccman.c */
extern cached_fm_pair *gx_add_fm_pair(P4(gs_font_dir *,
  gs_font *, const gs_uid *, const gs_state *));

/* Look up, and if necessary add, a font/matrix pair in the cache */
cached_fm_pair *
gx_lookup_fm_pair(register const gs_state *pgs)
{	float	mxx = pgs->char_tm.xx, mxy = pgs->char_tm.xy,
		myx = pgs->char_tm.yx, myy = pgs->char_tm.yy;
	gs_font *font = pgs->font;
	register gs_font_dir *dir = font->dir;
	register cached_fm_pair *pair =
		dir->fmcache.mdata + dir->fmcache.mnext;
	int count = dir->fmcache.mmax;
	gs_uid uid;
	if ( font->FontType == ft_composite )
	{	uid_set_invalid(&uid);
	}
	else
	{	uid = font->data.base.UID;
		if ( uid_is_valid(&uid) )
			font = 0;
	}
	while ( count-- )
	{	if ( pair == dir->fmcache.mdata )
			pair += dir->fmcache.mmax;
		pair--;
		/* We either have a non-zero font and an invalid UID, */
		/* or a zero font and a valid UID: */
		if (	(font != 0 ? pair->font == font :
			 uid_equal(&pair->UID, &uid)) &&
			pair->mxx == mxx && pair->mxy == mxy &&
			pair->myx == myx && pair->myy == myy
		   )
		{	if ( pair->font == 0 )
				pair->font = pgs->font;
			return pair;
		}
	}
	return gx_add_fm_pair(dir, pgs->font, &uid, pgs);
}

/* Look up a glyph in the cache. */
/* Return the cached_char or 0. */
cached_char *
gx_lookup_cached_char(const gs_state *pgs, const cached_fm_pair *pair,
  gs_glyph glyph, int wmode)
{	gs_font_dir *dir = pgs->font->dir;
	register cached_char *cc = *chars_head(dir, glyph, pair);
	while ( cc != 0 )
	{	if ( cc->code == glyph && cc->head.pair == pair && cc->wmode == wmode )
		{	if_debug3('K', "[K]found 0x%lx for glyph=0x%lx, wmode=%d\n",
				  (ulong)cc, (ulong)glyph, wmode);
			return cc;
		}
		cc = cc->next;
	}
	if_debug2('K', "[K]not found: glyph=0x%lx, wmode=%d\n",
		  (ulong)glyph, wmode);
	return 0;
}

/* Look up a character in an external font. */
/* Return the cached_char or 0. */
cached_char *
gx_lookup_xfont_char(const gs_state *pgs, cached_fm_pair *pair,
  gs_char chr, gs_glyph glyph, gs_proc_glyph_name_t name_proc, int wmode)
{	gs_font *font = pair->font;
	gx_xfont *xf;
	gx_xglyph xg;
	gs_int_point wxy;
	gs_int_rect bbox;
	cached_char *cc;
	if ( font == 0 )
		return NULL;
	if ( !pair->xfont_tried )
	{	/* Look for an xfont now. */
		gx_lookup_xfont(pgs, pair,
				font->data.base.nearest_encoding_index);
		pair->xfont_tried = 1;
	}
	xf = pair->xfont;
	if ( xf == 0 )
		return NULL;
	/***** The following is wrong.  We should only use the nearest *****/
	/***** registered encoding if the character is really the same. *****/
	xg = (*xf->common.procs->char_xglyph)(xf, chr, font->data.base.nearest_encoding_index, glyph, name_proc);
	if ( xg == gx_no_xglyph )
		return NULL;
	if ( (*xf->common.procs->char_metrics)(xf, xg, wmode, &wxy, &bbox) < 0 )
		return NULL;
	cc = gx_alloc_char_bits(font->dir, NULL, bbox.q.x - bbox.p.x,
				bbox.q.y - bbox.p.y);
	if ( cc == 0 )
		return NULL;
	/* Success.  Make the cache entry. */
	cc->code = glyph;
	cc->wmode = wmode;
	cc->xglyph = xg;
	cc->wxy.x = int2fixed(wxy.x);
	cc->wxy.y = int2fixed(wxy.y);
	cc->offset.x = int2fixed(-bbox.p.x);
	cc->offset.y = int2fixed(-bbox.p.y);
	if_debug5('k', "[k]xfont %s char %d/0x%x#0x%lx=>0x%lx\n",
		  font->font_name.chars,
		  font->data.base.nearest_encoding_index, (int)chr,
		  (ulong)glyph, (ulong)xg);
	if_debug6('k', "     wxy=(%d,%d) bbox=(%d,%d),(%d,%d)\n",
		  wxy.x, wxy.y, bbox.p.x, bbox.p.y, bbox.q.x, bbox.q.y);
	gx_add_cached_char(font->dir, NULL, cc, pair, 1);
	return cc;
}

/* Copy a cached character to the screen. */
/* Assume the caller has already done gx_color_load. */
/* Return 0 if OK, 1 if we couldn't do the operation but no error */
/* occurred, or a negative error code. */
int
gx_image_cached_char(register gs_show_enum *penum, register cached_char *cc)
{	register gs_state *pgs = penum->pgs;
	int x, y, w, h;
	int code;
	gs_fixed_point pt;
	gx_device *dev = pgs->device->info;
	gx_device_clip cdev;
	gx_xfont *xf;
top:	code = gx_path_current_point_inline(pgs->path, &pt);
	if ( code < 0 ) return code;
	/* If the character doesn't lie entirely within the */
	/* quick-check clipping rectangle, we have to */
	/* set up an intermediate clipping device. */
	pt.x -= cc->offset.x;
	x = fixed2int_var_rounded(pt.x) + penum->ftx;
	pt.y -= cc->offset.y;
	y = fixed2int_var_rounded(pt.y) + penum->fty;
	w = cc->width;
	h = cc->height;
#ifdef DEBUG
	if ( gs_debug['K'] )
	{	if ( cc_has_bits(cc) )
			debug_dump_bytes(cc_bits(cc),
					 cc_bits(cc) + cc->raster * h,
					 "[K]bits");
		else
			dputs("[K]no bits\n");
		dprintf3("[K]copying 0x%lx, offset=(%g,%g)\n", (ulong)cc,
			 fixed2float(-cc->offset.x),
			 fixed2float(-cc->offset.y));
		dprintf6("   at (%g,%g)+(%d,%d)->(%d,%d)\n",
			 fixed2float(pt.x), fixed2float(pt.y),
			 penum->ftx, penum->fty, x, y);
	}
#endif
	if (	(x < penum->ibox.p.x || x + w > penum->ibox.q.x ||
		 y < penum->ibox.p.y || y + h > penum->ibox.q.y) &&
		dev != (gx_device *)&cdev	/* might be 2nd time around */
	   )
	{	/* Check for the character falling entirely outside */
		/* the clipping region. */
		if ( x >= penum->obox.q.x || x + w <= penum->obox.p.x ||
		     y >= penum->obox.q.y || y + h <= penum->obox.p.y
		   )
			return 0;		/* nothing to do */
		cdev = gs_clip_device;
		cdev.target = dev;
		cdev.list = pgs->clip_path->list;
		dev = (gx_device *)&cdev;
		(*dev->procs->open_device)(dev);
		if_debug0('K', "[K](clipping)\n");
	}
	/* If an xfont can render this character, use it. */
	if ( cc->xglyph != gx_no_xglyph && (xf = cc->head.pair->xfont) != 0 )
	{	int cx = x + fixed2int(cc->offset.x);
		int cy = y + fixed2int(cc->offset.y);
		if ( color_is_pure(pgs->dev_color) )
		{	code = (*xf->common.procs->render_char)(xf,
				cc->xglyph, dev, cx, cy,
				pgs->dev_color->color1, 0);
			if ( code >= 0 )
				return_check_interrupt(0);
		}
		/* Can't render directly.  If we don't have a bitmap yet, */
		/* get it from the xfont now. */
		if ( !cc_has_bits(cc) )
		{	gx_device_memory mdev;
			mdev = mem_mono_device;
			mdev.target = dev;
			gx_open_cache_device(&mdev, cc);
			code = (*xf->common.procs->render_char)(xf,
				cc->xglyph, (gx_device *)&mdev,
				cx - x, cy - y,
				(gx_color_index)1, 1);
			if ( code < 0 )
				return_check_interrupt(1);
			gx_add_char_bits(cc->head.pair->font->dir,
					 &mdev, cc, 1);
			/* gx_add_char_bits may change width, height, */
			/* raster, and/or offset.  It's easiest to */
			/* start over from the top. */
			goto top;
		}
	}
	/* No xfont.  Render from the cached bits. */
	if ( color_is_pure(pgs->dev_color) )
	{	code = (*dev->procs->copy_mono)
			(dev, cc_bits(cc), 0, cc->raster, cc->id,
			x, y, w, h,
			gx_no_color_index, pgs->dev_color->color1);
	}
	else
	{	/* Use imagemask to render the character. */
		gs_image_enum *pie =
			(gs_image_enum *)gs_malloc(1,
				gs_image_enum_sizeof,
				"image_char(image_enum)");
		gs_matrix mat;
		int iy;
		if ( pie == 0 )
			return_error(gs_error_VMerror);
		/* Make a matrix that will place the image */
		/* at (x,y) with no transformation. */
		gs_make_translation((floatp)-x, (floatp)-y, &mat);
		gs_matrix_multiply(&ctm_only(pgs), &mat, &mat);
		code = gs_imagemask_init(pie, pgs, w, h, 0, &mat, 0);
		switch ( code )
		{
		case 1:			/* empty image */
			code = 0;
		default:
			break;
		case 0:
			for ( iy = 0; iy < h && code >= 0; iy++ )
			  code = gs_image_next(pie, cc_bits(cc) + iy *
				cc->raster, (w + 7) >> 3);
			gs_image_cleanup(pie);
		}
		gs_free((char *)pie, 1, gs_image_enum_sizeof,
			"image_char(image_enum)");
	}
	return_check_interrupt(code);
}
