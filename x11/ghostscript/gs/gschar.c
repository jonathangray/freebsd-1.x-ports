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

/* gschar.c */
/* Character writing operators for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "string_.h"
#include "gserrors.h"
#include "gxfixed.h"			/* ditto */
#include "gxarith.h"
#include "gxmatrix.h"
#include "gzstate.h"			/* must precede gzdevice */
#include "gzdevice.h"			/* must precede gxchar */
#include "gxdevmem.h"
#include "gxchar.h"
#include "gxcache.h"
#include "gxfont.h"
#include "gspath.h"
#include "gzpath.h"
#include "gzcolor.h"

/* Exported size of enumerator */
const uint gs_show_enum_sizeof = sizeof(gs_show_enum);

/* Imported procedures */
extern void gx_set_black(P1(gs_state *));

/* Forward declarations */
private int continue_kshow(P1(gs_show_enum *));
private int continue_show(P1(gs_show_enum *));
private int continue_show_update(P1(gs_show_enum *));
private int show_setup(P3(gs_show_enum *, gs_state *, const char *));
private void show_cache_setup(P1(gs_show_enum *));
private int show_state_setup(P1(gs_show_enum *));
private int show_origin_setup(P4(gs_state *, fixed, fixed, int));
private int stringwidth_setup(P3(gs_show_enum *, gs_state *, const char *));

/* Print the ctm if debugging */
#define print_ctm(s,pgs)\
  dprintf7("[p]%sctm=[%g %g %g %g %g %g]\n", s,\
	   pgs->ctm.xx, pgs->ctm.xy, pgs->ctm.yx, pgs->ctm.yy,\
	   pgs->ctm.tx, pgs->ctm.ty)

/* ------ String writing operators ------ */

/* Setup macros for show operators */
#define setup_show_n()\
  penum->size = size
#define setup_a()\
  penum->add = 1, penum->ax = ax, penum->ay = ay,\
  penum->slow_show = 1
#define setup_width()\
  penum->wchr = chr, penum->wcx = cx, penum->wcy = cy,\
  penum->slow_show = 1

/* show[_n] */
int
gs_show_n_init(register gs_show_enum *penum,
   gs_state *pgs, const char *str, uint size)
{	setup_show_n();
	penum->slow_show = 0;
	return show_setup(penum, pgs, str);
}
int
gs_show_init(gs_show_enum *penum, gs_state *pgs, const char *str)
{	return gs_show_n_init(penum, pgs, str, strlen(str));
}

/* ashow[_n] */
int
gs_ashow_n_init(register gs_show_enum *penum,
  gs_state *pgs, floatp ax, floatp ay, const char *str, uint size)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	setup_a();
	return code;
}
int
gs_ashow_init(gs_show_enum *penum,
  gs_state *pgs, floatp ax, floatp ay, const char *str)
{	return gs_ashow_n_init(penum, pgs, ax, ay, str, strlen(str));
}

/* widthshow[_n] */
int
gs_widthshow_n_init(register gs_show_enum *penum,
  gs_state *pgs, floatp cx, floatp cy, gs_char chr, const char *str, uint size)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	setup_width();
	return code;
}
int
gs_widthshow_init(gs_show_enum *penum,
  gs_state *pgs, floatp cx, floatp cy, gs_char chr, const char *str)
{	return gs_widthshow_n_init(penum, pgs, cx, cy, chr, str, strlen(str));
}

/* awidthshow[_n] */
int
gs_awidthshow_n_init(register gs_show_enum *penum,
  gs_state *pgs, floatp cx, floatp cy, gs_char chr, floatp ax, floatp ay,
  const char *str, uint size)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	setup_a();
	setup_width();
	return code;
}
int
gs_awidthshow_init(gs_show_enum *penum,
  gs_state *pgs, floatp cx, floatp cy, gs_char chr, floatp ax, floatp ay,
  const char *str)
{	return gs_awidthshow_n_init(penum, pgs, cx, cy, chr, ax, ay,
				    str, strlen(str));
}

/* kshow[_n] */
int
gs_kshow_n_init(register gs_show_enum *penum,
  gs_state *pgs, const char *str, uint size)
{	int code;
	if ( pgs->font->FontType == ft_composite)
		return_error(gs_error_invalidfont);
	setup_show_n();
	code = show_setup(penum, pgs, str);
	penum->do_kern = penum->slow_show = 1;
	return code;
}
int
gs_kshow_init(gs_show_enum *penum, gs_state *pgs, const char *str)
{	return gs_kshow_n_init(penum, pgs, str, strlen(str));
}

/* xyshow[_n] */
int
gs_xyshow_n_init(register gs_show_enum *penum,
   gs_state *pgs, const char *str, uint size)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	penum->do_kern = -1;
	penum->slow_show = 1;
	return code;
}
int
gs_xyshow_init(gs_show_enum *penum, gs_state *pgs, const char *str)
{	return gs_xyshow_n_init(penum, pgs, str, strlen(str));
}

/* glyphshow */
private gs_proc_encode_char(gs_glyphshow_encode_char);
int
gs_glyphshow_init(gs_show_enum *penum, gs_state *pgs, gs_glyph glyph)
{	int code;
	if ( pgs->font->FontType == ft_composite)
		return_error(gs_error_invalidfont);
	penum->size = 1;
	penum->slow_show = 0;
	code = show_setup(penum, pgs, "\000");	/* arbitrary char */
	penum->current_glyph = glyph;
	penum->encode_char = gs_glyphshow_encode_char;
	return code;
}
private gs_glyph
gs_glyphshow_encode_char(gs_show_enum *penum, gs_font *pfont, gs_char *pchr)
{	/* We just nil out the character, and return the pre-loaded glyph. */
	*pchr = gs_no_char;
	return penum->current_glyph;
}

/* cshow[_n] */
int
gs_cshow_n_init(register gs_show_enum *penum,
   gs_state *pgs, const char *str, uint size)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	penum->do_kern = -1;
	penum->stringwidth_flag = 1;
	penum->slow_show = 1;
	return code;
}
int
gs_cshow_init(gs_show_enum *penum, gs_state *pgs, const char *str)
{	return gs_cshow_n_init(penum, pgs, str, strlen(str));
}

/* ------ Related operators ------ */

/* stringwidth[_n] */
int
gs_stringwidth_n_init(gs_show_enum *penum, gs_state *pgs, const char *str, uint size)
{	setup_show_n();
	return stringwidth_setup(penum, pgs, str);
}
int
gs_stringwidth_init(gs_show_enum *penum, gs_state *pgs, const char *str)
{	return gs_stringwidth_n_init(penum, pgs, str, strlen(str));
}

/* Common code for stringwidth[_n] */
private int
stringwidth_setup(gs_show_enum *penum, gs_state *pgs, const char *str)
{	int code = (penum->slow_show = 0, show_setup(penum, pgs, str));
	if ( code < 0 ) return_error(code);
	penum->stringwidth_flag = 1;
	/* Do an extra gsave and suppress output */
	if ( (code = gs_gsave(pgs)) < 0 ) return code;
	penum->level = pgs->level;	/* for level check in show_update */
	/* Set up a null device that forwards xfont requests properly. */
	penum->dev_null = gs_null_device;
	penum->dev_null.target = pgs->device->info;
	pgs->device->info = (gx_device *)&penum->dev_null;
	/* Establish an arbitrary current point. */
	return gx_path_add_point(pgs->path, pgs->ctm.tx_fixed, pgs->ctm.ty_fixed);
}

/* charpath[_n] */
int
gs_charpath_n_init(gs_show_enum *penum, gs_state *pgs,
  const char *str, uint size, int bool)
{	int code;
	setup_show_n();
	code = show_setup(penum, pgs, str);
	penum->charpath_flag = (bool ? 2 : 1);
	penum->can_cache = 0;
	return code;
}
int
gs_charpath_init(gs_show_enum *penum, gs_state *pgs,
  const char *str, int bool)
{	return gs_charpath_n_init(penum, pgs, str, strlen(str), bool);
}

/* ------ Width/cache operators ------ */

private int set_cache_device(P6(gs_show_enum *, gs_state *,
  floatp, floatp, floatp, floatp));

/* setcachedevice */
int
gs_setcachedevice(gs_show_enum *penum, gs_state *pgs,
  floatp wx, floatp wy, floatp llx, floatp lly, floatp urx, floatp ury)
{	int code = gs_setcharwidth(penum, pgs, wx, wy);	/* default is don't cache */
	if ( code < 0 ) return code;
	return set_cache_device(penum, pgs, llx, lly, urx, ury);
}

/* setcachedevice2 */
int
gs_setcachedevice2(gs_show_enum *penum, gs_state *pgs,
  floatp w0x, floatp w0y, floatp llx, floatp lly, floatp urx, floatp ury,
  floatp w1x, floatp w1y, floatp vx, floatp vy)
{	int code;
	if ( penum->wmode )
	{	code = gs_setcharwidth(penum, pgs, w1x, w1y);
		if ( code < 0 ) return code;
		/****** ADJUST ORIGIN BY vx, vy ******/
	}
	else
	{	code = gs_setcharwidth(penum, pgs, w0x, w0y);
		if ( code < 0 ) return code;
	}
	return set_cache_device(penum, pgs, llx, lly, urx, ury);
}

/* Set up the cache device if relevant. */
/* Used by setcachedevice and setcachedevice2. */
private int
set_cache_device(register gs_show_enum *penum, gs_state *pgs,
  floatp llx, floatp lly, floatp urx, floatp ury)
{	/* See if we want to cache this character. */
	if ( pgs->in_cachedevice )		/* no recursion! */
		return 0;
	pgs->in_cachedevice = 1;	/* disable color/gray/image operators */
	/* We can only use the cache if ctm is unchanged */
	/* (aside from a possible translation), */
	/* and if the extent of the box is non-negative. */
	if ( !penum->can_cache || !pgs->char_tm_valid ||
	     llx > urx || lly > ury
	   )
		return 0;
	   {	gs_font_dir *dir = pgs->font->dir;
		gs_fixed_point cbox_ll, cbox_ur, cdim;
		long iwidth, iheight;
		cached_char *cc;
		gs_fixed_rect clip_box;
		int code;
		gs_distance_transform2fixed(&pgs->ctm, llx, lly, &cbox_ll);
		gs_distance_transform2fixed(&pgs->ctm, urx, ury, &cbox_ur);
		cdim.x = cbox_ur.x - cbox_ll.x;
		cdim.y = cbox_ur.y - cbox_ll.y;
		if ( cdim.x < 0 ) cdim.x = -cdim.x;
		if ( cdim.y < 0 ) cdim.y = -cdim.y;
#ifdef DEBUG
if ( gs_debug['k'] )
   {	dprintf4("[k]cbox=[%g %g %g %g]\n",
		 fixed2float(cbox_ll.x), fixed2float(cbox_ll.y),
		 fixed2float(cbox_ur.x), fixed2float(cbox_ur.y));
	print_ctm("  ", pgs);
   }
#endif
		iwidth = fixed2long(cdim.x) + 2;
		iheight = fixed2long(cdim.y) + 2;
		if (	iwidth != (ushort)iwidth ||
			iheight != (ushort)iheight
		   )
		  return 0;		/* much too big */
		if ( !penum->cache_set )
			show_cache_setup(penum);
		cc = gx_alloc_char_bits(dir,
				(gx_device_memory *)&penum->dev_cache_info,
				(ushort)iwidth,
				(ushort)iheight);
		if ( cc == 0 )
			return 0;	/* too big for cache */
		/* The mins handle transposed coordinate systems.... */
		/* Truncate the offsets to avoid artifacts later. */
		cc->offset.x = fixed_ceiling(-min(cbox_ll.x, cbox_ur.x));
		cc->offset.y = fixed_ceiling(-min(cbox_ll.y, cbox_ur.y));
		if_debug4('k', "[k]width=%ld, height=%ld, offset=[%g %g]\n",
			  iwidth, iheight,
			  fixed2float(cc->offset.x),
			  fixed2float(cc->offset.y));
		if ( (code = gs_gsave(pgs)) < 0 )
		   {	gx_free_cached_char(dir, cc);
			return code;
		   }
		/* Nothing can go wrong now.... */
		penum->cc = cc;
		cc->code = gs_show_current_glyph(penum);
		cc->wmode = penum->wmode;
		cc->wxy = penum->wxy;
		/* Install the device */
		pgs->device = &penum->dev_cache_dev;
		pgs->device_is_shared = 1;	/* don't deallocate */
		/* Adjust the translation in the graphics context */
		/* so that the character lines up with the cache. */
		gs_translate_to_fixed(pgs, cc->offset.x, cc->offset.y);
		/* Set the initial matrix for the cache device. */
		penum->dev_cache_info.initial_matrix = ctm_only(pgs);
		/* Reset the clipping path to match the metrics. */
		clip_box.p.x = clip_box.p.y = 0;
		clip_box.q.x = int2fixed(iwidth);
		clip_box.q.y = int2fixed(iheight);
		if ( (code = gx_clip_to_rectangle(pgs, &clip_box)) < 0 )
		  return code;
		gx_set_black(pgs);	/* Set the color to black. */
	   }
	penum->width_status = sws_cache;
	return 0;
}

/* setcharwidth */
int
gs_setcharwidth(register gs_show_enum *penum, gs_state *pgs, floatp wx, floatp wy)
{	if ( penum->width_status != sws_none )
		return_error(gs_error_undefined);
	gs_distance_transform2fixed(&pgs->ctm, wx, wy, &penum->wxy);
	penum->width_status = sws_no_cache;
	return 0;
}

/* ------ Enumerator ------ */

/* Do the next step of a show (or stringwidth) operation */
int
gs_show_next(gs_show_enum *penum)
{	return (*penum->continue_proc)(penum);
}

/* Continuation procedures */
#define show_fast_move(wxy, pgs)\
  gx_path_add_rel_point_inline(pgs->path, wxy.x, wxy.y)
private int show_update(P1(gs_show_enum *penum));
private int show_move(P1(gs_show_enum *penum));
private int show_proceed(P1(gs_show_enum *penum));
private int show_finish(P1(gs_show_enum *penum));
private int
continue_show_update(register gs_show_enum *penum)
{	int code = show_update(penum);
	if ( code < 0 ) return code;
	code = show_move(penum);
	if ( code != 0 ) return code;
	return show_proceed(penum);
}
private int
continue_show(register gs_show_enum *penum)
{	return show_proceed(penum);
}
/* For kshow, the CTM or font may have changed, so we have to reestablish */
/* the cached values in the enumerator. */
private int
continue_kshow(register gs_show_enum *penum)
{	int code = show_state_setup(penum);
	if ( code < 0 ) return code;
	return show_proceed(penum);
}

/* Update position */
private int
show_update(register gs_show_enum *penum)
{	register gs_state *pgs = penum->pgs;
	/* Update position for last character */
	switch ( penum->width_status )
	   {
	case sws_none:
		/* Adobe interpreters assume a character width of 0, */
		/* even though the documentation says this is an error.... */
		penum->wxy.x = penum->wxy.y = 0;
		break;
	case sws_cache:
	   {	/* Finish installing the cache entry. */
		cached_char *cc = penum->cc;
		int code;
		/* If the BuildChar/BuildGlyph procedure did a save and a */
		/* restore, it already undid the gsave in setcachedevice. */
		/* We have to check for this by comparing levels. */
		switch ( pgs->level - penum->level )
		   {
		default:
			return_error(gs_error_invalidfont);	/* WRONG */
		case 2:
			code = gs_grestore(pgs);
			if ( code < 0 ) return code;
		case 1:
			;
		   }
		gx_add_cached_char(pgs->font->dir, &penum->dev_cache_info,
				   cc, gx_lookup_fm_pair(pgs),
				   penum->current_scale);
		if ( !penum->stringwidth_flag && !penum->charpath_flag )
		{	/* Copy the bits to the real output device. */
			code = gs_grestore(pgs);
			if ( code < 0 ) return code;
			code = gx_color_load(pgs->dev_color, pgs);
			if ( code < 0 ) return code;
			return gx_image_cached_char(penum, cc);
		}
	   }
	case sws_no_cache: ;
	   }
	if ( penum->charpath_flag )
	{	/* Move back to the character origin, so that */
		/* show_move will get us to the right place. */
		int code = gx_path_add_point(pgs->show_gstate->path,
				penum->origin.x, penum->origin.y);
		if ( code < 0 ) return code;
	}
	return gs_grestore(pgs);
}

/* Move to next character */
private int
show_move(register gs_show_enum *penum)
{	register gs_state *pgs = penum->pgs;
	if ( penum->do_kern < 0 )
	{	/* xyshow or cshow */
		/****** WRONG FOR cshow, MUST MOVE FIRST ******/
		penum->continue_proc = continue_show;
		return gs_show_move;
	}
	if ( penum->add )
		gs_rmoveto(pgs, penum->ax, penum->ay);
	if ( penum->str[penum->index - 1] == penum->wchr )
		gs_rmoveto(pgs, penum->wcx, penum->wcy);
	/* wxy is in device coordinates */
	   {	int code = show_fast_move(penum->wxy, pgs);
		if ( code < 0 ) return code;
	   }
	/* Check for kerning, but not on the last character. */
	if ( penum->do_kern && penum->index < penum->size )
	   {	penum->continue_proc = continue_kshow;
		return gs_show_kern;
	   }
	return 0;
}
/* Process next character */
private int
show_proceed(register gs_show_enum *penum)
{	register gs_state *pgs = penum->pgs;
	const byte *str = penum->str;
	uint index;
	gs_font *pfont;
	cached_fm_pair *pair = 0;
	int wmode = penum->wmode;
	gs_char chr;
	gs_glyph glyph;
	int code;
	code = gx_color_load(pgs->dev_color, pgs);
	if ( code < 0 ) return code;
more:	/* Proceed to next character */
	pfont = pgs->font;
	if ( penum->can_cache )
	   {	/* Loop with cache */
		if ( pair == 0 )
		  pair = gx_lookup_fm_pair(pgs);
		while ( (index = penum->index++) != penum->size )
		{	cached_char *cc;
			chr = str[index];
			glyph = (*penum->encode_char)(penum, pfont, &chr);
			if ( glyph == gs_no_glyph )
				goto no_cache;
			cc = gx_lookup_cached_char(pgs, pair, glyph, wmode);
			if ( cc == 0 )
			{	/* If possible, try for an xfont before */
				/* rendering from the outline. */
				if ( pfont->ExactSize == fbit_use_outlines )
					goto no_cache;
				cc = gx_lookup_xfont_char(pgs, pair, chr,
					glyph, pfont->glyph_name_proc, wmode);
				if ( cc == 0 )
					goto no_cache;
			}
			/* Character is in cache. */
			if ( !penum->stringwidth_flag )
			{	code = gx_image_cached_char(penum, cc);
				if ( code < 0 ) return code;
				else if ( code > 0 ) goto no_cache;
			}
			if ( penum->slow_show )
			{	/* Split up the assignment so that the */
				/* Watcom compiler won't reserve esi/edi. */
				penum->wxy.x = cc->wxy.x;
				penum->wxy.y = cc->wxy.y;
				code = show_move(penum);
			}
			else
				code = show_fast_move(cc->wxy, pgs);
			if ( code )
			{	/* Might be kshow, so store the state. */
				penum->current_char = chr;
				penum->current_glyph = glyph;
				return code;
			}
		}
		/* All done. */
		return show_finish(penum);
	   }
	else
	   {	/* Can't use cache */
		if ( (index = penum->index++) == penum->size )
		  {	/* All done. */
			return show_finish(penum);
		  }
		chr = str[index];
		glyph = (*penum->encode_char)(penum, pfont, &chr);
	   }
no_cache:
	/* Character is not cached, client must render it. */
	penum->current_char = chr;
	penum->current_glyph = glyph;
	if ( (code = gs_gsave(pgs)) < 0 )
		return code;
	/* Reset the in_cachedevice flag, so that a recursive show */
	/* will use the cache properly. */
	pgs->in_cachedevice = 0;
	/* Reset the sampling scale, which is only used by Type 1 outlines. */
	penum->current_scale = 1;
	/* Set the charpath flag in the graphics context if necessary, */
	/* so that fill and stroke will add to the path */
	/* rather than having their usual effect. */
	pgs->in_charpath = penum->charpath_flag;
	pgs->stroke_adjust = 0;		/* per specification */
	   {	gs_fixed_point cpt;
		gx_path *ppath = pgs->path;
		if ( (code = gx_path_current_point_inline(ppath, &cpt)) < 0 )
			return code;
		cpt.x -= pgs->ctm.tx_fixed;
		cpt.y -= pgs->ctm.ty_fixed;
		gs_setmatrix(pgs, &char_tm_only(pgs));
		penum->origin.x = cpt.x += pgs->ctm.tx_fixed;
		penum->origin.y = cpt.y += pgs->ctm.ty_fixed;
		gs_newpath(pgs);
		code = show_origin_setup(pgs, cpt.x, cpt.y,
					 penum->charpath_flag);
		if ( code < 0 ) return code;
	   }
	penum->width_status = sws_none;
	penum->continue_proc = continue_show_update;
	/* Try using the build procedure in the font. */
	/* < 0 means error, 0 means success, 1 means failure. */
	code = (*pfont->build_char_proc)(penum, pgs, pfont, chr, glyph);
	if ( code < 0 ) return_error(code);
	if ( code == 0 )
	   {	code = show_update(penum);
		if ( code < 0 ) return code;
		code = show_move(penum);
		if ( code ) return code;
		goto more;
	   }
	return gs_show_render;
}

/* Finish show or stringwidth */
private int
show_finish(register gs_show_enum *penum)
{	register gs_state *pgs = penum->pgs;
	int code;
	if ( !penum->stringwidth_flag ) return 0;
	/* Save the accumulated width before returning, */
	/* and undo the extra gsave. */
	code = gs_currentpoint(pgs, &penum->width);
	if ( code < 0 ) return code;
	return gs_grestore(pgs);
}

/* Return the current character for rendering. */
gs_char
gs_show_current_char(const gs_show_enum *penum)
{	return penum->current_char;
}

/* Return the current glyph for rendering. */
gs_glyph
gs_show_current_glyph(const gs_show_enum *penum)
{	return penum->current_glyph;
}

/* Return the width of the just-enumerated character (for cshow). */
int
gs_show_current_width(const gs_show_enum *penum, gs_point *ppt)
{	return gs_idtransform(penum->pgs, fixed2float(penum->wxy.x),
			      fixed2float(penum->wxy.y), ppt);
}

/* Return the just-displayed character for kerning. */
gs_char
gs_kshow_previous_char(const gs_show_enum *penum)
{	return penum->current_char;
}

/* Return the about-to-be-displayed character for kerning. */
gs_char
gs_kshow_next_char(const gs_show_enum *penum)
{	return penum->str[penum->index];
}

/* ------ Miscellaneous accessors ------ */

/* Return the root font of the current font. */
gs_font *
gs_rootfont(gs_show_enum *penum, gs_state *pgs)
{	return (penum == 0 || penum->fdepth < 0 ? gs_currentfont(pgs) : penum->fstack[0]);
}

/* Return the accumulated width for stringwidth. */
void
gs_show_width(const gs_show_enum *penum, gs_point *ppt)
{	*ppt = penum->width;
}

/* Return the charpath flag. */
int
gs_show_in_charpath(const gs_show_enum *penum)
{	return penum->charpath_flag;
}

/* ------ Internal routines ------ */

/* Initialize a show enumerator. */
private int
show_setup(register gs_show_enum *penum, gs_state *pgs, const char *str)
{	int code;
	gs_font *pfont = pgs->font;
	penum->pgs = pgs;
	penum->level = pgs->level;
	penum->str = (const byte *)str;	/* avoid signed chars */
	penum->wchr = gs_no_char;
	penum->add = 0;
	penum->do_kern = 0;
	penum->charpath_flag = 0;
	penum->stringwidth_flag = 0;
	penum->index = 0;
	penum->continue_proc = continue_show;
	if ( (penum->is_composite = pfont->FontType == ft_composite) )
	   {	gs_font *rfont = pgs->font;
		penum->fstack[0] = rfont;
		penum->fdepth = 0;
		penum->pfont =
		  rfont->data.type0_data.FDepVector[rfont->data.type0_data.Encoding[0]];
	   }
	else
		penum->fdepth = -1;
	penum->wmode = pfont->WMode;
	penum->can_cache = 1;		/* show_state_setup may reset */
	code = show_state_setup(penum);
	if ( code < 0 ) return code;
	penum->cache_set = 0;
	pgs->show_gstate = pgs;
	return 0;
}

/* Initialize the gstate-derived parts of a show enumerator. */
/* We do this both when starting the show operation, */
/* and when returning from the kshow callout. */
private int
show_state_setup(gs_show_enum *penum)
{	gs_state *pgs = penum->pgs;
	gs_currentcharmatrix(pgs, NULL, 1);	/* make char_tm valid */
	if ( penum->can_cache &= /* no skewing or non-rectangular rotation */
		(is_fzero2(pgs->char_tm.xy, pgs->char_tm.yx) ||
		 is_fzero2(pgs->char_tm.xx, pgs->char_tm.yy)) )
	   {	gs_fixed_rect cbox;
		gx_cpath_box_for_check(pgs->clip_path, &cbox);
		penum->ibox.p.x = fixed2int_var_ceiling(cbox.p.x);
		penum->ibox.p.y = fixed2int_var_ceiling(cbox.p.y);
		penum->ibox.q.x = fixed2int_var(cbox.q.x);
		penum->ibox.q.y = fixed2int_var(cbox.q.y);
		gx_path_bbox(&pgs->clip_path->path, &cbox);
		penum->obox.p.x = fixed2int_var(cbox.p.x);
		penum->obox.p.y = fixed2int_var(cbox.p.y);
		penum->obox.q.x = fixed2int_var_ceiling(cbox.q.x);
		penum->obox.q.y = fixed2int_var_ceiling(cbox.q.y);
		penum->ftx = (int)fixed2long(pgs->char_tm.tx_fixed - pgs->ctm.tx_fixed);
		penum->fty = (int)fixed2long(pgs->char_tm.ty_fixed - pgs->ctm.ty_fixed);
	   }
	penum->encode_char = pgs->font->encode_char_proc;
	return 0;
}

/* Set up the cache device and related information. */
private void
show_cache_setup(gs_show_enum *penum)
{	gs_state *pgs = penum->pgs;
	device *dev = &penum->dev_cache_dev;
	penum->dev_cache_info = mem_mono_device;
	penum->dev_cache_info.target = pgs->device->info;
	dev->info = (gx_device *)&penum->dev_cache_info;
	dev->is_band_device = 0;
	dev->white = 1;
	dev->black = 1;
	/* Decide whether to oversample. */
	/* We have to decide this now, because */
	/* type1addpath has to know it in advance. */
	if ( penum->is_composite )
	{	penum->suggested_scale = 1;	/* WRONG, should */
				/* use info from eventual base font */
	}
	else
	{	gs_font *pfont = pgs->font;
		gs_fixed_point extent;
		gs_distance_transform2fixed(&pgs->char_tm,
			pfont->data.base.FontBBox.q.x -
			  pfont->data.base.FontBBox.p.x,
			pfont->data.base.FontBBox.q.y -
			  pfont->data.base.FontBBox.p.y,
			&extent);
		penum->suggested_scale =
		  (extent.x != 0 && extent.y != 0 &&
		   any_abs(extent.x) < int2fixed(16) &&
		   any_abs(extent.y) < int2fixed(16) ? 4 : 1);
	}
	penum->cache_set = 1;
}

/* Set the character origin as the origin of the coordinate system. */
/* Used before rendering characters, and for moving the origin */
/* in setcachedevice2 when WMode=1. */
private int
show_origin_setup(gs_state *pgs, fixed cpt_x, fixed cpt_y, int charpath_flag)
{	if ( !charpath_flag )
	  { /* Round the translation in the graphics state. */
	    /* This helps prevent rounding artifacts later. */
	    cpt_x = fixed_rounded(cpt_x);
	    cpt_y = fixed_rounded(cpt_y);
	  }
	gs_translate_to_fixed(pgs, cpt_x, cpt_y);
	return gx_path_add_point(pgs->path, pgs->ctm.tx_fixed,
				 pgs->ctm.ty_fixed);
}
