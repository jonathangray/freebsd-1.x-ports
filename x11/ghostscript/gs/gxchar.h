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

/* gxchar.h */
/* Internal character definition for Ghostscript library */
/* Requires gsmatrix.h, gxfixed.h, gzdevice.h */
#include "gschar.h"

/* The type of cached characters is opaque. */
#ifndef cached_char_DEFINED
#  define cached_char_DEFINED
typedef struct cached_char_s cached_char;
#endif
extern const uint cached_char_sizeof;

/* The type of cached font/matrix pairs is opaque. */
#ifndef cached_fm_pair_DEFINED
#  define cached_fm_pair_DEFINED
typedef struct cached_fm_pair_s cached_fm_pair;
#endif
extern const uint cached_fm_pair_sizeof;

/* An enumeration object for string display. */
typedef enum {
	sws_none,
	sws_cache,			/* setcachedevice */
	sws_no_cache			/* setcharwidth */
} show_width_status;
struct gs_show_enum_s {
	/* Following are set at creation time */
	gs_state *pgs;
	int level;			/* save the level of pgs */
	const byte *str;		/* char may be signed! */
	uint size;
	float wcx, wcy;			/* for widthshow */
	gs_char wchr;			/* ditto */
	float ax, ay;			/* for ashow */
	int add;			/* true if a[width]show */
	int do_kern;			/* 1 if kshow, -1 if [x][y]show */
					/* or cshow, 0 otherwise */
	int slow_show;			/* [a][width]show or kshow or */
					/* [x][y]show or cshow */
	int charpath_flag;		/* 0 for show, 1 for false */
					/* charpath, 2 for true charpath */
	int stringwidth_flag;		/* 0 for show/charpath, */
					/* 1 for stringwidth/cshow */
	int can_cache;			/* true if can cache chars */
	gs_int_rect ibox;		/* int version of quick-check */
					/* (inner) clipping box */
	gs_int_rect obox;		/* int version of (outer) clip box */
	int is_composite;		/* true if composite font */
	int ftx, fty;			/* transformed font translation */
	int wmode;			/* WMode of root font */
	/* Following are updated dynamically */
	gs_glyph (*encode_char)(P3(gs_show_enum *, struct gs_font_s *, gs_char *));
					/* copied from font, */
					/* except for glyphshow */
	int cache_set;			/* true if suggested_scale & */
					/* dev_cache_info/dev have been set */
	int suggested_scale;		/* suggested scaling factor for */
					/* oversampling, based on FontBBox */
					/* and CTM */
	gx_device_memory dev_cache_info;
	device dev_cache_dev;
	gx_device_null dev_null;	/* null device for stringwidth */
	uint index;			/* index within string */
	gs_char current_char;		/* current char for render or move */
	gs_glyph current_glyph;		/* current glyph ditto */
	gs_fixed_point wxy;		/* width of current char */
					/* in device coords */
	gs_fixed_point origin;		/* unrounded origin of current char */
					/* in device coords, needed for */
					/* charpath and WMode=1 */
	cached_char *cc;		/* being accumulated */
	gs_point width;			/* total width of string, set at end */
	show_width_status width_status;
	int current_scale;
	int (*continue_proc)(P1(struct gs_show_enum_s *));	/* continuation procedure */
	/* Following are dynamic, for composite fonts only */
#define max_font_depth 5
	struct gs_font_s *fstack[max_font_depth];
	int fdepth;
	struct gs_font_s *pfont;
};
#define gs_show_enum_s_DEFINED

/* Cached character procedures (in gxcache.c) */
#ifndef gs_font_dir_DEFINED
#  define gs_font_dir_DEFINED	
typedef struct gs_font_dir_s gs_font_dir;
#endif
void	gx_char_cache_init(P1(gs_font_dir *));
cached_char *
	gx_alloc_char_bits(P4(gs_font_dir *, gx_device_memory *, ushort, ushort));
void	gx_open_cache_device(P2(gx_device_memory *, cached_char *));
void	gx_free_cached_char(P2(gs_font_dir *, cached_char *));
cached_fm_pair *
	gx_lookup_fm_pair(P1(const gs_state *));
void	gx_add_cached_char(P5(gs_font_dir *, gx_device_memory *, cached_char *, cached_fm_pair *, int));
void	gx_add_char_bits(P4(gs_font_dir *, gx_device_memory *, cached_char *, int));
cached_char *
	gx_lookup_cached_char(P4(const gs_state *, const cached_fm_pair *, gs_glyph, int));
cached_char *
	gx_lookup_xfont_char(P6(const gs_state *, cached_fm_pair *, gs_char, gs_glyph, gs_proc_glyph_name((*)), int));
int	gx_image_cached_char(P2(gs_show_enum *, cached_char *));
