/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gxcache.h */
/* Definitions for character cache */
/* Requires gxchar.h or gxcdir.h */
#include "gsuid.h"
#include "gsxfont.h"

/* An entry for a (font,matrix) pair in the character cache. */
/* If UID is valid, font may be 0, since we keep entries for fonts */
/* unloaded by a restore if they have valid UIDs. */
struct cached_fm_pair_s {
	struct gs_font_s *font;		/* base font */
	gs_uid UID;			/* font UniqueID or XUID */
	float mxx, mxy, myx, myy;	/* transformation */
	int num_chars;			/* # of cached chars with this */
					/* f/m pair */
	int xfont_tried;		/* true if we looked up an xfont */
	gx_xfont *xfont;		/* the xfont (if any) */
	const gs_memory_procs *mprocs;	/* the allocator for the xfont */
};
/* If font == 0 and UID is invalid, this is a free entry. */
#define fm_pair_is_free(pair)\
  ((pair)->font == 0 && !uid_is_valid(&(pair)->UID))
#define fm_pair_set_free(pair)\
  ((pair)->font = 0, uid_set_invalid(&(pair)->UID))

/* The character cache contains both used and free blocks. */
/* All blocks have a common header; free blocks have ONLY the header. */
typedef struct cached_char_head_s {
	uint size;			/* total block size in bytes */
	cached_fm_pair *pair;		/* font/matrix pair, 0 if free */
} cached_char_head;
#define cc_head_is_free(cch) ((cch)->pair == 0)
#define cc_head_set_free(cch) ((cch)->pair = 0)
/*
 * A cached bitmap for an individual character.
 * The bits, if any, immediately follow the structure;
 * characters with only xfont definitions may not have bits.
 * We maintain the invariant that if cc->head.pair != 0 (the character
 * is visible in the cache), at least one of the following must be true:
 *	- cc_has_bits(cc);
 *	- cc->xglyph != gx_no_xglyph && cc->head.pair->xfont != 0.
 */
struct char_cache_chunk_s;
#ifndef cached_char_DEFINED
#  define cached_char_DEFINED
typedef struct cached_char_s cached_char;
#endif
struct cached_char_s {
	/* The code, font/matrix pair, and wmode */
	/* are the 'key' in the cache. */
	cached_char_head head;		/* (must be first, */
					/* references font/matrix pair) */
	gs_glyph code;			/* glyph code */
	ushort wmode;			/* writing mode (0 or 1) */
	cached_char *next;		/* next in replacement ring */
	struct char_cache_chunk_s *chunk;	/* chunk where this char */
					/* is allocated */
	/* The rest of the structure is the 'value'. */
	gx_xglyph xglyph;		/* the xglyph for the xfont, if any */
	ushort raster, height;		/* dimensions of bitmap */
	ushort width;
	gx_bitmap_id id;		/* if null, no bits follow */
	gs_fixed_point wxy;		/* width in device coords */
	gs_fixed_point offset;		/* (-llx, -lly) in device coords */
};
#define cc_is_free(cc) cc_head_is_free(&(cc)->head)
#define cc_set_free(cc) cc_head_set_free(&(cc)->head)
#define cc_has_bits(cc) ((cc)->id != gx_no_bitmap_id)

/* Define the size of the cache structures. */
/* We round the size of a cached_char so that */
/* an immediately following bitmap will be properly aligned. */
#define align_cached_char_mod\
  (max(align_bitmap_mod, max(arch_align_ptr_mod, arch_align_long_mod)))
#define sizeof_cached_char\
  round_up(sizeof(cached_char), align_cached_char_mod)
#define cc_bits(cc) ((byte *)(cc) + sizeof_cached_char)

/* Define the hash chain for a (glyph, fm_pair) key. */
/* The OSF/1 compiler doesn't like casting a pointer to a shorter int. */
#if arch_sizeof_ptr == arch_sizeof_int
typedef uint ptr_uint_t;
#else
typedef ulong ptr_uint_t;
#endif
#define chars_head(dir, glyph, pair)\
  &(dir)->ccache.chars[((uint)(glyph) + ((ptr_uint_t)(pair) << 4)) & (dir)->ccache.chars_mask]
