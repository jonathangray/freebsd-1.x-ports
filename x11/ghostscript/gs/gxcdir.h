/* Copyright (C) 1993 Aladdin Enterprises.  All rights reserved.

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

/* gxcdir.h */
/* Definitions for character cache management */

/* Font/matrix pair cache management */

#ifndef cached_fm_pair_DEFINED
#  define cached_fm_pair_DEFINED
typedef struct cached_fm_pair_s cached_fm_pair;
#endif
typedef struct fm_pair_cache_s {
	uint msize, mmax;		/* # of cached font/matrix pairs */
	cached_fm_pair *mdata;
	uint mnext;			/* rover for allocating font/matrix pairs */
} fm_pair_cache;

/*
 * We allocate the character cache in chunks, so as not to tie up memory
 * prematurely if it isn't needed (or something else needs it more).
 * Thus there is a structure for managing an entire cache, and another
 * structure for managing each chunk.
 */

typedef struct char_cache_chunk_s char_cache_chunk;
struct char_cache_chunk_s {
	char_cache_chunk *next;
	byte *data;		/* struct cached_char_head_s * */
	uint size;
};

#ifndef cached_char_DEFINED
#  define cached_char_DEFINED
typedef struct cached_char_s cached_char;
#endif
typedef struct char_cache_s {
	const gs_memory_procs *mprocs;
	uint bsize, bmax;		/* # of bytes for cached chars */
	uint bspace;			/* space allocated for chunks */
	uint csize, cmax;		/* # of cached chars */
	uint lower;			/* min size at which cached chars */
					/* should be stored compressed */
	uint upper;			/* max size of a single cached char */
	cached_char **chars;		/* chain heads */
	uint chars_mask;		/* (a power of 2 -1) */
	char_cache_chunk *chunks;	/* current chunk in circular list */
	uint cnext;			/* rover for allocating characters */
					/* in current chunk */
	char_cache_chunk initial_chunk;	/* dummy initial chunk */
} char_cache;
