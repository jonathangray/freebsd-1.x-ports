/* alloc_pool.c - routines for allocating memory from tagged pools */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ukcprog_alloc_sccsid[] = "@(#)alloc.c	1.6 19/9/92 UKC";

#include <stdio.h>	/* for NULL - grrr */
#include <ukcstdlib.h>
#include <ukcstring.h>

#include "ukcprog.h"

/*  This is a conservative guess at the per-request malloc overhead in
 *  bytes.  Nothing breaks if this is wrong.
 */
#define MALLOC_OVERHEAD	24

/*  When we run out a space in an alloc pool we add another block.
 *  We add small blocks (SBLOCKSIZE bytes each) for the first NSMALLBLOCKS
 *  requests, then switch switch to large (BLOCKSIZE) ones.
 *
 *  The intention is that we don't gobble large amounts of memory for
 *  a small alloc pool, but that we are reasonablty efficient for
 *  one that's continually growing.
 *
 *  Currently, we go slowly (256 bytes a go) for the first 8K, then
 *  fast (4K a go).
 */
#define NSMALLBLOCKS	32

/*  Size of the first block for an alloc pool (requested when the alloc
 *  pool is created) and for the subsequent NSMALLBLOCKS blocks.
 */
#define SBLOCKSIZE	(256 - sizeof(alloc_t) - MALLOC_OVERHEAD)

/*  Size of the requested for an alloc pool after the first NSMALLBLOCKS
 *  block additions.
 *
 *  Try to make the malloc request size a bit less than a power of two
 *  to compensate for brain-damaged mallocs that add overhead then round
 *  up to a power of two.
 */
#define BLOCKSIZE	(4096 - sizeof(ablock_t) - MALLOC_OVERHEAD)

/*  Maximum alignment requirements for all types *including* float and double.
 */
#define ALIGN		sizeof(double)

typedef struct ablockst {
	union {
		double align;
		struct ablock {
			char *abu_buf;
			char *abu_pos;
			char *abu_end;
			long abu_size;
			struct ablockst *abu_next;
		} a;
	} u;
} ablock_t;

#define ab_buf	u.a.abu_buf
#define ab_pos	u.a.abu_pos
#define ab_end	u.a.abu_end
#define ab_size	u.a.abu_size
#define ab_next	u.a.abu_next

typedef struct allocst {
	ablock_t *al_ablock;
	ablock_t *al_freelist;
	int al_nblocks;
	bool al_debug;
	ablock_t al_first_ablock;
} alloc_t;

typedef struct alloc_markst {
	alloc_id_t am_alloc_id;
	ablock_t *am_ablock;
	char *am_pos;
	char *am_end;
} alloc_mark_t;

static ablock_t *push_ablock PROTO((alloc_t *al, ablock_t *ab, unsigned size));
static ablock_t *find_ab PROTO((alloc_t *al, unsigned size));
static void reset_ablocks PROTO((alloc_t *al, ablock_t *limab));

/*  The default debug flag for a new alloc_pool.  When the debug flag
 *  is TRUE, we initialise memory to garbage, and set it to (different)
 *  garbage when free_alloc_pool is called.
 */
static bool Default_debug_flag = TRUE;

bool
alloc_set_default_debug_flag(val)
bool val;
{
	bool oldval;

	oldval = Default_debug_flag;
	Default_debug_flag = val;
	return oldval;
}

bool
alloc_set_debug_flag(alloc_id, val)
alloc_id_t alloc_id;
bool val;
{
	alloc_t *al;
	bool oldval;

	al = (alloc_t *)alloc_id;
	oldval = al->al_debug;
	al->al_debug = val;
	return oldval;
}

/*  Make a new alloc_pool().  We make an initial allocation of a small
 *  amount of memory, to make small alloc pool creation cheap (one malloc).
 */
alloc_id_t
alloc_create_pool()
{
	alloc_t *al;

	al = (alloc_t *)e_malloc(sizeof(alloc_t) + SBLOCKSIZE);
	al->al_ablock = NULL;
	al->al_freelist = NULL;
	al->al_nblocks = 0;
	al->al_debug = Default_debug_flag;
	push_ablock(al, &al->al_first_ablock, SBLOCKSIZE);

	return (alloc_id_t)al;
}

static void
reset_ablocks(al, limab)
alloc_t *al;
ablock_t *limab;
{
	ablock_t *ab, *next;
	bool debug;

	debug = al->al_debug;
	for (ab = al->al_ablock; ab != limab; ab = next) {
		next = ab->ab_next;
		if (debug)
			memset(ab->ab_buf, 0x42, ab->ab_size);
		ab->ab_pos = ab->ab_buf;
		ab->ab_end = ab->ab_pos + ab->ab_size;
		ab->ab_next = al->al_freelist;
		al->al_freelist = ab;
	}
}

void
alloc_reset_pool(alloc_id)
alloc_id_t alloc_id;
{
	alloc_t *al;
	ablock_t *ab;

	al = (alloc_t *)alloc_id;

	ab = &al->al_first_ablock;

	reset_ablocks(al, ab);

	if (al->al_debug)
		memset(ab->ab_buf, 0x42, ab->ab_size);
	ab->ab_pos = ab->ab_buf;
	ab->ab_end = ab->ab_pos + ab->ab_size;

	al->al_ablock = ab;
}

void
alloc_free_pool(alloc_id)
alloc_id_t alloc_id;
{
	alloc_t *al;
	ablock_t *ab, *next;
	bool debug;

	al = (alloc_t *)alloc_id;
	debug = al->al_debug;

	/*  The odd form of the loop here is because we want to overwrite
	 *  all blocks with garbage (if debug is set), but we don't want
	 *  to free the last block in the chain, which is allocated as part
	 *  of the header block.
	 */
	ab = al->al_ablock;
	for (;;) {
		next = ab->ab_next;
		if (debug)
			memset(ab->ab_buf, 0x42, ab->ab_size);
		if (next == NULL)
			break;
		free((char *)ab);
		ab = next;
	}

	free((char *)al);
}

static ablock_t *
push_ablock(al, ab, size)
alloc_t *al;
ablock_t *ab;
unsigned size;
{
	ab->ab_buf = ab->ab_pos = (char *)&ab[1];
	ab->ab_end = ab->ab_buf + size;
	ab->ab_size = size;
	ab->ab_next = al->al_ablock;
	al->al_ablock = ab;

	if (al->al_debug)
		memset(ab->ab_buf, 0x53, (size_t)size);
	
	return ab;
}

/*  Find an ablock with at least nbytes free.  If the block at the
 *  head of the free list is big enough, use that.  Otherwise malloc
 *  a new ablock and push it on the chain.
 */
static ablock_t *
find_ab(al, size)
alloc_t *al;
unsigned size;
{
	ablock_t *ab;

	if (al->al_freelist != NULL && al->al_freelist->ab_size >= size) {
		ab = al->al_freelist;
		al->al_freelist = al->al_freelist->ab_next;
		ab->ab_next = al->al_ablock;
		al->al_ablock = ab;
	}
	else {
		voidptr buf;
		unsigned blocksize;

		blocksize = (al->al_nblocks < NSMALLBLOCKS) ? SBLOCKSIZE : BLOCKSIZE;
		if (size < blocksize)
			size = blocksize;
		if ((buf = malloc((size_t)(sizeof(ablock_t) + size))) == NULL)
			return NULL;
		ab = push_ablock(al, (ablock_t *)buf, size);
		++al->al_nblocks;
	}
	return ab;
}

/*  Allocate nbytes from alloc pool alloc_id.  This interface never
 *  returns NULL - if memory runs out we panic.
 */
voidptr
alloc(alloc_id, nbytes)
alloc_id_t alloc_id;
int nbytes;
{
	ablock_t *ab;
	int over;
	char *ptr;

	over = nbytes % ALIGN;
	if (over != 0)
		nbytes += ALIGN - over;

	ab = ((alloc_t *)alloc_id)->al_ablock;

	/*  We cast nbytes to unsigned to catch negative values: they
	 *  turn into huge positive values which get caught by e_malloc().
	 */
	if ((unsigned)nbytes > ab->ab_end - ab->ab_pos) {
		ab = find_ab((alloc_t *)alloc_id, (unsigned)nbytes);
		if (ab == NULL)
			panic("out of memory in alloc");
	}

	ptr = ab->ab_pos;
	ab->ab_pos += nbytes;

	return ptr;
}

/*  Like alloc, but return NULL if we can't satisfy the request.
 */
voidptr
alloc_ck(alloc_id, nbytes)
alloc_id_t alloc_id;
int nbytes;
{
	ablock_t *ab;
	int over;
	char *ptr;

	over = nbytes % ALIGN;
	if (over != 0)
		nbytes += ALIGN - over;

	ab = ((alloc_t *)alloc_id)->al_ablock;

	/*  We cast nbytes to unsigned to catch negative values: they
	 *  turn into huge positive values which get caught by e_malloc().
	 */
	if ((unsigned)nbytes > ab->ab_end - ab->ab_pos) {
		ab = find_ab((alloc_t *)alloc_id, (unsigned)nbytes);
		if (ab == NULL)
			return NULL;
	}

	ptr = ab->ab_pos;
	ab->ab_pos += nbytes;

	return ptr;
}

alloc_mark_id_t
alloc_mark(alloc_id)
alloc_id_t alloc_id;
{
	alloc_t *al;
	alloc_mark_t *am;
	ablock_t *save_ab;
	char *save_pos, *save_end;

	al = (alloc_t *)alloc_id;

	save_ab = al->al_ablock;
	save_pos = save_ab->ab_pos;
	save_end = save_ab->ab_end;

	am = (alloc_mark_t *)alloc(alloc_id, sizeof(alloc_mark_t));
	am->am_alloc_id = alloc_id;
	am->am_ablock = save_ab;
	am->am_pos = save_pos;
	am->am_end = save_end;

	return (alloc_mark_id_t)am;
}

void
alloc_release(alloc_id, alloc_mark_id)
alloc_id_t alloc_id;
alloc_mark_id_t alloc_mark_id;
{
	alloc_t *al;
	ablock_t *ab;
	alloc_mark_t *am, mark;

	al = (alloc_t *)alloc_id;
	am = (alloc_mark_t *)alloc_mark_id;
	if (am->am_alloc_id != alloc_id)
		panic("id botch in ar");

	/*  If debug is set, we are about to step on the store that
	 *  the mark was allocated from, so save it.
	 */
	mark = *am;
	ab = mark.am_ablock;

	reset_ablocks(al, ab);

	if (al->al_debug) {
		memset(mark.am_pos, 0x42, ab->ab_pos - mark.am_pos);
		memset(ab->ab_end, 0x42, mark.am_end - ab->ab_end);
	}
	else {
		/*  Make sure the application can't use this mark again.
		 */
		am->am_alloc_id = NULL;
	}

	ab->ab_pos = mark.am_pos;
	ab->ab_end = mark.am_end;
	al->al_ablock = ab;
}

/*  Like alloc(), except that the result is assumed not to need alignment.
 *  We work from the other end of the pool than alloc so hopefully all the
 *  string requests will be packed together with no alignment holes.
 *
 *  We never return NULL - if we can't fulfill the request we panic.
 */
char *
allocstr(alloc_id, nbytes)
alloc_id_t alloc_id;
int nbytes;
{
	ablock_t *ab;

	ab = ((alloc_t *)alloc_id)->al_ablock;

	/*  We cast nbytes to unsigned to catch negative values: they
	 *  turn into huge positive values which get caught by e_malloc().
	 */
	if ((unsigned)nbytes > ab->ab_end - ab->ab_pos) {
		ab = find_ab((alloc_t *)alloc_id, (unsigned)nbytes);
		if (ab == NULL)
			panic("out of memory in allocstr");
	}

	return ab->ab_end -= nbytes;
}

char *
allocstr_ck(alloc_id, nbytes)
alloc_id_t alloc_id;
int nbytes;
{
	ablock_t *ab;

	ab = ((alloc_t *)alloc_id)->al_ablock;

	/*  We cast nbytes to unsigned to catch negative values: they
	 *  turn into huge positive values which get caught by e_malloc().
	 */
	if ((unsigned)nbytes > ab->ab_end - ab->ab_pos) {
		ab = find_ab((alloc_t *)alloc_id, (unsigned)nbytes);
		if (ab == NULL)
			return NULL;
	}

	return ab->ab_end -= nbytes;
}

char *
alloc_strdup(alloc_id, s)
alloc_id_t alloc_id;
const char *s;
{
	int nbytes;

	nbytes = strlen(s) + 1;
	return memcpy(allocstr(alloc_id, nbytes), s, nbytes);
}
