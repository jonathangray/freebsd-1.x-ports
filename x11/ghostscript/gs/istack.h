/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.

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

/* istack.h */
/* Definitions for Ghostscript stacks */

#ifndef istack_INCLUDED
#  define istack_INCLUDED

/********************************
 * NOTE: on MS-DOS systems, the stacks are stored in the data segment.
 * This leads to large performance gains, at the expense of having to swap
 * the stacks explicitly when switching contexts or handling segment under-
 * or overflow (none of which are implemented yet!).
 ********************************/

typedef ref _ds *s_ptr;
typedef const ref _ds *const_s_ptr;

/*
 * Eventually, stacks will be allocated in linked chunks;
 * only the current chunk will be kept in the data segment.
 * For now, there is only one chunk.
 */
typedef struct ref_stack_chunk_s ref_stack_chunk;
struct ref_stack_chunk_s {
	ref_stack_chunk *next;		/* next lower chunk on stack */
	uint used_size;
	ref contents;			/* t_array */
};
/*
 * The base of the stack is allocated statically.
 */
typedef struct ref_stack_s {
	s_ptr p;			/* current top element */
	s_ptr bot;			/* bottommost valid element */
	s_ptr top;			/* topmost valid element */
	uint bot_guard;			/* # of guard elements below bot */
	uint top_guard;			/* # of guard elements above top */
	ulong extension_size;		/* total used_sizes of other chunks */
	ref_stack_chunk *next;		/* lower chunks */
} ref_stack;

#endif					/* istack_INCLUDED */
