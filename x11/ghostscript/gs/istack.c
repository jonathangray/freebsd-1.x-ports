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

/* istack.c */
/* Ghostscript expandable stack manager */
#include "memory_.h"
#include "ghost.h"
#include "alloc.h"
#include "errors.h"
#include "istack.h"
#include "save.h"

/* Forward references */
private void pop_chunk(P1(ref_stack *));

/* Initialize a stack. */
void
ref_stack_init(register ref_stack *pstack, ref_stack_ptr body, uint size,
  uint bot_guard, uint top_guard)
{	pstack->p = (pstack->bot = body + bot_guard) - 1;
	pstack->top = body + size - top_guard - 1;
	pstack->bot_guard = bot_guard;
	pstack->top_guard = top_guard;
	pstack->extension_size = 0;
	pstack->next = 0;
}

/* Reset a stack. */
void
ref_stack_reset(register ref_stack *pstack)
{	while ( pstack->next ) pop_chunk(pstack);
	pstack->p = pstack->bot - 1;
}

/* ------ Internal routines ------ */

/* Pop the top chunk off a stack. */
private void
pop_chunk(register ref_stack *pstack)
{	register ref_stack_chunk *psc = pstack->next;
	ref_stack_chunk next = psc->next;
	pstack->extension_size -= psc->used_size;
	alloc_free_array(&psc->contents, "pop_chunk(refs)");
	alloc_free((char *)psc, 1, sizeof(*psc), "pop_chunk(chunk)");
	pstack->next = next;
}
