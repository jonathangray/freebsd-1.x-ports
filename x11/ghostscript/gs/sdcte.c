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

/* sdcte.c */
/* DCT encoding filter */
#include "stdio_.h"
#include "gdebug.h"
#include "stream.h"

/* THIS IS JUST A STUB. */

/* Imported procedures */
extern int s_filter_write_flush(P1(stream *));

/* Initialize DCTEncode filter */
void
s_DCTE_init(register stream *s, DCT_state *pdct)
{	s->state.dct = *pdct;
}

/* Flush the buffer for DCTEecode filter */
private int
s_DCTE_write_buf(register stream *s)
{	return 0;
}

/* Stream procedures */
const stream_procs s_DCTE_procs =
   {	s_std_noavailable, NULL, s_filter_write_flush, s_std_close,
	NULL, s_DCTE_write_buf
   };
