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

/* sdctd.c */
/* DCT decoding filter */
#include "stdio_.h"
#include "gdebug.h"
#include "stream.h"

/* THIS IS JUST A STUB. */

/* Initialize DCTDecode filter */
void
s_DCTD_init(register stream *s, DCT_state *pdct)
{	s->state.dct = *pdct;
}

/* Buffer refill for DCTDecode filter */
private int
s_DCTD_read_buf(register stream *s)
{	return 0;
}

/* Stream procedures */
const stream_procs s_DCTD_procs =
   {	s_std_noavailable, NULL, s_std_read_flush, s_std_close,
	s_DCTD_read_buf, NULL
   };
