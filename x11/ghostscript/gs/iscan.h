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

/* iscan.h */
/* Interface to Ghostscript scanner */
/* Requires stream.h */

/* Read a token from a stream. */
/* As usual, 0 is a normal return, <0 is an error. */
/* There are also two special return codes: */
#define scan_BOS 1		/* binary object sequence */
#define scan_EOF 2		/* end of stream */
extern int scan_token(P3(stream *s, int from_string, ref *pref));

/* Scan a number for cvi or cvr. */
/* The first argument is a t_string. */
extern int scan_number_only(P2(const ref *, ref *));
