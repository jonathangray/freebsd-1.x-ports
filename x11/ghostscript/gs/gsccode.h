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

/* gsccode.h */
/* Types for character codes in Ghostscript */

#ifndef gsccode_INCLUDED
#  define gsccode_INCLUDED

/* Define a character code.  Normally this is just a single byte from a */
/* string, but because of composite fonts, character codes must be 32 bits. */
typedef ulong gs_char;
#define gs_no_char ((gs_char)~0L)

/* Define a character glyph code, a.k.a. character name. */
typedef uint gs_glyph;
#define gs_no_glyph ((gs_glyph)~0)

/* Define a procedure for mapping a gs_glyph to its (string) name. */
#define gs_proc_glyph_name(proc)\
  const char *proc(P2(gs_glyph, uint *))
/* The following typedef is needed because ansi2knr can't handle */
/* gs_proc_glyph_name((*procname)) in a formal argument list. */
typedef gs_proc_glyph_name((*gs_proc_glyph_name_t));

#endif				/* gsccode_INCLUDED */
