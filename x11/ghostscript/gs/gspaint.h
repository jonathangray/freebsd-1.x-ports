/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.

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

/* gspaint.h */
/* Painting interface for Ghostscript library */
/* Requires gsstate.h and gsmatrix.h */

/* Painting */
int	gs_erasepage(P1(gs_state *)),
	gs_fill(P1(gs_state *)),
	gs_eofill(P1(gs_state *)),
	gs_stroke(P1(gs_state *));

/* Image tracing */
int	gs_imagepath(P4(gs_state *, int, int, const byte *));
