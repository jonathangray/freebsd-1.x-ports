/* Copyright (C) 1989, 1990, 1991, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gxfdir.h */
/* Font directory (font/character cache manager) definitions */
/* Requires gsfont.h. */
#include "gxcdir.h"

/* A font directory (font/character cache manager). */
struct gs_font_dir_s {
	const gs_memory_procs *mprocs;
		/* Original (unscaled) fonts */
	gs_font *orig_fonts;
		/* Scaled font cache */
	gs_font *scaled_fonts;		/* list of recently scaled fonts */
	uint ssize, smax;
		/* Font/matrix pair cache */
	fm_pair_cache fmcache;
		/* Character cache */
	char_cache ccache;
};
