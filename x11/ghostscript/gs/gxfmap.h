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

/* gxfmap.h */
/* Fraction map representation for Ghostscript */

#ifndef gxfmap_INCLUDED
#  define gxfmap_INCLUDED

#include "gxfrac.h"
#include "gxrefct.h"

/*
 * Define a cached map from fracs to fracs.  Level 1 uses this only
 * for the transfer function; level 2 also uses it for black generation
 * and undercolor removal.  Note that the map is parameterized by
 * the number of bits used to index it.  The intended use is
	struct xxx_map {
		frac_map(xxx_index_bits);
	}
 * Note that reference counting macros must be used to allocate, free,
 * and assign references to frac_maps.
 */
typedef float (*frac_map_proc)(P2(const gs_state *, floatp));
#define frac_map(index_bits)\
	rc_header rc;\
	frac_map_proc proc;\
	frac values[1 << (index_bits)]

#endif					/* gxfmap_INCLUDED */
