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

/* gxfrac.h */
/* Fraction representation for Ghostscript */

#ifndef gxfrac_INCLUDED
#  define gxfrac_INCLUDED

/* Represent a fraction in [0.0..1.0]. */
/* Note that the 1.0 endpoint is included. */
/* Since undercolor removal requires a signed frac, */
/* we limit fracs to 15 bits rather than 16. */
typedef short frac;
typedef short signed_frac;
#define frac_bits (arch_sizeof_short * 8 - 1)
#define frac_0 ((frac)0)
#define frac_1 ((frac)0x7fff)
#define frac_1_long ((long)frac_1)
#define frac_1_float ((float)frac_1)
/* Conversion between fracs and floats */
#define frac2float(fr) ((fr) / frac_1_float)
#define float2frac(fl) ((frac)(((fl) + 0.5 / frac_1_float) * frac_1_float))
/* Conversion between fracs and bytes representing fractions */
#define frac2byte(fr) ((byte)((fr) >> (frac_bits - 8)))
#define byte2frac(b) ((frac)(((uint)(b) << (frac_bits - 8)) + ((b) >> (16 - frac_bits))))

#endif					/* gxfrac_INCLUDED */
