/* Copyright (C) 1990 Aladdin Enterprises.  All rights reserved.

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

/* gxarith.h */
/* Arithmetic macros for Ghostscript library */

/* Define an in-line abs function, good for any signed numeric type. */
#define any_abs(x) ((x) < 0 ? -(x) : (x))

/* Test floating point values against zero. */
#if arch_floats_are_IEEE && (arch_sizeof_float == arch_sizeof_int || arch_sizeof_float == arch_sizeof_long)
#  if arch_sizeof_float == arch_sizeof_int
#    define _f_as_int(f) *(int *)(&(f))
#  else		/* arch_sizeof_float == arch_sizeof_long */
#    define _f_as_int(f) *(long *)(&(f))
#  endif
#  define is_fzero(f) ((_f_as_int(f) << 1) == 0)	/* +0 or -0 */
#  define is_fzero2(f1,f2) (((_f_as_int(f1) | _f_as_int(f2)) << 1) == 0)
#  define is_fneg(f) ((_f_as_int(f)) < 0)	/* -0 is negative, oh well */
#else		/* e.g. (i.e.?), VAX */
#  define is_fzero(f) ((f) == 0.0)
#  define is_fzero2(f1,f2) ((f1) == 0.0 && (f2) == 0.0)
#  define is_fneg(f) ((f) < 0.0)
#endif

/*
 * Define a macro for computing log2(n), where n=1,2,4,...,128.
 * Because some compilers limit the total size of a statement,
 * this macro must only mention n once.  The macro should really
 * only be used with compile-time constant arguments, but it will work
 * even if n is an expression computed at run-time.
 */
#define small_exact_log2(n)\
 ((uint)(05637042010L >> ((((n) % 11) - 1) * 3)) & 7)
