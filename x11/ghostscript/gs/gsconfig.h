/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gsconfig.h */
/* Prologue for gconfig.h */

/*
 * The Ghostscript makefile generates the file gconfig.h, which consists of
 * lines of the form
 *	device__(gs_xxx_device)
 * for each installed device;
 *	file_device__(gs_fdev_xxx)
 * for each available file device (e.g., pipe);
 *	oper__(xxx_op_defs)
 * for each Level 1 operator option;
 *	oper2__(xxx_op_defs)
 * for each Level 2 operator option;
 *	psfile__("gs_xxxx.ps")
 * for each optional initialization file.
 *
 * We include this file multiple times to generate various different
 * source structures.  (It's a hack, but we haven't come up with anything
 * more satisfactory.)
 */

/* Handle undefined initializer types. */
#undef device__
#ifdef device_
#  define device__(dev) device_(dev)
#else
#  define device__(dev) /* */
#endif
#undef file_device__
#ifdef file_device_
#  define file_device__(fdev) file_device_(fdev)
#else
#  define file_device__(fdev) /* */
#endif
#undef oper__
#ifdef oper_
#  define oper__(defs) oper_(defs)
#else
#  define oper__(defs) /* */
#endif
#undef oper2__
#ifdef oper2_
#  define oper2__(defs) oper2_(defs)
#else
#  define oper2__(defs) /* */
#endif
#undef psfile__
#ifdef psfile_
#  define psfile__(fname) psfile_(fname)
#else
#  define psfile__(fname) /* */
#endif
