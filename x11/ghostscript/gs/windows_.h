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

/* windows_.h */
/* Ghostscript substitute for windows.h */

#define STRICT
#include <windows.h>

#ifndef __WATCOMC__
	/* Define null equivalents of the Watcom 32-to-16-bit glue. */
#  define AllocAlias16(ptr) ((DWORD)(ptr))
#  define FreeAlias16(dword) /* */
#  define MK_FP16(fp32) ((DWORD)(fp32))
#  define MK_FP32(fp16) (fp16)
#  define GetProc16(proc, ptype) (proc)
#  define ReleaseProc16(cbp) /* */
#endif
