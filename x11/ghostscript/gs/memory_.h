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

/* memory_.h */
/* Generic substitute for Unix memory.h */

/* We must include std.h before any file that includes sys/types.h. */
#include "std.h"

/****** Note: the System V bcmp routine only returns zero or non-zero, ******/
/****** unlike memcmp which returns -1, 0, or 1. ******/

#ifdef __TURBOC__
/* The Turbo C implementation of memset swaps the arguments and calls */
/* the non-standard routine setmem.  We may as well do it in advance. */
#  undef memset				/* just in case */
#  include <mem.h>
#  define memset(dest,chr,cnt) setmem(dest,cnt,chr)
#else
#  ifdef VMS
	/* Apparently the newer VMS compilers include prototypes */
	/* for the mem... routines in <string.h>. */
#    include <string.h>
#  else
#    if defined(BSD4_2) || defined(UTEK)
	extern bcopy(), bcmp(), bzero();
#       define memcpy(dest,src,len) bcopy(src,dest,len)
#       define memcmp(b1,b2,len) bcmp(b1,b2,len)
	/* Define our own versions of missing routines (in gsmisc.c). */
#       define memory__need_memset
	extern void memset(P3(void *, char, unsigned));
#       if defined(UTEK)
#         define memory__need_memchr
	  extern const char *memchr(P3(const void *, char, unsigned));
#       endif
#    else				/* !BSD4_2 */
#      if defined(_POSIX_SOURCE) || defined(_HPUX_SOURCE) || defined(__WATCOMC__) || defined(THINK_C)
#        include <string.h>
#      else
#        include <memory.h>
#      endif				/* !_POSIX_SOURCE, ... */
#    endif				/* !BSD4_2, ... */
#  endif				/* !VMS */
#endif					/* !__TURBOC__ */

/* memflip8x8 transposes an 8 x 8 block of bits. */
/* line_size is the raster of the input data; */
/* dist is the distance between output bytes. */
/* Dot matrix printers need this.  The C code is in gsmisc.c. */
#ifdef __PROTOTYPES__
extern void memflip8x8(const byte *inp, int line_size, byte *outp, int dist);
#else
extern void memflip8x8();
#endif
