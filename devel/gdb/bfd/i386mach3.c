/* BFD back-end for i386 a.out binaries.
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This is for Mach 3, which uses a.out, not Mach-O.  */

/* There is no magic number or anything which lets us distinguish this target
   from i386aout or i386bsd.  So this target is only useful if it is the
   default target.  */

#define	PAGE_SIZE	4096
#define	SEGMENT_SIZE	PAGE_SIZE
#define TEXT_START_ADDR	0x10000
#define ARCH 32
#define BYTES_IN_WORD 4
#define N_HEADER_IN_TEXT(x)	1 		/* (N_MAGIG(x) == ZMAGIC) */

#define N_TXTSIZE(x)	((x).a_text)

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "aout/aout64.h"
#include "aout/stab_gnu.h"
#include "aout/ar.h"
#include "libaout.h"           /* BFD a.out internal data structures */

#define DEFAULT_ARCH bfd_arch_i386
#define MY(OP) CAT(i386mach3_,OP)
#define TARGETNAME "a.out-mach3"

#include "aout-target.h"
