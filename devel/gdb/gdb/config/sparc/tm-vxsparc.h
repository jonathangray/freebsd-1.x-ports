/* Target machine description for VxWorks sparc's, for GDB, the GNU debugger.
   Copyright 1993 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

This file is part of GDB.

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

#define	GDBINIT_FILENAME	".vxgdbinit"

#define	DEFAULT_PROMPT		"(vxgdb) "

#include "sparc/tm-spc-em.h"

/* FIXME: These are almost certainly wrong. */

/* Number of registers in a ptrace_getregs call. */

#define VX_NUM_REGS (NUM_REGS)

/* Number of registers in a ptrace_getfpregs call. */

/* #define VX_SIZE_FPREGS (don't know how many) */

