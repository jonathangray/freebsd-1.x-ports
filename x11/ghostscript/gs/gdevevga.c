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

/* gdevpcfb.c */
/* IBM PC EGA and VGA display drivers for Ghostscript */
/* This is fundamentally an EGA driver with some parameters */
/* that allow it to drive larger displays. */
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gdevpcfb.h"

/* ------ Internal routines ------ */

/* We can't catch signals.... */
void
ega_set_signals(gx_device *dev)
{
}

/* Read the device mode */
int
ega_get_mode(void)
{	registers regs;
	regs.h.ah = 0xf;
	int86(0x10, &regs, &regs);
	return regs.h.al;
}

/* Set the device mode */
void
ega_set_mode(int mode)
{	registers regs;
	regs.h.ah = 0;
	regs.h.al = mode;
	int86(0x10, &regs, &regs);
}
