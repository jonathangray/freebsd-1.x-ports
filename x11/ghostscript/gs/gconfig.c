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

/* gconfig.c */
/* Installed device table for Ghostscript */
#include "ghost.h"
/*
 * Since we only declare variables of type gx_device *,
 * it should be sufficient to define struct gx_device_s as
 * an abstract (undefined) structure.  However, the VAX VMS compiler
 * isn't happy with this, so we have to include the full definition.
 */
#include "gxdevice.h"

/* Declare the devices as extern. */
#define device_(dev) extern far_data gx_device dev;
#include "gconfig.h"
#undef device_

/* Set up the device table. */
#define device_(dev) &dev,
gx_device *gx_device_list[] = {
#include "gconfig.h"
	0
};
#undef device_

/* Set up the .ps file name string array. */
/* We fill in the lengths at initialization time. */
#define ref_(t) struct { struct tas_s tas; t value; }
#define string_(s)\
 { {(t_string<<r_type_shift)|a_readonly, 0}, s },
#define psfile_(fns) string_(fns)
ref_(const char *) gs_init_file_array[] = {
#include "gconfig.h"
	string_(0)
};
#undef psfile_

/* Here is where the library search path and the name of the */
/* initialization file are defined.  We supply defaults just in case */
/* someone compiles the file without the proper command line flags. */
#ifndef GS_LIB_DEFAULT
#  define GS_LIB_DEFAULT ""
#endif
#ifndef GS_INIT
#  define GS_INIT "gs_init.ps"
#endif
const char *gs_lib_default_path = GS_LIB_DEFAULT;
const char *gs_init_file = GS_INIT;

/* Some C compilers insist on executable code here, so.... */
void
gconfig_dummy(void)
{
}
