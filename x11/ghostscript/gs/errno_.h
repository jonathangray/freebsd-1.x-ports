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

/* errno_.h */
/* Generic substitute for Unix errno.h */

/* We must include std.h before any file that includes sys/types.h. */
#include "std.h"

/* All environments provide errno.h, but in some of them, errno.h */
/* only defines the error numbers, and doesn't declare errno. */
#include <errno.h>
#ifndef errno		/* in case it was #defined (very implausible!) */
extern int errno;
#endif
