/*
 * dirent.h --
 *
 *	This file is a replacement for <dirent.h> in systems that
 *	support the old BSD-style <sys/dir.h> with a "struct direct".
 *
 * Copyright 1991 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * $Header: /a/cvs/386BSD/ports/lang/tcl/compat/dirent.h,v 1.1 1993/08/09 10:57:35 alm Exp $ SPRITE (Berkeley)
 */

#ifndef _DIRENT
#define _DIRENT

#include <sys/dir.h>

#define dirent direct

#endif /* _DIRENT */
