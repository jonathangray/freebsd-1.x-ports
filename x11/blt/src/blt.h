/*
 * blt.h --
 *
 * Copyright 1993-1994 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

#ifndef _BLT_H
#define _BLT_H

#include "bltConfig.h"

#include <tcl.h>
#include <tk.h>
#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif /* HAVE_ERRNO_H */

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif /* HAVE_MEMORY_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif /* HAVE_MALLOC_H */

#include "bltList.h"

#ifdef hpux
#define BLACK		"black"
#define WHITE		"white"
#define BISQUE1		"bisque1"
#define BISQUE2		"bisque2"
#define BISQUE3     	"bisque3"
#define ANTIQUEWHITE1   "antiquewhite1"
#define GRAY		"grey"
#else
#define BLACK		"#000000"
#define WHITE		"#ffffff"
#define BISQUE1		"#ffe4c4"
#define BISQUE2		"#eed5b7"
#define BISQUE3     	"#cdb79e"
#define ANTIQUEWHITE1	"#ffefdb"
#define GRAY		"#b0b0b0"
#endif

/*
 * ----------------------------------------------------------------------
 *
 * 	X11/Xosdefs.h requires XNOSTDHDRS be set for some systems.
 *	This is a guess.  If I can't find STDC headers or unistd.h,
 *	assume that this is non-POSIX and non-STDC environment.
 *	(needed for Encore Umax 3.4 ?)
 *
 * ----------------------------------------------------------------------
 */
#if !defined(STDC_HEADERS) && !defined(HAVE_UNISTD_H)
#define XNOSTDHDRS 	1
#endif

/*
 * ----------------------------------------------------------------------
 *
 * 	The TCL_DYNAMIC macro requires "free" to be declared.
 *	Assume we need one if there's no stdlib.h or malloc.h
 *
 * ----------------------------------------------------------------------
 */
#if !defined(HAVE_STDLIB_H) && !defined(HAVE_MALLOC_H)
extern void free _ANSI_ARGS_((void *));
#endif

/*
 * ----------------------------------------------------------------------
 *
 *	If strerror isn't in the C library (we'll get it from
 *	libtcl.a) that we also need a forward declaration.
 *
 * ----------------------------------------------------------------------
 */
#ifndef HAVE_STRERROR
extern char *strerror _ANSI_ARGS_((int));
#endif
#ifndef HAVE_STRDUP
extern char *strdup _ANSI_ARGS_((CONST char *s));
#endif

/* Forward declarations */

extern int Blt_FindCmd _ANSI_ARGS_((Tcl_Interp *, char *, ClientData *));

#endif /*_BLT_H*/
