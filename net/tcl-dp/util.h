/*
 * util.h
 *
 *	The Tcl-DP package was actually developed as part
 *	of a larger distributed continuous media project.
 *
 *	The files util.h and util.c hold some of the
 *	other parts of the larger project which Tcl-DP
 *	depended upon:
 *
 *		atexit	-- execute a command when "exit" is invoked.
 *		exit	-- execute all commands previously defined
 *			   with "atexit", then exit normally.
 *
 * Copyright 1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#ifndef _T_UTIL_H
#define _T_UTIL_H

#include "tcl.h"
#include "tclInt.h"

extern void 	utilInit 	_ANSI_ARGS_((Tcl_Interp *interp));
extern double 	ReadSysClock 	_ANSI_ARGS_(());

#endif


