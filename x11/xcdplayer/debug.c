/*
 * Copyright (C) 1990 Regents of the University of California.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of the University of
 * California not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  the University of California makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 */

# include <X11/Intrinsic.h>

# include <varargs.h>
# include <stdio.h>

# include "debug.h"

/* VARARGS */
void
debug_printf(va_alist)
	va_dcl
{
	va_list		args;
	char		*fmt;

	va_start(args);

	/*
	 * first arg is whether or not to print.
	 */
	if ((va_arg(args, int) == 0) || (debug == False))
		return;

	fmt = va_arg(args, char *);
	vfprintf(stdout, fmt, args);

	fflush(stdout);

	va_end(args);
}
