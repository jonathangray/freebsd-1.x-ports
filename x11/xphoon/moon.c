/* Copyright (C) 1988, 1991 by Jef Poskanzer and Craig Leres.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#ifndef lint
static char rcsid[] =
    "@(#) $Header: /a/cvs/386BSD/ports/x11/xphoon/moon.c,v 1.1 1994/05/18 14:07:12 asami Exp $";
#endif

#include <X11/Xlib.h>
#include <moon.xbm>

void
getbits( wP, hP, moonP, cxP, cyP, rP )
	int* wP;
	int* hP;
	char** moonP;
	int* cxP;
	int* cyP;
	int* rP;
    {
    *wP = moon_width;
    *hP = moon_height;
    *moonP = moon_bits;
    *cxP = moon_width / 2;
    *cyP = moon_height / 2;
    *rP = *cxP - 1;
    }
