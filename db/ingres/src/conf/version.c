#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gconst.h"
#include "version.h"
#include "sccs.h"

SCCSID(@(#)version.c	8.9	6/12/88)

/*
**  VERSION.C -- define the current version
**
**	This just factors out the current version into one file
**	to make releases easier.  It is printed by the terminal
**	monitor, but should probably be loaded with everything
**	to guarantee a stamp in every file.
**
**	This file defines the system version identification.
*/
#ifndef SysIdent
#define SysIdent	"Ingres version %s.%s (4.22 - agc@uts.amdahl.com)"
#endif

char *
getsysident(void)
{
	static char	ident[MAX_LINE_SIZE];

	if (ident[0] == 0) {
		(void) sprintf(ident, SysIdent, MAJOR_VERSION, MINOR_VERSION);
	}
	return(ident);
}
