#include <stdio.h>
#include <stdarg.h>
#include <errno.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)IIsyserr.c	8.1	12/31/84)


/*
**  SYSERR -- SYStem ERRor message print and abort
**
**	Syserr acts like a printf with up to five arguments.
**	[changed to use stdargs - agc]
**
**	If the first argument to syserr is not zero,
**	the message "SYSERR:" is prepended.
**
**	If the extern variable `IIproc_name' is assigned to a
**	string, that string is prepended to the message.
**
**	All arguments must be null-terminated.
**
**	The function pointed to by `Exitfn' is then called.
**	It is initialized to be `exit'.
*/

extern char	*IIproc_name;

void
IIsyserr(char *fmt, ...)
{
	register int	usererr;
	va_list		vp;

	va_start(vp, fmt);
	printf("\n");
	usererr = (fmt == (char *) NULL);

	if (!usererr) {
		if (IIproc_name) {
			printf("%s ", IIproc_name);
		}
		noise(1);
		printf("SYSERR: ");
	}
	vfprintf(stdout, fmt, vp);
	va_end(vp);
	noise(1);
	if (!usererr && errno) {
		printf("\tsystem error %d\n", errno);
	}
	(void) fflush(stdout);
	exit(1);
}
