#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "sccs.h"

SCCSID(@(#)perror.c	8.1	12/31/84)

#ifndef HAVE_STRERROR
#ifndef DECLARED_SYS_NERR
extern int	sys_nerr;
#endif

#ifndef DECLARED_SYS_ERRLIST
extern char	*sys_errlist[];
#endif

char *
strerror(int errnum)
{
	return((errnum <= 0 || errnum >= sys_nerr) ?
			"Unknown error" :
			sys_errlist[errnum]);
}
#endif /* !HAVE_STERROR */

/*
 * Print the error indicated
 * in the cerror cell.
 * ----
 * this code stolen from the system perror, the only change is that we print
 * on 1, instead of 2 (which is usally closed).
 */

int	errno;

void
ingres_perror(char *s)
{
	register char *cp;
	register n;

	cp = strerror(errno);
	if ((n = strlen(s)) > 0) {
		write(1, s, n);
		write(1, ": ", 2);
	}
	write(1, cp, strlen(cp));
	write(1, "\n", 1);
}
