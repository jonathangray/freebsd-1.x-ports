#include <stdio.h>
#include <errno.h>
#include <stdarg.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "sccs.h"

#define INGRES_CTLMOD
#include "protos.h"

#ifndef BELL
#define BELL	'\007'
#endif /* !BELL */

SCCSID(@(#)syserr.c	8.1	12/31/84)

/*
**  SYSERR -- SYStem ERRor message print and abort
**
**	Syserr acts like a printf with up to five arguments.
**
**	If the first argument to syserr is not zero,
**	the message "SYSERR:" is prepended.
**
**	If the extern variable `Proc_name' is assigned to a
**	string, that string is prepended to the message.
**
**	All arguments must be null-terminated.
**
**	The function pointed to by `ExitFn' is then called.
**	It is initialized to be `exit'.
*/

int	Accerror;
void	(*ExitFn)()	 = exit;

void
syserr(char *fmt, ...)
{
	register int	usererr;
	register int	exitvalue;
	va_list		vp;
	char		*cp;

	printf("\n");
	usererr = (fmt == (char *) NULL);

	if (!usererr) {
		if ((cp = getprocname()) != (char *) NULL)
			printf("%s ", cp);
		noise(1);
		printf("SYSERR: ");
	}
	va_start(vp, fmt);
	vfprintf(stdout, fmt, vp);
	va_end(vp);
	noise(1);
	exitvalue = -1;
	if (!usererr) {
		if (errno) {
			exitvalue = errno;
			ingres_perror("UNIX error");
		}
		if (Accerror != 0) {
			printf("\taccess method error %d\n", Accerror);
		}
	}
	fflush(stdout);
	if (ExitFn == exit)
		ExitFn = abort;
	(*ExitFn)(exitvalue);
}

void
noise(int n)
{
	if (getenv("NOISE") != (char *) NULL) {
		while (n-- > 0) {
			(void) fputc(BELL, stdout);
		}
	}
}
