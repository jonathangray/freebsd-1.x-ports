#include <stdio.h>
#include <stdarg.h>

#include <useful.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)lprintf.c	8.1	12/31/84)

static char	*Proc_name;

void
setprocname(char *s)
{
	Proc_name = s;
}

char *
getprocname(void)
{
	return(Proc_name);
}

/*
**  LPRINTF -- labelled printf
**
**	Just like printf, except outputs the process name first.
**
**	Parameters:
**		fmt -- the format.
**		p1 - p6 -- the parameters.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		none
*/

void
lprintf(char *fmt, ...)
{
	va_list		vp;
	char		*cp;

	if ((cp = getprocname()) != (char *) NULL) {
		printf("%s: ", cp);
	}
	va_start(vp, fmt);
	vfprintf(stdout, fmt, vp);
	va_end(vp);
}
