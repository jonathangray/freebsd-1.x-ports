#include <sys/types.h>

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "monitor.h"
#include <trace.h>
#include <resp.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)trapquery.c	8.1	12/31/84)




/*
**  TRAPQUERY -- trap queries which succeeded.
**
**	This module traps the current query into the file
**	specified by Trapfile.  It will open Trapfile if
**	it is not already open.
**
**	Parameters:
**		retcode -- the return code of the query.
**		name -- the name of the file in which to dump the
**			query buffer.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Outputs the query buffer to Trapfile.
**
**	Called By:
**		go.
**
**	Files:
**		Trapfile -- the descriptor of the file in which to
**			trap the query.
**		Qbname -- the name of the query buffer.
**
**	Trace Flags:
**		none
**
**	Bugs:
**		If 'name' (that is, the {querytrap} macro) changes
**			during a run, the output file does not change.
*/

FILE	*Trapfile;

void
trapquery(struct resp *resp, char *name)
{
	register FILE	*iop;
	static int	first;
	register char	*sp, c;
	time_t		timevec;

	if (first < 0)
		return;
	if (Trapfile == NULL) {
		if ((Trapfile = fopen(name, "a")) == NULL) {
			printf("can't trap query in %s\n", name);
			first = -1;
			return;
		}
	}
	if (first == 0) {
		(void) time(&timevec);
		sp = ctime(&timevec);
		while (*sp)
			putc(*sp++, Trapfile);
		first++;
	}

	if ((iop = fopen(Qbname, "r")) == NULL)
		syserr("go: open 1");
	macinit(fgetc, iop, 1);

	while ((c = macgetch()) > 0)
		putc(c, Trapfile);

	if (resp->resp_resp == 0) {
		sp = (char *) locv(resp->resp_tups);
		while (*sp)
			putc(*sp++, Trapfile);
		putc('\n', Trapfile);
	}

	fclose(iop);
}
