#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "monitor.h"
#include "tree.h"
#include <func.h>
#include <pipes.h>
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)syscntrl.c	8.1	12/31/84)



/*
**  TRACE -- set/clear trace information dynamically.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sends the rest of the input line to all processes
**		and calls tTamper on the rest of the input line.
**
**	Trace Flags:
**		32
*/
void
trace(void)
{
	pb_t	pb;
	char	buf[120];

	/* get rest of trace command */
	pb_prime(&pb, PB_TRACE);
	pb.pb_proc = PB_WILD;
	if (fgets(buf, sizeof(buf), Input) == NULL)
		syserr("syscntrl: bad read");
	Prompt = TRUE;
	tTamper(buf, FuncVect[0]->fn_tflag, FuncVect[0]->fn_tvect, FuncVect[0]->fn_tsize);
	pb_put(buf, strlen(buf), &pb);

	pb.pb_stat |= PB_INFO;
	pb_flush(&pb);
}

/*
**  RESET -- do a system reset.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sends a reset block to all processes
**		and calls cm_reset in the monitor.
*/
void
reset(void)
{
	pb_t 	pb;

	pb_prime(&pb, PB_RESET);
	pb.pb_proc = PB_WILD;
	pb.pb_stat |= PB_INFO;
	pb_flush(&pb);
	cm_reset();
}
