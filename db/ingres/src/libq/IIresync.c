#include <ingres.h>
#include "IIglobals.h"
#include <signal.h>
#include "sccs.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "protos.h"

SCCSID(@(#)IIresync.c	8.1	12/31/84)


/*
**  RESYNCHRONIZE PIPES AFTER AN INTERRUPT
**
**	The pipes are all cleared out.  This routines must be called
**	by all processes in the system simultaneously.  It should be
**	called from the interrupt catching routine.
*/

int	IISyncs[CM_MAXPROC];
void	(*IIinterrupt)() =	exit;

RETSIGTYPE
IIresync(int n)
{
	register int	i;
	pb_t		pb;

	(void) signal(SIGINT,SIG_IGN);

	/*
	**  Send SYNC blocks to all processes that are adjacent
	**	in the write direction.
	**  Arrange to ignore blocks from all processes that
	**	are adjacent in the read direction.
	*/

	IIpb_prime(&pb, PB_SYNC);
	for (i = 0; i < CM_MAXPROC; i++) {
		IISyncs[i]++;

		/* send SYNC to parser */
		pb.pb_proc = 1;
		IIpb_write(&pb);
	}

	/* ovqp buffer flush is done in IIsetup() */

	/* Get out of a retrieve and clear errors */
	IIin_retrieve = 0;
	IIerrflag = 0;
	IIndomains = IIdomains = 0;
	IInewqry = 0;


	/* reset the signal */
	signal(SIGINT, IIresync);
	/* allow the user to service the interrupt */
	(*IIinterrupt)(-1);
	/*
	** If IIinterupt returns the user might hang in a retrieve
	*/

	IIsyserr("Interupt returned");
}
