#include <useful.h>
#include <pipes.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIpb_put.c	8.1	12/31/84)


/*
**  IIPB_PUT -- buffered put on pipe
**
**	This routine puts the named data out onto the pipe
**	determined by ppb->pb_proc.
**
**	Parameters:
**		dp -- a pointer to the data to write.
**		len -- the length of the data to write.
**		ppb -- a pointer to the pipe block.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		18.8 - 18.15
*/
void
IIpb_put(register char *dp, register int len, register pb_t *ppb)
{
	register int	i;


	/*
	**  Top loop.
	**	Loop until we have run out of things to write.
	*/

	while (len > 0) {
		/* compute the length to move */
		i = min(ppb->pb_nleft, len);

		/* move data into buffer and adjust ptrs & counts */
		IIbmove(dp, ppb->pb_xptr, i);
		dp += i;
		len -= i;
		ppb->pb_xptr += i;
		ppb->pb_nleft -= i;
		ppb->pb_nused += i;

		/* flush block if full */
		if (ppb->pb_nleft <= 0)
			IIpb_write(ppb);
	}
}
