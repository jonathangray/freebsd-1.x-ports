#include <ingres.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIpb_write.c	8.1	12/31/84)


/*
**  IIPB_WRITE -- Write pipe block
**
**	Writes the block specified by ppb down to the PARSER
**
**	Parameters:
**		ppb -- a ptr to the pipe block to write.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
*/
void
IIpb_write(register pb_t *ppb)
{
	register int	i;

	/* mark the block as coming from this process */
	ppb->pb_from = PB_FRONT;

	/* normal message */
	i = ppb->pb_proc;
	IIpb_wphys(ppb, IIw_down);


	/* reset some exciting pointers */
	ppb->pb_xptr = ppb->pb_data;
	ppb->pb_nleft = PB_DBSIZE;
	ppb->pb_nused = 0;
}
