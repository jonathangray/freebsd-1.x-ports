#include "tree.h"
#include "ctlmod.h"
#include "pipes.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)pb_read.c	8.1	12/31/84)

/*
**  PB_READ -- read a pipe block
**
**	This routine reads a pipe block into *ppb.  It also handles
**	all the low level protocol: RESET blocks, SYNC blocks, META
**	blocks, blocks intended for another process, etc.
**
**	When this routine returns, it returns with a block intended
**	for this process, which must be a REGULAR block, a RESPONSE
**	block, or an ERROR block.
**
**	Parameters:
**		ppb -- a pointer to the area which wants the block.
**
**	Returns:
**		none
**
**	Side Effects:
**		ppb is set to the named block.
**		Virtually any amount of stuff can happen, depending
**			on what is coming through the pipe.
**
**	Trace Flags:
**		12.4 - 12.9
*/
void
pb_read(register pb_t *ppb)
{
	register int	type;
	register int	from;
	extern void	(*ExitFn)();

	/*
	**  Top Loop.
	**	Hang waiting for a normal block.  Other blocks are
	**		handled inside the loop.
	**	We multiplex 'from' in here temporarily.
	*/

	for (;;) {
#ifdef xCTR1
		if (tTf(12, 4))
			lprintf("pb_read: fd=%d:\n", Cm.cm_input);
#endif
		from = pb_rphys(ppb, Cm.cm_input);
		if (from == 0) {
#ifdef xCTR1
			if (tTf(12, 4))
				lprintf("EOF\n");
#endif
			(*ExitFn)(0);
		} else if (from != PB_IOSIZE)
			syserr("pb_read: read error (%d)", from);
#ifdef xCTR1
		if (tTf(12, 4))
			pb_dump(ppb, TRUE);
#endif

		/* set up buffer pointers, etc. */
		ppb->pb_xptr = ppb->pb_data;
		ppb->pb_nleft = ppb->pb_nused;
		type = ppb->pb_type;
		from = ppb->pb_from;
#ifdef xCM_DEBUG
		if (from > CM_MAXPROC || from < 0)
			syserr("pb_read: from %d", from);
#endif

		/* mark this block if possibly from the front */
		if (Cm.cm_input == Cm.cm_proc[0].pr_ninput &&
		    BITISSET(PR_RADJCT, Cm.cm_proc[0].pr_stat))
			BITSET(PB_FRFR, ppb->pb_stat);

		/* handle RESET blocks before anything else */
		if (type == PB_RESET) {
			cm_reset();
			continue;
		}

		/* do sync block processing */
		if (type == PB_SYNC) {
/*
			if (Syncs[from] <= 0)
				lprintf("pb_read: bad SYNC block from %d", from);
			else
*/
				Syncs[from]--;
			continue;
		}

		/* see if we are ignoring from this process */
		if (Syncs[from] > 0)
			continue;

		/* check for re-routing */
		if (ppb->pb_proc != Cm.cm_myproc) {
			pb_write(ppb);
			if (ppb->pb_proc != PB_WILD)
				continue;
		}

		/*
		**  Block type dispatching.
		**	Regular, response, and error blocks return.
		**	Meta blocks are handled by calling other
		**		routines.
		*/

		switch (type) {
		  case PB_REG:
		  case PB_RESP:
		  case PB_ERR:
			/* handled by readinput() */
			return;

		  case PB_TRACE:
			pb_trace(ppb);
			break;

		  /* insert more meta handling before this line */

		  default:
			syserr("pb_read: type %d", type);
		}
	}
}

/*
**  PB_TRACE -- handle dynamic trace information
**
**	Parameters:
**		ppb -- a pipe block from which to get information.
**
**	Returns:
**		none.
**
**	Side Effects:
**		trace vectors will be changed.
*/
void
pb_trace(pb_t *ppb)
{
	register int		i;
	register func_t	*f;

	for (i = 0; i < NumFunc; i++) {
		f = FuncVect[i];
		if (f->fn_tflag != '\0')
			tTamper(ppb->pb_data, f->fn_tflag, f->fn_tvect, f->fn_tsize);
	}
}
