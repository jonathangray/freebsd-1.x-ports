#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "tree.h"
#include "ctlmod.h"
#include "pipes.h"
#include <symbol.h>
#include <tree.h>
#include <resp.h>
#include <ingres.h>
#include <aux.h>
#include <access.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)send_off.c	8.1	12/31/84)

/*
**  SEND_OFF -- send a call off to another process
**
**	This routine sends a call off to the process indicated
**	ppb->pb_proc.  It also sends parameters pc & pv.  It
**	just sends it, and does not wait for a response.
**
**	WARNING:
**		The pipe buffer is not flushed after putting out
**		the call; this must be done by the caller.
**
**	Parameters:
**		ppb -- a pointer to a pipe block which is used
**			as the I/O area, and also identifies
**			the target process.
**		pc -- the parameter count.
**		pv -- the parameter vector.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		11.0 - 11.7
*/

void
send_off(register pb_t *ppb, int pc, register paramv_t *pv)
{
	register int	i;
	resp_t		*rp;

#ifdef xCTR1
	if (tTf(11, 0)) {
		lprintf("send_off: ");
		pb_dump(ppb, FALSE);
	}
#endif

	/*
	**  Flush out catalog pages and standard output so that
	**	changes will show through.
	*/

	closecatalog(FALSE);
	fflush(stdout);
	BITCLR(PB_EOF, ppb->pb_stat);

	/*
	**  Cycle through the parameters writing each one out.
	*/
	rp = getresp();
	if (ppb->pb_type == PB_RESP) {
		pb_put((char *) rp, sizeof(resp_t), ppb);
	} else {
		for (i = 0; i < pc; i++) {
			send_arg(&pv[i], ppb);
		}
	}

	/* deallocate the space allocated to these parameters */
	freebuf(Qbuf, Ctx.ctx_pmark);
#ifdef xCTR1
	if (tTf(11, 1))
		lprintf("send_off: free %d\n", Ctx.ctx_pmark);
#endif
}

/*
**  SEND_ARG -- send argument down pipe.
**
**	Parameters:
**		pparm -- the parameter to send.
**		ppb -- the pipe to send it to.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
**
**	Called By:
**		send_off
**		do_seq
*/

void
send_arg(register paramv_t *pparm, register pb_t *ppb)
{
	register char	*p;

	switch (pparm->pv_type) {
	  case PV_STR:
		p = pparm->pv_val.pv_str;
		pb_tput(PV_STR, p, strlen(p) + 1, ppb);
		break;

	  case PV_INT:
		pb_tput(PV_INT, (char *) &pparm->pv_val.pv_int, sizeof(pparm->pv_val.pv_int), ppb);
		break;

	  case PV_QTREE:
		pb_tput(PV_QTREE, "", 0, ppb);
		writeqry((qtree_t *) pparm->pv_val.pv_qtree, pb_put, (char *)ppb);
		break;

	  case PV_TUPLE:
		pb_tput(PV_TUPLE, pparm->pv_val.pv_tuple, pparm->pv_len, ppb);
		break;

	  case PV_EOF:
		break;

	  default:
		syserr("send_arg: type %d", pparm->pv_type);
	}
}
