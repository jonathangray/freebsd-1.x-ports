#include <stdio.h>

#include "tree.h"
#include "ctlmod.h"
#include <resp.h>
#include "sccs.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)call_fn.c	8.1	12/31/84)

/*
**  CALL_FN -- call a local function
**
**	This routine, given a pointer to a local function descriptor,
**	calls the local function.
**
**	Parameters:
**		fno -- function definition vector number.
**		pc -- the parameter count
**		pv -- the parameter vector, gets passed to the
**			function.
**
**	Returns:
**		none
**
**	Side Effects:
**		Sets 'Resp' to the response vector for this function.
**		The old 'Resp' is completely obliterated.
**
**	Trace Flags:
**		3
*/
#ifdef xMONITOR
monitor_t	MonBuf[CM_MAXST];
#endif /* xMONITOR */

void
call_fn(int fno, int pc, paramv_t *pv)
{
	register func_t		*f;
	register char		*gp;
	register int		i;
#ifdef xMONITOR
	extern monitor_t	CmMonBuf;
	monitor_t		mon;
	monitor_t		*savemon;
#endif
	short			*tvect;
	char			*oldname;
	resp_t			*rp;
	extern short		*tT;

	rp = getresp();
	f = FuncVect[fno];
	if (fno > NumFunc || f->fn_fn == NULL || fno < 0)
		syserr("call_fn: undef fn %d", fno);
	Ctx.ctx_fn = f;
#ifdef xCTR1
	if (tTf(3, 0))
		lprintf("call_fn: fn %d (%s)\n", fno, f->fn_name);
#endif

	/*
	**  Save function globals.
	**	If the function we want to call is already active,
	**	and if it has a global data area, allocate space
	**	and save that area.
	*/

	if (f->fn_active > 0 && f->fn_gptr != NULL) {
		/* save globals */
		gp = xalloc(f->fn_gsize, 0, 1);
		bmove(f->fn_gptr, gp, f->fn_gsize);
		Ctx.ctx_glob = gp;
	} else {
		Ctx.ctx_glob = gp = NULL;
	}

	/*
	**  Clear the response vector to a known state and call
	**  the function.
	*/

	oldname = getprocname();
	setprocname(Ctx.ctx_name = f->fn_name);
	tvect = tT;
	Ctx.ctx_tvect = tT = f->fn_tvect;
	clrmem(rp, sizeof(resp_t));
	rp->resp_tups = -1;
	markopen(&Ctx.ctx_ofiles);
#ifdef xCTR2
	if (tTf(3, 1)) {
		lprintf("call_fn: calling %s\n", getprocname());
		prvect(pc, pv);
	}
#endif
#ifdef xCTR3
	if (tTf(3, 2)) {
		lprintf("call_fn: Ctx.ctx_ppb ");
		pb_dump(Ctx.ctx_ppb, FALSE);
	}
#endif /* xCTR3 */
#ifdef xMONITOR
	savemon = Ctx.ctx_mon;
	Ctx.ctx_mon = &mon;
	clrmem(&mon, sizeof(mon));
	markperf(&mon);
#endif /* xMONITOR */

	i = (*f->fn_fn)(pc, pv);

#ifdef xMONITOR
	markperf(&CmMonBuf);
	Ctx.ctx_mon = savemon;
	if (savemon != NULL)
		add_mon(&mon, savemon);
	add_mon(&mon, &MonBuf[(int) Ctx.ctx_ppb->pb_st]);
#endif /* xMONITOR */
#ifdef xCTR1
	if (tTf(3, 3))
		lprintf("call_fn: returns %d\n", i);
#endif
#ifdef xMONITOR
	if (tTf(0, 0))
		printf("CPU time for %s = %s sec\n",
				getprocname(),
				cvt_time(mon.mon_utime + mon.mon_stime));
#endif /* xMONITOR */
	rp->resp_resp = i;
	closeall(TRUE, Ctx.ctx_ofiles);

	/*
	**  Restore old global memory, if there was any.
	*/

	if (gp != NULL) {
		bmove(gp, f->fn_gptr, f->fn_gsize);
		Ctx.ctx_glob = NULL;
		xfree(gp);
	}
	setprocname(Ctx.ctx_name = oldname);
	Ctx.ctx_tvect = tT = tvect;
}
