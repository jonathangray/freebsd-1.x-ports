#include <stdio.h>
#include <stdarg.h>

#include "tree.h"
#include "ctlmod.h"
#include "pipes.h"
#include <pv.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)error.c	8.1	12/31/84)

/*
**  ERROR -- Ingres error message generator
**
**	Error message `num' is sent up towards the caller with param-
**	eters `msg'.  This routine may have any number of parameters,
**	but the last one must be zero.
**
**	Parameters:
**		num -- the error number.
**		msg -- the first in a null-terminated list of
**			arguments, all of type char *, which are
**			passed as arguments to the error.
**
**	Returns:
**		non-locally
**
**	Side Effects:
**		Many and vast.  The message gets passed up the
**		stack of activations.  The activation stack gets
**		popped.  Etc.
**
**	Trace Flags:
**		6.0 - 6.7
*/

/*VARARGS2*/
int
error(int num, ...)
{
	pb_t		pb;
	typedef int	ftype();
	char		*msg;
	va_list		vp;

	va_start(vp, num);
#ifdef xCTR1
	if (tTf(6, 0))
		lprintf("error: %d, Ctx.ctx_cmark %d\n", num, Ctx.ctx_cmark);
#endif

	/* flush the current input pipe */
	while (!BITISSET(PB_EOF, Ctx.ctx_ppb->pb_stat))
		pb_read(Ctx.ctx_ppb);

	/* free up some stack space (in case called from need) */
	if (num == -ERR_QBUF) {
		freebuf(Qbuf, Ctx.ctx_cmark);
		Ctx.ctx_pmark = Ctx.ctx_cmark;
		Ctx.ctx_new = TRUE;
	}

	/* create an error context & set the message parameters */
	initp();
	setp(PV_INT, &num, sizeof(num));
	for (;;) {
		msg = va_arg(vp, char *);
		if (msg == (char *) NULL) {
			break;
		}
		setp(PV_STR, msg, 0);
	}
	va_end(vp);

	/* send it to my caller */
	pb_prime(&pb, PB_ERR);
	call_setup(&pb, PB_NONE, (ftype *) NULL);

	/* send the message to the correct place & unwind the stack */
	proc_err(&pb, Ctx.ctx_pc, Ctx.ctx_pv);
	syserr("error: proc_err");
	return(num);
}

/*
**  NFERROR -- non-fatal error.
**
**	Errors of this type are passed directly to the front end.
**
**	Parameters:
**		(same as error)
**
**	Returns:
**		The error number.
**
**	Side Effects:
**		The message is sent off to the front end.  It
**		is marked as being informational only.
*/

int
nferror(int num, ...)
{
	char		*msg;
	pb_t		pb;
	va_list		vp;
	typedef int	ftype();

	va_start(vp, num);
	initp();
	setp(PV_INT, &num, sizeof(num));
	for (;;) {
		if ((msg = va_arg(vp, char *)) == (char *) NULL) {
			break;
		}
		setp(PV_STR, msg, 0);
	}
	pb_prime(&pb, PB_ERR);
	call_setup(&pb, PB_NONE, (ftype *) NULL);
	pb.pb_stat |= PB_INFO;
	pb.pb_proc = PB_FRONT;
	send_off(&pb, Ctx.ctx_pc, Ctx.ctx_pv);
	pb_flush(&pb);
	resetp();
	return (num);
}
