#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "ctlmod.h"
#include <ingres.h>
#include <tree.h>
#include <aux.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)setp.c	8.1	12/31/84)

/*
**  SETP -- set parameter
**
**	Sets a parameter, to be later sent by 'call' to whomever.
**
**	Parameters:
**		type -- parameter type.
**			PV_STRING -- a string, 'len' is ignored.
**			PV_TUPLE -- a tuple of length 'len'.
**			PV_QTREE -- a query tree pointer, 'len'
**				is ignored.
**			PV_INT -- an integer, 'len' is ignored.
**		val -- the value (real value if PV_INT, pointer
**			otherwise).
**		len -- the length of the tuple in PV_TUPLE mode.
**
**	Returns:
**		none
**
**	Side Effects:
**		Adjusts Ctx.ctx_pc & Ctx.ctx_pv.
**
**	Trace Flags:
**		4.8 - 4.15
*/

void
setp(register int type, void *val, register int len)
{
	register paramv_t	*pp;
	register char	*newp;
	int		*ip;

	/*
	**  Check the magic bounds.
	*/

	pp = (paramv_t *) NULL;
	if (!Ctx.ctx_init) {
		syserr("setp: no initp");
	} else if (Ctx.ctx_pc >= PV_MAXPC) {
		syserr("setp: overflow");
	} else {
		pp = &Ctx.ctx_pv[Ctx.ctx_pc++];
	}

	/*
	**  Figure out the length from the type.
	*/

	switch (type) {
	  case PV_STR:
		len = strlen(val) + 1;
		newp = need(Qbuf, len);
		bmove(val, newp, len);
		pp->pv_val.pv_str = newp;
		break;
	
	  case PV_TUPLE:
		pp->pv_val.pv_tuple = (char *) val;
		break;
	
	  case PV_QTREE:
		len = sizeof(pp->pv_val.pv_qtree);
		pp->pv_val.pv_qtree = (qtree_t *) val;
		break;

	  case PV_INT:
		ip = (int *) val;
		pp->pv_val.pv_int = *ip;
		break;

	
	  default:
		syserr("setp: type %d", type);
	}

	/*
	**  Set up the parameter.
	*/

	pp->pv_type = type;
	pp->pv_len = len;

#ifdef xCTR1
	if (tTf(4, 8)) {
		lprintf("setp: ");
		pr_parm(pp);
	}
#endif
}
