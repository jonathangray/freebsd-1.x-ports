#include <stdio.h>

#include <pv.h>
#include <ingres.h>
#include <func.h>
#include "sccs.h"

#define INGRES_IUTIL
#include "protos.h"

SCCSID(@(#)rupdate.c	8.1	12/31/84)

extern	short	tTdbu[];

func_t RupdatFn = {
	"RUPDATE",
	rupdate,
	null_fn,		/* initialization function */
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};
/*
**  RUBOUT SETUP FOR DEFFERED UPDATE PROCESSOR
**
**	These routines setup the special processing for the rubout
**	signal for the deferred update processor.  The update
**	processor is then called.
*/
int
rupdate(int pc, paramv_t *pv)
{
	register int	rtval;

	/* set up special signal processing */
	ruboff("batch update");

	/* call update */
	rtval = update(pc, pv);

	/* clean up signals */
	rubon();

	return (rtval);

}
