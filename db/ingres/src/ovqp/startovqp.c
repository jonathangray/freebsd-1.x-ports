#include <ingres.h>
#include <symbol.h>
#include <tree.h>
#include <aux.h>
#include "../decomp/globs.h"
#include "../ctlmod/pipes.h"
#include <signal.h>
#include "sccs.h"
#include <errors.h>
#include <math.h>
#include <errno.h>

#define INGRES_DECOMP
#include "protos.h"

SCCSID(@(#)startovqp.c	8.3	1/31/86)

/*
**	Give a user error for a floating point exceptions
*/
RETSIGTYPE
flptexcep(int n)
{
	ov_err(FLOATEXCEP);
}

/*
**	startovqp is called at the beginning of
**	the execution of ovqp.
*/
void
startovqp(void)
{
	if (Equel)
		startequel();

	De.ov_tupsfound = 0;	/* counts the number of tuples which sat the qual */
	De.ov_retrieve = De.ov_bopen = FALSE;
	/* catch floating point signals */
	signal(SIGFPE,  flptexcep);
}
