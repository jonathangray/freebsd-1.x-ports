#include <ingres.h>
#include <pv.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)ack_err.c	8.1	12/31/84)

/*
**  ACK_ERR -- the error passing routine for the parser
**
**	Trace Flags:
**		ack_err ~~ 65
*/
int
ack_err(void)
{
	extern int	Ingerr;
	extern int	Err_fnd;

#ifdef	xPTR1
	tTfp(65, 0, "ack_err\n");
#endif

	Ingerr = 1;

	Err_fnd += 1;

	return (1);
}
