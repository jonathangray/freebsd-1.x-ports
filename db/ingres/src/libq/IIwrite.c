#include <stdio.h>
#include <ingres.h>
#include <symbol.h>
#include <pipes.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIwrite.c	8.1	12/31/84)


/*
**	IIwrite is used to write a string to the
**	quel parser
*/
void
IIwrite(char *str)
{
	register char	*s;
	register int	i;

	s = str;
#ifdef xETR1
	if (IIdebug)
		printf("write:string='%s'\n", s);
#endif
	if (!IIingpid)
		IIsyserr("no preceding ##ingres statement");
	if (IIin_retrieve)
		IIsyserr("IIwrite:you cannot call ingres while in a retrieve");

	if (!IInewqry) {
		IIpb_prime(&IIpb, PB_REG);
		IIpb.pb_proc = 1;
		IIpb_put("\0\0\0", 3, &IIpb);

		IInewqry = 1;
	}

	if ((i = IIlength(s)) != 0)
		IIpb_put(s, i, &IIpb);
}
