#include <stdio.h>
#include <ingres.h>
#include <symbol.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIsetup.c	8.1	12/31/84)


/*
**	IIsetup is called to mark the start of a retrieve.
*/
void
IIsetup(void)
{
#ifdef xETR1
	if (IIdebug)
		printf("IIsetup\n");
#endif
	IIin_retrieve = 1;
	IIr_sym.type = IIr_sym.len = 0;
	IIdomains = 0;
	IIerrflag = 0;

	/* flush old data from IIpb and do pre-read */
	IIpb_flush(&IIpb);
	IIpb_prime(&IIpb, PB_NOTYPE);
}
