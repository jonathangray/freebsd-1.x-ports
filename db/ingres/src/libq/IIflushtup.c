#include <stdio.h>
#include <stdarg.h>

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIflushtup.c	8.1	12/31/84)


/*
**	IIflushtup is called to syncronize the data pipe
**	after a retrieve.
*/
void
IIflushtup(char *file_name, ...)
{
	va_list	vp;

	va_start(vp, file_name);
	if ((IIproc_name = file_name) != 0)
		IIline_no = va_arg(vp, int);
	va_end(vp);

#ifdef xATR1
	if (IIdebug)
		printf("IIflushtup: IIerrflag %d\n", IIerrflag);
#endif

	if (IIerrflag < 2000) {
		while ((IIpb.pb_stat & PB_EOF) == 0)
			IIpb_read(&IIpb);

		/* read the RESP block */
		IIpb_prime(&IIpb, PB_NOTYPE);
		IIreadinput(&IIpb);
	}

	IIin_retrieve = 0;
	IIndomains = 0;
	IIdomains = 0;
	IInewqry = 0;
}
