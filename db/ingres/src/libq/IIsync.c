#include <stdio.h>
#include <stdarg.h>

#include <ingres.h>
#include <symbol.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIsync.c	8.1	12/31/84)


/*
**	IIsync is called to syncronize the running
**	of a query with the running of the equel process.
**
**	The query is flushed and an EOP is written
**	to the quel parser.
**
**	The quel parser will write an end-of-pipe when
**	an operation is complete.
*/
void
IIsync(char *file_name, ...)
{
	va_list	vp;
	pb_t	pb;

	IIpb_flush(&IIpb);
	va_start(vp, file_name);
	if ((IIproc_name = file_name) != 0)
		IIline_no = va_arg(vp, int);
	va_end(vp);

#ifdef xETR1
	if (IIdebug)
		printf("IIsync\n");
#endif


	IIerrflag = 0;	/* reset error flag. If an error occures,
			** IIerrflag will get set in IIerror
			*/

	IIpb_prime(&pb, PB_NOTYPE);
	IIreadinput(&pb);
	IInewqry = 0;
}
