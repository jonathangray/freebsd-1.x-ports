#include <stdio.h>

#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include <resp.h>
#include <symbol.h>
#include <pv.h>
#include "tree.h"
#include <pipes.h>
#include <setjmp.h>
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)go.c	8.2	1/22/85)



/*
**  PROCESS QUERY
**
**	The appropriate messages are printed, and the query is scanned.
**	Tokens are passed to the parser.  A parser response is then
**	expected.
**
**	Trace Flags:
**		5
*/

#define	QRYTRAP		"{querytrap}"

jmp_buf		GoJmpBuf;

void
go(void)
{
	register char	*p;
	resp_t		*rp;
	FILE		*iop;
	char		c;
	pb_t		pb;

	clrline(1);
	fflush(Qryiop);
	if ((iop = fopen(Qbname, "r")) == NULL)
		syserr("go: open 1");
	if (Nodayfile >= 0)
		printf("Executing . . .\n\n");


	if (!Nautoclear)
		Autoclear = 1;

	/* arrange to call the parser */
	initp();
	call_setup(&pb, mdPARSER, NULL);
	pb_prime(&pb, PB_REG);
	pb.pb_proc = 1;		/**** PARSER MUST BE IN PROC ONE ****/
	send_off(&pb, 0, NULL);
	pb_tput(PV_EOF, "", 0, &pb);
	macinit(fgetc, iop, 1);
	while ((c = macgetch()) > 0)
		pb_put(&c, 1, &pb);
	pb_flush(&pb);
	fclose(iop);

	/* wait for the response */
	setjmp(GoJmpBuf);
	readinput(&pb);

	rp = getresp();
	if (rp->resp_tups >= 0) {
		macdefine("{tuplecount}", locv(rp->resp_tups), TRUE);
	}
	
	if (Error_id == 0 && (p = macro(QRYTRAP)) != NULL) {
		trapquery(rp, p);
	}
	
	resetp();

	mcall("{continuetrap}");

	prompt("\ncontinue");
}
