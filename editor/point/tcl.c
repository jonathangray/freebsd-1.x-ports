/* $Header: /a/cvs/386BSD/ports/editor/point/tcl.c,v 1.1 1994/02/15 22:12:41 jkh Exp $ */

#include <stdio.h>
#include "pt.h"

Tcl_Interp * pointsMainInterp = NULL;

char *
ExecTclCommand( command, result )
	char * command;
	int * result;
{
	extern Tcl_Interp * pointsMainInterp;
	char *last_command;
	int ret;

	last_command = "<none>";
	ret = Tcl_Eval( pointsMainInterp, command );

	if( result != NULL )
		*result = ret;
	if( *pointsMainInterp->result != 0 && ret != TCL_OK ) {
		printf("ExecTclCommand: ret=%d, msg=<%s>\n",
						ret, pointsMainInterp->result);
		printf("cmd=<<<%s>>>\n", command);
		printf("Tcl's errorInfo = <<<");
		(void)Tcl_Eval( pointsMainInterp, "puts stderr $errorInfo" );
		printf(">>>\n");
	}
	return pointsMainInterp->result;
}

/*ARGSUSED*/
int
doPtCommand( clientData, interp, argc, argv )
	ClientData clientData;
	Tcl_Interp * interp;
	int argc;
	char *argv[];
{
	int i;
	char *args[6];

	for( i = 1; i < argc; ++i )
		args[i-1] = argv[i];
	for( ; i <= 6; ++i )
		args[i-1] = "";
	(void)command( (int)clientData, args[0], args[1], args[2], args[3],
							args[4], args[5] );
	return TCL_OK;
}

/*ARGSUSED*/
void
ptTcl(w, event, args, nargs)
	int w;
	XButtonEvent *event;
	String *args;
	Cardinal *nargs;
{
	extern char msgBuffer[];

	int i;
	char ch, *to, *from, *limit;

	to = msgBuffer;
	limit = msgBuffer + MSGBUFFERSIZE - 1;
	*to = '\0';	/* make it empty to begin with */
	for( i = 0; i < *nargs; ++i ) {
		*to++ = ' ';
		from = args[i];
		while( 1 ) {
			if( to >= limit ) {
				sprintf( msgBuffer,
				  "Tcl command over %d bytes was ignored.",
				  MSGBUFFERSIZE );
				msg( msgBuffer, 1  );
				to = limit - 1;
				break;
			}
			ch = *to++ = *from++;
			if( ch == '\0' ) {
				--to;	/* back up over '\0' */
				break;
			}
		}
	}
	(void)ExecTclCommand( msgBuffer, NULL );
}

