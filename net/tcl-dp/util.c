/*
 * util.c
 *
 * Copyright 1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include <sys/types.h>
#include <sys/time.h>

#include "tcl.h"
#include "tclInt.h"

#include "util.h"

static char *exitCmd;
static int exitCmdSize;
static int exitCmdMax;

#define	max(a,b)	((a)>(b)?(a):(b))


/*
 *--------------------------------------------------------------
 *
 * Tcm_AtExitCmd --
 *
 *	This procedure is invoked to process the "atexit" command.
 *	See the user documentation for details.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
    /* ARGSUSED */
int
Tcm_AtExitCmd(clientData, interp, argc, argv)
    ClientData *clientData;
    Tcl_Interp *interp;		/* tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    int len;
    char *d, *s;
    int i, incr;

    if (argc == 1) {
	Tcl_SetResult(interp, exitCmd, TCL_VOLATILE);
	return TCL_OK;
    }
    if (argc != 2) {
	sprintf(interp->result, "wrong # args: should be \"%s ?cmd?\"",
		argv[0]);
	return TCL_ERROR;
    }
    len = strlen(argv[1]) + 1;

    /*
     * Realloc exitCmd if needed.
     */
    if ((exitCmdSize + len) > exitCmdMax) {
	incr = max(1024, len);
	exitCmdMax += incr;
	if (exitCmd == NULL) {
	    exitCmd = ckalloc(exitCmdMax);
	    memset(exitCmd, 0, exitCmdMax);
	} else {
	    exitCmd = realloc(exitCmd, exitCmdMax);
	    memset(exitCmd + exitCmdMax - incr, 0, incr);
	}
	if (exitCmd == NULL) {
	    sprintf(interp->result,
		    "Error allocating exit command: out of memory\n");
	    return TCL_ERROR;
	}
    }
    /* Make a hole at the beginning of the string */
    Tcl_SetResult(interp, exitCmd, TCL_VOLATILE);
    for (d=exitCmd+len+exitCmdSize-1, s=exitCmd+exitCmdSize-1, i=exitCmdSize;
	 i != 0; i--) {
	*d-- = *s--;
    }
    for (s=argv[1], d=exitCmd, i=len-1; i != 0; i--) {
	*d++ = *s++;
    }
    exitCmd[len - 1] = ';';
    exitCmdSize += len;
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * Tcm_ExitCmd --
 *
 *	This procedure is invoked to process the "exit" command.
 *	It first does the atexit callbacks, then calls
 *	the normal Tcl exit routines.  See the user documentation
 *	for details on what these do.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
int
Tcm_ExitCmd(clientData, interp, argc, argv)
    ClientData *clientData;
    Tcl_Interp *interp;		/* tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    char *termPtr;

    termPtr = exitCmd;
    while (termPtr && *termPtr) {
	/*
	 * Eval all the commands in exitCmd, and keep going in case of
	 * errors.
	 */
	Tcl_Eval(interp, termPtr, 0, &termPtr);
	while (*termPtr && (*termPtr != ';')) {
	    termPtr++;
	}
	termPtr++;
    }
    Tcl_ExitCmd(clientData, interp, argc, argv);
}


/*
 *--------------------------------------------------------------
 *
 * utilInit --
 *
 *	Initialize an interpreter with the util extensions to tcl.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	util related commands are bound to the interpreter.
 *
 *--------------------------------------------------------------
 */
void
utilInit(interp)
    Tcl_Interp *interp;		/* tcl interpreter */
{
    Tcl_CreateCommand(interp, "atexit", Tcm_AtExitCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "exit", Tcm_ExitCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
}


/*
 *----------------------------------------------------------------------
 *
 * ReadSysClock --
 *
 *	Return the value of the system clock as a double.
 *
 * Results:
 *	Value of the system clock.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
double
ReadSysClock()
{
    struct timeval tv;
    (void) gettimeofday(&tv, NULL);
    return (tv.tv_sec + tv.tv_usec / 1000000.0);
}

