/*
 * dpnetworkInit.c --
 *
 *	Initialize an interpreter with Tcl-DP.
 *	This mostly just makes a bunch of calls to Tcl_CreateCommand to
 *	create tcl commands for the various modules.
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include "dpInt.h"
#include "tkInt.h"

/*
 *----------------------------------------------------------------------
 *
 * Tdp_UpdateCmd --
 *
 *	This procedure is invoked to process the "dp_update" Tcl-DP
 *	command.  This is exactly the same as Tk's "update" command,
 *	execpt it doesn't require a connection to the X server.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tdp_UpdateCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int flags;

    if (argc == 1) {
	flags = TK_DONT_WAIT;
    } else if (argc == 2) {
	if (strncmp(argv[1], "idletasks", strlen(argv[1])) != 0) {
	    Tcl_AppendResult(interp, "bad argument \"", argv[1],
		    "\": must be idletasks", (char *) NULL);
	    return TCL_ERROR;
	}
	flags = TK_IDLE_EVENTS;
    } else {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " ?idletasks?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Handle all pending events.
     */

    while (Tk_DoOneEvent(flags) != 0) {
	/* Empty loop body */
    }

    /*
     * Must clear the interpreter's result because event handlers could
     * have executed commands.
     */

    Tcl_ResetResult(interp);
    return TCL_OK;
}

/* ARGSUSED */
int
Tdp_WaitVariable(clientData, interp, argc, argv)
    ClientData clientData;	/* unused */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    char *nargv[4];

    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " variable", (char *) NULL);
	return TCL_ERROR;
    }
    nargv[0] = argv[0];
    nargv[1] = "variable";
    nargv[2] = argv[1];
    nargv[3] = (char *)0;
    return(Tk_TkwaitCmd(clientData, interp, 3, nargv));
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_Init --
 *
 *	Initialize the full Tcl-DP package.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	network related commands are bound to the interpreter.
 *
 *--------------------------------------------------------------
 */
int
Tdp_Init (interp)
     Tcl_Interp *interp;
{
    static char initCmd[] =
	"if [file exists $dp_library/init.tcl] {\
	    source $dp_library/init.tcl\n\
	} else {\n\
	    set msg \"can't find $dp_library/init.tcl; perhaps you \"\n\
	    append msg \"need to\\ninstall Tcl-DP or set your DP_LIBRARY \"\n\
	    append msg \"environment variable?\"\n\
	    error $msg\n\
	}";
    char *libDir;

    libDir = getenv("DP_LIBRARY");
    if (libDir == NULL) {
	libDir = DP_LIBRARY;
    }
    Tcl_SetVar(interp, "dp_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "dp_version", DP_VERSION, TCL_GLOBAL_ONLY);

    Tcl_CreateCommand (interp, "dp_update", Tdp_UpdateCmd,
	   (ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand (interp, "dp_after", Tk_AfterCmd,
	   (ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand (interp, "dp_address", Tdp_AddressCmd,
	   (ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_waitvariable", Tdp_WaitVariable,
		(ClientData)0, (void (*) _ANSI_ARGS_((ClientData))) NULL);

    Tdp_Tcp_Init(interp);
    Tdp_RPCInit(interp);

    /*
     * Execute the tcl routine provided with Tcl-DP to preload all
     * library routines into the interpreter.  This is necessary
     * because some of the library routines shadow built in commands
     * (e.g., the close command)
     */
    return Tcl_GlobalEval (interp, initCmd);
}
