/*
 * networkInit.c --
 *
 *	Initialize an interpreter with Tcl-DP.
 *	This mostly just makes a bunch of calls to Tcl_CreateCommand to
 *	create tcl commands for the various modules.
 */

#include "tcl.h"
#include "util.h"
#include "network.h"


/*
 *--------------------------------------------------------------
 *
 * networkInit --
 *
 *	Initialize an interpreter with the network extensions to tcl.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	network related commands are bound to the interpreter.
 *
 *--------------------------------------------------------------
 */
void
networkInit (interp)
    Tcl_Interp *interp;		/* tcl interpreter */
{
    Tcl_CreateCommand (interp, "address", Tcm_AddressCmd,
		       (ClientData) NULL, (void (*) ()) NULL);

    Tcp_Init(interp);
    Tcm_RPCInit(interp);
}


/*
 *--------------------------------------------------------------
 *
 * dpInit --
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
void
dpInit (interp)
     Tcl_Interp *interp;
{
  utilInit (interp);
  networkInit (interp);
}

