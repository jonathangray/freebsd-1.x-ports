/* 
 * This wish style program can fork to a child that 
 * runs as a background daemon.
 *
 * Copyright 1990-1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

/**********************************************************************
 *
 * The variable below is the default prompt, stored in the "prompt"
 * variable.
 */

#define PROMPT "swish: "

/**********************************************************************/

#include <signal.h>
#include <stdlib.h>

#include "tkInt.h"
#include "tkConfig.h"

/**********************************************************************/

extern int isatty ();

Tcl_Interp *interp;		/* Main Tcl interpreter. 	*/
char       *myName;		/* Set to argv[0]. 		*/

Tcl_CmdBuf buffer;
int tty;

/*
 * Command used to initialize interp:
 */

char initInterp[] = "source [info library]/init.tcl;";

/**********************************************************************/

char *file  = NULL;
int  daemon = 0;

Tk_ArgvInfo argTable[] = {
    {"-file", TK_ARGV_STRING, (char *) NULL, (char *) &file,
	"File from which to read commands"},
    {"-daemon", TK_ARGV_CONSTANT, (char *) 1, (char *) &daemon,
	"Run as a background daemon"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};

/**********************************************************************/

/*
 * Called for a graceful exit on bus errors, etc.
 */

void
bye (sig)
    int sig;
{
    static int called;		/* To prevent recursion. 	*/
    char temp[64];
    char *str;

    switch (sig) {
	case SIGINT:
	    str = "interupt";
	    break;
	case SIGBUS:
	    str = "bus error";
	    break;
	case SIGSEGV:
	    str = "segmentation violation";
	    break;
	case SIGTERM:
	    str = "terminate";
	    break;
	default:
	    sprintf (temp, "unknown (%d).", sig);
	    str = temp;
	    break;
    }
    if (!called) {
	called = 1;
	fprintf (stderr, "%s: Caught %s signal. Cleaning up and exiting.\n",
		 myName, str);
	Tcl_Eval (interp, "exit", 0, (char **) NULL);
	exit (0);
    } else {
	fprintf (stderr, "%s: Caught %s signal while cleaning up. Aborting.\n",
		 myName, str);
	abort ();
    }
}

    /* ARGSUSED */
void
StdinProc (clientData, mask)
    ClientData clientData;		/* Not used. */
    int mask;
{
    char line[200];
    static int gotPartial = 0;
    char *cmd;
    int result;

    if (mask & TK_READABLE) {
	if (fgets(line, 200, stdin) == NULL) {
	    if (!gotPartial) {
		if (tty) {
		    Tcl_Eval(interp, "destroy .", 0, (char **) NULL);
		    exit(0);
		} else {
		    Tk_DeleteFileHandler(0);
		}
		return;
	    } else {
		line[0] = 0;
	    }
	}
	cmd = Tcl_AssembleCmd(buffer, line);
	if (cmd == NULL) {
	    gotPartial = 1;
	    return;
	}
	gotPartial = 0;
	result = Tcl_RecordAndEval(interp, cmd, 0);
	if (*interp->result != 0) {
	    if ((result != TCL_OK) || (tty)) {
		printf("%s\n", interp->result);
	    }
	}
	if (tty) {
	    printf(Tcl_GetVar (interp, "prompt", TCL_GLOBAL_ONLY));
	    fflush(stdout);
	}
    }
}

int
main (argc, argv)
    int argc;
    char **argv;
{
    char *args, *msg;
    char buf[1000];
    int result;

    myName = argv[0];

    interp = Tcl_CreateInterp ();

    if (Tk_ParseArgv (interp, (Tk_Window) NULL, 
		      &argc, argv, argTable, 0)	!= TCL_OK) 
      {
	fprintf (stderr, "%s\n", interp->result);
	exit (1);
      }

    if (daemon && (fork () != 0))
      exit (0);

    /* Install signal handlers */
    signal (SIGINT, bye);
    signal (SIGBUS, bye);
    signal (SIGSEGV, bye);
    signal (SIGTERM, bye);

#ifdef BERKELEY_PLATEAU

    /* Initialize extensions to Tcl */
    Tclx_Initialize (interp);

    /* Initialize basic Tk commands */
    Tk_Init (interp, NULL, "services");

#endif

#ifndef BERKELEY_PLATEAU

    /* Try standard Tk initialization */

    if (!(Tk_CreateMainWindow (interp, NULL, "services")))
      {
        fprintf(stderr, "%s\n", interp->result);
        exit(1);
      }

#endif

    args = Tcl_Merge (argc - 1, argv + 1);
    sprintf (buf, "%d", argc - 1);

    Tcl_SetVar (interp, "argv", args, TCL_GLOBAL_ONLY);
    Tcl_SetVar (interp, "argc", buf,  TCL_GLOBAL_ONLY);
    Tcl_SetVar (interp, "prompt", PROMPT, TCL_GLOBAL_ONLY);

    ckfree (args);

    dpInit (interp);

    result = Tcl_Eval (interp, initInterp, 0, (char **) NULL);
    if (result != TCL_OK)
      goto error;

    if (file) 
      {
	result = Tcl_VarEval (interp, "source ", file, (char *) NULL);
	if (result != TCL_OK)
	  goto error;
	tty = 0;
      } 
    else 
      {
	tty = isatty (0);
	Tk_CreateFileHandler (0, TK_READABLE, StdinProc, (ClientData) 0);
	if (tty)
	  printf (Tcl_GetVar (interp, "prompt", TCL_GLOBAL_ONLY));
      }

    fflush (stdout);
    buffer = Tcl_CreateCmdBuf ();

    while (1)
      Tk_DoOneEvent (0);

  error:
    msg = Tcl_GetVar (interp, "errorInfo", TCL_GLOBAL_ONLY);
    if (msg == NULL)
      msg = interp->result;

    fprintf (stderr, "%s\n", msg);
    bye (SIGTERM);
}
