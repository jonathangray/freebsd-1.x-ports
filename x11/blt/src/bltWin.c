/*
 * bltWin.c --
 *
 *	This module implements simple window commands for
 *	the Tk toolkit.
 *
 * Copyright 1993-1994 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

#include "blt.h"

#ifndef WINDOW_VERSION
#define WINDOW_VERSION "1.0"
#endif

/* ARGSUSED */
static int
WindowCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tk_Window mainWin = (Tk_Window)clientData;
    Tk_Window tkwin;
    char c;
    int length;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " option window\"", (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_NameToWindow(interp, argv[2], mainWin);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
#ifdef notdef
    if (!Tk_IsTopLevel(tkwin)) {
	Tcl_AppendResult(interp, "\"", argv[2], "\" is not a top level window",
	    (char *)NULL);
	return TCL_ERROR;
    }
#endif
    c = argv[1][0];
    length = strlen(argv[1]);
    if (Tk_WindowId(tkwin) == None) {
	Tk_MakeWindowExist(tkwin);
    }
    if ((c == 'm') && (strncmp(argv[1], "map", length) == 0)) {
	Tk_MapWindow(tkwin);
    } else if ((c == 'u') && (strncmp(argv[1], "unmap", length) == 0)) {
	Tk_UnmapWindow(tkwin);
    } else if ((c == 'l') && (strncmp(argv[1], "lower", length) == 0)) {
	XLowerWindow(Tk_Display(tkwin), Tk_WindowId(tkwin));
    } else if ((c == 'r') && (strncmp(argv[1], "raise", length) == 0)) {
	XRaiseWindow(Tk_Display(tkwin), Tk_WindowId(tkwin));
    } else {
	Tcl_AppendResult(interp, "unknown option \"", argv[1],
	    "\": should be lower, map, raise, or unmap", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

int
Blt_WindowInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, "blt_win", (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_win\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_win\" requires Tk", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", "blt_win", WINDOW_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, "blt_win", WindowCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}
