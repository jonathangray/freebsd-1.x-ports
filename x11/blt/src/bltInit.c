/*
 * bltInit.c --
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

#include <tcl.h>

#ifndef BLT_VERSION
#define BLT_VERSION "1.7"
#endif

/*
 * The inclusion of contributed commands/widgets can be suppressed by
 * defining the respective preprocessor symbol.
 */

#ifndef NO_HTEXT
EXTERN int Blt_HtextInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_GRAPH
EXTERN int Blt_GraphInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_BARCHART
EXTERN int Blt_BarchartInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_TABLE
EXTERN int Blt_TableInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_BUSY
EXTERN int Blt_BusyInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_WINDOW
EXTERN int Blt_WindowInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_BITMAP
EXTERN int Blt_BitmapInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_BGEXEC
EXTERN int Blt_BgExecInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_DRAGDROP
EXTERN int Blt_DragDropInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_DEBUG
EXTERN int Blt_DebugInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_WATCH
EXTERN int Blt_WatchInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_BELL
EXTERN int Blt_BellInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif
#ifndef NO_CUTBUFFER
EXTERN int Blt_CutbufferInit _ANSI_ARGS_((Tcl_Interp *interp));
#endif

int
Blt_Init(interp)
    Tcl_Interp *interp;		/* Interpreter to add extra commands */
{
    char *libDir;

    libDir = Tcl_GetVar2(interp, "env", "BLT_LIBRARY", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	libDir = BLT_LIBRARY;
    }
    Tcl_SetVar2(interp, "blt_library", (char *)NULL, libDir, TCL_GLOBAL_ONLY);
#ifndef NO_HTEXT
    if (Blt_HtextInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_GRAPH
    if (Blt_GraphInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_BARCHART
    if (Blt_BarchartInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_TABLE
    if (Blt_TableInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_BUSY
    if (Blt_BusyInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_WINDOW
    if (Blt_WindowInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_DRAGDROP
    if (Blt_DragDropInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_BELL
    if (Blt_BellInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_CUTBUFFER
    if (Blt_CutbufferInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_BITMAP
    if (Blt_BitmapInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_BGEXEC
    if (Blt_BgExecInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_DEBUG
    if (Blt_DebugInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
#ifndef NO_WATCH
    if (Blt_WatchInit(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
    Tcl_SetVar2(interp, "blt_versions", "BLT", BLT_VERSION, TCL_GLOBAL_ONLY);
    return TCL_OK;
}
