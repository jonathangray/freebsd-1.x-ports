/*
 * bltCutbuffer.c --
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
#include <X11/Xproto.h>

#ifndef CUTBUFFER_VERSION
#define CUTBUFFER_VERSION "1.0"
#endif

/* ARGSUSED */
static int
RotateErrorProc(clientData, errEventPtr)
    ClientData clientData;
    XErrorEvent *errEventPtr;
{
    int *errorPtr = (int *)clientData;

    *errorPtr = TCL_ERROR;
    return 0;
}

static int
GetCutNumber(interp, string, bufferPtr)
    Tcl_Interp *interp;
    char *string;
    int *bufferPtr;
{
    int number;

    if (Tcl_GetInt(interp, string, &number) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((number < 0) || (number > 7)) {
	Tcl_AppendResult(interp, "bad buffer number \"", string, "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    *bufferPtr = number;
    return TCL_OK;
}

static char *
GetCutBuffer(tkwin, buffer)
    Tk_Window tkwin;
    int buffer;
{
    char *dataPtr;
    int numBytes;
    int limit;
    register char *p;
    register int i;
    int c;

    dataPtr = XFetchBuffer(Tk_Display(tkwin), &numBytes, buffer);
    if (dataPtr == NULL) {
	return NULL;
    }
    if (dataPtr[numBytes - 1] == '\0') {
	limit = numBytes - 1;
    } else {
	limit = numBytes;
    }
    for (p = dataPtr, i = 0; i < limit; i++, p++) {
	c = (unsigned char)*p;
	if (c == 0) {
	    *p = '@';		/* Convert embedded NUL bytes */
	}
    }
    if (limit == numBytes) {
	char *newPtr;

	newPtr = (char *)malloc(numBytes + 1);
	if (newPtr == NULL) {
	    return NULL;
	}
	memcpy(newPtr, dataPtr, numBytes);
	newPtr[numBytes] = '\0';
	free(dataPtr);
	dataPtr = newPtr;
    }
    return (dataPtr);
}

static int
RotateCutBuffer(tkwin, buffer)
    Tk_Window tkwin;
    int buffer;
{
    int error = TCL_OK;
    Tk_ErrorHandler handler;

    handler = Tk_CreateErrorHandler(Tk_Display(tkwin), BadMatch,
	X_RotateProperties, -1, RotateErrorProc, (ClientData)&error);
    XRotateBuffers(Tk_Display(tkwin), buffer);
    Tk_DeleteErrorHandler(handler);
    XSync(Tk_Display(tkwin), False);
    return (error);
}

/*
 *----------------------------------------------------------------------
 *
 * CutBufferCmd --
 *
 *	This procedure is invoked to process the "cutbuffer" Tcl
 *	command. See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
CutbufferCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter.*/
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tk_Window tkwin = (Tk_Window)clientData;
    int buffer;			/* cut buffer number (0-7) */
    char c;
    int length;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " option ?args?\"", (char *)NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'g') && (strncmp(argv[1], "get", length) == 0)) {
	char *string;

	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " get ?buffer?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	buffer = 0;
	if (argc == 3) {
	    if (GetCutNumber(interp, argv[2], &buffer) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
	string = GetCutBuffer(tkwin, buffer);
	if (string != NULL) {
	    Tcl_SetResult(interp, string, TCL_DYNAMIC);
	}
	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "rotate", length) == 0)) {
	int count;

	if ((argc < 2) || (argc > 3)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " rotate ?buffer?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	count = 1;		/* Default: rotate one position */
	if (argc == 3) {
	    if (Tcl_GetInt(interp, argv[2], &count) != TCL_OK) {
		return TCL_ERROR;
	    }
	    if ((count < 0) || (count > 8)) {
		Tcl_AppendResult(interp, "bad rotate count \"", argv[2], "\"",
		    (char *)NULL);
		return TCL_ERROR;
	    }
	}
	if (RotateCutBuffer(tkwin, count) != TCL_OK) {
	    Tcl_AppendResult(interp, "\"", argv[0], " ", argv[1],
		"\": all cut buffers not set", (char *)NULL);
	    return TCL_ERROR;
	}
	return TCL_OK;
    } else if ((c == 's') && (strncmp(argv[1], "set", length) == 0)) {
	if ((argc < 2) || (argc > 4)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " set value ?buffer?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	buffer = 0;
	if (argc == 4) {
	    if (GetCutNumber(interp, argv[3], &buffer) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
	XStoreBuffer(Tk_Display(tkwin), argv[2], strlen(argv[2]) + 1, buffer);
	return TCL_OK;
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
	    "\": should be get, rotate, or set", (char *)NULL);
	return TCL_ERROR;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CutbufferInit --
 *
 *	This procedure is invoked to initialize the "cutbuffer" Tcl
 *	command. See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Blt_CutbufferInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, "blt_cutbuffer", (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_cutbuffer\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_cutbuffer\" requires Tk",
	    (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", "blt_cutbuffer", CUTBUFFER_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, "blt_cutbuffer", CutbufferCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}
