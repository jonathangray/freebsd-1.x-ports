/*
 * tkCutBuffer.c --
 *	Support for X cut buffers
 */
#include "tk.h"
#include <X11/Xproto.h>
#include <unistd.h>
#include <stdlib.h>

 
 /*
  *----------------------------------------------------------------------
  *
  * Tk_CutBufferCmd --
  *
  *	This procedure is invoked to process the "cutbuffer" Tcl command.
  *	See the user documenttaion for details on what it does.
  *
  * Results:
  *	A standard Tcl result.
  *
  * Side effects:
  *	None.
  *
  *----------------------------------------------------------------------
  */
 
 static int CutBufferError;	/* self-explanatory? */
 
 	/* ARGSUSED */
 static int RotPropError(clientData, errEventPtr)
     ClientData clientData;
     XErrorEvent *errEventPtr;
 {
     CutBufferError = 1;
     return 0;
 }
 
 	/* ARGSUSED */
 int
 Tk_CutBufferCmd(clientData, interp, argc, argv)
     ClientData clientData;	/* Main window associated with
 				 * interpreter.*/
     Tcl_Interp *interp;		/* Current interpreter. */
     int argc;			/* Number of arguments. */
     char **argv;		/* Argument strings. */
 {
     Tk_Window tkwin = (Tk_Window) clientData;
     int buffer;			/* cut buffer number (0-7) */
     int length;
     int nbytes;			/* number of bytes in prpoperty */
     char c;
     char *data;
     unsigned char *p;
     int i;
     int limit;
     
     if (argc < 2) {
 	Tcl_AppendResult(interp, "wrong # args: should be \"",
 		argv[0], " option ?args?\"", (char *) NULL);
 	return TCL_ERROR;
     }
 
     c = argv[1][0];
     length = strlen(argv[1]);
     if ((c == 'g') && (strncmp(argv[1], "get", length) == 0)) {
 	if (argc == 2) {
 	    buffer = 0;
 	} else if (argc == 3) {
 	    if ((Tcl_GetInt(interp, argv[2], &buffer) != TCL_OK)
 		|| (buffer < 0) || (buffer > 7)) {
 		goto badBuffer;
	    }
 	} else {
 	    goto wrongArgs;
	}
	return GetCutBuffer(interp, tkwin, &nbytes, buffer);
     } else if ((c == 'r') && (strncmp(argv[1], "rotate", length) == 0)) {
 	Tk_ErrorHandler handler;
 	int rotate;
 
 	if (argc == 2) {
 	    rotate = 0;
 	} else if (argc == 3) {
 	    if ((Tcl_GetInt(interp, argv[2], &rotate) != TCL_OK)
 		|| (rotate < -7) || (rotate > 7)) {
 		Tcl_ResetResult(interp);
 		Tcl_AppendResult(interp, "bad rotate number \"",
 			argv[2], "\"", (char *) NULL);
 		return TCL_ERROR;
 	    }
 	} else {
 	    goto wrongArgs;
 	}
 	CutBufferError = 0;
 	handler = Tk_CreateErrorHandler(Tk_Display(tkwin), BadMatch,
 		X_RotateProperties, -1, RotPropError, (ClientData) NULL);
 	XRotateBuffers(Tk_Display(tkwin), rotate);
 	Tk_DeleteErrorHandler(handler);
 	XSync(Tk_Display(tkwin), False);
 	if (CutBufferError) {
 	    Tcl_ResetResult(interp);
 	    Tcl_AppendResult(interp, "\"", argv[0], " ", argv[1],
 		    "\": all cut buffers not set", (char *) NULL);
 	    return TCL_ERROR;
 	} else {
 	    return TCL_OK;
 	}
     } else if ((c == 's') && (strncmp(argv[1], "set", length) == 0)) {
 	if (argc == 2) {
 	    buffer = 0;
	    return GetCutBuffer(interp, tkwin, &nbytes, buffer);
 	} else if (argc == 3) {
 	    if ((Tcl_GetInt(interp, argv[2], &buffer) == TCL_OK)
		&& (buffer >= 0) && (buffer <= 7)) {
		return GetCutBuffer(interp, tkwin, &nbytes, buffer);
	    }
 	    Tcl_ResetResult(interp);
 	    buffer = 0;
 	    data = argv[2];
 	} else if (argc == 4) {
 	    if ((Tcl_GetInt(interp, argv[2], &buffer) != TCL_OK)
 		|| (buffer < 0) || (buffer > 7)) {
 		goto badBuffer;
 	    }
 	    data = argv[3];
 	} else {
 	    Tcl_AppendResult(interp, "wrong # args: should be \"",
 		argv[0], " set bufNum value\"", (char *) NULL);
 	    return TCL_ERROR;
 	}
 	nbytes = strlen(data) + 1;
 	XStoreBuffer(Tk_Display(tkwin), data, nbytes, buffer);
 	Tcl_SetResult(interp, data, TCL_VOLATILE);
 	return TCL_OK;
     } else {
 	Tcl_AppendResult(interp, "bad option \"", argv[1],
 		"\": must be get, rotate, or set", (char *) NULL);
 	return TCL_ERROR;
     }

     badBuffer:
     Tcl_ResetResult(interp);
     Tcl_AppendResult(interp, "bad cut buffer number \"",
             argv[2], "\"", (char *) NULL);
     return TCL_ERROR;
 
     wrongArgs:
     Tcl_AppendResult(interp, "wrong # args: should be \"",
             argv[0], " option ?bufNum?\"", (char *) NULL);
      return TCL_ERROR;
  }

int
GetCutBuffer(interp, tkwin, nbytesPtr, buffer)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    int *nbytesPtr;
    char *buffer;
{
    char *data, *p;
    int limit, i;

    data = XFetchBuffer(Tk_Display(tkwin), nbytesPtr, buffer);
    if (data == NULL) {
	interp->result[0] = '\0';
	return TCL_OK;
    }
    if (data[*nbytesPtr - 1] == '\0') {
	limit = *nbytesPtr - 1;
    } else {
	limit = *nbytesPtr;
    }
    for (p = (unsigned char *) data, i = 0; i < limit; i++, p++) {
	if (!(isascii(*p) && (isprint(*p) || isspace(*p)))) {
	    *p = '@';
	}
    }
    if (limit == *nbytesPtr) {
	char *newData = ckalloc(*nbytesPtr + 1);
	memcpy(newData, data, *nbytesPtr);
	newData[*nbytesPtr] = '\0';
	XFree(data);
	data = newData;
    }
    Tcl_SetResult(interp, data, TCL_DYNAMIC);
    return TCL_OK;
}

