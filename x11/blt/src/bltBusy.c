/*
 * bltBusy.c --
 *
 *	This module implements busy windows for the Tk toolkit.
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
 * Busy command created by George Howlett.
 */

#include "blt.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#ifndef BUSY_VERSION
#define BUSY_VERSION "1.2"
#endif

typedef struct {
    Tk_Window host;		/* Host window of the busy window. It is used
				 * to manage the size and position of the busy
				 * window. */
    unsigned int width, height;	/* Last known size of the host window. Also
				 * specifies the size of the busy window. */
    int hostX, hostY;		/* Last known position of the host window */
    int x, y;			/* Position of the busy window in its parent */
    Tk_Window mainWin;		/* Used to key searches in the window hierarchy
				 * See the "hosts" command. */
    int mapped;			/* If non-zero, busy window is mapped */
    Window window;		/* Busy window: InputOnly class window */
    Display *display;		/* Display of parent, host, and busy windows */

    Cursor cursor;		/* Cursor for the busy window */
    Tk_Window parent;		/* Parent window of the busy window. It may be
			         * the host window or a mutual ancestor of the
				 * host window */

} BusyWin;

#define TRUE		1
#define FALSE		0
#define DEF_BUSY_CURSOR "watch"

static int ParseParent _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *value, char *widgRec, int offset));
static char *PrintParent _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption ParentOption =
{
    ParseParent, PrintParent, (ClientData)0
};

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_CURSOR, "-cursor", "busyCursor", "BusyCursor",
	DEF_BUSY_CURSOR, Tk_Offset(BusyWin, cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-in", (char *)NULL, (char *)NULL, (char *)NULL,
	Tk_Offset(BusyWin, parent), TK_CONFIG_NULL_OK, &ParentOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

static int initialized = 0;	/* Flag to initialize the hash table */
static Tcl_HashTable busyWinTable;	/* Hash table of busy window structures keyed
				   * by the address of the host Tk window */

/* Forward declarations */
static void DestroyBusyWindow _ANSI_ARGS_((ClientData clientData));

/*
 *----------------------------------------------------------------------
 *
 * ParseParent --
 *
 *	Convert the pathname of a Tk window and check to see if its a
 *	valid parent for the busy window.
 *
 *	 A valid parent window is either
 *
 *    	    1) the host window itself, or
 * 	    2) a mutual ancestor of the host window.
 *
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseParent(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Used to search for window */
    char *value;		/* Pathname of new parent window */
    char *widgRec;		/* BusyWin structure record */
    int offset;			/* not used */
{
    BusyWin *busyPtr = (BusyWin *)(widgRec);
    Tk_Window parent;
    int x, y;

    x = y = 0;
    if (value == NULL) {
	parent = busyPtr->host;
    } else {
	parent = Tk_NameToWindow(interp, value, tkwin);
	if (parent == NULL) {
	    return TCL_ERROR;
	}
	if (parent != busyPtr->host) {
	    Tk_Window ancestor;

	    for (ancestor = busyPtr->host; ancestor != parent;
		ancestor = Tk_Parent(ancestor)) {
		x += Tk_X(ancestor) + Tk_Changes(ancestor)->border_width;
		y += Tk_Y(ancestor) + Tk_Changes(ancestor)->border_width;
		if (Tk_IsTopLevel(ancestor)) {
		    break;
		}
	    }
	    if (ancestor != parent) {
		Tcl_AppendResult(interp, "\"", Tk_PathName(parent),
		    "\" in not a parent of \"", Tk_PathName(busyPtr->host),
		    "\"", (char *)NULL);
		return TCL_ERROR;
	    }
	}
    }
    if (parent != busyPtr->parent) {
	if (busyPtr->window != None) {
	    XReparentWindow(busyPtr->display, busyPtr->window,
		Tk_WindowId(parent), x, y);
	    XMapRaised(busyPtr->display, busyPtr->window);
	}
	busyPtr->parent = parent;
	busyPtr->x = x;
	busyPtr->y = y;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintParent --
 *
 *	Returns the path name of the parent window for busy window
 *
 * Results:
 *	The path name of the parent of the busy window.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintParent(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* BusyWin structure record */
    int offset;			/* not used */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    BusyWin *busyPtr = (BusyWin *)(widgRec);

    return (Tk_PathName(busyPtr->parent));
}

/*
 * ------------------------------------------------------------------
 *
 * HostWindowEventProc --
 *
 *	This procedure is invoked by the Tk dispatcher for the following
 *	events on the host window.  If the host and parent windows are
 *	the same, only the first event is important.
 *
 *	   1) ConfigureNotify  - The host window has been resized or
 *				 moved.  Move and resize the busy window
 *				 to be the same size and position of the
 *				 host window.
 *
 *	   2) DestroyNotify    - The host window was destroyed. Destroy
 *				 the busy window and the free resources
 *				 used.
 *
 *	   3) MapNotify	       - The host window was (re)mapped. Map the
 *				 busy window again.
 *
 *	   4) UnmapNotify      - The host window was unmapped. Unmap the
 *				 busy window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the host window gets deleted, internal structures get
 * 	cleaned up.  When it gets resized, the busy window is resized
 *	accordingly. If it's mapped, the busy window is mapped. And
 *	when it's unmapped, the busy window is unmapped.
 *
 * -------------------------------------------------------------------
 */
static void
HostWindowEventProc(clientData, eventPtr)
    ClientData clientData;	/* Busy window record */
    register XEvent *eventPtr;	/* Event which triggered call to routine */
{
    register BusyWin *busyPtr = (BusyWin *)clientData;

    if (eventPtr->type == DestroyNotify) {
	/*
	 * Arrange for the busy structure to be removed at a proper time.
	 */
	Tk_EventuallyFree((ClientData)busyPtr, DestroyBusyWindow);
    } else if (eventPtr->type == ConfigureNotify) {
	if ((busyPtr->width != Tk_Width(busyPtr->host)) ||
	    (busyPtr->height != Tk_Height(busyPtr->host)) ||
	    (busyPtr->hostX != Tk_X(busyPtr->host)) ||
	    (busyPtr->hostY != Tk_Y(busyPtr->host))) {
	    int x, y;

	    busyPtr->width = Tk_Width(busyPtr->host);
	    busyPtr->height = Tk_Height(busyPtr->host);
	    busyPtr->hostX = Tk_X(busyPtr->host);
	    busyPtr->hostY = Tk_Y(busyPtr->host);

	    x = y = 0;
	    if (busyPtr->parent != busyPtr->host) {
		Tk_Window ancestor;

		for (ancestor = busyPtr->host; ancestor != busyPtr->parent;
		    ancestor = Tk_Parent(ancestor)) {
		    x += Tk_X(ancestor) + Tk_Changes(ancestor)->border_width;
		    y += Tk_Y(ancestor) + Tk_Changes(ancestor)->border_width;
		}
	    }
	    if (busyPtr->window != None) {
		XMoveResizeWindow(busyPtr->display, busyPtr->window, x, y,
		    busyPtr->width, busyPtr->height);
	    }
	    busyPtr->x = x;
	    busyPtr->y = y;
	}
    } else if (eventPtr->type == MapNotify) {
	if ((busyPtr->parent != busyPtr->host) && (!busyPtr->mapped)) {
	    XMapRaised(busyPtr->display, busyPtr->window);
	    busyPtr->mapped = TRUE;
	}
    } else if (eventPtr->type == UnmapNotify) {
	if ((busyPtr->parent != busyPtr->host) && (busyPtr->mapped)) {
	    XUnmapWindow(busyPtr->display, busyPtr->window);
	    busyPtr->mapped = FALSE;
	}
    }
}

/*
 * ------------------------------------------------------------------
 *
 * CreateBusyWindow --
 *
 *	Creates a child InputOnly window which obscures the parent
 *      window thereby effectively blocking device events.  The size
 *      and position of the busy window is exactly that of the host
 *      window.
 *
 * Results:
 *	Returns a pointer to the new busy window structure.
 *
 * Side effects:
 *	When the busy window is eventually mapped, it will screen
 *	device events (in the area of the host window) from reaching
 *      its parent window and its children.  User feed back can be
 *      achieved by changing the cursor.
 *
 * -------------------------------------------------------------------
 */
static BusyWin *
CreateBusyWindow(interp, tkwin, searchWin)
    Tcl_Interp *interp;		/* Interpreter to report error to */
    Tk_Window tkwin;		/* Window hosting the busy window */
    Tk_Window searchWin;	/* Window to use in searches of the parent
				 * window by pathname */
{
    Tcl_HashEntry *entryPtr;
    BusyWin *busyPtr;
    XSetWindowAttributes attributes;
    int dummy;

    busyPtr = (BusyWin *)malloc(sizeof(BusyWin));
    if (busyPtr == NULL) {
	interp->result = "can't allocate busy window";
	return NULL;
    }
    busyPtr->host = busyPtr->parent = tkwin;
    busyPtr->mainWin = searchWin;
    busyPtr->cursor = None;
    busyPtr->window = None;
    busyPtr->display = Tk_Display(tkwin);
    busyPtr->x = busyPtr->y = 0;

    /*
     * Ignore the important events while the busy window is mapped.
     */
    attributes.do_not_propagate_mask = (KeyPressMask | KeyReleaseMask |
	ButtonPressMask | ButtonReleaseMask | PointerMotionMask);
    attributes.event_mask = (KeyPressMask | KeyReleaseMask |
	ButtonPressMask | ButtonReleaseMask | PointerMotionMask);

    Tk_MakeWindowExist(tkwin);
    /*
     * Create an InputOnly window the size of the host window.
     */
    busyPtr->width = Tk_Width(tkwin);
    busyPtr->height = Tk_Height(tkwin);
    busyPtr->window = XCreateWindow(busyPtr->display, Tk_WindowId(tkwin),
	busyPtr->x, busyPtr->y, busyPtr->width, busyPtr->height,
	(unsigned int)0, CopyFromParent, InputOnly, CopyFromParent,
	CWDontPropagate, &attributes);
    if (busyPtr->cursor != None) {
	XDefineCursor(busyPtr->display, busyPtr->window, busyPtr->cursor);
    }
    XMapRaised(busyPtr->display, busyPtr->window);
    busyPtr->mapped = TRUE;
    /*
     * Track events in the host window to see if it is resized or destroyed.
     */
    Tk_CreateEventHandler(tkwin, StructureNotifyMask, HostWindowEventProc,
	(ClientData)busyPtr);
    entryPtr = Tcl_CreateHashEntry(&busyWinTable, (char *)tkwin, &dummy);
    Tcl_SetHashValue(entryPtr, (char *)busyPtr);
    return (busyPtr);
}

/*
 * ------------------------------------------------------------------
 *
 * DestoryBusyWindow --
 *
 *	This procedure is called from the Tk event dispatcher. It
 * 	releases X resources and memory used by the busy window and
 *	updates the internal hash table.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory and resources are released and the Tk event handler
 *	is removed.
 *
 * -------------------------------------------------------------------
 */
static void
DestroyBusyWindow(clientData)
    ClientData clientData;	/* Busy window structure record */
{
    BusyWin *busyPtr = (BusyWin *)clientData;
    Tcl_HashEntry *entryPtr;

    if (busyPtr->cursor != None) {
	Tk_FreeCursor(busyPtr->display, busyPtr->cursor);
    }
    Tk_DeleteEventHandler(busyPtr->host, StructureNotifyMask,
	HostWindowEventProc, (ClientData)busyPtr);
    entryPtr = Tcl_FindHashEntry(&busyWinTable, (char *)busyPtr->host);
    Tcl_DeleteHashEntry(entryPtr);
    free((char *)busyPtr);
}

/*
 * ------------------------------------------------------------------
 *
 * GetBusyWindow --
 *
 *	Returns the busy window structure associated with the host
 *      window, keyed by its path name.  The clientData argument is
 *      the main window of the interpreter, used to search for the
 *      host window in its own window hierarchy.
 *
 * Results:
 *	If path name represents a host window with a busy window, a
 *	pointer to the busy window structure is returned. Otherwise,
 *	NULL is returned and an error message is left in
 *	interp->result.
 *
 * -------------------------------------------------------------------
 */
static BusyWin *
GetBusyWindow(clientData, interp, pathName)
    ClientData clientData;	/* Window used to reference search  */
    Tcl_Interp *interp;		/* Interpreter to report errors to */
    char *pathName;		/* Path name of parent window */
{
    Tcl_HashEntry *entryPtr;
    Tk_Window searchWin = (Tk_Window)clientData;
    Tk_Window tkwin;

    tkwin = Tk_NameToWindow(interp, pathName, searchWin);
    if (tkwin == NULL) {
	return NULL;
    }
    entryPtr = Tcl_FindHashEntry(&busyWinTable, (char *)tkwin);
    if (entryPtr == NULL) {
	Tcl_AppendResult(interp, "can't find busy window \"", pathName, "\"",
	    (char *)NULL);
	return NULL;
    }
    return ((BusyWin *)Tcl_GetHashValue(entryPtr));
}

/*
 * ------------------------------------------------------------------
 *
 * StatusBusyWindow --
 *
 *	Returns the status of the busy window; whether it's blocking
 *	events or not.
 *
 * Results:
 *	Returns a normal TCL result. If path name represents a busy
 *	window, the status is returned via interp->result and TCL_OK
 *	is returned. Otherwise, TCL_ERROR is returned and an error
 *	message is left in interp->result.
 *
 * -------------------------------------------------------------------
 */
static int
StatusBusyWindow(clientData, interp, pathName)
    ClientData clientData;	/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report error to */
    char *pathName;		/* Path name of host window */
{
    BusyWin *busyPtr;

    busyPtr = GetBusyWindow(clientData, interp, pathName);
    if (busyPtr == NULL) {
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)busyPtr);
    interp->result = (busyPtr->mapped) ? "1" : "0";
    Tk_Release((ClientData)busyPtr);
    return TCL_OK;
}

/*
 * ------------------------------------------------------------------
 *
 * ForgetBusyWindow --
 *
 *	Destroys the busy window associated with the host window and
 *	arranges for internal resources to the released when they're
 *	not being used anymore.
 *
 * Results:
 *	Returns a normal TCL result. If path name represents a busy
 *	window, it is destroyed and TCL_OK is returned. Otherwise,
 *	TCL_ERROR is returned and an error message is left in
 *	interp->result.
 *
 * Side effects:
 *	The busy window is removed.  Other related memory and resources
 *	are eventually released by the Tk dispatcher.
 *
 * -------------------------------------------------------------------
 */
static int
ForgetBusyWindow(clientData, interp, pathName)
    ClientData clientData;	/* Main window of the interpreter */
    Tcl_Interp *interp;		/* Interpreter to report errors to */
    char *pathName;		/* Path name of host window */
{
    BusyWin *busyPtr;

    busyPtr = GetBusyWindow(clientData, interp, pathName);
    if (busyPtr == NULL) {
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)busyPtr);
    if (busyPtr->window != None) {
	XDestroyWindow(busyPtr->display, busyPtr->window);
	busyPtr->window = None;
    }
    Tk_EventuallyFree((ClientData)busyPtr, DestroyBusyWindow);
    Tk_Release((ClientData)busyPtr);
    return TCL_OK;
}

/*
 * ------------------------------------------------------------------
 *
 * ReleaseBusyWindow --
 *
 *	Unmaps the busy window, thereby permitting device events
 *	to be received by the parent window and its children.
 *
 * Results:
 *	Returns a normal TCL result. If path name represents a busy
 *	window, it is unmapped and TCL_OK is returned. Otherwise,
 *	TCL_ERROR is returned and an error message is left in
 *	interp->result.
 *
 * Side effects:
 *	The busy window is unmapped, allowing the parent window and
 *	its children to receive events again.
 *
 * -------------------------------------------------------------------
 */
static int
ReleaseBusyWindow(clientData, interp, pathName)
    ClientData clientData;	/* Main window of the interpreter */
    Tcl_Interp *interp;		/* Interpreter to report errors to */
    char *pathName;		/* Path name of host window */
{
    BusyWin *busyPtr;

    busyPtr = GetBusyWindow(clientData, interp, pathName);
    if (busyPtr == NULL) {
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)busyPtr);
    if ((busyPtr->mapped) && (busyPtr->window != None)) {
	busyPtr->mapped = FALSE;
	XUnmapWindow(busyPtr->display, busyPtr->window);
    }
    Tk_Release((ClientData)busyPtr);
    return TCL_OK;
}

/*
 * ------------------------------------------------------------------
 *
 * HoldBusyWindow --
 *
 *	Creates (if necessary) and maps a busy window, thereby
 *	preventing device events from being be received by the parent
 *      window and its children.
 *
 * Results:
 *	Returns a normal TCL result. If path name represents a busy
 *	window, it is unmapped and TCL_OK is returned. Otherwise,
 *	TCL_ERROR is returned and an error message is left in
 *	interp->result.
 *
 * Side effects:
 *	The busy window is created and mapped, blocking events from the
 *	parent window and its children.
 *
 * -------------------------------------------------------------------
 */
static int
HoldBusyWindow(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of the interpreter */
    Tcl_Interp *interp;		/* Interpreter to report errors to */
    int argc;
    char **argv;		/* Window name and option pairs */
{
    Tk_Window searchWin = (Tk_Window)clientData;
    Tk_Window tkwin;
    Tcl_HashEntry *entryPtr;
    BusyWin *busyPtr;
    int result;

    tkwin = Tk_NameToWindow(interp, argv[0], searchWin);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    entryPtr = Tcl_FindHashEntry(&busyWinTable, (char *)tkwin);
    if (entryPtr == NULL) {
	busyPtr = (BusyWin *)CreateBusyWindow(interp, tkwin, searchWin);
    } else {
	busyPtr = (BusyWin *)Tcl_GetHashValue(entryPtr);
    }
    if (busyPtr == NULL) {
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)busyPtr);
    result = Tk_ConfigureWidget(interp, tkwin, configSpecs, argc - 1, argv + 1,
	(char *)busyPtr, 0);
    if ((result == TCL_OK) && (busyPtr->window != None)) {
	/*
	 * Define the new cursor and map the busy window
	 */
	if (busyPtr->cursor != None) {
	    XDefineCursor(busyPtr->display, busyPtr->window, busyPtr->cursor);
	}
	XMapRaised(busyPtr->display, busyPtr->window);
	busyPtr->mapped = TRUE;
    }
    Tk_Release((ClientData)busyPtr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureBusyWindow --
 *
 *	This procedure is called to process an argv/argc list
 *	in order to configure (or reconfigure) a busy window.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information get set for busyPtr;  old resources
 *      get freed, if there were any.  The busy window may be reparented
 *      to a new parent window.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureBusyWindow(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of the interpreter */
    Tcl_Interp *interp;		/* Interpreter to report errors to */
    int argc;
    char **argv;		/* Host window path name and options */
{
    BusyWin *busyPtr;
    int result;

    busyPtr = GetBusyWindow(clientData, interp, argv[1]);
    if (busyPtr == NULL) {
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)busyPtr);
    if (argc == 2) {
	result = Tk_ConfigureInfo(interp, busyPtr->host, configSpecs,
	    (char *)busyPtr, (char *)NULL, 0);
    } else if (argc == 3) {
	result = Tk_ConfigureInfo(interp, busyPtr->host, configSpecs,
	    (char *)busyPtr, argv[2], 0);
    } else {
	Cursor oldCursor;

	oldCursor = busyPtr->cursor;
	result = Tk_ConfigureWidget(interp, busyPtr->host, configSpecs,
	    argc - 2, argv + 2, (char *)busyPtr, TK_CONFIG_ARGV_ONLY);
	if ((result == TCL_OK) && (busyPtr->window != None)) {
	    /* Set cursor on busy window */
	    if (busyPtr->cursor != None) {
		XDefineCursor(busyPtr->display, busyPtr->window,
		    busyPtr->cursor);
	    } else if (oldCursor != None) {
		XUndefineCursor(busyPtr->display, busyPtr->window);
	    }
	}
    }
    Tk_Release((ClientData)busyPtr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * BusyCmd --
 *
 *	This procedure is invoked to process the "busy" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */
static int
BusyCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of the interpreter */
    Tcl_Interp *interp;		/* Interpreter associated with command */
    int argc;
    char **argv;
{
    char c;
    int length;

    if (!initialized) {
	Tcl_InitHashTable(&busyWinTable, TCL_ONE_WORD_KEYS);
	initialized = 1;
    }
    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " option window\"", (char *)NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == '.') ||
	((c == 'h') && (strncmp(argv[1], "hold", length) == 0))) {
	register int i, count;
	char *savePtr;

	if (c == 'h') {
	    if (argc < 3) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " hold window ?options...?\"", (char *)NULL);
		return TCL_ERROR;
	    }
	    argc--, argv++;
	}
	for (i = 1; i < argc; i++) {
	    /*
	     * Find the end of the option-value pairs for this window.
	     */
	    for (count = i + 1; count < argc; count += 2) {
		if (argv[count][0] != '-') {
		    break;
		}
	    }
	    savePtr = argv[count];
	    argv[count] = NULL;
	    if (HoldBusyWindow(clientData, interp,
		    argc - i, argv + i) != TCL_OK) {
		return TCL_ERROR;
	    }
	    argv[count] = savePtr;
	    i = count;
	}
	return TCL_OK;
    } else if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)) {
	return (ConfigureBusyWindow(clientData, interp, argc - 1, argv + 1));
    } else if ((c == 'r') && (strncmp(argv[1], "release", length) == 0)) {
	register int i;

	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" release window ?window ...?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	for (i = 2; i < argc; i++) {
	    if (ReleaseBusyWindow(clientData, interp, argv[i]) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
	return TCL_OK;
    } else if ((c == 'f') && (strncmp(argv[1], "forget", length) == 0)) {
	register int i;

	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" forget window ?window ...?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	for (i = 2; i < argc; i++) {
	    if (ForgetBusyWindow(clientData, interp, argv[i]) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
	return TCL_OK;
    } else if ((c == 'h') && (strncmp(argv[1], "hosts", length) == 0)) {
	Tk_Window searchWin = (Tk_Window)clientData;
	Tcl_HashEntry *entryPtr;
	Tcl_HashSearch cursor;
	BusyWin *busyPtr;

	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" hosts ?pattern?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	for (entryPtr = Tcl_FirstHashEntry(&busyWinTable, &cursor);
	    entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&cursor)) {
	    busyPtr = (BusyWin *)Tcl_GetHashValue(entryPtr);
	    if (busyPtr->mainWin == searchWin) {
		if ((argc != 3) ||
		    (Tcl_StringMatch(Tk_PathName(busyPtr->host), argv[2]))) {
		    Tcl_AppendElement(interp, Tk_PathName(busyPtr->host));
		}
	    }
	}
	return TCL_OK;
    } else if ((c == 's') && (strncmp(argv[1], "status", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" status window\"", (char *)NULL);
	    return TCL_ERROR;
	}
	return (StatusBusyWindow(clientData, interp, argv[2]));
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1], "\" should be ",
	    "configure, forget, hold, hosts, or release", (char *)NULL);
	return TCL_ERROR;
    }
}

int
Blt_BusyInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, "blt_busy", (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_busy\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_busy\" requires Tk", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", "blt_busy", BUSY_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, "blt_busy", BusyCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}
