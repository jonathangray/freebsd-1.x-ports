/*
 * Copyright 1992 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

/*
 *                   The aXe Server Mode Implementation
 *
 * o aXe creates a server window and writes its ID as the value of the
 *   AXE_SERVER property for the host and user running aXe on the default root
 *   window, where the server window is the window of a widget that listens for
 *   AXE_COAXE, AXE_FAXE and AXE_POLEAXE property changes.  The AXE_SERVER
 *   property name for a host and user is AXE_SERVER_<hostname>_<user>
 *
 * o A client on some host wishing to be served reads the root window's
 *   AXE_SERVER_<hostname>_<user> property and writes the file, or files, to be
 *   edited and the ID of a window to be notified when editing has been
 *   completed as the value of the AXE_COAXE, AXE_FAXE or AXE_POLEAXE property
 *   of the server window.
 *
 * o The ID of the window to be notified is stored as the value of the
 *   AXE_COAXE property on the aXe window that is created for the client.  A
 *   client writes the AXE_FAXE property if it does not wait to be notified, in
 *   which case no property is stored on the aXe window.  A client writes the
 *   AXE_POLEAXE property in order to terminate the server.
 *
 * o There is no guarantee that the values of the root window's
 *   AXE_SERVER_<hostname>_<user> properties will be valid when read - aXe may
 *   have been terminated without the corresponding property being deleted and
 *   the window ID may even have been re-allocated in the meantime - so it is
 *   stamped with the AXE_SERVER_<hostname>_<user> property to enable a
 *   validity check to be carried out by the client.
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include <X11/Xp/Table.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <AxeIntrinsic.h>
#include <AxeWindow.h>
#include <AxeEditor.h>
#include <AxeMenuBtn.h>
#include <Confirmer.h>
#include <FileNomWin.h>
#include <FileNom.h>
#include <version.h>
#include <axeLogo.xbm>
#include <sys/param.h>
#include <stdio.h>

#ifdef EXTENSION
#include "Language.h"
#endif

#include "util.h"

/* R4 concession */
#ifndef XPointer
#define XPointer caddr_t
#endif

#define QUIT "QuitQuitQuitQuitQui"

typedef struct  _ResourceRec
{
    String        version;
    int           windows;
    Boolean       server;
    Boolean       fileServer;
    String        geometry;
} ResourceRec, *ResourceRecPtr;

static void AxeQuit(), AxeService();

static XtActionsRec actions[] = {
    "axe-quit",       AxeQuit,
    "axe-service",    AxeService,
};

static char translations[] = "\
                         <ClientMessage>: axe-quit() \n\
                         <PropertyNotify>AXE_COAXE:  axe-service(AXE_COAXE) \n\
                         <PropertyNotify>AXE_FAXE:   axe-service(AXE_FAXE)  \n\
                         <PropertyNotify>AXE_POLEAXE:axe-service(AXE_POLEAXE)";

/*ARGSUSED*/
static void
AxeQuit(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (event->xclient.message_type == XA_STRING &&
	                         strncmp(event->xclient.data.b, QUIT, 20) == 0)
    {
	exit(0);
    }
}

/*ARGSUSED*/
static void
AxeService(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Display *display = XtDisplay(widget);
    long propLen = 3;
    Atom gotAtom;
    int gotFormat;
    unsigned long gotItems, moreBytes; 
    unsigned char *propValue;
    char *p;
    int windows;
    Window wid;
    Widget newWindow;
    WidgetList axeEditor;

    if (strcmp(params[0], AXE_POLEAXE) == 0)
    {
        /*AxeExit();  Destroys remaining windows and notifies waiting coaxes */
        /* StopServer(display);   Wipes out AXE_SERVER_<hostname> property */
        exit(0);
    }
    
    /*
     * Do an initial read to get the window ID and calculate out how 
     * much there is left to be read in rounded up 32-bit chunks
     */
    if (XGetWindowProperty(display,
                           XtWindow(widget),
                           XInternAtom(display, params[0], False),
                           0, propLen,
                           False,
                           XA_STRING,
                           &gotAtom,
                           &gotFormat,
                           &gotItems,
                           &moreBytes,
                           &propValue) == Success)
    {
        if (gotAtom == XA_STRING)
        {
            sscanf((char *) propValue, "%12d", &wid);
	    propLen = (moreBytes + 4) / 4;
	    XtFree((char *) propValue);
	}
    }

    /* Read the remainder - the filename part - of the property */
    if (XGetWindowProperty(display,
                           XtWindow(widget),
                           XInternAtom(display, params[0], False),
                           (long) 3, propLen,
                           False,
                           XA_STRING,
                           &gotAtom,
                           &gotFormat,
                           &gotItems,
                           &moreBytes,
                           &propValue) == Success)
    {
        if (gotAtom == XA_STRING)
        {
	    for (p = (char *) propValue, windows = 0;
		 strlen(p) != 0  || (strlen(p) == 0 && windows == 0);
		 p += strlen(p) + 1, ++windows)
	    {
		newWindow = AxeEditorCreateWindow(widget, XtParent(widget), p);

		XtRealizeWidget(newWindow);

		if (strcmp(params[0], AXE_COAXE) == 0)
		{
		    XtVaGetValues(newWindow, XtNchildren, &axeEditor, NULL);
		    XChangeProperty(display,
				    XtWindow(AxeEditorEdWidget(axeEditor[0])),
				    XInternAtom(display, params[0], False),
				    XA_WINDOW,
				    32,
				    PropModeReplace,
				    (unsigned char *) &wid,
				    1);
		}

		XtPopup(newWindow, XtGrabNone);
	    }
        }
        XtFree((char *) propValue);
    }
}

/* ARGSUSED */
static void
NewWindow(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    XtPopup(AxeEditorCreateWindow((Widget) client_data, (Widget) client_data,
				             (String) call_data), XtGrabNone);
}

static void
Select(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    FileNominatorStruct *data = (FileNominatorStruct *) call_data;
    char filename[MAXPATHLEN];

    if (data->directoryPart && data->filenamePart
	                    && (data->filenameStatus & FileNominatorReadable))
    {
	strcpy(filename, data->directoryPart);
	strcat(filename, data->filenamePart);

	NewWindow(widget, client_data, (XtPointer) filename);
    }
    else
    {
	XBell(XtDisplay(widget), 100);
	return;
    }
}

/*ARGSUSED*/
static void
ShowMenuChange(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    XtVaSetValues(widget,
		  XtNsensitive, (int) call_data ? True : False,
		  NULL);
}

/*ARGSUSED */
static void
Poleaxe(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    exit(0);
}

static void
DelayQuit(widget)
     Widget widget;
{
    /*
     * Send a client message to the server window advising that Quit has been
     * invoked. The reason for delaying the Quit like this is so that coaxes
     * waiting for notification really get notified. Notification is carried
     * in the Destroy method of the AxeText widgets that are closed in the
     * Quit callback. As Destroy does not get called until the callback has
     * completed we can't call exit as early as the callback.
     */

    XClientMessageEvent clientEvent;

    clientEvent.type = ClientMessage;
    clientEvent.display = XtDisplay(widget);
    clientEvent.window = XtWindow(widget);
    clientEvent.message_type = XA_STRING;
    clientEvent.format = 8;
    strncpy(clientEvent.data.b, QUIT, 20);

    XSendEvent(XtDisplay(widget), XtWindow(widget),
	                          False, NoEventMask, (XEvent *) &clientEvent);
}

/* ARGSUSED */
static void
ReallyQuit(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    AxeEditorIterate(AxeForceClose);

    DelayQuit((Widget) client_data);
}

/* ARGSUSED */
static void
Quit(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    static Widget confirmer = (Widget) 0;

    if (!AxeEditorIterate(AxeSafeClose))
    {
	if (!confirmer)
	{
	    confirmer
		= XtVaCreatePopupShell("confirmer", confirmerWidgetClass,
				       (Widget) client_data, NULL);
	}
	
	ConfirmerRequestConfirmation(confirmer,
				     "There are unsaved changes",
				     "Quit anyway", ReallyQuit,
				     "unused", NULL,
				     client_data);
    }
    else
    {
	DelayQuit((Widget) client_data);
    }
}

/* ARGSUSED */
static void
Help(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    AxeEditorHelpWindow("Axeis", (Widget) client_data);
}

static XrmOptionDescRec options[] = {
    {"-buttons",      "*buttons",      XrmoptionNoArg,   (XPointer) "True"},
    {"-blinkPeriod",  "*blinkPeriod",  XrmoptionSepArg,  (XPointer) NULL},
    {"-fileTitle",    "*fileTitle",    XrmoptionNoArg,   (XPointer) "True"},
    {"-infoTimeout",  "*infoTimeout",  XrmoptionSepArg,  (XPointer) NULL},
    {"-miniBuffer",   "*suppressMinibuffer", XrmoptionNoArg,
	                                                 (XPointer) "False"},
    {"-noserver",     ".server",       XrmoptionNoArg,   (XPointer) "False"},
    {"-windows",      ".windows",      XrmoptionSepArg,  (XPointer) NULL},
};

static XtResource resources[] =
{
    { "version", "Version", XtRString, sizeof(String),
      XtOffset(ResourceRecPtr, version), XtRImmediate, (XtPointer) 0
    },
    { "geometry", "Geometry", XtRString, sizeof(String),
      XtOffset(ResourceRecPtr, geometry), XtRImmediate, (XtPointer) 0
    },
    { "server", "Server", XtRBoolean, sizeof(Boolean),
      XtOffset(ResourceRecPtr, server), XtRImmediate, (XtPointer) True
    },
    { "fileServer", "Server", XtRBoolean, sizeof(Boolean),
      XtOffset(ResourceRecPtr, fileServer), XtRImmediate, (XtPointer) False
    },
    { "windows", "Windows", XtRInt, sizeof(int),
      XtOffset(ResourceRecPtr, windows), XtRImmediate, (XtPointer) 0
    },
};

main(argc, argv)
     int argc;
     char **argv;
{
    XtAppContext app;
    Widget top, noapps, msg, server, fnom, table, waiter, poleaxe, help;
    Widget show = 0;
    Window serverWindow;
    Display *display;
    ResourceRec res;
    int parse, x, y;
    unsigned int width, height;
    char resource[MAXPATHLEN];
    Pixel fg, bg;
    int arg, argmost;
    Boolean buffering, newWindow;
    String nameList = 0;
    XrmDatabase db;

    top = XtVaAppInitialize(&app, "Axe",
                            options, XtNumber(options),
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
                            &argc,
#else
			    (Cardinal *) &argc,
#endif
			    argv,
                            NULL, NULL);
    display = XtDisplay(top);
    db = XtDatabase(display);

    XtAppAddActions(app, actions, XtNumber(actions));

    XtVaGetApplicationResources(top, (XtPointer) &res,
				resources, XtNumber(resources),
				NULL);

    if (!res.version || strcmp(res.version, QVERSION) != 0)
    {
	char message[1024];

        strcpy(message, "This is ");
        strcat(message, QVERSION);
        strcat(message, ".\n\n");
	strcat(message, "The application defaults  file\n");
	strcat(message, "for  this release has not been\n");
	strcat(message, "found.");

        if (res.version)
        {
	    strcat(message, "\n\nIt may help to know in solving\n");
	    strcat(message, "the problem that the  defaults\n");
	    strcat(message, "file for ");
            strcat(message, res.version);
            strcat(message, " was found.");
        }

	noapps = XtVaCreateManagedWidget("noapps", formWidgetClass, top, NULL);

	msg = XtVaCreateManagedWidget("msg", labelWidgetClass, noapps,
				      XtNlabel, message,
				      NULL);

	poleaxe = XtVaCreateManagedWidget("Quit", commandWidgetClass, noapps,
					  XtNfromVert, msg,
					  NULL);
	XtAddCallback(poleaxe, XtNcallback, Poleaxe, (XtPointer) 0);
	
	XtRealizeWidget(top);
	XtAppMainLoop(app);
    }

    if (res.geometry)
    {
	parse = XParseGeometry(res.geometry, &x, &y, &width, &height);
	if (parse & WidthValue)
	{
	    sprintf(resource, "*AxeText*columns:%d", width);
	    XrmPutLineResource(&db, resource);
	}
	if (parse & HeightValue)
	{
	    sprintf(resource, "*AxeText*rows:%d", height); 
	    XrmPutLineResource(&db, resource);
	}
    }

    if (res.fileServer)
    {
	server = XtVaCreatePopupShell("fileServer",
				      fileNominatorWindowWidgetClass,
				      top, NULL);

	fnom = FileNominatorWindowFileNominatorWidget(server);
	XtAddCallback(fnom, XtNselectCallback, Select, (XtPointer) top);
	XtAddCallback(fnom, XtNcancelCallback, Quit, (XtPointer) server);
	res.server = True;
    }
    else if (res.server)
    {
	server = XtVaCreatePopupShell("server", topLevelShellWidgetClass,
				      top, NULL);

	table = XtVaCreateManagedWidget("table", xpTableWidgetClass, server,
					XtNshrinkSimple, False,
					NULL);

	waiter = XtVaCreateManagedWidget("waiter", commandWidgetClass, table,
					 NULL);
	XtAddCallback(waiter, XtNcallback, NewWindow, (XtPointer) top);

	XtVaGetValues(waiter,
		      XtNforeground, &fg,
		      XtNbackground, &bg,
		      NULL);

	XtVaSetValues(waiter,
		      XtNbackgroundPixmap,
		      XCreatePixmapFromBitmapData(display, XDefaultRootWindow(display), (char *) axeLogo_bits, axeLogo_width, axeLogo_height, fg, bg, XDefaultDepthOfScreen(XtScreen(waiter))),
		      NULL);

        show = XtVaCreateManagedWidget("show", axeMenuButtonWidgetClass, table,
				XtNsensitive, False,
				NULL);
	AxeEditorAddShowMenuCallback(show, ShowMenuChange, (XtPointer) 0);

	poleaxe = XtVaCreateManagedWidget("poleaxe", commandWidgetClass, table,
					  NULL);
	XtAddCallback(poleaxe, XtNcallback, Quit, (XtPointer) server);

	help
	    = XtVaCreateManagedWidget("help", commandWidgetClass, table, NULL);
	XtAddCallback(help, XtNcallback, Help, (XtPointer) top);
    }
    
    if (res.server)
    {
	XtOverrideTranslations(server, XtParseTranslationTable(translations));

	XtRealizeWidget(server);
	serverWindow = XtWindow(server);

	TrapErrors(display);

	/* If there is already a server running this one will replace it */
	XChangeProperty(display,
			XDefaultRootWindow(display),
			HostUserServerAtom(display),
			XA_WINDOW,
			32,
			PropModeReplace,
			(unsigned char *) &serverWindow,
			1);

	if (xerror == Success)
	{
	    XChangeProperty(display,
			    serverWindow,
			    HostUserServerAtom(display),
			    XA_STRING,
			    32,
			    PropModeReplace,
			    NULL,
			    0);
	}

	DontTrapErrors(display);
    }
    else
    {
	XrmPutLineResource(&db, "*ExitOnCloseLastWindow:True");
    }

#ifdef EXTENSION
    interpreter.Initialize(top);
#endif

    argmost = (res.windows) ? res.windows : (res.server ? 0 : 1);
    for (buffering = False, arg = 1;  arg < argc;  ++arg)
    {
	if (strncmp(argv[arg], "-buffer", strlen(argv[arg])) == 0)
	{
	    buffering = True;
	}

	if (buffering)
	{
	    --argmost;
	}
    }

    if (argc == 1 && !res.server)
    {
	for (arg = 1;  arg <= argmost;  ++arg)
	{
	    NewWindow((XtPointer) 0, (XtPointer) top, (XtPointer) 0);
	}
    }
    else
    {
	for (arg = argc - 1;  arg < argmost;  ++arg)
	{
	    NewWindow((XtPointer) 0, (XtPointer) top, (XtPointer) 0);
	}

	for (buffering = False, newWindow = True, arg = 1;  arg < argc;  ++arg)
	{
	    if (strncmp(argv[arg], "-buffer", strlen(argv[arg])) == 0)
	    {
		buffering = True;
		newWindow = True;
		if (nameList)
		{
		    NewWindow((XtPointer) 0, (XtPointer) top,
			                                 (XtPointer) nameList);
		    XtFree(nameList);
		    nameList = 0;
		}
		continue;
	    }

	    if (buffering)
	    {
		if (newWindow)
		{
		    nameList = XtMalloc((unsigned) (strlen(argv[arg]) + 1));
		    strcpy(nameList, argv[arg]);
		    newWindow = False;
		}
		else
		{
		    int len = strlen(nameList);

		    nameList
			= XtRealloc(nameList, 
				 (unsigned) (len + 1 + strlen(argv[arg]) + 1));
		    nameList[len] = ' ';
		    strcpy(&nameList[len + 1], argv[arg]);
		}
	    }
	    else
	    {
		NewWindow((XtPointer) 0, (XtPointer) top,
			                                (XtPointer) argv[arg]);
	    }
	}
	if (nameList)
	{
	    NewWindow((XtPointer) 0, (XtPointer) top, (XtPointer) nameList);
	    XtFree(nameList);
	}
    }

    if (res.server)
    {
	int argc;
	String *argv;
	
	XtVaGetValues(top, XtNargc, &argc, XtNargv, &argv, NULL);
	XSetCommand(display, XtWindow(server), argv, argc);

	XtPopup(server, XtGrabNone);
    }

    XtAppMainLoop(app);
}
