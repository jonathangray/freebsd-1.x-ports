#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <sys/param.h>
#include <AxeEditor.h>
#include <AxeiiText.h>

extern char *getenv();

#include "Language.h"

static void InterpFocus(), Execute();

static XtActionsRec actions[] = {
    "interp-focus", InterpFocus,
    "execute",      Execute,
};

/*ARGSUSED*/
static void
InterpFocus(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (!XtIsSubclass(widget, axeEditorWidgetClass))
    {
	return;
    }

    interpreter.SetBuffer(AxeEditorEdWidget(widget));
}

/*ARGSUSED*/
static void
Execute(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
#if TCL_MAJOR_VERSION < 7
    Tcl_CmdBuf assembley;
    Cardinal param;
    char *cmd;

    if (*num_params == 0)
    {
        return;
    }

    assembley = Tcl_CreateCmdBuf();
    for (param = 0;  param < *num_params;  ++param)
    {
        cmd = Tcl_AssembleCmd(assembley, params[param]);
        cmd = Tcl_AssembleCmd(assembley, " ");
    }
    cmd = Tcl_AssembleCmd(assembley, "");

    (void) Tcl_Eval(interpreter.interp, cmd, 0, NULL);

    Tcl_DeleteCmdBuf(assembley);
#else
    Tcl_DString assembley;
    Cardinal param;

    if (*num_params == 0)
    {
        return;
    }

    Tcl_DStringInit(&assembley);
    for (param = 0;  param < *num_params;  ++param)
    {
        /*(void) Tcl_DStringAppendElement(&assembley, params[param], -1);*/
        (void) Tcl_DStringAppendElement(&assembley, params[param]);
    }
    
    (void) Tcl_Eval(interpreter.interp, assembley.string);

    Tcl_DStringFree(&assembley);
#endif
}

/*ARGSUSED*/
static int 
Action(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    Window root, child;
    unsigned int mask;
    XEvent event;

    if (argc < 2)
    {
	interpreter.interp->result = "Usage: action arg [arg]";
        return TCL_ERROR;
    }

    XQueryPointer(XtDisplay(interpreter.buffer), XtWindow(interpreter.buffer),
		  &root, &child,
                  &event.xbutton.x_root, &event.xbutton.y_root,
                  &event.xbutton.x, &event.xbutton.y, &mask);
    event.type = ButtonPress;

    XtCallActionProc(interpreter.buffer, argv[1], &event, &argv[2], argc - 2);

    return TCL_OK;
}

/*ARGSUSED*/
static int 
GetPosition(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    sprintf(interpreter.interp->result, "%d",
	                         XawTextGetInsertionPoint(interpreter.buffer));
    return TCL_OK;
}

/*ARGSUSED*/
static int 
SetPosition(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    if (argc < 2)
    {
	interpreter.interp->result = "Usage: set-position arg";
	return TCL_ERROR;
    }

    XawTextSetInsertionPoint(interpreter.buffer,
			                      (XawTextPosition) atoi(argv[1]));
    return TCL_OK;
}

/*ARGSUSED*/
static int 
GetSelection(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    Widget source = XawTextGetSource(interpreter.buffer);
    String selection, s;
    XawTextPosition start, finish;
    int lines = 0;

    XawTextGetSelectionPos(interpreter.buffer, &start, &finish);
    selection = AxeiiTextRead(interpreter.buffer, start, finish);

    for (s = selection;  *s != 0;  ++s)
    {
	if (*s == '\n')
	{
	    ++lines;
	}
    }

    if (*(s - 1) != '\n')
    {
	++lines;
    }
    
    sprintf(interpreter.interp->result, "%d %d %d ", start, finish, lines);

    Tcl_AppendElement(interpreter.interp, selection);

    XtFree(selection);

    return TCL_OK;
}

/*ARGSUSED*/
static int 
SetSelection(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    Widget source = XawTextGetSource(interpreter.buffer);

    if (argc < 3)
    {
	interpreter.interp->result = "Usage: set-selection arg arg";
	return TCL_ERROR;
    }

    XawTextSetSelection(interpreter.buffer, (XawTextPosition) atoi(argv[1]),
			                    (XawTextPosition) atoi(argv[2]));

    return TCL_OK;
}

/*ARGSUSED*/
static int 
EvalBuffer(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    int result;
    String script =
	AxeiiTextRead(interpreter.buffer,
		      (XawTextPosition) 0,
		      XawTextSourceScan(XawTextGetSource(interpreter.buffer),
					(XawTextPosition) 0,
					XawstAll,
					XawsdRight,
					1,
					True));
#if TCL_MAJOR_VERSION < 7
    result = Tcl_Eval(interpreter.interp, script, 0, NULL);
#else
    result = Tcl_Eval(interpreter.interp, script);
#endif

    XtFree(script);

    return result;
}

static int
SetOneValue(widget, argc, argv)
     Widget widget;
     int argc;
     char *argv[];
{
    if (argc < 3)
    {
	interpreter.interp->result = "Usage: set-value arg arg";
	return TCL_ERROR;
    }

    XtVaSetValues(widget,
		  XtVaTypedArg, argv[1], XtRString, argv[2], strlen(argv[2]),
		  NULL);
    
    return TCL_OK;
}

/*ARGSUSED*/
static int 
SetValue(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    return SetOneValue(interpreter.buffer, argc, argv);
}

/*ARGSUSED*/
static int 
SetSourceValue(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    return SetOneValue(XawTextGetSource(interpreter.buffer), argc, argv);
}

/*ARGSUSED*/
static int 
SetSinkValue(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    Widget sink;

    XtVaGetValues(interpreter.buffer, XtNtextSink, &sink, NULL);
    
    return SetOneValue(sink, argc, argv);
}

static void 
SetBuffer(buffer)
     Widget buffer;
{
    interpreter.buffer = buffer;
}

static XtResource resources[] = {
    { "axeLibDir", "AxeLibDir", XtRString, sizeof(String),
      (Cardinal) 0, XtRImmediate, (XtPointer) 0
    }
};

static void
Initialize(widget)
     Widget widget;
{
    XtAppContext app = XtWidgetToApplicationContext(widget);
    char tclFile[MAXPATHLEN], *home, *tcl;

    Tcl_Interp *interp = interpreter.interp = Tcl_CreateInterp();

    Tcl_CreateCommand(interp, "action", Action, (ClientData) 0,
                                                   (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "get-selection", GetSelection, (ClientData) 0,
                                                   (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "set-selection", SetSelection, (ClientData) 0,
                                                   (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "get-position", GetPosition, (ClientData) 0,
                                                   (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "set-position", SetPosition, (ClientData) 0,
                                                   (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "eval-buffer", EvalBuffer, (ClientData) 0,
                                                   (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "set-value", SetValue, (ClientData) 0,
                                                   (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "set-source-value", SetSourceValue,
		                   (ClientData) 0, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "set-sink-value", SetSinkValue, (ClientData) 0,
                                                   (Tcl_CmdDeleteProc *) NULL);

    XtVaGetApplicationResources(widget, (XtPointer) &tcl,
                                resources, XtNumber(resources),
                                NULL);
    strcpy(tclFile, tcl);
    strcat(tclFile, "/axe.tcl");
    (void) Tcl_EvalFile(interp, tclFile);

    home = getenv("HOME");
    strcpy(tclFile, home);
    strcat(tclFile, "/axe.tcl");
    (void) Tcl_EvalFile(interp, tclFile);

    XtAppAddActions(app, actions, XtNumber(actions));
}

InterpRec interpreter = {
    Initialize,
    (Tcl_Interp *) 0,
    SetBuffer,
    (Widget) 0,
};
