
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>

/* SeDecl.h includes stdio.h */
#include "SeDecl.h"

#define CheckNumParam(num) {if (*numParam != num) \
  SimpleError("Wrong Number of Parameters");}

extern Widget   dialWidget;

void
CallCallbacksAction(widget)
	 Widget          widget;
{
  XtCallCallbacks(widget, XtNdestroyCallback, NULL);
}

void
BeepAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  CheckNumParam(0);
  Beep();
}

void
CloseWindowAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  int             DismissDirectory();
  Widget          dirWidget;
  int             i;

  for (i = 0; i < *numParam; i++) {
	if (strcmp(param[i], "Dial") == 0)
	  {if ((dirWidget = XtNameToWidget(dialWidget, "directory")))
		 DismissDirectory(dirWidget);}
	
	else SimpleErrorF("Invalid Parameter `%s'", param[i], "", "");
  } /* for... */
}

void
DialEntriesAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  ErrorIfBusy();
  CheckNumParam(1);

  if (strcmp(*param, "Default")) TopDial(dialWidget, (XtPointer)*param);
  else TopDial(dialWidget, (XtPointer)qres.defaultPhoneEntries);
}

void
DivertFileAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  void            DivertFile(),
                  ExecDivertFile();
         
  ErrorIfBusy();
  if (*numParam > 1)
	SimpleError("Wrong Number of Parameters");

  if (*numParam == 0) DivertFile(widget);
  else ExecDivertFile(widget, *param);
}

void
DoNothingAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  CheckNumParam(0);
}

void
EchoAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  if (*numParam == 0) show("");
  CheckNumParam(1);
  show(*param);
}

void
FileTransferAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  void            TopTransfer();
  static String   pParam[2];
  int             i;

  ErrorIfBusy();
  if (*numParam < 1 || *numParam > 2)
	SimpleError("Wrong Number of Parameters");

  for (i = 0; i < *numParam;) 
	{pParam[i] = param[i]; pParam[++i] = NULL;}

  TopTransfer(widget, (XtPointer)pParam);
}

void
IconifyWindowAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  int             IconifyShell();
  Widget          dirWidget;
  static String   termWindowId = NULL;
  int             i;

  for (i = 0; i < *numParam; i++) {
	if (strcmp(param[i], "Main") == 0) IconifyShell(widget);

	else if (strcmp(param[i], "Dial") == 0)
	  {if ((dirWidget = XtNameToWidget(dialWidget, "directory")))
		 IconifyShell(dirWidget);}

	else if (strcmp(param[i], "Term") == 0) {
	  if (termWindowId == NULL) termWindowId = (String)getenv("WINDOWID");
	  if (termWindowId) 
		XIconifyWindow(XtDisplay(widget), (Window)atol(termWindowId),
					   XScreenNumberOfScreen(XtScreen(widget)));
	  else {
		SeError("Could not get terminal window ID");
		SeNotice("Maybe you're not using xterm?");
		SimpleError("WINDOWID not Found");
	  }
	} /* if strcmp Term... */

	else SimpleErrorF("Invalid Parameter `%s'", param[i], "", "");
  } /* for... */
}

void
HangupAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  void            ExecHangup();

  ErrorIfBusy();
  CheckNumParam(0);
  ExecHangup();
}

void
ManualDialAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  void            ManualDial(),
                  ExecManualDial();

  ErrorIfBusy();
  if (*numParam > 1)
	SimpleError("Wrong Number of Parameters");

  if (*numParam == 0) ManualDial(widget);
  else ExecManualDial(widget, *param);
}

void
MessageAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  if (*numParam == 0) SeyonMessage("");
  CheckNumParam(1);
  SeyonMessage(*param);
}

void
OpenWindowAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  int             IconifyShell();
  Widget          dirWidget;
  static String   termWindowId = NULL;
  int             i;

  for (i = 0; i < *numParam; i++) {
	if (strcmp(param[i], "Main") == 0) XtMapWidget(GetShell(widget));

	else if (strcmp(param[i], "Dial") == 0)
	  if ((dirWidget = XtNameToWidget(dialWidget, "directory")))
		{XtPopup(dirWidget, XtGrabNone); XtMapWidget(dirWidget);}
	  else TopDial(dialWidget, NULL);

	else if (strcmp(param[i], "Term") == 0) {
	  if (termWindowId == NULL) termWindowId = (String)getenv("WINDOWID");
	  if (termWindowId) 
		XMapRaised(XtDisplay(widget), (Window)atol(termWindowId));
	  else {
		SeError("Could not get terminal window ID");
		SeNotice("Maybe you're not using xterm?");
		SimpleError("WINDOWID not Found");
	  }
	} /* if strcmp Term... */

	else SimpleErrorF("Invalid Parameter `%s'", param[i], "", "");
  } /* for... */
}

void
QuitAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  ErrorIfBusy();
  CheckNumParam(0);
  ExecExit();
}

void
RestartTerminalAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  ErrorIfBusy();
  CheckNumParam(0);
  RestartTerminal();
}

void
RunScriptAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  void            DialogRunScript(),
                  RunScript();
           
  ErrorIfBusy();
  if (*numParam == 0) DialogRunScript(widget);
  CheckNumParam(1);
  RunScript(widget, *param);
}

void
SetAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  void            s_set();

  ErrorIfBusy();
  CheckNumParam(2);
  sprintf((lptr = line), "%s %s", param[0], param[1]);
  eof_flag = 0;
  s_set();
}

void
ShellCommandAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  ErrorIfBusy();
  CheckNumParam(1);
  ShellCommand(*param);
}

void
SleepAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  CheckNumParam(1);
  sleep(atoi(*param));
}

void
TransmitAction(widget, event, param, numParam)
	 Widget          widget;
	 XEvent*         event;
	 String*         param;
	 Cardinal*       numParam;
{
  ErrorIfBusy();
  CheckNumParam(1);
  MdmPutString(*param);
}

/*---------------------------------------------------------------------------+
| DispatchActions - parses an action stack and dispatches its actions.
+---------------------------------------------------------------------------*/

void
DispatchActions(intData, stringData, widget)
	 int    intData;
	 String stringData;
	 Widget widget;
{
  int                 SeAppMSleep();
  void                ParseThis();

  static Boolean      actionRunning = False,
                      prevActionAsync = False,
                      startup = True;
  static Widget       actionWidget;
  static String       actionStack;

  void                (*actionProc)();
  static char         actionName[SM_BUF],
                      args[SM_BUF][SM_BUF];
  static String       argsArray[SM_BUF];
  static Cardinal     numArgs;
  int                 i;

  struct _actionTable {
	String        actionKeyWord;
	void          (*actionProc)();
	Boolean       async;
  };

  static struct _actionTable actionTable[] = {
    {"Beep", BeepAction, False},
    {"CloseWindow", CloseWindowAction, False},
    {"DialEntries", DialEntriesAction, True},
    {"DivertFile", DivertFileAction, True},
    {"DoNothing", DoNothingAction, False},
    {"Echo", EchoAction, False},
	{"FileTransfer", FileTransferAction, True},
    {"IconifyWindow", IconifyWindowAction, False},
    {"Hangup", HangupAction, False},
    {"ManualDial", ManualDialAction, True},
    {"Message", MessageAction, False},
    {"OpenWindow", OpenWindowAction, False},
    {"Quit", QuitAction, False},
    {"RestartTerminal", RestartTerminalAction, False},
    {"RunScript", RunScriptAction, True},
    {"Set", SetAction, False},
    {"ShellCommand", ShellCommandAction, True},
    {"Sleep", SleepAction, False},
    {"Transmit", TransmitAction, False},
	{NULL, NULL, False},
  };

  switch (intData) {

  case ACTION_NEW_ACTION:
	strcpy(actionName, stringData);
	numArgs = 0;
	return;

  case ACTION_NEW_ARG:
	strcpy((argsArray[numArgs] = args[numArgs]), stringData);
	numArgs++;
	return;

  case ACTION_ARGS_END:
	if (prevActionAsync) 
	  while(inhibit_child) 
		XtAppProcessEvent(XtWidgetToApplicationContext(actionWidget), XtIMAll);

	for (i = 0; (actionProc = actionTable[i].actionProc) && 
		 strcmp(actionName, actionTable[i].actionKeyWord); i++);

	if (actionProc == NULL)
	  {SeError(FmtString("Invalid action: ``%s''", actionName, "", ""));
	   SimpleError("Invalid Action Specified");}

	(*actionProc)(actionWidget, NULL, argsArray, &numArgs);
	prevActionAsync = actionTable[i].async;
	return;

  case ACTION_PARSE_ERROR:
	SeError(FmtString("%s in ``%s''", stringData, actionStack, ""));
	return;

  case ACTION_DISPATCH:
	if (actionRunning)
	  SimpleError("Action Still Running");
	if (stringData == NULL) {
	  SeError("Serious (report it): stringData = NULL in DispatchActions");
	  return;
	}

	actionWidget = widget;
	actionStack = XtNewString(stringData);

	actionRunning = True;
	prevActionAsync = False;
	ParseThis(actionStack, DispatchActions);

	actionRunning = False;
	XtFree(actionStack);

	if (startup) {
	  startup = False;
	  ParseThis(FmtString("Message(\"Welcome to Seyon version %s%s\"); %s",
						  VERSION, REVISION, "RestartTerminal();"), 
				DispatchActions);
	}
	  
	return;
  }
}

/*---------------------------------------------------------------------------+
| DispatchActionsCallback - callback for dispatching SeQuickKey actions.
+---------------------------------------------------------------------------*/

void
DispatchActionsCallback(widget, clientData)
	 XtPointer widget;
	 XtPointer clientData;
{
  void                GetQuickKeyResources();

  String              widgetName;
  struct _quickKeyRes quickKeyRes;

  GetQuickKeyResources((widgetName = XtName(widget)), &quickKeyRes);  

  if (quickKeyRes.action == NULL || quickKeyRes.action[0] == '\0')
	SimpleError("No Action Attached");

  DispatchActions(ACTION_DISPATCH, quickKeyRes.action, widget);
}


