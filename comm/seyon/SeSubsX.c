
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include <time.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "seyon.h"
#include "SeDecl.h"

extern char    *strsqtok();
extern int      GetModemStat();

int             seyon_message_up = 0;

void
UpdateStatusBox(clientData)
     XtPointer       clientData;
{
  static Widget *statusWidget;
  /* All members of oldState are guaranteed to be initialized to False
	 since the array is declared static, similarly for online */
  static Boolean online, oldState[NUM_MDM_STAT];
  Boolean newState;
  char buf[10];
  static int stat[NUM_MDM_STAT] = {MDM_DCD, MDM_DTR, MDM_DSR, MDM_RTS, 
									 MDM_CTS, MDM_RNG};
  int modemStat, i;
  static time_t startTime, oldOnlineTime = 0;
  time_t onlineTime = 0;

  if (clientData)
	statusWidget = (Widget*)clientData;

  if ((modemStat = GetModemStat(0)) >= 0)
	for (i = 0; i < 6; i++)
	  if ((newState = (Boolean)((modemStat & stat[i]) ? True : False)) != 
		  oldState[i])
		SeSetUnsetToggle(statusWidget[i], (oldState[i] = newState));
  
  if (online == False) {
	onlineTime = 0;
	startTime = time((time_t*)0);
  }
  if ((online = oldState[0]))
	onlineTime = (time((time_t*)0) - startTime) / 60;

  if (onlineTime != oldOnlineTime) {
	oldOnlineTime = onlineTime;
	sprintf(buf, "%02d:%02d", onlineTime / 60, onlineTime % 60);
	SeSetLabel(statusWidget[0], buf);
  }

  if (clientData)
	XtAppAddTimeOut(app_con, qres.modemStatusInterval * 1000, 
					UpdateStatusBox, clientData);
}

void
FunMessage()
{
  static int      msg_index = 0;
  String          msg;
  char            vermsg[SM_BUF];

  if (seyon_message_up <= 0) {

    msg = qres.funMessages[msg_index++];
    if (msg == NULL) {
      msg_index = 0;
      sprintf(vermsg, "Welcome to Seyon version %s%s", VERSION, REVISION);
      msg = vermsg;
    }

    SetStatusMessage(msg);
  }

  seyon_message_up--;

  XtAppAddTimeOut(XtWidgetToApplicationContext(topLevel),
				  qres.funMessagesInterval * 1000, FunMessage, NULL);
}

struct _procRequest {
  int             action;
  char            msg[80];
  char            arg[90];
};

void
ExecProcRequest(client_data)
     XtPointer       client_data;
{
  void                DispatchActions(),
                      RunScript();
  struct _procRequest procRequest;

  read_pipe_data(child_pipe, &procRequest, sizeof(procRequest));

  if (*(procRequest.msg))
    SeyonMessage(procRequest.msg);

  switch (procRequest.action) {
  case KILL_TERM:
    KillTerminal();
    break;
  case START_TERM:
    StartTerminal();
    break;
  case SUSPEND_TERM:
	SuspContTerminal(0);
    break;
  case CONTINUE_TERM:
	SuspContTerminal(1);
    break;
  case DISPATCH_ACTION:
	DispatchActions(ACTION_DISPATCH, procRequest.arg, genericWidget);
    break;
  case EXEC_SCRIPT:
	RunScript(NULL, procRequest.arg);
	break;
  case TOP_DIAL_START:
	qres.dialAutoStart = True;
  case TOP_DIAL:
	if ((procRequest.arg)[0]) TopDial(dialWidget, procRequest.arg);
	else TopDial(dialWidget, NULL);
	break;
  case POPUP_ERROR:
	PopupError(procRequest.arg, NULL);
	break;
  case EXIT_PROGRAM:
	s_exit();
    break;
  case SET_MESSAGE:
    break;
  default:
    break;
  }
}

void
write_child_info(pd, action, msg)
     int            *pd;
     int             action;
     char           *msg;
{
  struct _procRequest procRequest;

  procRequest.action = action;

  if (msg) strcpy(procRequest.msg, msg);
  else *procRequest.msg = '\0';

  write_pipe_data(pd, &procRequest, sizeof(procRequest));
}

void
ProcRequest(action, msg, arg)
     int             action;
     char           *msg,
                    *arg;
{
  struct _procRequest procRequest;

  procRequest.action = action;
  strcpy(procRequest.msg, msg);
  strcpy(procRequest.arg, arg);

  write_pipe_data(child_pipe, &procRequest, sizeof(procRequest));
}

void
writef_child_info(pd, action, fmt, a, b, c)
     int            *pd;
     int             action;
     char           *fmt,
                    *a,
                    *b,
                    *c;
{
  char            buffer[SM_BUF];

  sprintf(buffer, fmt, a, b, c);
  write_child_info(pd, action, buffer);
}

void
SeyonMessage(msg)
     String          msg;
{
  seyon_message_up = 300 / qres.funMessagesInterval;
  SetStatusMessagef("- %s -", msg, "", "");
}

void
SeyonMessagef(fmt, a, b, c)
     String          fmt,
                     a,
                     b,
                     c;
{
  char            buf[REG_BUF];

  sprintf(buf, fmt, a, b, c);
  SeyonMessage(buf);
}

Boolean
read_seyon_file(name, line)
     char           *name,
                    *line[];
{
  FILE           *fp;

  if ((fp = open_file(name, qres.defaultDirectory)) == NULL)
    return False;

  ReadCommentedFile(fp, line);
  fclose(fp);

  return True;
}

#define done(value, type) \
{ \
  if (toVal->addr != NULL) { \
	if (toVal->size < sizeof(type)) { \
	  toVal->size = sizeof(type); \
	  return False; \
	} \
	*(type*)(toVal->addr) = (value); \
  } \
  else { \
    static type static_val; \
    static_val = (value); \
    toVal->addr = (XtPointer)&static_val; \
  } \
  toVal->size = sizeof(type); \
  return True; \
}

Boolean
CvtStringToStringArray(display, args, num_args, fromVal, toVal,
		       converter_data)
     Display        *display;
     XrmValue       *args;
     Cardinal       *num_args;
     XrmValue       *fromVal;
     XrmValue       *toVal;
     XtPointer      *converter_data;
{
  String          fromStr,
                  buf;
  static String   strArr[REG_BUF];
  int             n;

  if (*num_args != 0)
    XtAppWarningMsg(app_con, "wrongParameters", "cvtStringToStringArray",
		    "XtToolkitError",
		"String to StringArray conversion needs no extra arguments",
		    (String *) NULL, (Cardinal *) NULL);

  fromStr = (String) fromVal->addr;
/*  buf = XtMalloc((strlen(fromStr)+1) * sizeof(char));
  strcpy(buf, fromStr);*/
  buf = fromStr;

  if ((strArr[0] = strsqtok(buf)) == NULL) {
/*	XtFree(buf);*/
    done(NULL, String *);
  }

  for (n = 1; n < XtNumber(strArr); n++)
    if ((strArr[n] = strsqtok(NULL)) == NULL)
      done(strArr, String *);

  XtAppWarningMsg(app_con, "tooManyStrings", "cvtStringToStringArray",
		  "XtToolkitError",
	   "Too many strings specified for String to StringArray converter",
		  (String *) NULL, (Cardinal *) NULL);
  strArr[--n] = NULL;
  done(strArr, String *);
}

#undef done
