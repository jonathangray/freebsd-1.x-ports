
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Dialog.h>
#include <signal.h>
#include <fcntl.h>

#include "seyon.h"
#include "SeDecl.h"

#ifndef HELPFILE
#define HELPFILE "/usr/lib/X11/seyon.help"
#endif

extern void     TopAbout(),
                TopHelp(),
                TopSet(),
                TopTransfer(),
                TopMisc(),
                SetNewlineTrMode(),
                FunMessage();

extern void     upload_acc_ok(),
                setVal_action_ok(),
                manual_dial_action_ok(),
                divert_action_ok();

extern Boolean  CvtStringToStringArray();

void            HangupConfirm(),
                ExecHangup(),
                ExitConfirm(),
                ExitAction(),
                test();

Boolean         inhibit_child = False;
Widget          w_exit,
                w_kill,
                statusMessage;
Widget          dialWidget, genericWidget;
pid_t           w_child_pid = 0;
Pixmap          progIcon;

#include "progIcon.h"

void
InitVariables(topLevel)
     Widget          topLevel;
{
  char            buffer[REG_BUF];

  sprintf(captureFile, "%s/%s", expand_fname(qres.defaultDirectory, buffer),
	  qres.captureFile);

  SetNewlineTrMode(qres.newlineTranslation);
}

void
SetIcon(topLevelWidget)
     Widget          topLevelWidget;
{
  progIcon = 
	XCreateBitmapFromData(XtDisplay(topLevelWidget),
						  DefaultRootWindow(XtDisplay(topLevelWidget)),
						  progIcon_bits, progIcon_width,
						  progIcon_height);
  XtVaSetValues(topLevelWidget, XtNiconPixmap, progIcon, NULL);
}

void
CreateCommandCenter()
{
  void            GetQuickKeyResources(),
                  GetValueByPopupOKAction(),
                  DispatchActionsCallback();
  void            TopShell();
  void            KillChildProc();

  Widget          mainBox,
                  menuBox,
                  statusBox,
                  messageBox,
                  quickKeyBox;
  static Widget   status[NUM_MDM_STAT];
  static Atom     wm_delete_window;
  Dimension       menuBoxWidth;

  struct _quickKeyRes quickKeyRes;
  Widget          quickKeyW;
  char            qKBuf[REG_BUF];
  int             i, n = 0;

  static XtActionsRec actionTable[] = {
    {"UploadOk", upload_acc_ok},
    {"SetValOk", setVal_action_ok},
    {"ManualDialOk", manual_dial_action_ok},
    {"DivertOk", divert_action_ok},
    {"Exit", ExitAction},
    {"GetValueByPopupOK", GetValueByPopupOKAction},
  };

  XtAppAddActions(XtWidgetToApplicationContext(topLevel), actionTable, 
				  XtNumber(actionTable));

  mainBox = SeAddPaned("mainBox", topLevel);
  statusBox = AddBox("statusBox", mainBox);
  messageBox = SeAddForm("messageBox", mainBox);
  quickKeyBox = AddBox("quickKeyBox", mainBox);
  menuBox = AddBox("menuBox", mainBox);

  status[0] = SeAddToggle("dcd", statusBox, NULL);
  status[1] = SeAddToggle("dtr", statusBox, NULL);
  status[2] = SeAddToggle("dsr", statusBox, NULL);
  status[3] = SeAddToggle("rts", statusBox, NULL);
  status[4] = SeAddToggle("cts", statusBox, NULL);
  status[5] = SeAddToggle("rng", statusBox, NULL);

  statusMessage = SeAddLabel("message", messageBox);

  AddButton("about", menuBox, TopAbout, NULL);
  AddButton("help", menuBox, TopHelp, NULL);
  AddButton("set", menuBox, TopSet, NULL);
  genericWidget = dialWidget = AddButton("dial", menuBox, TopDial, NULL);
  AddButton("transfer", menuBox, TopTransfer, NULL);
  AddButton("shellCommand", menuBox, TopShell, NULL);
  AddButton("misc", menuBox, TopMisc, NULL);
  AddButton("hangup", menuBox, HangupConfirm, NULL);
  w_exit = AddButton("exit", menuBox, ExitConfirm, NULL);
  w_kill = AddButton("kill", menuBox, KillChildProc, NULL);

  for (i = 0; i < MAX_SEQUICKKEYS; i++) {
	sprintf(qKBuf, "quickKey%d", i+1);
	GetQuickKeyResources(qKBuf, &quickKeyRes);
	if (quickKeyRes.visible) {
      n++;
	  quickKeyW = 
		AddButton(qKBuf, quickKeyBox, DispatchActionsCallback, NULL);
	}
  }
  if (n == 0) XtDestroyWidget(quickKeyBox);

  XtOverrideTranslations(topLevel,
		            XtParseTranslationTable("<Message>WM_PROTOCOLS: Exit()"));

  /* Call UpdateStatusBox() before realizing the top-level widget so that
	 the status widget array in that function would be properly initialized
	 before the user is able to call hangup(), since the latter passes NULL
	 to UpdateStatusBox() */
  UpdateStatusBox((XtPointer)status);

  XtSetMappedWhenManaged(topLevel, False);
  XtRealizeWidget(topLevel);
  XtVaGetValues(menuBox, XtNwidth, &menuBoxWidth, NULL);
  XtVaSetValues(statusBox, XtNwidth, menuBoxWidth, NULL);
  if (n) XtVaSetValues(quickKeyBox, XtNwidth, menuBoxWidth, NULL);

  /*
   * (ideally one should not assume any fixed numbers, but this will do 
   * for now).
   *
   * width of message box = label width
   *                      + 2*border width (2*1)
   *                      + defaultDistance (right) (1)
   *                      + horizDistance (left) (4)
   *
   * desired width of label = width of menu box
   *                        - hSpace of menu box (2*4) (to align with buttons)
   *                        - border with of label (2*1)
   *                        = width of menu box - 10
   *
   * => width of message box = width of menu box - 10 + 7
   * => width of message box < width of menu box
   * => menu box width is the controlling factor in deciding the widnow
   *    width, as desired.
   */
  XtVaSetValues(statusMessage, XtNresizable, True, NULL);
  XtVaSetValues(statusMessage, XtNwidth, menuBoxWidth - 10, NULL);
  XtVaSetValues(statusMessage, XtNresizable, False, NULL);

  wm_delete_window = XInternAtom(XtDisplay(topLevel), 
								 "WM_DELETE_WINDOW", False);
  XSetWMProtocols(XtDisplay(topLevel), XtWindow(topLevel), 
				  &wm_delete_window, 1);
}

void
TopAbout(parent)
     Widget          parent;
{
  Widget          popup,
                  mBox,
                  uBox,
                  lBox,
                  pic,
                  msg,
                  caption;
  Pixmap          pix;
  Dimension       width1,
                  width2;
  char            msgStr[LRG_BUF];
#include "authPic.h"

  popup = SeAddPopup("about", parent);
  mBox = SeAddPaned("mBox", popup);
  uBox = SeAddBox("uBox", mBox);
  lBox = SeAddBox("lBox", mBox);

  msg = SeAddLabel("msg", uBox);
  pic = SeAddLabel("pic", uBox);
  caption = SeAddLabel("caption", uBox);

  sprintf(msgStr, "%s %s rev. %s\n%s\n%s\n%s", "Seyon version", VERSION,
	  REVISION, "Copyright 1992-1993", "(c) Muhammad M. Saggaf",
	  "All rights reserved");
  XtVaSetValues(msg, XtNlabel, msgStr, NULL);

  pix = XCreateBitmapFromData(XtDisplay(pic),
			      DefaultRootWindow(XtDisplay(pic)),
			      authPic_bits, authPic_width,
			      authPic_height);
  XtVaSetValues(pic, XtNbitmap, pix, NULL);

  width1 = SeWidgetWidth(msg);
  width2 = SeWidgetWidth(pic);
  width1 = width1 > width2 ? width1 : width2;
  width2 = SeWidgetWidth(caption);
  width1 = width1 > width2 ? width1 : width2;

  XtVaSetValues(msg, XtNwidth, width1, NULL);
  XtVaSetValues(pic, XtNwidth, width1, NULL);
  XtVaSetValues(caption, XtNwidth, width1, NULL);

  SeAddButton("dismiss", lBox, DestroyShell);

  XtPopupSpringLoaded(popup);
}

/*
 * TopHelp: displays the help file.
 */

void
TopHelp(widget)
     Widget          widget;
{
  Widget          DoDisplayFile();
  Widget          displayPopup;

  XtVaSetValues(widget, XtNsensitive, False, NULL);
  displayPopup = DoDisplayFile(widget, qres.helpFile);
  XtAddCallback(displayPopup, XtNdestroyCallback, SetSensitiveOn, widget);

  PositionShell(displayPopup, widget, SHELLPOS_HWFH);
  XtPopup(displayPopup, XtGrabNone);
}

void
ExecHangup()
{
  MdmHangup();
  SeyonMessage("Line Disconnected");
}

void
DoHangup(widget)
     Widget          widget;
{
  DestroyShell(widget);
  ExecHangup();
}

void
HangupConfirm(widget)
     Widget          widget;
{
  Widget          popup,
                  dialog;

  ErrorIfBusy()

  if (qres.hangupConfirm) {
	popup = AddSimplePopup("hangup", widget);
	dialog = SeAddDialog("dialog", popup);
	
	XawDialogAddButton(dialog, "yes", DoHangup, (XtPointer) dialog);
	XawDialogAddButton(dialog, "cancel", DestroyShell, NULL);
	
	PopupCentered(popup, widget);
  }
  else
	ExecHangup();
}

void
ExitNoHangup(widget)
     Widget          widget;
{
  DestroyShell(widget);
  s_exit(widget);
}

void
ExitHangup(widget)
     Widget          widget;
{
  ExecHangup();
  ExitNoHangup(widget);
}

void
ExitConfirm(widget)
     Widget          widget;
{
  Widget          popup,
                  dialog;

  if (qres.exitConfirm && !qres.ignoreModemDCD && Online()) {
	popup = AddSimplePopup("exit", widget);
	dialog = SeAddDialog("dialog", popup);
	
	XawDialogAddButton(dialog, "yes", ExitHangup, NULL);
	XawDialogAddButton(dialog, "no", ExitNoHangup, NULL);
	XawDialogAddButton(dialog, "cancel", DestroyShell, NULL);
	
	PopupCentered(popup, widget);
  }
  else
	s_exit();
}

void
ExitAction(widget)
     Widget          widget;
{
  Boolean         wExitButtonStatus;

  /* Prevent the user from exiting the program by f.delete if exiting
	 is not permitted */
  XtVaGetValues(w_exit, XtNsensitive, &wExitButtonStatus, NULL);
  ReturnIfTrue(!wExitButtonStatus);
  s_exit();
}

void
w_exit_up(w_exit_status)
     Boolean         w_exit_status;
{
  XtVaSetValues(w_exit, XtNsensitive, w_exit_status, NULL);
}

void
SetKillButtonSens(killWidgetStatus)
     Boolean         killWidgetStatus;
{
  XtVaSetValues(w_kill, XtNsensitive, killWidgetStatus, NULL);
  w_exit_up(!killWidgetStatus);
}

void
w_kill_up(w_kill_status)
     Boolean         w_kill_status;
{
  SetKillButtonSens(w_kill_status);
}

void
KillChildProc()
{
  if (w_child_pid == 0) return;
  if (kill(w_child_pid, SIGTERM) == 0) w_child_pid = 0;
}

void
GetQuickKeyResources(quickKeyName, quickKeyRes)
	 String               quickKeyName;
	 struct _quickKeyRes *quickKeyRes;
{
#define offset(field) XtOffsetOf(struct _quickKeyRes, field)
  static XtResource resources[] = {
    {"visible", "Visible", XtRBoolean, sizeof(Boolean),
	   offset(visible), XtRImmediate, (XtPointer)False},
    {"action", "Action", XtRString, sizeof(String),
	   offset(action), XtRString, (XtPointer)""},
  };
#undef offset
  
  XtGetSubresources(topLevel, (XtPointer)quickKeyRes, quickKeyName, "Command",
					resources, XtNumber(resources), NULL, 0);
}

void
GetResources()
{
#define offset(field) XtOffsetOf(struct QueryResources, field)

  static XtResource resources[] = {
    {"modems", "Modems", XtRString, sizeof(String),
	   offset(modems), XtRString, (XtPointer)""},
    {"script", "Script", XtRString, sizeof(String),
	   offset(script), XtRString, (XtPointer) NULL},
	
    {"defaultBPS", "DefaultBPS", XtRString, sizeof(String),
	   offset(defaultBPS), XtRString, (XtPointer) "9600"},
    {"defaultBits", "DefaultBits", XtRInt, sizeof(int),
	   offset(defaultBits), XtRImmediate, (XtPointer) 8},
    {"defaultParity", "DefaultParity", XtRInt, sizeof(int),
	   offset(defaultParity), XtRImmediate, (XtPointer) 0},
    {"defaultStopBits", "DefaultStopBits", XtRInt, sizeof(int),
	   offset(defaultStopBits), XtRImmediate, (XtPointer) 1},
    {"stripHighBit", "StripHighBit", XtRBoolean, sizeof(Boolean),
	   offset(stripHighBit), XtRImmediate, (XtPointer) False},
    {"backspaceTranslation", "BackspaceTranslation", XtRBoolean,
	   sizeof(Boolean), offset(backspaceTranslation), XtRImmediate,
	   (XtPointer) False},
    {"metaKeyTranslation", "MetaKeyTranslation", XtRBoolean,
	   sizeof(Boolean), offset(metaKeyTranslation), XtRImmediate,
	   (XtPointer) True},
    {"xonxoffFlowControl", "XonxoffFlowControl", XtRBoolean,
	   sizeof(Boolean), offset(xonxoffFlowControl), XtRImmediate,
	   (XtPointer) False},
    {"rtsctsFlowControl", "RtsctsFlowControl", XtRBoolean,
	   sizeof(Boolean), offset(rtsctsFlowControl), XtRImmediate,
	   (XtPointer) False},
    {"newlineTranslation", "NewlineTranslation", XtRString,
	   sizeof(String), offset(newlineTranslation), XtRImmediate,
	   (XtPointer) "cr"},

    {"dialPrefix", "DialPrefix", XtRString, sizeof(String),
	   offset(dialPrefix), XtRString, (XtPointer) "ATDT"},
    {"dialSuffix", "DialSuffix", XtRString, sizeof(String),
	   offset(dialSuffix), XtRString, (XtPointer) "^M"},
    {"dialCancelString", "DialCancelString", XtRString, sizeof(String),
	   offset(dialCancelString), XtRString, (XtPointer) "^M"},

    {"dialTimeOut", "DialTimeOut", XtRInt, sizeof(int),
	   offset(dialTimeOut), XtRImmediate, (XtPointer) 45},
    {"dialDelay", "DialDelay", XtRInt, sizeof(int),
	   offset(dialDelay), XtRImmediate, (XtPointer) 10},
    {"dialRepeat", "DialRepeat", XtRInt, sizeof(int),
	   offset(dialRepeat), XtRImmediate, (XtPointer) 5},

    {"connectString", "ConnectString", XtRString, sizeof(String),
	   offset(connectString), XtRString, (XtPointer) "CONNECT"},
    {"noConnectString1", "NoConnectString1", XtRString, sizeof(String),
	   offset(noConnectString[0]), XtRString, (XtPointer) "NO CARRIER"},
    {"noConnectString2", "NoConnectString2", XtRString, sizeof(String),
	   offset(noConnectString[1]), XtRString, (XtPointer) "NO DIALTONE"},
    {"noConnectString3", "NoConnectString3", XtRString, sizeof(String),
	   offset(noConnectString[2]), XtRString, (XtPointer) "BUSY"},
    {"noConnectString4", "NoConnectString4", XtRString, sizeof(String),
	   offset(noConnectString[3]), XtRString, (XtPointer) "VOICE"},

    {"hangupBeforeDial", "HangupBeforeDial", XtRBoolean, sizeof(Boolean),
	   offset(hangupBeforeDial), XtRImmediate, (XtPointer)True},
    {"dialAutoStart", "DialAutoStart", XtRBoolean, sizeof(Boolean),
	   offset(dialAutoStart), XtRImmediate, (XtPointer)False},
    {"dialDirFormat", "DialDirFormat", XtRString, sizeof(String),
	   offset(dialDirFormat), XtRString,
	   (XtPointer)"%-15s %-15s %6s %1c%1c%1c %1c%1c %s"},
    {"defaultPhoneEntries", "DefaultPhoneEntries", XtRString, sizeof(String),
	   offset(defaultPhoneEntries), XtRString, (XtPointer)NULL},

    {"startupAction", "StartupAction", XtRString, sizeof(String),
	   offset(startupAction), XtRString, 
	   (XtPointer)"RunScript(startup);"},
    {"postConnectAction", "PostConnectAction", XtRString, sizeof(String),
	   offset(postConnectAction), XtRString, (XtPointer)"Beep();"},

    {"autoZmodem", "AutoZmodem", XtRBoolean, sizeof(Boolean), 
	   offset(autoZmodem), XtRImmediate, (XtPointer)True},
    {"autoZmodemAction", "AutoZmodemAction", XtRString, sizeof(String),
	   offset(autoZmodemAction), XtRString, 
	   (XtPointer)"ShellCommand($rz);"},

    {"modemVMin", "ModemVMin", XtRInt, sizeof(int),
	   offset(modemVMin), XtRImmediate, (XtPointer) 1},
    {"ignoreModemDCD", "IgnoreModemDCD", XtRBoolean, sizeof(Boolean),
	   offset(ignoreModemDCD), XtRImmediate, (XtPointer) False},
    {"hangupViaDTR", "HangupViaDTR", XtRBoolean, sizeof(Boolean),
	   offset(hangupViaDTR), XtRImmediate, (XtPointer)False},
    {"modemAttentionString", "ModemAttentionString", XtRString, sizeof(String),
	   offset(modemAttentionString), XtRString, (XtPointer)"+++"},
    {"modemHangupString", "ModemHangupString", XtRString, sizeof(String),
	   offset(modemHangupString), XtRString, (XtPointer)"ATH^M"},
    {"hangupConfirm", "HangupConfirm", XtRBoolean, sizeof(Boolean),
	   offset(hangupConfirm), XtRImmediate, (XtPointer) True},
    {"exitConfirm", "ExitConfirm", XtRBoolean, sizeof(Boolean),
	   offset(exitConfirm), XtRImmediate, (XtPointer) True},

    {"defaultDirectory", "DefaultDirectory", XtRString, sizeof(String),
	   offset(defaultDirectory), XtRString, (XtPointer) "~/.seyon"},
    {"scriptDirectory", "scriptDirectory", XtRString, sizeof(String),
	   offset(scriptDirectory), XtRString, (XtPointer) NULL},
    {"startupFile", "StartupFile", XtRString, sizeof(String),
	   offset(startupFile), XtRString, (XtPointer) "startup"},
    {"phoneFile", "PhoneFile", XtRString, sizeof(String),
	   offset(phoneFile), XtRString, (XtPointer) "phonelist"},
    {"protocolsFile", "ProtocolsFile", XtRString, sizeof(String),
	   offset(protocolsFile), XtRString, (XtPointer) "protocols"},
    {"captureFile", "CaptureFile", XtRString, sizeof(String),
	   offset(captureFile), XtRString, (XtPointer) "capture"},
    {"helpFile", "HelpFile", XtRString, sizeof(String),
	   offset(helpFile), XtRString, (XtPointer) HELPFILE},

    {"modemStatusInterval", "ModemStatusInterval", XtRInt, sizeof(int),
	   offset(modemStatusInterval), XtRImmediate, (XtPointer) 5},

    {"idleGuard", "IdleGuard", XtRBoolean, sizeof(Boolean),
	   offset(idleGuard), XtRImmediate, (XtPointer) False},
    {"idleGuardInterval", "IdleGuardInterval", XtRInt, sizeof(int),
	   offset(idleGuardInterval), XtRImmediate, (XtPointer) 300},
    {"idleGuardString", "IdleGuardString", XtRString, sizeof(String),
	   offset(idleGuardString), XtRImmediate, (XtPointer) " ^H"},

    {"showFunMessages", "ShowFunMessages", XtRBoolean, sizeof(Boolean),
	   offset(showFunMessages), XtRImmediate, (XtPointer) True},
    {"funMessagesInterval", "FunMessagesInterval", XtRInt, sizeof(int),
	   offset(funMessagesInterval), XtRImmediate, (XtPointer) 15},
    {"funMessages", "FunMessages", XtRStringArray, sizeof(String*),
	   offset(funMessages), XtRStringArray, (XtPointer) NULL},
  };

#undef offset

  XtSetTypeConverter(XtRString, XtRStringArray, CvtStringToStringArray,
					 NULL, 0, XtCacheNone, NULL);

  XtGetApplicationResources(topLevel, (XtPointer)&qres, resources,
							XtNumber(resources), NULL, 0);
}
