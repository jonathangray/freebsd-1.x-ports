
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include <setjmp.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xmu/CharSet.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/List.h>
#include "MultiList.h"
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Paned.h>

#include "seyon.h"
#include "SeDecl.h"

extern Widget   topLevel;
extern Widget   statusMessage;
extern Pixmap   progIcon;

void            SePopupMsg(),
                EditSaveFile(),
                StopMainLoop();

Boolean         DoMainLoop;

/*
 * does nothing
 */

void
DoNothing()
{
  return;
}

/*
 * distroys a popup
 */

void
DestroyShell(widget)
     Widget          widget;
{
  Widget          shell = GetShell(widget);

  XtPopdown(shell);
  XtDestroyWidget(shell);
}

void
DestroyShellCallBack(widget, child)
     Widget          widget;
     XtPointer       child;
{
  DestroyShell((Widget)child);
}

void
DestroyParentPopup(widget, child)
     Widget          widget;
     XtPointer       child;
{
  DestroyShellCallBack(widget, child);
}

void
DismissPopup(widget, upFlag)
     Widget          widget;
     XtPointer       upFlag;
{
  *((Boolean*)upFlag) = False;
  DestroyShell(widget);
}

void
ResetFlag(widget, flag)
     Widget          widget;
     XtPointer       flag;
{
  *((Boolean*)flag) = False;
}

void
SetSensitiveOn(widget, callData)
     Widget          widget;
     XtPointer       callData;
{
  XtVaSetValues((Widget)callData, XtNsensitive, True, NULL);
}

/*
 * creates a button
 */

Widget
SeAddButton(name, parent, call_back)
     String          name;
     Widget          parent;
     void            (*call_back) ();
{
  Widget          widget;

  widget = XtCreateManagedWidget(name, commandWidgetClass, parent, NULL, 0);
  XtAddCallback(widget, XtNcallback, call_back, (XtPointer) parent);
  return widget;
}

/*
 * similar to the above, but also passes client data to the call back
 * WCD = with client data
 */

Widget
SeAddButtonWCD(name, parent, callBack, clientData)
     String          name;
     Widget          parent;
     void            (*callBack) ();
     XtPointer       clientData;
{
  return AddButton(name, parent, callBack, clientData);
}

Widget
AddButton(name, parent, callBack, clientData)
     String          name;
     Widget          parent;
     void            (*callBack) ();
     XtPointer       clientData;
{
  Widget          widget;

  widget = XtCreateManagedWidget(name, commandWidgetClass, parent, NULL, 0);
  if (callBack)
	XtAddCallback(widget, XtNcallback, callBack, clientData);
  return widget;
}

Widget
SeAddButtonWithClientData(name, parent, call_back, client_data)
     String          name;
     Widget          parent;
     void            (*call_back) ();
     XtPointer       client_data;
{
  return SeAddButtonWCD(name, parent, call_back, client_data);
}

/*
 * creates a label
 */

Widget
SeAddLabel(name, parent)
     String          name;
     Widget          parent;
{
  return XtCreateManagedWidget(name, labelWidgetClass, parent, NULL, 0);
}

/*
 * sets a widget's label
 */

void
SeSetLabel(widget, label)
     Widget          widget;
     String          label;
{
  XtVaSetValues(widget, XtNlabel, label, NULL);
}

/*
 * creates a toggle
 */

Widget
SeAddToggleWCD(name, parent, call_back, clientData)
     String          name;
     Widget          parent;
     void            (*call_back) ();
     XtPointer       clientData;
{
  Widget          widget;

  widget = XtCreateManagedWidget(name, toggleWidgetClass, parent, NULL, 0);
  if (call_back) {
    if (clientData == NULL)
      clientData = (XtPointer) parent;
	XtAddCallback(widget, XtNcallback, call_back, clientData);
  }
  return widget;
}

Widget
SeAddToggle(name, parent, call_back)
     String          name;
     Widget          parent;
     void            (*call_back) ();
{
  return SeAddToggleWCD(name, parent, call_back, NULL);
}

/*
 * sets or unsets a toggle
 */

void
SeSetUnsetToggle(widget, state)
     Widget          widget;
     Boolean         state;
{
  XtVaSetValues(widget, XtNstate, state, NULL);
}

/*
 * gets a toggle's state
 */

Boolean
SeGetToggleState(widget)
     Widget          widget;
{
  Boolean         state;

  XtVaGetValues(widget, XtNstate, &state, NULL);
  return state;
}

Widget
SePopupRadio(popup_name, parent, name, active, call_back, clientData)
     String          popup_name;
     Widget          parent;
     String          name[];
     int             active;
     void            (*call_back) ();
     XtPointer       clientData;
{
  Widget          popup,
                  mBox,
                  uBox,
                  lBox,
                  toggle,
                  widget;
  long            i = 0;

  popup = AddSimplePopup(popup_name, parent);
  mBox = SeAddPaned("mBox", popup);
  uBox = SeAddBox("Radio", mBox);
  lBox = SeAddBox("lBox", mBox);

  toggle =
    XtVaCreateManagedWidget(name[i], toggleWidgetClass, uBox, XtNradioData,
			    (XtPointer) (i + 1), NULL);
  SeSetUnsetToggle(toggle, (active == i + 1));
  XtAddCallback(toggle, XtNcallback, call_back, clientData);

  for (i++; name[i]; i++) {
    widget =
      XtVaCreateManagedWidget(name[i], toggleWidgetClass, uBox, XtNradioGroup,
			   toggle, XtNradioData, (XtPointer) (i + 1), NULL);
    SeSetUnsetToggle(widget, (active == i + 1));
    XtAddCallback(widget, XtNcallback, call_back, clientData);
  }

  SeAddButtonWCD("dismiss", lBox, DestroyParentPopup, (XtPointer) mBox);

  PopupCentered(popup, parent);
  return toggle;
}

/*
 * AddBox: creates a box.
 */

Widget
AddBox(name, parent)
     String          name;
     Widget          parent;
{
  return XtCreateManagedWidget(name, boxWidgetClass, parent, NULL, 0);
}

Widget
SeAddBox(name, parent)
     String          name;
     Widget          parent;
{
  return AddBox(name, parent);
}

/*
 * AddDialog: creates a dialog widget.
 */

Widget
AddDialog(name, parent)
     String          name;
     Widget          parent;
{
  return XtCreateManagedWidget(name, dialogWidgetClass, parent, NULL, 0);
}

Widget
SeAddDialog(name, parent)
     String          name;
     Widget          parent;
{
  return AddDialog(name, parent);
}

/*
 * sets a dialog's label
 */

void
SeSetDialogValue(dialog, value)
     Widget          dialog;
     String          value;
{
  Arg             args;

  XtSetArg(args, XtNvalue, value);
  XtSetValues(dialog, &args, 1);
}

Widget
PopupDialogGetValue(name, parent, callBack, clientData, defVal)
     String          name;
     Widget          parent;
     void            (*callBack)();
     XtPointer       clientData;
     String          defVal;
{
  Widget          popup,
                  dialog;

  popup = AddSimplePopup(name, parent);
  dialog = AddDialog("dialog", popup);

  XtVaSetValues(dialog, XtNvalue, (defVal ? defVal : ""), NULL);
  if (clientData == NULL) clientData = (XtPointer)dialog;

  XawDialogAddButton(dialog, "ok", callBack, clientData);
  XawDialogAddButton(dialog, "cancel", DestroyShell, (XtPointer)dialog);

  return dialog;
}

char   getValueDefValue[REG_BUF];
void   (*getValueExecProc)();

void
GetValueDispatchProc(valueWidget)
     Widget          valueWidget;
{
  Widget          dialog = XtParent(valueWidget);

  strcpy(getValueDefValue, XawDialogGetValueString(dialog));
  DestroyShell(dialog);

  (*getValueExecProc)(XtParent(GetShell(valueWidget)), getValueDefValue);
}

void
GetValueByPopupOKAction(widget)
  Widget          widget;
{
  GetValueDispatchProc(widget);
}

void
GetValueByPopup(widget, name, callback)
     Widget          widget;
     String          name;
	 XtCallbackProc  callback;
{
  Widget          popup,
                  dialog;
  
  getValueExecProc = (void (*)())callback;
  popup = GetShell((dialog = PopupDialogGetValue(name, widget,
							   GetValueDispatchProc, NULL, 
							   getValueDefValue)));

  PopupCentered(popup, widget);
}

Widget
SePopupDialogGetStringE(popup_name, parent, ok_callback,
			ok_client_data, def_val, UL)
     String          popup_name;
     Widget          parent;
     void            (*ok_callback) ();
     XtPointer       ok_client_data;
     String          def_val;
     Boolean         UL;
{
  Widget          dialog;

  dialog = PopupDialogGetValue(popup_name, parent, ok_callback,
								ok_client_data, def_val);
  PopupCentered(GetShell(dialog), parent);
  return dialog;
}

Widget
SePopupDialogGetString(popup_name, parent, ok_callback,
		       ok_client_data)
     String          popup_name;
     Widget          parent;
     void            (*ok_callback) ();
     XtPointer       ok_client_data;
{
  return SePopupDialogGetStringE(popup_name, parent, ok_callback,
				 ok_client_data, NULL, False);
}

/*
 * creates a from
 */

Widget
SeAddForm(name, parent)
     String          name;
     Widget          parent;
{
  return XtCreateManagedWidget(name, formWidgetClass, parent, NULL, 0);
}

Widget
AddPaned(name, parent)
     String          name;
     Widget          parent;
{
  return XtCreateManagedWidget(name, panedWidgetClass, parent, NULL, 0);
}

Widget
SeAddPaned(name, parent)
     String          name;
     Widget          parent;
{
  return AddPaned(name, parent);
}

/*
 * sets a viewport's dimensions
 */

void
SeSetViewportDimensions(viewport, child, max_height)
     Widget          viewport,
                     child;
     Dimension       max_height;
{
  SeSetWidgetWidth(viewport, SeWidgetWidth(child) + 14);

  if (SeWidgetHeight(child) > max_height)
    SeSetWidgetHeight(viewport, max_height);
}

/*
 * sets a viwport's dimensions according to a child list
 */

void
SeSetViewportDimFromList(viewport, list, rows)
     Widget          viewport,
                     list;
     Cardinal        rows;
{
  XFontStruct    *font;
  Dimension       height,
                  internalHeight,
                  rowSpacing,
                  borderWidth;

  XtVaGetValues(list, XtNfont, &font, XtNinternalHeight, &internalHeight,
				XtNrowSpacing, &rowSpacing, XtNborderWidth, &borderWidth, 
				NULL);

  height = font->ascent + font->descent;
  height = height * rows + rowSpacing * (rows - 1) +
    2 * (internalHeight + borderWidth);

  SeSetViewportDimensions(viewport, list, height);
}

/*
 * sets a viwport's dimensions according to a child multi-list
 */

void
SeSetViewportDimFromMultiList(viewport, list, rows)
     Widget          viewport,
                     list;
     Cardinal        rows;
{
  Dimension       rowHeight,
                  borderWidth;

  XtVaGetValues(list, XtNrowHeight, &rowHeight, XtNborderWidth, &borderWidth,
				NULL);

  SeSetViewportDimensions(viewport, list,
			  rowHeight * rows + 2 * borderWidth);
}

Widget
GetShell(widget)
     Widget          widget;
{
  while ((widget != NULL) && !XtIsShell(widget))
    widget = XtParent(widget);

  return (widget);
}

int
IconifyShell(widget)
     Widget          widget;
{
  widget = GetShell(widget);
  return (int)XIconifyWindow(XtDisplay(widget), XtWindow(widget), 
							 XScreenNumberOfScreen(XtScreen(widget)));
}

/*
 *   Creates a popup with a bit more control on geometry.
 *   WG = With Geometry
 */

void
CenterShell(widget, geomParent)
     Widget          widget,
                     geomParent;
{
  Dimension width, height, borderWidth;
  Position x, y, maxPos;

  XtVaGetValues(geomParent, XtNwidth, &width, XtNheight, &height, NULL);
  XtTranslateCoords(geomParent, (Position)width/2, (Position)height/2, 
					&x, &y);

  widget = GetShell(widget);
  if (!XtIsRealized(widget)) XtRealizeWidget(widget);

  XtVaGetValues(widget, XtNwidth, &width, XtNheight, &height, XtNborderWidth,
				&borderWidth, NULL);

  width += 2 * borderWidth;
  height += 2 * borderWidth;

  x -= (Position)width/2;
  if (x < 0) x = 0;
  if (x > (maxPos = (Position)(XtScreen(widget)->width - width))) 
	x = maxPos;

  y -= (Position)height/2;
  if (y < 0) y = 0;
  if (y > (maxPos = (Position)(XtScreen(widget)->height - height))) 
	y = maxPos;

  XtVaSetValues(widget, XtNx, x, XtNy, y, NULL);
}

void
CenterShellOnRoot(widget)
     Widget          widget;
{
  Dimension width, height, borderWidth;
  Position x, y;

  widget = GetShell(widget);
  if (!XtIsRealized(widget)) XtRealizeWidget(widget);

  XtVaGetValues(widget, XtNwidth, &width, XtNheight, &height, XtNborderWidth,
				&borderWidth, NULL);

  width += 2 * borderWidth;
  height += 2 * borderWidth;

  x = (Position)(XtScreen(widget)->width - width)/2;
  y = (Position)(XtScreen(widget)->height - height)/2;

  XtVaSetValues(widget, XtNx, x, XtNy, y, NULL);
}

void
PositionShell(widget, parent, posStyle)
     Widget          widget;
     Widget          parent;
	 int             posStyle;
{
  Dimension width, height /*, borderWidth*/;
  Position x, y;

  widget = GetShell(widget);
  if (!XtIsRealized(widget)) XtRealizeWidget(widget);

  switch (posStyle) {
  case  SHELLPOS_HWFH:
	XtVaGetValues(parent, XtNwidth, &width, XtNheight, &height, NULL);
    XtTranslateCoords(parent, (Position)width/2, (Position)height, &x, &y);
    XtVaSetValues(widget, XtNx, x, XtNy, y, NULL);
	break;
  }
}

Widget
SeAddPopupWG(name, parent, x_widget, y_widget, x_offset, y_offset, topLev,
	     setGeom)
     String          name;
     Widget          parent,
                     x_widget,
                     y_widget;
     Position        x_offset,
                     y_offset;
     Boolean         topLev;
     Boolean         setGeom;
{
  Widget          popup;
  Position        x,
                  y,
                  dummy;

  if (x_widget == NULL)
    x_widget = parent;
  if (y_widget == NULL)
    y_widget = x_widget;

  if (topLev)
    popup = XtVaCreatePopupShell(name, topLevelShellWidgetClass, parent,
								 XtNiconPixmap, progIcon, NULL);
  else
    popup = XtVaCreatePopupShell(name, transientShellWidgetClass, parent,
								 XtNtransientFor, GetShell(parent),
								 XtNiconPixmap, progIcon, NULL);

  if (setGeom) {
    XtTranslateCoords(x_widget, x_offset, (Position)0, &x, &dummy);
    XtTranslateCoords(y_widget, (Position)0, y_offset, &dummy, &y);
    XtVaSetValues(popup, XtNx, x, XtNy, y, NULL);
  }

  return popup;
}

Widget
AddSimplePopup(name, parent)
     String          name;
     Widget          parent;
{
  return XtVaCreatePopupShell(name, transientShellWidgetClass, parent,
							  XtNtransientFor, GetShell(parent),
							  XtNiconPixmap, progIcon, NULL);
}

Widget
SeAddPopup(name, parent)
     String          name;
     Widget          parent;
{
  return SeAddPopupWG(name, parent, NULL, NULL, SeWidgetWidth(parent) / 2,
		      SeWidgetHeight(parent), False, True);
}

Widget
SeAddPopupOffset(name, parent, geomParent)
     String          name;
     Widget          parent;
     Widget          geomParent;
{
  return SeAddPopupWG(name, parent, geomParent, geomParent, 10, 10, False,
		      True);
}

Widget
SeAddPopupUL(name, parent)
     String          name;
     Widget          parent;
{
  return SeAddPopupOffset(name, parent, XtParent(parent));
}

Widget
SeAddPopupSh(name, parent)
     String          name;
     Widget          parent;
{
  return SeAddPopupOffset(name, parent, GetShell(parent));
}

/*
 * pops up a message
 */

void
SePopupMsg(parent, msg)
     Widget          parent;
     String          msg;
{
  Widget          popup,
                  dialog;
  Arg             args;

  popup = SeAddPopup("message", parent);

  XtSetArg(args, XtNlabel, msg);
  dialog = XtCreateManagedWidget("dialog", dialogWidgetClass, popup,
				 &args, 1);

  XawDialogAddButton(dialog, "dismiss", DestroyShell, (XtPointer)dialog);

  XtPopup(popup, XtGrabExclusive);
}

void
SePopupMsgf(parent, fmt, a, b, c)
     Widget          parent;
     String          fmt,
                     a,
                     b,
                     c;
{
  char            buf[REG_BUF];

  sprintf(buf, fmt, a, b, c);
  SePopupMsg(parent, buf);
}

Widget
SePopupNotice(parent, title, call_back, msg)
     Widget          parent;
     String          title;
     void            (*call_back) ();
     String          msg;
{
  Widget          popup,
                  dialog;

  popup = SeAddPopupUL("notice", parent);
  XtVaSetValues(popup, XtNtitle, title, NULL);
  dialog = XtVaCreateManagedWidget("dialog", dialogWidgetClass, popup,
				   XtNlabel, msg, NULL);

  XawDialogAddButton(dialog, "dismiss", call_back, (XtPointer) dialog);

  XtPopup(popup, XtGrabExclusive);
  return popup;
}

void
SePopupNoticeF(parent, title, call_back, fmt, a, b, c, d)
     Widget          parent;
     String          title;
     void            (*call_back) ();
     String          fmt,
                     a,
                     b,
                     c,
                     d;
{
  char            buf[REG_BUF];

  sprintf(buf, fmt, a, b, c);
  SePopupNotice(parent, title, call_back, buf);
}

/*
 * almost similar to the above
 */

void
SeTransMsg(name, parent)
     String          name;
     Widget          parent;
{
  Widget          popup,
                  dialog;

  popup = SeAddPopup(name, parent);
  dialog = XtCreateManagedWidget("dialog", dialogWidgetClass, popup,
				 NULL, 0);

  XawDialogAddButton(dialog, "dismiss", DestroyShell, (XtPointer)dialog);

  XtPopupSpringLoaded(popup);
}

/*
 * pops up a message to the effect that a feature is not implemented
 */

void
NotImplemented(w)
     Widget          w;
{
  SeTransMsg("notImplemented", w);
}

void
SeSetValue(widget, name, value)
     Widget          widget;
     String          name;
     XtArgVal        value;
{
  Arg             args;

  XtSetArg(args, name, value);
  XtSetValues(widget, &args, 1);
}

/*
 * returns a widget's height
 */

Dimension
SeWidgetHeight(widget)
     Widget          widget;
{
  Dimension       height;
  Arg             args;

  XtSetArg(args, XtNheight, &height);
  XtGetValues(widget, &args, 1);
  return height;
}

/*
 * sets a widget's height
 */

void
SeSetWidgetHeight(widget, height)
     Widget          widget;
     Dimension       height;
{
  Arg             args;

  XtSetArg(args, XtNheight, height);
  XtSetValues(widget, &args, 1);
}

/*
 * returns a widget's width
 */

Dimension
SeWidgetWidth(widget)
     Widget          widget;
{
  Dimension       width;
  Arg             args;

  XtSetArg(args, XtNwidth, &width);
  XtGetValues(widget, &args, 1);
  return width;
}

/*
 * sets a widget's width
 */

void
SeSetWidgetWidth(widget, width)
     Widget          widget;
     Dimension       width;
{
  Arg             args;

  XtSetArg(args, XtNwidth, width);
  XtSetValues(widget, &args, 1);
}

/*
 * sets the status message
 */

void
SetStatusMessage(msg)
     String          msg;
{
  SeSetLabel(statusMessage, msg);
/*  XFlush(XtDisplay(statusMessage));*/
}

/*
 * similar to the above, but accepts a formmat string
 */

void
SetStatusMessagef(fmt, a, b, c)
     String          fmt,
                     a,
                     b,
                     c;
{
  char            buffer[REG_BUF];

  sprintf(buffer, fmt, a, b, c);
  SetStatusMessage(buffer);
}

/*---------------------------------------------------------------------------+
| Beep - rings the terminal bell.
+---------------------------------------------------------------------------*/

void
Beep()
{
  XKeyboardControl kb;
  Display          *display = XtDisplay(topLevel);
  int              pitch[3]    = {200, 400, 500},
                   duration[3] = { 50, 100, 100},
                   i;

  for (i = 0; i < 3; i++) {
	kb.bell_pitch = pitch[i];
	kb.bell_duration = duration[i];
	XChangeKeyboardControl(display, KBBellPitch | KBBellDuration, &kb);
  
	XBell(display, 100);
	XFlush(display);
	usleep(100000L);
  }
}

/*---------------------------------------------------------------------------+
| DoDisplayFile - prepares a pop up file view.
+---------------------------------------------------------------------------*/

Widget
DoDisplayFile(parent, fileName)
     Widget          parent;
     XtPointer       fileName;
{
  Widget          popup,
                  form;

  popup = AddSimplePopup("display", parent);
  form = SeAddForm("form", popup);

  XtVaCreateManagedWidget("text", asciiTextWidgetClass, form, XtNtype,
						  XawAsciiFile, XtNstring, (String)fileName,
						  XtNeditType, XawtextRead, XtNdisplayCaret, False,
						  NULL);

  AddButton("dismiss", form, DestroyShell, NULL);

  return popup;
}

/*---------------------------------------------------------------------------+
| DisplayFile - pops up a file for viewing.
+---------------------------------------------------------------------------*/

void
DisplayFile(parent, fileName)
     Widget          parent;
     XtPointer       fileName;
{
  Widget          popup;

  popup = DoDisplayFile(parent, fileName);
  CenterShell(popup, parent); 
  XtPopup(popup, XtGrabNone);
}


/*---------------------------------------------------------------------------+
| DialogDisplayFile - prompts for a file name then pops up the file for 
|                     viewing.
+---------------------------------------------------------------------------*/

void
DialogDisplayFile(widget)
     Widget          widget;
{
  GetValueByPopup(widget, "dialogDisplayFile", DisplayFile);
}

/*---------------------------------------------------------------------------+
| EditFile - pops up a file for editing.
+---------------------------------------------------------------------------*/

void
EditFile(parent, fileName)
     Widget          parent;
     XtPointer       fileName;
{
  Widget          popup,
                  form,
                  text;

  popup = AddSimplePopup("edit", parent);
  form = SeAddForm("form", popup);

  text = 
	XtVaCreateManagedWidget("text", asciiTextWidgetClass, form, XtNtype,
							XawAsciiFile, XtNstring, (String)fileName, 
							XtNeditType, XawtextEdit, XtNdisplayCaret, True,
							NULL);

  AddButton("save", form, EditSaveFile, (XtPointer)text);
  AddButton("dismiss", form, DestroyShell, NULL);

  CenterShell(popup, parent); 
  XtPopup(popup, XtGrabNone);
}

/*---------------------------------------------------------------------------+
| DialogEditFile - prompts for a file name then pops up the file for editing.
+---------------------------------------------------------------------------*/

void
DialogEditFile(widget)
     Widget          widget;
{
  GetValueByPopup(widget, "dialogEditFile", EditFile);
}

/*---------------------------------------------------------------------------+
| EditSaveFile - saves the file being edited.
+---------------------------------------------------------------------------*/

void
EditSaveFile(widget, text)
     Widget          widget;
     XtPointer       text;
{
  Widget          textSource;

  XtVaGetValues((Widget)text, XtNtextSource, &textSource, NULL);

  if (XawAsciiSave(textSource) != True)
    SePopupMsg(widget, "File Save Failed");
}

void
SeAppMainLoop(appContext)
     XtAppContext    appContext;
{
  DoMainLoop = True;
  while (DoMainLoop) XtAppProcessEvent(appContext, XtIMAll);
}

void
StopMainLoop()
{
  DoMainLoop = False;
}

#ifdef notdef
int
SeAppMSleep(appContext, msec)
     XtAppContext    appContext;
     unsigned long   msec;
{
  Widget          widget = statusMessage;
  static Boolean  inSleep = False;

  if (inSleep) return -1;
  XtAppAddTimeOut(appContext, msec, (XtTimerCallbackProc)StopMainLoop, NULL);

/*  XtAddGrab(widget, True, False);*/ 
  inSleep = True;
  SeAppMainLoop(appContext);
/*  XtRemoveGrab(widget);*/
  inSleep = False;
  return 0;
}
#endif
