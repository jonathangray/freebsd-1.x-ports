
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
#include <X11/Xaw/Dialog.h>

#include "seyon.h"
#include "SeDecl.h"

#define AddPopupTopLevelNoGeom(name, parent) \
  SeAddPopupWG(name, parent, NULL, NULL, 0, 0, True, False)
#define AddPopupCentered(name, parent, geomW) \
  SeAddPopupWG(name, parent, geomW, geomW, SeWidgetWidth(geomW)/2, \
			   SeWidgetHeight(geomW)/2, False, True)

extern Widget   topLevel;

void
ErrorExitCallback(widget, exitProc)
     Widget          widget;
     XtPointer       exitProc;
{
  /* Can you believe this? */
  (*((int (*)())exitProc)) (1);
  /* I'm just casting the variable to be a function returning int
	 and then calling the function pointed to by the variable */
}

void
PopupInitError(name, callback)
     String          name;
     void            (*callback) ();
{
  Widget          popup,
                  dialog;

  popup = AddPopupTopLevelNoGeom("initError", topLevel);
  dialog = SeAddDialog(name, popup);
  XawDialogAddButton(dialog, "exit", ErrorExitCallback, (XtPointer) callback);

  PopupCenteredOnRoot(popup);
  Beep();
  XtMapWidget(popup);
}

void
PopupFatalError(name)
     String          name;
{
  Widget          popup,
                  dialog;
  void            cleanup_exit();

  if (XtIsRealized(topLevel))
    popup = AddSimplePopup("fatalError", topLevel);
  else
    popup = AddPopupTopLevelNoGeom("fatalError", topLevel);

  dialog = SeAddDialog(name, popup);
  XawDialogAddButton(dialog, "exit", ErrorExitCallback,
		     (XtPointer) cleanup_exit);

  Beep();

  if (XtIsRealized(topLevel))
	{PopupCentered(popup, topLevel); return;}
	  
  PopupCenteredOnRoot(popup);
  XtMapWidget(popup);
  XtAppMainLoop(app_con);
}

void
PopupError(name, parent)
     String          name;
     Widget          parent;
{
  Widget          popup,
                  dialog;

  if (!parent) parent = topLevel;
  if (XtIsRealized(parent))
	popup = AddSimplePopup("error", parent, topLevel);
  else
	popup = AddPopupTopLevelNoGeom("error", parent);

  dialog = SeAddDialog(name, popup);
  XawDialogAddButton(dialog, "dismiss", DestroyShellCallBack, 
					 (XtPointer)popup);

  Beep();

  if (XtIsRealized(parent))
	{PopupCentered(popup, parent); return;}
	  
  PopupCenteredOnRoot(popup);
  XtMapWidget(popup);
}

#ifdef notdef
void
SePopupWarningF(parent, fmt, a, b, c, d)
     Widget          parent;
     String          fmt,
                     a,
                     b,
                     c,
                     d;
{
  SePopupNoticeF(parent, 0, "Seyon Warning", DestroyParentPopup,
		 fmt, a, b, c, d);
}

void
SePopupInitWarningF(parent, fmt, a, b, c, d)
     Widget          parent;
     String          fmt,
                     a,
                     b,
                     c,
                     d;
{
  SePopupNoticeF(parent, 0, "Seyon Initialization Warning",
		 DestroyParentPopup, fmt, a, b, c, d);
}
#endif
