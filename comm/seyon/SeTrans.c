
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
*/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Viewport.h>
#include "MultiList.h"

#include <math.h>

#include "seyon.h"
#include "SeDecl.h"

int             ReadParseProtFile();
void            DoTransfer(),
                DoUpload(),
                exec_upload(),
                ReReadProtFile(),
                SeTransfer();

struct _protItem {
  char            name[LIT_BUF];
  char            command[REG_BUF];
  Boolean         reqName;
};

struct _protItem *protItems[MAX_ENT];
XfwfMultiListWidget mlw;
int             transCurItemIndex;

void
TopTransfer(widget, clientData)
     Widget          widget;
     XtPointer       clientData;
{
  void            EditFile();

  Widget          popup, mBox, uBox, lBox,
                  view,
                  list;
  static char     protocolsFile[REG_BUF];
  static String   disItems[MAX_ENT + 1] = {NULL};

  ErrorIfBusy();

  if (disItems[0] == NULL) {
    strcpy(protocolsFile, qres.protocolsFile);
    if (ReadParseProtFile(protocolsFile, disItems) < 0)
      return;
  }

  popup = AddSimplePopup("transfer", widget);
  mBox = SeAddPaned("mBox", popup);
  uBox = AddBox("uBox", mBox);
  lBox = AddBox("lBox", mBox);

  view = XtCreateManagedWidget("view", viewportWidgetClass, uBox, NULL, 0);
  list = XtVaCreateManagedWidget("list", xfwfMultiListWidgetClass, view, 
								 XtNlist, disItems, XtNmaxSelectable, 1, NULL);
  mlw = (XfwfMultiListWidget) list;
  SeSetViewportDimFromList(view, list, 10);
  XtAddCallback(list, XtNcallback, DoTransfer, clientData);

  AddButton("dismiss", lBox, DestroyShell, NULL);
  AddButton("ok", lBox, DoTransfer,  clientData);
  AddButton("edit", lBox, EditFile, (XtPointer)protocolsFile);
  AddButton("reread", lBox, ReReadProtFile,	(XtPointer)disItems);

  if (clientData) DoTransfer(list, clientData, NULL);
  else PopupCentered(popup, widget);
}

char            lastUploadFile[REG_BUF];

void
DoTransfer(widget, clientData, callData)
     Widget          widget;
     XtPointer       clientData,
                     callData;
{
  XfwfMultiListReturnStruct *item;
  Widget          popup;
  String*         actionData = (String*)clientData;
  char            fullCommand[LRG_BUF];

  if (clientData)
	{if ((transCurItemIndex = atoi(actionData[0]) - 1) < 0 ||  
		 transCurItemIndex > MAX_ENT - 1)
	   SimpleError("Invalid Entry Number");}
  else {
	if ((item = XfwfMultiListGetHighlighted(mlw))->num_selected == 0)
	  SimpleError("No Item Selected");
	transCurItemIndex =  item->selected_items[0];
  }
  
  strcpy(fullCommand, protItems[transCurItemIndex]->command);

  if (protItems[transCurItemIndex]->reqName)
	if (actionData == NULL ||  actionData[1] == NULL) {
	  popup = GetShell(PopupDialogGetValue("upload", widget, exec_upload, 
										   NULL, lastUploadFile));
	  PopupCentered(popup, (clientData) ? XtParent(GetShell(widget)) : widget);
	  return;
	}
	else
	  strcat(strcat(fullCommand, " "), actionData[1]);
  
  DestroyShell(widget);
  ShellCommand(fullCommand);
}

void
ReReadProtFile(widget, disItems)
     Widget          widget;
     XtPointer       disItems[];
{
  Widget          protWidget = XtParent(GetShell(widget));

  FreeList(disItems);
  DestroyShell(widget);
  TopTransfer(protWidget, NULL);
}

void
exec_upload(widget)
     Widget          widget;
{
  Widget          dialog = XtParent(widget);
  static char     cmd[REG_BUF];

  strcpy(lastUploadFile, XawDialogGetValueString(dialog));
  sprintf(cmd, "%s %s", protItems[transCurItemIndex]->command,
	  lastUploadFile);

  DestroyShell(XtParent(GetShell(widget)));
  ShellCommand(cmd);
}

void
upload_acc_ok(widget)
     Widget          widget;
{
  exec_upload(widget);
}

void
DoShellCommand(widget, command)
     Widget          widget;
     XtPointer       command;
{
  ShellCommand((String)command);
}

void
TopShell(widget)
     Widget          widget;
{
  void  GetValueByPopup();
  ErrorIfBusy();
  GetValueByPopup(widget, "shellCommand", DoShellCommand);
}

int
ReadParseProtFile(fname, disItems)
     String          fname;
     String          disItems[];
{
  FILE           *fp;
  String          rawItems[MAX_ENT + 1];
  char           *buf,
                  reqName[10];
  int             i,
                  n;

  if ((fp = open_file(fname, qres.defaultDirectory)) == NULL)
    return -1;

  ReadCommentedFile(fp, rawItems);
  fclose(fp);

  FreeList(protItems);
  for (i = 0; (buf = rawItems[i]); i++) {
    /*
	 * allocate the record
	 */
    protItems[i] = XtNew(struct _protItem);
    /*
	 * find the name
	 */
    GetWord(buf, protItems[i]->name);
    /*
	 * find the command
	 */
    GetWord(lptr, protItems[i]->command);
    /*
	 * find other stuff
	 */
    GetWord(lptr, reqName);
    if (reqName[0] == 'y' || reqName[0] == 'Y')
      protItems[i]->reqName = True;
    else
      protItems[i]->reqName = False;
  }
  protItems[i] = (struct _protItem *)NULL;

  FreeList(rawItems);
  FreeList(disItems);

  for (n = 0; n < i; n++)
    disItems[n] = XtNewString(protItems[n]->name);

  disItems[n] = NULL;

  return 0;
}
