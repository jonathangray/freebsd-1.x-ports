/*
 * Copyright 1992, 1993 The University of Newcastle upon Tyne
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

#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/ShellP.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <AxeIntrinsic.h>
#include <FileNomWin.h>
#include <FileNom.h>
#include <NumericPad.h>
#include <CtrlCodeSel.h>
#include <Preference.h>
#include <Confirmer.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
extern int errno;

#include <stdio.h>

#include "util.h"

#include "AxeTextP.h"

#define PRIVATE(w,field) (((AxeTextWidget) w)->axe.field)
#define AXEII(w,field) (((AxeTextWidget) w)->axeii.field)

#define Offset(field) XtOffsetOf(AxeTextRec, axe.field)

#define nullfile  "/dev/null"

static XtResource resources[] = {
    {XtNassociatedDirectory, XtCAssociatedDirectory, XtRString, sizeof(String),
         Offset(associated_directory), XtRString, (XtPointer) NULL},
    {XtNgrabOnPopup, XtCGrabOnPopup, XtRBoolean, sizeof(Boolean),
         Offset(grab_on_popup), XtRImmediate, (XtPointer) False},
    {XtNchangeCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(change_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNmessageCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(message_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNenableBackups, XtCEnableBackups, XtRBoolean, sizeof(Boolean),
         Offset(enable_backups), XtRImmediate, (XtPointer) False},
    {XtNbackupNamePrefix, XtCBackupNamePrefix, XtRString, sizeof(String),
         Offset(backup_name_prefix), XtRString, (XtPointer) ""},
    {XtNbackupNameSuffix, XtCBackupNameSuffix, XtRString, sizeof(String),
         Offset(backup_name_suffix), XtRString, (XtPointer) ".BAK"}
};

#undef Offset

typedef enum {NONE, READ, EDIT} AccessMode;

static Widget MakePopup();
static void MakeConfirmPopup();
static Widget MakeFileNominator(), MakeNumericPad(), MakeControlSelector();
static Widget MakePreferencePopup();
static int CheckPermission();
static void GotoLine(), IncludeFile();
static void ClearBuffer();
static void SaveFile(), SaveAs(), SaveFileSelect(), SaveOver(), SaveAgain();
static void ReloadFile(), LoadFile(), NoUndo();
static void RecordAndNotifyChange(), SourceChange();
static void SetPreferences(), AcceptPreferences();
static void InsertControl(), MessageNotify();

static void Initialize(), Destroy();
static Boolean SetValues();

AxeTextClassRec axeTextClassRec = {
    /* Core class part */
  {
    /* superclass	     */	(WidgetClass) &axeiiTextClassRec,
    /* class_name	     */ "AxeText",
    /* widget_size	     */ sizeof(AxeTextRec),
    /* class_initialize      */ NULL,
    /* class_part_initialize */ NULL,
    /* class_inited          */	FALSE,
    /* initialize	     */	Initialize,
    /* initialize_hook       */	NULL,
    /* realize		     */	XtInheritRealize,
    /* actions		     */ NULL,
    /* num_actions	     */	0,
    /* resources	     */	resources,
    /* num_resources	     */	XtNumber(resources),
    /* xrm_class	     */	NULLQUARK,
    /* compress_motion	     */	TRUE,
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
    /* compress_exposure     */	XtExposeGraphicsExpose | XtExposeNoExpose,
#else
    /* compress_exposure     */	XtExposeGraphicsExpose,
#endif
    /* compress_enterleave   */	TRUE,
    /* visible_interest	     */	FALSE,
    /* destroy		     */	Destroy,
    /* resize		     */	XtInheritResize,
    /* expose		     */	XtInheritExpose,
    /* set_values	     */	SetValues,
    /* set_values_hook       */	NULL,			
    /* set_values_almost     */	XtInheritSetValuesAlmost,  
    /* get_values_hook       */	NULL,
    /* accept_focus	     */	XtInheritAcceptFocus,
    /* version		     */	XtVersion,
    /* callback offsets      */	NULL,
    /* tm_table              */	XtInheritTranslations,
    /* query_geometry	     */	XtInheritQueryGeometry,
    /* display_accelerator   */	NULL,
    /* extension	     */	NULL,
  },
  { /* simple fields */
    /* change_sensitive */      XtInheritChangeSensitive
  },
  { /* text  fields */
    /* empty            */      0
  },
  { /* ascii fields */
    /* empty            */      0
  },
  { /* axeii fields */
    /* extension        */	NULL
  },
  { /* axe   fields */
    /* exension	        */	NULL
  }
};

WidgetClass axeTextWidgetClass = (WidgetClass) &axeTextClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    AxeTextWidget atw = (AxeTextWidget) new;
    String fileName;
    int check;
    Boolean exists;
    AccessMode mode;

    PRIVATE(new,initial_directory) = (String) 0;
    PRIVATE(new,file_nominator) = (Widget) 0;
    PRIVATE(new,numeric_pad) = (Widget) 0; 
    PRIVATE(new,control_selector) = (Widget) 0;
    PRIVATE(new,preference_popup) = (Widget) 0;
    PRIVATE(new,confirm_popup) = MakePopup("confirm", new, MakeConfirmPopup);
    PRIVATE(new,backed_up) = False;

    XtVaGetValues(new, XtNstring, &fileName, NULL);
    if (strlen(fileName) > 0)
    {
	check = CheckPermission(fileName, &exists, &mode);
    }
    
    if (strlen(fileName) > 0 && (check == 0) && exists)
    {
        XtVaSetValues(new,
		      XtNtype, XawAsciiFile,
		      XtNeditType, mode == READ ? XawtextRead : XawtextEdit,
		      NULL);
	atw->axe.associated_file = NULL;
	atw->axe.null_file = False;
    }
    else
    {
        if (strlen(fileName) == 0)
	{
	    atw->axe.associated_file = NULL;
	}
        else
	{
	    atw->axe.associated_file = XtNewString(fileName);
	}

	XtVaSetValues(new,
		      XtNtype, XawAsciiFile,
		      XtNstring, nullfile, 
		      XtNeditType, XawtextEdit,
		      NULL);

	atw->axe.null_file = True;
    }
    atw->axe.is_modified = False;

    XtAddCallback(new, XtNgotoLineCallback, GotoLine, (XtPointer) 0);
    XtAddCallback(new, XtNincludeFileCallback, IncludeFile, (XtPointer) 0);
    XtAddCallback(new, XtNinsertControlCallback, InsertControl, (XtPointer) 0);
    XtAddCallback(new, XtNpreferencesCallback, SetPreferences, (XtPointer) 0);
    XtAddCallback(new, XtNmodifiedCallback, SourceChange, (XtPointer) 0);
    XtAddCallback(new, XtNclearBufferCallback, ClearBuffer, (XtPointer) 0);
    XtAddCallback(new, XtNsaveFileCallback, SaveFile, (XtPointer) 0);
    XtAddCallback(new, XtNsaveAsCallback, SaveAs, (XtPointer) 0);
    XtAddCallback(new, XtNloadFileCallback, LoadFile, (XtPointer) 0);
    XtAddCallback(new, XtNreloadFileCallback, ReloadFile, (XtPointer) 0);
    XtAddCallback(new, XtNnoUndoCallback, NoUndo, (XtPointer) 0);

    AxeiiTextWatchForChanges(atw);
}

static void
Destroy(w)
     Widget w;
{
    Display *display = XtDisplay(w);
    Atom gotAtom;
    int gotFormat;
    unsigned long gotItems, moreBytes; 
    unsigned char *propValue;
    Window wid;

    if (PRIVATE(w,initial_directory))
    {
	XtFree(PRIVATE(w,initial_directory));
    }

    if (PRIVATE(w,associated_file))
    {
	XtFree(PRIVATE(w,associated_file));
    }

    if (XGetWindowProperty(display,
                           XtWindow(w),
                           XInternAtom(display, AXE_COAXE, False),
                           0, 1,
                           False,
                           XA_WINDOW,
                           &gotAtom,
                           &gotFormat,
                           &gotItems,
                           &moreBytes,
                           &propValue) == Success)
    {
        if (gotAtom == XA_WINDOW)
        {
            wid = *((Window *) propValue);

            /*
             * Don't want to know if an error is generated
             * because client window has disappeared
             */
            TrapErrors(display);

            XChangeProperty(display,
                            wid,
                            XInternAtom(display, AXE_COAXE, False),
                            XA_STRING,
                            8,
                            PropModeReplace,
                            NULL,
                            0);

            DontTrapErrors(display); 
        }
        XtFree((char *) propValue);
    }
}

/* ARGSUSED */
static Boolean
SetValues(old, request, new, args, num_args)
     Widget old, request, new;
     ArgList args;
     Cardinal *num_args;
{
    AxeTextWidget oldatw = (AxeTextWidget) old;
    AxeTextWidget newatw = (AxeTextWidget) new;

    /* Too late if FileNominator has already been created */
    if (PRIVATE(new,file_nominator))
    {
	return False;
    }

#define NE(field) (oldatw->field != newatw->field)

    if (NE(axe.associated_directory) && newatw->axe.associated_directory
	                         && *(newatw->axe.associated_directory) == '/')
    {
	PRIVATE(new,initial_directory)
	                       = XtNewString(newatw->axe.associated_directory);
    }

#undef NE

    return False;
}

/*************************************************************
 *
 *                         utilities
 *
 *************************************************************/

static int 
CheckPermission(file, exists, mode)
     String file;
     Boolean *exists;
     AccessMode *mode;
{
    int status = 0;
    struct stat buf;

    *exists = False;
    *mode = NONE;
    
    if (!file)
    {
	return status;
    }

    if (access(file, F_OK) == 0)
    {
        *exists = True;
        status = 0;
        if (access(file, R_OK | W_OK) == 0)
        {
            *mode = EDIT;
        }
        else if (access(file, R_OK) == 0)
        {
            *mode = READ;
        }
        else
        {
            status = EACCES;
        }
    }
    else if (errno == ENOENT)
    {
        *mode = EDIT;
    }
    else
    {
        status = errno;
    }

    if (status == 0 && *exists && *mode != NONE)
    {
        if (stat(file, &buf) == 0)
        {
            if (!(buf.st_mode & S_IFREG))
            {
                status = EACCES;
            }
        }
        else
        {
            status = errno;
        }
    }

    return status;
}

static void
RecordAndNotifyChange(w, state)
     Widget w;
     Boolean state;
{
    PRIVATE(w,is_modified) = state;
    XtCallCallbacks(w, XtNchangeCallback, (XtPointer) 0);
}
    
/*ARGSUSED*/
static void
SourceChange(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    RecordAndNotifyChange(w, True);
}

static void
MessageNotify(w, code)
     Widget w;
     int code;
{
    XtCallCallbackList(w, PRIVATE(w,message_callbacks), (XtPointer) code);
}

static Boolean
CreatePopupLater(client_data)
     XtPointer client_data;
{
    Widget popup = (Widget) client_data;
    XtCreatePopupChildProc create;

    XtVaGetValues(popup,
		  XtNcreatePopupChildProc, &create,
		  NULL);

    XtVaSetValues(popup,
		  XtNcreatePopupChildProc, (XtCreatePopupChildProc) 0,
		  NULL);

    if (create)
    {
	(*create) (popup);
    }

    return True;
}

static void
PositionedPopup(axe, popup)
     Widget axe, popup;
{
    PopupCentred(popup,
		 PRIVATE(axe,grab_on_popup) ? XtGrabExclusive : XtGrabNone);
}

static Widget
MakePopup(name, parent, childProc)
     String name;
     Widget parent;
     XtCreatePopupChildProc childProc;
{
    Widget pop;

    pop = XtVaCreatePopupShell(name, transientShellWidgetClass, parent,
			       XtNcreatePopupChildProc, childProc,
			       NULL);

    if (childProc)
    {
	XtAppAddWorkProc(XtWidgetToApplicationContext(parent),
			                   CreatePopupLater, (XtPointer) pop);
    }

    return pop;
}

/* ARGSUSED */
static void
PopDown(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    XtPopdown((Widget) client_data);
}

static void
NominateFile(widget, title, callback)
     Widget widget;
     String title;
     XtCallbackProc callback;
{
    Widget fileNom, nominator;
    XtCallbackList callbacks, callb;
    XtCallbackStatus callbackStatus;
    int closure = 10101;

    if (!PRIVATE(widget,file_nominator))
    {
        PRIVATE(widget,file_nominator) = MakeFileNominator(widget);
    }
    fileNom = PRIVATE(widget,file_nominator);
    nominator = FileNominatorWindowFileNominatorWidget(fileNom);

    if ((callbackStatus = XtHasCallbacks(nominator, XtNselectCallback))
	                                                  == XtCallbackHasSome)
    {
	XtVaGetValues(nominator,
		      XtNselectCallback, &callbacks,
		      NULL);

	for (callb = callbacks;  (*callb).callback;  ++callb)
	{
	    if ((*callb).closure == (XtPointer) closure
		                              && (*callb).callback != callback)
	    {
		XtRemoveCallback(nominator, XtNselectCallback,
				          (*callb).callback, (*callb).closure);
		XtAddCallback(nominator, XtNselectCallback,
			                        callback, (XtPointer) closure);
	    }
	}
    }
    else if (callbackStatus == XtCallbackHasNone)
    {
	XtAddCallback(nominator, XtNselectCallback, callback,
		                                          (XtPointer) closure);
    }

    XtVaSetValues(fileNom,
		  XtNtitle, title,
		  NULL);
	
    PositionedPopup(widget, fileNom);
}

static void
ConfirmDiscard(widget, callback)
     Widget widget;
     XtCallbackProc callback;
{
    ConfirmerRequestConfirmation(PRIVATE(widget,confirm_popup),
				 "There are unsaved changes",
				 "Discard changes", callback,
				 "unused", NULL,
				 (XtPointer) widget);
}

/*************************************************************
 *
 *                    FileNominator Popup
 *
 *************************************************************/

/* ARGSUSED */
static void
OnPopup(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    FileNominatorClearName((Widget) client_data);
}

static Widget
MakeFileNominator(axeText)
     Widget axeText;
{
    char *assocFile, assocDir[MAXPATHLEN];
    Widget fileNomWin, fileNom;

    if ( (assocFile = AxeTextGetAssociatedFile(axeText)) )
    {
	strcpy(assocDir, assocFile);
	assocDir[rindex(assocDir, '/') - assocDir] = '\0';
    }

    fileNomWin = XtVaCreatePopupShell(
	"filenompop", fileNominatorWindowWidgetClass, axeText,
	XtNinitialDirectory, assocFile ? assocDir
				       : PRIVATE(axeText,initial_directory),
	NULL);

    if (PRIVATE(axeText,initial_directory))
    {
	XtFree(PRIVATE(axeText,initial_directory));
	PRIVATE(axeText,initial_directory) = (String) 0;
    }

    fileNom = FileNominatorWindowFileNominatorWidget(fileNomWin);
    XtAddCallback(fileNomWin, XtNpopupCallback, OnPopup, (XtPointer) fileNom);
    XtAddCallback(fileNom, XtNcancelCallback, PopDown, (XtPointer) fileNomWin);

    XtRealizeWidget(fileNomWin);

    return fileNomWin;
}

/*************************************************************
 *
 *                    NumericPad Popup
 *
 *************************************************************/

/* ARGSUSED */
static void
GotoLineEnter(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axeText = AxeTextWidgetParentOf(widget);
    NumericPadReturnStruct *number = (NumericPadReturnStruct *) call_data;

    XtPopdown(PRIVATE(axeText,numeric_pad));

    AxeTextGotoLine(axeText, number->conversion); 
}

/* ARGSUSED */
static void
ClearPadDown(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    NumericPadClear((Widget) client_data);
}

static Widget
MakeNumericPad(axeText)
     Widget axeText;
{
    Widget popup = XtVaCreatePopupShell("gotopop", transientShellWidgetClass,
					                        axeText, NULL);
    Widget numPad
	= XtVaCreateManagedWidget("gotoBody",
				  numericPadWidgetClass, popup, NULL);
    XtAddCallback(numPad, XtNenterCallback, GotoLineEnter, (XtPointer) popup);
    XtAddCallback(numPad, XtNcancelCallback, PopDown, (XtPointer) popup);

    XtAddCallback(popup, XtNpopdownCallback, ClearPadDown, (XtPointer) numPad);

    XtRealizeWidget(popup);

    return popup;
}

/*************************************************************
 *
 *                  ControlCodeSelector Popup
 *
 *************************************************************/

/* ARGSUSED */
static void
ControlEnter(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axeText = AxeTextWidgetParentOf(widget);
    char ch = (char) ((int) call_data);
    XawTextBlock textBlock;
    XawTextPosition insertPos;
    void (*proc)();

    insertPos = XawTextGetInsertionPoint(axeText);
    textBlock.firstPos = 0;
    textBlock.length = 1;
    textBlock.ptr = &ch;
    textBlock.format = FMT8BIT;

    if ( (proc = AxeiiTextUndoPreInsert(axeText)) )
    {
	(*proc)(axeText);
    }

    XawTextReplace(axeText, insertPos, insertPos, &textBlock);
    
    XawTextSetInsertionPoint(axeText, insertPos + 1);
}

/* ARGSUSED */
static void
ClearCtrlDown(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    ControlCodeSelectorClear((Widget) client_data);
}

static Widget
MakeControlSelector(axeText)
     Widget axeText;
{
    Widget popup = XtVaCreatePopupShell("control", transientShellWidgetClass,
					axeText, NULL);
    Widget ctrlSel
	= XtVaCreateManagedWidget("ctrlBody", controlCodeSelectorWidgetClass,
				  popup, NULL);
					       
    XtAddCallback(ctrlSel, XtNenterCallback, ControlEnter, (XtPointer) popup);
    XtAddCallback(ctrlSel, XtNcancelCallback, PopDown, (XtPointer) popup);

    XtAddCallback(popup, XtNpopdownCallback, ClearCtrlDown,
		                                          (XtPointer) ctrlSel);
    XtRealizeWidget(popup);

    return popup;
}

/*************************************************************
 *
 *                       Confirmer Popup
 *
 *************************************************************/

static void
MakeConfirmPopup(popup)
     Widget popup;
{
    /*
     * ConfirmerWidget is a subclass of TransientShell, so grab the
     * name of the shell that MakePopup made then jettison it.
     */
    Widget axeText = XtParent(popup), oldpop = popup;

    popup = XtVaCreatePopupShell(XtName(oldpop),
				 confirmerWidgetClass, axeText, NULL);

    XtDestroyWidget(oldpop);

    PRIVATE(axeText,confirm_popup) = popup;
}

/*************************************************************
 *
 *                     Preference Popup
 *
 *************************************************************/

static Widget
MakePreferencePopup(axeText)
     Widget axeText;
{
    Widget popup
	= XtVaCreatePopupShell("preferences", transientShellWidgetClass,
			       axeText, NULL);
    Widget prefPop
	= XtVaCreateManagedWidget("prefBody", preferenceWidgetClass, popup,
				  NULL);
    XtAddCallback(prefPop, XtNenterCallback, AcceptPreferences,
		                                            (XtPointer) popup);
    XtAddCallback(prefPop, XtNcancelCallback, PopDown, (XtPointer) popup);

    XtRealizeWidget(popup);

    return popup;
}

/*************************************************************
 *
 *                        goto-line
 *
 *************************************************************/

/*ARGSUSED*/
static void 
GotoLine(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    if (!PRIVATE(w,numeric_pad))
    {
	PRIVATE(w,numeric_pad) = MakeNumericPad(w);
    }

    PositionedPopup(w, PRIVATE(w,numeric_pad));
}

/*************************************************************
 *
 *                        include-file
 *
 *************************************************************/

/* ARGSUSED */
static void
IncludeFileSelect(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axe = AxeTextWidgetParentOf(widget);
    FileNominatorStruct *data = (FileNominatorStruct *) call_data;
    String newName;
    void (*proc)();

    extern Boolean InsertFileNamed();

    if (data->directoryPart && data->filenamePart &&
	                      (data->filenameStatus & FileNominatorReadable))
    {
        newName = XtMalloc(strlen(data->directoryPart)
                           + strlen(data->filenamePart) + 1);
        strcpy(newName, data->directoryPart);
        strcat(newName, data->filenamePart);

	if ( (proc = AxeiiTextUndoPreInsert(axe)) )
	{
	    (*proc)(axe);
	}

	XtPopdown(PRIVATE(axe,file_nominator));
	(void) InsertFileNamed(axe, newName);

	XtFree(newName);
    }
    else
    {
	MessageNotify(axe, AxeTextunreadableFile);
    }
}

/* ARGSUSED */
static void
IncludeFile(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    NominateFile(widget, "insert file", IncludeFileSelect);
}

/*************************************************************
 *
 *                        insert-control
 *
 *************************************************************/

/* ARGSUSED */
static void
InsertControl(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    if (!PRIVATE(w,control_selector))
    {
	PRIVATE(w,control_selector) = MakeControlSelector(w);
    }

    PositionedPopup(w, PRIVATE(w,control_selector));
}

/*************************************************************
 *
 *                       set-preferences
 *
 *************************************************************/

/*ARGSUSED*/
static void 
AcceptPreferences(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    Widget axeText = AxeTextWidgetParentOf(w);
    PreferenceReturnStruct *preferences = (PreferenceReturnStruct *) call_data;

    XtPopdown((Widget) client_data);

    XtVaSetValues(axeText,
		  XtNscrollVertical, preferences->scrollMode,
		  XtNwrap, preferences->wrapMode,
		  XtNautoFill, preferences->autoFill,
		  XtNeditType, preferences->editType,
		  XtNtabEvery, preferences->tabEvery,
		  NULL);
}

/*ARGSUSED*/
static void 
SetPreferences(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    if (!PRIVATE(w,preference_popup))
    {
	PRIVATE(w,preference_popup) = MakePreferencePopup(w);
    }

    PositionedPopup(w, PRIVATE(w,preference_popup));
}

/*************************************************************
 *
 *                         clear-text
 *
 *************************************************************/

/*ARGSUSED*/
static void 
ReallyClearBuffer(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    Widget axe = (Widget) client_data;

    if (w != axe)
    {
	XtPopdown(PRIVATE(axe,confirm_popup));
    }

    if (PRIVATE(axe,associated_file))
    {
	XtFree(PRIVATE(axe,associated_file));
    }

    PRIVATE(axe,null_file) = True;
    PRIVATE(axe,associated_file) = NULL;
    PRIVATE(axe,backed_up) = False;

    XtVaSetValues(axe, XtNstring, nullfile, NULL);

    RecordAndNotifyChange(axe, False);
    AxeiiTextWatchForChanges(axe);
}

/*ARGSUSED*/
static void 
ClearBuffer(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    if (PRIVATE(w,is_modified))
    {
	ConfirmDiscard(w, ReallyClearBuffer);
    }
    else
    {
	ReallyClearBuffer(w, (XtPointer) w, (XtPointer) 0);
    }
}

/*************************************************************
 *
 *                          save-file
 *
 *************************************************************/

static Boolean
SaveAsFile(asciiSrc, filename)
     Widget asciiSrc;
     String filename;
{
    Widget text = XtParent(asciiSrc);
    XawTextPosition pos = XawTextGetInsertionPoint(text), endpos, eol;
    XawTextBlock textBlock;
    char backupName[BUFSIZ];
    char *r;
    Boolean backedUp = False;
    int status;
    struct stat stats;

    endpos = XawTextSourceScan(asciiSrc, pos, XawstAll, XawsdRight, 1, True);

    eol = XawTextSourceScan(asciiSrc, endpos, XawstEOL, XawsdLeft, 1, False);
    if (eol != endpos)
    {
        textBlock.firstPos = 0;
        textBlock.length = 1;
        textBlock.ptr = "\n";
        textBlock.format = FMT8BIT;

        XawTextReplace(text, endpos, endpos, &textBlock);
    }

    if (PRIVATE(text,enable_backups) && !PRIVATE(text,backed_up))
    {
	r = rindex(filename, '/') + 1;
	strncpy(backupName, filename, r - filename);
	backupName[r - filename] = '\0';
	strcat(backupName, PRIVATE(text,backup_name_prefix));
	strcat(backupName, r);
	strcat(backupName, PRIVATE(text,backup_name_suffix));

	
	if ( (status = rename(filename, backupName)) == 0 || errno == ENOENT)
	{
	    status = stat(backupName, &stats);
	    backedUp = True;
	}
	else
	{
	    return False;
	}
    }
	
    if (XawAsciiSaveAsFile(asciiSrc, filename))
    {
	if (backedUp)
	{
	    if (status == 0)
	    {
		(void) chmod(filename, stats.st_mode);
	    }
	    PRIVATE(text,backed_up) = True;
	}

	MessageNotify(text, AxeTextsaveSuccess);

	return True;
    }
    else
    {
	if (backedUp)
	{
	    /* What if this goes wrong? */
	    (void) rename(backupName, filename);
	}

	/* Error notification to be made by caller */
	return False;
    }
}

/*ARGSUSED*/
static void
SaveOver(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    AxeTextWidget axe = (AxeTextWidget) AxeTextWidgetParentOf(w);

    if (!AxeTextSaveAsFile(axe,	(FileNominatorStruct *) client_data))
    {
	MessageNotify(axe, AxeTextsaveFailure);
    }

    AxeTextFreeNominatorStruct((FileNominatorStruct *) client_data);
}

/*ARGSUSED*/
static void
SaveAgain(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    AxeTextFreeNominatorStruct((FileNominatorStruct *) client_data);

    NominateFile(AxeTextWidgetParentOf(w), "file save", SaveFileSelect);
}

/*ARGSUSED*/
static void
SaveFileSelect(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    Widget axe = AxeTextWidgetParentOf(w);
    FileNominatorStruct *data = (FileNominatorStruct *) call_data, *nomination;
    
    if (data->directoryPart && data->filenamePart)
    {
	if (data->filenameStatus & FileNominatorNonexistent)
	{
	    if (AxeTextSaveAsFile(axe, data))
	    {
		XtPopdown(PRIVATE(axe,file_nominator));
	    }
	    else
	    {
		MessageNotify(axe, AxeTextsaveFailure);
	    }
	}
	else
	{
	    XtPopdown(PRIVATE(axe,file_nominator));

	    /*
	     * The FileNominatorStruct in the call_data is owned by the
	     * FileNominator. As it is possible for the user to use the
	     * FileNominator for other purposes before responding to the
	     * Confirmer we had better take a copy.
	     */
	    nomination = XtNew(FileNominatorStruct);

	    nomination->directoryPart = XtNewString(data->directoryPart);
	    nomination->directoryStatus = data->directoryStatus;
	    nomination->filenamePart = XtNewString(data->filenamePart);
	    nomination->filenameStatus = data->filenameStatus;

	    ConfirmerRequestConfirmation(PRIVATE(axe,confirm_popup),
					 "The file already exists",
					 "overwrite", SaveOver,
					 "re-select", SaveAgain,
					 nomination);
	}
    }
    else
    {
	MessageNotify(axe, AxeTextinvalidDirectory);
    }
}

static Boolean
FileSaved(w)
     Widget w;
{
    Boolean saved;

    if ((saved = SaveAsFile(XawTextGetSource(w), AxeTextGetAssociatedFile(w))))
    {
	RecordAndNotifyChange(w, False);
	AxeiiTextWatchForChanges(w);
    }

    return saved;
}

/*ARGSUSED*/
static void
SaveFile(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    if (!PRIVATE(w,is_modified))
    {
	MessageNotify(w, AxeTextnotSaved);
    }
    else if (PRIVATE(w,null_file) && !PRIVATE(w,associated_file))
    {
	NominateFile(w, "file save", SaveFileSelect);
    }
    else if (!FileSaved(w))
    {
	MessageNotify(w, AxeTextsaveFailure);
    }
}

/*************************************************************
 *
 *                           save-as
 *
 *************************************************************/

/*ARGSUSED*/
static void
SaveAs(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    NominateFile(w, "file save", SaveFileSelect);
}

/*************************************************************
 *
 *                         reload-file
 *
 *************************************************************/

/* ARGSUSED */
static void
ReloadFileConfirm(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axe = (Widget) client_data;
    char *filename, saveFilename[MAXPATHLEN];
    XawTextEditType editType;
    Boolean exists;
    AccessMode mode;
    void (*proc)();
    
    if (widget != axe)
    {
	XtPopdown(PRIVATE(axe,confirm_popup));
    }

    XtVaGetValues(axe,
		  XtNstring, &filename,
		  XtNeditType, &editType,
		  NULL);
    strcpy(saveFilename, filename);

    if (CheckPermission(saveFilename, &exists, &mode) != 0)
    {
	MessageNotify(axe, AxeTextreloadFailure);
	return;
    }
    
    /* AsciiSrc attempts to open for update if mode is XawtextEdit,
     * so avoid by switching into XawtextRead mode. But still
     * making assumption that access hasn't been removed altogether.
     */
    if (editType == XawtextEdit)
    {
	XtVaSetValues(axe, XtNeditType, XawtextRead, NULL);
    }
		     
    /* Reload the file */
    XtVaSetValues(axe, XtNstring, saveFilename, NULL);
    
    /* Adjust editing mode if necessary */
    if (mode == EDIT)
    {
	XtVaSetValues(axe, XtNeditType, XawtextEdit, NULL);
    }

    RecordAndNotifyChange(axe, False);
    AxeiiTextWatchForChanges(axe);

    if ( (proc = AxeiiTextUndoPostLoad(axe)) )
    {
        (*proc)(axe);
    }
}

/* ARGSUSED */
static void
ReloadFile(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    String file;

    XtVaGetValues(widget, XtNstring, &file, NULL);
    if (strcmp(file, nullfile) == 0)
    {
	MessageNotify(widget, AxeTextreloadFailure);
	return;
    }

    if (PRIVATE(widget,is_modified))
    {
	ConfirmDiscard(widget, ReloadFileConfirm);
    }
    else
    {
	ReloadFileConfirm(widget, (XtPointer) widget, (XtPointer) 0);
    }
}

/*************************************************************
 *
 *                          load-file
 *
 *************************************************************/

/*ARGSUSED*/
static void
LoadFileSelect(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    Widget axe = AxeTextWidgetParentOf(w);

    if (AxeTextLoadFile(axe, (FileNominatorStruct *) call_data))
    {
	XtPopdown(PRIVATE(axe,file_nominator));
    }
}

/* ARGSUSED */
static void
LoadFileConfirm(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axe = (Widget) client_data;

    XtPopdown(PRIVATE(axe,confirm_popup));

    NominateFile(axe, "file load", LoadFileSelect);
}

/* ARGSUSED */
static void
LoadFile(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    if (PRIVATE(widget,is_modified))
    {
	ConfirmDiscard(widget, LoadFileConfirm);
    }
    else
    {
	NominateFile(widget, "file load", LoadFileSelect);
    }
}

/* ARGSUSED */
static void
NoUndo(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    MessageNotify(widget, AxeTextnoUndo);
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

Widget
AxeTextWidgetParentOf(w)
     Widget w;
{
    Widget axeText;

    for (axeText = w;  axeText;  axeText = XtParent(axeText))
    {
	if (XtIsSubclass(axeText, axeTextWidgetClass))
	{
	    return axeText;
	}
    }

    return (Widget) 0;
}

Widget
AxeTextFileNominator(w)
     Widget w;
{
    if (XtIsSubclass(w, axeTextWidgetClass))
    {
	CompositeWidget nominator;
	
	if ( (nominator = ((CompositeWidget) PRIVATE(w,file_nominator)) ))
	{
	    return nominator->composite.children[0];
	}
    }

    return (Widget) 0;
}

Boolean
AxeTextIsModified(atw)
     Widget atw;
{
    if (XtIsSubclass(atw, axeTextWidgetClass))
    {
	return PRIVATE(atw,is_modified);
    }
    else
    {
	return False;
    }
}

String
AxeTextGetAssociatedFile(atw)
     Widget atw;
{
    String file;

    if (!XtIsSubclass(atw, axeTextWidgetClass))
    {
	return (String) 0;
    }

    if (PRIVATE(atw,null_file))
    {
	if (PRIVATE(atw,associated_file))
	{
	    return PRIVATE(atw,associated_file);
	}
	else
	{
	    return (String) 0;
	}
    }
    else
    {
	XtVaGetValues(atw,
		      XtNstring, &file,
		      NULL);
	return file;
    }
}

Boolean
AxeTextSaveFile(atw)
     Widget atw;
{
    if (!XtIsSubclass(atw, axeTextWidgetClass))
    {
	return False;
    }
    
    if (!AxeTextGetAssociatedFile(atw))
    {
	return False;
    }
    else
    {
	return FileSaved(atw);
    }
}

Boolean
AxeTextSaveAsFile(widget, fnomStruct)
     Widget widget;
     FileNominatorStruct *fnomStruct;
{
    String fileName, newName;
     
    if (!XtIsSubclass(widget, axeTextWidgetClass))
    {
	return False;
    }

    if (fnomStruct->directoryPart && fnomStruct->filenamePart)
    {
	newName = XtMalloc(strlen(fnomStruct->directoryPart)
			   + strlen(fnomStruct->filenamePart) + 1);
	strcpy(newName, fnomStruct->directoryPart);
	strcat(newName, fnomStruct->filenamePart);

	if (SaveAsFile(XawTextGetSource(widget), newName))
	{
	    if (PRIVATE(widget,null_file))
	    {
		PRIVATE(widget,null_file) = False;
		if (PRIVATE(widget,associated_file))
		{
		    XtFree(PRIVATE(widget,associated_file));
		}
		PRIVATE(widget,associated_file) = NULL;
		XtVaSetValues(widget, XtNstring, newName, NULL);
	    }
	    else
	    {
		XtVaGetValues(widget, XtNstring, &fileName, NULL);
		if (strcmp(fileName, newName) != 0)
		{
		    XtVaSetValues(widget, XtNstring, newName, NULL);
		}
	    }

	    RecordAndNotifyChange(widget, False);
	    AxeiiTextWatchForChanges(widget);

	    XtFree(newName);

	    return True;
	}
	else
	{
	    MessageNotify(widget, AxeTextsaveFailure);

	    XtFree(newName);

	    return False;
	}
    }
    else
    {
	MessageNotify(widget, AxeTextinvalidDirectory);
	return False;
    }
}

void
AxeTextFileToNominatorStruct(file, nomStruct)
     String file;
     FileNominatorStruct *nomStruct;
{
    String r;
    struct stat fstats;
    int status;

    status = stat(file, &fstats);
    if (status != -1 && fstats.st_mode & S_IFDIR)
    {
	nomStruct->filenamePart = NULL;
	nomStruct->filenameStatus = FileNominatorNonexistent;
	status |= (access(file, R_OK) == 0) ? FileNominatorReadable : 0;
	status |= (access(file, W_OK) == 0) ? FileNominatorWritable : 0;
	status |= (access(file, X_OK) == 0) ? FileNominatorExecutable : 0;
	nomStruct->directoryPart = XtNewString(file);
	nomStruct->directoryStatus = status;
    }
    else if (status == 0 || (status == -1 && errno == ENOENT))
    {
	status |= (access(file, R_OK) == 0) ? FileNominatorReadable : 0;
	status |= (access(file, W_OK) == 0) ? FileNominatorWritable : 0;
	status |= (access(file, X_OK) == 0) ? FileNominatorExecutable : 0;
	nomStruct->filenameStatus = status;
	r = rindex(file, '/');
	nomStruct->filenamePart = XtNewString(r + 1);
	*(r + 1) = '\0';
	status = stat(file, &fstats);
	if (status == -1)
	{
	    nomStruct->directoryPart = 0;
	    nomStruct->directoryStatus = FileNominatorNonexistent;
	}
	else
	{
	    status |= (access(file, R_OK) == 0) ? FileNominatorReadable : 0;
	    status |= (access(file, W_OK) == 0) ? FileNominatorWritable : 0;
	    status |= (access(file, X_OK) == 0) ? FileNominatorExecutable : 0;
	    nomStruct->directoryPart = XtNewString(file);
	    nomStruct->directoryStatus = status;
	}
    }
    else
    {
	nomStruct->directoryPart = NULL;
	nomStruct->directoryStatus = FileNominatorNonexistent;
	nomStruct->filenamePart = NULL;
	nomStruct->filenameStatus = FileNominatorNonexistent;
    }
}

void
AxeTextFreeNominatorStruct(nomStruct)
     FileNominatorStruct *nomStruct;
{
    XtFree(nomStruct->filenamePart);
    XtFree(nomStruct->directoryPart);
}

Boolean
AxeTextLoadFile(widget, fnomStruct)
     Widget widget;
     FileNominatorStruct *fnomStruct;
{
    String newName, useName;
    XawTextEditType editMode;
    Boolean returnStatus;
    void (*proc)();
    
    if (!XtIsSubclass(widget, axeTextWidgetClass))
    {
	return False;
    }

    if (!fnomStruct->filenamePart)
    {
	MessageNotify(widget, AxeTextinvalidFilename);
	return False;
    }

    if (!fnomStruct->directoryPart)
    {
	MessageNotify(widget, AxeTextinvalidDirectory);
	return False;
    }

    if (fnomStruct->directoryStatus & FileNominatorExecutable)
    {
	newName = XtMalloc(strlen(fnomStruct->directoryPart)
			   + strlen(fnomStruct->filenamePart) + 1);
	strcpy(newName, fnomStruct->directoryPart);
	strcat(newName, fnomStruct->filenamePart);

	if (PRIVATE(widget,associated_file))
	{
	    XtFree(PRIVATE(widget,associated_file));
	}

	if (fnomStruct->filenameStatus & FileNominatorNonexistent)
	{
	    PRIVATE(widget,null_file) = True;
	    PRIVATE(widget,associated_file) = XtNewString(newName);
	    useName = XtNewString(nullfile);
	    editMode = XawtextEdit;
	}
	else 
	{
	    PRIVATE(widget,null_file) = False;
	    PRIVATE(widget,associated_file) = NULL;
	    useName = newName;
	    editMode = (fnomStruct->filenameStatus & FileNominatorWritable)
		                                   ? XawtextEdit : XawtextRead;
	}

	if (fnomStruct->filenameStatus & FileNominatorReadable ||
	    fnomStruct->filenameStatus & FileNominatorNonexistent)
	{
	    XtVaSetValues(widget,
			  XtNstring, useName,
			  XtNeditType, editMode,
			  NULL);

	    RecordAndNotifyChange(widget, False);
	    AxeiiTextWatchForChanges(widget);
	    PRIVATE(widget,backed_up) = False;

	    if ( (proc = AxeiiTextUndoPostLoad(widget)) )
	    {
		(*proc)(widget);
	    }

	    returnStatus = True;
	}
	else
	{
	    MessageNotify(widget, AxeTextunreadableFile);
	    returnStatus = False;
	}

	XtFree(newName);
	if (useName != newName)
	{
	    XtFree(useName);
	}
    }
    else
    {
	MessageNotify(widget, AxeTextinvalidDirectory);
	returnStatus = False;
    }

    return returnStatus;
}

void
AxeTextGotoLine(atw, line)
     Widget atw;
     int line;
{
    XawTextPosition bol, eol;

    if (!XtIsSubclass(atw, axeTextWidgetClass))
    {
	return;
    }

    AxeiiTextGotoLine(atw, line);

    bol = XawTextGetInsertionPoint(atw);
    eol = XawTextSourceScan(XawTextGetSource(atw), bol, XawstEOL, XawsdRight,
			    1, True);
    XawTextSetSelection(atw, bol, eol);

    XtCallActionProc(atw, "redraw-display",
		     (XEvent *) 0, (String *) 0, (Cardinal) 0);

}

#undef AXEII
#undef PRIVATE
