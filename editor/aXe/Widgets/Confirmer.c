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
 *
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

#include <X11/IntrinsicP.h>	
#include <X11/StringDefs.h>	
#include <X11/Xp/Table.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>

#include "util.h"

#include "ConfirmerP.h"
#include "warn.xbm"

#define CLASS(field) confirmerClassRec.confirmer_class.field
#define PRIVATE(w,field) (((ConfirmerWidget) w)->confirmer.field)

static void Initialize();
static Widget ConfirmerOf();
static void OnPopup(), Confirm(), Alt(), Cancel();

#define ShellOffset(field) XtOffsetOf(ConfirmerRec, shell.field)
#define Offset(field) XtOffsetOf(ConfirmerRec, confirmer.field)

static XtResource resources[] = {
    /* Shell Widget Resources */
    { XtNallowShellResize, XtCAllowShellResize, XtRBoolean, sizeof(Boolean),
	 ShellOffset(allow_shell_resize), XtRImmediate, (XtPointer) False},

    /* Confirmer Widget Resources */
    {XtNgrabOnPopup, XtCGrabOnPopup, XtRBoolean, sizeof(Boolean),
         Offset(grab_on_popup), XtRImmediate, (XtPointer) False},
};

#undef Offset

ConfirmerClassRec confirmerClassRec = {
  {
    /* superclass         */    (WidgetClass) &transientShellClassRec,
    /* class_name         */    "Confirmer",
    /* size               */    sizeof(ConfirmerRec),
    /* Class Initializer  */    NULL,
    /* class_part_initialize*/  NULL,
    /* Class init'ed ?    */    FALSE,
    /* initialize         */    Initialize,
    /* initialize_notify  */    NULL,
    /* realize            */    XtInheritRealize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    resources,
    /* resource_count     */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/    FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    XtInheritResize,
    /* expose             */    XtInheritExpose,
    /* set_values         */    NULL,
    /* set_values_hook      */  NULL,
    /* set_values_almost    */  XtInheritSetValuesAlmost,  
    /* get_values_hook      */  NULL,
    /* accept_focus       */    NULL,
    /* intrinsics version */    XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table             */  XtInheritTranslations,
    /* query_geometry       */  NULL,
    /* display_accelerator  */  NULL,
    /* extension            */  NULL
  },{
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child       */    XtInheritInsertChild,
    /* delete_child       */    XtInheritDeleteChild,
    /* extension            */  NULL
  },{
    /* extension            */  NULL
  },{
    /* extension            */  NULL
  },{
    /* extension            */  NULL
  },{
    /* extension            */  NULL
  },{
    /* extension            */  NULL
    }
};

WidgetClass confirmerWidgetClass = (WidgetClass) &confirmerClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    Display *display = XtDisplay(new);
    Widget conform, actions, cancel; 
    static char layout[] = "exclam 0 0; warning 0 1; actions 0 2;";
    XpTableLoc tableLoc;

    if (!CLASS(warnLogo))
    {
        CLASS(warnLogo) =
            XCreateBitmapFromData(display, XDefaultRootWindow(display),
                                  (char *) warn_bits, warn_width, warn_height);
    }

    XtAddCallback(new, XtNpopupCallback, OnPopup, (XtPointer) 0);

    tableLoc = XpTableLocParse(layout);
    conform = XtVaCreateManagedWidget("conf", xpTableWidgetClass, new,
				      XtNlayout, tableLoc,
				      XtNmarginWidth, 5,
				      XtNmarginHeight, 5,
				      XtNrowSpacing, 5,
				      XtNcolumnSpacing, 5,
				      NULL);
    XpTableLocFree(tableLoc);

    XtVaCreateManagedWidget("exclam", labelWidgetClass, conform,
				     XtNbitmap, CLASS(warnLogo),
				     XtNborderWidth, 0,
				     NULL);

    PRIVATE(new,warning_widget) =
	XtVaCreateManagedWidget("warning", labelWidgetClass, conform,
				NULL);

    actions = XtVaCreateManagedWidget("actions", boxWidgetClass, conform,
				      NULL);

    PRIVATE(new,confirm_widget) =
	XtVaCreateManagedWidget("confirm", commandWidgetClass, actions,
				NULL);

    PRIVATE(new,alt_widget) =
	XtVaCreateManagedWidget("alternative", commandWidgetClass, actions,
				NULL);

    cancel = XtVaCreateManagedWidget("cancel", commandWidgetClass, actions,
				     NULL);
    XtAddCallback(cancel, XtNcallback, Cancel, (XtPointer) new);

    XtInstallAllAccelerators(conform, conform);
}

static Widget
ConfirmerOf(widget)
     Widget widget;
{
    Widget w;

    for (w = widget;  w;  w = XtParent(w))
    {
        if (XtIsSubclass(w, confirmerWidgetClass))
        {
            return w;
        }
    }

    return (Widget) 0;
}    

/* ARGSUSED */
static void
OnPopup(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    XBell(XtDisplay(widget), 100);
}

/* ARGSUSED */
static void
Confirm(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget confirmer = ConfirmerOf(widget);

    XtPopdown(confirmer);
    PRIVATE(confirmer,confirm_callback) (widget, client_data, call_data);
}

/* ARGSUSED */
static void
Alt(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget confirmer = ConfirmerOf(widget);

    XtPopdown(confirmer);
    PRIVATE(confirmer,alt_callback) (widget, client_data, call_data);
}

/* ARGSUSED */
static void
Cancel(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    XtPopdown((Widget) client_data);
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

void
ConfirmerRequestConfirmation(w, warning, confirm, confCallback,
			     alt, altCallback, client_data)
     Widget w;
     String warning;
     String confirm, alt;
     XtCallbackProc confCallback, altCallback;
     XtPointer client_data;
{
    String nlab;

    if (!XtIsSubclass(w, confirmerWidgetClass))
    {
	return;
    }
    
    XtVaSetValues(PRIVATE(w,warning_widget), XtNlabel, warning, NULL);

    nlab = XtMalloc(strlen(confirm) + 6);
    strcpy(nlab, confirm);
    strcat(nlab, " (^A)");

    XtVaSetValues(PRIVATE(w,confirm_widget),
		  XtNlabel, nlab,
		  NULL);

    XtFree(nlab);

    if (altCallback)
    {
	nlab = XtMalloc(strlen(alt) + 6);
	strcpy(nlab, alt);
	strcat(nlab, " (^B)");

    }
    else
    {
	nlab = alt;
    }

    XtVaSetValues(PRIVATE(w,alt_widget),
		  XtNlabel, nlab,
		  XtNsensitive, altCallback ? True : False,
		  NULL);

    if (nlab != alt)
    {
	XtFree(nlab);
    }

    XtRemoveAllCallbacks(PRIVATE(w,confirm_widget), XtNcallback);
    XtRemoveAllCallbacks(PRIVATE(w,alt_widget), XtNcallback);
    if (confCallback)
    {
	PRIVATE(w,confirm_callback) = confCallback;
	XtAddCallback(PRIVATE(w,confirm_widget),
		      XtNcallback, Confirm,
		      (confCallback == ConfirmerPopdown) ? (XtPointer) w
		                                         : client_data);
    }
    if (altCallback)
    {
	PRIVATE(w,alt_callback) = altCallback;
	XtAddCallback(PRIVATE(w,alt_widget),
		      XtNcallback, Alt,
		      (altCallback == ConfirmerPopdown) ? (XtPointer) w
		                                        : client_data);
    }

    PopupCentred(w, PRIVATE(w,grab_on_popup) ? XtGrabExclusive : XtGrabNone);
}

/* ARGSUSED */
void
ConfirmerPopdown(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    XtPopdown((Widget) client_data);
}

