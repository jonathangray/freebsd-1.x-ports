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

#include <X11/Xos.h>
#include <X11/IntrinsicP.h>	
#include <X11/StringDefs.h>	
#include <AxeEditor.h>
#include <AxeText.h>

#include "AxeWindowP.h"

#define CARETHEIGHT (XT_REVISION < 5 ? caretHeight : 0)

#define CLASS(field) axeWindowClassRec.axeWindow_class.field
#define PRIVATE(w,field) (((AxeWindowWidget) w)->axeWindow.field)

#define WMShellOffset(field) XtOffsetOf(AxeWindowRec, wm.field)
#define TopLevelOffset(field) XtOffsetOf(AxeWindowRec, topLevel.field)
#define Offset(field) XtOffsetOf(AxeWindowRec, axeWindow.field)

static XtResource resources[] = {
    /* WMShell Widget Resources */
    { XtNtitle, XtCTitle, XtRString, sizeof(String),
	 WMShellOffset(title), XtRImmediate, (XtPointer) "axe"},

    /* TopLevelShell Widget Resources */
    { XtNiconName, XtCIconName, XtRString, sizeof(String),
	 TopLevelOffset(icon_name), XtRImmediate, (XtPointer) "axe"},

    /* AxeWindow Widget Resources */
    {XtNfile, XtCFile, XtRString, sizeof(String),
         Offset(file), XtRString, NULL},
    {XtNfileTitle, XtCFileTitle, XtRBoolean, sizeof(Boolean),
         Offset(file_title), XtRImmediate, (XtPointer) False},
    {XtNfileTitlePrefix, XtCFileTitlePrefix, XtRString, sizeof(String),
         Offset(file_title_prefix), XtRImmediate, (XtPointer) "axe:"},
    {XtNiconNamePrefix, XtCFileTitlePrefix, XtRString, sizeof(String),
         Offset(icon_name_prefix), XtRImmediate, (XtPointer) "axe:"},
    {XtNexitOnCloseLastWindow, XtCExitOnCloseLastWindow,
	 XtRBoolean, sizeof(Boolean),
         Offset(exit_on_close_last_window), XtRImmediate, (XtPointer) False},
};

#undef Offset

static void Initialize(), Destroy(), Resize();
static XtGeometryResult GeometryManager();
static void DeleteChild();
static void ChangeOfTitle(), SetWMHints(), CloseWindow();

static XtActionsRec actions[] = {
    "close-window",         CloseWindow,
};

static char translations[] = "<Message>WM_PROTOCOLS:close-window()";

AxeWindowClassRec axeWindowClassRec = {
  {
    /* superclass         */    (WidgetClass) &topLevelShellClassRec,
    /* class_name         */    "AxeWindow",
    /* size               */    sizeof(AxeWindowRec),
    /* Class Initializer  */    NULL,
    /* class_part_initialize*/  NULL,
    /* Class init'ed ?    */    FALSE,
    /* initialize         */    Initialize,
    /* initialize_notify  */    NULL,
    /* realize            */    XtInheritRealize,
    /* actions            */    actions,
    /* num_actions        */    XtNumber(actions),
    /* resources          */    resources,
    /* resource_count     */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/    FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    Destroy,
    /* resize             */    Resize,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook      */  NULL,
    /* set_values_almost    */  XtInheritSetValuesAlmost,  
    /* get_values_hook      */  NULL,
    /* accept_focus       */    NULL,
    /* intrinsics version */    XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table             */  translations,
    /* query_geometry       */  NULL,
    /* display_accelerator  */  NULL,
    /* extension            */  NULL
  },{
    /* geometry_manager   */    GeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child       */    XtInheritInsertChild,
    /* delete_child       */    DeleteChild,
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

WidgetClass axeWindowWidgetClass = (WidgetClass) &axeWindowClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    Widget aew;

    CLASS(number_of_windows) += 1;

    aew = XtVaCreateManagedWidget("panes", axeEditorWidgetClass, new,
				  XtNfile, PRIVATE(new,file),
				  NULL);

    if (PRIVATE(new,file_title))
    {
	XtAddCallback(aew, XtNchangeCallback, ChangeOfTitle, (XtPointer) new);
    }
}

static void
Destroy(w)
     Widget w;
{
    CLASS(number_of_windows) -= 1;

    if (CLASS(number_of_windows) == 0 && PRIVATE(w,exit_on_close_last_window))
    {
	exit(0);
    }
}

static void
Resize(w)
    Widget w;
{
    (*((TopLevelShellWidgetClass)
       (axeWindowWidgetClass-> core_class.superclass))->core_class.resize) (w);

    SetWMHints(w);
}

static XtGeometryResult
GeometryManager(w, desired, allowed)
     Widget w;
     XtWidgetGeometry *desired, *allowed;
{
    XtGeometryResult result;

    result = (*((TopLevelShellWidgetClass) 
                 (axeWindowWidgetClass->
		  core_class.superclass))->
	           composite_class.geometry_manager) (w, desired, allowed);

    if (result == XtGeometryYes)
    {
	SetWMHints(XtParent(w));
    }

    return result;
}

static void
DeleteChild(child)
     Widget child;
{
    (*((CompositeWidgetClass)
       (axeWindowWidgetClass->
	core_class.superclass))->composite_class.delete_child) (child);

    XtDestroyWidget(XtParent(child));
}

/* ARGSUSED */
static void
ChangeOfTitle(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget aww = (Widget) client_data;
    String title = (String) call_data, newTitle, iconName, icon;
    int len = strlen(PRIVATE(aww,file_title_prefix));
    int ilen = strlen(PRIVATE(aww,icon_name_prefix));

    if (len == 0)
    {
	newTitle = title;
    }
    else
    {
	len += strlen(title) + 1;
	newTitle = (String) XtMalloc(len);
	strcpy(newTitle, PRIVATE(aww,file_title_prefix));
	strcat(newTitle, title);
    }

    if ( (icon = rindex(title, '/')))
    {
	icon += 1;
    }
    else
    {
	icon = title;
    }

    if (ilen == 0)
    {
	iconName = icon;
    }
    else
    {
	ilen += strlen(icon) + 1;
	iconName = (String) XtMalloc(ilen);
	strcpy(iconName, PRIVATE(aww,icon_name_prefix));
	strcat(iconName, icon);
    }

    XtVaSetValues(aww,
		  XtNtitle, newTitle,
		  XtNiconName, iconName,
		  NULL);

    if (newTitle != title)
    {
	XtFree(newTitle);
    }

    if (iconName != icon)
    {
	XtFree(iconName);
    }
}

static void 
SetWMHints(shell)
     Widget shell;
{
    XSizeHints hints;
    long supp;
    XFontStruct *font;
    Position lm, rm, tm, bm;
    Dimension height, pheight;
    Widget sink;
    XRectangle caret;
    Dimension caretHeight;
    Atom wm_delete_window;
    WidgetList children;
    Cardinal numChildren;
    Display *display = XtDisplay(shell);
    Window window = XtWindow(shell);
    
    XtVaGetValues(shell,
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);

    XtVaGetValues(AxeEditorEdWidget(children[0]),
		  XtNfont, &font,
		  XtNheight, &height,
		  XtNleftMargin, &lm,
		  XtNrightMargin, &rm,
		  XtNtopMargin, &tm,
		  XtNbottomMargin, &bm,
                  XtNtextSink, &sink,
                  NULL);

    XawTextSinkGetCursorBounds(sink, &caret);
    caretHeight = (Dimension) caret.height;

    XtVaGetValues(children[0],
		  XtNheight, &pheight,
		  NULL);

    if (!XGetWMNormalHints(display, window, &hints, &supp))
    {
	hints.flags = 0;
    }
    hints.flags |= PResizeInc | PBaseSize;
    hints.width_inc = font->max_bounds.width;
    hints.height_inc = font->max_bounds.ascent + font->max_bounds.descent;
    hints.base_width = lm + rm;
    hints.base_height = pheight - (height - tm - bm - CARETHEIGHT);

    XSetWMNormalHints(display, window, &hints);

    wm_delete_window = XInternAtom (display, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols(display, window, &wm_delete_window, 1);
}

/* ARGSUSED */
static void
CloseWindow(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallActionProc(AxeEditorEdWidget(AxeWindowAxeEditor(widget)),
		     "close-window", event, params, *num_params);
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

Widget
AxeWindowAxeEditor(aww)
     Widget aww;
{
    if (!XtIsSubclass(aww, axeWindowWidgetClass))
    {
	return (Widget) 0;
    }
    else
    {
	return ((AxeWindowWidget) aww)->composite.children[0];
    }
}
