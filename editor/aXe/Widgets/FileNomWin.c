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
#include <X11/Xaw/List.h>
#include <FileNom.h>

#include "FileNomWinP.h"

#define Offset(field) XtOffsetOf(FileNominatorWindowRec, fileNominatorWindow.field)

static XtResource resources[] = {
    {XtNinitialDirectory, XtCInitialDirectory, XtRString, sizeof(String),
         Offset(initial_directory), XtRString, (XtPointer) NULL},
};

#undef Offset

#define CLASS(field) fileNominatorWindowClassRec.fileNominatorWindow_class.field
#define PRIVATE(w,field) (((FileNominatorWindowWidget) w)->fileNominatorWindow.field)

static char nominatorTranslations[] = 
    "<FocusIn>:scroll-on-movement(True) \n\
     <FocusOut>:scroll-on-movement(False)";

static void ClassInitialize(), Initialize(), Resize();
static XtGeometryResult GeometryManager();
static void SetWMHints();

FileNominatorWindowClassRec fileNominatorWindowClassRec = {
  {
    /* superclass         */    (WidgetClass) &transientShellClassRec,
    /* class_name         */    "FileNominatorWindow",
    /* size               */    sizeof(FileNominatorWindowRec),
    /* Class Initializer  */    ClassInitialize,
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
    /* resize             */    Resize,
    /* expose             */    NULL,
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
    /* geometry_manager   */    GeometryManager,
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

WidgetClass fileNominatorWindowWidgetClass = (WidgetClass) &fileNominatorWindowClassRec;

static void
ClassInitialize()
{
     CLASS(nominatorTranslations) =
         XtParseTranslationTable(nominatorTranslations);
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    char *off = "False";
    Widget fileNom = PRIVATE(new,nominator) = 
       XtVaCreateManagedWidget("filenom", fileNominatorWidgetClass, new,
			       XtNinitialDirectory,
			                        PRIVATE(new,initial_directory),
			       NULL);
    XtOverrideTranslations(fileNom, CLASS(nominatorTranslations));

    XtCallActionProc(fileNom, "scroll-on-movement", (XEvent *) 0, &off, 1);
}

static void
Resize(w)
    Widget w;
{
    (*((TransientShellWidgetClass)
       (fileNominatorWindowWidgetClass-> core_class.superclass))->core_class.resize) (w);

    SetWMHints(w);
}

static XtGeometryResult
GeometryManager(w, desired, allowed)
     Widget w;
     XtWidgetGeometry *desired, *allowed;
{
    XtGeometryResult result;

    result = (*((TransientShellWidgetClass) 
                 (fileNominatorWindowWidgetClass->
		  core_class.superclass))->
	           composite_class.geometry_manager) (w, desired, allowed);

    if (result == XtGeometryYes)
    {
	SetWMHints(XtParent(w));
    }

    return result;
}

static void 
SetWMHints(fnww)
     Widget fnww;
{
    Widget viewport, vscroller;
    XSizeHints hints;
    long supp;
    XFontStruct *font;
    Dimension vpWidth, vpHeight, inWidth, inHeight, rowSpacing, thickness;
    Atom wm_delete_window;
    Window window = XtWindow(fnww);
    Display *display = XtDisplay(fnww);
    
    if (!XtIsRealized(fnww))
    {
	return;
    }

    XtVaGetValues(FileNominatorListWidget(PRIVATE(fnww,nominator)),
                  XtNfont, &font,
		  XtNrowSpacing, &rowSpacing,
		  XtNinternalWidth, &inWidth,
		  XtNinternalHeight, &inHeight,
                  NULL);

    viewport = FileNominatorViewportWidget(PRIVATE(fnww,nominator));
    XtVaGetValues(viewport,
		  XtNwidth, &vpWidth,
		  XtNheight, &vpHeight,
		  NULL);

    if (!XGetWMNormalHints(display, window, &hints, &supp))
    {
        hints.flags = 0;
    }
    hints.flags |= PResizeInc | PBaseSize;
    hints.height_inc = font->max_bounds.ascent
	                               + font->max_bounds.descent + rowSpacing;
    hints.base_height = fnww->core.height
	                              - (vpHeight - 2 * inHeight) - rowSpacing;

    /* Monospaaced or character cell font? */
    if (font->min_bounds.width == font->max_bounds.width)
    {
	if ((vscroller = XtNameToWidget(viewport, "vertical")))
	{
	    XtVaGetValues(vscroller, XtNthickness, &thickness, NULL);
	    vpWidth -= thickness;
	}
	hints.width_inc = font->min_bounds.width;
	hints.base_width = fnww->core.width - (vpWidth - 2 * inWidth);
    }

    XSetWMNormalHints(display, window, &hints);

    wm_delete_window = XInternAtom (display, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols(display, window, &wm_delete_window, 1);
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

Widget
FileNominatorWindowFileNominatorWidget(fnww)
     Widget fnww;
{
    if (XtIsSubclass(fnww, fileNominatorWindowWidgetClass))
    {
        return PRIVATE(fnww,nominator);
    }
    else
    {
        return (Widget) 0;
    }
}
