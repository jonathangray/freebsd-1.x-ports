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

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <AxeEditor.h>
#include <AxeSmeBSB.h>

#include "AxeSimMenuP.h"

#define PRIVATE(w,field) (((AxeSimpleMenuWidget) w)->axeSimpleMenu.field)

static void Initialize();
static void UpdateInfo();

static XtActionsRec actions [] = {
    "update-info",      UpdateInfo,
};

AxeSimpleMenuClassRec axeSimpleMenuClassRec = {
  {
    /* superclass         */    (WidgetClass) &simpleMenuClassRec,
    /* class_name         */    "AxeSimpleMenu",
    /* size               */    sizeof(AxeSimpleMenuRec),
    /* class_initialize   */    NULL,
    /* class_part_initialize*/  NULL,
    /* Class init'ed      */    FALSE,
    /* initialize         */    Initialize,
    /* initialize_hook    */    NULL,
    /* realize            */    XtInheritRealize,
    /* actions            */    actions,
    /* num_actions        */    XtNumber(actions),
    /* resources          */    NULL,
    /* resource_count     */    0,
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    TRUE, 
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/    TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    XtInheritResize,
    /* expose             */    XtInheritExpose,
    /* set_values         */    NULL,
    /* set_values_hook    */    NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,  
    /* get_values_hook    */    NULL,
    /* accept_focus       */    XtInheritAcceptFocus,
    /* intrinsics version */    XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table           */    XtInheritTranslations,
    /* query_geometry     */    XtInheritQueryGeometry,
    /* display_accelerator*/    XtInheritDisplayAccelerator,
    /* extension          */    NULL
  },{
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child       */    XtInheritInsertChild,
    /* delete_child       */    XtInheritDeleteChild,
    /* extension          */    NULL
  },{
    /* Shell extension    */    NULL
  },{
    /* Override extension */    NULL
  },{
    /* Simple Menu extension*/  NULL
  },{
    /* Simple Axe Menu ext  */  NULL
  }
};

WidgetClass axeSimpleMenuWidgetClass = (WidgetClass) &axeSimpleMenuClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    ((AxeSimpleMenuWidget) new)->axeSimpleMenu.popperUpper = (Widget) 0;
}

/*************************************************************
 *
 *                        update-info
 *
 *************************************************************/

/* ARGSUSED */
static void
UpdateInfo(widget, event, params, num_params)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    static Widget previousEntry = 0;
    Widget entry;
    String help;

    if (*num_params && strcmp(params[0], "Reset") == 0)
    {
        previousEntry = 0;
        return;
    }

    if ((entry =
	     XawSimpleMenuGetActiveEntry(widget)) && entry != previousEntry)
    {
        previousEntry = entry;

	XtVaGetValues(entry,
		      XtNhelp, &help,
		      NULL);

	AxeEditorUpdateInfoBar(PRIVATE(widget,popperUpper), help);
    }
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

void
AxeSimpleMenuStorePopperUpper(menu, popper)
     Widget menu, popper;
{
    if (XtIsSubclass(menu, axeSimpleMenuWidgetClass))
    {
	PRIVATE(menu,popperUpper) = popper;
    }
}
    
Widget 
AxeSimpleMenuFetchPopperUpper(menu)
     Widget menu;
{
    Widget popperUpper;

    if (XtIsSubclass(menu, axeSimpleMenuWidgetClass))
    {
	popperUpper = PRIVATE(menu,popperUpper);
	/* PRIVATE(menu,popperUpper) = (Widget) 0; */
    }
    else
    {
	popperUpper = (Widget) 0;
    }
    return popperUpper;
}

#undef PRIVATE
