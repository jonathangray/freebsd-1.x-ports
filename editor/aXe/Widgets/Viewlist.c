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
#include <X11/Xaw/List.h>
#include <ViewlistP.h>

#define PRIVATE(w,field) (((ViewlistWidget) w)->viewlist.field)

#define Offset(field) XtOffsetOf(ViewlistRec, viewlist.field)

static XtResource resources[] = {
    {XtNlistWidget, XtCListWidget, XtRWidget, sizeof(Widget),
         Offset(list_widget), XtRImmediate, (XtPointer) 0},
};

#undef Offset

static void Initialize();
static XtGeometryResult GeometryManager();

ViewlistClassRec viewlistClassRec = {
  { /* core_class fields */
    /* superclass         */    (WidgetClass) &viewportClassRec,
    /* class_name         */    "Viewlist",
    /* widget_size        */    sizeof(ViewlistRec),
    /* class_initialize   */    NULL,
    /* class_part_init    */    NULL,
    /* class_inited       */    FALSE,
    /* initialize         */    Initialize,
    /* initialize_hook    */    NULL,
    /* realize            */    XtInheritRealize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    resources,
    /* num_resources      */    XtNumber(resources),
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
    /* accept_focus       */    NULL,
    /* version            */    XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    NULL,
    /* query_geometry     */    XtInheritQueryGeometry,
    /* display_accelerator*/    XtInheritDisplayAccelerator,
    /* extension          */    NULL
  },
  { /* composite_class fields */
    /* geometry_manager   */    GeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child       */    XtInheritInsertChild,
    /* delete_child       */    XtInheritDeleteChild,
    /* extension          */    NULL
  },
  { /* constraint_class fields */
    /* subresourses       */    NULL,
    /* subresource_count  */    0,
    /* constraint_size    */    sizeof(ViewportConstraintsRec),
    /* initialize         */    NULL,
    /* destroy            */    NULL,
    /* set_values         */    NULL,
    /* extension          */    NULL
  },
  { /* form_class fields */
    /* layout             */    XtInheritLayout
  },
  { /* viewport_class fields */
    /* empty              */    0
  },
  { /* viewlist_class fields */
    /* extension          */    NULL
  }
};

WidgetClass viewlistWidgetClass = (WidgetClass) &viewlistClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    PRIVATE(new,list_widget) =
	XtVaCreateManagedWidget("list", listWidgetClass, new, NULL);
}

static XtGeometryResult
GeometryManager(w, desired, allowed)
     Widget w;
     XtWidgetGeometry *desired, *allowed;
{
    ViewlistWidget vw = (ViewlistWidget) XtParent(w);
    XtWidgetGeometry request;
    XtGeometryResult result;

#define REQUESTS(flag) (desired->request_mode & flag)

    request.request_mode = desired->request_mode;
    request.width = desired->width;
    request.height = desired->height;

    if (request.width < vw->core.width)
    {
	request.width = vw->core.width;
	request.request_mode |= CWWidth;
    }

    result = XtMakeGeometryRequest((Widget) vw, &request, allowed);

    if (REQUESTS(XtCWQueryOnly))
    {
        return result;
    }

    if (result == XtGeometryYes)
    {
        if (REQUESTS(CWWidth))
        {
	    w->core.width = vw->core.width;
        }
    
        if (REQUESTS(CWHeight) && request.height < vw->core.height)
        {
	    w->core.height = vw->core.height;
        }
    }

    return result;

#undef REQUESTS

/*
    if (desired->width >= vw->core.width)
    {
	w->core.width = desired->width;
	w->core.height = desired->height;
	if (desired->height < vw->core.height)
	{
	    w->core.height = vw->core.height;
	}

printf("Returning Yes\n");
	return XtGeometryYes;
    }
    else
    {
	allowed->request_mode = desired->request_mode;
	allowed->width = vw->core.width;
	allowed->height = desired->height;

printf("Returning Almost\n");
	return XtGeometryAlmost;
    }
*/
}
