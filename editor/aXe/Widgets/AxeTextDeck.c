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
#include <AxeIntrinsic.h>
#include <AxeEditor.h>
#include <AxeText.h>
#include <FileNom.h>
#include <sys/param.h>
#include <stdio.h>

#include <AxeTextDeckP.h>

#define CLASS(field) axeTextDeckClassRec.axeTextDeck_class.field
#define PRIVATE(w,field) (((AxeTextDeckWidget) w)->axeTextDeck.field)

#define Offset(field) XtOffsetOf(AxeTextDeckRec, axeTextDeck.field)

static XtResource resources[] = {
    {XtNfile, XtCFile, XtRString, sizeof(String),
         Offset(file), XtRString, NULL},
    {XtNchangeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList), 
         Offset(change_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct*),
	 Offset(font), XtRFontStruct, (XtPointer) NULL},
};

#undef Offset

static void Initialize();
static void Realize(), Resize(), InsertChild(), DeleteChild();
static Boolean SetValues();
static XtGeometryResult GeometryManager();

static void DeckChange(), Message(), Where(), Size();

AxeTextDeckClassRec axeTextDeckClassRec = {
{
/* core_class fields      */
    /* superclass         */    (WidgetClass) &compositeClassRec,
    /* class_name         */    "AxeTextDeck",
    /* widget_size        */    sizeof(AxeTextDeckRec),
    /* class_initialize   */    NULL,
    /* class_part_init    */	NULL,
    /* class_inited       */	FALSE,
    /* initialize         */    Initialize,
    /* initialize_hook    */	NULL,
    /* realize            */    Realize,
    /* actions            */    NULL,
    /* num_actions	  */	0,
    /* resources          */    resources,
    /* num_resources      */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion	  */	TRUE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    Resize,
    /* expose             */    XtInheritExpose,
    /* set_values         */    SetValues,
    /* set_values_hook    */	NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */	NULL,
    /* accept_focus       */    NULL,
    /* version            */	XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    NULL,
    /* query_geometry     */	XtInheritQueryGeometry,
    /* display_accelerator*/	XtInheritDisplayAccelerator,
    /* extension          */	NULL
  },{
/* composite_class fields */
    /* geometry_manager   */    GeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child	  */	InsertChild,
    /* delete_child	  */	DeleteChild,
    /* extension          */	NULL
  },{
/* AxeTextDeck class fields */
    /* empty		  */	0,
  }
};

WidgetClass axeTextDeckWidgetClass = (WidgetClass) &axeTextDeckClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    String file = PRIVATE(new,file), fp, finalfp, fq, theFile, expandedName;
    char filename[MAXPATHLEN];
    extern String AxeEditorExpandName();

    /*
     * The file resource may be NULL or contain multiple files.
     * Do some cheap and nasty parsing.
     */
    fp = file;
    if (fp && *fp == '\0')
    {
	fp = NULL;
    }

    if (fp)
    {
	finalfp = fp + strlen(fp);
    }
    else
    {
	finalfp = filename;     /* Anything that's not NULL */
    }
    while (fp != finalfp)
    {
	if (fp)
	{
	    while (*fp == ' ' || *fp == '\n' || *fp == '\t')
		++fp;
	
	    if (!*fp)
		break;

	    if  ( (fq = index(fp, ' ')) || (fq = index(fp, '\n'))
		                                     || (fq = index(fp, '\t')))
	    {
		strncpy(filename, fp, fq - fp);
		filename[fq - fp] = '\0';
		theFile = filename;
		fp = fq + 1;
	    }
	    else
	    {
		theFile = fp;
		fp = file + strlen(file);
	    }
	}
	else
	{
	    theFile = fp;
	    fp = finalfp;
	}

	if (theFile)
	{
	    expandedName = AxeEditorExpandName(theFile);
	}
	else
	{
	    expandedName = theFile;
	}
	
	XtVaCreateManagedWidget("ed", axeTextWidgetClass, new,
				XtNstring, expandedName,
				NULL);
	
	if (expandedName)
	{
	    XtFree(expandedName);
	}
    }
}

static void 
Realize(w, valueMask, attributes)
Widget w;
Mask *valueMask;
XSetWindowAttributes *attributes;
{
    WidgetList children;
    Cardinal numChildren, child;

    /*
     * Realizing in reverse order from normal means that the last value
     * put in PRIVATE(w,ed) by InsertChild is correct when the deck is
     * created with multiple children; also, the order is consistent
     * with the stacking of windows, i.e. last named file on top.
     */

    (*axeTextDeckWidgetClass->core_class.superclass->core_class.realize)
        (w, valueMask, attributes);

    XtVaGetValues(w,
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);

    for (child = 0;  child < numChildren;  ++child)
    {
	XtRealizeWidget(children[child]);
    }

    XMapSubwindows(XtDisplay(w), XtWindow(w));
}

static void
Resize(w)
     AxeTextDeckWidget w;
{
    int child;
    Widget childWidget;

    for (child = 0;  child < w->composite.num_children;  ++child)
    {
	childWidget = ((CompositeWidget) w)->composite.children[child]; 
	if (childWidget == PRIVATE(w,ed))
	{
	    XtConfigureWidget(childWidget,
			      (Position) -(childWidget->core.border_width),
			      (Position) -(childWidget->core.border_width),
			      w->core.width, w->core.height,
			      childWidget->core.border_width);
	}
    }
}

/* ARGSUSED */
static Boolean
SetValues(old, request, new, args, num_args)
     Widget old, request, new;
     ArgList args;
     Cardinal *num_args;
{
    AxeTextDeckWidget oatdw = (AxeTextDeckWidget) old;
    AxeTextDeckWidget atdw = (AxeTextDeckWidget) new;
    int child;

    if (atdw->axeTextDeck.font != oatdw->axeTextDeck.font)
    {
	for (child = 0;  child < atdw->composite.num_children;  ++child)
	{
	    XtVaSetValues(atdw->composite.children[child],
			  XtNfont, atdw->axeTextDeck.font,
			  NULL);
	}
    }

    return False;
}

static void
InsertChild(w)
     Widget w;
{
    AxeTextDeckWidget atdw = (AxeTextDeckWidget) XtParent(w);
    XFontStruct *font;

    (*((CompositeWidgetClass)
       (axeTextDeckWidgetClass->
	core_class.superclass))->composite_class.insert_child) (w);

    /* The size of the AxeTextDeck containing the child is determined from
     * the size of the child in the case of the first child, so the core
     * variables obviously aren't available.
     */
    if (atdw->composite.num_children ==1)
    {
	XtVaGetValues(w,
		      XtNfont, &font,
		      NULL);
	PRIVATE(atdw,font) = font;

	atdw->core.width = (atdw->composite.children[0])->core.width;
	atdw->core.height = (atdw->composite.children[0])->core.height;
	
	XtMoveWidget(w,
		     (Position) -w->core.border_width,
		     (Position) -w->core.border_width);
    }
    else {
	XtConfigureWidget(w,
			  (Position) -w->core.border_width,
			  (Position) -w->core.border_width,
			  XtParent(w)->core.width, XtParent(w)->core.height,
			  w->core.border_width);
    }

    PRIVATE(atdw,ed) = w;

    XtAddCallback(w, XtNchangeCallback, DeckChange, (XtPointer) 0);
    XtAddCallback(w, XtNmessageCallback, Message, (XtPointer) 0);
    XtAddCallback(w, XtNwhereCallback, Where, (XtPointer) 0);
    XtAddCallback(w, XtNsizeCallback, Size, (XtPointer) 0);
    XtCallCallbackList((Widget) atdw, PRIVATE(atdw,change_callbacks),
		                                                (XtPointer) 0);
}

static void
DeleteChild(child)
     Widget child;
{
    AxeTextDeckWidget atdw = (AxeTextDeckWidget) XtParent(child);

    (*((CompositeWidgetClass)
       (axeTextDeckWidgetClass->
        core_class.superclass))->composite_class.delete_child) (child);

    if (atdw->composite.num_children == 0)
    {
	XtDestroyWidget(XtParent(child));
    }
    else
    {
	AxeTextDeckRaise(atdw, atdw->composite.children[0]);
    }
}
    
/* ARGSUSED */
static XtGeometryResult
GeometryManager(w, desired, allowed)
     Widget w;
     XtWidgetGeometry *desired, *allowed;
{
    AxeTextDeckWidget atdw = (AxeTextDeckWidget) XtParent(w);
    XtWidgetGeometry request;
    XtGeometryResult result;

#define REQUESTS(flag) (desired->request_mode & flag)

    request.request_mode = desired->request_mode;
    request.width = desired->width;
    request.height = desired->height;

    result = XtMakeGeometryRequest((Widget) atdw, &request,
			      (XtWidgetGeometry *) 0);

    if (REQUESTS(XtCWQueryOnly))
    {
	return result;
    }
	
    if (result == XtGeometryYes)
    {
	if (REQUESTS(CWWidth))
	{
	    w->core.width = desired->width;
	}
    
	if (REQUESTS(CWHeight))
	{
	    w->core.height = desired->height;
	}

	return XtGeometryYes;
    }
    else
    {
	return XtGeometryNo;
    }

#undef REQUESTS
}

/* ARGSUSED */
static void
DeckChange(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    XtCallCallbackList(XtParent(w), PRIVATE(XtParent(w),change_callbacks),
		                                                (XtPointer) 0);
}

/* ARGSUSED */
static void
Message(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    int code = (int) call_data;
    Widget axeEditor = AxeEditorParentOf(w);
    
    switch (code)
    {
    case AxeTextsaveSuccess:
	AxeEditorUpdateInfoBar(axeEditor, "File written");
	break;
    case AxeTextnotSaved:
	AxeEditorUpdateInfoBar(axeEditor, "No changes to save");
	break;
    case AxeTextsaveFailure:
	AxeEditorUpdateInfoBar(axeEditor, "Save failed");
	break;
    case AxeTextinvalidFilename:
	AxeEditorUpdateInfoBar(axeEditor, "Invalid filename component in path");
	break;
    case AxeTextinvalidDirectory:
	AxeEditorUpdateInfoBar(axeEditor, "Invalid directory component in path");
	break;
    case AxeTextunreadableFile:
	AxeEditorUpdateInfoBar(axeEditor, "File unreadable");
	break;
    case AxeTextinvalidControlCode:
	AxeEditorUpdateInfoBar(axeEditor,
			       "Control code value is greater than 255 decimal");
	break;
    case AxeTextreloadFailure:
	AxeEditorUpdateInfoBar(axeEditor, "Reload failed");
	break;
    case AxeTextnoUndo:
	AxeEditorUpdateInfoBar(axeEditor, "Nothing to undo");
	break;
    default:
	AxeEditorUpdateInfoBar(axeEditor, "Unknown error reported");
	break;
    }

    if (code > AxeTexterrorCodes)
    {
	XBell(XtDisplay(w), 100);
    }
}

/*ARGSUSED*/
static void 
Where(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    AxeiiTextWhereStruct *where = (AxeiiTextWhereStruct *) call_data;
    char buf[BUFSIZ];

    sprintf(buf, "Line %d  Position %d", where->line, where->position);
    AxeEditorUpdateInfoBar(AxeEditorParentOf(w), buf);
}

/*ARGSUSED*/
static void 
Size(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    AxeiiTextSizeStruct *size = (AxeiiTextSizeStruct *) call_data;
    char buf[BUFSIZ];

    sprintf(buf, "The text buffer is %d x %d", size->columns, size->rows);
    AxeEditorUpdateInfoBar(AxeEditorParentOf(w), buf);
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

Widget
AxeTextDeckTop(atdw)
     Widget atdw;
{
    if (XtIsSubclass(atdw, axeTextDeckWidgetClass))
    {
	return PRIVATE(atdw,ed);
    }

    return (Widget) 0;
}

void
AxeTextDeckRaise(atdw, w)
     Widget atdw, w;
{
    int rows, cols;
    Dimension width, height;

    if (!XtIsSubclass(atdw, axeTextDeckWidgetClass))
    {
	return;
    }

    /*
     * The window being raised should be the same size as the one on top
     * Resetting the deck size should cope with scrollbar differences
     */
    XtVaGetValues(PRIVATE(atdw,ed), XtNrows, &rows, XtNcolumns, &cols, NULL);
    XtVaSetValues(w, XtNrows, rows, XtNcolumns, cols, NULL);
    AxeiiTextRowsColumnsToWidthHeight(rows, cols, w, &width, &height),

    XRaiseWindow(XtDisplay(w), XtWindow(w));
    PRIVATE(atdw,ed) = w;

    XtVaSetValues(atdw, XtNwidth, width, XtNheight, height, NULL);

    XtCallCallbackList(atdw, PRIVATE(atdw,change_callbacks), (XtPointer) 0);
}

AxeTextDeckList
AxeTextDeckListOf(atdw)
     Widget atdw;
{
    WidgetList children, copyChildren;
    Cardinal numChildren;
    int child;
    AxeTextDeckList list;
    String assocFile;

    if (!XtIsSubclass(atdw, axeTextDeckWidgetClass))
    {
	return (AxeTextDeckList) 0;
    }

    XtVaGetValues(atdw,
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);
    
    copyChildren = (WidgetList) XtMalloc(numChildren * sizeof(Widget));
    for (child = 0;  child < numChildren;  ++child)
    {
	copyChildren[child] = children[child];
    }

    list = (AxeTextDeckList)
	          XtMalloc((numChildren + 1) * sizeof(AxeTextDeckListItem));

    for (child = 0;  child < numChildren;  ++child)
    {
	list[child].widget = copyChildren[child];
	assocFile = AxeTextGetAssociatedFile(copyChildren[child]);
	list[child].file = assocFile ? XtNewString(assocFile)
	                             : XtNewString(noname);
    }
    list[numChildren].widget = (Widget) 0;
    list[numChildren].file = (String) 0;

    XtFree((char *) copyChildren);

    return list;
}

Boolean
AxeTextDeckIterate(atdw, iteration)
     Widget atdw;
     AxeIterationType iteration;
{
    WidgetList children;
    Cardinal numChildren;
    int child, count = 0;

    if (!XtIsSubclass(atdw, axeTextDeckWidgetClass))
    {
	return False;
    }

    XtVaGetValues(atdw,
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);
    
    for (child = 0;  child < numChildren;  ++child)
    {
	switch (iteration)
	{
	case AxeIsModified:
	    if (AxeTextIsModified(children[child]))
	    {
		++count;
	    }
	    break;
	case AxeSafeClose:
	    if (!AxeTextIsModified(children[child]))
	    {
		XtDestroyWidget(children[child]);
		++count;
	    }
	    break;
	case AxeSave:
	    if (AxeTextIsModified(children[child]))
	    {
		if (AxeTextSaveFile(children[child]))
		{
		    ++count;
		}
	    }
	    else
	    {
		++count;
	    }
	    break;
	default:
	    ;
	}
    }

    return (iteration == AxeIsModified) ? count > 0 : count == numChildren;
}

void
AxeTextDeckCloseTop(atdw)
     Widget atdw;
{
    if (!XtIsSubclass(atdw, axeTextDeckWidgetClass))
    {
	return;
    }

    XtDestroyWidget(PRIVATE(atdw,ed));
}

void
AxeTextDeckSetAssociatedDirectory(atdw, directory)
     Widget atdw;
     String directory;
{
    WidgetList children;
    Cardinal numChildren;
    int child;

    if (!XtIsSubclass(atdw, axeTextDeckWidgetClass))
    {
	return;
    }

    XtVaGetValues(atdw,
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);
    
    for (child = 0;  child < numChildren;  ++child)
    {
	XtVaSetValues(children[child],
		      XtNassociatedDirectory, directory,
		      NULL);
    }
}
    

#undef PRIVATE
#undef CLASS
