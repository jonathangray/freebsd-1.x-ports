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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>

#include <CtrlCodeSelP.h>

#define CLASS(field) controlCodeSelectorClassRec.control_code_selector_class.field
#define PRIVATE(w,field) (((ControlCodeSelectorWidget) w)->control.field)

#define TableOffset(field) XtOffsetOf(ControlCodeSelectorRec, table.field)
#define Offset(field) XtOffsetOf(ControlCodeSelectorRec, control.field)

static char layout[] = "list 0 0 12; \
                        code 0 1 2 whH;  enter 2 1 5 whH;  cancel 7 1 5 whH;";

static XtResource resources[] = {
    /* Table Widget Resources */
    { XtNlayout, XtCLayout, XtRXpTableLoc, sizeof(XpTableLoc),
	  TableOffset(default_layout), XtRString, (XtPointer) layout },
    { XtNmarginWidth, XtCMargins, XtRInt, sizeof(int),
	  TableOffset(margin_width), XtRImmediate, (XtPointer) 5 },
    { XtNmarginHeight, XtCMargins, XtRInt, sizeof(int),
	  TableOffset(margin_height), XtRImmediate, (XtPointer) 5 },
    { XtNcolumnSpacing, XtCSpacing, XtRInt, sizeof(int),
	  TableOffset(col_spacing), XtRImmediate, (XtPointer) 5 },
    { XtNrowSpacing, XtCSpacing, XtRInt, sizeof(int),
	  TableOffset(row_spacing), XtRImmediate, (XtPointer) 5 },

    /* ControlCodeSelector Widget Resources */
    {XtNbase, XtCBase, XtRInt, sizeof(int),
         Offset(base), XtRImmediate, (XtPointer) 8},
    {XtNenterCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
         Offset(enter_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNcancelCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
         Offset(cancel_callbacks), XtRCallback, (XtPointer) NULL},
};

#undef Offset

static char *ctrlCodes[] = {"    NUL",
                            "A - SOH", "B - STX",
			    "C - ETX", "D - EOT",
			    "E - ENQ", "F - ACK",
			    "G - BEL", "H - BS ",
			    "I - HT ", "J - NL ",
			    "K - VT ", "L - NP ",
			    "M - CR ", "N - SO ",
			    "O - SI ", "P - DLE",
			    "Q - DC1", "R - DC2",
			    "S - DC3", "T - DC4",
			    "U - NAK", "V - SYN",
			    "W - ETB", "X - CAN",
			    "Y - EM ", "Z - SUB",
			    "    ESC", "    FS ",
			    "    GS ", "    RS ",
			    "    US ",
};

static char listTranslations[] = "<Btn2Up>: Set() Notify() Forward()";

static void ClassInitialize(), Initialize();
static Boolean SetValues();
static void LabelCodeWidget(), Selected(), Enter(), Cancel(), Forward();

static XtActionsRec actions[] = {
    "Forward",   Forward,
};

ControlCodeSelectorClassRec controlCodeSelectorClassRec = {
  {
    /* superclass             */     (WidgetClass) &xpTableClassRec,  
    /* class_name             */     "ControlCodeSelector",                    
    /* size                   */     sizeof(ControlCodeSelectorRec),           
    /* class_initialize       */     ClassInitialize,
    /* class_part_initialize  */     NULL,                               
    /* class_inited           */     FALSE,                              
    /* initialize             */     Initialize,
    /* initialize_hook        */     NULL,                               
    /* realize                */     XtInheritRealize,                   
    /* actions                */     actions,
    /* num_actions            */     XtNumber(actions),
    /* resources              */     resources,
    /* resource_count         */     XtNumber(resources),
    /* xrm_class              */     NULLQUARK,                          
    /* compress_motion        */     FALSE,                              
    /* compress_exposure      */     TRUE,                               
    /* compress_enterleave    */     TRUE,                               
    /* visible_interest       */     FALSE,                              
    /* destroy                */     NULL,                               
    /* resize                 */     XtInheritResize,                    
    /* expose                 */     XtInheritExpose,                    
    /* set_values             */     SetValues,
    /* set_values_hook        */     NULL,                               
    /* set_values_almost      */     XtInheritSetValuesAlmost,           
    /* get_values_hook        */     NULL,                               
    /* accept_focus           */     NULL,                               
    /* version                */     XtVersion,                          
    /* callback_private       */     NULL,                               
    /* tm_table               */     XtInheritTranslations,
    /* query_geometry         */     XtInheritQueryGeometry,             
    /* display_accelerator    */     XtInheritDisplayAccelerator,        
    /* extension              */     NULL                                
  },/* CoreClass fields initialization */
  {
    /* composite class fields */
    /* geometry_manager   */   XtInheritGeometryManager,
    /* change_managed     */   XtInheritChangeManaged,
    /* insert_child       */   XtInheritInsertChild,
    /* delete_child       */   XtInheritDeleteChild,
    /* extension          */   NULL
  },/* CompositeClass fields initialization */
  {
    /* extension              */     NULL,
  },/* TableClass fields initialization */
  {
    /* extension              */     NULL,
  },/* ControlCodeSelectorClass fields initialization */
};

WidgetClass controlCodeSelectorWidgetClass = (WidgetClass) &controlCodeSelectorClassRec;

static void
ClassInitialize()
{
    CLASS(listTranslations) = XtParseTranslationTable(listTranslations);
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    Widget list, enter, cancel;

    list = PRIVATE(new,list_widget)
	= XtVaCreateManagedWidget("list", listWidgetClass, new,
				  XtNlist, ctrlCodes,
				  XtNnumberStrings, XtNumber(ctrlCodes),
				  NULL);
    XtOverrideTranslations(list, CLASS(listTranslations));
    XtAddCallback(list, XtNcallback, Selected, (XtPointer) new);

    PRIVATE(new,code_widget)
	= XtVaCreateManagedWidget("code", labelWidgetClass, new, NULL);
    LabelCodeWidget(new);

    enter = XtVaCreateManagedWidget("enter", commandWidgetClass, new, NULL);
    XtAddCallback(enter, XtNcallback, Enter, (XtPointer) 0);

    cancel = XtVaCreateManagedWidget("cancel", commandWidgetClass, new, NULL);
    XtAddCallback(cancel, XtNcallback, Cancel, (XtPointer) 0);
}

/* ARGSUSED */
static Boolean
SetValues(old, request, new, args, num_args)
     Widget old, request, new;
     ArgList args;
     Cardinal *num_args;
{
    ControlCodeSelectorWidget oldccsw = (ControlCodeSelectorWidget) old;
    ControlCodeSelectorWidget newccsw = (ControlCodeSelectorWidget) new;
    XawListReturnStruct *list;
    int base;

#define NE(field) (oldccsw->field != newccsw->field)

    if (NE(control.base))
    {
	base = PRIVATE(new,base);
	if (base != 8 && base != 10 && base != 16)
	{
	    PRIVATE(new,base) = 10;
	}

	list = XawListShowCurrent(PRIVATE(new,list_widget));
	if (list->list_index != XAW_LIST_NONE)
	{
	    Selected((XtPointer) 0, (XtPointer) new, (XtPointer) list);
	}
	else
	{
	    LabelCodeWidget(new);
	}
	XtFree((char *) list);
    }
    
#undef NE

    return False;
}

static void
LabelCodeWidget(ctrl)
     ControlCodeSelectorWidget ctrl;
{
    XtVaSetValues(PRIVATE(ctrl,code_widget),
		  XtNlabel, "   ",
		  NULL);
}

static void
Feed(widget)
     Widget widget;
{
    Widget ctrl = XtParent(widget);
    XawListReturnStruct *selection
	= XawListShowCurrent(PRIVATE(ctrl,list_widget));

    if (selection->list_index != XAW_LIST_NONE)
    {
	XtCallCallbackList(ctrl, PRIVATE(ctrl,enter_callbacks),
			                    (XtPointer) selection->list_index);
    }

    XtFree((char *) selection);
}    

/* ARGSUSED */
static void
Forward(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Feed(widget);
}

/* ARGSUSED */
static void
Selected(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget ctrl = (Widget) client_data;
    XawListReturnStruct *selection = (XawListReturnStruct *) call_data;
    char based[4];

    switch (PRIVATE(ctrl,base))
    {
    case 8:sprintf(based, "%03o", selection->list_index);
	break;
    case 10:
	sprintf(based, "%d", selection->list_index);
	break;
    case 16:
	sprintf(based, "x%02x", selection->list_index);
	break;
    }

    XtVaSetValues(PRIVATE(ctrl,code_widget),
		  XtNlabel, based,
		  NULL);
}

/* ARGSUSED */
static void
Enter(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Feed(widget);
}

/* ARGSUSED */
static void
Cancel(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget ctrl = XtParent(widget);

    XtCallCallbackList(ctrl, PRIVATE(ctrl,cancel_callbacks), (XtPointer) 0);
}
/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

void
ControlCodeSelectorClear(ccsw)
     Widget ccsw;
{
    if (!XtIsSubclass(ccsw, controlCodeSelectorWidgetClass))
    {
	return;
    }

    XawListUnhighlight(PRIVATE(ccsw,list_widget));

    LabelCodeWidget(ccsw);
}

#undef PRIVATE
#undef CLASS
