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
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>

#include <NumericPadP.h>

#define OCT  8
#define DEC 10
#define HEX 16

#define CLASS(field) numericPadClassRec.pad_class.field
#define PRIVATE(w,field) (((NumericPadWidget) w)->pad.field)

#define TableOffset(field) XtOffsetOf(NumericPadRec, table.field)
#define Offset(field) XtOffsetOf(NumericPadRec, pad.field)

static char layout[] = "oct 0 0 4; dec 4 0 4; hex 8 0 4; \
                        c 0 1 3; d 3 1 3;  e 6 1 3;  f 9 1 3; \
                        a 0 2 3; 8 3 2 3;  9 6 2 3;  b 9 2 3; \
                        4 0 3 3; 5 3 3 3;  6 6 3 3;  7 9 3 3; \
                        0 0 4 3; 1 3 4 3;  2 6 4 3;  3 9 4 3; \
                                   text 0 5 12; \
                           enter 0 6 6;  cancel 6 6 6;";

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

    /* NumericPad Widget Resources */
    {XtNbase, XtCBase, XtRInt, sizeof(int),
         Offset(base), XtRImmediate, (XtPointer) DEC},
    {XtNenterCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
         Offset(enter_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNcancelCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
         Offset(cancel_callbacks), XtRCallback, (XtPointer) NULL},
};

#undef Offset

static char digits[] = "0123456789abcdef";

static char textTranslations[] = "<Key>Return: Forward() \n\
                                  <Key>KP_Enter: Forward()";
static void ClassInitialize(), Initialize();
static Boolean SetValues();
static void SetSensitivity(), ModeChange(), Type(), Enter(), Cancel(), Forward();

static XtActionsRec actions[] = {
    "Forward",   Forward,
};

NumericPadClassRec numericPadClassRec = {
  {
    /* superclass             */     (WidgetClass) &xpTableClassRec,  
    /* class_name             */     "NumericPad",                    
    /* size                   */     sizeof(NumericPadRec),           
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
  },/* NumericPadClass fields initialization */
};

WidgetClass numericPadWidgetClass = (WidgetClass) &numericPadClassRec;

static void
ClassInitialize()
{
     CLASS(textTranslations) = XtParseTranslationTable(textTranslations);
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    Widget dec, oct, hex, button, text, enter, cancel;
    int digit;
    char label[2];

    dec = PRIVATE(new,dec_widget) =
	XtVaCreateManagedWidget("dec", toggleWidgetClass, new,
				XtNstate, PRIVATE(new,base) == DEC,
				NULL);
    XtAddCallback(dec, XtNcallback, ModeChange, (XtPointer) new);
    XtVaSetValues(dec, XtNradioData, dec, NULL);

    oct = PRIVATE(new,oct_widget) =
	XtVaCreateManagedWidget("oct", toggleWidgetClass, new,
				XtNstate, PRIVATE(new,base) == OCT,
				XtNradioGroup, dec,
				NULL);
    XtAddCallback(oct, XtNcallback, ModeChange, (XtPointer) new);
    XtVaSetValues(oct, XtNradioData, oct, NULL);

    hex = PRIVATE(new,hex_widget) =
	XtVaCreateManagedWidget("hex", toggleWidgetClass, new,
				XtNstate, PRIVATE(new,base) == HEX,
				XtNradioGroup, dec,
				NULL);
    XtAddCallback(hex, XtNcallback, ModeChange, (XtPointer) new);
    XtVaSetValues(hex, XtNradioData, hex, NULL);

    for (digit = 0;  digit < 16;  ++digit)
    {
	sprintf(label, "%c", digits[digit]);
	button
	    = XtVaCreateManagedWidget(label, commandWidgetClass, new,
				      XtNsensitive, digit < 10 ? True : False,
				      NULL);
	XtAddCallback(button, XtNcallback, Type,
		                            (XtPointer) ((int) digits[digit]));
    }
    SetSensitivity(new);
    
    text = PRIVATE(new,text_widget)
	= XtVaCreateManagedWidget("text", asciiTextWidgetClass, new,
				  XtNeditType, XawtextEdit,
				  NULL);
    XtOverrideTranslations(text, CLASS(textTranslations));

    enter = XtVaCreateManagedWidget("enter", commandWidgetClass, new, NULL);
    XtAddCallback(enter, XtNcallback, Enter, (XtPointer) text);

    cancel = XtVaCreateManagedWidget("cancel", commandWidgetClass, new, NULL);
    XtAddCallback(cancel, XtNcallback, Cancel, NULL);

    XtSetKeyboardFocus(new, text);
}

/* ARGSUSED */
static Boolean
SetValues(old, request, new, args, num_args)
     Widget old, request, new;
     ArgList args;
     Cardinal *num_args;
{
    NumericPadWidget oldnpw = (NumericPadWidget) old;
    NumericPadWidget newnpw = (NumericPadWidget) new;
    int base;

#define NE(field) (oldnpw->field != newnpw->field)

    if (NE(pad.base))
    {
	base = PRIVATE(new,base);
	if (base != OCT && base != DEC && base != HEX)
	{
	    base = PRIVATE(new,base) = DEC;
	}

	XtVaSetValues(PRIVATE(new,oct_widget),
		      XtNstate, base == OCT,
		      NULL);

	XtVaSetValues(PRIVATE(new,dec_widget),
		      XtNstate, base == DEC,
		      NULL);

	XtVaSetValues(PRIVATE(new,hex_widget),
		      XtNstate, base == HEX,
		      NULL);

	SetSensitivity(new);
    }
    
#undef NE

    return False;
}

static void
SetSensitivity(widget)
     Widget widget;
{
    WidgetList children;
    Cardinal numChildren, child;
    int ch, base = PRIVATE(widget,base);

    XtVaGetValues(widget,
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);

    /* Making assumptions about order children added */
    for (child = 0;  child < numChildren;  ++child)
    {
	if (!XtIsSubclass(children[child], toggleWidgetClass))
	{
	    break;
	}
    }

    for (ch = 0;  ch < HEX;  ++ch)
    {
	XtVaSetValues(children[child++],
		      XtNsensitive, ch < base ? True : False,
		      NULL);
    }
}

/* ARGSUSED */
static void
ModeChange(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget pad = (Widget) client_data;
    int base;

    if (XawToggleGetCurrent(widget) != (XtPointer) widget)
    {
	return;
    }

    if (widget == PRIVATE(pad,oct_widget))
    {
	base = OCT;
    }
    else if (widget == PRIVATE(pad,dec_widget))
    {
	base = DEC;
    }
    else
    {
	base = HEX;
    }

    PRIVATE(pad,base) = base;

    SetSensitivity(pad);
}

/* ARGSUSED */
static void
Type(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    char digit = (char) ((int) client_data);
    Widget text = ((NumericPadWidget) XtParent(widget))->pad.text_widget;
    XawTextBlock textBlock;
    XawTextPosition insertPos;

    insertPos = XawTextGetInsertionPoint(text);
    textBlock.firstPos = 0;
    textBlock.length = 1;
    textBlock.ptr = &digit;
    textBlock.format = FMT8BIT;

    XawTextReplace(text, insertPos, insertPos, &textBlock);
    
    XawTextSetInsertionPoint(text, insertPos + 1);
}

static void
Feed(widget)
     Widget widget;
{
    Widget pad = XtParent(widget);
    Widget text = PRIVATE(pad,text_widget);
    int c, base = PRIVATE(pad,base), value = 0;
    NumericPadReturnStruct result;
    String p;
    Boolean error = False;
    
    XtVaGetValues(text,
		  XtNstring, &result.value,
		  NULL);

    for (p = result.value;  *p;  ++p)
    {
	for (c = 0;  c < HEX;  ++c)
	{
	    if (*p == digits[c])
	    {
		if (c >= base)
		{
		    error = True;
		}
		break;
	    }
	}
	if (error)
	{
	    break;
	}
	else
	{
	    value = (base * value) + c;
	}
    }

    if (!error)
    {
	result.base = PRIVATE(pad,base);
	result.conversion = value;
	XtCallCallbackList(pad, PRIVATE(pad,enter_callbacks),
			                                 (XtPointer) &result);
    }
    else
    {
	XawTextSetSelection(text, p - result.value, p - result.value + 1);
        XawTextSetInsertionPoint(text,
				 (XawTextPosition) (p - result.value + 1));
	XBell(XtDisplay(text), 100);
    }
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
    Widget pad = XtParent(widget);

    XtCallCallbackList(pad, PRIVATE(pad,cancel_callbacks), (XtPointer) 0);
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

void
NumericPadClear(npw)
     Widget npw;
{
    if (!XtIsSubclass(npw, numericPadWidgetClass))
    {
	return;
    }

    XtVaSetValues(PRIVATE(npw,text_widget),
		  XtNstring, "",
		  NULL);
}
