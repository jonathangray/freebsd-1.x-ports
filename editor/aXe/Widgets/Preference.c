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
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Box.h>
#include <AxeText.h>

#include <PreferenceP.h>

#define PRIVATE(w,field) (((PreferenceWidget) w)->preference.field)

#define TableOffset(field) XtOffsetOf(PreferenceRec, table.field)
#define Offset(field) XtOffsetOf(PreferenceRec, preference.field)

static char layout[] = "lscroll 0 0; bscroll 1 0; \
                        lwrap   0 1; bwrap   1 1; \
                        ledmode 0 2; bedmode 1 2; \
                        ltabs   0 3; btabs   1 3; \
                        lapply  0 5; bapply  1 5; \
                        accept  0 6;";

static XtResource resources[] = {
    /* Table Widget Resources */
    { XtNlayout, XtCLayout, XtRXpTableLoc, sizeof(XpTableLoc),
	  TableOffset(default_layout), XtRString, (XtPointer) layout },

    /* Preference Widget Resources */
    {XtNenterCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
         Offset(enter_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNcancelCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
         Offset(cancel_callbacks), XtRCallback, (XtPointer) NULL},
};

#undef Offset

static void Initialize();
static void AutoFill(), NoAutoFill(), Ready(), Accept(), Cancel();

PreferenceClassRec preferenceClassRec = {
  {
    /* superclass             */     (WidgetClass) &xpTableClassRec,  
    /* class_name             */     "Preference",                    
    /* size                   */     sizeof(PreferenceRec),           
    /* class_initialize       */     NULL,                               
    /* class_part_initialize  */     NULL,                               
    /* class_inited           */     FALSE,                              
    /* initialize             */     Initialize,
    /* initialize_hook        */     NULL,                               
    /* realize                */     XtInheritRealize,                   
    /* actions                */     NULL,
    /* num_actions            */     0,
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
    /* set_values             */     NULL,                               
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
  },/* PreferenceClass fields initialization */
};

WidgetClass preferenceWidgetClass = (WidgetClass) &preferenceClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    Widget bscroll, bwrap, bedmode, btabs;
    Widget bapply, accept, enter, cancel;
    
    /* The parent is a popup and its parent is an AxeText widget */;
    XtAddCallback(XtParent(new), XtNpopupCallback, Ready, (XtPointer) 0);

    XtVaCreateManagedWidget("lscroll", labelWidgetClass, new, NULL);

    bscroll = XtVaCreateManagedWidget("bscroll", boxWidgetClass, new, NULL);

    PRIVATE(new,alwaysScroll) =
	XtVaCreateManagedWidget("Always", toggleWidgetClass, bscroll, NULL);

    PRIVATE(new,neededScroll) =
	XtVaCreateManagedWidget("When Needed", toggleWidgetClass, bscroll,
				XtNradioGroup, PRIVATE(new,alwaysScroll),
				NULL);
    PRIVATE(new,neverScroll) =
	XtVaCreateManagedWidget("Never", toggleWidgetClass, bscroll,
				XtNradioGroup, PRIVATE(new,alwaysScroll),
				NULL);
    


    XtVaCreateManagedWidget("lwrap", labelWidgetClass, new, NULL);

    bwrap = XtVaCreateManagedWidget("bwrap", boxWidgetClass, new, NULL);

    PRIVATE(new,lineWrap) =
	XtVaCreateManagedWidget("Line", toggleWidgetClass, bwrap, NULL);
    XtAddCallback(PRIVATE(new,lineWrap), XtNcallback, NoAutoFill,
		                                             (XtPointer) new);
    PRIVATE(new,wordWrap) =
	XtVaCreateManagedWidget("Word", toggleWidgetClass, bwrap,
				XtNradioGroup, PRIVATE(new,lineWrap),
				NULL);
    XtAddCallback(PRIVATE(new,wordWrap), XtNcallback, NoAutoFill,
		                                             (XtPointer) new);
    PRIVATE(new,noWrap) =
	XtVaCreateManagedWidget("No Wrap", toggleWidgetClass, bwrap,
				XtNradioGroup, PRIVATE(new,lineWrap),
				NULL);
    
    PRIVATE(new,autoFill) =
	XtVaCreateManagedWidget("autoFill", toggleWidgetClass, bwrap,
				NULL);
    XtAddCallback(PRIVATE(new,autoFill), XtNcallback, AutoFill,
		                                             (XtPointer) new);
    


    XtVaCreateManagedWidget("ledmode", labelWidgetClass, new, NULL);

    bedmode = XtVaCreateManagedWidget("bedmode", boxWidgetClass, new, NULL);

    PRIVATE(new,write) =
	XtVaCreateManagedWidget("Write", toggleWidgetClass, bedmode, NULL);

    PRIVATE(new,read) =
	XtVaCreateManagedWidget("Read", toggleWidgetClass, bedmode,
				XtNradioGroup, PRIVATE(new,write),
				NULL);



    XtVaCreateManagedWidget("ltabs", labelWidgetClass, new, NULL);

    btabs = XtVaCreateManagedWidget("btabs", boxWidgetClass, new, NULL);

    PRIVATE(new,tabEvery) =
	XtVaCreateManagedWidget("tabEvery", axeiiTextWidgetClass, btabs,
				XtNrows, 1, XtNcolumns, 2,
				XtNeditType, XawtextEdit,
				NULL);
    XtSetKeyboardFocus(new, PRIVATE(new,tabEvery));


    XtVaCreateManagedWidget("lapply", labelWidgetClass, new, NULL);

    bapply = XtVaCreateManagedWidget("bapply", boxWidgetClass, new, NULL);

    PRIVATE(new,thisBuffer) =
	XtVaCreateManagedWidget("This Buffer", toggleWidgetClass, bapply, NULL);

    PRIVATE(new,thisWindow) =
	XtVaCreateManagedWidget("This Window", toggleWidgetClass, bapply,
				XtNradioGroup, PRIVATE(new,thisBuffer),
				XtNsensitive, False,
				NULL);

    PRIVATE(new,allWindows) =
	XtVaCreateManagedWidget("All Windows", toggleWidgetClass, bapply,
				XtNradioGroup, PRIVATE(new,thisBuffer),
				XtNsensitive, False,
				NULL);

    accept = XtVaCreateManagedWidget("accept", boxWidgetClass, new, NULL);

    enter = XtVaCreateManagedWidget("Enter", commandWidgetClass, accept, NULL);
    XtAddCallback(enter, XtNcallback, Accept, (XtPointer) new);

    cancel = XtVaCreateManagedWidget("Cancel", commandWidgetClass, accept, NULL);
    XtAddCallback(cancel, XtNcallback, Cancel, (XtPointer) new);
}

/* ARGSUSED */
static void
AutoFill(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget noWrap = PRIVATE((Widget) client_data,noWrap);
    Boolean autoFill;
    XtPointer radioData;

    XtVaGetValues(widget, XtNstate, &autoFill, NULL);
    
    if (autoFill)
    {
	XtVaGetValues(noWrap, XtNradioData, &radioData, NULL);

	XawToggleSetCurrent(noWrap, radioData);
    }
}

/* ARGSUSED */
static void
NoAutoFill(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Boolean state;

    XtVaGetValues(widget, XtNstate, &state, NULL);

    if (state)
    {
	XtVaSetValues(PRIVATE(((Widget) client_data),autoFill),
		      XtNstate, False,
		      NULL);
    }
}

/* ARGSUSED */
static void
Ready(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axeText = AxeTextWidgetParentOf(widget), nominator;
    Widget pref = ((CompositeWidget) widget)->composite.children[0];
    XawTextScrollMode scroll;
    XawTextWrapMode wrap;
    Boolean fill;
    XawTextEditType edit;
    int every;
    char buf[8];

    XtVaGetValues(axeText,
		  XtNscrollVertical, &scroll,
		  XtNwrap, &wrap,
		  XtNautoFill, &fill,
		  XtNeditType, &edit,
		  XtNtabEvery, &every,
		  NULL);
    
    XtVaSetValues(scroll == XawtextScrollAlways ?  PRIVATE(pref,alwaysScroll) :
	       scroll == XawtextScrollWhenNeeded ? PRIVATE(pref,neededScroll) :
		                                   PRIVATE(pref,neverScroll),
		  XtNstate, True,
		  NULL);
	
    XtVaSetValues(wrap == XawtextWrapLine ? PRIVATE(pref,lineWrap) :
		  wrap == XawtextWrapWord?  PRIVATE(pref,wordWrap) :
		                            PRIVATE(pref,noWrap),
		  XtNstate, True,
		  NULL);

    XtVaSetValues(PRIVATE(pref,autoFill),
		  XtNstate, fill,
		  NULL);
    
    XtVaSetValues(edit == XawtextEdit ? PRIVATE(pref,write) :
		                        PRIVATE(pref,read),
		  XtNstate, True,
		  NULL);

    sprintf(buf, "%d", every);
    XtVaSetValues(PRIVATE(pref,tabEvery),
		  XtNstring, buf,
		  NULL);

    XtVaSetValues(PRIVATE(pref,thisBuffer),
		  XtNstate, True,
		  NULL);
}

/* ARGSUSED */
static void
Accept(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget pref;
    PreferenceReturnStruct preferences;
    String scroll, wrap, edit, tabs;

    pref = (Widget) client_data;

    scroll = XawToggleGetCurrent(PRIVATE(pref,alwaysScroll));
    if (strcmp(scroll, "Always") == 0)
    {
	preferences.scrollMode = XawtextScrollAlways;
    }
    else if(strcmp(scroll, "When Needed") == 0)
    {
	preferences.scrollMode = XawtextScrollWhenNeeded;
    }
    else
    {
	preferences.scrollMode = XawtextScrollNever;
    }

    wrap = (String) XawToggleGetCurrent(PRIVATE(pref,lineWrap));
    if (strcmp(wrap, "Line") == 0)
    {
	preferences.wrapMode = XawtextWrapLine;
    }
    else if (strcmp(wrap, "Word") == 0)
    {
	preferences.wrapMode = XawtextWrapWord;
    }
    else
    {
	preferences.wrapMode = XawtextWrapNever;
    }
	    
    XtVaGetValues(PRIVATE(pref,autoFill),
		  XtNstate, &preferences.autoFill,
		  NULL);

    edit = XawToggleGetCurrent(PRIVATE(pref,write));
    if (strcmp(edit, "Write") == 0)
    {
	preferences.editType = XawtextEdit;
    }
    else
    {
	preferences.editType = XawtextRead;
    }

    XtVaGetValues(PRIVATE(pref,tabEvery),
		  XtNstring, &tabs,
		  NULL);
    preferences.tabEvery = atoi(tabs);

    XtCallCallbackList(pref,
		      PRIVATE(pref,enter_callbacks), (XtPointer) &preferences);
}

/* ARGSUSED */
static void
Cancel(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget pref = XtParent(XtParent(widget));

    XtCallCallbackList(pref, PRIVATE(pref,cancel_callbacks), (XtPointer) 0);
}
