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

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>       /* for XtNcreateChildProc */
#include <AxeEditor.h>

#include <AxeMenuBtnP.h>

static void Initialize();

#if defined(XtSpecificationRelease) && XtSpecificationRelease < 5
static void GetValuesHook();
#endif

static void SavePopper(), UpdateInfo(), MaybeMake(), MaybePop(), MaybeDrop();

static XtActionsRec actions[] = {
    "update-info",      UpdateInfo,
    "maybe-make",       MaybeMake,
    "maybe-pop",        MaybePop,
    "maybe-drop",       MaybeDrop,
};

static char translations[] = "\
     <Enter>: highlight() update-info() maybe-pop() \n\
     <BtnDown>: set() notify() reset() maybe-make() PopupMenu()\n\
     <Leave>: reset() maybe-drop()";

#define offset(field) XtOffsetOf(AxeMenuButtonRec, axeMenuButton.field)

static XtResource resources[] = {
  {XtNhelp,  XtCHelp, XtRString, sizeof(String),
     offset(help), XtRString, NULL},
};   

#undef offset

AxeMenuButtonClassRec axeMenuButtonClassRec = {
  {
    /* superclass             */     (WidgetClass) &menuButtonClassRec,  
    /* class_name             */     "AxeMenuButton",                    
    /* size                   */     sizeof(AxeMenuButtonRec),           
    /* class_initialize       */     NULL,                               
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
    /* set_values             */     NULL,                               
    /* set_values_hook        */     NULL,                               
    /* set_values_almost      */     XtInheritSetValuesAlmost,           
    /* get_values_hook        */     NULL,                               
    /* accept_focus           */     NULL,                               
    /* version                */     XtVersion,                          
    /* callback_private       */     NULL,                               
    /* tm_table               */     translations,
    /* query_geometry         */     XtInheritQueryGeometry,             
    /* display_accelerator    */     XtInheritDisplayAccelerator,        
    /* extension              */     NULL                                
  },  /* CoreClass fields initialization */
  {
    /* change_sensitive       */     XtInheritChangeSensitive            
  },  /* SimpleClass fields initialization */
#ifdef XAW3D
  {
    /* shadowdraw             */     XtInheritXaw3dShadowDraw,
  },  /* ThreeD fields initialization */
#endif
  {
    /* foo                    */     0,
  },  /* LabelClass fields initialization */
  {
    /* makes_compiler_happy   */     0,
  },  /* CommandClass fields initialization */
  {
    /* makes_compiler_happy   */     0,
  },  /* MenuButtonClass fields initialization */
  {
    /* extension              */     NULL,
  },  /* AxeMenuButtonClass fields initialization */
};

WidgetClass axeMenuButtonWidgetClass = (WidgetClass) &axeMenuButtonClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    XtAddCallback(new, XtNcallback, SavePopper,
		                           (XtPointer) AxeEditorParentOf(new));
}

/* ARGSUSED */
static void
SavePopper(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    String menuName;
    Widget w, menu;

    XtVaGetValues(widget, XtNmenuName, &menuName, NULL);

    for (menu = (Widget) 0, w = widget;  w;  w = XtParent(w))
    {
	if ((menu = XtNameToWidget(w, menuName)))
	{
	    break;
	}
    }
    AxeSimpleMenuStorePopperUpper(menu, (Widget) client_data);
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
    String help;

    XtVaGetValues(widget,
		  XtNhelp, &help,
		  NULL);

    AxeEditorUpdateInfoBar(AxeEditorParentOf(widget), help);
}

/*************************************************************
 *
 *                          maybe-make
 *
 *************************************************************/

/*ARGSUSED*/
static void
MaybeMake(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    String menuName;
    Widget menu, temp;
    XtCreatePopupChildProc create;

    XtVaGetValues(widget, XtNmenuName, &menuName, NULL);

    temp = widget;
    while(temp != NULL) {
	menu = XtNameToWidget(temp, menuName);
	if (menu == NULL) 
	    temp = XtParent(temp);
	else
	    break;
    }

    XtVaGetValues(menu,
		  XtNcreatePopupChildProc, &create,
		  NULL);

    if (create)
    {
	(*create) (menu);

	XtVaSetValues(menu,
		      XtNcreatePopupChildProc, (XtCreatePopupChildProc) 0,
		      NULL);
    }
}

/*************************************************************
 *
 *                         maybe-pop
 *
 *************************************************************/

/*ARGSUSED*/
static void
MaybePop(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (event->xcrossing.state | (Button1Mask & Button2Mask & Button3Mask &
                                  Button4Mask & Button5Mask))
    {
        XtCallActionProc(widget,
                         "maybe-make",
                         event,
                         (String *) 0,
                         (Cardinal) 0);

	SavePopper(widget, (XtPointer) AxeEditorParentOf(widget),
			                                        (XtPointer) 0);
        XtCallActionProc(widget,
                         "PopupMenu",
                         event,
                         (String *) 0,
                         (Cardinal) 0);
    }
}

/*************************************************************
 *
 *                         maybe-drop
 *
 *************************************************************/

/*ARGSUSED*/
static void
MaybeDrop(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Dimension height;
    String menuName;
    Widget w, menu;

    if (event->xcrossing.state | (Button1Mask & Button2Mask & Button3Mask &
                                  Button4Mask & Button5Mask))
    {
        XtVaGetValues(widget,
                      XtNheight, &height,
                      XtNmenuName, &menuName,
                      NULL);

        if (event->xcrossing.y > 0 && event->xcrossing.y >= height)
        {
            return;
        }

        for (w = widget;  w;  w = XtParent(w))
        {
            if ((menu = XtNameToWidget(w, menuName)))
            {
                XtPopdown(menu);
                break;
            }
        }
    }
}
