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
#include <AxeEditor.h>

#include <AxeCommandP.h>

static void UpdateInfo();

static XtActionsRec actions[] = {
    "update-info",      UpdateInfo,
};

static char translations[] = "<Enter>: highlight() update-info()";

#define CLASS(field) axeCommandClassRec.axe_command_class.field

#define Offset(field) XtOffsetOf(AxeCommandRec, axeCommand.field)

static XtResource resources[] = {
  {XtNhelp,  XtCHelp, XtRString, sizeof(String),
       Offset(help), XtRString, NULL},
};   

#undef Offset

static void ClassInitialize(), Initialize();

AxeCommandClassRec axeCommandClassRec = {
  {
    /* superclass             */     (WidgetClass) &commandClassRec,  
    /* class_name             */     "AxeCommand",                    
    /* size                   */     sizeof(AxeCommandRec),           
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
  },  /* CoreClass fields initialization */
  {
    /* change_sensitive       */     XtInheritChangeSensitive            
  },  /* SimpleClass fields initialization */
#ifdef XAW3D
  {
    /* shadowdraw             */     XtInheritXaw3dShadowDraw,
  },   /* ThreeD class fields initialization */
#endif
  {
    /* foo                    */     0,
  },  /* LabelClass fields initialization */
  {
    /* makes_compiler_happy   */     0,
  },  /* CommandClass fields initialization */
  {
    /* extension              */     NULL,
  },  /* AxeCommandClass fields initialization */
};

WidgetClass axeCommandWidgetClass = (WidgetClass) &axeCommandClassRec;

static void
ClassInitialize()
{
     CLASS(translations) = XtParseTranslationTable(translations);
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    XtOverrideTranslations(new, CLASS(translations));
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

#undef CLASS
