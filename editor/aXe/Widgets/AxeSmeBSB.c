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

#include <AxeSmeBSBP.h>

#define offset(field) XtOffsetOf(AxeSmeBSBRec, axeSmeBsb.field)

static XtResource resources[] = {
  {XtNhelp,  XtCHelp, XtRString, sizeof(String),
     offset(help), XtRString, NULL},
};   

#undef offset

AxeSmeBSBClassRec axeSmeBSBClassRec = {
  {
    /* superclass         */    (WidgetClass) &smeBSBClassRec,
    /* class_name         */    "AxeSmeBSB",
    /* size               */    sizeof(AxeSmeBSBRec),
    /* class_initializer  */    NULL,
    /* class_part_initialize*/  NULL,
    /* Class init'ed      */    FALSE,
    /* initialize         */    NULL,
    /* initialize_hook    */    NULL,
    /* realize            */    NULL,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    resources,
    /* resource_count     */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE, 
    /* compress_exposure  */    FALSE,
    /* compress_enterleave*/    FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    NULL,
    /* expose             */    XtInheritExpose,
    /* set_values         */    NULL,
    /* set_values_hook    */    NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,  
    /* get_values_hook    */    NULL,
    /* accept_focus       */    NULL,
    /* intrinsics version */    XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table           */    NULL,
    /* query_geometry     */    XtInheritQueryGeometry,
    /* display_accelerator*/    NULL,
    /* extension          */    NULL
  },{
    /* Menu Entry Fields */
      
    /* highlight */             XtInheritHighlight,
    /* unhighlight */           XtInheritUnhighlight,
    /* notify */                XtInheritNotify,
    /* extension          */    NULL
  },
#ifdef XAW3D
  {
    /* ThreeD Fields */

    /* shadowdraw         */    XtInheritXawSme3dShadowDraw
  },
#endif
  {
    /* BSB Menu entry Fields */  

    /* extension          */    NULL
  }, {
    /* AxeBSB Menu entry Fields */  

    /* extension          */    NULL
  }

};

WidgetClass axeSmeBSBObjectClass = (WidgetClass) &axeSmeBSBClassRec;



