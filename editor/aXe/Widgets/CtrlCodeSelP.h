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

#ifndef _ControlCodeSelectorP_h
#define _ControlCodeSelectorP_h

#include <X11/Xp/TableP.h>
#include <CtrlCodeSel.h>
     
typedef struct _ControlCodeSelectorClass
{
    XtPointer      extension;
    XtTranslations listTranslations;
} ControlCodeSelectorClassPart;

typedef struct _ControlCodeSelectorClassRec
{
    CoreClassPart                core_class;
    CompositeClassPart           composite_class;
    XpTableClassPart             table_class;
    ControlCodeSelectorClassPart control_code_selector_class;
} ControlCodeSelectorClassRec;

extern ControlCodeSelectorClassRec controlCodeSelectorClassRec;

typedef struct _ControlCodeSelectorPart
{
    /* resources         */
    int                  base;
    XtCallbackList       enter_callbacks;
    XtCallbackList       cancel_callbacks;

    /* private variables */
    Widget               list_widget;
    Widget               code_widget;
} ControlCodeSelectorPart;

typedef struct _ControlCodeSelectorRec
{
    CorePart                core;
    CompositePart           composite;
    XpTablePart             table;
    ControlCodeSelectorPart control;
} ControlCodeSelectorRec;

#endif /* _ControlCodeSelectorP_h */
