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

#ifndef _ViewlistP_h
#define _ViewlistP_h

/* Don't know why these two are needed, when the superclass doesn't */
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

#include <X11/Xaw/ViewportP.h>
#include <Viewlist.h>

typedef struct {
    XtPointer extension;
} ViewlistClassPart;

typedef struct _ViewlistClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ConstraintClassPart	constraint_class;
    FormClassPart	form_class;
    ViewportClassPart	viewport_class;
    ViewlistClassPart   viewlist_list;
} ViewlistClassRec;

extern ViewlistClassRec viewlistClassRec;

typedef struct _ViewlistPart {
    /* resources */
    Widget list_widget;
    
    /* private variables */
} ViewlistPart;

typedef struct _ViewlistRec {
    CorePart	core;
    CompositePart	composite;
    ConstraintPart	constraint;
    FormPart		form;
    ViewportPart	viewport;
    ViewlistPart        viewlist;
} ViewlistRec;

#endif /* _ViewlistP_h */
