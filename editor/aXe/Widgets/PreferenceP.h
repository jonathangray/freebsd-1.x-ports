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

#ifndef _PreferenceP_h
#define _PreferenceP_h

#include <X11/Xp/TableP.h>
#include <Preference.h>
     
typedef struct _PreferenceClass
{
    XtPointer extension;
} PreferenceClassPart;

typedef struct _PreferenceClassRec
{
    CoreClassPart               core_class;
    CompositeClassPart          composite_class;
    XpTableClassPart            table_class;
    PreferenceClassPart         preference_class;
} PreferenceClassRec;

extern PreferenceClassRec preferenceClassRec;

typedef struct _PreferencePart
{
    /* resources         */
    XtCallbackList       enter_callbacks;
    XtCallbackList       cancel_callbacks;

    /* private variables */
    Widget               alwaysScroll, neededScroll, neverScroll;
    Widget               lineWrap, wordWrap, noWrap, autoFill;
    Widget               read, write;
    Widget               tabEvery;
    Widget               thisBuffer, thisWindow, allWindows;
} PreferencePart;

typedef struct _PreferenceRec
{
    CorePart            core;
    CompositePart       composite;
    XpTablePart         table;
    PreferencePart      preference;
} PreferenceRec;

#endif /* _PreferenceP_h */
