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

#ifndef _AxeSimMenP_h
#define _AxeSimMenP_h

#include <X11/IntrinsicP.h>
#include <X11/Xaw/SimpleMenP.h>
#include <AxeSimMenu.h>

typedef struct {
    XtPointer extension;
} AxeSimpleMenuClassPart;

typedef struct _AxeSimpleMenuClassRec {
    CoreClassPart	    core_class;
    CompositeClassPart      composite_class;
    ShellClassPart          shell_class;
    OverrideShellClassPart  overrideShell_class;
    SimpleMenuClassPart	    simpleMenu_class;
    AxeSimpleMenuClassPart  axeMenu_class;
} AxeSimpleMenuClassRec;

extern AxeSimpleMenuClassRec axeSimpleMenuClassRec;

typedef struct _AxeSimpleMenuPart {
    Widget popperUpper;
} AxeSimpleMenuPart;

typedef struct _AxeSimpleMenuRec {
    CorePart		core;
    CompositePart 	composite;
    ShellPart 	        shell;
    OverrideShellPart   override;
    SimpleMenuPart	simpleMenu;
    AxeSimpleMenuPart   axeSimpleMenu;
} AxeSimpleMenuRec;

#endif /* _AxeSimMenP_h */
