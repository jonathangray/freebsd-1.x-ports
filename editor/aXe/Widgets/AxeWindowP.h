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

#ifndef AxeWindowP_h
#define AxeWindowP_h

#include <X11/ShellP.h>
#include "AxeWindow.h"

typedef struct {
    XtPointer extension;
    int       number_of_windows;
} AxeWindowClassPart;

typedef struct _AxeWindowClassRec {
    CoreClassPart          core_class;
    CompositeClassPart     composite_class;
    ShellClassPart         shell_class;
    WMShellClassPart       wm_shell_class;
    VendorShellClassPart   vendor_shell_class;
    TopLevelShellClassPart top_level_shell_class;
    AxeWindowClassPart	   axeWindow_class;
} AxeWindowClassRec;

extern AxeWindowClassRec axeWindowClassRec;

typedef struct {
    /* Resources */
    String       file;
    Boolean      file_title;
    String       file_title_prefix;
    String       icon_name_prefix;
    Boolean      exit_on_close_last_window;
} AxeWindowPart;

typedef struct _AxeWindowRec {
    CorePart          core;
    CompositePart     composite;
    ShellPart         shell;
    WMShellPart       wm;
    VendorShellPart   vendor;
    TopLevelShellPart topLevel;
    AxeWindowPart     axeWindow;
} AxeWindowRec;

#endif /* AxeWindowP_h */
