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

#ifndef ConfirmerP_h
#define ConfirmerP_h

#include <X11/ShellP.h>
#include "Confirmer.h"

typedef struct {
    XtPointer      extension;
    Pixmap         warnLogo;
} ConfirmerClassPart;

typedef struct _ConfirmerClassRec {
    CoreClassPart           core_class;
    CompositeClassPart      composite_class;
    ShellClassPart          shell_class;
    WMShellClassPart        wm_shell_class;
    VendorShellClassPart    vendor_shell_class;
    TransientShellClassPart transient_shell_class;
    ConfirmerClassPart	    confirmer_class;
} ConfirmerClassRec;

extern ConfirmerClassRec confirmerClassRec;

typedef struct {
    /* Resources */
    Boolean           grab_on_popup;

    /* Private data */
    Widget            warning_widget;
    Widget            confirm_widget;
    XtCallbackProc    confirm_callback;
    Widget            alt_widget;
    XtCallbackProc    alt_callback;
} ConfirmerPart;

typedef struct _ConfirmerRec {
    CorePart           core;
    CompositePart      composite;
    ShellPart          shell;
    WMShellPart        wm;
    VendorShellPart    vendor;
    TransientShellPart transient;
    ConfirmerPart      confirmer;
} ConfirmerRec;

#endif /* ConfirmerP_h */

