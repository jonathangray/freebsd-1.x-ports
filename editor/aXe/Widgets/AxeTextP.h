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

#ifndef _AxeTextP_h
#define _AxeTextP_h

#include <AxeiiTextP.h>
#include <AxeText.h>

typedef struct {
    XtPointer      extension;
} AxeClassPart;

typedef struct _AxeTextClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    TextClassPart	text_class;
    AsciiClassPart	ascii_class;
    AxeiiClassPart      axeii_class;
    AxeClassPart        axe_class;
} AxeTextClassRec;

extern AxeTextClassRec axeTextClassRec;

typedef struct {
    /* Resources         */
    String               associated_directory;
    Boolean              grab_on_popup;
    XtCallbackList       change_callbacks;
    XtCallbackList       message_callbacks;
    Boolean              enable_backups;
    String               backup_name_prefix;
    String               backup_name_suffix;

    /* Private variables */
    String               initial_directory;
    Boolean              null_file;
    String               associated_file;
    Boolean              is_modified;
    Widget               file_nominator;
    Widget               numeric_pad;
    Widget               control_selector;
    Widget               confirm_popup;
    Widget               confirm_message;
    Widget               confirm_confirm;
    Widget               preference_popup;
    Boolean              backed_up;
} AxePart;

typedef struct _AxeTextRec {
    CorePart		core;
    SimplePart		simple;
    TextPart		text;
    AsciiPart           ascii;
    AxeiiPart           axeii;
    AxePart             axe;
} AxeTextRec;

#endif /* _AxeTextP_h */
