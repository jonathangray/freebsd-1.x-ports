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

#ifndef _AxeiiTextP_h
#define _AxeiiTextP_h

#include <X11/Xaw/AsciiTextP.h>
#include <AxeiiText.h>
#include <AxeiiUndo.h>
#include "regexp.h"

typedef struct {
    XtPointer       extension;
    XtTranslations  translations;
    XtTranslations  search_translations;
    XtTranslations  replace_translations;

    void            (*preInsertUndo)();
    void            (*preReplaceUndo)();
    void            (*postLoadUndo)();
    void            (*superInsertChar)();
    void            (*superSearch)();
} AxeiiClassPart;

typedef struct _AxeiiTextClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    TextClassPart	text_class;
    AsciiClassPart	ascii_class;
    AxeiiClassPart      axeii_class;
} AxeiiTextClassRec;

extern AxeiiTextClassRec axeiiTextClassRec;

typedef struct {
    /*     Resources    */
    int                 rows;
    int                 columns;
    Boolean             block_caret;
    String              caret_bitmap;
    int                 tab_every;
    long                blink_period;
    Boolean             expand_tabs;
    Boolean             watching_changes;
    XtCallbackList      modified_callbacks;
    XtCallbackList      goto_line_callbacks;
    XtCallbackList      include_file_callbacks;
    XtCallbackList      insert_control_callbacks;
    XtCallbackList      where_callbacks;
    XtCallbackList      size_callbacks;
    XtCallbackList      preferences_callbacks;
    XtCallbackList      clear_buffer_callbacks;
    XtCallbackList      save_file_callbacks;
    XtCallbackList      save_as_callbacks;
    XtCallbackList      load_file_callbacks;
    XtCallbackList      reload_file_callbacks;
    XtCallbackList      no_undo_callbacks;
    Boolean             undo;
    int                 undo_level;
    Boolean             delete_on_insert;

    /* Private variables */
    XtActionHookId      action_hook;
    XtIntervalId        blink_timer;
    int                 fwd_bwd_indent;
    XawTextPosition     mark;
    XtTranslations      translations;
    XtTranslations      keymap;

    /* Regular expression searching */
    Widget              search;
    Widget              label1;
    Widget              label2;
    Widget              forwards;
    Widget              searchText;
    Widget              replaceText;
    Widget              searchTranslations;
    Widget              replaceTranslations;
    Widget              searchOne;
    XtCallbackList      searchCallbacks;
    Widget              replaceOne;
    XtCallbackList      replaceOneCallbacks;
    Widget              replaceAll;
    XtCallbackList      replaceAllCallbacks;
    regexp              *compexp;
    String              matchedText;
    XawTextPosition     matchBegin;
    XawTextPosition     matchEnd;

    XtActionHookId      undo_hook;
    XtActionHookId      insert_popup_hook;
    XtActionHookId      search_popup_hook;
    UndoRecord          *undo_head;
    UndoRecord          *undo_tail;
    Boolean             undoing;
    XrmQuark            lastAction;
    XtCallbackList      insertFileCallbacks;
    XtCallbackList      searchOneCallbacks;
    XtCallbackList      searchAllCallbacks;
} AxeiiPart;

typedef struct _AxeiiTextRec {
    CorePart		core;
    SimplePart		simple;
    TextPart		text;
    AsciiPart           ascii;
    AxeiiPart           axeii;
} AxeiiTextRec;

#endif /* _AxeiiTextP_h */
