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

#ifndef AxeEditorP_h
#define AxeEditorP_h

#include <X11/CompositeP.h>
#include <AxeEditor.h>
#include "regexp.h"

struct childInfo {
    Boolean first;
    Boolean replace;
    Widget ed;
    Widget window;
    XawTextPosition selStart;
    XawTextPosition selEnd;
};

typedef struct {
    String      name;
    Boolean     chosen;
} MenuRecord;

typedef struct {
    String      name;
    String      action;
} ButtonRecord;

typedef struct {
    Widget        widget;
    XtCallbackRec cbr;
} ShowMenuRec, *ShowMenuList;

typedef struct {
    String      label;
    String      name;
    XFontStruct *fontStruct;
} FontListStruct;

typedef struct
{
    XrmQuark name;
    int numArgs;
    String arg;
} MacroCommand, *MacroList;

typedef struct {
    /* Class variables */
    XtPointer  extension;
    Pixmap     axeLogo;
    Pixmap     fwdArrow;
    Pixmap     bwdArrow;
    Pixmap     vtCentre;
    Pixmap     hzCentre;
    Pixmap     tickMark;
    Pixmap     modMark;

    Widget     *editors;
    int        numberOfEditors;
    int        maxEditors;
    Widget     menu_parent;
    Widget     show_menu;
    int        show_menu_callbacks;
    ShowMenuList show_menu_callback_list;

    XtTranslations mini_translations;
    XtAccelerators mini_accelerators;
    FontListStruct *mini_menu;
    FontListStruct *default_mini_menu;
    
    char       *home_dir;
    int        home_dir_len;
    char       *current_dir;
    int        current_dir_len;

    XtActionHookId macro_id;
    int            macro_size;
    int            max_macro_size;
    Boolean        in_macro_string;
    MacroList      macro_commands;

    Boolean        show_work_proc;
} AxeEditorClassPart;

typedef struct _AxeEditorClassRec {
    CoreClassPart       core_class;
    CompositeClassPart  composite_class;
    AxeEditorClassPart	axeEditor_class;
} AxeEditorClassRec;

extern AxeEditorClassRec axeEditorClassRec;

typedef struct {
    /* Resources */
    String         file;
    String         menu_list;
    Boolean        buttons;
    String         button_list;
    int            info_timeout;
    Boolean        focus_to_text;
    Boolean        suppressFilename;
    Boolean        suppressInfobar;
    Boolean        suppressMinibuffer;
    String         font_list;
    Dimension      internal_border_width;
    XtCallbackList change_callbacks;
    String         full_pathnames;
    
    /* Private data */
    Widget         file_name;
    Widget         info_bar;
    XtIntervalId   info_timer;
    Widget         menu_bar;
    Widget         ed_deck;
    Widget         mini_buffer;
    Widget         mini_button;
    Widget         confirmer;
    FontListStruct *font_table;
    XtTranslations defaultTextTranslations;
    Boolean        accelerateMini;
} AxeEditorPart;

typedef struct _AxeEditorRec {
    CorePart       core;
    CompositePart  composite;
    AxeEditorPart  axeEditor;
} AxeEditorRec;

#endif /* AxeEditorP_h */
