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

#ifndef AxeEditor_h
#define AxeEditor_h

#include <X11/Xaw/Paned.h>

typedef struct _AxeEditorClassRec *AxeEditorWidgetClass;
typedef struct _AxeEditorRec      *AxeEditorWidget;

extern WidgetClass axeEditorWidgetClass;

extern int     AxeEditorNextEditor();
extern Widget  AxeEditorParentOf();
extern Widget  AxeEditorFileNameWidget();
extern Widget  AxeEditorInfoBarWidget();
extern Widget  AxeEditorMenuBarWidget();
extern Widget  AxeEditorEdWidget();
extern Widget  AxeEditorMiniBufferWidget();
extern void    AxeEditorUpdateInfoBar();
extern Boolean AxeEditorIterate();
extern Widget  AxeEditorCreateWindow();
extern void    AxeEditorHelpWindow();
extern void    AxeEditorAddShowMenuCallback();

#define XtNbuttons             "buttons"
#define XtCButtons             "Buttons"
#define XtNmenuList            "menuList"
#define XtCMenuList            "MenuList"
#define XtNbuttonList          "buttonList"
#define XtCButtonList          "ButtonList"
#define XtNinfoTimeout         "infoTimeout"
#define XtCInfoTimeout         "InfoTimeout"
#define XtNfocusToText         "focusToText"
#define XtCFocusToText         "FocusToText"
#define XtCSuppressPane        "SuppressPane"
#define XtNsuppressFilename    "suppressFilename"
#define XtNsuppressInfobar     "suppressInfobar"
#define XtNsuppressMinibuffer  "suppressMinibuffer"
#define XtNfontList            "fontList"
#define XtCFontList            "FontList"
#define XtNinternalBorderWidth "internalBorderWidth"
#define XtNchangeCallback      "changeCallback"
#define XtNminiMenu            "miniMenu"
#define XtCMiniMenu            "MiniMenu"
#define XtNfullPathnames       "fullPathnames"
#define XtCFullPathnames       "FullPathnames"

#endif /* AxeEditor_h */
