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

#ifndef _AxeiiText_h
#define _AxeiiText_h

#include <X11/Xaw/AsciiText.h>

typedef struct _AxeiiTextClassRec	*AxeiiTextWidgetClass;
typedef struct _AxeiiTextRec	        *AxeiiTextWidget;

extern WidgetClass axeiiTextWidgetClass;

extern void AxeiiTextWatchForChanges();

typedef struct {
    int line;
    int position;
} AxeiiTextWhereStruct;

typedef struct {
    int rows;
    int columns;
} AxeiiTextSizeStruct;

#define XtNrows                  "rows"
#define XtCRows                  "Rows"
#define XtNcolumns               "columns"
#define XtCColumns               "Columns"
#define XtNblockCaret            "blockCaret"
#define XtCBlockCaret            "BlockCaret"
#define XtNcaretBitmap           "caretBitmap"
#define XtNtabEvery              "tabEvery"
#define XtCTabEvery              "TabEvery"
#define XtNblinkPeriod           "blinkPeriod"
#define XtCBlinkPeriod           "BlinkPeriod"
#define XtNexpandTabs            "expandTabs"
#define XtCExpandTabs            "ExpandTabs"
#define XtNwatchingChanges       "watchingChanges"
#define XtCWatchingChanges       "WatchingChanges"
#define XtNmodifiedCallback      "modifiedCallback"
#define XtNgotoLineCallback      "gotoLineCallback"
#define XtNincludeFileCallback   "includeFileCallback"
#define XtNinsertControlCallback "insertControlCallback"
#define XtNwhereCallback         "whereCallback"
#define XtNsizeCallback          "sizeCallback"
#define XtNpreferencesCallback   "preferencesCallback"
#define XtNclearBufferCallback   "clearBufferCallback"
#define XtNsaveFileCallback      "saveFileCallback"
#define XtNsaveAsCallback        "saveAsCallback"
#define XtNloadFileCallback      "loadFileCallback"
#define XtNreloadFileCallback    "reloadFileCallback"
#define XtNnoUndoCallback        "noUndoCallback"
#define XtNundo                  "undo"
#define XtCUndo                  "Undo"
#define XtNundoLevel             "undoLevel"
#define XtCUndoLevel             "UndoLevel"
#define XtNdeleteOnInsert        "deleteOnInsert"
#define XtCDeleteOnInsert        "DeleteOnInsert"

void AxeiiTextWidthHeightToRowsColumns();
void AxeiiTextRowsColumnsToWidthHeight();
void AxeiiTextWatchForChanges();
void AxeiiTextGotoLine();
Boolean AxeiiTextReSearch();
String AxeiiTextRead();
void AxeiiTextGetRegion();
void CopyCallbackList();
void (*AxeiiTextUndoPreInsert())();
void (*AxeiiTextUndoPreReplace())();
void (*AxeiiTextUndoPostLoad())();

#endif /* _AxeiiText_h */
