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

#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Converters.h>
#include <X11/keysym.h>
#include <ctype.h>
#include <stdio.h>
#include <sys/file.h>
#include "AxeiiSink.h"
#include "AxeiiTextP.h"
#include "AxeiiUndo.h"
#include "regexp.h"

/*
 * Mimics use in TextPop.c w.r.t. search popup's direction toggles' radio data 
*/
#define R_OFFSET 1

#define CLOSURE 01010
#define CARETHEIGHT (XT_REVISION < 5 ? caretHeight : 0)

#define Offset(field) XtOffsetOf(AxeiiTextRec, axeii.field)

static XtResource resources[] = {
    {XtNrows, XtCRows, XtRInt, sizeof(int),
         Offset(rows), XtRImmediate, (XtPointer) 24},
    {XtNcolumns, XtCColumns, XtRInt, sizeof(int),
         Offset(columns), XtRImmediate, (XtPointer) 80},
    /*
     * These have yet to be catered for in SetValues
     */
    {XtNblockCaret, XtCBlockCaret, XtRBoolean, sizeof(Boolean),
         Offset(block_caret), XtRImmediate, (XtPointer) False},
    {XtNcaretBitmap, XtCBitmap, XtRString, sizeof(String),
         Offset(caret_bitmap), XtRString, (XtPointer) NULL},

    {XtNtabEvery, XtCTabEvery, XtRInt, sizeof(int),
         Offset(tab_every), XtRImmediate, (XtPointer) NULL},
    {XtNblinkPeriod, XtCBlinkPeriod, XtRLong, sizeof(long),
         Offset(blink_period), XtRString, (XtPointer) "500"},
    {XtNexpandTabs, XtCExpandTabs, XtRBoolean, sizeof(Boolean),
         Offset(expand_tabs), XtRImmediate, (XtPointer) False},
    {XtNwatchingChanges, XtCWatchingChanges, XtRBoolean, sizeof(Boolean),
         Offset(watching_changes), XtRImmediate, (XtPointer) False},
    {XtNmodifiedCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(modified_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNgotoLineCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(goto_line_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNincludeFileCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(include_file_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNinsertControlCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(insert_control_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNwhereCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(where_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNsizeCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(size_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNpreferencesCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(preferences_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNclearBufferCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(clear_buffer_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNsaveFileCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(save_file_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNsaveAsCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(save_as_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNreloadFileCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(reload_file_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNloadFileCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(load_file_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNnoUndoCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(no_undo_callbacks), XtRCallback, (XtPointer) NULL},
    {XtNundo, XtCUndo, XtRBoolean, sizeof(Boolean),
         Offset(undo), XtRImmediate, (XtPointer) True},
    {XtNundoLevel, XtCUndoLevel, XtRInt, sizeof(int),
         Offset(undo_level), XtRImmediate, (XtPointer) 1},
    {XtNdeleteOnInsert, XtCDeleteOnInsert, XtRBoolean, sizeof(Boolean),
         Offset(delete_on_insert), XtRImmediate, (XtPointer) False},
};

#undef Offset

#define CLASS(field) axeiiTextClassRec.axeii_class.field
#define PRIVATE(w,field) (((AxeiiTextWidget) w)->axeii.field)

static void InsertCharAction(), SearchAction();
static void GotoLine(), SearchCaret(), FwdSearchSelection(), BwdSearchSelection();
static void ReSearch(), DoReSearch(), DoReReplace(), PopdownSearch();
static void Search(), ReplaceOne(), ReplaceAll();
static void IncludeFile(), IncludeSelection(), Paste(), InsertControl();
static void DeleteWord(), DeleteLine();
static void Where(), Size(), CentreLine(), SetPreferences(); 
static void ClearBuffer();
static void SaveFile(), SaveAs(), LoadFile(), ReloadFile();
static void MatchParens(), FindMatch();
static void ForwardLine(), BackwardLine(), DelSelOrPrevChar(), InsertOrExpandTab();
static void SetMark(), HighlightRegion(), KillRegion(), CopyRegion(), YankRegion();
static void KeyMap();

static void TextSetTabs();

static void FwdBwdLineHook();

static char matchingPairs[] = { '(', ')', '{', '}', '[', ']', '\0', '\0'};

static char searchTranslations[] = 
  "~Shift<Key>Return:      DoReSearch(Popdown) \n\
   Shift<Key>Return:       DoReSearch() SetField(Replace) \n\
   Ctrl<Key>q,<Key>Tab:    insert-char()    \n\
   Ctrl<Key>c:             PopdownSearchAction() \n\
   <Btn1Down>:             select-start() SetField(Search) \n\
   <Key>Tab:               DoReSearch() SetField(Replace)";

static char replaceTranslations[] = 
  "~Shift<Key>Return:      DoReReplace(Popdown) \n\
   Shift<Key>Return:       SetField(Search) \n\
   Ctrl<Key>q,<Key>Tab:    insert-char()     \n\
   Ctrl<Key>c:             PopdownSearchAction() \n\
   <Btn1Down>:             select-start() DoReSearch() SetField(Replace)\n\
   <Key>Tab:               SetField(Search)";

static char translations[] = "<Key>Tab:insert-or-expand-tab()";

static XtActionsRec actions[] = {
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
    "insert-char",               InsertCharAction, /* Text Widget */
    "search",                    SearchAction,     /*  Overrides  */
#endif
    "goto-line",                 GotoLine,
    "search-line",               GotoLine,    /* For backwards compatability */
    "search-caret",              SearchCaret,
    "forward-search-selection",  FwdSearchSelection,
    "backward-search-selection", BwdSearchSelection,
    "re-search",                 ReSearch,
    "DoReSearch",                DoReSearch,
    "DoReReplace",               DoReReplace,

    "include-file",              IncludeFile,
    "include-selection",         IncludeSelection,
    "paste",                     Paste,
    "insert-control",            InsertControl,

    "delete-word",               DeleteWord,
    "delete-line",               DeleteLine,

    "where",                     Where,
    "size",                      Size,
    "centre-line",               CentreLine,
    "set-preferences",           SetPreferences,

    "clear-buffer",              ClearBuffer,

    "save-file",                 SaveFile,
    "save-as",                   SaveAs,
    "revert-file",               ReloadFile,  /* For backwards compatability */
    "load-file",                 LoadFile,
    "reload-file",               ReloadFile,  /* In place of revert-file */

    "match-parens",              MatchParens,
    "find-match",                FindMatch,
    "forward-line",              ForwardLine,
    "backward-line",             BackwardLine,
    "delete-selection-or-previous-character", DelSelOrPrevChar,
    "insert-or-expand-tab",      InsertOrExpandTab,

    "set-mark",                  SetMark,
    "highlight-region",          HighlightRegion,
    "kill-region",               KillRegion,
    "copy-region",               CopyRegion,
    "yank-region",               YankRegion,

    "keymap",                    KeyMap,
    
    "undo",                      UndoAction,
};

static void ClassInitialize(), Initialize(), Destroy(), Resize();
static Boolean  SetValues();

AxeiiTextClassRec axeiiTextClassRec = {
    /* Core class part */
  {
    /* superclass	     */	(WidgetClass) &asciiTextClassRec,
    /* class_name	     */ "AxeiiText",
    /* widget_size	     */ sizeof(AxeiiTextRec),
    /* class_initialize      */ ClassInitialize,
    /* class_part_initialize */ NULL,
    /* class_inited          */	FALSE,
    /* initialize	     */	Initialize,
    /* initialize_hook       */	NULL,
    /* realize		     */	XtInheritRealize,
    /* actions		     */ actions,
    /* num_actions	     */	XtNumber(actions),
    /* resources	     */	resources,
    /* num_resources	     */	XtNumber(resources),
    /* xrm_class	     */	NULLQUARK,
    /* compress_motion	     */	TRUE,
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
    /* compress_exposure     */	XtExposeGraphicsExpose | XtExposeNoExpose,
#else
    /* compress_exposure     */	XtExposeGraphicsExpose,
#endif
    /* compress_enterleave   */	TRUE,
    /* visible_interest	     */	FALSE,
    /* destroy		     */	Destroy,
    /* resize		     */	Resize,
    /* expose		     */	XtInheritExpose,
    /* set_values	     */	SetValues,
    /* set_values_hook       */	NULL,			
    /* set_values_almost     */	XtInheritSetValuesAlmost,  
    /* get_values_hook       */	NULL,
    /* accept_focus	     */	XtInheritAcceptFocus,
    /* version		     */	XtVersion,
    /* callback offsets      */	NULL,
    /* tm_table              */	XtInheritTranslations,
    /* query_geometry	     */	XtInheritQueryGeometry,
    /* display_accelerator   */	XtInheritDisplayAccelerator,
    /* extension	     */	NULL,
  },
   { /* Simple fields */
    /* change_sensitive */      XtInheritChangeSensitive
  },
  { /* text fields */
    /* empty            */      0
  },
  { /* ascii fields */
    /* empty            */      0
  },
  { /* axeii fields */
    /* extension        */	NULL
  }
};

WidgetClass axeiiTextWidgetClass = (WidgetClass) &axeiiTextClassRec;


static void
ClassInitialize()
{
    XtActionList actionList;
    Cardinal action, numActions;

    XtAddConverter(XtRString, XtRLong, XmuCvtStringToLong, NULL, 0);

    CLASS(translations) = XtParseTranslationTable(translations);
    CLASS(search_translations) = XtParseTranslationTable(searchTranslations);
    CLASS(replace_translations) = XtParseTranslationTable(replaceTranslations);

    CLASS(preInsertUndo) = InsertUndo;
    CLASS(preReplaceUndo) = ReplaceUndo;
    CLASS(postLoadUndo) = ResetUndo;

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
    XtGetActionList(textWidgetClass, &actionList, &numActions);
    for (action = 0;  action < numActions;  ++action)
    {
	if (strcmp(actionList[action].string, "insert-char") == 0)
	{
	    CLASS(superInsertChar) = actionList[action].proc;
	}
	else if (strcmp(actionList[action].string, "search") == 0)
	{
	    CLASS(superSearch) = actionList[action].proc;
	}
    }
    XtFree((char *) actionList);
#endif
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) new;
    Dimension width, height;

    atw->axeii.action_hook = (XtActionHookId) 0;
    atw->axeii.blink_timer = (XtIntervalId) 0;
    atw->axeii.watching_changes = False;
    atw->axeii.mark = (XawTextPosition) 0;

    AxeiiTextRowsColumnsToWidthHeight(atw->axeii.rows, atw->axeii.columns, atw,
				      &width, &height);

    atw->core.width = width;
    atw->core.height = height;

    if (atw->axeii.caret_bitmap)
    {
	XtVaSetValues(new,
		      XtNtextSink,
		         XtVaCreateWidget("axeiisink",
				      axeiiSinkObjectClass,
				      new,
				      XtNcursorBitmap, atw->axeii.caret_bitmap,
				      NULL),
		      NULL);
    }
    else if (atw->axeii.block_caret)
    {
	XtVaSetValues(new,
		      XtNtextSink, XtVaCreateWidget("axeiisink",
						    axeiiSinkObjectClass, new,
						    XtNblockCursor, True,
						    NULL),
		      NULL);
    }

    if (atw->axeii.tab_every)
    {
	TextSetTabs(atw, atw->axeii.tab_every);
    }
    else
    {
	atw->axeii.tab_every = 8;
    }

    XtOverrideTranslations(new, CLASS(translations));

    atw->axeii.translations = atw->core.tm.translations;

    atw->axeii.search = (Widget) 0;
    atw->axeii.label1 = (Widget) 0;
    atw->axeii.compexp = (regexp *) 0;
    atw->axeii.matchedText = (String) 0;
    atw->axeii.matchBegin = 1;  /* Different. So that AxeiiTextReSearch */
    atw->axeii.matchEnd = 2;    /* search for "^" works at beginning of */
				/* file */

    if (atw->axeii.undo)
    {
	InitialiseUndo(atw);
	atw->axeii.undo_hook
	    = XtAppAddActionHook(XtWidgetToApplicationContext(new), UndoHook,
			                                      (XtPointer) new);
	atw->axeii.lastAction = 0;
	atw->axeii.insertFileCallbacks = 0;
	atw->axeii.searchOneCallbacks = 0;
	atw->axeii.searchAllCallbacks = 0;
    }	

    /* Cause scrollbar lengths to be recomputed for new core width & height */
    (*((AsciiTextWidgetClass)
       (axeiiTextWidgetClass->core_class.superclass))->core_class.resize)(new);
}
    
static void Destroy(w)
    Widget w;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) w;

    if (atw->axeii.action_hook)
    {
	XtRemoveActionHook(atw->axeii.action_hook);
    }

    if (atw->axeii.blink_timer)
    {
	XtRemoveTimeOut(atw->axeii.blink_timer);
    }

    if (atw->axeii.label1)
    {
	XtFree((char*) atw->axeii.searchCallbacks);
	XtFree((char*) atw->axeii.replaceOneCallbacks);
	XtFree((char*) atw->axeii.replaceAllCallbacks);
	XtFree((char*) atw->axeii.compexp);
	XtDestroyWidget(atw->axeii.searchTranslations);
	XtDestroyWidget(atw->axeii.replaceTranslations);
	XtFree(atw->axeii.matchedText);
    }

    if (atw->axeii.undo)
    {
	XtRemoveActionHook(atw->axeii.undo_hook);
	FreeUndo(atw, True);
	XtFree((char *) atw->axeii.insertFileCallbacks);
	XtFree((char *) atw->axeii.searchOneCallbacks);
	XtFree((char *) atw->axeii.searchAllCallbacks);
    }	
}

static void
Resize(w)
     Widget w;
{
    /*
     * Too bad if not resizing in character increments
     */
    AxeiiTextWidget atw = (AxeiiTextWidget) w;
    int rows, columns;

    (*((AsciiTextWidgetClass)
       (axeiiTextWidgetClass->core_class.superclass))->core_class.resize) (w);

    AxeiiTextWidthHeightToRowsColumns(w, &rows, &columns);

    atw->axeii.rows = rows;
    atw->axeii.columns = columns;
}

/* ARGSUSED */
static Boolean
SetValues(old, request, new, args, num_args)
     Widget old, request, new;
     ArgList args;
     Cardinal *num_args;
{
    AxeiiTextWidget oldatw = (AxeiiTextWidget) old;
    AxeiiTextWidget newatw = (AxeiiTextWidget) new;
    Dimension width, height;
    int i, rows, columns;
    Boolean redisplay = False;

    for (i = 0;  i < *num_args;  ++i)
    {
	if (strcmp(args[i].name,"font") == 0)
	{
	    redisplay = True;
	}
    }

#define NE(field) (oldatw->field != newatw->field)

    if (NE(axeii.rows) || NE(axeii.columns) || redisplay)
    {
	AxeiiTextRowsColumnsToWidthHeight(newatw->axeii.rows,
					  newatw->axeii.columns,
					  newatw,
					  &width, &height);
	newatw->core.width = width;
	newatw->core.height = height;

	redisplay = True;
    }
    else if (NE(core.width) || NE(core.height))
    {
	AxeiiTextWidthHeightToRowsColumns(newatw, &rows, &columns);

	newatw->axeii.rows = rows;
	newatw->axeii.columns = columns;

	redisplay = True;
    }

    if (NE(axeii.tab_every))
    {
	TextSetTabs(newatw, newatw->axeii.tab_every);
	redisplay = True;
    }

    if (NE(axeii.watching_changes))
    {
	if (PRIVATE(new,watching_changes))
	{
	    AxeiiTextWatchForChanges(new);
	}
	else
	{
	    XtRemoveAllCallbacks(XawTextGetSource(new), XtNcallback);
	}
    }

    if (NE(text.scroll_vert))
    {
	AxeiiTextRowsColumnsToWidthHeight(newatw->axeii.rows,
					  newatw->axeii.columns,
					  newatw,
					  &width, &height);
	newatw->core.width = width;
	newatw->core.height = height;

	redisplay = True;
    }

#undef NE

    return redisplay;
}

/*************************************************************
 *
 *                         utilities
 *
 *************************************************************/

static void
GetLineInfo(widget, bol, insert, eol)
     Widget widget;
     XawTextPosition *bol, *insert, *eol;
{
    Widget textSrc;
    XawTextPosition target;

    textSrc = XawTextGetSource(widget);

    target = XawTextGetInsertionPoint(widget);

    *bol = XawTextSourceScan(textSrc, target, XawstEOL, XawsdLeft, 1, False);
    *insert = target;
    *eol = XawTextSourceScan(textSrc, target, XawstEOL, XawsdRight, 1, True);
}

static void
TextSetTabs(widget, tabevery)
     Widget widget;
     int tabevery;
{
    int columns, rows;
    Widget sink;
    int tabCount, *tabStops, tabPos;

    AxeiiTextWidthHeightToRowsColumns(widget, &rows, &columns);
    XtVaGetValues(widget,
		  XtNtextSink, &sink,
		  NULL);

    tabCount = columns / tabevery;
    tabStops = (int *) XtMalloc(tabCount * sizeof(int));
    for (tabPos = 1;  tabPos <= tabCount;  ++tabPos)
    {
	tabStops[tabPos - 1] = tabPos * tabevery;
    }
    XawTextSinkSetTabs(sink, tabCount, tabStops);
    XtFree((char *) tabStops);
}

/*ARGSUSED*/
static void 
blinkOff(client_data, id)
     XtPointer client_data;
     XtIntervalId *id;
{
    Widget widget = (Widget) client_data;

    XawTextUnsetSelection(widget);
    PRIVATE(widget,blink_timer) = 0;
}

/*ARGSUSED*/
static void
AsciiSourceChanged(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    Widget atw = XtParent(w);

    XtRemoveCallback(w, XtNcallback, AsciiSourceChanged, (XtPointer) CLOSURE);
    PRIVATE(atw,watching_changes) = False;

    XtCallCallbackList(atw, PRIVATE(atw,modified_callbacks), (XtPointer) 0);
}

/*************************************************************
 *
 *                           search
 *
 *************************************************************/

/*ARGSUSED*/
static void 
SearchAction(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4

    /*
     * Regular expression searching and undo modify the behaviour
     * of the search popup. Restoration to the original state is
     * expected to be carried out on popping down, so ensure that
     * gets done before coming up in the new state.
     */
    if (PRIVATE(widget,search))
    {
	XtPopdown(PRIVATE(widget,search));
    }

    (*CLASS(superSearch))(widget, event, params, num_params);

    if (!PRIVATE(widget,search))
    {
	PRIVATE(widget,search) = XtNameToWidget(widget, ".search");
    }

#endif
}

/*************************************************************
 *
 *                          re-search
 *
 *************************************************************/

/*ARGSUSED*/
static void
NewRegexp(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget text = (Widget) client_data;

    XtFree((char *) PRIVATE(text,compexp));
    PRIVATE(text,compexp) = (regexp *) 0;
    XtRemoveAllCallbacks(widget, XtNcallback);
}

/*ARGSUSED*/
static void
PopdownSearch(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    AxeiiTextWidget axeii = (AxeiiTextWidget) client_data;
    XtTranslations translations;

    XtVaSetValues(PRIVATE(axeii,search), "title", "search", NULL);
    XtRemoveCallback(widget, XtNpopdownCallback, PopdownSearch, client_data);

    
    XtVaGetValues(PRIVATE(axeii,searchTranslations),
		  XtNtranslations, &translations,
		  NULL);
    XtVaSetValues(PRIVATE(axeii,searchText),
		  XtNtranslations, translations,
		  NULL);

    XtVaGetValues(PRIVATE(axeii,replaceTranslations),
		  XtNtranslations, &translations,
		  NULL);
    XtVaSetValues(PRIVATE(axeii,replaceText),
		  XtNtranslations, translations,
		  NULL);

    XtRemoveAllCallbacks(PRIVATE(axeii,searchOne), XtNcallback);
    XtAddCallbacks(PRIVATE(axeii,searchOne),
		   XtNcallback, PRIVATE(axeii,searchCallbacks));

    XtRemoveAllCallbacks(PRIVATE(axeii,replaceOne), XtNcallback);
    XtAddCallbacks(PRIVATE(axeii,replaceOne),
		   XtNcallback, PRIVATE(axeii,replaceOneCallbacks));

    XtRemoveAllCallbacks(PRIVATE(axeii,replaceAll), XtNcallback);
    XtAddCallbacks(PRIVATE(axeii,replaceAll),
		   XtNcallback, PRIVATE(axeii,replaceAllCallbacks));

    NewRegexp(XawTextGetSource(PRIVATE(axeii,searchText)), (XtPointer) axeii);
}

/*ARGSUSED*/
static void
ReSearch(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    XtTranslations translations;
    XtCallbackList list;
    char buf[BUFSIZ];

    if ( (*num_params < 1) || (*num_params > 2) )
    {
	sprintf(buf, "AxeiiText Widget - re-search: %s\n%s", 
		"This action must have only", "one or two parameters");
	XtAppWarning(XtWidgetToApplicationContext(widget), buf);
	return;
    }

    XtCallActionProc(widget, "search", event, params, *num_params);

    if (!PRIVATE(widget,label1))
    {
	PRIVATE(widget,searchTranslations) =
	    XtVaCreateWidget("tmpSearch", coreWidgetClass, widget, NULL);
	PRIVATE(widget,replaceTranslations) =
	    XtVaCreateWidget("tmpReplace", coreWidgetClass, widget, NULL);

	PRIVATE(widget,label1)
	               = XtNameToWidget(PRIVATE(widget,search), "*label1");
	PRIVATE(widget,label2)
	               = XtNameToWidget(PRIVATE(widget,search), "*label2");
	PRIVATE(widget,forwards)
	               = XtNameToWidget(PRIVATE(widget,search), "*forwards");
	PRIVATE(widget,searchText)
	               = XtNameToWidget(PRIVATE(widget,search), "*searchText");
	PRIVATE(widget,replaceText)
	              = XtNameToWidget(PRIVATE(widget,search), "*replaceText");
	PRIVATE(widget,searchOne)
	              = XtNameToWidget(PRIVATE(widget,search), "*search");
	PRIVATE(widget,replaceOne)
	              = XtNameToWidget(PRIVATE(widget,search), "*replaceOne");
	PRIVATE(widget,replaceAll)
	              = XtNameToWidget(PRIVATE(widget,search), "*replaceAll");

	XtVaGetValues(PRIVATE(widget,searchText),
		      XtNtranslations, &translations,
		      NULL);
	XtVaSetValues(PRIVATE(widget,searchTranslations),
		      XtNtranslations, translations,
		      NULL);

	XtVaGetValues(PRIVATE(widget,replaceText),
		      XtNtranslations, &translations,
		      NULL);
	XtVaSetValues(PRIVATE(widget,replaceTranslations),
		      XtNtranslations, translations,
		      NULL);

        XtVaGetValues(PRIVATE(widget,searchOne),
		      XtNcallback, &list,
		      NULL);
        CopyCallbackList(list, &PRIVATE(widget,searchCallbacks));

        XtVaGetValues(PRIVATE(widget,replaceOne),
		      XtNcallback, &list,
		      NULL);
	CopyCallbackList(list, &PRIVATE(widget,replaceOneCallbacks));

        XtVaGetValues(PRIVATE(widget,replaceAll),
		      XtNcallback, &list,
		      NULL);
	CopyCallbackList(list, &PRIVATE(widget,replaceAllCallbacks));
    }

    XtVaSetValues(PRIVATE(widget,search), "title", "RE search", NULL);
    XtAddCallback(PRIVATE(widget,search), XtNpopdownCallback, PopdownSearch,
		                                           (XtPointer) widget);
    XtOverrideTranslations(PRIVATE(widget,searchText),
			                           CLASS(search_translations));
    XtAddCallback(XawTextGetSource(PRIVATE(widget,searchText)),
		                   XtNcallback, NewRegexp, (XtPointer) widget);

    XtOverrideTranslations(PRIVATE(widget,replaceText),
			                          CLASS(replace_translations));

    XtRemoveAllCallbacks(PRIVATE(atw,searchOne), XtNcallback);
    XtAddCallback(PRIVATE(widget,searchOne),
		                      XtNcallback, Search, (XtPointer) widget);

    XtRemoveAllCallbacks(PRIVATE(atw,replaceOne), XtNcallback);
    XtAddCallback(PRIVATE(widget,replaceOne),
		                  XtNcallback, ReplaceOne, (XtPointer) widget);

    XtRemoveAllCallbacks(PRIVATE(atw,replaceAll), XtNcallback);
    XtAddCallback(PRIVATE(widget,replaceAll),
		                  XtNcallback, ReplaceAll, (XtPointer) widget);
}

static void
SetSearchLabels(widget, message1, message2, bell)
     Widget widget;
     String message1, message2;
     Boolean bell;
{
    XtVaSetValues(PRIVATE(widget,label1), XtNlabel, message1, NULL);
    XtVaSetValues(PRIVATE(widget,label2), XtNlabel, message2, NULL);

    if (bell)
    {
	XBell(XtDisplay(widget), 0);
    }
}

static Boolean
PerformSearch(widget)
     Widget widget;
{
    String regexpr;
    Boolean forward;

    XtVaGetValues(PRIVATE(widget,searchText),
		  XtNstring, &regexpr,
		  NULL);

    if (!PRIVATE(widget,compexp))
    {
        if (!(PRIVATE(widget,compexp) = regcomp(regexpr)))
	{
	    return False;
	}

	XtAddCallback(XawTextGetSource(PRIVATE(widget,searchText)),
		                   XtNcallback, NewRegexp, (XtPointer) widget);
    }

    /*
     * The next line depends on implementation in TextPop.c.
     */
    forward = (((XawTextScanDirection) ((int) XawToggleGetCurrent(PRIVATE(widget,forwards)) - R_OFFSET)) == XawsdRight);

    return AxeiiTextReSearch(widget, forward, regexpr,
			     PRIVATE(widget,compexp),
			     &PRIVATE(widget,matchedText),
			     &PRIVATE(widget,matchBegin),
			     &PRIVATE(widget,matchEnd));
}

/*ARGSUSED*/
static void
DoReSearch(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeii;
    Boolean popdown = False, found;

    for (axeii = XtParent(widget);
	 !XtIsSubclass(axeii, axeiiTextWidgetClass); 
	 axeii = XtParent(axeii)) {}

    if ((*num_params == 1) && ((params[0][0] == 'p') || (params[0][0] == 'P')))
    {
	popdown = True;
    }
    
    SetSearchLabels(axeii, "", "", False);

    found = PerformSearch(axeii);

    if (RegError)
    {
	SetSearchLabels(axeii, "Regexp error:", RegError, True);
	RegError = 0;
    }
    else if (found && popdown)
    {
	XtCallActionProc(widget,
			 "PopdownSearchAction", event, params, *num_params);
    }
    else if (!found)
    {
	SetSearchLabels(axeii, "Could not find search string.", "", True);
    }
}

/*ARGSUSED*/
static void
Search(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    AxeiiTextWidget axeii = (AxeiiTextWidget) client_data;
    Boolean found;

    SetSearchLabels(axeii, "", "", False);

    found = PerformSearch(axeii);

    if (RegError)
    {
	SetSearchLabels(axeii, "Regexp error:", RegError, True);
	RegError = 0;
    }
    else if (!found)
    {
	SetSearchLabels(axeii, "Could not find search string.", "", True);
    }
}

static Boolean
PerformReplace(widget, once)
     Widget widget;
     Boolean once;
{
    XawTextPosition begin, end;
    XawTextBlock block;
    String replacement, regexpr;
    Boolean success = True;
    UndoRecord *record;

    if (!PRIVATE(widget,matchedText))
    {
	return False;
    }

    for (;;)
    {
	XawTextGetSelectionPos(widget, &begin, &end);

	if ((begin != PRIVATE(widget,matchBegin) ||
	    end != PRIVATE(widget,matchEnd)) && begin != end)
	{
	    SetSearchLabels(widget,
			  "Selection has been modified; aborting.", "", True);
	    return False;
	}

	XtVaGetValues(PRIVATE(widget,replaceText),
		                               XtNstring, &replacement, NULL);
	block.firstPos = 0;
	block.length = strlen(replacement);
	block.ptr = replacement;
	block.format = FMT8BIT;

	if (PRIVATE(widget,undo))
	{
	    record = LinkNewUndoRecord(widget);
	    record->begin = PRIVATE(widget,matchBegin);
	    record->end = PRIVATE(widget,matchEnd);
	    record->undoType = REPLACE;
	    record->data = AxeiiTextRead(widget,
					 PRIVATE(widget,matchBegin),
					 PRIVATE(widget,matchEnd));
	}

	if (XawTextReplace(widget,
			   PRIVATE(widget,matchBegin),
			   PRIVATE(widget,matchEnd), &block) != XawEditDone)
	{
	    SetSearchLabels(widget, "*** Error while replacing.", "", True);
	    success = False;

	    if (PRIVATE(widget,undo))
	    {
		UnlinkUndoRecord(widget, record);
	    }

	    break;
	}
	else
	{
	    XawTextPosition end = PRIVATE(widget,matchBegin) + block.length;
	    XawTextSetInsertionPoint(widget, end);

	    if (PRIVATE(widget,undo))
	    {
		record->end = end;
	    }
	}
	
	/*
	 * An automatic search after a single replace is not performed 
	 * prior to X11R5 patchlevel 20 since it results in the next
	 * match not being selected, and hence not replaceable. The
	 * standard search popup suffers similarly. 
	 */
#ifndef HAVEX11R5PL20	
	if (once)
	{
	    break;
	}
#endif

	/*
	 * If the regular expression is plain ^ or $ we need to take 
	 * steps to force the next match to occur on the next line, 
	 * otherwise we get into an infinite loop on the current line
	 */
        XtVaGetValues(PRIVATE(widget,searchText),
                      XtNstring, &regexpr,
                      NULL);

	if (regexpr[0] == '^' || regexpr[strlen(regexpr) - 1] == '$')
	{
	    Boolean forward = (((XawTextScanDirection) ((int) XawToggleGetCurrent(PRIVATE(widget,forwards)) - R_OFFSET)) == XawsdRight);
	    XawTextPosition before, after;

	    before = XawTextGetInsertionPoint(widget);

	    if (forward)
	    {
		after = XawTextSourceScan(XawTextGetSource(widget),
				        before, XawstEOL, XawsdRight, 1, True);
	    }
	    else
	    {
		after = XawTextSourceScan(XawTextGetSource(widget),
				        before, XawstEOL, XawsdLeft, 1, True);
	    }

	    /* Attempted to pass beyond beginning or end of buffer? */
	    if (after == 0 ||
		after == XawTextSourceScan(XawTextGetSource(widget), after,
				          XawstPositions, XawsdRight, 1, True))
	    {
		XtFree(PRIVATE(widget,matchedText)); /* Kludge to stop  */
		PRIVATE(widget,matchedText) = 0;     /* single replaces */
		PRIVATE(widget,matchBegin) = 1;      /* beyond ends     */
		PRIVATE(widget,matchEnd) = 2;        /* doing anything  */
		break;
	    }
	    else
	    {
		XawTextSetInsertionPoint(widget, after);
	    }
	}

	/* End of special processing for ^ and $ */

	if (!PerformSearch(widget))
	{
	    break;
	}
#ifdef HAVEX11R5PL20
	if (once)
	{
	    break;
	}
#endif
    }
    return once ? success : False;
}
     
/*ARGSUSED*/
static void
DoReReplace(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeii;
    Boolean popdown;

    for (axeii = XtParent(widget);
	 !XtIsSubclass(axeii, axeiiTextWidgetClass); 
	 axeii = XtParent(axeii)) {}

    if ((*num_params == 1) && ((params[0][0] == 'p') || (params[0][0] == 'P')))
    {
	popdown = True;
    }
    
    if (PerformReplace(axeii, True) && popdown)
    {
	XtCallActionProc(widget,
			 "PopdownSearchAction", event, params, *num_params);
    }
}

/*ARGSUSED*/
static void
ReplaceOne(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    AxeiiTextWidget axeii = (AxeiiTextWidget) client_data;

    (void) PerformReplace(axeii, True);
}

/*ARGSUSED*/
static void
ReplaceAll(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    AxeiiTextWidget axeii = (AxeiiTextWidget) client_data;

    (void) PerformReplace(axeii, False);
}

/*************************************************************
 *
 *             forward/backward-search-selection
 *
 *************************************************************/

/*ARGSUSED*/
static void
DeliverSelection(w, client_data, selection, type, value, length, format)
     Widget w;
     XtPointer client_data;
     Atom *selection, *type;
     XtPointer value;
     unsigned long *length;
     int *format;
{
    XawTextBlock textBlock;
    XawTextPosition position;
    XawTextScanDirection direction = (XawTextScanDirection) client_data;

    textBlock.firstPos = 0;
    textBlock.length = (int) *length;
    textBlock.ptr = (char *) value;
    textBlock.format = FMT8BIT;

    if ((position = XawTextSearch(w, direction, &textBlock))
	!= XawTextSearchError)
    {
	XawTextSetSelection(w, position,
			    (XawTextPosition) (position + textBlock.length));

	XawTextSetInsertionPoint(w, (direction == XawsdRight)
				 ? position + textBlock.length : position);
    }
    XtFree(value);
}

static void
SearchSelection(widget, direction)
     Widget widget;
     XawTextScanDirection direction;
{
    XtGetSelectionValue(widget, XA_PRIMARY, XA_STRING, DeliverSelection,
			(XtPointer) direction,
			XtLastTimestampProcessed(XtDisplay(widget)));
}

/*ARGSUSED*/
static void
FwdSearchSelection(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    SearchSelection(widget, XawsdRight);
}

/*ARGSUSED*/
static void
BwdSearchSelection(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    SearchSelection(widget, XawsdLeft);
}

static void 
WhereAmI(widget, line, position)
     Widget widget;
     int *line, *position;
{
    Widget textSrc, textSink;
    Position lmargin;
    XFontStruct *font;
    XawTextPosition caret, bol, eol, nexteol, garbage;
    int where = 1, howfar, distance, junk;

    XtVaGetValues(widget,
                  XtNtextSource, &textSrc,
                  XtNtextSink, &textSink,
                  XtNfont, &font,
                  XtNleftMargin, &lmargin,
                  NULL);


    caret = XawTextGetInsertionPoint(widget);
    eol = XawTextSourceScan(textSrc, 0, XawstEOL, XawsdRight, 1, False);
    nexteol = XawTextSourceScan(textSrc, eol + 1, XawstEOL,
                                XawsdRight, 1, False);
    while (eol < caret)
    {
        eol = nexteol;
        nexteol = XawTextSourceScan(textSrc, eol + 1, XawstEOL,
                                    XawsdRight, 1, False);
        ++where;
    }
    
    bol = XawTextSourceScan(textSrc, caret, XawstEOL, XawsdLeft, 1, False);
    XawTextSinkFindDistance(textSink, bol, (int) lmargin, caret,
                            &distance, &garbage, &junk);

    howfar = distance / font->max_bounds.width;

    *line = where;
    *position = howfar;
}

/*************************************************************
 *
 *                       insert-char
 *
 *************************************************************/

/*ARGSUSED*/
static void 
InsertCharAction(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4

    if (PRIVATE(widget,delete_on_insert))
    {
	XawTextPosition begin, end;
	XawTextEditType editType;
	XawTextBlock block;
	UndoRecord *tail = PRIVATE(widget,undo_tail);

	if (!IsModifierKey(XtGetActionKeysym(event, (Modifiers *) 0)))
        {
	    XawTextGetSelectionPos(widget, &begin, &end);

	    if (begin != end && !PRIVATE(widget,blink_timer))
	    {
		if (PRIVATE(widget,undo))
		{
		    /*
		     * Convert the DELETING undo record that has just been
		     * created to one of type REPLACE that will cause the
		     * character that is about to be inserted to be replaced
		     * by the selected text that is about to be deleted when
		     * undone. This implementation assumes that the
		     * character being inserted is the first of a sequence
		     * of insertions, the reasoning being that any preceding
		     * sequence will have been terminated by the setting of
		     * the selection.
		     */
		    XtVaGetValues(widget,
				  XtNeditType, &editType,
				  NULL);

		    if (editType != XawtextRead)
		    {
			tail->begin = begin;
			tail->end = begin + 1;
			tail->undoType = REPLACE;
			tail->data = AxeiiTextRead(widget, begin, end);
			tail->complete = 0;
		    }
		}
		
		block.firstPos = 0;
		block.length = 0;
		block.ptr = 0;
		block.format = FMT8BIT;
		(void) XawTextReplace(widget, begin, end, &block);
		XawTextSetInsertionPoint(widget, begin);
	    }
	}
    }
	
    (*CLASS(superInsertChar))(widget, event, params, num_params);

#endif
}

/*************************************************************
 *
 *                       goto-line
 *
 *************************************************************/

/*ARGSUSED*/
static void 
GotoLine(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget,
		       PRIVATE(widget,goto_line_callbacks), (XtPointer) 0);
}

/*************************************************************
 *
 *                       search-caret
 *
 *************************************************************/

/*ARGSUSED*/
static void 
SearchCaret(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XawTextPosition bol, insPos, eol;

    GetLineInfo(widget, &bol, &insPos, &eol);
    if (eol > bol)
    {
	--eol;
    }
    XawTextSetSelection(widget, bol, eol + 1);
    /* Force scrolling if necessary */
    XawTextSetInsertionPoint(widget, insPos);
}

/*************************************************************
 *
 *                        include-file
 *
 *************************************************************/

/*ARGSUSED*/
static void 
IncludeFile(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget, PRIVATE(widget,include_file_callbacks),
		                                                (XtPointer) 0);
}

/*************************************************************
 *
 *                       include-selection
 *
 *************************************************************/

/*ARGSUSED*/
static void 
IncludeSelection(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    String parm[2];
    Cardinal num_parm;

    parm[0] = "PRIMARY";
    parm[1] = (char *) 0;
    num_parm = 1;
    XtCallActionProc(widget, "insert-selection", event, parm, num_parm);
}

/*************************************************************
 *
 *                            paste
 *
 *************************************************************/

/*ARGSUSED*/
static void 
Paste(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    String parm[2];
    Cardinal num_parm;

    parm[0] = "CUT_BUFFER1";
    parm[1] = (char *) 0;
    num_parm = 1;
    XtCallActionProc(widget, "insert-selection", event, parm, num_parm);
}

/*************************************************************
 *
 *                        insert-control
 *
 *************************************************************/

/*ARGSUSED*/
static void 
InsertControl(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget, PRIVATE(widget,insert_control_callbacks),
		                                                (XtPointer) 0);
}

/*************************************************************
 *
 *                         delete-word
 *
 *************************************************************/

static void 
DeleteWord(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallActionProc(widget, "forward-word",     event, params, *num_params);
    XtCallActionProc(widget, "backward-word",    event, params, *num_params);
    XtCallActionProc(widget, "delete-next-word", event, params, *num_params);
}

/*************************************************************
 *
 *                        delete-line
 *
 *************************************************************/

/*ARGSUSED*/
static void 
DeleteLine(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallActionProc(widget, "beginning-of-line",  event, params, *num_params);
    XtCallActionProc(widget, "kill-to-end-of-line", event, params,*num_params);
}

/*************************************************************
 *
 *                          where
 *
 *************************************************************/

/*ARGSUSED*/
static void 
Where(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AxeiiTextWhereStruct where;

    WhereAmI(widget, &where.line, &where.position);

    XtCallCallbackList(widget,
		       PRIVATE(widget,where_callbacks), (XtPointer) &where);
}

/*************************************************************
 *
 *                          size
 *
 *************************************************************/

/*ARGSUSED*/
static void 
Size(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AxeiiTextSizeStruct size;

    size.rows = PRIVATE(widget,rows);
    size.columns = PRIVATE(widget,columns);

    XtCallCallbackList(widget,
		       PRIVATE(widget,size_callbacks), (XtPointer) &size);
}

/*************************************************************
 *
 *                       centre-line
 *
 *************************************************************/

/*ARGSUSED*/
static void
CentreLine(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    XawTextPosition bol, insPos, eol;
    String currentLine, cp, ep, newLine;
    int indent, i;
    XawTextBlock newText;

    GetLineInfo(widget, &bol, &insPos, &eol);
    if (eol > bol)
    {
	--eol;
    }
    currentLine = AxeiiTextRead(widget, bol, eol);

    for (cp = currentLine;  *cp != '\0' && (*cp == ' ' || *cp == '\t');  ++cp)
	;

    for (ep = cp + strlen(cp) - 1;
	 ep > cp && (*ep == ' ' || *ep == '\t');
	 --ep)
	;
    *++ep = '\0';
    
    if (strlen(cp) >= atw->axeii.columns)
    {
	indent = 0;
    }
    else
    {
	indent = (atw->axeii.columns - strlen(cp)) / 2;
    }

    newLine = (String) XtMalloc(indent + strlen(cp) + 1);
    for (i = 0;  i < indent;  ++i)
    {
	newLine[i] = ' ';
    }
    newLine[i] = '\0';
    strcat(newLine, cp);

    newText.firstPos = 0;
    newText.length = strlen(newLine);
    newText.ptr = newLine;
    newText.format = FMT8BIT;

    XawTextReplace(widget, bol, eol, &newText);
    eol = XawTextSourceScan(XawTextGetSource(widget), bol, XawstEOL,
			                                 XawsdRight, 1, False);
    XawTextSetInsertionPoint(widget, eol);

    XtFree(currentLine);
    XtFree(newLine);
}

/*************************************************************
 *
 *                       set-preferences
 *
 *************************************************************/

/*ARGSUSED*/
static void 
SetPreferences(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget,
		       PRIVATE(widget,preferences_callbacks), (XtPointer) 0);
}

/*************************************************************
 *
 *                         clear-text
 *
 *************************************************************/

/*ARGSUSED*/
static void 
ClearBuffer(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget,
		       PRIVATE(widget,clear_buffer_callbacks), (XtPointer) 0);
}

/*************************************************************
 *
 *                          save-file
 *
 *************************************************************/

/*ARGSUSED*/
static void 
SaveFile(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget,
		       PRIVATE(widget,save_file_callbacks), (XtPointer) 0);
}

/*************************************************************
 *
 *                           save-as
 *
 *************************************************************/

/*ARGSUSED*/
static void 
SaveAs(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget,
		       PRIVATE(widget,save_as_callbacks), (XtPointer) 0);
}

/*************************************************************
 *
 *                         reload-file
 *
 *************************************************************/

/*ARGSUSED*/
static void 
ReloadFile(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget, PRIVATE(widget,reload_file_callbacks),
		                                                (XtPointer) 0);
}

/*************************************************************
 *
 *                          load-file
 *
 *************************************************************/

/*ARGSUSED*/
static void 
LoadFile(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtCallCallbackList(widget, PRIVATE(widget,load_file_callbacks),
		                                                (XtPointer) 0);
}

/*************************************************************
 *
 *                       match-parens
 *
 *************************************************************/

/*ARGSUSED*/
static void 
MatchParens(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) w;
    Widget textSrc = XawTextGetSource(w);
    
    XawTextPosition top, from, open, close;
    XawTextBlock openPatt, closePatt;
    int level;
    char openParen, closeParen;
    
    if (event->type != KeyPress)
    {
	return;
    }
    
    XtCallActionProc(w,
		     "insert-char",
		     event,
		     (String *) 0, (Cardinal) 0);

    if (PRIVATE(atw,blink_period) == 0)
    {
	return;
    }
    
    (void) XLookupString((XKeyEvent *) event, &closeParen, 1,
			 (KeySym *) 0, (XComposeStatus *) 0);
    switch (closeParen)
    {
    case ')':
	openParen = '(';
	break;
    case '}':
	openParen = '{';
	break;
    case ']':
	openParen = '[';
	break;
    default:
	return;
    }

    openPatt.firstPos = closePatt.firstPos = 0;
    openPatt.length = closePatt.length = 1;
    openPatt.ptr = &openParen;
    closePatt.ptr = &closeParen;
    openPatt.format = closePatt.format = FMT8BIT;

    level = 0;
    top = XawTextTopPosition(w);
    from = XawTextGetInsertionPoint(w) - 1;
    for(;;)
    {
	open = XawTextSourceSearch(textSrc, from, XawsdLeft, &openPatt);
	close = XawTextSourceSearch(textSrc, from, XawsdLeft, &closePatt);
	if (open == XawTextSearchError)
	{
	    break;
	}
	else if (close > open)
	{
	    ++level;
	    if (close > top)
	    {
		from = close;
	    }
	    else
	    {
		break;
	    }
	}
	else if (level == 0 && open >= top)
	{
	    if (PRIVATE(atw,blink_timer))
	    {
		XtRemoveTimeOut(PRIVATE(atw,blink_timer));
	    }
	    XawTextSetSelection(w, open, open + 1);
	    PRIVATE(atw,blink_timer)
		= XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				  (unsigned long) PRIVATE(atw,blink_period),
				  blinkOff,
				  (XtPointer) w);
	    break;
	}
	else
	{
	    --level;
	    if (open > top)
	    {
		from = open;
	    }
	    else
	    {
		break;
	    }
	}
    }
}

/*************************************************************
 *
 *                       find-match
 *
 *************************************************************/

/*ARGSUSED*/
static void
FindMatch(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget asciiSrc = XawTextGetSource(w);
    XawTextPosition insPos = XawTextGetInsertionPoint(w), from, found, open, close;
    XawTextBlock openBrack, closeBrack;
    XawTextScanDirection direction;
    char opn = '\0', cls;
    int i, level;

    
    if (insPos > 0)
    {
	insPos -= 1;
    }
    (void) XawTextSourceRead(asciiSrc, insPos, &closeBrack, 1);
    
    cls = *closeBrack.ptr;
    for (i = 0;  matchingPairs[i];  i += 2)
    {
	if (cls == matchingPairs[i])
	{
	    direction = XawsdRight;
	    opn = matchingPairs[i + 1];
	    break;
	}
    }
    
    if (!opn)
    {
	for (i = 1;  matchingPairs[i];  i += 2)
	{
	    if (cls == matchingPairs[i])
	    {
		direction = XawsdLeft;
		opn = matchingPairs[i - 1];
		break;
	    }
	}
    }
    
    openBrack.firstPos = closeBrack.firstPos = 0;
    openBrack.length = closeBrack.length = 1;
    openBrack.ptr = &opn;
    closeBrack.ptr = &cls;
    
    found = XawTextSearchError;
    for (level = 1, from = (direction == XawsdLeft) ? insPos : insPos + 1;;)
    {
	if ((open = XawTextSourceSearch(asciiSrc, from, direction, &openBrack))
	    == XawTextSearchError)
	{
	    break;
	}

	if ((close
	     = XawTextSourceSearch(asciiSrc, from, direction,
				   &closeBrack)) != XawTextSearchError)
	{
	    if ( open > close)
	    {
		if (direction == XawsdLeft)
		{
		    --level;
		    from = open;
		}
		else
		{
		    ++level;
		    from = close + 1;
		}
	    }
	    else
	    {
		if (direction == XawsdLeft)
		{
		    ++level;
		    from = close;
		}
		else
		{
		    --level;
		    from = open + 1;
		}
	    }
	}
	else
	{
	    --level;
	    if (direction == XawsdLeft)
	    {
		from = open;
		}
	    else
	    {
		from = open + 1;
	    }
	}

	if (level == 0)
	{
	    found = from;
	    break;
	}
    }

    if (found != XawTextSearchError)
    {
        if (direction == XawsdLeft)
	{
	    ++insPos;
	}
	XawTextSetSelection(w, found <= insPos ? found : insPos,
			                      found > insPos ? found : insPos);
    }
}

/*************************************************************
 *
 *               forward-line & backward-line
 *
 *************************************************************/

static void
AddActionHook(w)
     Widget w;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) w;
    Widget source, sink;
    Position lmargin;
    int insertPosition, distance, morejunk;
    XawTextPosition bol, junk;

    if (!atw->axeii.action_hook)
    {
	atw->axeii.action_hook =
	    XtAppAddActionHook(XtWidgetToApplicationContext(w),
			       FwdBwdLineHook, (XtPointer) w);
	
	XtVaGetValues(w,
		      XtNtextSource, &source,
		      XtNtextSink, &sink,
		      XtNleftMargin, &lmargin,
		      XtNinsertPosition, &insertPosition,
		      NULL);

	bol =  XawTextSourceScan(source, insertPosition,
				 XawstEOL, XawsdLeft, 1, False);

	XawTextSinkFindDistance(sink, bol, (int) lmargin,
				(XawTextPosition) insertPosition,
				&distance, &junk, &morejunk);

	atw->axeii.fwd_bwd_indent = distance;
    }
}

static void
FwdBwdLine(w, dir)
     Widget w;
     XawTextScanDirection dir;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) w;
    Widget source, sink;
    int insertPosition, junk;
    Position lmargin;
    XawTextPosition bol, eol, position;

    XtVaGetValues(w,
		  XtNtextSource, &source,
		  XtNtextSink, &sink,
		  XtNleftMargin, &lmargin,
		  XtNinsertPosition, &insertPosition,
		  NULL);
    
    bol = XawTextSourceScan(source, insertPosition, XawstEOL, dir,
			    (dir == XawsdLeft) ? 2 : 1, (dir == XawsdRight));

    eol = XawTextSourceScan(source, bol, XawstEOL, XawsdRight, 1, False);

    XawTextSinkFindPosition(sink, bol, (int) lmargin,
			    atw->axeii.fwd_bwd_indent,
			    False, &position, &junk, &junk);

    if (eol > position)
    {
	XawTextSetInsertionPoint(w, position);
    }
    else
    {
	XawTextSetInsertionPoint(w, eol);
    }
}

/*ARGSUSED*/
static void
BackwardLine(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AddActionHook(w);
    FwdBwdLine(w, XawsdLeft);
}

/*ARGSUSED*/
static void
ForwardLine(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AddActionHook(w);
    FwdBwdLine(w, XawsdRight);
}

/*ARGSUSED*/
static void
FwdBwdLineHook(w, client_data, action_name, event, params, num_params)
     Widget w;
     XtPointer client_data;
     String action_name;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) w;

    if ((Widget) client_data != w)
    {
	return;
    }

    if (strcmp(action_name, "forward-line") != 0 &&
	strcmp(action_name, "backward-line") != 0)
    {
	XtRemoveActionHook(atw->axeii.action_hook);
	atw->axeii.action_hook = (XtActionHookId) 0;
	return;
    }
}

/*************************************************************
 *
 *          delete-selection-or-previous-character
 *
 *************************************************************/

/*ARGSUSED*/
static void
DelSelOrPrevChar(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XawTextPosition begin, end;

    XawTextGetSelectionPos(w, &begin, &end);

    if (begin != end && !PRIVATE(w,blink_timer))
    {
        XtCallActionProc(w,
                         "delete-selection",
                         event,
                         (String *) 0,
                         (Cardinal) 0);
    }
    else
    {
        XtCallActionProc(w,
                         "delete-previous-character",
                         event,
                         (String *) 0,
                         (Cardinal) 0);
    }
}

/*************************************************************
 *
 *                    insert-or-expand-tab
 *
 *************************************************************/

static void
InsertOrExpandTab(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XawTextPosition bol, before = XawTextGetInsertionPoint(w), junk;
    Position lmargin;
    Widget source, sink;
    XFontStruct *font;
    int bdistance, adistance, moreJunk;
    XawTextBlock block;
    static char spaces[] = "                                ";

    XtCallActionProc(w, "insert-char", event, params, *num_params);

    if (!PRIVATE(w,expand_tabs))
    {
	return;
    }

    XtVaGetValues(w,
		  XtNleftMargin, &lmargin,
		  XtNtextSource, &source,
		  XtNtextSink, &sink,
		  XtNfont, &font,
		  NULL);

    bol = XawTextSourceScan(source, before, XawstEOL, XawsdLeft, 1, False);
    XawTextSinkFindDistance(sink, bol, (int) lmargin, before, &bdistance,
			                                     &junk, &moreJunk);
    XawTextSinkFindDistance(sink, bol, (int) lmargin, before + 1, &adistance,
			                                     &junk, &moreJunk);
    block.firstPos = 0;
    block.length = (adistance - bdistance) / font->max_bounds.width;
    block.ptr = spaces;
    block.format = FMT8BIT;
    XawTextReplace(w, before, before + 1, &block);

    XawTextSetInsertionPoint(w, before + block.length);
}

/*************************************************************
 *
 *                          set-mark
 *
 *************************************************************/

/*ARGSUSED*/
static void
SetMark(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    PRIVATE(w,mark) = XawTextGetInsertionPoint(w);
}

/*************************************************************
 *
 *                      highlight-region
 *
 *************************************************************/

/*ARGSUSED*/
static void
HighlightRegion(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XawTextPosition from, to;
    
    AxeiiTextGetRegion(w, &from, &to);
    XawTextSetSelection(w, from, to);
}

/*************************************************************
 *
 *                        kill-region
 *
 *************************************************************/

/*ARGSUSED*/
static void
KillRegion(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XawTextPosition from, to;
    String region;
    XawTextBlock block;

    AxeiiTextGetRegion(w, &from, &to);
    region  = AxeiiTextRead(w, from, to);
    XStoreBuffer(XtDisplay(w), region, to - from, 2);
    
    block.firstPos = 0;
    block.length = 0;
    block.ptr = NULL;
    block.format = FMT8BIT;
    XawTextReplace(w, from, to, &block);

    XtFree(region);
}

/*************************************************************
 *
 *                        copy-region
 *
 *************************************************************/

/*ARGSUSED*/
static void
CopyRegion(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XawTextPosition from, to;
    String region;

    AxeiiTextGetRegion(w, &from, &to);
    region = AxeiiTextRead(w, from, to);
    XStoreBuffer(XtDisplay(w), region, to - from, 2);
    
    XtFree(region);
}

/*************************************************************
 *
 *                        yank-region
 *
 *************************************************************/

/*ARGSUSED*/
static void
YankRegion(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XawTextPosition pos = XawTextGetInsertionPoint(w);
    XawTextBlock block;

    block.firstPos = 0;
    block.ptr = XFetchBuffer(XtDisplay(w), &block.length, 2);
    block.format = FMT8BIT;
    XawTextReplace(w, pos, pos, &block);
    XawTextSetInsertionPoint(w, pos + block.length);
}

/*************************************************************
 *
 *                           keymap
 *
 *************************************************************/

/* ARGSUSED */
static void
KeyMap(widget, event, params, num_params)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    static XtResource key_resources[] = {
        {XtNtranslations, XtCTranslations, XtRTranslationTable,
	     sizeof(XtTranslations), 0, XtRTranslationTable, (XtPointer) 0}
    };
    char mapName[1000];
    char mapClass[1000];

    if (*num_params != 1)
    {
	return;
    }

    if (strcmp(params[0], "None") == 0)
    {
        XtOverrideTranslations(widget, PRIVATE(widget,translations));
        return;
    }

    (void) sprintf( mapName, "%sKeymap", params[0] );
    (void) strcpy( mapClass, mapName );
    if (islower(mapClass[0]))
    {
	mapClass[0] = toupper(mapClass[0]);
    }

    XtGetSubresources(widget, (XtPointer) &PRIVATE(widget,keymap),
		      mapName, mapClass,
		      key_resources, (Cardinal) 1, NULL, (Cardinal) 0);

    if (PRIVATE(widget,keymap))
    {
        XtOverrideTranslations(widget, PRIVATE(widget,keymap));
    }
}

/*************************************************************
 *
 *                    Semi-public functions
 *
 *************************************************************/

/*
 * CopyCallbackList is used by the undo module,
 * which is logically part of AxeiiText anyway.
 */
void
CopyCallbackList(src, dst)
     XtCallbackList src, *dst;
{
    int number;

    for (number = 0;  src[number].callback;  ++number);
    ++number;

    *dst =
	(XtCallbackList) XtMalloc((Cardinal) (number * sizeof(XtCallbackRec)));

    for (--number;  number >= 0;  --number)
    {
	(*dst)[number].callback = src[number].callback;
	(*dst)[number].closure = src[number].closure;
    }
}

/*
 * AxeiiTextReSearch isn't really public; it is like this so that it can
 * be called by AxeEditor. The String pointed to by match on entry should
 * either be NULL or freeable by XtFree. The returned string should either
 * be freed by the caller or passed in the next call.
 */
Boolean
AxeiiTextReSearch(widget, forward, regexpr, compexp, match, begin, end)
     Widget widget;
     Boolean forward;
     String regexpr;
     regexp *compexp;
     String *match;
     XawTextPosition *begin, *end;
{
    Widget textSrc;
    XawTextBlock block;
    XawTextPosition pos, lastPos, xoff, yoff; 
    XawTextPosition bol, eol, lastEol, swap, readfrom, toread;
    Boolean found;
    int length;
    String subText, startp, endp;

    if (!XtIsSubclass(widget, axeiiTextWidgetClass))
    {
	return False;
    }

    textSrc = XawTextGetSource(widget);
    found = False;
    pos = XawTextGetInsertionPoint(widget);

    if (regexpr[0] == '^')
    {
	bol = XawTextSourceScan(textSrc, pos, XawstEOL, XawsdLeft, 1, False);

	if (forward)
	{
	    /*
	     * Move to next line if not at beginning of a line or
	     * the last match matched the beginning of this line
	     */
	    if (pos != bol ||
		(pos == bol && PRIVATE(widget,matchBegin) == pos &&
			                      PRIVATE(widget,matchEnd) == pos))
	    {
		pos = XawTextSourceScan(textSrc, pos, XawstEOL, XawsdRight,
					                              1, True);
	    }

	    /*
	     * Don't go past last newline if nothing follows it
	     */
	    if (pos != XawTextSourceScan(textSrc, pos, XawstPositions,
					                  XawsdRight, 1, True))
	    {
		XawTextSetInsertionPoint(widget, pos);
	    }
	    else
	    {
		return False;
	    }
	}
	else /* backwards */
	{
	    /*
	     * Move to previous line if not at beginning of a line or
	     * the last match matched the beginning of this line or
	     * on empty line at end of file
	     */
	    if (pos == bol && (PRIVATE(widget,matchBegin) == pos &&
			                    PRIVATE(widget,matchEnd) == pos ||
		pos == XawTextSourceScan(textSrc, pos, XawstPositions,
					                 XawsdRight, 1, True)))
	    {
		pos = XawTextSourceScan(textSrc, pos, XawstEOL, XawsdLeft,
					                             2, False);
	    }
	    else
	    {
		pos = bol;
	    }

	    /*
	     * Beginning of first line matches unless previously matched
	     */
	    if (pos == 0 && PRIVATE(widget,matchBegin) == pos &&
			                       PRIVATE(widget,matchEnd) == pos)
	    {
		return False;
	    }
	    else
	    {
		XawTextSetInsertionPoint(widget, pos);
	    }
	}
    }
    else if (regexpr[strlen(regexpr) - 1] == '$')
    {
	eol = XawTextSourceScan(textSrc, pos, XawstEOL, XawsdRight, 1, False);

	if (forward)
	{
	    /*
	     * Move to next line if not at end of a line or
	     * the last match matched the end of this line
	     */
	    if (pos != eol)
	    {
		pos = eol;
	    }
	    else if (PRIVATE(widget,matchBegin) == pos &&
			                      PRIVATE(widget,matchEnd) == pos)
	    {
		XawTextPosition one, two;

		one = XawTextSourceScan(textSrc, pos, XawstPositions,
					                  XawsdRight, 1, True);
		two = XawTextSourceScan(textSrc, pos, XawstEOL, XawsdRight,
					                             2, False);
		/*
		 * Don't go past last newline if nothing follows it
		 */
		if (one != two)
		{
		    pos = two;
		    XawTextSetInsertionPoint(widget, pos);
		}
		else
		{
		    return False;
		}
	    }
	}
	else /* backwards */
	{
	    /*
	     * Move to previous line if not at end of a line or
	     * the last match matched the end of this line or
	     * at end of file beyond last newline
	     */
	    if (pos != eol ||
		((PRIVATE(widget,matchBegin) == pos &&
			                   PRIVATE(widget,matchEnd) == pos)) ||
		(pos == XawTextSourceScan(textSrc, pos, XawstPositions, 
					                XawsdRight, 1, True) &&
		 pos == XawTextSourceScan(textSrc, pos, XawstEOL, XawsdLeft,
					                            1, False)))
	    {
		pos = XawTextSourceScan(textSrc, pos, XawstEOL, XawsdLeft,
					                              1, True);
	    }

	    /*
	     * Trap attempts to go past beginning of file
	     */
	    if (pos > 0)
	    {
		XawTextSetInsertionPoint(widget, pos);
	    }
	    else
	    {
		return False;
	    }
	}
    }

    for (lastPos = pos - 1, eol = -1, lastEol = 0;
	 pos != lastPos && eol != lastEol; )
    {
        lastEol = eol;
        if (forward)
        {
            eol = XawTextSourceScan(textSrc, pos, XawstEOL,
                                                         XawsdRight, 1, False);
        }
        else
        {
            eol = XawTextSourceScan(textSrc, pos, XawstEOL,
                                                         XawsdLeft, 1, False);
            swap = pos; pos = eol; eol = swap;
        }

        length = (int) (eol - pos);
        block.format = FMT8BIT;
	XtFree(*match);
        *match = XtMalloc(length + 1);
        (*match)[0] = '\0';
        for(toread = length, readfrom = pos;
            toread > 0;
            toread = toread - block.length, readfrom = readfrom + block.length)         {
            (void) XawTextSourceRead(textSrc, readfrom, &block, toread);
            (void) strncat(*match, block.ptr, block.length);
        }

        if (regexec(compexp,*match))
        {
	    found = True;
            startp = compexp->startp[0];
            endp = compexp->endp[0];
            if (!forward && startp != endp)
            {
                for (subText = startp + 1;
                     regexec(compexp, subText);
                     subText = compexp->startp[0] + 1)
                {
                    if ((compexp->endp[0] - compexp->startp[0])
			                                     > (endp - startp))
                    {
                        startp = compexp->startp[0];
                        endp = compexp->endp[0];
                    }
                }

		if (RegError)
		{
		    break;
		}

                compexp->startp[0] = startp;
                compexp->endp[0] = endp;
            }

            xoff = (XawTextPosition) (compexp->startp[0] - *match);
            yoff = (XawTextPosition) (compexp->endp[0] - *match);

            XawTextSetSelection(widget, pos + xoff, pos + yoff);
	    *begin = pos + xoff;
	    *end = pos + yoff;
	    
            if (forward)
            {
                XawTextSetInsertionPoint(widget, pos + yoff);
            }
            else
            {
                XawTextSetInsertionPoint(widget, pos + xoff);
            }
            break;
        }

	if (RegError)
	{
	    break;
	}
	
        lastPos = pos;
        if (forward)
        {
            pos = eol + 1;
        }
        else
        {
            if (pos > 0)
            {
                pos -= 1;
            }
        }
    }

    if (!found)
    {
	*begin = *end = 0;
    }

    return found;
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

void 
AxeiiTextWidthHeightToRowsColumns(widget, rows, cols)
     Widget widget;
     int *rows, *cols;
{
    XFontStruct *font;
    Position lm, rm, tm, bm;
    Dimension width, height, caretHeight;
    Widget sink;
    XRectangle caret;

    XtVaGetValues(widget,
		  XtNfont, &font,
		  XtNwidth, &width,
		  XtNheight, &height,
		  XtNleftMargin, &lm,
		  XtNrightMargin, &rm,
		  XtNtopMargin, &tm,
		  XtNbottomMargin, &bm,
		  XtNtextSink, &sink,
		  NULL);

    XawTextSinkGetCursorBounds(sink, &caret);
    caretHeight = (Dimension) caret.height;

    *cols = (width - lm - rm) / font->max_bounds.width;
    *rows = (height - tm - bm - CARETHEIGHT) /
	    (font->max_bounds.ascent + font->max_bounds.descent);
}

void
AxeiiTextRowsColumnsToWidthHeight(rows, cols, widget, width, height)
     int rows, cols;
     Widget widget;
     Dimension *width, *height;
{
    XFontStruct *font;
    
    Dimension caretHeight;
    Position lm, rm, tm, bm;
    Widget sink;
    XRectangle caret;

    XtVaGetValues(widget,
		  XtNfont, &font,
		  XtNleftMargin, &lm,
		  XtNrightMargin, &rm,
		  XtNtopMargin, &tm,
		  XtNbottomMargin, &bm,
		  XtNtextSink, &sink,
		  NULL);

    XawTextSinkGetCursorBounds(sink, &caret);
    caretHeight = (Dimension) caret.height;

    *width = cols * font->max_bounds.width + lm + rm;
    *height = rows * (font->max_bounds.ascent +font->max_bounds.descent)
                   + tm + bm + CARETHEIGHT;
}

void
AxeiiTextWatchForChanges(atw)
     Widget atw;
{
    if (XtIsSubclass(atw, axeiiTextWidgetClass)
	                                     && !PRIVATE(atw,watching_changes))
    {
	XtAddCallback(XawTextGetSource(atw), XtNcallback,
		                      AsciiSourceChanged, (XtPointer) CLOSURE);

	PRIVATE(atw,watching_changes) = True;
    }
}

void
AxeiiTextGotoLine(atw, line)
     Widget atw;
     int line;
{
    XawTextPosition bol;

    if (!XtIsSubclass(atw, axeiiTextWidgetClass))
    {
	return;
    }

    bol = XawTextSourceScan(XawTextGetSource(atw), (XawTextPosition) 0,
			    XawstEOL,XawsdRight, line - 1, True);

    XawTextSetInsertionPoint(atw, bol);
}

String
AxeiiTextRead(text, from, to)
     Widget text;
     XawTextPosition from, to;
{
    int length = (int) (to - from), toread;
    XawTextBlock block;
    XawTextPosition readfrom;
    String theText = XtMalloc(length + 1);

    block.format = FMT8BIT;
    theText[0] = '\0';
    for(toread = length, readfrom = from;
        toread > 0;
        toread = toread - block.length, readfrom = readfrom + block.length) 
    {
        (void) XawTextSourceRead(XawTextGetSource(text), readfrom,
				 &block, toread);
        (void) strncat(theText, block.ptr, block.length);
    }

    return theText;
}

void
AxeiiTextGetRegion(widget, begin, end)
     Widget widget;
     XawTextPosition *begin, *end;
{
    XawTextPosition now = XawTextGetInsertionPoint(widget);

    *begin = PRIVATE(widget,mark) <= now ? PRIVATE(widget,mark) : now;
    *end = PRIVATE(widget,mark) > now ? PRIVATE(widget,mark) : now;
}

void (*
AxeiiTextUndoPreInsert(atw))()
     Widget atw;
{
    if (XtIsSubclass(atw, axeiiTextWidgetClass) && PRIVATE(atw,undo))
    {
	return CLASS(preInsertUndo);
    }

    return 0;
}

void (*
AxeiiTextUndoPreReplace(atw))()
     Widget atw;
{
    if (XtIsSubclass(atw, axeiiTextWidgetClass) && PRIVATE(atw,undo))
    {
	return CLASS(preReplaceUndo);
    }

    return 0;
}

void (*
AxeiiTextUndoPostLoad(atw))()
     Widget atw;
{
    if (XtIsSubclass(atw, axeiiTextWidgetClass) && PRIVATE(atw,undo))
    {
	return CLASS(postLoadUndo);
    }

    return 0;
}

#undef PRIVATE
#undef CLASS

