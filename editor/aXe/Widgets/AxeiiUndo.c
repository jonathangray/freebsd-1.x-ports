/*
 * Copyright 1993 The University of Newcastle upon Tyne
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

/* 
 *                   The implemenatation of Undo in aXe 
 * 
 * Undo in aXe is built upon the Action Hook mechanism. Advantage is taken of
 * the fact that a procedure that will be called before every action routine
 * is invoked can be specified. aXe tries to predict in the hook procedure
 * what the action will do and construct an undo record containing enough
 * information to enable the inverse operation to be carried out. If
 * sufficient information can't be obtained at that time a routine that will
 * complete the job is registered for calling just before the next action is
 * similarly treated.
 * 
 * The information enabling undos to be carried out is held on a per widget
 * basis in doubly linked lists of records having the following structure:-
 * 
 *                            +--------------+
 *                            |     prev     |
 *                            +--------------+
 *                            |    begin     |
 *                            +--------------+
 *                            |     end      |
 *                            +--------------+
 *                            |   undoType   |
 *                            +--------------+
 *                            |     data     |
 *                            +--------------+
 *                            |   complete   |
 *                            +--------------+
 *                            |     next     |
 *                            +--------------+
 * 
 * where
 * 
 * prev     - is a link to the previous record.
 * 
 * begin    - is the position at which the undo takes place.
 * 
 * end      - together with begin delimits a region of text involved in
 *            the undo.
 * 
 * undoType - specifies the type of undo operation, and is either INSERT,
 *            DELETE or REPLACE.
 * 
 * data     - is a pointer to an associated string.
 * 
 * complete - is a pointer to a routine that should be called to complete
 *            the gathering of information for the undo record.
 * 
 * next     - is a pointer to the next record.
 * 
 * Records are used in the following way for the three types of alteration:
 *   
 *   Insertion
 *         begin    - start of region to be deleted
 *         end      - end of region to be deleted
 *         undoType - DELETE
 *         data     - NULL
 *         complete - usually needed
 *   
 *   Deletion      
 *         begin    - point at which text should be inserted
 *         end      - undefined, probably 0
 *         undoType - INSERT
 *         data     - the text to be inserted
 *         complete - not usually needed
 *   
 *   Replacement
 *         begin    - start of region to be replaced
 *         end      - end of region to be replaced
 *         undoType - REPLACE
 *         data     - replacement text
 *         complete - may or may not be needed
 *
 * An empty chain is represented by a spare record that is pointed to by head
 * and tail pointers in the widget record. Two of its fields are used to
 * contain other information: prev is used to point to the record that would
 * be used if an undo were requested, and begin is used to record the length
 * of the chain. If prev points to the head record, as it does initially, and
 * an undo is requested then it is changed to point to the tail record before
 * embarking on the undo. After an undo it is made to point to the previous
 * record pointed to by the undo record just used.
 * 
 * New records are appended to the chain and undos are carried out by
 * traversing backwards from the tail. As each undo is performed a record to
 * undo the undo is constructed and appended. If sufficient consecutive undos
 * are performed to cause the head of the chain to be reached, wraparound to
 * the end of the now extended tail takes place, enabling the most recent
 * undos also to be undone. The process comes to a halt when the first
 * non-undo action is encountered, at which point, if appropriate, the chain
 * is reduced to its preferred length. That length is specified by the
 * resource undoLevel, a value of 0 for which implies unlimited undo
 * information being kept and the length of the chain never being cropped.
 * 
 * A single record is kept for a block of consecutively inserted characters up
 * to a line in length, rather than maintaining records for the individual
 * characters. Thus executing undo will delete the entire block, not the most
 * recently inserted character. That is best done by deleting the character.
 * Control codes entered via the control code selector popup are an exception
 * in that individual undo records are kept. Undo records are kept for deleted
 * characters.
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include "AxeiiTextP.h"
#include "AxeiiUndo.h"

#define HEAD(w)  ((w)->axeii.undo_head)
#define TAIL(w)  ((w)->axeii.undo_tail)
#define UNDO(w)  ((w)->axeii.undo_head->prev)
#define LEVEL(w) ((w)->axeii.undo_head->begin)

typedef struct {
    String action;
    void   (*proc)();
} ActionForUndo; 

static XrmQuark undoQuark;
static XrmQuark inscharQuark, newlineQuark, indentQuark, backupQuark;

static void NoOp();
static void InsertChar(), InsertText(), InsertFile();
static void TransposeChars(), ClearBuffer(), DeleteSelection();
static void DeleteAmount(), DeleteNextWord(), DeletePreviousWord();
static void DeleteEndLine(), DeleteEndParagraph(), DeleteSelOrChar();
static void DeletePrevChar(), DeleteNextChar();
static void CentreLine(), FormParagraph();
static void DeleteRegion();
static void Undoing();
static void RecordAmount(), RecordParagraph();
static void ReSearch(), SearchReplace();
static void AdjustChainLength();

static Boolean searchFromReSearch = False;

#ifdef DEBUG
static void PrintChain();
#endif

static ActionForUndo actionTable[] = {
    {"centre-line",                 CentreLine},
    {"clear-buffer",                ClearBuffer},
    {"undo",                        Undoing},
    {"kill-region",                 DeleteRegion},
    {"yank-region",                 InsertText},
    {"backward-kill-word",          DeletePreviousWord},
    {"delete-next-character",       DeleteNextChar},
    {"delete-next-word",            DeleteNextWord},
    {"delete-previous-character",   DeletePrevChar},
    {"delete-previous-word",        DeletePreviousWord},
    {"delete-selection",            DeleteSelection},
    {"form-paragraph",              FormParagraph},
    {"insert-char",                 InsertChar},
    {"insert-file",                 InsertFile},
    {"insert-selection",            InsertText},
    {"insert-string",               InsertText},
    {"kill-selection",              DeleteSelection},
    {"kill-to-end-of-line",         DeleteEndLine},
    {"kill-to-end-of-paragraph",    DeleteEndParagraph},
    {"kill-word",                   DeleteNextWord},
    {"newline",                     InsertChar},
    {"newline-and-backup",          InsertChar},
    {"newline-and-indent",          InsertChar},
    {"re-search",                   ReSearch},
    {"search",                      SearchReplace},
    {"transpose-characters",        TransposeChars},
/*    {"delete-selection-or-previous-character",  DeleteSelOrChar}, */
/*    {"DoReReplace",                 NoOp}, */
/*    {"DoReSearch",                  NoOp}, */
/*    {"backward-line",               NoOp}, */
/*    {"backward-search-selection",   NoOp}, */
/*    {"copy-region",                 NoOp}, */
/*    {"delete-line",                 NoOp}, */
/*    {"delete-word",                 NoOp}, */
/*    {"find-match",                  NoOp}, */
/*    {"forward-line",                NoOp}, */
/*    {"forward-search-selection",    NoOp}, */
/*    {"goto-line",                   NoOp}, */
/*    {"highlight-region",            NoOp}, */
/*    {"include-file",                NoOp}, */
/*    {"include-selection",           NoOp}, */
/*    {"insert-control",              NoOp}, */
/*    {"insert-or-expand-tab",        NoOp}, */
/*    {"keymap",                      NoOp}, */
/*    {"load-file",                   NoOp}, */
/*    {"match-parens",                NoOp}, */
/*    {"paste",                       NoOp}, */
/*    {"re-search",                   NoOp}, */
/*    {"reload-file",                 NoOp}, */
/*    {"save-as",                     NoOp}, */
/*    {"save-file",                   NoOp}, */
/*    {"search-caret",                NoOp}, */
/*    {"search-line",                 NoOp}, */
/*    {"set-mark",                    NoOp}, */
/*    {"set-preferences",             NoOp}, */
/*    {"size",                        NoOp}, */
/*    {"where",                       NoOp}, */
/*    {"DoReplaceAction",             NoOp}, */
/*    {"DoSearchAction",              NoOp}, */
/*    {"InsertFileAction",            NoOp}, */
/*    {"PopdownSearchAction",         NoOp}, */
/*    {"SetField",                    NoOp}, */
/*    {"backward-character",          NoOp}, */
/*    {"backward-paragraph",          NoOp}, */
/*    {"backward-word",               NoOp}, */
/*    {"beginning-of-file",           NoOp}, */
/*    {"beginning-of-line",           NoOp}, */
/*    {"display-caret",               NoOp}, */
/*    {"end-of-file",                 NoOp}, */
/*    {"end-of-line",                 NoOp}, */
/*    {"extend-adjust",               NoOp}, */
/*    {"extend-end",                  NoOp}, */
/*    {"extend-start",                NoOp}, */
/*    {"focus-in",                    NoOp}, */
/*    {"focus-out",                   NoOp}, */
/*    {"forward-character",           NoOp}, */
/*    {"forward-paragraph",           NoOp}, */
/*    {"forward-word",                NoOp}, */
/*    {"insert-file",                 NoOp}, */
/*    {"multiply",                    NoOp}, */
/*    {"next-line",                   NoOp}, */
/*    {"next-page",                   NoOp}, */
/*    {"no-op",                       NoOp}, */
/*    {"previous-line",               NoOp}, */
/*    {"previous-page",               NoOp}, */
/*    {"redraw-display",              NoOp}, */
/*    {"scroll-one-line-down",        NoOp}, */
/*    {"scroll-one-line-up",          NoOp}, */
/*    {"search",                      NoOp}, */
/*    {"select-adjust",               NoOp}, */
/*    {"select-all",                  NoOp}, */
/*    {"select-end",                  NoOp}, */
/*    {"select-save",                 NoOp}, */
/*    {"select-start",                NoOp}, */
/*    {"select-word",                 NoOp}, */
/*    {"close-all",                   NoOp}, */
/*    {"close-buffer",                NoOp}, */
/*    {"close-window",                NoOp}, */
/*    {"deiconify-all",               NoOp}, */
/*    {"end-macro",                   NoOp}, */
/*    {"exec-macro",                  NoOp}, */
/*    {"help",                        NoOp}, */
/*    {"iconify-all",                 NoOp}, */
/*    {"mini-abort",                  NoOp}, */
/*    {"mini-clear",                  NoOp}, */
/*    {"mini-commit",                 NoOp}, */
/*    {"mini-insert",                 NoOp}, */
/*    {"mini-leave",                  NoOp}, */
/*    {"mini-load",                   NoOp}, */
/*    {"mini-pipe",                   NoOp}, */
/*    {"mini-saveas",                 NoOp}, */
/*    {"mini-search",                 NoOp}, */
/*    {"mini-select",                 NoOp}, */
/*    {"mini-shell",                  NoOp}, */
/*    {"new-buffer",                  NoOp}, */
/*    {"new-window",                  NoOp}, */
/*    {"repeat-macro",                NoOp}, */
/*    {"restack",                     NoOp}, */
/*    {"save-all",                    NoOp}, */
/*    {"save-and-close",              NoOp}, */
/*    {"save-and-exit ",              NoOp}, */
/*    {"start-macro",                 NoOp}, */
/*    {"update-info",                 NoOp}, */
    {0,                             0}
};

static XrmQuark allQuarks[sizeof(actionTable) / sizeof(ActionForUndo)] = {
    0
};

/*************************************************************
 *
 *                       Chain Management
 *
 *************************************************************/

UndoRecord* 
NewUndoRecord()
{
    UndoRecord *record;

    record = XtNew(UndoRecord);
    record->prev = record->next = 0;
    record->begin = record->end = 0;
    record->undoType = 0;
    record->data = 0;
    record->complete = 0;

    return record;
}

void
InitialiseUndo(atw)
     AxeiiTextWidget atw;
{
    int s;
    String str;

    if (!allQuarks[0])
    {
	for (s = 0;  (str = actionTable[s].action);  ++s)
	{
	    allQuarks[s] = XrmStringToQuark(str);

	    if (strcmp(str, "undo") == 0)
	    {
		undoQuark = allQuarks[s];
	    }
	    else if (strcmp(str, "insert-char") == 0)
	    {
		inscharQuark = allQuarks[s];
	    }
	    else if (strcmp(str, "newline") == 0)
	    {
		newlineQuark = allQuarks[s];
	    }
	    else if (strcmp(str, "newline-and-indent") == 0)
	    {
		indentQuark = allQuarks[s];
	    }
	    else if (strcmp(str, "newline-and-backup") == 0)
	    {
		backupQuark = allQuarks[s];
	    }
	}
	allQuarks[s] = 0;
    }

    HEAD(atw) = TAIL(atw) = NewUndoRecord();
    UNDO(atw) = HEAD(atw);
}

UndoRecord*
LinkNewUndoRecord(widget)
     Widget widget;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    UndoRecord *record = NewUndoRecord();
    
    record->prev = TAIL(atw);
    TAIL(atw)->next = record;
    TAIL(atw) = record;
    ++LEVEL(atw);

    return record;
}

void
UnlinkUndoRecord(widget, record)
     Widget widget;
     UndoRecord *record;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;

    TAIL(atw) = record->prev;
    if (UNDO(atw) == record)
    {
	UNDO(atw) = record->prev;
    }
    record->prev->next = 0;
    XtFree(record->data);
    XtFree((char *) record);
    --LEVEL(atw);
}

void
FreeUndo(widget, head)
     Widget widget;
     Boolean head;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    UndoRecord *this = HEAD(atw), *next;;

    if (!head)
    {
	this = this->next;
	HEAD(atw)->next = 0;
	UNDO(atw) = TAIL(atw) = HEAD(atw);
	LEVEL(atw) = 0;
    }

    for (next = this ? this->next : 0;
	 this;
	 this = next, next = this ? this->next : 0)
    {
	XtFree(this->data);
	XtFree((char *) this);
    }
}

static void
AdjustChainLength(widget)
     Widget widget;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    int level = atw->axeii.undo_level, removals;
    UndoRecord *next, *delete;

    if (!level || (removals = LEVEL(atw) - level) <= 0)
    {
	return;
    }

    for (next = HEAD(atw)->next;  removals > 0;  --removals)
    {
	delete = next;
	next = delete->next;
	if (UNDO(atw) == delete)
	{
	    UNDO(atw) = HEAD(atw);
	}
	XtFree(delete->data);
	XtFree((char *) delete);
    }

    HEAD(atw)->next = next;
    next->prev = HEAD(atw);
    LEVEL(atw) = level;

#ifdef DEBUG
    PrintChain(widget);
#endif
}

/*************************************************************
 *
 *           The Action Hook on which it all hangs
 *
 *************************************************************/

/*ARGSUSED*/
void 
UndoHook(widget, client_data, action, event, params, num_params)
     Widget widget;
     XtPointer client_data;
     String action;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    XrmQuark quark = XrmStringToQuark(action);
    XawTextEditType editType;
    int q;
    void (*proc)(), (*complete)();
    
    if (widget != (Widget) client_data)
    {
	return;
    }

    for (q = 0;  allQuarks[q] && quark != allQuarks[q];  ++q)
	;

    /* Perform 2nd phase of previous action */
    if ( (!(proc = actionTable[q].proc) || proc != InsertChar ||
	  (proc == InsertChar && atw->axeii.lastAction != inscharQuark)) &&
	                                     (complete = TAIL(atw)->complete))

    {
	(*complete)(widget);
    }

    if (quark != undoQuark || atw->axeii.lastAction != undoQuark)
    {
	AdjustChainLength(widget);
    }

    if (proc)
    {
	/* No point in continuing if action is going to fail */
	XtVaGetValues(widget, XtNeditType, &editType, NULL);
	if (editType == XawtextRead)
	{
	    return;
	}
    
	(*proc)(widget, &quark);
    }

    atw->axeii.lastAction = quark;
}

/*************************************************************
 *
 *                           Insert
 *
 *************************************************************/

static void
RecordAmount(widget)
     Widget widget;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    UndoRecord *tail = TAIL(atw);

    if (!tail->end)
    {
	tail->end = XawTextGetInsertionPoint(widget);
    }

    if (tail->begin == tail->end)
    {
	UnlinkUndoRecord(widget, tail);
    }
    else
    {
	if (tail->undoType == DELETING)
	{
	    tail->undoType = DELETE;
	}
	tail->complete = 0;
    }
}

static void
RecordParagraph(widget)
     Widget widget;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;

    TAIL(atw)->end = XawTextSourceScan(XawTextGetSource(widget),
				       XawTextGetInsertionPoint(widget),
				       XawstParagraph, XawsdRight, 1, False);
    TAIL(atw)->complete = 0;
}

/*ARGSUSED*/
static void
RemoveActionHook(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    XtRemoveActionHook(((AxeiiTextWidget) client_data)->axeii.insert_popup_hook);
}

/*ARGSUSED*/
static void 
InsertFileAction(widget, client_data, action, event, params, num_params)
     Widget widget;
     XtPointer client_data;
     String action;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget atw = (Widget) client_data;

    if (strcmp(action, "InsertFileAction") != 0)
    {
	return;
    }

    /* Create the undo record */
    InsertText(atw, (XrmQuark) 0);
}

/*ARGSUSED*/
static void
InsertFileCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget atw = (Widget) client_data;
    AxeiiTextWidget axtw = (AxeiiTextWidget) atw;
    XtCallbackList callb;

    /* Create the undo record */
    InsertText(atw, (XrmQuark) 0);

    /* Call the original callback list */
    for (callb = axtw->axeii.insertFileCallbacks;  callb->callback;  ++callb)
    {
	(*callb->callback)(atw, callb->closure, (XtPointer) 0);
    }
}

static Boolean
ModifyInsertFile(client_data)
     XtPointer client_data;
{
    Widget widget = (Widget) client_data;
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    Widget popup = XtNameToWidget(widget, "*insertFile");
    Widget insert = XtNameToWidget(widget, "*insertFile*insert");
    XtCallbackList callbacks, callb;
    int ncallb;

    /*
     * We have arrived here as a result of the insert-file action
     * being invoked, i.e. the popup has just been created. It is
     * too early to create an undo record as the user might cancel
     * the popup or make other changes before really inserting a
     * file. So what we do is add an action hook routine to the
     * filename entry field and save the callback list attached to
     * the insert button, replacing it with our own. The user is
     * committed when they hit Return or click the insert button so
     * it is safe for our action hook and callback routines to
     * create an undo record. A callback that removes the action
     * hook is attached to the popup's popdownCallback list.
     *
     * Add an action hook routine to trap the user hitting
     * Return in the text field of the insert file popup.
     */
    atw->axeii.insert_popup_hook
	= XtAppAddActionHook(XtWidgetToApplicationContext(widget),
			                 InsertFileAction, (XtPointer) widget);
    /*
     * We only save the callback list once. Assumption
     * is that it is the same for all text widgets.
     */
    if (atw->axeii.insertFileCallbacks)
    {
	return True;
    }

    XtVaGetValues(insert, XtNcallback, &callbacks, NULL);

    CopyCallbackList(callbacks, &atw->axeii.insertFileCallbacks);

    XtRemoveAllCallbacks(insert, XtNcallback);

    XtAddCallback(popup, XtNpopdownCallback, RemoveActionHook,
		                                           (XtPointer) widget);
    XtAddCallback(insert, XtNcallback, InsertFileCallback, (XtPointer) widget);

    return True;
}

static void
InsertChar(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    UndoRecord *record;

    if (*quark == inscharQuark)
    {
	if (TAIL(atw)->undoType == DELETING)
	{
	    return;
	}
    }
    else if (*quark == newlineQuark || *quark == indentQuark)
    {
	if (TAIL(atw)->undoType == DELETING)
	{
	    *quark = 0;  /* force completion on next action */
	    return;
	}
    }
    else if (*quark == backupQuark)
    {
	if (TAIL(atw)->undoType == DELETING)
	{
	    TAIL(atw)->end = XawTextGetInsertionPoint(widget) + 1;
	    *quark = 0;  /* force completion on next action */
	    return;
	}
    }

    record = LinkNewUndoRecord(widget);
    record->begin = XawTextGetInsertionPoint(widget);
    if (*quark == backupQuark)          /* Can this ever be true */
    {
	record->end = record->begin + 1;
    }
    record->undoType = DELETING;
    record->complete = RecordAmount;
}

/*ARGSUSED*/
static void 
InsertText(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    UndoRecord *record = LinkNewUndoRecord(widget);

    record->begin = XawTextGetInsertionPoint(widget);
    record->undoType = DELETE;
    record->complete = RecordAmount;
}    

/*ARGSUSED*/
static void
InsertFile(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    /*
     * The insert-file action is about to be called. As a result the
     * insert popup will be created. We need to do some work on it.
     * Let's hope that a work proc will get in before the user
     */
    (void) XtAppAddWorkProc(XtWidgetToApplicationContext(widget),
			                 ModifyInsertFile, (XtPointer) widget);
}

void
InsertUndo(widget)
     Widget widget;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;

    /*
     * This routine may be called from the minibuffer as a result of
     * an "r" command, i.e. not as the result of an action. Hence it
     * has to do some of the work of UndoHook. The reason for the last
     * statement of the block is in case it is called in the middle
     * of a sequence of inserting characters, in which case the completion
     * of the insertion of the file won't get done. Setting lastAction to
     * 0 gets round the problem.
     */
    if (TAIL(atw)->complete)
    {
	(*(TAIL(atw)->complete))(widget);
	TAIL(atw)->complete = 0;
	atw->axeii.lastAction = 0;
    }

    InsertText(widget, (XrmQuark) 0);
}

void
ReplaceUndo(widget)
     Widget widget;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    UndoRecord *record;
    XawTextPosition begin, end;

    /*
     * This routine may be called from the minibuffer as a result of
     * a "|" command, i.e. not as the result of an action. Hence it
     * has to do some of the work of UndoHook. The reason for the last
     * statement of the block is in case it is called in the middle
     * of a sequence of inserting characters, in which case the completion
     * of the insertion of the text won't get done. Setting lastAction to
     * 0 gets round the problemy.
     */
    if (TAIL(atw)->complete)
    {
	(*(TAIL(atw)->complete))(widget);
	TAIL(atw)->complete = 0;
	atw->axeii.lastAction = 0;
    }

    XawTextGetSelectionPos(widget, &begin, &end);
    if (end != begin)
    {
        record = LinkNewUndoRecord(widget);
        record->begin = begin;
        record->undoType = REPLACE;
        record->data = AxeiiTextRead(widget, begin, end);
	record->complete = RecordAmount;
    }
}

/*************************************************************
 *
 *                           Delete
 *
 *************************************************************/

static void
MakeUndoRecord(widget, begin, end, dir, op, complete)
     Widget widget;
     XawTextPosition begin, end;
     XawTextScanDirection dir;
     int op;
     void (*complete)();
{
    UndoRecord *record;

    if (end != begin)
    {
	record = LinkNewUndoRecord(widget);
	record->begin = (dir == XawsdRight) ? begin : end;
	record->end = (dir == XawsdRight) ? end : begin;
	record->undoType = op;
	record->data = AxeiiTextRead(widget, record->begin, record->end);

	if (complete)
	{
	    record->complete = complete;
	}
    }
}

/*ARGSUSED*/
static void
DeleteSelection(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    XawTextPosition begin, end;

    XawTextGetSelectionPos(widget, &begin, &end);
    MakeUndoRecord(widget, begin, end, XawsdRight, INSERT, NULL);
}

static void
DeleteScanAmount(widget, type, dir, op, complete)
     Widget widget;
     XawTextScanType type;
     XawTextScanDirection dir;
     int op;
     void (*complete)();
{
    XawTextPosition pos, begin, end;

    begin = XawTextGetInsertionPoint(widget);
    end = XawTextSourceScan(XawTextGetSource(widget),
				                   begin, type, dir, 1, False);

    if (begin == end && type == XawstEOL && dir == XawsdRight)
    {
	end = XawTextSourceScan(XawTextGetSource(widget), begin,
				                XawstEOL, XawsdRight, 1, True);
	if (end - begin != 1)
	{
	    end = begin;
	}
    }

    MakeUndoRecord(widget, begin, end, dir, op, complete);
}

/*ARGSUSED*/
static void
DeleteNextWord(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    DeleteScanAmount(widget, XawstWhiteSpace, XawsdRight, INSERT, NULL);
}

/*ARGSUSED*/
static void
DeletePreviousWord(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    DeleteScanAmount(widget, XawstWhiteSpace, XawsdLeft, INSERT, NULL);
}

/*ARGSUSED*/
static void
DeleteEndLine(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    DeleteScanAmount(widget, XawstEOL, XawsdRight, INSERT, NULL);
}

/*ARGSUSED*/
static void
DeleteEndParagraph(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    DeleteScanAmount(widget, XawstParagraph, XawsdRight, INSERT, NULL);
}

static void
DeleteOneChar(widget, dir)
     Widget widget;
     XawTextScanDirection dir;
{
    XawTextPosition begin, end;
    Widget textSrc = XawTextGetSource(widget);

    begin = XawTextGetInsertionPoint(widget);

    if (dir == XawsdLeft)
    {
	end = begin > 0 ? begin - 1 : 0;
    }
    else
    {
	end = XawTextSourceScan(textSrc, begin, XawstPositions, dir, 1, True);
    }

    MakeUndoRecord(widget, begin, end, dir, INSERT, NULL);
}


/*ARGSUSED*/
static void
DeletePrevChar(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    DeleteOneChar(widget, XawsdLeft);
}

/*ARGSUSED*/
static void
DeleteNextChar(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    DeleteOneChar(widget, XawsdRight);
}

/*
 * Not needed, because the work is done by the delete-selection
 * and delete-previous-character actions when they are called.
 * 
static void
DeleteSelOrChar(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    XawTextPosition begin, end;

    XawTextGetSelectionPos(widget, &begin, &end);
    if (begin == end)
    {
	DeletePrevChar(widget, quark);
    }
    else
    {
	DeleteSelection(widget, quark);
    }
}
*/

/*ARGSUSED*/
static void
DeleteRegion(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    XawTextPosition begin, end;

    AxeiiTextGetRegion(widget, &begin, &end);
    MakeUndoRecord(widget, begin, end, XawsdRight, INSERT, NULL);
}

/*************************************************************
 *
 *                           Replace
 *
 *************************************************************/

/*ARGSUSED*/
static void
CentreLine(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    XawTextPosition pos = XawTextGetInsertionPoint(widget), begin, end;

    begin = XawTextSourceScan(XawTextGetSource(widget), pos,
			                        XawstEOL, XawsdLeft, 1, False);
    end = XawTextSourceScan(XawTextGetSource(widget), pos,
			                       XawstEOL, XawsdRight, 1, False);

    MakeUndoRecord(widget, begin, end, XawsdRight, REPLACE, RecordAmount);

    TAIL((AxeiiTextWidget) widget)->end = 0;
}

/*ARGSUSED*/
static void
FormParagraph(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    XawTextPosition pos = XawTextGetInsertionPoint(widget), begin, end;

    begin = XawTextSourceScan(XawTextGetSource(widget), pos,
			                  XawstParagraph, XawsdLeft, 1, False);
    end = XawTextSourceScan(XawTextGetSource(widget), pos,
			                 XawstParagraph, XawsdRight, 1, False);

    MakeUndoRecord(widget, begin, end, XawsdRight, REPLACE, RecordParagraph);
}    

/*ARGSUSED*/
static void
TransposeChars(widget, quark)
     Widget widget;
     XrmQuark quark;
{
    XawTextPosition pos = XawTextGetInsertionPoint(widget), begin, end;

    begin = XawTextSourceScan(XawTextGetSource(widget), pos,
			                   XawstPositions, XawsdLeft, 1, True);
    end = XawTextSourceScan(XawTextGetSource(widget), pos,
			                  XawstPositions, XawsdRight, 1, True);

    if (end - begin != 2)
    {
	end = begin;
    }

    MakeUndoRecord(widget, begin, end, XawsdRight, REPLACE, NULL);
}

/*ARGSUSED*/
static void
ReSearch(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    searchFromReSearch = True;
}

static Boolean
RemoveSearchHook(client_data)
     XtPointer client_data;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) client_data;

    XtRemoveActionHook(atw->axeii.search_popup_hook);

    return True;
}

/*ARGSUSED*/
static void
SearchPopdown(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    XtCallbackList callb;
    Widget one = XtNameToWidget(widget, "*replaceOne");
    Widget all = XtNameToWidget(widget, "*replaceAll");
    AxeiiTextWidget atw = (AxeiiTextWidget) XtParent(widget);

    /*
     * This may be treating the symptons rather than the cause, but removing
     * the action hook directly here causes core dumps when hitting the Cancel
     * button if the Search button has been used. In the absence of an
     * understanding of the problem get round it by doing the work later in
     * a work proc.
     */
    (void) XtAppAddWorkProc(XtWidgetToApplicationContext(widget),
			              RemoveSearchHook, (XtPointer) atw);
    /*
     * Restore "Replace One" and "Replace All" callbacks
     */
    XtRemoveAllCallbacks(one, XtNcallback);
    for (callb = atw->axeii.searchOneCallbacks;  callb->callback;  ++callb)
    {
	XtAddCallback(one, XtNcallback, callb->callback, callb->closure);
    }

    XtRemoveAllCallbacks(all, XtNcallback);
    for (callb = atw->axeii.searchAllCallbacks;  callb->callback;  ++callb)
    {
	XtAddCallback(all, XtNcallback, callb->callback, callb->closure);
    }

    /*
     * Remove ourself; it appears to work but is this really safe?
     */
    XtRemoveCallback(widget, XtNpopdownCallback, SearchPopdown, client_data);
}

/*ARGSUSED*/
static void
ReplaceText(widget, srchFor, rplcWith)
     Widget widget;
     String *srchFor, *rplcWith;
{
    Widget searchFor, replaceWith;
    String selection, searchText, replaceText;
    XawTextPosition begin, end;
    UndoRecord *record;

    *srchFor = 0;
    *rplcWith = 0;

    XawTextGetSelectionPos(widget, &begin, &end);
    /* Selection has probably been tampered with */
    if (begin == end)
    {
	return;
    }

    selection = AxeiiTextRead(widget, begin, end);
    searchFor = XtNameToWidget(widget, "*search*searchText");
    XtVaGetValues(searchFor, XtNstring, &searchText, NULL);
    
    /* Selection has probably been tampered with */
    if (strcmp(selection, searchText) != 0)
    {
	XtFree(selection);
	return;
    }
    XtFree(selection);
    
    replaceWith = XtNameToWidget(widget, "*search*replaceText");
    XtVaGetValues(replaceWith, XtNstring, &replaceText, NULL);
    
    record = LinkNewUndoRecord(widget);
    record->begin = begin;
    record->end = begin + strlen(replaceText);
    record->undoType = REPLACE;
    record->data = XtNewString(searchText);

    *srchFor = searchText;
    *rplcWith = replaceText;
}

/*ARGSUSED*/
void 
DoReplaceAction(widget, client_data, action, event, params, num_params)
     Widget widget;
     XtPointer client_data;
     String action;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget atw = (Widget) client_data;
    String searchFor, replaceWith;

    if (strcmp(action, "DoReplaceAction") != 0)
    {
	return;
    }

    /* Create the undo record */
    ReplaceText(atw, &searchFor, &replaceWith);
}

/*ARGSUSED*/
static void
DoReplaceCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget atw = (Widget) client_data;
    AxeiiTextWidget axtw = (AxeiiTextWidget) atw;
    String searchFor, replaceWith;
    XtCallbackList callb;

    /* Create the undo record */
    ReplaceText(atw, &searchFor, &replaceWith);

    if (!searchFor || !replaceWith)
    {
	return;
    }

    /* Call the original callback list */
    for (callb = axtw->axeii.searchOneCallbacks;  callb->callback;  ++callb)
    {
	(*callb->callback)(atw, callb->closure, (XtPointer) 0);
    }
}

/*ARGSUSED*/
static void
DoReplaceAllCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget atw = (Widget) client_data;
    AxeiiTextWidget axtw = (AxeiiTextWidget) atw;
    Widget textSrc = XawTextGetSource(atw);
    String searchFor, replaceWith;
    int slen, rlen, diff, adjustment;
    XawTextPosition found;
    XawTextBlock block;
    UndoRecord *record;
    XtCallbackList callb;

    /* Do the first one */
    ReplaceText(atw, &searchFor, &replaceWith);

    if (!searchFor || !replaceWith)
    {
	return;
    }

    slen = strlen(searchFor);
    rlen = strlen(replaceWith);
    diff = rlen - slen;
    block.firstPos = 0;
    block.length = slen;
    block.ptr = searchFor;
    block.format = FMT8BIT;

    /*
     * Deal with subsequent occurrences, making allowance for the
     * difference in lengths of the search and replacement strings
     */
    for (found = XawTextGetInsertionPoint(atw), adjustment = diff;
	 (found = XawTextSourceSearch(textSrc, found, XawsdRight, &block ))
	                                                 != XawTextSearchError;
	 found += block.length, adjustment += diff)
    {
	record = LinkNewUndoRecord(atw);
	record->begin = found + adjustment;
	record->end = found + rlen + adjustment;
	record->undoType = REPLACE;
	record->data = XtNewString(block.ptr);
    }

    /*
     * Now call the original callback list to perform the replacements
     */
    for (callb = axtw->axeii.searchAllCallbacks;  callb->callback;  ++callb)
    {
	(*callb->callback)(atw, callb->closure, (XtPointer) 0);
    }
}

static Boolean
ModifySearchReplace(client_data)
     XtPointer client_data;
{
    Widget widget = (Widget) client_data;
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    Widget popup = XtNameToWidget(widget, "*search");
    Widget with = XtNameToWidget(popup, "*replaceText");
    Widget one = XtNameToWidget(popup, "*replaceOne");
    Widget all = XtNameToWidget(popup, "*replaceAll");
    XtCallbackList callbacks, callb;
    int ncallb;

    /*
     * We have arrived here as a result of the search action being
     * invoked on its own, i.e. not from re-search. It is too early to
     * create an undo record as the user might cancel the popup or make
     * other changes before replacing anything.  So what we do is add an
     * action hook routine to the "Replace with" entry field and save the
     * callback list attached to the Replace One/All buttons, replacing them
     * with our own. The user is committed when they hit Return or click the
     * Replace One/All buttons, so it is safe for our action hook and callback
     * routines to create undo record/s. A callback that removes them is
     * is attached to the popup's popdownCallback list.
     *
     * Add an action hook routine to trap the user hitting
     * Return in the text field of the search file popup.
     */
    atw->axeii.search_popup_hook
	= XtAppAddActionHook(XtWidgetToApplicationContext(widget),
			                  DoReplaceAction, (XtPointer) widget);

    XtVaGetValues(one, XtNcallback, &callbacks, NULL);
    XtFree((char *) atw->axeii.searchOneCallbacks);
    CopyCallbackList(callbacks, &atw->axeii.searchOneCallbacks);
    XtRemoveAllCallbacks(one, XtNcallback);
    XtAddCallback(one, XtNcallback, DoReplaceCallback, (XtPointer) widget);

    XtVaGetValues(all, XtNcallback, &callbacks, NULL);
    XtFree((char *) atw->axeii.searchAllCallbacks);
    CopyCallbackList(callbacks, &atw->axeii.searchAllCallbacks);
    XtRemoveAllCallbacks(all, XtNcallback);
    XtAddCallback(all, XtNcallback, DoReplaceAllCallback, (XtPointer) widget);

    XtAddCallback(popup, XtNpopdownCallback, SearchPopdown,
		                                           (XtPointer) widget);
    return True;
}

/*ARGSUSED*/
static void
SearchReplace(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    /*
     * The search action is about to be called. The search popup may
     * be about to be created or already exist from earlier execution
     * of the search or re-search actions. Whatever the case, we need
     * to do some work on it, but only if not called via the re-search
     * action since re-search (own code) does undos a different way.
     * Let's hope that a work proc will get in before the user.
     */
    if (!searchFromReSearch)
    {
	(void) XtAppAddWorkProc(XtWidgetToApplicationContext(widget),
			              ModifySearchReplace, (XtPointer) widget);
    }
    else
    {
	searchFromReSearch = False;
    }
}

/*************************************************************
 *
 *                            Reset
 *
 *************************************************************/

void
ResetUndo(widget)
     Widget widget;
{
    FreeUndo(widget, False);
}

/*ARGSUSED*/
static void
ClearBuffer(widget, quark)
     Widget widget;
     XrmQuark quark;
{
    FreeUndo(widget, False);
}

/*************************************************************
 *
 *                            Undo
 *
 *************************************************************/

static void 
Undoing(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;

    if (*quark == undoQuark)
    {
        atw->axeii.undoing = (atw->axeii.lastAction == undoQuark);
    }
}    

/*ARGSUSED*/
void
UndoAction(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    XawTextBlock block;
    UndoRecord *element, *inverse;
    String replaced;

    if (!atw->axeii.undo)
    {
	XtCallCallbackList(widget, atw->axeii.no_undo_callbacks, (XtPointer)0);
	return;
    }

    if (!atw->axeii.undo)
    {
	return;
    }

    if (!atw->axeii.undoing)
    {
	UNDO(atw) = TAIL(atw);
    }
    else if (UNDO(atw) == HEAD(atw))
    {
	UNDO(atw) = TAIL(atw);
    }

    if (UNDO(atw) == HEAD(atw))
    {
	XtCallCallbackList(widget, atw->axeii.no_undo_callbacks, (XtPointer)0);
	return;
    }

    /*
     * Perform the undo action specified in the undo record;
     * Construct a record to specify the inverse action;
     * Add that record to the tail of the undo chain;
     */
    element = UNDO(atw);
    inverse = LinkNewUndoRecord(atw);
    inverse->begin = element->begin;
    block.firstPos = 0;
    block.format = FMT8BIT;
    switch (element->undoType)
    {
    case INSERT:
	block.length = strlen(element->data);
	block.ptr = element->data;
	XawTextReplace(widget, element->begin,
		               element->begin, &block);
	XawTextSetInsertionPoint(widget,
				 element->begin + block.length);

	inverse->end = inverse->begin + block.length;
	inverse->undoType = DELETE;
	break;
    case DELETE:
	inverse->data = AxeiiTextRead(widget, element->begin, element->end);
	block.length = 0;
	block.ptr = 0;
	XawTextReplace(widget, element->begin, element->end, &block);
	XawTextSetInsertionPoint(widget, element->begin);
	
	inverse->end = element->end;
	inverse->undoType = INSERT;
	break;
    case REPLACE:
	replaced = AxeiiTextRead(widget, element->begin, element->end);

	block.length = strlen(element->data);
	block.ptr = element->data;
	XawTextReplace(widget, element->begin, element->end, &block);
	XawTextSetInsertionPoint(widget, element->begin + block.length);

	inverse->end = inverse->begin + block.length;
	inverse->undoType = element->undoType;
	inverse->data = replaced;
	break;
    default:
	break;
    }

    UNDO(atw) = element->prev;

#ifdef DEBUG
    PrintChain(widget);
#endif
}

/*************************************************************
 *
 *                      Were useful once
 *
 *************************************************************/

/*
static void 
NoOp(widget, quark)
     Widget widget;
     XrmQuark *quark;
{
    printf("%s\n", XrmQuarkToString(*quark));
}    
*/

#ifdef DEBUG
static void
PrintChain(widget)
     Widget widget;
{
    AxeiiTextWidget atw = (AxeiiTextWidget) widget;
    UndoRecord *next;

    printf("\n========================\n");
    for (next = HEAD(atw);  next;  next = next->next)
    {
	printf("%x\n  %x %d %d %d %s %x\n", next, next->prev, next->begin,
	                                    next->end, next->undoType,
	                                    next->data, next->next);	       
    }
}
#endif
