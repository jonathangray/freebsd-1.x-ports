/* $XConsortium: button.c,v 1.66 91/05/31 17:00:03 gildea Exp $ */
/* $Id: button.c,v 1.1 1994/06/27 17:17:43 asami Exp $ */
/*
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts.
 *
 *                         All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital Equipment
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior permission.
 *
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

/*
button.c	Handles button events in the terminal emulator.
		does cut/paste operations, change modes via menu,
		passes button events through to some applications.
				J. Gettys.
*/

#include "ptyx.h"		/* Xlib headers included here. */
#include <X11/Xatom.h>
#include <stdio.h>

#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/StdSel.h>

#include "data.h"
#include "error.h"
#include "menu.h"

extern char *malloc();

extern void DoSecureKeyboard();

#define KeyState(x) (((x) & (ShiftMask|ControlMask)) + (((x) & Mod1Mask) ? 2 : 0))
    /* adds together the bits:
        shift key -> 1
        meta key  -> 2
        control key -> 4 */
  
#define TEXTMODES 4
#define NBUTS 3
#define DIRS 2
#define UP 1
#define DOWN 0
#define SHIFTS 8		/* three keys, so eight combinations */
#define	Coordinate(r,c)		((r) * (term->screen.max_col+1) + (c))

extern char *xterm_name;

static void PointToRowCol();
static void SelectionReceived();
static void TrackDown();
static void ComputeSelect();
static void EditorButton();
static void ExtendExtend();
static void ReHiliteText();
static void SelectSet();
static void StartSelect();
static int Length();
#ifdef KTERM
static Ichr *SaveText();
#else /* !KTERM */
static char *SaveText();
#endif /* !KTERM */

extern XtermWidget term;

/* Selection/extension variables */

/* Raw char position where the selection started */
static int rawRow, rawCol;

/* Selected area before CHAR, WORD, LINE selectUnit processing */
static int startRRow, startRCol, endRRow, endRCol = 0;

/* Selected area after CHAR, WORD, LINE selectUnit processing */
static int startSRow, startSCol, endSRow, endSCol = 0;

/* Valid rows for selection clipping */
static int firstValidRow, lastValidRow;

/* Start, end of extension */
static int startERow, startECol, endERow, endECol;

/* Saved values of raw selection for extend to restore to */
static int saveStartRRow, saveStartRCol, saveEndRRow, saveEndRCol;

/* Multi-click handling */
static int numberOfClicks = 0;
static long int lastButtonUpTime = 0;
typedef int SelectUnit;
#define SELECTCHAR 0
#define SELECTWORD 1
#define SELECTLINE 2
#define NSELECTUNITS 3
static SelectUnit selectUnit;

/* Send emacs escape code when done selecting or extending? */
static int replyToEmacs;

#if defined(KTERM) && defined(KTERM_MBCC)
/*
 * by Kiyoshi KANAZAWA, Nov. 29, 1990.
 * Support word-select for MBCS.
*/
static int mbcsCharClass ();
#endif	/* KTERM && KTERM_MBCC */

Boolean SendMousePosition(w, event)
Widget w;
XEvent* event;
{
    register TScreen *screen = &((XtermWidget)w)->screen;
    
    if (screen->send_mouse_pos == 0) return False;

    if (event->type != ButtonPress && event->type != ButtonRelease)
	return False;

#define KeyModifiers \
    (event->xbutton.state & (ShiftMask | LockMask | ControlMask | Mod1Mask | \
			     Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask ))

#define ButtonModifiers \
    (event->xbutton.state & (ShiftMask | LockMask | ControlMask | Mod1Mask | \
			     Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask ))

    switch (screen->send_mouse_pos) {
      case 1: /* X10 compatibility sequences */

	if (KeyModifiers == 0) {
	    if (event->type == ButtonPress)
		EditorButton(event);
	    return True;
	}
	return False;

      case 2: /* DEC vt200 compatible */

	if (KeyModifiers == 0 || KeyModifiers == ControlMask) {
	    EditorButton(event);
	    return True;
	}
	return False;

      case 3: /* DEC vt200 hilite tracking */
	if (  event->type == ButtonPress &&
	      KeyModifiers == 0 &&
	      event->xbutton.button == Button1 ) {
	    TrackDown(event);
	    return True;
	}
	if (KeyModifiers == 0 || KeyModifiers == ControlMask) {
	    EditorButton(event);
	    return True;
	}
	/* fall through */

      default:
	return False;
    }
#undef KeyModifiers
}


/*ARGSUSED*/
void HandleSelectExtend(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XMotionEvent */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
	register TScreen *screen = &((XtermWidget)w)->screen;
	int row, col;

	screen->selection_time = event->xmotion.time;
	switch (eventMode) {
		case LEFTEXTENSION :
		case RIGHTEXTENSION :
			PointToRowCol (event->xmotion.y, event->xmotion.x, 
				       &row, &col);
			ExtendExtend (row, col);
			break;
		case NORMAL :
			/* will get here if send_mouse_pos != 0 */
		        break;
	}
}

static void EndExtend();

static void do_select_end (w, event, params, num_params, use_cursor_loc)
Widget w;
XEvent *event;			/* must be XButtonEvent */
String *params;			/* selections */
Cardinal *num_params;
Bool use_cursor_loc;
{
	((XtermWidget)w)->screen.selection_time = event->xbutton.time;
	switch (eventMode) {
		case NORMAL :
		    (void) SendMousePosition(w, event);
		    break;
		case LEFTEXTENSION :
		case RIGHTEXTENSION :
		    EndExtend(w, event, params, *num_params, use_cursor_loc);
		    break;
	}
}


void HandleSelectEnd(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent */
String *params;			/* selections */
Cardinal *num_params;
{
	do_select_end (w, event, params, num_params, False);
}


void HandleKeyboardSelectEnd(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent */
String *params;			/* selections */
Cardinal *num_params;
{
	do_select_end (w, event, params, num_params, True);
}




struct _SelectionList {
    String *params;
    Cardinal count;
    Time time;
#ifdef KTERM /* from exterm */
    Boolean asked;
    Atom selection;
#endif /* KTERM */
};


static void _GetSelection(w, time, params, num_params)
Widget w;
Time time;
String *params;			/* selections in precedence order */
Cardinal num_params;
{
    Atom selection;
    int cutbuffer;

    XmuInternStrings(XtDisplay(w), params, (Cardinal)1, &selection);
    switch (selection) {
      case XA_CUT_BUFFER0: cutbuffer = 0; break;
      case XA_CUT_BUFFER1: cutbuffer = 1; break;
      case XA_CUT_BUFFER2: cutbuffer = 2; break;
      case XA_CUT_BUFFER3: cutbuffer = 3; break;
      case XA_CUT_BUFFER4: cutbuffer = 4; break;
      case XA_CUT_BUFFER5: cutbuffer = 5; break;
      case XA_CUT_BUFFER6: cutbuffer = 6; break;
      case XA_CUT_BUFFER7: cutbuffer = 7; break;
      default:	       cutbuffer = -1;
    }
    if (cutbuffer >= 0) {
	register TScreen *screen = &((XtermWidget)w)->screen;
	int inbytes;
	unsigned long nbytes;
	int fmt8 = 8;
	Atom type = XA_STRING;
	char *line = XFetchBuffer(screen->display, &inbytes, cutbuffer);
	nbytes = (unsigned long) inbytes;
	if (nbytes > 0)
	    SelectionReceived(w, NULL, &selection, &type, (XtPointer)line,
			      &nbytes, &fmt8);
	else if (num_params > 1)
	    _GetSelection(w, time, params+1, num_params-1);
    } else {
	struct _SelectionList* list;
	if (--num_params) {
	    list = XtNew(struct _SelectionList);
	    list->params = params + 1;
	    list->count = num_params; /* decremented above */
	    list->time = time;
#ifdef KTERM /* from exterm */
	    list->asked = True;
	    list->selection = selection;
#endif /* KTERM */
	} else list = NULL;
#ifdef KTERM
	XtGetSelectionValue(w, selection, XA_TEXT(XtDisplay(w)), SelectionReceived,
			    (XtPointer)list, time);
#else /* !KTERM */
	XtGetSelectionValue(w, selection, XA_STRING, SelectionReceived,
			    (XtPointer)list, time);
#endif /* !KTERM */
    }
}

/* SelectionReceived: stuff received selection text into pty */

/* ARGSUSED */
static void SelectionReceived(w, client_data, selection, type,
			      value, length, format)
Widget w;
XtPointer client_data;
Atom *selection, *type;
XtPointer value;
unsigned long *length;
int *format;
{
    int pty = ((XtermWidget)w)->screen.respond;	/* file descriptor of pty */
    register char *lag, *cp, *end;
    char *line = (char*)value;
#ifdef KTERM
    char lbuf[256 + 1];
#endif /* KTERM */
				  
    if (*type == 0 /*XT_CONVERT_FAIL*/ || *length == 0 || value == NULL) {
	/* could not get this selection, so see if there are more to try */
	struct _SelectionList* list = (struct _SelectionList*)client_data;
	if (list != NULL) {
#ifdef KTERM /* from exterm */
            /* ask XA_STRING again.
             * Warning: hope owner not triggered between the 2 requests
             *          XA_STRING and XA_COMPOUNT_TEXT.
             */
            if (list->asked) {
                list->asked = False;
                XtGetSelectionValue(w, list->selection, XA_STRING,
                        SelectionReceived, (XtPointer)list, list->time);
            } else {
                _GetSelection(w, list->time, list->params, list->count);
                XtFree(client_data);
            }
#else /* !KTERM */
	    _GetSelection(w, list->time, list->params, list->count);
	    XtFree(client_data);
#endif /* !KTERM */
	}
	return;
    }

#ifdef KTERM
    if (*type == XA_COMPOUND_TEXT(XtDisplay(w))) {
	Char *ct = (Char *)value;
	Ichr *cs;
	int (*func)();
	int n;
	int convCStoJIS();
# ifdef KTERM_KANJI
	int convCStoEUC(), convCStoSJIS();
# endif /* KTERM_KANJI */
	Ichr cbuf[256 + 1];

	n = convCTtoCS(ct, *length, NULL);
	if (n < 0) { /* data broken */
	    XtFree(client_data);
	    XtFree(value);
	    return;
	}
	cs = (n > 256) ? (Ichr *)XtMalloc((n + 1) * sizeof(Ichr)) : cbuf;
	(void)convCTtoCS(ct, *length, cs);

# ifdef KTERM_KANJI
	switch (((XtermWidget)w)->flags & (EUC_KANJI|SJIS_KANJI)) {
	case EUC_KANJI:
	    func = convCStoEUC;
	    break;
	case SJIS_KANJI:
	    func = convCStoSJIS;
	    break;
	default:
	    func = convCStoJIS;
	    break;
	}
# else /* !KTERM_KANJI */
	func = convCStoJIS;
# endif /* !KTERM_KANJI */

	n = (*func)(cs, NULL);
	line = (n > 256) ? XtMalloc(n + 1) : lbuf;
	(void)(*func)(cs, line);
	end = line + n;
	if (cs != cbuf) XtFree((char *)cs);
    } else { /* must be XA_STRING */
	char *p, *q;
	int n = *length;

	line = (n > 256) ? XtMalloc(n + 1) : lbuf;
	bcopy((char *)value, line, n);
	line[n] = '\0';
	p = (char *) value;
	q = line;
	while (n-- > 0) {
	    if (!(*p & 0x80)) {
		*q++ = *p;
	    }
	    p++;
	}
	end = q;
    }
#else /* !KTERM */
    /* Write data to pty a line at a time. */
    /* Doing this one line at a time may no longer be necessary
       because v_write has been re-written. */

    end = &line[*length];
#endif /* !KTERM */
    lag = line;
    for (cp = line; cp != end; cp++)
	{
	    if (*cp != '\n') continue;
	    *cp = '\r';
	    v_write(pty, lag, cp - lag + 1);
	    lag = cp + 1;
	}
    if (lag != end)
	v_write(pty, lag, end - lag);

#ifdef KTERM
    if (line != lbuf) XtFree(line);
#endif /* KTERM */
    XtFree(client_data);
    XtFree(value);
}


void
HandleInsertSelection(w, event, params, num_params)
Widget w;
XEvent *event;			/* assumed to be XButtonEvent* */
String *params;			/* selections in precedence order */
Cardinal *num_params;
{
    if (SendMousePosition(w, event)) return;
    _GetSelection(w, event->xbutton.time, params, *num_params);
}


static void
SetSelectUnit(buttonDownTime, defaultUnit)
    unsigned long buttonDownTime;
    SelectUnit defaultUnit;
{
/* Do arithmetic as integers, but compare as unsigned solves clock wraparound */
	if ((long unsigned)((long int)buttonDownTime - lastButtonUpTime)
	 > term->screen.multiClickTime) {
		numberOfClicks = 1;
		selectUnit = defaultUnit;
	} else {
		++numberOfClicks;
		selectUnit = ((selectUnit + 1) % NSELECTUNITS);
	}
}

static void do_select_start (w, event, startrow, startcol)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
int startrow, startcol;
{
	if (SendMousePosition(w, event)) return;
	SetSelectUnit(event->xbutton.time, SELECTCHAR);
	replyToEmacs = FALSE;
	StartSelect(startrow, startcol);
}

/* ARGSUSED */
void
HandleSelectStart(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
	register TScreen *screen = &((XtermWidget)w)->screen;
	int startrow, startcol;

	firstValidRow = 0;
	lastValidRow  = screen->max_row;
	PointToRowCol(event->xbutton.y, event->xbutton.x, &startrow, &startcol);
	do_select_start (w, event, startrow, startcol);
}


/* ARGSUSED */
void
HandleKeyboardSelectStart(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
	register TScreen *screen = &((XtermWidget)w)->screen;

	do_select_start (w, event, screen->cursor_row, screen->cursor_col);
}


static void
TrackDown(event)
    register XButtonEvent *event;
{
	int startrow, startcol;

	SetSelectUnit(event->time, SELECTCHAR);
	if (numberOfClicks > 1 ) {
		PointToRowCol(event->y, event->x, &startrow, &startcol);
		replyToEmacs = TRUE;
		StartSelect(startrow, startcol);
	} else {
		waitingForTrackInfo = 1;
		EditorButton(event);
	}
}


#define boundsCheck(x)	if (x < 0) \
			    x = 0; \
			else if (x >= screen->max_row) \
			    x = screen->max_row;

TrackMouse(func, startrow, startcol, firstrow, lastrow)
int func, startrow, startcol, firstrow, lastrow;
{
	TScreen *screen = &term->screen;

	if (!waitingForTrackInfo) {	/* Timed out, so ignore */
		return;
	}
	waitingForTrackInfo = 0;
	if (func == 0) return;
	boundsCheck (startrow)
	boundsCheck (firstrow)
	boundsCheck (lastrow)
	firstValidRow = firstrow;
	lastValidRow  = lastrow;
	replyToEmacs = TRUE;
	StartSelect(startrow, startcol);
}

static void
StartSelect(startrow, startcol)
    int startrow, startcol;
{
	TScreen *screen = &term->screen;

	if (screen->cursor_state)
	    HideCursor ();
	if (numberOfClicks == 1) {
		/* set start of selection */
		rawRow = startrow;
		rawCol = startcol;
		
	} /* else use old values in rawRow, Col */

	saveStartRRow = startERow = rawRow;
	saveStartRCol = startECol = rawCol;
	saveEndRRow   = endERow   = rawRow;
	saveEndRCol   = endECol   = rawCol;
	if (Coordinate(startrow, startcol) < Coordinate(rawRow, rawCol)) {
		eventMode = LEFTEXTENSION;
		startERow = startrow;
		startECol = startcol;
	} else {
		eventMode = RIGHTEXTENSION;
		endERow = startrow;
		endECol = startcol;
	}
	ComputeSelect(startERow, startECol, endERow, endECol, False);

}

static void
EndExtend(w, event, params, num_params, use_cursor_loc)
    Widget w;
    XEvent *event;			/* must be XButtonEvent */
    String *params;			/* selections */
    Cardinal num_params;
    Bool use_cursor_loc;
{
	int	row, col;
	TScreen *screen = &term->screen;
	char line[9];

	if (use_cursor_loc) {
	    row = screen->cursor_row;
	    col = screen->cursor_col;
	} else {
	    PointToRowCol(event->xbutton.y, event->xbutton.x, &row, &col);
	}
	ExtendExtend (row, col);
	lastButtonUpTime = event->xbutton.time;
	if (startSRow != endSRow || startSCol != endSCol) {
		if (replyToEmacs) {
			if (rawRow == startSRow && rawCol == startSCol 
			    && row == endSRow && col == endSCol) {
			 	/* Use short-form emacs select */
				strcpy(line, "\033[t");
				line[3] = ' ' + endSCol + 1;
				line[4] = ' ' + endSRow + 1;
				v_write(screen->respond, line, 5);
			} else {
				/* long-form, specify everything */
				strcpy(line, "\033[T");
				line[3] = ' ' + startSCol + 1;
				line[4] = ' ' + startSRow + 1;
				line[5] = ' ' + endSCol + 1;
				line[6] = ' ' + endSRow + 1;
				line[7] = ' ' + col + 1;
				line[8] = ' ' + row + 1;
				v_write(screen->respond, line, 9);
			}
			TrackText(0, 0, 0, 0);
		}
	}
	SelectSet(w, event, params, num_params);
	eventMode = NORMAL;
}

void
HandleSelectSet(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
	SelectSet (w, event, params, *num_params);
}

static void SaltTextAway();

/* ARGSUSED */
static void
SelectSet (w, event, params, num_params)
    Widget	w;
    XEvent	*event;
    String	*params;
    Cardinal    num_params;
{
	/* Only do select stuff if non-null select */
	if (startSRow != endSRow || startSCol != endSCol) {
		SaltTextAway(startSRow, startSCol, endSRow, endSCol,
			     params, num_params);
	} else
		DisownSelection(term);
}

#define Abs(x)		((x) < 0 ? -(x) : (x))

/* ARGSUSED */
static void do_start_extend (w, event, params, num_params, use_cursor_loc)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
Bool use_cursor_loc;
{
	TScreen *screen = &((XtermWidget)w)->screen;
	int row, col, coord;

	if (SendMousePosition(w, event)) return;
	firstValidRow = 0;
	lastValidRow  = screen->max_row;
	SetSelectUnit(event->xbutton.time, selectUnit);
	replyToEmacs = FALSE;

	if (numberOfClicks == 1) {
		/* Save existing selection so we can reestablish it if the guy
		   extends past the other end of the selection */
		saveStartRRow = startERow = startRRow;
		saveStartRCol = startECol = startRCol;
		saveEndRRow   = endERow   = endRRow;
		saveEndRCol   = endECol   = endRCol;
	} else {
		/* He just needed the selection mode changed, use old values. */
		startERow = startRRow = saveStartRRow;
		startECol = startRCol = saveStartRCol;
		endERow   = endRRow   = saveEndRRow;
		endECol   = endRCol   = saveEndRCol;

	}
	if (use_cursor_loc) {
	    row = screen->cursor_row;
	    col = screen->cursor_col;
	} else {
	    PointToRowCol(event->xbutton.y, event->xbutton.x, &row, &col);
	}
	coord = Coordinate(row, col);

	if (Abs(coord - Coordinate(startSRow, startSCol))
	     < Abs(coord - Coordinate(endSRow, endSCol))
	    || coord < Coordinate(startSRow, startSCol)) {
	 	/* point is close to left side of selection */
		eventMode = LEFTEXTENSION;
		startERow = row;
		startECol = col;
	} else {
	 	/* point is close to left side of selection */
		eventMode = RIGHTEXTENSION;
		endERow = row;
		endECol = col;
	}
	ComputeSelect(startERow, startECol, endERow, endECol, True);
}

static void
ExtendExtend (row, col)
    int row, col;
{
	int coord = Coordinate(row, col);
	
	if (eventMode == LEFTEXTENSION 
	 && (coord + (selectUnit!=SELECTCHAR)) > Coordinate(endSRow, endSCol)) {
		/* Whoops, he's changed his mind.  Do RIGHTEXTENSION */
		eventMode = RIGHTEXTENSION;
		startERow = saveStartRRow;
		startECol = saveStartRCol;
	} else if (eventMode == RIGHTEXTENSION
	 && coord < Coordinate(startSRow, startSCol)) {
	 	/* Whoops, he's changed his mind.  Do LEFTEXTENSION */
		eventMode = LEFTEXTENSION;
		endERow   = saveEndRRow;
		endECol   = saveEndRCol;
	}
	if (eventMode == LEFTEXTENSION) {
		startERow = row;
		startECol = col;
	} else {
		endERow = row;
		endECol = col;
	}
	ComputeSelect(startERow, startECol, endERow, endECol, False);
}


void HandleStartExtend(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
    do_start_extend (w, event, params, num_params, False);
}

void HandleKeyboardStartExtend(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
    do_start_extend (w, event, params, num_params, True);
}





ScrollSelection(screen, amount)
register TScreen* screen;
register int amount;
{
    register int minrow = -screen->savedlines;

    /* Sent by scrollbar stuff, so amount never takes selection out of
       saved text */

    /* XXX - the preceeding is false; cat /etc/termcap (or anything
       larger than the number of saved lines plus the screen height) and then
       hit extend select */

    startRRow += amount; endRRow += amount;
    startSRow += amount; endSRow += amount;
    rawRow += amount;
    screen->startHRow += amount;
    screen->endHRow += amount;

    if (startRRow < minrow) {
	startRRow = minrow;
	startRCol = 0;
    }
    if (endRRow < minrow) {
	endRRow = minrow;
        endRCol = 0;
    }
    if (startSRow < minrow) {
	startSRow = minrow;
	startSCol = 0;
    }
    if (endSRow < minrow) {
	endSRow = minrow;
	endSCol = 0;
    }
    if (rawRow < minrow) {
	rawRow = minrow;
	rawCol = 0;
    }
    if (screen->startHRow < minrow) {
	screen->startHRow = minrow;
	screen->startHCol = 0;
    }
    if (screen->endHRow < minrow) {
	screen->endHRow = minrow;
	screen->endHCol = 0;
    }
    screen->startHCoord = Coordinate (screen->startHRow, screen->startHCol);
    screen->endHCoord = Coordinate (screen->endHRow, screen->endHCol);
}


/*ARGSUSED*/
ResizeSelection (screen, rows, cols)
    TScreen *screen;
    int rows, cols;
{
    rows--;				/* decr to get 0-max */
    cols--;

    if (startRRow > rows) startRRow = rows;
    if (startSRow > rows) startSRow = rows;
    if (endRRow > rows) endRRow = rows;
    if (endSRow > rows) endSRow = rows;
    if (rawRow > rows) rawRow = rows;

    if (startRCol > cols) startRCol = cols;
    if (startSCol > cols) startSCol = cols;
    if (endRCol > cols) endRCol = cols;
    if (endSCol > cols) endSCol = cols;
    if (rawCol > cols) rawCol = cols;
}

static void
PointToRowCol(y, x, r, c)
    register int y, x;
    int *r, *c;
/* Convert pixel coordinates to character coordinates.
   Rows are clipped between firstValidRow and lastValidRow.
   Columns are clipped between to be 0 or greater, but are not clipped to some
       maximum value. */
{
	register TScreen *screen = &term->screen;
	register row, col;

	row = (y - screen->border) / FontHeight(screen);
	if(row < firstValidRow)
		row = firstValidRow;
	else if(row > lastValidRow)
		row = lastValidRow;
	col = (x - screen->border - screen->scrollbar) / FontWidth(screen);
	if(col < 0)
		col = 0;
	else if(col > screen->max_col+1) {
		col = screen->max_col+1;
	}
	*r = row;
	*c = col;
}

static int
LastTextCol(row)
    register int row;
{
	register TScreen *screen =  &term->screen;
	register int i;
#ifdef KTERM
	register Bchr *ch;
#else /* !KTERM */
	register Char *ch;
#endif /* !KTERM */

	for ( i = screen->max_col,
#ifdef KTERM
	        ch = screen->buf[row + screen->topline] + i ;
	      i >= 0 && !(ch->attr & CHARDRAWN) ;
	      ch--, i--)
#else /* !KTERM */
	        ch = screen->buf[2 * (row + screen->topline) + 1] + i ;
	      i >= 0 && !(*ch & CHARDRAWN) ;
	      ch--, i--)
#endif /* !KTERM */
	    ;
	return(i);
}	

/*
** double click table for cut and paste in 8 bits
**
** This table is divided in four parts :
**
**	- control characters	[0,0x1f] U [0x80,0x9f]
**	- separators		[0x20,0x3f] U [0xa0,0xb9]
**	- binding characters	[0x40,0x7f] U [0xc0,0xff]
**  	- execeptions
*/
static int charClass[256] = {
/* NUL  SOH  STX  ETX  EOT  ENQ  ACK  BEL */
    32,   1,   1,   1,   1,   1,   1,   1,
/*  BS   HT   NL   VT   NP   CR   SO   SI */
     1,  32,   1,   1,   1,   1,   1,   1,
/* DLE  DC1  DC2  DC3  DC4  NAK  SYN  ETB */
     1,   1,   1,   1,   1,   1,   1,   1,
/* CAN   EM  SUB  ESC   FS   GS   RS   US */
     1,   1,   1,   1,   1,   1,   1,   1,
/*  SP    !    "    #    $    %    &    ' */
    32,  33,  34,  35,  36,  37,  38,  39,
/*   (    )    *    +    ,    -    .    / */
    40,  41,  42,  43,  44,  45,  46,  47,
/*   0    1    2    3    4    5    6    7 */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   8    9    :    ;    <    =    >    ? */
    48,  48,  58,  59,  60,  61,  62,  63,
/*   @    A    B    C    D    E    F    G */
    64,  48,  48,  48,  48,  48,  48,  48,
/*   H    I    J    K    L    M    N    O */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   P    Q    R    S    T    U    V    W */ 
    48,  48,  48,  48,  48,  48,  48,  48,
/*   X    Y    Z    [    \    ]    ^    _ */
    48,  48,  48,  91,  92,  93,  94,  48,
/*   `    a    b    c    d    e    f    g */
    96,  48,  48,  48,  48,  48,  48,  48,
/*   h    i    j    k    l    m    n    o */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   p    q    r    s    t    u    v    w */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   x    y    z    {    |    }    ~  DEL */
    48,  48,  48, 123, 124, 125, 126,   1,
/* x80  x81  x82  x83  IND  NEL  SSA  ESA */
     1,   1,   1,   1,   1,   1,   1,   1,
/* HTS  HTJ  VTS  PLD  PLU   RI  SS2  SS3 */
     1,   1,   1,   1,   1,   1,   1,   1,
/* DCS  PU1  PU2  STS  CCH   MW  SPA  EPA */
     1,   1,   1,   1,   1,   1,   1,   1,
/* x98  x99  x9A  CSI   ST  OSC   PM  APC */
     1,   1,   1,   1,   1,   1,   1,   1,
/*   -    i   c/    L   ox   Y-    |   So */
   160, 161, 162, 163, 164, 165, 166, 167,
/*  ..   c0   ip   <<    _        R0    - */
   168, 169, 170, 171, 172, 173, 174, 175,
/*   o   +-    2    3    '    u   q|    . */
   176, 177, 178, 179, 180, 181, 182, 183,
/*   ,    1    2   >>  1/4  1/2  3/4    ? */
   184, 185, 186, 187, 188, 189, 190, 191,
/*  A`   A'   A^   A~   A:   Ao   AE   C, */
    48,  48,  48,  48,  48,  48,  48,  48,
/*  E`   E'   E^   E:   I`   I'   I^   I: */
    48,  48,  48,  48,  48,  48,  48,  48,
/*  D-   N~   O`   O'   O^   O~   O:    X */ 
    48,  48,  48,  48,  48,  48,  48, 216,
/*  O/   U`   U'   U^   U:   Y'    P    B */
    48,  48,  48,  48,  48,  48,  48,  48,
/*  a`   a'   a^   a~   a:   ao   ae   c, */
    48,  48,  48,  48,  48,  48,  48,  48,
/*  e`   e'   e^   e:    i`  i'   i^   i: */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   d   n~   o`   o'   o^   o~   o:   -: */
    48,  48,  48,  48,  48,  48,  48,  248,
/*  o/   u`   u'   u^   u:   y'    P   y: */
    48,  48,  48,  48,  48,  48,  48,  48};

int SetCharacterClassRange (low, high, value)
    register int low, high;		/* in range of [0..255] */
    register int value;			/* arbitrary */
{

    if (low < 0 || high > 255 || high < low) return (-1);

    for (; low <= high; low++) charClass[low] = value;

    return (0);
}

/*
 * sets startSRow startSCol endSRow endSCol
 * ensuring that they have legal values
 */

static void
ComputeSelect(startRow, startCol, endRow, endCol, extend)
    int startRow, startCol, endRow, endCol;
    Bool extend;
{
	register TScreen *screen = &term->screen;
#ifdef KTERM
	register Bchr *ptr;
	Char gset;
#else /* !KTERM */
	register Char *ptr;
#endif /* !KTERM */
	register int length;
	register int class;
	int osc = startSCol;

	if (Coordinate(startRow, startCol) <= Coordinate(endRow, endCol)) {
		startSRow = startRRow = startRow;
		startSCol = startRCol = startCol;
		endSRow   = endRRow   = endRow;
		endSCol   = endRCol   = endCol;
	} else {	/* Swap them */
		startSRow = startRRow = endRow;
		startSCol = startRCol = endCol;
		endSRow   = endRRow   = startRow;
		endSCol   = endRCol   = startCol;
	}	

	switch (selectUnit) {
		case SELECTCHAR :
			if (startSCol > (LastTextCol(startSRow) + 1)) {
				startSCol = 0;
				startSRow++;
			}
			if (endSCol > (LastTextCol(endSRow) + 1)) {
				endSCol = 0;
				endSRow++;
			}
			break;
		case SELECTWORD :
			if (startSCol > (LastTextCol(startSRow) + 1)) {
				startSCol = 0;
				startSRow++;
			} else {
#ifdef KTERM
				ptr = screen->buf[startSRow+screen->topline]
				 + startSCol;
#ifdef KTERM_MBCC
				if (ptr->gset == MBC2)
				{	/* 2nd byte of a mbcs character */
					startSCol--;
					ptr--;
				}
				if (ptr->gset != MBC2 && (ptr->gset & MBCS) != 0)
				{	/* 1st byte of a mbcs character */
					class = mbcsCharClass (ptr);
					do
					{
						startSCol -= 2;
						ptr -= 2;
					} while (startSCol >= 0
					&& ptr->gset != MBC2 && (ptr->gset & MBCS) != 0
					&& class == mbcsCharClass (ptr));
					startSCol++;
					ptr++;
				}
				else if (ptr->gset == GSET_KANA)
					do
					{
						--startSCol;
						--ptr;
					} while (startSCol >= 0
					&& ptr->gset == GSET_KANA);
				else
				{
					gset = ptr->gset;
					class = charClass[ptr->code];
					do {
						--startSCol;
						--ptr;
					} while (startSCol >= 0
					&& ptr->gset == gset
				 	&& charClass[ptr->code] == class);
				}
#else	/* !KTERM_MBCC */
				class = charClass[ptr->code];
				do {
					--startSCol;
					--ptr;
				} while (startSCol >= 0
				 && charClass[ptr->code] == class);
#endif	/* !KTERM_MBCC */
#else /* !KTERM */
				ptr = screen->buf[2*(startSRow+screen->topline)]
				 + startSCol;
				class = charClass[*ptr];
				do {
					--startSCol;
					--ptr;
				} while (startSCol >= 0
				 && charClass[*ptr] == class);
#endif /* !KTERM */
				++startSCol;
			}
			if (endSCol > (LastTextCol(endSRow) + 1)) {
				endSCol = 0;
				endSRow++;
			} else {
				length = LastTextCol(endSRow);
#ifdef KTERM
				ptr = screen->buf[endSRow+screen->topline]
				 + endSCol;
#ifdef KTERM_MBCC
				if (ptr->gset == MBC2)
				{	/* 2nd byte of a mbcs character */
					endSCol--;
					ptr--;
				}
				if (ptr->gset != MBC2 && (ptr->gset & MBCS) != 0)
				{	/* 1st byte of a mbcs character */
					class = mbcsCharClass (ptr);
					do
					{
						endSCol += 2;
						ptr += 2;
					} while (endSCol < length
					&& ptr->gset != MBC2 && (ptr->gset & MBCS) != 0
					&& class == mbcsCharClass (ptr));
				}
				else if (ptr->gset == GSET_KANA)
					do
					{
						++endSCol;
						++ptr;
					} while (endSCol <= length
					&& ptr->gset == GSET_KANA);
				else
				{
					gset = ptr->gset;
					class = charClass[ptr->code];
					do {
						++endSCol;
						++ptr;
					} while (endSCol <= length
					&& ptr->gset == gset
				 	&& charClass[ptr->code] == class);
				}
#else	/* !KTERM_MBCC */
				class = charClass[ptr->code];
				do {
					++endSCol;
					++ptr;
				} while (endSCol <= length
				 && charClass[ptr->code] == class);
#endif	/* !KTERM_MBCC */
#else /* !KTERM */
				ptr = screen->buf[2*(endSRow+screen->topline)]
				 + endSCol;
				class = charClass[*ptr];
				do {
					++endSCol;
					++ptr;
				} while (endSCol <= length
				 && charClass[*ptr] == class);
#endif /* !KTERM */
				/* Word select selects if pointing to any char
				   in "word", especially in that it includes
				   the last character in a word.  So no --endSCol
				   and do special eol handling */
				if (endSCol > length+1) {
					endSCol = 0;
					++endSRow;
				}
			}
			break;
		case SELECTLINE :
			if (term->screen.cutToBeginningOfLine) {
			    startSCol = 0;
			} else if (!extend) {
			    startSCol = osc;
			}
			if (term->screen.cutNewline) {
			    endSCol = 0;
			    ++endSRow;
			} else {
			    endSCol = LastTextCol(endSRow) + 1;
			}
			break;
	}

	TrackText(startSRow, startSCol, endSRow, endSCol);
	return;
}

#if defined(KTERM) && defined(KTERM_MBCC)
/*
 * by Kiyoshi KANAZAWA, Nov. 29, 1990.
 *
 * MBCS is divided to ARABIAN-number, ENGLISH, HIRAGANA, KATAKANA,
 * KANJI (including both dai_1_suijun and dai_2_suijun), GREEK,
 * RUSSIAN, KEISEN and OTHERS.
 *
 * It is assumed that
 *     HIRAGANA, KATAKANA and KANJI belong to the same character class.
 *     ARABIAN-number and ENGLISH   belong to the same character class.
 *     Each character in OTHERS makes one character class by itself.
*/

#define MBCS_ARABIAN	0			/* arabia suuji		*/
#define MBCS_ENGLISH	MBCS_ARABIAN		/* eigo			*/
#define MBCS_HIRAGANA	(MBCS_ENGLISH + 1)	/* hiragana		*/
#define MBCS_KATAKANA	MBCS_HIRAGANA		/* katakana		*/
#define MBCS_KANJI	MBCS_KATAKANA		/* kanji		*/
#define MBCS_GREEK	(MBCS_KANJI + 1)	/* girisha moji		*/
#define MBCS_RUSSIAN	(MBCS_GREEK + 1)	/* roshia moji		*/
#define MBCS_KEISEN 	(MBCS_RUSSIAN + 1)	/* keisen		*/

static int mbcsCharClass (ptr)
register Bchr *ptr;
{
	register unsigned char	c1, c2;

	c2 = (ptr + 1)->code;
	switch (c1 = ptr->code)
	{
	case 0x21:
		switch (c2)
		{
		case 0x38:
		case 0x39:
		case 0x3a:
			return (MBCS_KANJI);
		case 0x3c:
			return (MBCS_KATAKANA);
		}
		break;
	case 0x23:
		if (0x30 <= c2 && c2 <= 0x39)
			return (MBCS_ARABIAN);
		if (0x41 <= c2 && c2 <= 0x5a || 0x61 <= c2 && c2 <= 0x7a)
			return (MBCS_ENGLISH);
		break;
	case 0x24:
		if (0x21 <= c2 && c2 <= 0x73)
			return (MBCS_HIRAGANA);
		break;
	case 0x25:
		if (0x21 <= c2 && c2 <= 0x76)
			return (MBCS_KATAKANA);
		break;
	case 0x26:
		if (0x21 <= c2 && c2 <= 0x38 || 0x41 <= c2 && c2 <= 0x58)
			return (MBCS_GREEK);
		break;
	case 0x27:
		if (0x21 <= c2 && c2 <= 0x41 || 0x51 <= c2 && c2 <= 0x71)
			return (MBCS_RUSSIAN);
		break;
	case 0x28:
		if (0x21 <= c2 && c2 <= 0x40)
			return (MBCS_KEISEN);
		break;
	default:
		if (0x30 <= c1 && c1 <= 0x4e && 0x21 <= c2 && c2 <= 0x7e
			|| c1 == 0x4f && (0x21 <= c2 || c2 <= 0x53)) /* dai_1_suijun */
			return (MBCS_KANJI);
		if (0x50 <= c1 && c1 <= 0x73 && 0x21 <= c2 && c2 <= 0x7e
			|| c1 == 0x74 && (0x21 <= c2 || c2 <= 0x24)) /* dai_2_suijun */
			return (MBCS_KANJI);
		break;
	}
	return ((c1 << 8) | c2);	/*	return mbcs code	*/
}

#endif	/* KTERM && KTERM_MBCC */

TrackText(frow, fcol, trow, tcol)
    register int frow, fcol, trow, tcol;
    /* Guaranteed (frow, fcol) <= (trow, tcol) */
{
	register int from, to;
	register TScreen *screen = &term->screen;
	int old_startrow, old_startcol, old_endrow, old_endcol;

	old_startrow = screen->startHRow;
	old_startcol = screen->startHCol;
	old_endrow = screen->endHRow;
	old_endcol = screen->endHCol;
	if (frow == old_startrow && fcol == old_startcol &&
	    trow == old_endrow   && tcol == old_endcol) return;
	screen->startHRow = frow;
	screen->startHCol = fcol;
	screen->endHRow   = trow;
	screen->endHCol   = tcol;
	from = Coordinate(frow, fcol);
	to = Coordinate(trow, tcol);
	if (to <= screen->startHCoord || from > screen->endHCoord) {
	    /* No overlap whatsoever between old and new hilite */
	    ReHiliteText(old_startrow, old_startcol, old_endrow, old_endcol);
	    ReHiliteText(frow, fcol, trow, tcol);
	} else {
	    if (from < screen->startHCoord) {
		    /* Extend left end */
		    ReHiliteText(frow, fcol, old_startrow, old_startcol);
	    } else if (from > screen->startHCoord) {
		    /* Shorten left end */
		    ReHiliteText(old_startrow, old_startcol, frow, fcol);
	    }
	    if (to > screen->endHCoord) {
		    /* Extend right end */
		    ReHiliteText(old_endrow, old_endcol, trow, tcol);
	    } else if (to < screen->endHCoord) {
		    /* Shorten right end */
		    ReHiliteText(trow, tcol, old_endrow, old_endcol);
	    }
	}
	screen->startHCoord = from;
	screen->endHCoord = to;
}

static void
ReHiliteText(frow, fcol, trow, tcol)
    register int frow, fcol, trow, tcol;
    /* Guaranteed that (frow, fcol) <= (trow, tcol) */
{
	register TScreen *screen = &term->screen;
	register int i;

	if (frow < 0)
	    frow = fcol = 0;
	else if (frow > screen->max_row)
	    return;		/* nothing to do, since trow >= frow */

	if (trow < 0)
	    return;		/* nothing to do, since frow <= trow */
	else if (trow > screen->max_row) {
	    trow = screen->max_row;
	    tcol = screen->max_col+1;
	}
	if (frow == trow && fcol == tcol)
		return;

	if(frow != trow) {	/* do multiple rows */
		if((i = screen->max_col - fcol + 1) > 0) {     /* first row */
		    ScrnRefresh(screen, frow, fcol, 1, i, True);
		}
		if((i = trow - frow - 1) > 0) {		       /* middle rows*/
		    ScrnRefresh(screen, frow+1, 0,i, screen->max_col+1, True);
		}
		if(tcol > 0 && trow <= screen->max_row) {      /* last row */
		    ScrnRefresh(screen, trow, 0, 1, tcol, True);
		}
	} else {		/* do single row */
		ScrnRefresh(screen, frow, fcol, 1, tcol - fcol, True);
	}
}

static _OwnSelection();

static void
SaltTextAway(crow, ccol, row, col, params, num_params)
    /*register*/ int crow, ccol, row, col;
    String *params;			/* selections */
    Cardinal num_params;
    /* Guaranteed that (crow, ccol) <= (row, col), and that both points are valid
       (may have row = screen->max_row+1, col = 0) */
{
	register TScreen *screen = &term->screen;
	register int i, j = 0;
	int eol;
#ifdef KTERM
	Ichr *line, *lp;
	Bchr *ch;
#else /* !KTERM */
	char *line, *lp;
#endif /* !KTERM */

	if (crow == row && ccol > col) {
	    int tmp = ccol;
	    ccol = col;
	    col = tmp;
	}

	--col;
	/* first we need to know how long the string is before we can save it*/

#ifdef KTERM
	ch = screen->buf[crow + screen->topline];
	if (ch[ccol].gset == MBC2)
	    ccol--;
	ch = screen->buf[row + screen->topline];
	if (ch[col].gset & MBCS && ch[col].gset != MBC2) /* MBC1 */
	    col++;
#endif /* !KTERM */
	if ( row == crow ) j = Length(screen, crow, ccol, col);
	else {	/* two cases, cut is on same line, cut spans multiple lines */
		j += Length(screen, crow, ccol, screen->max_col) + 1;
		for(i = crow + 1; i < row; i++) 
			j += Length(screen, i, 0, screen->max_col) + 1;
		if (col >= 0)
			j += Length(screen, row, 0, col);
	}
	
	/* now get some memory to save it in */

	if (screen->selection_size <= j) {
#ifdef KTERM
	    if((line = (Ichr *)malloc((unsigned)((j + 1) * sizeof(Ichr)))) == (Ichr *)NULL)
		SysError(ERROR_BMALLOC2);
	    XtFree((char *)screen->selection);
#else /* !KTERM */
	    if((line = malloc((unsigned) j + 1)) == (char *)NULL)
		SysError(ERROR_BMALLOC2);
	    XtFree(screen->selection);
#endif /* !KTERM */
	    screen->selection = line;
	    screen->selection_size = j + 1;
	} else line = screen->selection;
	if (!line || j < 0) return;

#ifdef KTERM
	line[j].code = '\0';		/* make sure it is null terminated */
	line[j].gset = 0;
#else /* !KTERM */
	line[j] = '\0';		/* make sure it is null terminated */
#endif /* !KTERM */
	lp = line;		/* lp points to where to save the text */
	if ( row == crow ) lp = SaveText(screen, row, ccol, col, lp, &eol);
	else {
		lp = SaveText(screen, crow, ccol, screen->max_col, lp, &eol);
		if (eol)
#ifdef KTERM
		{
			lp->code = '\n';
			lp++->gset = GSET_ASCII;
		}
#else /* !KTERM */
			*lp ++ = '\n';	/* put in newline at end of line */
#endif /* !KTERM */
		for(i = crow +1; i < row; i++) {
			lp = SaveText(screen, i, 0, screen->max_col, lp, &eol);
			if (eol)
#ifdef KTERM
			{
				lp->code = '\n';
				lp++->gset = GSET_ASCII;
			}
#else /* !KTERM */
				*lp ++ = '\n';
#endif /* !KTERM */
			}
		if (col >= 0)
			lp = SaveText(screen, row, 0, col, lp, &eol);
	}
#ifdef KTERM
	lp->code = '\0';		/* make sure we have end marked */
	lp->gset = 0;
#else /* !KTERM */
	*lp = '\0';		/* make sure we have end marked */
#endif /* !KTERM */
	
	screen->selection_length = (lp - line);
	_OwnSelection(term, params, num_params);
}

static Boolean ConvertSelection(w, selection, target,
				type, value, length, format)
Widget w;
Atom *selection, *target, *type;
XtPointer *value;
unsigned long *length;
int *format;
{
    Display* d = XtDisplay(w);
    XtermWidget xterm = (XtermWidget)w;

    if (xterm->screen.selection == NULL) return False; /* can this happen? */

    if (*target == XA_TARGETS(d)) {
	Atom* targetP;
	Atom* std_targets;
	unsigned long std_length;
	XmuConvertStandardSelection(
		    w, xterm->screen.selection_time, selection,
		    target, type, (caddr_t*)&std_targets, &std_length, format
		   );
	*length = std_length + 5;
	*value = (XtPointer)XtMalloc(sizeof(Atom)*(*length));
	targetP = *(Atom**)value;
	*targetP++ = XA_STRING;
	*targetP++ = XA_TEXT(d);
	*targetP++ = XA_COMPOUND_TEXT(d);
	*targetP++ = XA_LENGTH(d);
	*targetP++ = XA_LIST_LENGTH(d);
	bcopy((char*)std_targets, (char*)targetP, sizeof(Atom)*std_length);
	XtFree((char*)std_targets);
	*type = XA_ATOM;
	*format = 32;
	return True;
    }

    if (*target == XA_STRING ||
	*target == XA_TEXT(d) ||
	*target == XA_COMPOUND_TEXT(d)) {
#ifdef KTERM
	if (*target == XA_COMPOUND_TEXT(d) || *target == XA_TEXT(d)) {
	    *type = XA_COMPOUND_TEXT(d);
	    *length = convCStoCT(xterm->screen.selection, NULL);
	    *value = (caddr_t)XtMalloc(*length + 1);
	    (void)convCStoCT(xterm->screen.selection, (Char *)*value);
	} else {
	    *type = XA_STRING;
	    *length = convCStoLatin1(xterm->screen.selection, NULL);
	    *value = (caddr_t)XtMalloc(*length + 1);
	    (void)convCStoLatin1(xterm->screen.selection, (Char *)*value);
	}
#else /* !KTERM */
	if (*target == XA_COMPOUND_TEXT(d))
	    *type = *target;
	else
	    *type = XA_STRING;
	*value = xterm->screen.selection;
	*length = xterm->screen.selection_length;
#endif /* !KTERM */
	*format = 8;
	return True;
    }
    if (*target == XA_LIST_LENGTH(d)) {
	*value = XtMalloc(4);
	if (sizeof(long) == 4)
	    *(long*)*value = 1;
	else {
	    long temp = 1;
	    bcopy( ((char*)&temp)+sizeof(long)-4, (char*)*value, 4);
	}
	*type = XA_INTEGER;
	*length = 1;
	*format = 32;
	return True;
    }
    if (*target == XA_LENGTH(d)) {
	*value = XtMalloc(4);
	if (sizeof(long) == 4)
#ifdef KTERM
	    *(long*)*value = convCStoCT(xterm->screen.selection, NULL);
#else /* !KTERM */
	    *(long*)*value = xterm->screen.selection_length;
#endif /* !KTERM */
	else {
#ifdef KTERM
	    long temp = convCStoCT(xterm->screen.selection, NULL);
#else /* !KTERM */
	    long temp = xterm->screen.selection_length;
#endif /* !KTERM */
	    bcopy( ((char*)&temp)+sizeof(long)-4, (char*)*value, 4);
	}
	*type = XA_INTEGER;
	*length = 1;
	*format = 32;
	return True;
    }
    if (XmuConvertStandardSelection(w, xterm->screen.selection_time, selection,
				    target, type,
				    (caddr_t *)value, length, format))
	return True;

    /* else */
    return False;

}


static void LoseSelection(w, selection)
  Widget w;
  Atom *selection;
{
    register TScreen* screen = &((XtermWidget)w)->screen;
    register Atom* atomP;
    int i;
    for (i = 0, atomP = screen->selection_atoms;
	 i < screen->selection_count; i++, atomP++)
    {
	if (*selection == *atomP) *atomP = (Atom)0;
	switch (*atomP) {
	  case XA_CUT_BUFFER0:
	  case XA_CUT_BUFFER1:
	  case XA_CUT_BUFFER2:
	  case XA_CUT_BUFFER3:
	  case XA_CUT_BUFFER4:
	  case XA_CUT_BUFFER5:
	  case XA_CUT_BUFFER6:
	  case XA_CUT_BUFFER7:	*atomP = (Atom)0;
	}
    }

    for (i = screen->selection_count; i; i--) {
	if (screen->selection_atoms[i-1] != 0) break;
    }
    screen->selection_count = i;

    for (i = 0, atomP = screen->selection_atoms;
	 i < screen->selection_count; i++, atomP++)
    {
	if (*atomP == (Atom)0) {
	    *atomP = screen->selection_atoms[--screen->selection_count];
	}
    }

    if (screen->selection_count == 0)
	TrackText(0, 0, 0, 0);
}


#ifndef KTERM
/* ARGSUSED */
static void SelectionDone(w, selection, target)
Widget w;
Atom *selection, *target;
{
    /* empty proc so Intrinsics know we want to keep storage */
}
#endif /* !KTERM */


static /* void */ _OwnSelection(termw, selections, count)
    register XtermWidget termw;
    String *selections;
    Cardinal count;
{
    Atom* atoms = termw->screen.selection_atoms;
    int i;
    Boolean have_selection = False;

    if (termw->screen.selection_length < 0) return;

    if (count > termw->screen.sel_atoms_size) {
	XtFree((char*)atoms);
	atoms = (Atom*)XtMalloc(count*sizeof(Atom));
	termw->screen.selection_atoms = atoms;
	termw->screen.sel_atoms_size = count;
    }
    XmuInternStrings( XtDisplay((Widget)termw), selections, count, atoms );
    for (i = 0; i < count; i++) {
	int cutbuffer;
	switch (atoms[i]) {
	  case XA_CUT_BUFFER0: cutbuffer = 0; break;
	  case XA_CUT_BUFFER1: cutbuffer = 1; break;
	  case XA_CUT_BUFFER2: cutbuffer = 2; break;
	  case XA_CUT_BUFFER3: cutbuffer = 3; break;
	  case XA_CUT_BUFFER4: cutbuffer = 4; break;
	  case XA_CUT_BUFFER5: cutbuffer = 5; break;
	  case XA_CUT_BUFFER6: cutbuffer = 6; break;
	  case XA_CUT_BUFFER7: cutbuffer = 7; break;
	  default:	       cutbuffer = -1;
	}
	if (cutbuffer >= 0)
	    if ( termw->screen.selection_length >
		 4*XMaxRequestSize(XtDisplay((Widget)termw))-32)
		fprintf(stderr,
			"%s: selection too big (%d bytes), not storing in CUT_BUFFER%d\n",
			xterm_name, termw->screen.selection_length, cutbuffer);
	    else
#ifdef KTERM
	    {
		/* Since type of a CUT_BUFFER is STRING, KANJI and KANA
		 * characters can't be stored in a CUT_BUFFER
		 */
		int nw;
		int nc = 0;
		Ichr *p;
		char *s, *q;
		p = termw->screen.selection;
		for (nw = termw->screen.selection_length; nw > 0; nw--, p++) {
		    if (p->gset == GSET_ASCII)
			nc++;
		}
		if (nc > 0) {
		    char buf[256];
		    p = termw->screen.selection;
		    s = q = (nc > 256) ? XtMalloc(nc) : buf;
		    for (nw = termw->screen.selection_length; nw > 0; nw--, p++) {
			if (p->gset == GSET_ASCII)
			    *q++ = p->code & 0x7f;
		    }
		    XStoreBuffer( XtDisplay((Widget)termw), s, nc, cutbuffer );
		    if (s != buf) XtFree(s);
		} else
		    XStoreBuffer( XtDisplay((Widget)termw), NULL, nc, cutbuffer );
	    }
#else /* !KTERM */
		XStoreBuffer( XtDisplay((Widget)termw), termw->screen.selection,
			      termw->screen.selection_length, cutbuffer );
#endif /* !KTERM */
	else if (!replyToEmacs) {
	    have_selection |=
		XtOwnSelection( (Widget)termw, atoms[i],
			    termw->screen.selection_time,
#ifdef KTERM
			    ConvertSelection, LoseSelection, NULL );
#else /* !KTERM */
			    ConvertSelection, LoseSelection, SelectionDone );
#endif /* !KTERM */
	}
    }
    if (!replyToEmacs)
	termw->screen.selection_count = count;
    if (!have_selection)
	TrackText(0, 0, 0, 0);
}

/* void */
DisownSelection(termw)
    register XtermWidget termw;
{
    Atom* atoms = termw->screen.selection_atoms;
    Cardinal count = termw->screen.selection_count;
    int i;

    for (i = 0; i < count; i++) {
	int cutbuffer;
	switch (atoms[i]) {
	  case XA_CUT_BUFFER0: cutbuffer = 0; break;
	  case XA_CUT_BUFFER1: cutbuffer = 1; break;
	  case XA_CUT_BUFFER2: cutbuffer = 2; break;
	  case XA_CUT_BUFFER3: cutbuffer = 3; break;
	  case XA_CUT_BUFFER4: cutbuffer = 4; break;
	  case XA_CUT_BUFFER5: cutbuffer = 5; break;
	  case XA_CUT_BUFFER6: cutbuffer = 6; break;
	  case XA_CUT_BUFFER7: cutbuffer = 7; break;
	  default:	       cutbuffer = -1;
	}
	if (cutbuffer < 0)
	    XtDisownSelection( (Widget)termw, atoms[i],
			       termw->screen.selection_time );
    }
    termw->screen.selection_count = 0;
    termw->screen.startHRow = termw->screen.startHCol = 0;
    termw->screen.endHRow = termw->screen.endHCol = 0;
}


/* returns number of chars in line from scol to ecol out */
/* ARGSUSED */
static int
Length(screen, row, scol, ecol)
    register int row, scol, ecol;
    register TScreen *screen;
{
        register int lastcol = LastTextCol(row);

	if (ecol > lastcol)
	    ecol = lastcol;
	return (ecol - scol + 1);
}

/* copies text into line, preallocated */
#ifdef KTERM
static Ichr *
#else /* !KTERM */
static char *
#endif /* !KTERM */
SaveText(screen, row, scol, ecol, lp, eol)
    int row;
    int scol, ecol;
    TScreen *screen;
#ifdef KTERM
    register Ichr *lp;		/* pointer to where to put the text */
#else /* !KTERM */
    register char *lp;		/* pointer to where to put the text */
#endif /* !KTERM */
    int *eol;
{
	register int i = 0;
#ifdef KTERM
	register Bchr *ch = screen->buf[row + screen->topline];
	register Char g;
#else /* !KTERM */
	register Char *ch = screen->buf[2 * (row + screen->topline)];
#endif /* !KTERM */
	Char attr;
	register int c;

	*eol = 0;
	i = Length(screen, row, scol, ecol);
	ecol = scol + i;
	if (*eol == 0) {
		if(ScrnGetAttributes(screen, row + screen->topline, 0, &attr, 1) == 1) {
			*eol = (attr & LINEWRAPPED) ? 0 : 1;
		} else {
			/* If we can't get the attributes, assume no wrap */
			/* CANTHAPPEN */
			(void)fprintf(stderr, "%s: no attributes for %d, %d\n",
				xterm_name, row, ecol - 1);
			*eol = 1;
		}
	}
	for (i = scol; i < ecol; i++) {
#ifdef KTERM
		c = ch[i].code & ~NEEDMAP;
		g = ch[i].gset;
		if (c < ' ' || c == 0x7f && !(g & CS96)) {
			lp->code = ' ';
			lp->gset = GSET_ASCII;
		} else {
			lp->code = c;
			lp->gset = g;
		}
		lp++;
#else /* !KTERM */
	        c = ch[i];
		if (c == 0)
			c = ' ';
		else if(c < ' ') {
			if(c == '\036')
				c = '#'; /* char on screen is pound sterling */
			else
				c += 0x5f; /* char is from DEC drawing set */
		} else if(c == 0x7f)
			c = 0x5f;
		*lp++ = c;
#endif /* !KTERM */
	}
	return(lp);
}

static void
EditorButton(event)
    register XButtonEvent *event;
{
	register TScreen *screen = &term->screen;
	int pty = screen->respond;
	char line[6];
	register unsigned row, col;
	int button; 

	button = event->button - 1; 

	row = (event->y - screen->border) 
	 / FontHeight(screen);
	col = (event->x - screen->border - screen->scrollbar)
	 / FontWidth(screen);
	(void) strcpy(line, "\033[M");
	if (screen->send_mouse_pos == 1) {
		line[3] = ' ' + button;
	} else {
		line[3] = ' ' + (KeyState(event->state) << 2) + 
			((event->type == ButtonPress)? button:3);
	}
	line[4] = ' ' + col + 1;
	line[5] = ' ' + row + 1;
	v_write(pty, line, 6);
}


/*ARGSUSED*/
void HandleGINInput (w, event, param_list, nparamsp)
    Widget w;
    XEvent *event;
    String *param_list;
    Cardinal *nparamsp;
{
    if (term->screen.TekGIN && *nparamsp == 1) {
	int c = param_list[0][0];
	switch (c) {
	  case 'l': case 'm': case 'r':
	  case 'L': case 'M': case 'R':
	    break;
	  default:
	    Bell ();			/* let them know they goofed */
	    c = 'l';			/* provide a default */
	}
	TekEnqMouse (c | 0x80);
	TekGINoff();
    } else {
	Bell ();
    }
}


/* ARGSUSED */
void HandleSecure(w, event, params, param_count)
    Widget w;
    XEvent *event;		/* unused */
    String *params;		/* [0] = volume */
    Cardinal *param_count;	/* 0 or 1 */
{
    Time time = CurrentTime;

    if ((event->xany.type == KeyPress) ||
	(event->xany.type == KeyRelease))
	time = event->xkey.time;
    else if ((event->xany.type == ButtonPress) ||
	     (event->xany.type == ButtonRelease))
      time = event->xbutton.time;
    DoSecureKeyboard (time);
}
