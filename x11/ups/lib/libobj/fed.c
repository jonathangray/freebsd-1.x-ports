/* fed.c - field editing package */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/*  The routines here are meant to make field editing easier.
 */

char shared_fed_c_sccsid[] = "@(#)fed.c	1.21 26/7/92 (UKC)";

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>

#include <local/ukcprog.h>
#include "fed.h"

static void get_act PROTO((struct edescst *edesc));
static void preprocess_key PROTO((struct edescst *edesc));
static void preprocess_puck PROTO((struct edescst *edesc));
static int cancel_edit PROTO((struct edescst *edesc));
static void paste_selection PROTO((edesc_t *edesc));
static void redraw_string PROTO((edesc_t *edesc));
static char *make_copy PROTO((const char *s, int len));

/*  Event mask for wn_next_event.
 */
#define EVENT_MASK	(EV_BUTTON_UP | EV_BUTTON_DOWN | EV_MOUSE_MOVED | \
					EV_KEY | EV_INTERRUPT | \
					EV_WINDOW_EXPOSED | EV_WINDOW_RESIZED)

static char *
make_copy(s, len)
const char *s;
int len;
{
	return strncpy(e_malloc(len + 1), s, len);
}

/*  Initialise struct edescst. Set ed_{wn,orig,maxlen,scrlen}
 *  to the given values. Make a copy of orig in ed_copy, and set
 *  ed_curlen to the length of orig.
 */
void
make_edesc(edesc, wn, orig, maxlen, fg, bg)
register struct edescst *edesc;
window_t wn;
const char *orig;
int maxlen, fg, bg;
{
	edesc->ed_wn = wn;
	edesc->ed_font = (wn == -1) ? NULL : wn_get_sysfont();
	edesc->ed_fg = fg;
	edesc->ed_bg = bg;
	edesc->ed_orig = orig;
	edesc->ed_maxlen = maxlen;
	edesc->ed_scrlen = 0;
	edesc->ed_flags = 0; /* cursor off, line cursor, insert mode */
	edesc->ed_copy = make_copy(orig, maxlen);
	edesc->ed_copy[maxlen] = '\0';
	edesc->ed_curlen = strlen(edesc->ed_copy);
	edesc->ed_quitfunc = NULL;
	edesc->ed_keyfunc = NULL;
	edesc->ed_puckfunc = NULL;
	edesc->ed_expose_handler = NULL;
}

/*  Get the next user action using wn_next_event().
 *  Set the fields ed_{act,char,x,y,buttons} of *edesc appropriately.
 */
static void
get_act(edesc)
register struct edescst *edesc;
{
	event_t event;

	wn_next_event(edesc->ed_wn, EVENT_MASK, &event);
	if (event.ev_type == EV_KEY) {
		edesc->ed_act = EDA_KEY;
		edesc->ed_char = event.ev_char;
	}
	else if ((event.ev_type & (EV_WINDOW_RESIZED | EV_WINDOW_EXPOSED)) ||
		 event.ev_buttons & B_ANY) {
		edesc->ed_act = EDA_PUCK;
		edesc->ed_puck_x = event.ev_x;
		edesc->ed_puck_y = event.ev_y;
		edesc->ed_buttons = event.ev_buttons |
				    (event.ev_type & (EV_BUTTON_UP|EV_BUTTON_DOWN));
	}
	else
		edesc->ed_act = EDA_NONE;
}

/*  Do the default processing for key ch. Interprets the characters
 *  RETURN (confirm edit), DEL (delete last character), and OOPS
 *  (delete to start of line).
 *  If the character is any of these, return EDM_INSERT_CHAR if it
 *  is printable, otherwise EDM_ILLEGAL_CHAR.
 */
static void
preprocess_key(edesc)
register struct edescst *edesc;
{
	int meaning;
	
	switch(edesc->ed_char) {
	case RETURN:
	case ESC:
		edesc->ed_flags &= ~EDF_UNGETPUCK;
		meaning = EDM_CONFIRM;
		break;
	case DEL:
		meaning = EDM_DELETE_CHAR;
		break;
	case OOPS:
		meaning = EDM_DELETE_LINE;
		break;
	case CONTROL_C:
		meaning = EDM_CANCEL;
		break;
	case CONTROL_L:
		meaning = EDM_FORWARD_SPACE;
		break;
	case BACKSPACE:
		meaning = EDM_BACK_SPACE;
		break;
	default:
		meaning = (edesc->ed_char < 32) ? EDM_ILLEGAL_CHAR
						: EDM_INSERT_CHAR;
		break;
	}
	edesc->ed_meaning = meaning;
}

/*  Do default processing for the puck. B_MIDDLE indicates new text
 *  cursor position. A press of B_MIDDLE outside the field area or
 *  a press of B_RIGHT or B_LEFT anywhere means end of edit.
 *
 *  x,y, and buttons are the values of edesc->ed_{x,y,buttons},
 */
static void
preprocess_puck(edesc)
register struct edescst *edesc;
{
	int x, y, buttons;
	unsigned long resize_type;
	
	x = edesc->ed_puck_x;
	y = edesc->ed_puck_y;
	buttons = edesc->ed_buttons & (B_ANY | B_SHIFT_KEY);
	resize_type = wn_get_resize_event(edesc->ed_wn);
	if (resize_type == EV_WINDOW_EXPOSED && edesc->ed_expose_handler != NULL) {
		ed_cursor(edesc, EDC_CURSOR_OFF);
		(*edesc->ed_expose_handler)(resize_type, TRUE);
		redraw_string(edesc);
		edesc->ed_meaning = EDM_CONT;
	}
	else if (resize_type != 0) {
		event_t event;

		edesc->ed_meaning = EDM_CANCEL;

		/*  We just consumed the resize event, so put it back.
		 */
		event.ev_wn = edesc->ed_wn;
		event.ev_type = resize_type;
		event.ev_x = x;
		event.ev_y = y;
		wn_pushback_event(&event);
	}
	else if (buttons == (B_MIDDLE | B_SHIFT_KEY)) {
		edesc->ed_meaning = EDM_PASTE_SELECTION;
		wn_wait_for_release_of(edesc->ed_wn, B_MIDDLE);
	}
	else if (buttons & (B_RIGHT | B_LEFT) ||
			((edesc->ed_buttons & B_STATE_CHANGE) &&
			       (x < 0 || x >= wn_get_width(edesc->ed_wn) ||
				y < 0 || y >= wn_get_height(edesc->ed_wn)))) {
		edesc->ed_flags |= EDF_UNGETPUCK;
		edesc->ed_meaning = EDM_CONFIRM;
	}
	else if (buttons == B_MIDDLE) {
		edesc->ed_meaning = EDM_SETCURSOR;
		edesc->ed_newpos = wn_strpos(edesc->ed_copy, x, edesc->ed_font,
				       !(edesc->ed_flags & EDF_BLOCK_CURSOR));
	}
	else
		panic("no buttons pressed in preprocess_puck");
}

/*  Do the edit indicated by the ed_meaning field of edesc
 */
void
do_edit(edesc, meaning)
register struct edescst *edesc;
int meaning;
{
	int ch;
	
	switch(meaning) {
	case EDM_ILLEGAL_CHAR:
		ch = edesc->ed_char;
		if (isascii(ch) && isprint(ch))
			errf("illegal character");
		else if (ch >= 0 && ch < 32)
			errf("illegal character ^%c", ch + 64);
		else
			errf("illegal character <%d>", ch);
		break;
	case EDM_SETCURSOR:
		/*  move editing cursor to ed_newpos */
		if (edesc->ed_newpos > edesc->ed_curlen)
			edesc->ed_newpos = edesc->ed_curlen;
		else if (edesc->ed_newpos < 0)
			edesc->ed_newpos = 0;
		ed_cursor(edesc, EDC_CURSOR_OFF);
		edesc->ed_curpos = edesc->ed_newpos;
		ed_cursor(edesc, EDC_CURSOR_ON);
		break;
	case EDM_PASTE_SELECTION:
		paste_selection(edesc);
		break;
	default:
		process_key(edesc, meaning);
		break;
	}
}

static void
paste_selection(edesc)
edesc_t *edesc;
{
	const char *buf;
	int i, nbytes;

	wn_get_selection(&buf, &nbytes);

	for (i = 0; i < nbytes; ++i) {
		edesc->ed_char = buf[i];

		preprocess_key(edesc);
		if (edesc->ed_keyfunc != NULL)
			(*edesc->ed_keyfunc)(edesc);

		if (edesc->ed_meaning != EDM_INSERT_CHAR) {
			wn_bell(edesc->ed_wn);
			break;
		}
		
		if (process_key(edesc, edesc->ed_meaning) != 0)
			break;
	}
}

/*  Field editing routine. This is called to edit a field, and
 *  only returns when the edit is confirmed or cancelled (except
 *  when the EDF_ONETRIP bit is set - see below).
 *
 *  We go round in a loop, calling get_act() to get a user action,
 *  which can be a puck press or a key press. The raw input record
 *  is interpreted by preprocess_puck() or preprocess_key(), then
 *  passed to the ed_key() or ed_puck() function given in edesc so
 *  that the user can alter the default interpretation.
 *  At this point, the meaning of the action is recorded in the
 *  ed_meaning field, and do_edit() is called to perform the edit.
 *  unless the meaning is EDM_CONT, EDM_CANCEL or EDM_CONFIRM.  
 *  ... more comment needed
 */
int
edit_field(edesc)
register struct edescst *edesc;
{
	int n_quit_attempts, meaning, res, quit;
	
	n_quit_attempts = 0;
	quit = FALSE;
	do {
		if (!(edesc->ed_flags & EDF_ONETRIP))
			do {
				get_act(edesc);
			} while (edesc->ed_act == EDA_NONE);
		if (edesc->ed_act == EDA_KEY) {
			preprocess_key(edesc);
			if (edesc->ed_keyfunc != NULL)
				(*edesc->ed_keyfunc)(edesc);
		}
		else {
			preprocess_puck(edesc);
			if (edesc->ed_puckfunc != NULL)
				(*edesc->ed_puckfunc)(edesc);
		}
		switch(meaning = edesc->ed_meaning) {
		case EDM_CONFIRM:
			ed_cursor(edesc, EDC_CURSOR_OFF);
			res = (*edesc->ed_quitfunc)(edesc, n_quit_attempts);
			if (res == EDR_CANT_QUIT)
				n_quit_attempts++;
			quit =  res == EDR_CONFIRM_CHANGE ||
				res == EDR_CONFIRM_NO_CHANGE ||
				res == EDR_CANCEL_AND_QUIT;
			if (!quit)
				ed_cursor(edesc, EDC_CURSOR_ON);
			break;
		case EDM_CONT:
			res = EDR_CONT;
			break;
		case EDM_CANCEL:
			res = EDR_CANCEL;
			break;
		default:
			do_edit(edesc, meaning);
			n_quit_attempts = 0;
			res = EDR_CONT;
			break;
		}
		if (meaning == EDM_CANCEL || res == EDR_CANCEL ||
					     res == EDR_CANCEL_AND_QUIT)
			quit = cancel_edit(edesc);
	} while (!(quit || (edesc->ed_flags & EDF_ONETRIP)));
	
	/*  unget the puck record if the input record can be
	 *  used by the next process to call wn_getpuck.
	 */
	if (quit && (edesc->ed_flags & EDF_UNGETPUCK))
		wn_ungetpuck(edesc->ed_wn, edesc->ed_puck_x,
					edesc->ed_puck_y, edesc->ed_buttons);
	return res;
}

/*  Replace ed_orig with new_orig.
 */
void
ed_new_orig(edesc, new_orig)
struct edescst *edesc;
const char *new_orig;
{
	edesc->ed_orig = new_orig;
}

/*  Replace the string we are editing with s, and put the cursor
 *  at the position indicated by newpos.
 */
void
ed_newstring(edesc, s)
struct edescst *edesc;
const char *s;
{
	ed_cursor(edesc, EDC_CURSOR_OFF);
	if (edesc->ed_copy != NULL)
		free(edesc->ed_copy);
	edesc->ed_copy = make_copy(s, edesc->ed_maxlen);
	edesc->ed_curlen = strlen(s);
	redraw_string(edesc);
}			

static void
redraw_string(edesc)
edesc_t *edesc;
{
	int wn;

	edesc->ed_newpos = edesc->ed_curpos;

	/*  Sometimes ups wants to run without a window ...
	 */
	if ((wn = edesc->ed_wn) == -1)
		return;

	wn_set_area(wn, 0, 0, wn_get_width(wn), wn_get_height(wn), WN_BG);
	wn_text(wn, edesc->ed_font, edesc->ed_copy,
				0, 0, edesc->ed_fg, edesc->ed_bg, WN_USE_TOP);
	do_edit(edesc, EDM_SETCURSOR);
}

/*  Cancel an edit. Restore the original string on screen, update the
 *  working copy from the original string, and ensure that the text
 *  cursor is still in bounds (the original string may have been shorter
 *  than the cancelled buffer).
 *  Return whether we should finish the edit
 */
static int
cancel_edit(edesc)
register struct edescst *edesc;
{
	int oldwidth, newwidth, height;
	font_t *font;
	window_t wn;
	
	ed_cursor(edesc, EDC_CURSOR_OFF);
	wn = edesc->ed_wn;
	font = edesc->ed_font;
	oldwidth = wn_strwidth(edesc->ed_copy, font);
	newwidth = wn_strwidth(edesc->ed_orig, font);
	height = font->ft_height;
	wn_set_area(wn, 0, 0, oldwidth, height, WN_BG);
	if (edesc->ed_scrlen != 0)
		wn_move_area(wn, oldwidth, 0, edesc->ed_scrlen, height,
							newwidth, 0, WN_BG);
	wn_text(wn, font, edesc->ed_orig, 0, 0, edesc->ed_fg, edesc->ed_bg, WN_USE_TOP);
	(void) strcpy(edesc->ed_copy, edesc->ed_orig);
	edesc->ed_curlen = strlen(edesc->ed_copy);
	if (edesc->ed_curpos > edesc->ed_curlen)
		edesc->ed_curpos = edesc->ed_curlen;
	if (edesc->ed_flags & EDF_CONT_ON_CANCEL) {
		ed_cursor(edesc, EDC_CURSOR_ON);
		return FALSE;
	}
	return TRUE;
}

/*  Process EDM_DELETE_LINE, EDM_DELETE_CHAR, EDM_INSERT_CHAR,
 *  EDM_FORWARD_SPACE, and EDM_BACK_SPACE.
 *
 *  Call smove() to update the working copy, and ed_cursor() to update
 *  the text cursor, as necessary.
 *
 *  Beep for DEL/OOPS at the start of a line, and normal characters
 *  when we have reached the maximum buffer length
 */
int
process_key(edesc, meaning)
register struct edescst *edesc;
int meaning;
{
	window_t wn;
	int pos, newpos, newlen, do_move, ch, overwrite;
	font_t *font;
	int old_x, new_x, width, height;
	char *s, buf[2];
	
	pos = edesc->ed_curpos;
	ch = edesc->ed_char;
	switch(meaning) {
	case EDM_DELETE_LINE:
		newpos = (pos == 0) ? -1 : 0;
		do_move = TRUE;
		break;
	case EDM_INSERT_CHAR:
		newpos = pos + 1;
		do_move = TRUE;
		break;
	case EDM_FORWARD_SPACE:
		newpos = pos + 1;
		do_move = FALSE;
		break;
	case EDM_BACK_SPACE:
	case EDM_DELETE_CHAR:
		newpos = pos - 1;
		do_move = meaning == EDM_DELETE_CHAR;
		break;
	default:
		panic("unknown meaning type in process_key");
		newpos = 0; /* to satisfy gcc */
		do_move = 0; /* to satisfy gcc */
	}

	overwrite = (edesc->ed_flags & EDF_OVERWRITE) ? 1 : 0;
	newlen = edesc->ed_curlen;

	if (do_move && !overwrite)
		newlen += newpos - pos;

	if (newpos < 0 || newpos > newlen || newlen > edesc->ed_maxlen) {
		wn_bell(edesc->ed_wn);
		return -1;
	}

	s = edesc->ed_copy;
	font = edesc->ed_font;
	wn = edesc->ed_wn;
	height = font->ft_height;

	wn_updating_off(wn);
	ed_cursor(edesc, EDC_CURSOR_OFF);
	old_x = wn_strnwidth(s, pos, font);

	if (do_move && !overwrite) {
		int i;

		if (meaning == EDM_INSERT_CHAR) {
			*buf = ch;
			new_x = old_x + wn_strnwidth(buf, 1, font);
		}
		else
			new_x = wn_strnwidth(s, newpos, font);
		width = wn_strwidth(s + pos, font) + edesc->ed_scrlen;

		/*  Shunt the bytes.  Note that we copy the final NUL here.
		 *  We can't use memcpy because the regions overlap.
		 *  We can't use memmove because it is not widespread enough.
		 */
		if (newpos > pos) {
			for (i = edesc->ed_curlen; i >= pos; --i)
				s[i + (newpos - pos)] = s[i];
		}
		else {
			for (i = pos; i <= edesc->ed_curlen; ++i)
				s[i - (pos - newpos)] = s[i];
		}

		wn_move_area(wn, old_x,0, width,height, new_x,0, edesc->ed_bg);
		edesc->ed_curlen = newlen;
	}

	if (meaning == EDM_INSERT_CHAR) {
		buf[0] = ch;
		buf[1] = '\0';
		wn_text(wn, font, buf, old_x, 0,
					edesc->ed_fg, edesc->ed_bg, WN_USE_TOP);
		edesc->ed_copy[pos] = ch;
	}

	edesc->ed_curpos = newpos;
	ed_cursor(edesc, EDC_CURSOR_ON);
	wn_updating_on(wn);

	return 0;
}

/*  Turn the editing cursor on or off. Use colour INVERT so same set_area
 *  call is used for on and off. Don't need to do anything if the cursor
 *  is already off and on_or_off is off.
 *  Does block or vertical bar cursor.
 */
void
ed_cursor(edesc, cursor_on)
register struct edescst *edesc;
int cursor_on;
{
	font_t *font;
	int fg, bg, pos, ch;
	char *s, buf[2];

	if (!(edesc->ed_flags & EDF_CURSOR_ON) == cursor_on) {
		font = edesc->ed_font;
		s = edesc->ed_copy;
		pos = edesc->ed_curpos;
		if (edesc->ed_flags & EDF_BLOCK_CURSOR) {
			ch = (s[pos] != '\0') ? s[pos] : ' ';
			if (cursor_on) {
				fg = edesc->ed_bg;
				bg = edesc->ed_fg;
			}
			else {
				fg = edesc->ed_fg;
				bg = edesc->ed_bg;
			}
			buf[0] = ch;
			buf[1] = '\0';
			wn_text(edesc->ed_wn, font, buf,
					wn_strnwidth(s, pos, font), 0,
					fg, bg, WN_USE_TOP);
		}
		else	
			wn_set_area(edesc->ed_wn,
					wn_strnwidth(s, pos, font), 0,
					2, font->ft_height, INVERT);
		edesc->ed_flags ^= EDF_CURSOR_ON;
	}
}	
