/* ui_src.c - input handling in the source window */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ui_src_c_sccsid[] = "@(#)ui_src.c	1.30 13/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/menu3.h>

#include <local/ukcprog.h>
#include <local/obj/fed.h>
#include "ups.h"
#include "symtab.h"
#include "src.h"
#include "tdr.h"
#include "ui.h"
#include "ui_priv.h"
#include "cursors.h"
#include "menudata.h"

#include "expr.h"
#include "obj_bpt.h"
#include "exec.h"
#include "state.h"

#define MAXVARLEN	40

typedef enum { QR_CONFIRM, QR_NEWLINE, QR_DEL, QR_OTHER } quit_reason_t;

static void confirm_var PROTO((event_t *ev, srcwin_id_t srcwin_id,
			       fil_t *fil, int lnum, const char *text,
			       const char *start, const char *end));
static void edit_editblock PROTO((event_t *ev, srcwin_id_t srcwin_id, srcinfo_t *si));
static int editblock_quitfunc PROTO((edesc_t *edesc, int n_tries));
static void editblock_keyfunc PROTO((edesc_t *edesc));
static int get_edit_tabspaces PROTO((void));
static int update_lines PROTO((srcinfo_t *si, editblockinfo_t *ei, int lnum,
			       const char *line, int nlines, lexinfo_t *lx));

/*  Give feedback on the selection of a variable in the source
 *  window.  Text points to the string containing the line being
 *  selected; start and end point to the start and end of the
 *  variable name.
 *
 *  We draw a box round the variable name which is lit only when
 *  when the mouse is over the name.  If the button is released
 *  with the box displayed, we call show_var() or show_func() to
 *  try to display the variable.
 */
static void
confirm_var(ev, srcwin_id, fil, lnum, text, start, end)
event_t *ev;
srcwin_id_t srcwin_id;
fil_t *fil;
int lnum;
const char *text, *start, *end;
{
	char *cptr;
	int have_box, in_word;
	srcinfo_t sibuf;
	char buf[MAXVARLEN + 1];

	have_box = FALSE;
	in_word = TRUE;
	for(;;) {
		if (in_word != have_box) {
			src_draw_box(srcwin_id, lnum, start - text, end - start);
			have_box = !have_box;
		}

		wn_next_event(ev->ev_wn, EVENT_MASK, ev);
		if (!(ev->ev_buttons & B_LEFT))
			break;

		src_get_info(srcwin_id, ev->ev_x, ev->ev_y, &sibuf);
		in_word = sibuf.si_cnum != -1 && sibuf.si_src_lnum == lnum &&
			  sibuf.si_text + sibuf.si_cnum >= start &&
			  sibuf.si_text + sibuf.si_cnum < end;
	}

	if (have_box) {
		cursor_t old_cursor;

		old_cursor = wn_get_window_cursor(WN_STDWIN);
		set_bm_cursor(WN_STDWIN, CU_WAIT);

		src_draw_box(srcwin_id, lnum, start - text, end - start);
		(void) strncpy(buf, start, end - start);

		buf[end - start] = '\0';

		/*  f77 maps all upper case variable names to
		 *  lower case, so follow suit.
		 */
		if (fil->fi_language == LANG_FORTRAN) {
			for (cptr = buf; *cptr != '\0'; ++cptr)	
				if (isupper(*cptr))
					*cptr = tolower(*cptr);
		}

		td_record_show_var(fil, lnum, buf);
		show_var(srcwin_id, fil, lnum, buf);	

		wn_define_cursor(WN_STDWIN, old_cursor);
	}
}

/*  Display the name and lnum as the source file name and line
 *  number.
 *
 *  BUG: this function is a quick hack, full of magic numbers.
 */
void
echo_src_name_and_lnum(srcwin_id, name, lnum)
srcwin_id_t srcwin_id;
const char *name;
int lnum;
{
	static char old[60];
	char nbuf[60];
	int wn, nchars, name_nchars, namelen, width, height;
	font_t *font;

	wn = (int)src_get_data(srcwin_id);
	if (name == NULL || lnum == -1)
		return;

	wn_get_window_size(wn, &width, &height);
	font = wn_get_sysfont();
	nchars = (width - 8) / font->ft_width;
	if (nchars > sizeof(nbuf) - 1)
		nchars = sizeof(nbuf) - 1;

	namelen = strlen(name);
	name_nchars = nchars - 5;
	if (strlen(name) < name_nchars)
		sprintf(nbuf, "%*s:%-4d", name_nchars, name, lnum % 10000);
	else
		sprintf(nbuf, "%s:%-4d", name + namelen - name_nchars, lnum % 10000);

	if (strcmp(nbuf, old) != 0) {
		wn_tputs(wn, nbuf, 4, (height - font->ft_height) / 2);
		(void) strcpy(old, nbuf);
	}
}

/*  Mouse hole caption function for the source window.  No captions
 *  displayed if there is no source in the source window.
 */
int
mfn_source(caps, arg)
int caps;
char *arg;
{
	srcinfo_t sibuf;

	src_get_info((srcwin_id_t )arg, 0, 0, &sibuf);
	return (sibuf.si_fil != NULL) ? caps : 0;
}

static int
get_edit_tabspaces()
{
	font_t *srcfont, *editfont;
	int tabspaces;

	src_get_fonts(get_current_srcwin(), &srcfont, &editfont);

	tabspaces = get_tabwidth();
	if (editfont->ft_width != 0)
		tabspaces = (tabspaces * srcfont->ft_width) / editfont->ft_width;
	
	return tabspaces;
}

/*  Keyboard input function source edit blocks.
 */
static void
editblock_keyfunc(edesc)
edesc_t *edesc;
{
	int i, tabspaces, nspaces;

	if (edesc->ed_act != EDA_KEY)
		return;

	switch (edesc->ed_char) {
	case RETURN:
		edesc->ed_user = (int)QR_NEWLINE;
		break;
	case OOPS:
	case DEL:
		if (edesc->ed_curpos == 0) {
			edesc->ed_user = (int)QR_DEL;
			edesc->ed_meaning = EDM_CONFIRM;
		}
		break;
	case CONTROL('D'):
		/*  Sort of do what ^D does in input mode in vi.
		 *
		 *  Sorry if you use EMACS.  I wrote it, I get to
		 *  to choose what editor it is almost, but not quite
		 *  entirely unlike.
		 */

		if (edesc->ed_curpos == 0)
			break;
		
		for (i = 0; i < edesc->ed_curpos; ++i) {
			if (edesc->ed_copy[i] != ' ')
				break;
		}
		if (i < edesc->ed_curpos)
			break;

		tabspaces = get_edit_tabspaces();
		wn_updating_off(edesc->ed_wn);
		do {
			do_edit(edesc, EDM_DELETE_CHAR);
		} while (edesc->ed_curpos % tabspaces != 0);
		wn_updating_on(edesc->ed_wn);

		edesc->ed_meaning = EDM_CONT;
		break;
	case '\t':
		tabspaces = get_edit_tabspaces();
		nspaces = tabspaces - edesc->ed_curpos % tabspaces;

		edesc->ed_char = ' ';
		wn_updating_off(edesc->ed_wn);
		while (--nspaces >= 0)
			do_edit(edesc, EDM_INSERT_CHAR);
		wn_updating_on(edesc->ed_wn);

		/*  Make the meaning a NOP.
		 */
		edesc->ed_meaning = EDM_CONT;
		break;
	
	default:
		break;
	}
}

static int
editblock_quitfunc(edesc, n_tries)
edesc_t *edesc;
int n_tries;
{
	if (strcmp(edesc->ed_copy, edesc->ed_orig) == 0)
		return EDR_CONFIRM_NO_CHANGE;
	return EDR_CONFIRM_CHANGE;
}

/*  We call this routine to tell the editblock stuff about the line
 *  that has changed as a result of the user's edit to line lnum
 *  of the block.
 *
 *  We call this on confirmation of the edit and also (with lx NULL)
 *  just before doing a line split or join.  The reason for the latter
 *  cases is to refresh the textwin's knowledge of what is displayed
 *  on each line.  If we don't do this it will do it's optimised minimal
 *  redraw based on stale data and get things wrong.
 */
static int
update_lines(si, ei, lnum, line, nlines, lx)
srcinfo_t *si;
editblockinfo_t *ei;
int lnum;
const char *line;
int nlines;
lexinfo_t *lx;
{
	const char **lines;
	int i, res;
	call_callback_t call_callback;

	lines = (const char **)e_malloc(nlines * sizeof(char *));

	for (i = 0; i < nlines; ++i)
		lines[i] = ei->ei_lines[i];
	lines[lnum] = line;

	call_callback = (lx != NULL) ? DO_CALL_CALLBACK : DONT_CALL_CALLBACK;

	res = change_editblock(si->si_editblock_id, lines, nlines,
					call_callback, (callback_arg_t)lx);

	free((char *)lines);

	/*  Refresh caller's passed in editblock info, as the
	 *  change_editblock call above will have made ei_lines stale.
	 */
	get_editblock_info(si->si_editblock_id, ei);

	return res;
}

/*  Handle user edits of an editblock block.  The bulk of the work
 *  is supporting multi-line editing on top of edit_field() single
 *  line edits.
 */
static void
edit_editblock(ev, srcwin_id, si)
event_t *ev;
srcwin_id_t srcwin_id;
srcinfo_t *si;
{
	int line_height, event_nchars, width, lnum;
	lexinfo_t lxbuf;
	bool finished;
	quit_reason_t quitres;

	if (si->si_wn != ev->ev_wn)
		panic("wn botch in ee");

	lnum = si->si_editblock_offset;
	width = wn_get_width(si->si_wn);
	line_height = si->si_font->ft_height;
	event_nchars = -1;

	clear_selection();

	wn_pushback_event(ev);

	finished = FALSE;
	do {
		edesc_t edescbuf;
		editblockinfo_t eibuf;
		int res, edit_res, i, edit_wn, y, nlines;
		const char **lines;

		get_editblock_info(si->si_editblock_id, &eibuf);
		nlines = eibuf.ei_nlines;

		src_show_lnum(srcwin_id, eibuf.ei_src_lnum + lnum, TRUE);
		y = src_lnum_to_ypos(srcwin_id, eibuf.ei_fil,
							eibuf.ei_src_lnum + lnum);

		/*  Make a window that fits the edit block.
		 */
		edit_wn = wn_create_subwin(si->si_wn, 0, y, width, line_height,
								   WN_INPUT_OUTPUT);

		make_edesc(&edescbuf, edit_wn, eibuf.ei_lines[lnum],
						width / si->si_font->ft_width,
						eibuf.ei_fg, eibuf.ei_bg);
		edescbuf.ed_user = (int)QR_CONFIRM;
		edescbuf.ed_quitfunc = (ed_quitfunc_t)editblock_quitfunc;
		edescbuf.ed_keyfunc = (ed_keyfunc_t)editblock_keyfunc;
		edescbuf.ed_font = si->si_font;

		if (event_nchars != -1) {
			edescbuf.ed_newpos = event_nchars;
			do_edit(&edescbuf, EDM_SETCURSOR);
		}

		edit_res = suppress_ta_cursor_then_edit_field(&edescbuf,
						            (const char *)NULL);
		if (edit_res == EDR_CONFIRM_CHANGE ||
						edit_res == EDR_CONFIRM_NO_CHANGE)
			quitres = (quit_reason_t)edescbuf.ed_user;
		else
			quitres = QR_OTHER;

		switch (quitres) {
		case QR_OTHER:
			finished = TRUE;
			break;
		case QR_CONFIRM:
			{
				bool in_lines;

				if (edescbuf.ed_act == EDA_PUCK) {
					int ty;

					ty = edescbuf.ed_puck_y;
					in_lines = ty >= -lnum * line_height &&
						   ty < (nlines - lnum) * line_height;
				}
				else
					in_lines = FALSE;
						
				res = update_lines(si, &eibuf,
						   lnum, edescbuf.ed_copy,
						   nlines,
						   in_lines ? NULL : &lxbuf);

				if (in_lines)
					finished = TRUE;
				else if (lxbuf.lx_lnum == -1)
					finished = TRUE;
				else {
					if (edescbuf.ed_flags & EDF_UNGETPUCK)
						wn_next_event(edit_wn,EVENT_MASK,ev);
					if (lxbuf.lx_lnum >= nlines) {
						lnum = nlines - 1;
						event_nchars =
						   strlen(eibuf.ei_lines[lnum]);
					}
					else {
						lnum = lxbuf.lx_lnum;
						event_nchars = lxbuf.lx_cnum;
					}
				}
			}
			break;
		case QR_DEL:
			{
				char *joined_line;

				if (lnum == 0) {
					errf("Can't delete past line 1");
					event_nchars = 0;
					break;
				}

				update_lines(si, &eibuf, lnum,
					     edescbuf.ed_copy, nlines,
					     (lexinfo_t *)NULL);

				joined_line = strf("%s%s",
						eibuf.ei_lines[lnum - 1],
						edescbuf.ed_copy);
				event_nchars = strlen(eibuf.ei_lines[lnum - 1]);

				--nlines;
				lines = (const char **)e_malloc(nlines *
								sizeof(char *));
				for (i = 0; i < lnum - 1; ++i)
					lines[i] = eibuf.ei_lines[i];
				lines[i++] = joined_line;
				for (; i < nlines; ++i)
					lines[i] = eibuf.ei_lines[i + 1];

				res = change_editblock(si->si_editblock_id, lines,
							nlines, DONT_CALL_CALLBACK,
							(callback_arg_t)NULL);
				free(joined_line);
				free((char *)lines);
				--lnum;
			}
			break;
		case QR_NEWLINE:
			{
				char *oldpart, *newpart;
				const char *cptr, *oldline;
				int pos;

				update_lines(si, &eibuf, lnum,
					     edescbuf.ed_copy, nlines,
					     (lexinfo_t *)NULL);

				pos = edescbuf.ed_curpos;
				oldline = edescbuf.ed_copy;
				
				oldpart = strf("%.*s", pos, oldline);
				for (cptr = oldline; isspace(*cptr); ++cptr)
					;
				event_nchars = cptr - oldline;
				newpart = strf("%.*s%s",
						       event_nchars, oldline,
						       oldline + pos); 

				++nlines;
				lines = (const char **)e_malloc(nlines *
								   sizeof(char *));
				for (i = 0; i < lnum; ++i)
					lines[i] = eibuf.ei_lines[i];
				lines[i++] = oldpart;
				lines[i++] = newpart;
				for (; i < nlines; ++i)
					lines[i] = eibuf.ei_lines[i - 1];

				res = change_editblock(si->si_editblock_id, lines,
							nlines, DONT_CALL_CALLBACK,
							(callback_arg_t)NULL);
				free(oldpart);
				free(newpart);
				free((char *)lines);
				++lnum;
			}
			break;
		default:
			panic("bad quitres in eel");
		}
		wn_close_window(edit_wn);
	} while (!finished);
}

int
highlight_source(fil, lnum)
fil_t *fil;
int lnum;
{
	srcwin_id_t srcwin_id;

	if (td_set_displayed_source(fil, lnum, "highlight"))
		return 0;

	/* BUG: fil == NULL means turn any highlighting off, but we can't
	 *      call set_highlighted_fil with a NULL fil, so for the
	 *	moment we ignore the problem.
	 */
	if (fil == NULL)
		return 0;

	srcwin_id = get_current_srcwin();

	/*  Convert from 1.. numbering to 0..
	 */
	--lnum;

	set_highlighted_line(fil, lnum);

	if (src_show(srcwin_id, fil, lnum, TRUE) != 0)
		return -1;

	echo_src_name_and_lnum(srcwin_id, fil->fi_name, lnum + 1);
	return 0;
}

void
show_source(fil, lnum, centre)
fil_t *fil;
int lnum;
bool centre;
{
	srcwin_id_t srcwin_id;

	if (td_set_displayed_source(fil, lnum, "showsource"))
		return;

	/*  Convert from 1.. numbering to 0..
	 */
	--lnum;

	srcwin_id = get_current_srcwin();
	src_show(srcwin_id, fil, lnum, centre);
	echo_src_name_and_lnum(srcwin_id, fil->fi_name, lnum + 1);
}

fil_t *
get_displayed_fil()
{
	srcinfo_t sibuf;

	if (td_get_displayed_fil(&sibuf.si_fil) != 0)
		src_get_info(get_current_srcwin(), 0, 0, &sibuf);
	return sibuf.si_fil;
}

void
source_window_menu_func(arg, md, rv)
char *arg;
int md, rv;
{
	srcwin_id_t srcwin_id;

	srcwin_id = (srcwin_id_t)arg;

	switch (rv) {
	case MR_SRCWIN_BACK:
		src_pop(srcwin_id);
		break;
	case MR_SRCWIN_SEARCH_FORWARDS:
	case MR_SRCWIN_SEARCH_BACKWARDS:
		src_search(srcwin_id, get_typing_line_string(),
						rv == MR_SRCWIN_SEARCH_FORWARDS);
		break;
	case MR_SRCWIN_PAGE_UP:
		src_scroll(srcwin_id, -src_get_window_height(srcwin_id) / 2);
		break;
	case MR_SRCWIN_PAGE_DOWN:
		src_scroll(srcwin_id, src_get_window_height(srcwin_id) / 2);
		break;
	default:
		panic("bad rv in swmf");
	}
	Mclear(md);
}

/*  Input function for the source display area.  The left button
 *  selects variable or function names, the middle button
 *  produces the forward/back popup menu, and the right button
 *  the add breakpoint/execute to here menu.
 */
/* ARGSUSED */
void
source_window_event_handler(srcwin_id, ev)
srcwin_id_t srcwin_id;
event_t *ev;
{
	srcinfo_t sibuf;
	int buttons;

	src_get_info(srcwin_id, ev->ev_x, ev->ev_y, &sibuf);
	echo_src_name_and_lnum(srcwin_id, 
			       (sibuf.si_fil != NULL) ? sibuf.si_fil->fi_name : NULL,
			       sibuf.si_fil_lnum + 1);

	buttons = ev->ev_buttons & (B_ANY | B_SHIFT_KEY | B_CONTROL_KEY);
	if (buttons == B_LEFT && sibuf.si_src_lnum != -1) {
		const char *start, *end;

		if (sibuf.si_editblock_id != NULL) {
			int oldstate;

			oldstate = updating(OBJ_UPDATING_OFF);
			clear_selection();
			select_editblock(sibuf.si_editblock_id, EDL_SELECT,
								DO_CALL_CALLBACK);
			(void) updating(oldstate);
		}
		else if (sibuf.si_cnum != -1 &&
		        get_varname(sibuf.si_fil,
				    sibuf.si_text,
				    sibuf.si_text + sibuf.si_cnum,
				    MAXVARLEN,
				    &start, &end))
			confirm_var(ev, srcwin_id, sibuf.si_fil, sibuf.si_src_lnum,
							sibuf.si_text, start, end);
		else 
			wn_wait_for_release_of(ev->ev_wn, B_ANY);
	}
	else if (buttons == B_MIDDLE) {
		if (sibuf.si_editblock_id != NULL) {

			/* BUG: this check should probably not be here,
			 *      as this file shouldn't know about the
			 *      meaning of source code.
			 */
			if (sibuf.si_fil->fi_language != LANG_C) {
				errf("Sorry, can only add breakpoint code in C functions");
				wn_wait_for_release_of(ev->ev_wn, B_ANY);
			}
			else
				edit_editblock(ev, srcwin_id, &sibuf);
		}
		else {
			errf("Not in an editable section");
			wn_wait_for_release_of(ev->ev_wn, B_ANY);
		}
	}
	else if (buttons == B_RIGHT && sibuf.si_src_lnum != -1) {
		static const char *bptcaps[] = {
			"add breakpoint",
			"execute to here",
			NULL
		};
		static popup_t bptpop = { -1, TRUE, 0, bptcaps };
		long sa;
		int res, lnum;
		func_t *f;

		sa = src_draw_arrow(srcwin_id, sibuf.si_src_lnum);
		res = select_from_popup(ev->ev_wn, B_RIGHT, &bptpop,
							ev->ev_x, ev->ev_y);
		wn_restore_area(sa);
		wn_free_area(sa);

		lnum = sibuf.si_fil_lnum + 1;

		switch(res) {
		case 0:
		case 1:
			f = lnum_to_func(sibuf.si_fil, lnum);
			if (f != NULL) {
				td_record_func_and_lnum_cmd(f, lnum,
					(res == 0) ? "breakpoint" : "execto");
				if (res == 0)
					add_breakpoint_object(f, lnum);
				else
					exec_to_lnum(f, lnum);
			}
			else
				errf("Line %d of %s is not within a function",
						      lnum, sibuf.si_fil->fi_name);
			break;
		case NO_SELECTION:
			break;
		default:
			panic("unknown popup return value");
		}
	}
}
