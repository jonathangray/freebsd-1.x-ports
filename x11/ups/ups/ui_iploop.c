/* ui_iploop.c - main loop, and some miscellaneous ui functions */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ui_iploop_c_sccsid[] = "@(#)ui_iploop.c	1.31 19/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/wn.h>
#include <local/obj/obj.h>

#include <local/ukcprog.h>
#include <local/obj/fed.h>
#include <local/obj/newtb.h>
#include <mtrprog/strcache.h>

#include "cursors.h"
#include "ups.h"
#include "symtab.h"
#include "ui.h"
#include "tdr.h"
#include "src.h"
#include "ui_priv.h"
#include "data.h"
#include "expr.h"
#include "state.h"
#include "as.h"		/* for dump_as_assember() */
#include "reg.h"	/* for re_redraw_root() and clear_message() */
#include "debug.h"

static void my_cu_set PROTO((int wn, int cursor));
static void ta_puckfunc PROTO((struct edescst *edesc));
static int ta_quitfunc PROTO((struct edescst *edesc));
static void draw_message PROTO((int wn, const char *mesg));
static void dump_selected_objects PROTO((void));

/*  Scroll loop.
 *
 *  If the left mouse button is pressed, go into sample  mode and
 *  scroll continuously at a rate depending on the distance
 *  that the mouse is moved from the place it was first pressed.
 *
 *  If the middle button is pressed, goto position y in the display area
 *  or source window, where y is the y coordinate in the window originally
 *  in ev->ev_wn.
 */
void
tbar_event_handler(tbar_id, ev)
tbar_id_t tbar_id;
event_t *ev;
{
	static char scrolldist[] = { 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4 };
	int wn, height, dist;
	int orig_y, old_inmode;

	if (ev->ev_buttons == 0) {
		int unit;

		unit = tb_tbpos_to_unit(tbar_id, ev->ev_wn, ev->ev_y, FALSE);
		tb_show_pos(tbar_id, unit);
	}

	wn = ev->ev_wn;
	height = wn_get_height(wn);
	if (ev->ev_buttons & B_LEFT) {
		orig_y = ev->ev_y;
#ifdef VLINE
		wn_sc_vline(wn, ev->ev_y);
#endif
		old_inmode = wn_inmode(wn, WN_SAMPLE);
		for (;;) {
			wn_next_event(wn, EVENT_MASK, ev);
			if ((ev->ev_buttons & B_LEFT) == 0)
				break;
			if (ev->ev_y < 0 || ev->ev_y >= height)
				continue;
			if (orig_y == ev->ev_y)
				my_cu_set(wn, CU_SC_PRESSED);
			else
				my_cu_set(wn, (orig_y > ev->ev_y) ? CU_SCROLL_UP
						       		  : CU_SCROLL_DOWN);
			dist = (orig_y - ev->ev_y) / 3;
			if (dist >= 0 && dist < sizeof(scrolldist))
				dist = scrolldist[dist];
			else if (dist < 0 && -dist < sizeof(scrolldist))
				dist = -scrolldist[-dist]; 
			wn_updating_off(wn);

			tb_scroll(tbar_id, dist, TRUE);
			wn_updating_on(wn);
		}
#ifdef VLINE
		wn_sc_off(wn);
#endif
		(void) wn_inmode(wn, old_inmode);
		my_cu_set(wn, CU_SBAR);
	}
	else if (ev->ev_buttons & B_MIDDLE) {
		int unit;

		unit = tb_tbpos_to_unit(tbar_id, ev->ev_wn, ev->ev_y, TRUE);
		tb_goto(tbar_id, unit, TRUE);
		wn_wait_for_release_of(ev->ev_wn, B_ANY);
	}
}

/*  Set the cursor if the new cursor is different from the current one.
 */
static void
my_cu_set(wn, cursor)
int wn;
int cursor;
{
	static int old_cursor = -1;

	if (cursor != old_cursor) {
		old_cursor = cursor;
		set_bm_cursor(wn, cursor);
	}
}

wn_abort_func_t
set_user_abort_func(func)
wn_abort_func_t func;
{
	return td_have_window() ? wn_set_abort_func(WN_STDWIN, func) : NULL;
}

/*  Display a message in the message area and ring the bell.
 *  The beep is suppressed if the format starts with \b.
 *
 *  The value of errno is unchanged by this function (some functions
 *  rely on this property).
 */
void
display_message(mesg)
const char *mesg;
{
	int wn, want_bell;
	int save_errno;

	want_bell = *mesg != '\b';
	if (!want_bell)
		++mesg;

	save_errno = errno;

	wn = get_message_wn();
	if (wn == -1) {
		write(2, mesg, strlen(mesg));
		write(2, "\n", 1);
	}
	else {
		if (want_bell)
			wn_bell(wn);
		draw_message(wn, mesg);
	}

	errno = save_errno;
}

void
clear_message()
{
	draw_message(get_message_wn(), "");
}

static void
draw_message(wn, mesg)
int wn;
const char *mesg;
{
	char copy[256];
	int w, h, maxlen;

	wn_get_window_size(wn, &w, &h);
	wn_set_area(wn, 1, 1, w - 1, h - 1, WN_BG);

	maxlen = wn_strpos(mesg, w - 3, (font_t *)NULL, FALSE);
	if (maxlen < strlen(mesg)) {
		(void) memcpy(copy, mesg, maxlen);
		copy[maxlen] = '\0';
		mesg = copy;
	}
	wn_tputs(wn, mesg, 2, 2);
}

/*  ed_puckfunc for the typing line (see fed.c). Since the typing line
 *  is a permanent field, and we want the cursor always displayed, just
 *  transform EDM_CONFIRM into EDM_CONT.
 */
static void
ta_puckfunc(edesc)
struct edescst *edesc;
{
	if (edesc->ed_meaning == EDM_CONFIRM)
		edesc->ed_meaning = EDM_CONT;
}

/*  ed_quitfunc for the typing area (see fed.c). Interpret this as a
 *  request to treat the typing line as a variable name to be displayed.
 *
 *  If the string starts with "$debug:" then invoke a debugging routine.
 *  If the string starts with "$check" invoke td_check on the rest of it.
 */
/* ARGSUSED */
static int
ta_quitfunc(edesc)
struct edescst *edesc;
{
	static const char debug[] = "$debug:";
	static const char check[] = "$check";

	/*  Ignore a zero length string.
	 */
	if (*edesc->ed_copy == '\0')
		return EDR_CONT;

	if (strncmp(edesc->ed_copy, debug, sizeof(debug) - 1) == 0) {
		do_debug_command(edesc->ed_copy + sizeof(debug) - 1);
	}
	else if (strncmp(edesc->ed_copy, check, sizeof(check) - 1) == 0) {
		td_check(edesc->ed_copy + sizeof(check) - 1);
	}
	else {
		cursor_t old_cursor;

		old_cursor = wn_get_window_cursor(WN_STDWIN);
		set_bm_cursor(WN_STDWIN, CU_WAIT);
		td_record_show_var((fil_t *)NULL, 0, edesc->ed_copy);
		show_var_from_typing_line(get_current_srcwin(), edesc->ed_copy);
		wn_define_cursor(WN_STDWIN, old_cursor);
	}

	return EDR_CONT;
}
	
void
do_debug_command(line)
const char *line;
{
	typedef enum {
		ASM, ASMSRC, SCSTATS, SETDEFAULTOBJ,
		LOADSYMS, DUMPDISPLAY, DUMPOBJS, DUMPSTACK, DUMPU, DBFLAGS,
		ABORT, QUIT, BADADDR,
		LIST
	} command_t;
	typedef struct cmdst {
		const char *cmd_name;
		command_t cmd_command;
	} cmd_t;
	static cmd_t cmdtab[] = {
		"asm",		ASM,
		"asmsrc",	ASMSRC,
		"scstats",	SCSTATS,
		"loadsyms",	LOADSYMS,
		"setdefobj",	SETDEFAULTOBJ,
		"dumpdisplay",	DUMPDISPLAY,
		"dumpobjs",	DUMPOBJS,
		"dumpstack",	DUMPSTACK,
		"dumpu",	DUMPU,
		"dbflags",	DBFLAGS,
		"abort",	ABORT,
		"badaddr",	BADADDR,
		"quit",		QUIT,
		"?",		LIST
	};
#define NCMDS	(sizeof cmdtab / sizeof *cmdtab)
	static const unsigned long badaddr = 0x42424242;
	cmd_t *cmd;
	const char *cmdname;
	int len;

	while (*line == ' ')
		++line;
	if (*line == '\0') {
		errf("Null $debug command (`?' for list)");
		return;
	}

	td_record_debug_command(line);

	cmdname = line;
	while (*line != ' ' && *line != '\0')
		++line;
	len = line - cmdname;
	while (*line == ' ')
		++line;
	
	for (cmd = cmdtab; cmd < &cmdtab[NCMDS]; ++cmd)
		if (strncmp(cmd->cmd_name, cmdname, len) == 0 &&
						cmd->cmd_name[len] == '\0')
			break;
	if (cmd == cmdtab + sizeof cmdtab / sizeof *cmdtab) {
		errf("Unknown $debug command %.*s (`?' for list)",
								len, cmdname);
		return;
	}

	switch (cmd->cmd_command) {
	case LIST:
		{
			char *s, *new;

			s = strsave("cmds:");
			for (cmd = cmdtab; cmd < &cmdtab[NCMDS]; ++cmd) {
				new = strf("%s %s", s, cmd->cmd_name);
				free(s);
				s = new;
			}
			errf("\b%s", s);
		}
		break;
	case DBFLAGS:
		if (*line == '\0')
			errf("\bDebug_flags=0x%04x", Debug_flags);
		else {
			unsigned long old;

			old = Debug_flags;
			Debug_flags = strtol(line, (char **)NULL, 0);
			errf("Debug_flags=0x%04x (was 0x%04x)",
							Debug_flags, old);
		}
		break;
	case ASM:
	case ASMSRC:
		dump_as_assembler(line, cmd->cmd_command == ASMSRC);
		break;
	case SCSTATS:
		sc_dump_stats_of_newest_sc();
		break;
	case LOADSYMS:
		debug_load_symbols(line);
		break;
	case SETDEFAULTOBJ:
		td_set_default_obj_to_selection();
		break;
	case DUMPDISPLAY:
		dump_object((objid_t)NULL, (char *)stdout, OBJ_DESCENDENTS);
		break;
	case DUMPOBJS:
		dump_selected_objects();
		break;
	case DUMPU:
		dump_uarea_to_file((*line != '\0') ? line : "ups_uarea");
		break;
	case DUMPSTACK:
		dump_stack_to_file((*line != '\0') ? line : "ups_stack");
		break;
	case ABORT:
		panic("you asked for it, you got it.  Goodbye ...");
	case BADADDR:
		errf("Writing a byte to address 0x%x", badaddr);
		*(char *)badaddr = 42;
		errf("Write to address 0x%x did not fault!", badaddr);
		break;
	case QUIT:
		exit(1);
	default:
		panic("unknown command");
	}
	return;
}

static void
dump_selected_objects()
{
	sel_t *sel;

	for (sel = get_selection(); sel != NULL; sel = sel->se_next) {
		dump_object(sel->se_code, (char *)stdout, OBJ_SELF);
		dump_object(sel->se_code, (char *)stdout, OBJ_DESCENDENTS);
	}
}

/*  Initialise or  reinitialise the typing line.  This is called on
 *  startup or after a window size change.
 */
void
ta_init(edesc, wn, font)
edesc_t *edesc;
int wn;
font_t *font;
{
	static int first_call = TRUE;
	int curpos, maxlen, win_width, win_height;
	char *oldstring;
	const char *s;

	wn_get_window_size(wn, &win_width, &win_height);

	maxlen = win_width / font->ft_width;
	if (first_call) {
		s = "";
		oldstring = 0; /* to satisfy gcc */
		curpos = 0;
	}
	else {
		/*  If the window has got narrower, we may have to truncate
		 *  the typing area string.
		 */
		int newlen;

		oldstring = edesc->ed_copy;
		newlen = wn_strpos(oldstring, maxlen * font->ft_width, font, FALSE);
		oldstring[newlen] = '\0';
		s = oldstring;

		curpos = edesc->ed_curpos;
	}

	make_edesc(edesc, wn, s, maxlen, WN_FG, WN_BG);
	edesc->ed_font = font;
	edesc->ed_flags |= EDF_ONETRIP | EDF_CONT_ON_CANCEL;
	edesc->ed_puckfunc = (ed_puckfunc_t)ta_puckfunc;
	edesc->ed_quitfunc = (ed_quitfunc_t)ta_quitfunc;
	edesc->ed_meaning = EDM_SETCURSOR;
	edesc->ed_newpos = wn_strnwidth(s, curpos, font);

	wn_set_area(wn, 0, 0, win_width, win_height, WN_BG);
	wn_text(wn, font, s, 0, 0, WN_FG, WN_BG, WN_USE_TOP);
	do_edit(edesc, EDM_SETCURSOR);

	if (!first_call)
		free(oldstring);
	first_call = FALSE;
}

/*  Remove the typing area cursor, then call edit_field then restore it.
 *  This wrapper round edit_field is used for all field editing so that
 *  the typing area marker bar is suppressed while other editing is
 *  going on.
 */
int
suppress_ta_cursor_then_edit_field(edesc, what)
struct edescst *edesc;
const char *what;
{
	int res;
	
	if (td_have_window()) {
		edesc->ed_expose_handler = re_redraw_root;

		ed_cursor(get_typing_line_edesc(), EDC_CURSOR_OFF);
		res = td_record_edit_field(edesc, what);
		ed_cursor(get_typing_line_edesc(), EDC_CURSOR_ON);
	}
	else {
		res = td_record_edit_field(edesc, what);
	}

	/*  The source editing stuff does multiline editing and wants
	 *  access to ed_copy after calling (*ed_quitfunc)().  We know we
	 *  are being called by that when what == NULL.
	 */
	if (what != NULL)
		free(edesc->ed_copy);

	return res;
}
