/* textwin.c - routines to display text in a window */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_textwin_c_sccsid[] = "@(#)textwin.c	1.16 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>

#include <local/ukcprog.h>
#include <mtrprog/regex.h>

#include "textwin.h"

/*  Structure describing an existing text line.
 *
 *  This is used to avoid unnecessary repainting.
 */
typedef struct linedescst {
	int ld_y;
	int ld_fg;
	int ld_bg;
	tw_hlinfo_t ld_hlinfo;
	char *ld_line;
	int ld_linesize;
	struct linedescst *ld_next;
} linedesc_t;

/*  Structure describing a source window.  Only one of these at present.
 */
typedef struct textwinst {
	int tw_wn;		/* window we are using */
	int tw_bg;		/* background colour for the source window */

	int tw_width;		/* width of the window */
	int tw_height;		/* height of the window */
	int tw_line_height;	/* height of a line of the file */

	int tw_y;		/* y offset of display from start of file */

	tw_getline_proc_t tw_getline; /* ptr to function which gets a line of text */
	char *tw_getline_arg;	/* arg for tw_getline */

	int tw_nlines;		/* # displayed lines */

	linedesc_t *tw_ldlist;	/* line descriptor list head */

	tw_callback_t tw_callback;
	char *tw_data;
} textwin_t;

/*  Flags used when searching.  Active only for the duration of each search.
 */
static int Internal_regex_error = FALSE;
static int Stop_searching;

/*  A free list of linedesc entries.
 */
static linedesc_t *Free_linedesc_list = NULL;

static void forget_cached_lines PROTO((textwin_t *tw));
static void adjust_ldlist PROTO((textwin_t *tw, int delta));
static void display_line PROTO((textwin_t *tw, int y));
static void fill_textwin PROTO((textwin_t *tw, int start_y, int stop_y));
static void set_nlines PROTO((textwin_t *tw, int nlines));
static tw_search_res_t search_line PROTO((const char *line, int cnum, bool forwards,
			      int *p_cnum, int *p_nchars));
static void set_stop_searching_flag PROTO((void));
static bool highlighting_same PROTO((tw_hlinfo_t *hl1, tw_hlinfo_t *hl2));
static void show_text PROTO((int wn, font_t *font, const char *s,
			     int x, int y, int fg, int bg, int cnum, int nchars));

/*  Macro to convert a line number in a file to a pixel offset from the
 *  start of the display area, given the offset of the display area.
 */
#define LTOP(tw, l, offset)	(((l) * tw->tw_line_height) - offset)

/*  Inverse of LTOP() - convert a pixel offset to a line number.
 */
#define PTOL(tw, p, offset)	(((p) + offset) / tw->tw_line_height)

static void
adjust_ldlist(tw, delta)
textwin_t *tw;
int delta;
{
	linedesc_t *ld, *next, *freelist, *newlist;
	int lim;

	lim = tw->tw_height - tw->tw_line_height;

	newlist = NULL;
	freelist = Free_linedesc_list;
	for (ld = tw->tw_ldlist; ld != NULL; ld = next) {
		next = ld->ld_next;
		ld->ld_y -= delta;
		if (ld->ld_y >= 0 && ld->ld_y < lim) {
			ld->ld_next = newlist;
			newlist = ld;
		}
		else {
			ld->ld_next = freelist;
			freelist = ld;
		}
	}
	tw->tw_ldlist = newlist;
	Free_linedesc_list = freelist;
}

static void
forget_cached_lines(tw)
textwin_t *tw;
{
	linedesc_t *ld, *next, *freelist;

	freelist = Free_linedesc_list;
	for (ld = tw->tw_ldlist; ld != NULL; ld = next) {
		next = ld->ld_next;
		ld->ld_next = freelist;
		freelist = ld;
	}
	Free_linedesc_list = freelist;
	tw->tw_ldlist = NULL;
}

static bool
highlighting_same(hl1, hl2)
tw_hlinfo_t *hl1, *hl2;
{
	if (hl1->hl_type == hl2->hl_type) {
		switch (hl1->hl_type) {
		case TW_HL_NONE:
		case TW_HL_BOX:
		case TW_HL_INVERT_ALL:
			return TRUE;
		case TW_HL_INVERT:
			return hl1->hl_cnum == hl2->hl_cnum &&
					hl1->hl_nchars == hl2->hl_nchars;
		default:
			panic("bad type in hs");
		}
	}
	return FALSE;
}

static void
display_line(tw, y)
textwin_t *tw;
int y;
{
	int maxlen, linelen, fg, bg, lnum, x;
	font_t *font;
	tw_hlinfo_t hl;
	const char *s, *line;
	linedesc_t *ld;

	lnum = PTOL(tw, y, tw->tw_y);
	if (LTOP(tw, lnum, tw->tw_y) != y)
		panic("textwin offset botch in display_line");

	hl.hl_type = TW_HL_NONE; /* default value */
	(*tw->tw_getline)(tw->tw_getline_arg, lnum, &line, &fg, &bg, &font, &hl);

	linelen = strlen(line);
	maxlen = wn_strpos(line, tw->tw_width, font, FALSE);

	/*  Is there a line record for this y position?
	 */
	for (ld = tw->tw_ldlist; ld != NULL; ld = ld->ld_next)
		if (ld->ld_y == y)
			break;

	if (ld != NULL) {
		if (strcmp(ld->ld_line, line) == 0 &&
		    ld->ld_fg == fg && ld->ld_bg == bg &&
		    highlighting_same(&ld->ld_hlinfo, &hl))
			return;
	}
	else {
		if (Free_linedesc_list != NULL) {
			ld = Free_linedesc_list;
			Free_linedesc_list = Free_linedesc_list->ld_next;
		}
		else {
			ld = (linedesc_t *)e_malloc(sizeof(linedesc_t));
			ld->ld_line = NULL;
			ld->ld_linesize = 0;
		}
		ld->ld_y = y;
		ld->ld_next = tw->tw_ldlist;
		tw->tw_ldlist = ld;
	}
	if (ld->ld_linesize <= linelen) {
		if (ld->ld_line != NULL)
			free(ld->ld_line);
		ld->ld_linesize = linelen + 1;
		ld->ld_line = e_malloc(ld->ld_linesize);
	}
	(void) strcpy(ld->ld_line, line);
	ld->ld_fg = fg;
	ld->ld_bg = bg;
	ld->ld_hlinfo = hl;

	wn_set_area(tw->tw_wn, 0, y, tw->tw_width, tw->tw_line_height, bg);

	s = ld->ld_line;
	x = 0;

	ld->ld_line[maxlen] = '\0';

	switch (hl.hl_type) {
	case TW_HL_BOX:
		{
			int width, height;

			width = wn_strwidth(s, font);
			height = font->ft_height;
			wn_text(tw->tw_wn, font, s, x, y, fg, bg, WN_USE_TOP);
			wn_invert_box(tw->tw_wn, x, y, width, height);
			wn_invert_box(tw->tw_wn, x-1, y-1, width + 2, height + 2);
		}
		break;

	case TW_HL_INVERT_ALL:
		/*  Don't use text output for leading whitespace - this is so we
		 *  don't highlight leading whitespace if the line is selected.
		 */
		while (*s == ' ')
			++s;
		x += (s - ld->ld_line) * font->ft_width_tab[' '];
		wn_text(tw->tw_wn, font, s, x, y, bg, fg, WN_USE_TOP);
		break;

	case TW_HL_INVERT:
		show_text(tw->tw_wn, font, s, x, y, fg, bg,
							0, hl.hl_cnum);
		show_text(tw->tw_wn, font, s, x, y, bg, fg,
							hl.hl_cnum, hl.hl_nchars);
		show_text(tw->tw_wn, font, s, x, y, fg, bg,
					hl.hl_cnum + hl.hl_nchars,
					strlen(s) - (hl.hl_cnum + hl.hl_nchars));
		break;
		
	case TW_HL_NONE:
		wn_text(tw->tw_wn, font, s, x, y, fg, bg, WN_USE_TOP);
		break;

	default:
		panic("bad hl in dl");
	}

	ld->ld_line[maxlen] = line[maxlen];
}

static void
show_text(wn, font, s, x, y, fg, bg, cnum, nchars)
int wn;
font_t *font;
const char *s;
int x, y, fg, bg, cnum, nchars;
{
	static char *buf = NULL;
	static int bufsize = 0;

	if (nchars <= 0)
		return;
	
	if (nchars > bufsize) {
		if (buf != NULL)
			free(buf);
		bufsize = nchars;
		buf = e_malloc(bufsize + 1);
	}
	strncpy(buf, s + cnum, nchars);
	buf[nchars] = '\0';

	wn_text(wn, font, buf,
			x + wn_strnwidth(s, cnum, font), y, fg, bg, WN_USE_TOP);
}

static void
fill_textwin(tw, start_y, stop_y)
textwin_t *tw;
int start_y, stop_y;
{
	int lnum, y, line_height, bg;

	bg = tw->tw_bg;
	line_height = tw->tw_line_height;
	lnum = (tw->tw_y + start_y + line_height - 1) / line_height;
	y = lnum * line_height - tw->tw_y;

	if (y > start_y)
		wn_set_area(tw->tw_wn, 0, start_y, tw->tw_width, y - start_y, bg);

	for (; y + line_height <= stop_y; y += line_height)  {
		if (lnum >= tw->tw_nlines)
			break;
		display_line(tw, y);
		++lnum;
	}
	
	if (y < stop_y)
		wn_set_area(tw->tw_wn, 0, y, tw->tw_width, stop_y - y, bg);
}

void
tw_redraw_text(textwin_id, redraw_all_lines)
textwin_id_t textwin_id;
bool redraw_all_lines;
{
	textwin_t *tw;

	tw = (textwin_t *)textwin_id;

	if (redraw_all_lines) {
		int old_height, new_y, max_y;

		forget_cached_lines(tw);

		old_height = tw->tw_height;
		wn_get_window_size(tw->tw_wn, &tw->tw_width, &tw->tw_height);

		max_y = (tw->tw_nlines - 1) * tw->tw_line_height;
		new_y = tw->tw_y;

		/*  If the displayed text extends beyond the bottom of the
		 *  window or the window is growing, use the bottom of the
		 *  window as the anchor point.
		 */
		if (max_y > tw->tw_height || tw->tw_height > old_height)
			new_y += old_height - tw->tw_height;

		/*  Constrain new_y to point somewhere in the text.
		 */
		if (new_y < 0)
			new_y = 0;
		if (new_y > max_y)
			new_y = max_y;
		tw->tw_y = new_y;

		if (tw->tw_callback != NULL) {
			(*tw->tw_callback)(tw->tw_data, TW_NEW_VISIBLE_UNITS,
						tw->tw_height / tw->tw_line_height,
						tw->tw_height);
		}
	}
	fill_textwin(tw, 0, tw->tw_height);
}

textwin_id_t
tw_make_textwin(wn, bg, line_height)
int wn, bg, line_height;
{
	textwin_t *tw;
	int win_width, win_height;

	tw = (textwin_t *)e_malloc(sizeof(textwin_t));

	wn_get_window_size(wn, &win_width, &win_height);

	tw->tw_wn = wn;
	tw->tw_bg = bg;

	tw->tw_width = win_width;
	tw->tw_height = win_height;
	tw->tw_line_height = line_height;

	tw->tw_y = 0;

	tw->tw_getline = NULL;
	tw->tw_getline_arg = NULL;

	tw->tw_nlines = 0;

	tw->tw_ldlist = NULL;

	tw->tw_callback = NULL;
	tw->tw_data = NULL;

	return (textwin_id_t)tw;
}

static void
set_nlines(tw, nlines)
textwin_t *tw;
int nlines;
{
	if (tw->tw_callback != NULL)
		(*tw->tw_callback)(tw->tw_data, TW_NEW_TOTAL_UNITS,
						nlines, nlines * tw->tw_line_height);
	tw->tw_nlines = nlines;
}

void
tw_set_callback_and_data(textwin_id, callback, data)
textwin_id_t textwin_id;
tw_callback_t callback;
char *data;
{
	textwin_t *tw;

	tw = (textwin_t *)textwin_id;

	tw->tw_callback = callback;
	tw->tw_data = data;
}

void
tw_set_text(textwin_id, bg, getline, getline_arg, nlines, lnum, centre)
textwin_id_t textwin_id;
int bg;
tw_getline_proc_t getline;
char *getline_arg;
int nlines, lnum;
bool centre;
{
	textwin_t *tw;

	tw = (textwin_t *)textwin_id;

	tw->tw_bg = bg;
	tw->tw_getline = getline;
	tw->tw_getline_arg = getline_arg;
	set_nlines(tw, nlines);
	
	tw_goto(textwin_id, lnum, centre);
}

int
tw_get_window_width(textwin_id)
textwin_id_t textwin_id;
{
	return ((textwin_t *)textwin_id)->tw_width;
}

int
tw_get_window_height(textwin_id)
textwin_id_t textwin_id;
{
	return ((textwin_t *)textwin_id)->tw_height;
}

int
tw_get_line_height(textwin_id)
textwin_id_t textwin_id;
{
	return ((textwin_t *)textwin_id)->tw_line_height;
}

int
tw_lnum_to_ypos(textwin_id, lnum)
textwin_id_t textwin_id;
int lnum;
{
	textwin_t *tw;

	tw = (textwin_t *)textwin_id;
	return LTOP(tw, lnum, tw->tw_y);
}

int
tw_ypos_to_lnum(textwin_id, ypos)
textwin_id_t textwin_id;
int ypos;
{
	textwin_t *tw;

	tw = (textwin_t *)textwin_id;
	return PTOL(tw, ypos, tw->tw_y);
}

bool
tw_is_visible(textwin_id, lnum)
textwin_id_t textwin_id;
int lnum;
{
	textwin_t *tw;
	int y;

	tw = (textwin_t *)textwin_id;
	y = LTOP(tw, lnum, tw->tw_y);
	return y >= 0 && y < tw->tw_height - tw->tw_line_height;
}

/*  Set *p_line to the line of source under (x,y) in the source window.
 *  Point *p_pos at the character in *p_line pointed to.
 *
 *  If there is no character under (x,y), return FALSE otherwise return TRUE.
 */
int
tw_get_line_info(textwin_id, x, y, p_line, p_font, p_lnum)
textwin_id_t textwin_id;
int x, y;
const char **p_line;
font_t **p_font;
int *p_lnum;
{
	textwin_t *tw;
	const char *line;
	int lnum, fg, bg;

	tw = (textwin_t *)textwin_id;

	if (x < 0)
		lnum = y / tw->tw_line_height;
	else
		lnum = PTOL(tw, y, tw->tw_y);

	if (tw->tw_getline == NULL || lnum >= tw->tw_nlines) {
		*p_line = NULL;
		*p_font = NULL;
		*p_lnum = -1;
		return FALSE;
	}

	(*tw->tw_getline)(tw->tw_getline_arg, lnum, &line, &fg, &bg, p_font,
								(tw_hlinfo_t *)NULL);
	*p_line = line;
	*p_lnum = lnum;

	return TRUE;
}

/*  Display an arrow pointing at source line lnum, which is assumed to
 *  be visible in the source window.
 *
 *  Return the handle returned by wn_save_area() on the area under the
 *  arrow - this should be used to remove the arrow.
 */
long
tw_draw_arrow(textwin_id, lnum)
textwin_id_t textwin_id;
int lnum;
{
	static unsigned short bits[] = {
		0x0000, 0x0000, 0x0400, 0x0c00,
		0x1c00, 0x3fff, 0x7c00, 0x3fff,
		0x1c00, 0x0c00, 0x0400, 0x0000,
		0x0000, 0x0000, 0x0000, 0x0000
	};
#define ARROW_X_HOT 15
#define ARROW_Y_HOT 6
	static bitmap_t arrow = wn_static_bm(16, 16, 1, 0, 0, BM_BIT0_LEFT, bits);
	textwin_t *tw;
	const char *cptr, *line;
	int fg, bg;
	font_t *font;
	int x, y;
	long sa;

	tw = (textwin_t *)textwin_id;

	(*tw->tw_getline)(tw->tw_getline_arg, lnum, &line, &fg, &bg, &font,
								(tw_hlinfo_t *)NULL);

	for (cptr = line; *cptr == ' ' && *cptr != '\0'; ++cptr)
		;
	x = wn_strnwidth(line, cptr - line, font) - ARROW_X_HOT - 5;
	y = LTOP(tw, lnum, tw->tw_y) + tw->tw_line_height / 2 - ARROW_Y_HOT;
	sa = wn_save_area(tw->tw_wn, x, y, 16, 16);
	wn_put_image(&arrow, 0, 0, 16, 16, tw->tw_wn, x, y, R_RPL, fg, bg);
	return sa;
}

/*  Display an inverted box round the nchars characters starting at character
 *  cno of line lnum.
 *
 *  We assume that all the arguments are within range, and the the text is
 *  all visible in the source window.
 *
 *  The box is drawn using wn_invert_box(), so a second call to this function
 *  with the same arguments will clear the box.
 */
void
tw_draw_box(textwin_id, lnum, col, nchars)
textwin_id_t textwin_id;
int lnum, col, nchars;
{
	int x, y, width, height;
	const char *line;
	int fg, bg;
	font_t *font;
	textwin_t *tw;

	tw = (textwin_t *)textwin_id;

	(*tw->tw_getline)(tw->tw_getline_arg, lnum, &line, &fg, &bg, &font,
								(tw_hlinfo_t *)NULL);

	x = wn_strnwidth(line, col, font);
	y = LTOP(tw, lnum, tw->tw_y);
	width = wn_strnwidth(line + col, nchars, font);
	height = font->ft_height;

	wn_invert_box(tw->tw_wn, x, y, width, height);
	wn_invert_box(tw->tw_wn, x - 1, y - 1, width + 2, height + 2);
}

/*  Handle a change in the number of lines in a file.  Redisplay and
 *  change the thumb bar, if any.
 *
 *  Ensure that line vis_lnum is visible after the change.
 */
void
tw_handle_nlines_delta(textwin_id, lnum, old_nlines, new_nlines, vis_lnum, centre)
textwin_id_t textwin_id;
int lnum, old_nlines, new_nlines, vis_lnum;
bool centre;
{
	textwin_t *tw;

	tw = (textwin_t *)textwin_id;

	set_nlines(tw, tw->tw_nlines + new_nlines - old_nlines);

	if (vis_lnum == -1 || tw_is_visible(textwin_id, vis_lnum))
		fill_textwin(tw, 0, tw->tw_height);
	else
		tw_goto(textwin_id, vis_lnum, centre);
}

/*  Display line lnum of file fil at the top of the current source window.
 */
void
tw_goto(textwin_id, lnum, centre)
textwin_id_t textwin_id;
int lnum;
bool centre;
{
	textwin_t *tw;

	tw = (textwin_t *)textwin_id;

	if (centre) {
		lnum -= tw->tw_height / tw->tw_line_height / 2;
		if (lnum < 0)
			lnum = 0;
	}
	tw->tw_y = lnum * tw->tw_line_height;
	forget_cached_lines(tw);
	wn_updating_off(tw->tw_wn);
	fill_textwin(tw, 0, tw->tw_height);
	wn_updating_on(tw->tw_wn);

	if (tw->tw_callback != NULL)
		(*tw->tw_callback)(tw->tw_data, TW_NEW_POS, lnum, tw->tw_y);
}

/*  Scroll the displayed source npixels up (or down if npixels is negative).
 *  We will not scroll past the start or end of the file.
 *
 *  We return the number of pixels scrolled.
 */
int
tw_scroll(textwin_id, npixels)
textwin_id_t textwin_id;
int npixels;
{
	int old_start_line, old_stop_line, new_start_line, new_stop_line;
	int com_start_line, com_stop_line;
	int old_com_start_y, new_com_start_y, new_com_stop_y, com_height;
	int wn, line_height, old_y, new_y, max_y;
	register textwin_t *tw;

	tw = (textwin_t *)textwin_id;
	wn = tw->tw_wn;
	line_height = tw->tw_line_height;
	
	old_y = tw->tw_y;
	new_y = old_y + npixels;

	if (new_y < 0) {
		npixels -= new_y;
		new_y = 0;
	}
	max_y = (tw->tw_nlines - 1) * line_height;
	if (new_y > max_y) {
		npixels -= new_y - max_y;
		new_y = max_y;
	}
	if (npixels == 0)
		return 0;
	
	old_start_line = (old_y + line_height - 1) / line_height;
	new_start_line = (new_y + line_height - 1) / line_height;
	old_stop_line = (old_y + tw->tw_height) / line_height;
	new_stop_line = (new_y + tw->tw_height) / line_height;

	com_start_line = (old_start_line > new_start_line) ? old_start_line
							   : new_start_line;
	com_stop_line = (old_stop_line < new_stop_line) ? old_stop_line
							: new_stop_line;

	old_com_start_y = LTOP(tw, com_start_line, old_y);
	new_com_start_y = LTOP(tw, com_start_line, new_y);
	com_height = (com_stop_line - com_start_line) * line_height;
	new_com_stop_y = new_com_start_y + com_height;

	wn_updating_off(wn);

	wn_rop(wn, 0, old_com_start_y, tw->tw_width, com_height, 0, new_com_start_y);

	tw->tw_y = new_y;

	if (wn_last_rop_was_damaged(wn))
		forget_cached_lines(tw);
	else
		adjust_ldlist(tw, npixels);
	fill_textwin(tw, 0, tw->tw_height);

	wn_updating_on(wn);

	if (tw->tw_callback != NULL)
		(*tw->tw_callback)(tw->tw_data, TW_NEW_POS,
					tw->tw_y / tw->tw_line_height, tw->tw_y);

	return npixels;
}

void
re_fail(mesg, op)
const char *mesg;
int op;
{
	Internal_regex_error = TRUE;
}

static void
set_stop_searching_flag()
{
	Stop_searching = TRUE;
}
	
/*  see ftypes.h. This function must be named as the *_search function
 *  for file types using these routines
 */
/* ARGSUSED */
tw_search_res_t
tw_search(textwin_id, pattern, lnum, cnum, forwards, p_lnum, p_cnum, p_nchars)
textwin_id_t textwin_id;
const char *pattern;
int lnum, cnum;
bool forwards;
int *p_lnum, *p_cnum, *p_nchars;
{
	const char *line;
	int lim, inc, fg, bg;
	font_t *font;
	tw_search_res_t res;
	const char *mesg;
	textwin_t *tw;
	wn_abort_func_t old_abort_func;
	
	tw = (textwin_t *)textwin_id;

	if (tw->tw_nlines == 0)
		return TW_SR_NOT_FOUND;

	if ((mesg = re_comp(pattern)) != NULL) {
		errf("%s", mesg);
		return TW_SR_SEARCH_ERROR;
	}

	(*tw->tw_getline)(tw->tw_getline_arg, lnum, &line, &fg, &bg, &font,
							(tw_hlinfo_t *)NULL);

	if (forwards) {
		inc = 1;
		lim = tw->tw_nlines;
		res = search_line(line, cnum, TRUE, p_cnum, p_nchars);
	}
	else {
		char *buf;

		inc = -1;
		lim = -1;

		buf = e_malloc(cnum + 1);
		(void) strncpy(buf, line, cnum);
		buf[cnum] = '\0';
		res = search_line(buf, 0, FALSE, p_cnum, p_nchars);
		free(buf);
	}

	Stop_searching = Internal_regex_error = FALSE;

	old_abort_func = wn_set_abort_func(WN_STDWIN, set_stop_searching_flag);
	while (res == TW_SR_NOT_FOUND) {
		if (Stop_searching) {
			res = TW_SR_INTERRUPTED;
			break;
		}

		lnum += inc;
		if (lnum == lim)
			break;

		(*tw->tw_getline)(tw->tw_getline_arg, lnum, &line,
					      &fg, &bg, &font, (tw_hlinfo_t *)NULL);
		res = search_line(line, 0, forwards, p_cnum, p_nchars);
	}
	wn_set_abort_func(WN_STDWIN, old_abort_func);

	*p_lnum = lnum;
	return res;
}

static tw_search_res_t
search_line(line, cnum, forwards, p_cnum, p_nchars)
const char *line;
int cnum;
bool forwards;
int *p_cnum, *p_nchars;
{
	int lim;

	if (e_re_exec(line, cnum, p_cnum, &lim) == 0)
		return TW_SR_NOT_FOUND;
	if (!forwards) {
		while (e_re_exec(line, lim, p_cnum, &lim) == 1)
			;
	}

	if (Internal_regex_error)
		return TW_SR_SEARCH_ERROR;

	*p_nchars = lim - *p_cnum;

	return TW_SR_FOUND;
}

