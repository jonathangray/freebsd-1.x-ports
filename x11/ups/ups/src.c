/* src.c - source window code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_src_c_sccsid[] = "@(#)src.c	1.27 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdlib.h>

#include <local/wn.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "textwin.h"
#include "src.h"

/*  Element in the list built by src_push_current_pos and used by src_pop().
 */
typedef struct histst {
	fil_t *hi_fil;
	int hi_lnum;
	struct histst *hi_next;
} hist_t;

typedef struct srcwinst {
	fil_t *sw_fil;
	int sw_wn;
	int sw_fg;
	int sw_bg;
	hist_t *sw_hist;
	textwin_id_t sw_textwin_id;
	struct srcwinst *sw_next;
	font_t *sw_srcfont;
	font_t *sw_editfont;
	char *sw_data;

	int sw_hl_lnum;
	int sw_hl_cnum;
	int sw_hl_nchars;
} srcwin_t;

#define NO_HL_LNUM	(-1)

/*  Element in the list of selected edit blocks.
 */
typedef struct seleditblockst {
	struct editblockst *se_el;
	struct seleditblockst *se_next;
	struct seleditblockst *se_prev;
} seleditblock_t;

/*  Element in the list of editable lines for a file.
 *  These are pseudo source lines added by ups for things like breakpoints.
 *
 *  Each fil_t structure has a list of these blocks, which is kept sorted
 *  by real source line number.
 *
 *  Outside this file edit blocks are identified by an opaque handle
 *  of type editblock_id_t.
 */
typedef struct editblockst {
	int eb_lnum;			/* Real src line that this block precedes */
	int eb_nlines;			/* Number of lines in this editable block */
	char **eb_lines;		/* Array of the text of the lines */
	struct editblockst *eb_next;	/* Next editable block in this file */
	seleditblock_t *eb_se;		/* Pointer to entry on selected list */
	editblock_callback_t eb_callback; /* Selection callback function */
	editblock_edit_callback_t eb_edit_callback;	/* Edit callback function */
	char *eb_data;			/* User data */
	fil_t *eb_fil;			/* Pointer back to file block */
} editblock_t;

#define FI_EDITLINES(fil)	((editblock_t *)fil->fi_editblocks_id)

/*  The HLH Clipper compiler won't accept the Selected_list and Srclist
 *  initialisations below (it doesn't enter names as defined until the end
 *  of the declarator line).  So we add these extern declarations as a workaround.
 *
 *  Other compilers do this as well, and the extern declarations don't do
 *  any harm, so we don't #ifdef them.
 */
extern srcwin_t Srclist;
extern seleditblock_t Selected_list;

/*  Head/tail of the circular list of selected blocks.  Initially empty.
 */
static seleditblock_t Selected_list = { NULL, &Selected_list, &Selected_list };

/*  Head/tail of the circular list of source files.
 */
static srcwin_t Srclist = { 0, 0, 0, 0, 0, 0, &Srclist };

static void free_lines PROTO((char **lines, int nlines));
static char **dup_lines PROTO((const char **lines, int nlines));
static void get_editblock_colors PROTO((int *p_bg, int *p_fg));
static int lnum_fil_to_src PROTO((fil_t *fil, int fil_lnum));
static int lnum_src_to_fil PROTO((fil_t *fil, int src_lnum));
static editblock_t *src_lnum_to_editblock PROTO((fil_t *fil, int src_lnum));
static int get_editblock_src_lnum PROTO((editblock_t *eb));
static void set_src_pos PROTO((srcwin_t *sw, fil_t *fil, int lnum, bool centre));
static void get_src_fonts PROTO((font_t **p_srcfont, font_t **p_editfont));
static int check_and_get PROTO((fil_t *fil, int fil_lnum));
static void src_getline PROTO((char *arg, int src_lnum,
			       const char **p_line, int *p_fg, int *p_bg,
			       font_t **p_font, tw_hlinfo_t *hl));
static void handle_nlines_delta PROTO((fil_t *fil, int lnum,
				       int old_nlines, int new_nlines,
				       int vis_lnum));

/*  Free a vector of nlines lines
 */
static void
free_lines(lines, nlines)
char **lines;
int nlines;
{
	int i;

	for (i = 0; i < nlines; ++i)
		free(lines[i]);
	free((char *)lines);
}

/*  Make a copy of the lines vector.
 */
static char **
dup_lines(lines, nlines)
const char **lines;
int nlines;
{
	char **new_lines;
	int i;

	new_lines = (char **)e_malloc(nlines * sizeof(char *));
	for (i = 0; i < nlines; ++i)
		new_lines[i] = strsave(lines[i]);
	return new_lines;
}

void
register_editblock_callback(editblock_id, callback)
editblock_id_t editblock_id;
editblock_callback_t callback;
{
	((editblock_t *)editblock_id)->eb_callback = callback;
}

void
register_editblock_edit_callback(editblock_id, edit_callback)
editblock_id_t editblock_id;
editblock_edit_callback_t edit_callback;
{
	((editblock_t *)editblock_id)->eb_edit_callback = edit_callback;
}

void
set_editblock_data(editblock_id, data)
editblock_id_t editblock_id;
char *data;
{
	((editblock_t *)editblock_id)->eb_data = data;
}

char *
get_editblock_data(editblock_id)
editblock_id_t editblock_id;
{
	return ((editblock_t *)editblock_id)->eb_data;
}

/*  Change the lines of an editblock block.
 *
 *  Return 0 for success, -1 for failure (we fail if there is an edit callback
 *  for this block and it returns false.
 */
int
change_editblock(editblock_id, lines, nlines, call_callback, callback_arg)
editblock_id_t editblock_id;
const char **lines;
int nlines;
call_callback_t call_callback;
callback_arg_t callback_arg;
{
	editblock_t *eb;
	int oldnlines;
	char **oldlines;

	eb = (editblock_t *)editblock_id;

	if (call_callback == DO_CALL_CALLBACK &&
	    eb->eb_edit_callback != NULL &&
	    !(*eb->eb_edit_callback)(editblock_id, lines, nlines, callback_arg))
		return -1;

	oldlines = eb->eb_lines;
	oldnlines = eb->eb_nlines;
	eb->eb_lines = dup_lines(lines, nlines);
	eb->eb_nlines = nlines;
	free_lines(oldlines, oldnlines);

	handle_nlines_delta(eb->eb_fil, lnum_fil_to_src(eb->eb_fil, eb->eb_lnum),
						             oldnlines, nlines, -1);

	return 0;
}

void
get_editblock_info(editblock_id, ei)
editblock_id_t editblock_id;
editblockinfo_t *ei;
{
	editblock_t *eb;

	eb = (editblock_t *)editblock_id;

	ei->ei_fil = eb->eb_fil;
	ei->ei_fil_lnum = eb->eb_lnum + 1;
	ei->ei_src_lnum = get_editblock_src_lnum(eb);
	ei->ei_lines = eb->eb_lines;
	ei->ei_nlines = eb->eb_nlines;

	get_editblock_colors(&ei->ei_bg, &ei->ei_fg);
}

int
src_lnum_to_ypos(srcwin_id, fil, lnum)
srcwin_id_t srcwin_id;
fil_t *fil;
int lnum;
{
	srcwin_t *sw;

	sw = (srcwin_t *)srcwin_id;
	
	if (sw->sw_fil != fil)	
		panic("fil botch in lty");
	
	return tw_lnum_to_ypos(sw->sw_textwin_id, lnum);
}

/*  Add an edit block to file fil and return the address of the block.
 *
 *  Redisplay if fil is currently displayed in a source window.
 */
editblock_id_t
add_editblock(fil, fil_lnum, lines, nlines)
fil_t *fil;
int fil_lnum;
const char **lines;
int nlines;
{
	editblock_t *new, *eb, *prev;
	int src_lnum;

	/*  Convert from 1.. numbering to 0..
	 */
	--fil_lnum;

	/*  We want the line number passed to handle_nlines_delta to
	 *  be the source line number *before* the edit block is added,
	 *  so get its value now.
	 */
	src_lnum = lnum_fil_to_src(fil, fil_lnum);

	new = (editblock_t *)e_malloc(sizeof(editblock_t));
	new->eb_lnum = fil_lnum;
	new->eb_lines = dup_lines(lines, nlines);
	new->eb_nlines = nlines;
	new->eb_callback = NULL;
	new->eb_data = 0;
	new->eb_fil = fil;
	new->eb_se = NULL;

	/*  Insert it in the list, maintining the sort by line number.
	 */
	prev = NULL;
	for (eb = FI_EDITLINES(fil); eb != NULL; eb = eb->eb_next){
		if (eb->eb_lnum > fil_lnum)
			break;
		prev = eb;
	}
	if (prev == NULL)
		fil->fi_editblocks_id = (long)new;
	else
		prev->eb_next = new;
	new->eb_next = eb;

	handle_nlines_delta(fil, src_lnum, 0, nlines, -1);
	
	return (editblock_id_t)new;
}

void
select_editblock(editblock_id, action, call_callback)
editblock_id_t editblock_id;
editblock_action_t action;
call_callback_t call_callback;
{
	seleditblock_t *se;
	editblock_t *eb;
	srcwin_t *sw;

	eb = (editblock_t *)editblock_id;;

	switch (action) {
	case EDL_SELECT:
		if (eb->eb_se != NULL)
			return;
		se = (seleditblock_t *)e_malloc(sizeof(seleditblock_t));
		se->se_el = eb;
		se->se_next = Selected_list.se_next;
		se->se_prev = &Selected_list;
		Selected_list.se_next = se->se_next->se_prev = se;
		eb->eb_se = se;
		break;
	case EDL_DESELECT:
		if (eb->eb_se == NULL)
			return;
		se = eb->eb_se;
		se->se_prev->se_next = se->se_next;
		se->se_next->se_prev = se->se_prev;
		eb->eb_se = NULL;
		free((char *)se);
		break;
	default:
		panic("unknown action in se");
	}

	if (call_callback == DO_CALL_CALLBACK && eb->eb_callback != NULL)
		(*eb->eb_callback)(editblock_id, action);

	for (sw = Srclist.sw_next; sw != &Srclist; sw = sw->sw_next)
		if (eb->eb_fil == sw->sw_fil)
			tw_redraw_text(sw->sw_textwin_id, FALSE);
}

void
remove_all_editblocks(fil, call_callback)
fil_t *fil;
call_callback_t call_callback;
{
	editblock_t *eb, *next;

	for (eb = FI_EDITLINES(fil); eb != NULL; eb = next) {
		next = eb->eb_next;
		remove_editblock((editblock_id_t)eb, call_callback);
		eb = next;
	}
}

void
deselect_all_editblocks(call_callback)
call_callback_t call_callback;
{
	seleditblock_t *se;

	for (se = Selected_list.se_next; se != &Selected_list; se = se->se_next)
		select_editblock((editblock_id_t)se->se_el, EDL_DESELECT,
								call_callback);
}

/*  Remove an edit block editblock.
 *
 *  Redisplay if fil is currently displayed in a source window.
 */
void
remove_editblock(editblock_id, call_callback)
editblock_id_t editblock_id;
call_callback_t call_callback;
{
	editblock_t *editblock, *eb, *prev;
	int src_lnum;

	editblock = (editblock_t *)editblock_id;

	if (editblock->eb_se != NULL)
		select_editblock(editblock_id, EDL_DESELECT, call_callback);
	if (call_callback == DO_CALL_CALLBACK && editblock->eb_callback != NULL)
		(*editblock->eb_callback)(editblock_id, EDL_REMOVE);


	/*  Delete the block from the list, and map fil_lnum to src_lnum.
	 */
	src_lnum = editblock->eb_lnum;
	prev = NULL;
	eb = FI_EDITLINES(editblock->eb_fil);
	for (; eb != editblock; eb = eb->eb_next) {
		if (eb == NULL)
			panic("bad block in re");
		src_lnum += eb->eb_nlines;
		prev = eb;
	}
	if (prev == NULL)
		eb->eb_fil->fi_editblocks_id = (long)eb->eb_next;
	else
		prev->eb_next = eb->eb_next;

	handle_nlines_delta(editblock->eb_fil, src_lnum, eb->eb_nlines, 0, -1);
	
	free_lines(eb->eb_lines, eb->eb_nlines);
	free((char *)eb);
}

/*  This is called whenever an edit block is changed.  We notify any source
 *  windows displaying the file of the change.
 */
static void
handle_nlines_delta(fil, lnum, old_nlines, new_nlines, vis_lnum)
fil_t *fil;
int lnum, old_nlines, new_nlines, vis_lnum;
{
	srcwin_t *sw;

	for (sw = Srclist.sw_next; sw != &Srclist; sw = sw->sw_next)
		if (sw->sw_fil == fil) {
			if (sw->sw_hl_lnum != NO_HL_LNUM && sw->sw_hl_lnum >= lnum) {
				if (sw->sw_hl_lnum < lnum + old_nlines)
					sw->sw_hl_lnum = NO_HL_LNUM;
				else
					sw->sw_hl_lnum += new_nlines - old_nlines;
			}
			tw_handle_nlines_delta(sw->sw_textwin_id, lnum,
						old_nlines, new_nlines,
						vis_lnum, TRUE);
		}
}

static void
get_editblock_colors(p_bg, p_fg)
int *p_bg, *p_fg;
{
	static color_t color = { -1, 0xffff, 0xffff, 0 };

	if (color.co_pixel == -1) {
		const char *colorname;

		if ((colorname = wn_get_default("EditlinesColor")) != NULL)
			wn_parse_color(colorname, &color);
	 	if (wn_get_nplanes() == 1 || wn_get_pixels_by_color(&color, 1) != 0)
			color.co_pixel = WN_FG;
	}
	*p_bg = WN_BG;
	*p_fg = color.co_pixel;
}

/*  Convert a real source line number to a line number in the source
 *  window.  We adjust for any editable blocks.
 */
static int
lnum_fil_to_src(fil, fil_lnum)
fil_t *fil;
int fil_lnum;
{
	editblock_t *eb;
	int src_lnum;

	src_lnum = fil_lnum;
	for (eb = FI_EDITLINES(fil); eb != NULL; eb = eb->eb_next) {
		if (eb->eb_lnum > fil_lnum)
			break;
		src_lnum += eb->eb_nlines;
	}
	return src_lnum;
}

/*  Inverse of lnum_fil_to_src() above.
 */
static int
lnum_src_to_fil(fil, src_lnum)
fil_t *fil;
int src_lnum;
{
	editblock_t *eb;
	int fil_lnum;

	fil_lnum = src_lnum;
	for (eb = FI_EDITLINES(fil); eb != NULL; eb = eb->eb_next) { 
		if (eb->eb_lnum > fil_lnum)
			break;
		if (fil_lnum < eb->eb_lnum + eb->eb_nlines) {
			fil_lnum = eb->eb_lnum;
			break;
		}
		fil_lnum -= eb->eb_nlines;
	}

	return fil_lnum;
}

static editblock_t *
src_lnum_to_editblock(fil, src_lnum)
fil_t *fil;
int src_lnum;
{
	editblock_t *eb;
	int lnum, delta;

	delta = 0;
	for (eb = FI_EDITLINES(fil); eb != NULL; eb = eb->eb_next) {
		lnum = eb->eb_lnum + delta;
		if (src_lnum >= lnum && src_lnum < lnum + eb->eb_nlines)
			return eb;
		delta += eb->eb_nlines;
	}
	return NULL;
}

static int
get_editblock_src_lnum(eb)
editblock_t *eb;
{
	editblock_t *el1;
	int src_lnum;

	src_lnum = eb->eb_lnum;
	for (el1 = FI_EDITLINES(eb->eb_fil); el1 != eb; el1 = el1->eb_next)
		src_lnum += el1->eb_nlines;
	return src_lnum;
}

static void
src_getline(arg, src_lnum, p_line, p_fg, p_bg, p_font, hl)
char *arg;
int src_lnum;
const char **p_line;
int *p_fg, *p_bg;
font_t **p_font;
tw_hlinfo_t *hl;
{
	const char *line;
	int fil_lnum;
	bool is_selected;
	font_t *font;
	editblock_t *eb;
	fil_t *fil;
	srcwin_t *sw;

	sw = (srcwin_t *)arg;
	fil = sw->sw_fil;

	line = NULL;
	fil_lnum = src_lnum;
	for (eb = FI_EDITLINES(fil); eb != NULL; eb = eb->eb_next) {
		if (eb->eb_lnum > fil_lnum)
			break;
		if (eb->eb_lnum + eb->eb_nlines > fil_lnum) {
			line = eb->eb_lines[fil_lnum - eb->eb_lnum];
			break;
		}
		fil_lnum -= eb->eb_nlines;
	}
	if (line != NULL) {
		get_editblock_colors(p_bg, p_fg);
		is_selected = FALSE;
		font = sw->sw_editfont;
	}
	else {
		line = so_getline(fil->fi_so, fil_lnum);
		*p_bg = sw->sw_bg;
		*p_fg = sw->sw_fg;
		if (hl != NULL && lnum_is_highlighted(fil, fil_lnum))
			hl->hl_type = TW_HL_INVERT_ALL;
		font = sw->sw_srcfont;
	}

	/*  Don't do regex search highlighting if the line has already
	 *  been highlighted because we are stopped there (i.e. we want
	 *  flow-of-execution highlighting to take precedence.
	 */
	if (hl != NULL && hl->hl_type == TW_HL_NONE && src_lnum == sw->sw_hl_lnum) {
		hl->hl_type = TW_HL_INVERT;
		hl->hl_cnum = sw->sw_hl_cnum;
		hl->hl_nchars = sw->sw_hl_nchars;
	}

	*p_line = line;
	*p_font = font;
}

static void
get_src_fonts(p_srcfont, p_editfont)
font_t **p_srcfont, **p_editfont;
{
	static font_t *srcfont = NULL, *editfont = NULL;

	if (srcfont == NULL) {
		const char *name, *default_editfont_name;

		if ((name = wn_get_default("SrcFont")) != NULL) {
			if ((srcfont = wn_open_font(name)) == NULL)
				errf("Can't open srcfont %s - using default",
									     name);
		}
		if (srcfont == NULL)
			srcfont = wn_get_sysfont();
		
		if (wn_get_wm_type() == WN_X11)
			default_editfont_name =
				"-*-fixed-bold-r-normal--15-140-*-*-*-*-*-*";
		else
			default_editfont_name = "screen.b.14";

		if ((name = wn_get_default("EditFont")) != NULL) {
			editfont = wn_open_font(name);
			if (editfont == NULL) {
				errf("Can't open editfont %s - trying %s",
						name, default_editfont_name);
			}
		}
		if (editfont == NULL) {
			editfont = wn_open_font(default_editfont_name);
			if (editfont == NULL) {
				errf("Can't open default edit font %s - using system font",
							default_editfont_name);
			}
		}

		if (editfont == NULL)
			editfont = wn_get_sysfont();
	}

	*p_srcfont = srcfont;
	*p_editfont = editfont;
}

srcwin_id_t
src_make_srcwin(wn)
int wn;
{
	srcwin_t *sw;
	int line_height;

	sw = (srcwin_t *)e_malloc(sizeof(srcwin_t));

	sw->sw_fil = NULL;
	sw->sw_wn = wn;
	sw->sw_fg = WN_FG;
	sw->sw_bg = WN_BG;
	sw->sw_hist = NULL;
	get_src_fonts(&sw->sw_srcfont, &sw->sw_editfont);
	sw->sw_data = NULL;
	sw->sw_hl_lnum = NO_HL_LNUM;

	if (sw->sw_srcfont->ft_height > sw->sw_editfont->ft_height)
		line_height = sw->sw_srcfont->ft_height;
	else
		line_height = sw->sw_editfont->ft_height;

	sw->sw_textwin_id = tw_make_textwin(wn, WN_BG, line_height);

	sw->sw_next = Srclist.sw_next;
	Srclist.sw_next = sw;

	return (srcwin_id_t)sw;
}

void
src_set_textwin_callback_and_data(srcwin_id, textwin_callback, data)
srcwin_id_t srcwin_id;
tw_callback_t textwin_callback;
char *data;
{
	tw_set_callback_and_data(((srcwin_t *)srcwin_id)->sw_textwin_id,
				 textwin_callback, data);
}

void
src_set_data(srcwin_id, data)
srcwin_id_t srcwin_id;
char *data;
{
	((srcwin_t *)srcwin_id)->sw_data = data;
}

char *
src_get_data(srcwin_id)
srcwin_id_t srcwin_id;
{
	return ((srcwin_t *)srcwin_id)->sw_data;
}

void
src_get_fonts(srcwin_id, p_srcfont, p_editfont)
srcwin_id_t srcwin_id;
font_t **p_srcfont, **p_editfont;
{
	srcwin_t *sw;

	sw = (srcwin_t *)srcwin_id;

	*p_srcfont = sw->sw_srcfont;
	*p_editfont = sw->sw_editfont;
}

void
src_draw_box(srcwin_id, lnum, col, nchars)
srcwin_id_t srcwin_id;
int lnum, col, nchars;
{
	srcwin_t *sw;

	sw = (srcwin_t *)srcwin_id;

	if (sw->sw_hl_lnum != NO_HL_LNUM) {
		sw->sw_hl_lnum = NO_HL_LNUM;
		tw_redraw_text(sw->sw_textwin_id, FALSE);
	}
	tw_draw_box(sw->sw_textwin_id, lnum, col, nchars);
}

long
src_draw_arrow(srcwin_id, lnum)
srcwin_id_t srcwin_id;
int lnum;
{
	return tw_draw_arrow(((srcwin_t *)srcwin_id)->sw_textwin_id, lnum);
}

void
src_goto_ypos(srcwin_id, ypos)
srcwin_id_t srcwin_id;
int ypos;
{
	srcwin_t *sw;
	int lnum;

	sw = (srcwin_t *)srcwin_id;
	lnum = ypos / tw_get_line_height(sw->sw_textwin_id);
	tw_goto(sw->sw_textwin_id, lnum, FALSE);
}

void
src_search(srcwin_id, pattern, forwards)
srcwin_id_t srcwin_id;
const char *pattern;
bool forwards;
{
	srcwin_t *sw;
	tw_search_res_t search_res;
	int start_lnum, start_cnum, lnum, cnum, nchars;

	sw = (srcwin_t *)srcwin_id;

	if (*pattern == '\0') {
		errf("Can't search for zero length pattern");
		return;
	}
	if (sw->sw_fil == NULL) {
		errf("Can't search - no source displayed");
		return;
	}

	if (sw->sw_hl_lnum != NO_HL_LNUM &&
				tw_is_visible(sw->sw_textwin_id, sw->sw_hl_lnum)) {
		start_lnum = sw->sw_hl_lnum;
		start_cnum = sw->sw_hl_cnum + (forwards ? sw->sw_hl_nchars : 0);
	}
	else {
		start_lnum = tw_ypos_to_lnum(sw->sw_textwin_id, 0);
		start_cnum = 0;
	}

	search_res = tw_search(sw->sw_textwin_id,
			       pattern, start_lnum, start_cnum, forwards,
			       &lnum, &cnum, &nchars);
	
	switch (search_res) {
	case TW_SR_FOUND:
		if (!tw_is_visible(sw->sw_textwin_id, lnum))
			src_push_current_pos(srcwin_id);
		sw->sw_hl_lnum = lnum;
		sw->sw_hl_cnum = cnum;
		sw->sw_hl_nchars = nchars;
		src_show_lnum(srcwin_id, lnum, TRUE);
		break;
	case TW_SR_NOT_FOUND:
		errf("Reached %s of file without finding \"%s\"",
					forwards ? "end" : "start", pattern);
		break;
	case TW_SR_SEARCH_ERROR:
		errf("Internal error in searching");
		break;
	case TW_SR_INTERRUPTED:
		errf("Search interrupted");
		if (!tw_is_visible(sw->sw_textwin_id, lnum))
			src_push_current_pos(srcwin_id);
		src_show_lnum(srcwin_id, lnum, TRUE);
		break;
	default:
		panic("bad res from ts");
	}
}

int
src_scroll(srcwin_id, npixels)
srcwin_id_t srcwin_id;
int npixels;
{
	return tw_scroll(((srcwin_t *)srcwin_id)->sw_textwin_id, npixels);
}

void
src_redraw(srcwin_id)
srcwin_id_t srcwin_id;
{
	tw_redraw_text(((srcwin_t *)srcwin_id)->sw_textwin_id, TRUE);
}

int
src_get_window_height(srcwin_id)
srcwin_id_t srcwin_id;
{
	return tw_get_window_height(((srcwin_t *)srcwin_id)->sw_textwin_id);
}

/*  Fill in the srcinfo_t block pointed to by si with information about
 *  what is under position x,y in the source window.
 *
 *  If x is zero or positive, y is taken as relative to the top of the window,
 *  otherwise y is taken as relative to the start of the source file.
 */
void
src_get_info(srcwin_id, x, y, si)
srcwin_id_t srcwin_id;
int x, y;
srcinfo_t *si;
{
	srcwin_t *sw;

	sw = (srcwin_t *)srcwin_id;

	si->si_wn = sw->sw_wn;

	if (sw->sw_fil == NULL) {
		si->si_fil = NULL;
		si->si_editblock_id = NULL;
		si->si_editblock_offset = -1;
		si->si_text = NULL;
		si->si_fil_lnum = si->si_src_lnum = si->si_cnum = -1;
		return;
	}

	si->si_fil = sw->sw_fil;

	tw_get_line_info(sw->sw_textwin_id, x, y,
			    &si->si_text, &si->si_font, &si->si_src_lnum);

	if (si->si_text != NULL)
		si->si_fil_lnum = lnum_src_to_fil(sw->sw_fil, si->si_src_lnum);
	else
		si->si_fil_lnum = -1;

	if (si->si_fil_lnum == -1) {
		si->si_cnum = -1;
		si->si_editblock_id = NULL;
		si->si_editblock_offset = -1;
	}
	else {
		editblock_t *eb;
		int cnum;

		eb = src_lnum_to_editblock(sw->sw_fil, si->si_src_lnum);
		si->si_editblock_id = (editblock_id_t)eb;

		if (eb != NULL)
			si->si_editblock_offset =
				      si->si_src_lnum - get_editblock_src_lnum(eb);
		else
			si->si_editblock_offset = -1;

		cnum = wn_strpos(si->si_text, x, si->si_font, eb != NULL);
		si->si_cnum = (si->si_text[cnum] != '\0') ? cnum : -1;
	}
}

void
src_clear_history(srcwin_id)
srcwin_id_t srcwin_id;
{
	srcwin_t *sw;
	hist_t *hist, *next;

	sw = (srcwin_t *)srcwin_id;

	hist = sw->sw_hist;
	for (hist = sw->sw_hist; hist != NULL; hist = next) {
		next = hist->hi_next;
		free((char *)hist);
		hist = next;
	}
	sw->sw_hist = NULL;
}

void
src_push_current_pos(srcwin_id)
srcwin_id_t srcwin_id;
{
	srcwin_t *sw;
	hist_t *hist;

	sw = (srcwin_t *)srcwin_id;

	if (sw->sw_fil != NULL) {
		hist = (hist_t *)e_malloc(sizeof(hist_t));
		hist->hi_fil = sw->sw_fil;
		hist->hi_lnum = tw_ypos_to_lnum(sw->sw_textwin_id, 0);
		hist->hi_next = sw->sw_hist;
		sw->sw_hist = hist;
	}
}

void
src_pop(srcwin_id)
srcwin_id_t srcwin_id;
{
	srcwin_t *sw;
	hist_t *hist;

	sw = (srcwin_t *)srcwin_id;

	if (sw->sw_hist == NULL)
		errf("No previous position to go back to");
	else {
		hist = sw->sw_hist;
		sw->sw_hist = hist->hi_next;
		set_src_pos(sw, hist->hi_fil, hist->hi_lnum, FALSE);
		free((char *)hist);
	}
}

static void
set_src_pos(sw, fil, lnum, centre)
srcwin_t *sw;
fil_t *fil;
int lnum;
bool centre;
{
	int nlines;

	if (sw->sw_fil != fil) {
		sw->sw_fil = fil;
		sw->sw_hl_lnum = NO_HL_LNUM;
	}

	if (fil != NULL && so_mod_time(fil->fi_so) > get_target_mod_time(fil)) {
		errf("Source file %s is newer than %s",
					fil->fi_name, get_target_name(fil));
		sw->sw_fg = WN_BG;
		sw->sw_bg = WN_FG;
	}
	else {
		sw->sw_fg = WN_FG;
		sw->sw_bg = WN_BG;
	}

	nlines = lnum_fil_to_src(fil, so_get_nlines(fil->fi_so));
	tw_set_text(sw->sw_textwin_id, sw->sw_bg, src_getline, (char *)sw,
							nlines, lnum, centre);
}

/*  Display source so that line fil_lnum of file fil is visible in the source
 *  window, and highlighted.  Just change the highlighting if the old
 *  and new highlighted lines are visible in the window.
 */
int
src_show(srcwin_id, fil, fil_lnum, centre)
srcwin_id_t srcwin_id;
fil_t *fil;
int fil_lnum;
bool centre;
{
	srcwin_t *sw;
	int src_lnum;

	sw = (srcwin_t *)srcwin_id;
	if (check_and_get(fil, fil_lnum) != 0)
		return -1;
	src_lnum = lnum_fil_to_src(fil, fil_lnum);
	if (sw->sw_fil != fil)
		set_src_pos(sw, fil, src_lnum, centre);
	else
		src_show_lnum(srcwin_id, src_lnum, centre);
	return 0;
}

void
src_show_lnum(srcwin_id, src_lnum, centre)
srcwin_id_t srcwin_id;
int src_lnum;
bool centre;
{
	srcwin_t *sw;

	sw = (srcwin_t *)srcwin_id;

	if (centre && tw_is_visible(sw->sw_textwin_id, src_lnum))
		tw_redraw_text(sw->sw_textwin_id, FALSE);
	else
		tw_goto(sw->sw_textwin_id, src_lnum, TRUE);
}

static int
check_and_get(fil, fil_lnum)
fil_t *fil;
int fil_lnum;
{
	if (open_source_file(fil, TRUE) != 0)
		return -1;

	if (fil_lnum >= so_get_nlines(fil->fi_so)) {
		errf("Can't display line %d of %s - it only has %d lines",
			       fil_lnum + 1, fil->fi_name,
			       so_get_nlines(fil->fi_so));
		return -1;
	}
	return 0;
}
