/* newtb.c - thumb bar package */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char shared_newtb_c_sccsid[] = "@(#)newtb.c	1.10 26/4/92 (UKC)";

/*  This source file implements a thumb bar package. It supports
 *  horizontal and vertical thumb bars, with marks. You initialise a
 *  thumb bar by calling tb_create, which returns a thumb bar handle.
 *  This handle is used as an argument to the thumb bar routines.
 */


#include <local/wn.h>
#include <stdlib.h>
#include <string.h>

#include <local/ukcprog.h>

#include "newtb.h"

/*  structure describing a thumb bar. Thumb bars can be horizontal
 *  or vertical. In comments like "left (top)", "left" is for a
 *  horizontal tb, "(top)" is for a vertical tb.
 */
typedef struct {
	window_t tb_wn;		/* window for this thumb bar */
	
	short tb_vsize;		/* tb width (height) */
	
	short tb_box_vstart;	/* left (top) of tb black box */
	short tb_box_vsize;	/* width (height) of box */
	short tb_box_fstart;	/* top (left) of tb black box */
	short tb_box_fsize;	/* height (width) of box */

	short tb_marks_fstart;	/* top (left) fixed marks position */
	short tb_marks_fsize;	/* height (width) or marks (0 => no marks) */
	char *tb_markmap;	/* byte map of marks */
	bool tb_no_change;	/* TRUE if mark map has not changed */

	bool tb_is_visible;	/* tb is visible in a window */
	bool tb_box_is_vertical;	/* TRUE for a vertical tb */

	int tb_curpos;		/* where the box is in info units */
	int tb_visible_units;	/* size of the displayed info */
	int tb_total_units;	/* total size of the info */

	tb_callback_t tb_callback;	/* callback function for gotos */
	char *tb_callback_data;	/* argument for callback func */
} tbar_t;

static void get_bsize PROTO((tbar_t *tb, int *p_s, int *p_e));
static void draw_box PROTO((tbar_t *tb, int os, int oe));
static void box PROTO((tbar_t *tb, int vs, int ve, int color));
static void set_units PROTO((tbar_t *tb, int visible_units, int total_units));
static void mark PROTO((tbar_t *tb, int pos, int size, int color));
static void i_refresh PROTO((tbar_t *tb, int pos, int lim));

void
tb_destroy(tbar_id)
tbar_id_t tbar_id;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	if (tb->tb_markmap != NULL)
		free(tb->tb_markmap);
	free((char *)tb);
}

/*   initialise the thumb bar to position and size shown.
 */
tbar_id_t
tb_create(wn)
int wn;
{
	tbar_t *tb;
	
	tb = (tbar_t *)e_malloc(sizeof(tbar_t));
	tb->tb_wn = wn;
	tb->tb_callback = NULL;
	tb->tb_curpos = 0;
	tb->tb_visible_units = 0;
	tb->tb_total_units = 0;

	tb->tb_box_is_vertical = FALSE;
	tb->tb_box_fstart = 0;
	tb->tb_box_vstart = 0;
	tb->tb_box_fsize = 0;
	tb->tb_box_vsize = 0;

	tb->tb_marks_fstart = 0;
	tb->tb_marks_fsize = 0;
	tb->tb_markmap = NULL;
	tb->tb_no_change = FALSE;

	tb->tb_is_visible = FALSE;

	return (tbar_id_t)tb;
}

void
tb_set_callback_and_data(tbar_id, callback, data)
tbar_id_t tbar_id;
tb_callback_t callback;
char *data;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	tb->tb_callback = callback;
	tb->tb_callback_data = data;
}

void
tb_show_pos(tbar_id, unit)
tbar_id_t tbar_id;
int unit;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	if (tb->tb_callback != NULL)
		(*tb->tb_callback)(tb->tb_callback_data, TB_SHOW_POS, unit);
}

void
tb_set_box_pos_and_size(tbar_id, box_is_vertical, box_fstart, box_fsize)
tbar_id_t tbar_id;
bool box_is_vertical;
int box_fstart, box_fsize;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	tb->tb_box_fstart = box_fstart;
	tb->tb_box_fsize = box_fsize;
	tb->tb_box_is_vertical = box_is_vertical;
}

void
tb_set_marks_pos_and_size(tbar_id, marks_fstart, marks_fsize)
tbar_id_t tbar_id;
int marks_fstart, marks_fsize;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	tb->tb_marks_fstart = marks_fstart;
	tb->tb_marks_fsize = marks_fsize;
}

void
tb_expose(tbar_id)
tbar_id_t tbar_id;
{
	int vsize, width, height;
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;

	wn_get_window_size(tb->tb_wn, &width, &height);
	vsize = tb->tb_box_is_vertical ? height : width;
	
	if (tb->tb_marks_fsize != 0) {
		if (tb->tb_markmap != NULL && vsize != tb->tb_vsize) {
			free(tb->tb_markmap);
			tb->tb_markmap = NULL;
		}
		if (tb->tb_markmap == NULL)
			tb->tb_markmap = e_malloc(vsize);
		memset(tb->tb_markmap, '\0', vsize);
	}
	tb->tb_vsize = vsize;

	if (tb->tb_box_fsize == 0)
		tb->tb_box_fsize = tb->tb_box_is_vertical ? width : height;

	tb->tb_is_visible = TRUE;
	wn_set_area(tb->tb_wn, 0, 0, width, height, WN_BG);

	tb->tb_box_vstart = tb->tb_box_vsize = 0;
	tb_goto((tbar_id_t)tb, tb->tb_curpos, FALSE);
}

/*  set the visible_units field of thumb bar - this is the number of units of
 *  information visible on the display
 */
void
tb_set_visible_units(tbar_id, visible_units)
tbar_id_t tbar_id;
int visible_units;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	set_units(tb, visible_units, tb->tb_total_units);
}

/*   set the total_units field of thumb bar - this is the total number of
 *   units of information available
 */
void
tb_set_total_units(tbar_id, total_units)
tbar_id_t tbar_id;
int total_units;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	set_units(tb, tb->tb_visible_units, total_units);
}

/*   return the visible_units field of thumb bar (see above)
 */
int
tb_get_visible_units(tbar_id)
tbar_id_t tbar_id;
{
	return ((tbar_t *)tbar_id)->tb_visible_units;
}
	
static void
set_units(tb, visible_units, total_units)
tbar_t *tb;
int visible_units, total_units;
{
	int os, oe;
	
	get_bsize(tb, &os, &oe);

	tb->tb_visible_units = visible_units;
	tb->tb_total_units = total_units;

	if (total_units == 0) {
		tb->tb_box_vsize = tb->tb_vsize;
		tb->tb_box_vstart = 0;
	}
	else {
		int box_size;
		int temp;
		
		box_size = tb->tb_vsize * tb->tb_visible_units;
		box_size /= total_units;
		tb->tb_box_vsize = (box_size == 0) ? 1 : box_size;

		temp = tb->tb_curpos * tb->tb_vsize;
		tb->tb_box_vstart = temp / tb->tb_total_units;
	}

	draw_box(tb, os, oe);
}

static void
box(tb, vs, ve, color)
tbar_t *tb;
int vs, ve, color;
{
	int fs, fe;

	fs = tb->tb_box_fstart;
	fe = fs + tb->tb_box_fsize;
	if (ve > vs) {
		if (tb->tb_box_is_vertical)
			wn_set_area(tb->tb_wn, fs, vs, fe - fs, ve - vs, color);
		else
			wn_set_area(tb->tb_wn, vs, fs, ve - vs, fe - fs, color);
	}
}

static void
get_bsize(tb, p_s, p_e)
tbar_t *tb;
int *p_s, *p_e;
{
	if (tb->tb_is_visible) {
		*p_s = tb->tb_box_vstart;
		*p_e = *p_s + tb->tb_box_vsize;

		if (*p_s < 0)
			*p_s = 0;
		if (*p_e > tb->tb_vsize)
			*p_e = tb->tb_vsize;
	}
	else
		*p_s = *p_e = 0;
}
		
#define MAX(x, y)	(((x) > (y)) ? (x) : (y))
#define MIN(x, y)	(((x) < (y)) ? (x) : (y))

/*  draw the marker box for the thumb bar tb in color color, which can
 *  be WN_FG or WN_BG. Clips the box to the thumb bar dimensions.
 */
static void
draw_box(tb, os, oe)
tbar_t *tb;
int os, oe;
{
	int ns, ne, cs, ce;
	
	tb->tb_is_visible = TRUE;
	get_bsize(tb, &ns, &ne);

	cs = MAX(os, ns);
	ce = MIN(oe, ne);

	if (ce < cs) {
		box(tb, os, oe, WN_BG);
		box(tb, ns, ne, WN_FG);
	}
	else {
		box(tb, os, cs, WN_BG);
		box(tb, ce, oe, WN_BG);
		box(tb, ns, cs, WN_FG);
		box(tb, ce, ne, WN_FG);
	}
}

/*  goto pos (in info units) of tbar_id - ie move the box to show this
 */
void
tb_goto(tbar_id, pos, call_callback)
tbar_id_t tbar_id;
int pos;
bool call_callback;
{
	tbar_t *tb;
	int os, oe, temp;
	
	tb = (tbar_t *)tbar_id;
	if (tb->tb_total_units <= 0)
		return;

	if (call_callback && tb->tb_callback != NULL)
		pos = (*tb->tb_callback)(tb->tb_callback_data, TB_GOTO, pos);

	get_bsize(tb, &os, &oe);

	tb->tb_curpos = pos;
	temp = pos * tb->tb_vsize;
	tb->tb_box_vstart = temp / tb->tb_total_units;
	draw_box(tb, os, oe);
}

/*   move n units from current position in thumb bar
 */
void
tb_scroll(tbar_id, n, call_callback)
tbar_id_t tbar_id;
int n;
bool call_callback;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	
	if (call_callback && tb->tb_callback != NULL)
		n = (*tb->tb_callback)(tb->tb_callback_data, TB_SCROLL, n);

	tb_goto(tbar_id, tb->tb_curpos + n, FALSE);
}

/*   Return the info pos (in info units) indicated by coord (in tb relative
 *   pixels).
 */
int
tb_tbpos_to_unit(tbar_id, wn, coord, centre)
int wn;
tbar_id_t tbar_id;
int coord;
bool centre;
{
	tbar_t *tb;
	int res, temp, tx, ty;
	
	tb = (tbar_t *)tbar_id;
	wn_trans_coords(wn, coord, coord, tb->tb_wn, &tx, &ty);
	coord = (tb->tb_box_is_vertical) ? ty : tx;
	if (coord < 0)
		coord = 0;
	if (coord >= tb->tb_vsize)
		coord = tb->tb_vsize - 1;

	temp = coord * tb->tb_total_units;
	res = temp / tb->tb_vsize;

	if (centre) {
		res -= tb->tb_visible_units / 2;
		if (res < 0)
			res = 0;
	}

	return res;
}	

/*  clear all the thumb bar marks
 */
void
tb_clear_marks(tbar_id)
tbar_id_t tbar_id;
{
	tbar_t *tb;
	
	tb = (tbar_t *)tbar_id;

	if (tb->tb_markmap == NULL)
		panic("no markmap in cm");
	memset(tb->tb_markmap, '\0', tb->tb_vsize);
	mark(tb, 0, tb->tb_vsize, WN_BG);
}

/*  display any marks that have been made or cleared with updating off
 *  (see tb_mark() below).
 */
void
tb_refresh(tbar_id)
tbar_id_t tbar_id;
{
	tbar_t *tb;

	tb = (tbar_t *)tbar_id;
	if (tb->tb_markmap == NULL)
		panic("no markmap in r");
	if (!tb->tb_no_change)
		i_refresh(tb, 0, wn_get_width(tb->tb_wn));
}

/*  internal refresh routine. Displays marks from the mark map for this
 *  tb from pos to lim.
 */
static void
i_refresh(tb, pos, lim)
tbar_t *tb;
int pos, lim;
{
	char *mtab;
	int curcol;
	int lastpos, save;

	mtab = tb->tb_markmap;
	save = mtab[lim];
	mtab[lim] = !mtab[lim-1];
	lastpos = pos;
	curcol = (mtab[pos] > 0) ? WN_FG : WN_BG;

	for (; pos <= lim; pos++) {
		if (((mtab[pos] > 0) ? WN_FG : WN_BG) != curcol) {
			mark(tb, lastpos, pos - lastpos, curcol);
			curcol = (curcol == WN_BG) ? WN_FG : WN_BG;
			lastpos = pos;
		}
	}

	mtab[lim] = save;
	tb->tb_no_change = TRUE;
}
	
/*  Mark an area from pos to size (in info units) in color color on
 *  thumb bar tbar_id.
 *  If update is FALSE, the marks will not be displayed until the next
 *  call of tb_refresh or tb_mark with updating on.
 *  The package remembers how many times a particular bit of the thumb bar
 *  has been marked, and bits do not disappear until their mark count
 *  has fallen to zero. (E.g. if you mark a bit twice, you must unmark
 *  it twice to make the bit go white again.
 */
void
tb_mark(tbar_id, pos, size, color, update)
tbar_id_t tbar_id;
int pos, size, color, update;
{
	tbar_t *tb;
	char *cptr, *lim;
	int temp, inc, mpos;
	
	tb = (tbar_t *)tbar_id;

	if (tb->tb_markmap == NULL)
		panic("no tb markmap in m");
	if (tb->tb_total_units <= 0)
		return;
	temp = pos * tb->tb_vsize;
	mpos = temp / tb->tb_total_units;
	cptr = tb->tb_markmap + mpos; 
	temp = size * tb->tb_vsize;
	lim = cptr + temp / tb->tb_total_units + 1;
	if (lim - tb->tb_markmap > wn_get_width(tb->tb_wn))
		lim = wn_get_width(tb->tb_wn) + tb->tb_markmap;
	if (lim == cptr)
		lim++;
	inc = (color == WN_FG) ? 1 : -1;
	while (cptr < lim)
		*cptr++ += inc;
	if (update) {
		if (tb->tb_no_change)
			i_refresh(tb, mpos, lim-tb->tb_markmap);
		else
			tb_refresh(tbar_id);
	}
	else
		tb->tb_no_change = FALSE;
}

/*  Do the actual display of marks on a tb, form pos to size in tb
 *  relative pixels, in color color.
 */
static void
mark(tb, pos, size, color)
tbar_t *tb;
int pos, size, color;
{
	int x, y, width, height;
	
	if (tb->tb_box_is_vertical) {
		width = tb->tb_marks_fsize;
		height = size;
		x = tb->tb_marks_fstart;
		y = pos;
	}
	else {
		width = size;
		height = tb->tb_marks_fsize;
		x = pos;
		y = tb->tb_marks_fstart;
	}
	wn_set_area(tb->tb_wn, x, y, width, height, color);
}
