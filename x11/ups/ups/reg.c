/* reg.c - general purpose window region package */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_reg_c_sccsid[] = "@(#)reg.c	1.14 29/6/92 (UKC)";

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdlib.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>
#include <local/wn.h>
#include <local/obj/mhole.h>

#include "reg.h"
#include "cursors.h"
#include "debug.h"
#include "tdr.h"

#define XPOS_OR_WIDTH	0
#define YPOS_OR_HEIGHT	1

#define XPOS	XPOS_OR_WIDTH
#define YPOS	YPOS_OR_HEIGHT

#define WIDTH	XPOS_OR_WIDTH
#define HEIGHT	YPOS_OR_HEIGHT

typedef struct regionst {
	int re_wn;

	float re_prop;
	short re_minsize[2];
	short re_size[2];

	unsigned char re_flags;
	unsigned char re_cursor;

	unsigned char re_left_margin;
	unsigned char re_right_margin;
	unsigned char re_top_margin;
	unsigned char re_bottom_margin;

	const char **re_mhcaps;

	re_draw_proc_t re_draw_proc;
	re_input_proc_t re_input_proc;
	re_destroy_proc_t re_destroy_proc;

	char *re_data;

	struct regionst *re_parent;
	struct regionst *re_subregs;
	struct regionst *re_next;
} region_t;

#define RF_VSPLIT	0x01	/* region is split vertically (cf horizontally) */
#define RF_SIZE_CHANGED	0x02	/* size of region has changed since last redraw */
#define RF_REDO_PROPS	0x04	/* proportions of subregs need re-normalising */
#define RF_FIXED_SIZE	0x08	/* re_prop was specified as 0.0 */

#define HH			/* Define this if you like Douglas Adams */

static region_t *add_region PROTO((region_t *par, int wn, double prop, int size));
static void resize PROTO((region_t *par));
static void expose PROTO((region_t *par, re_draw_reason_t expose_reason));
static region_t *new_region PROTO((void));
static void check_margin_range PROTO((int n));
static void set_win_size PROTO((region_t *re, int *pos));
static void await_big_enough_window PROTO((region_t *re));
static void renormalise_props PROTO((region_t *par, bool skip_minsize_adjustment));
static void check_minsizes PROTO((region_t *par));
static void set_minsize PROTO((region_t *re, int *minsize));
static void update_window_positions PROTO((region_t *par, int dim));
static int avail_pixels PROTO((region_t *start, region_t *lim, int dim));
static region_t *prev_re PROTO((region_t *re));
static void swap_regions PROTO((region_t *sel_re, event_t *event));
static void swap_children PROTO((region_t *re1, region_t *re2));
static void rswap_regions PROTO((region_t *sel_re, event_t *ev, bool going_up));
#ifdef HH
static void grey_out PROTO((region_id_t region_id, int wn,
			    int width, int height, re_draw_reason_t draw_reason));
#endif /* HH */

ALLOC_NEW(static,region_t,region,re_next)

/*  BUG: these should be packaged up with the root of a region tree in
 *       a regtree_t structure
 */
static bool Quit_event_loop;
static region_id_t Keyboard_focus_region_id;
static region_id_t Root_region_id;

void
re_set_data(region_id, data)
region_id_t region_id;
char *data;
{
	((region_t *)region_id)->re_data = data;
}

void
re_set_cursor(region_id, cursor)
region_id_t region_id;
int cursor;
{
	if (cursor < 0 || cursor > 255)
		panic("bad cursor in sc");

	((region_t *)region_id)->re_cursor = cursor;
}

void
re_set_mhcaps(region_id, left_caption, middle_caption, right_caption)
region_id_t region_id;
const char *left_caption, *middle_caption, *right_caption;
{
	region_t *re;

	re = (region_t *)region_id;

	re->re_mhcaps = (const char **)e_malloc(4 * sizeof(char *));
	re->re_mhcaps[0] = left_caption;
	re->re_mhcaps[1] = middle_caption;
	re->re_mhcaps[2] = right_caption;
	re->re_mhcaps[3] = "";	/* anachronism from Perq days */
}

char *
re_get_data(region_id)
region_id_t region_id;
{
	return ((region_t *)region_id)->re_data;
}

int
re_get_wn(region_id)
region_id_t region_id;
{
	return ((region_t *)region_id)->re_wn;
}

void
re_set_minsize(region_id, width, height)
region_id_t region_id;
int width, height;
{
	region_t *re;
	int minsize[2];

	re = (region_t *)region_id;

	/*  Composite regions minimum sizes must be derived from their
	 *  sub-regions.
	 */
	if (re->re_subregs != NULL)
		panic("setminsize botch");

	minsize[WIDTH] = width + re->re_left_margin + re->re_right_margin;
	minsize[HEIGHT] = height + re->re_top_margin + re->re_bottom_margin;

	set_minsize(re, minsize);
}

static void
set_minsize(re, minsize)
region_t *re;
int *minsize;
{
	region_t *par;

	/*  Propagate the new minumum size requirements up the tree.
	 */
	for (par = re->re_parent; par != NULL; par = par->re_parent) {
		int dim, otherdim, par_minsize[2];

		if (re->re_minsize[WIDTH] == minsize[WIDTH] &&
		    re->re_minsize[HEIGHT] == minsize[HEIGHT])
			return;

		dim = (par->re_flags & RF_VSPLIT) ? WIDTH : HEIGHT;
		otherdim = 1 - dim;

		par_minsize[WIDTH] = par->re_minsize[WIDTH];
		par_minsize[HEIGHT] = par->re_minsize[HEIGHT];

		par_minsize[dim] += minsize[dim] - re->re_minsize[dim];
		if (minsize[otherdim] > par_minsize[otherdim])
			par_minsize[otherdim] = minsize[otherdim];
		
		re->re_minsize[WIDTH] = minsize[WIDTH];
		re->re_minsize[HEIGHT] = minsize[HEIGHT];
		check_minsizes(re);

		re = par;
		minsize[WIDTH] = par_minsize[WIDTH];
		minsize[HEIGHT] = par_minsize[HEIGHT];
	}

	re->re_minsize[WIDTH] = minsize[WIDTH];
	re->re_minsize[HEIGHT] = minsize[HEIGHT];
	check_minsizes(re);

}

static void
check_minsizes(par)
region_t *par;
{
	region_t *re;
	int dim, otherdim, minsize[2];

	if (par->re_subregs == NULL)
		return;

	dim = (par->re_flags & RF_VSPLIT) ? WIDTH : HEIGHT;
	otherdim = 1 - dim;

	minsize[WIDTH] = minsize[HEIGHT] = 0;
	for (re = par->re_subregs; re != NULL; re = re->re_next) {
		minsize[dim] += re->re_minsize[dim];
		if (re->re_minsize[otherdim] > minsize[otherdim])
			minsize[otherdim] = re->re_minsize[otherdim];
	}

	if (minsize[dim] != par->re_minsize[dim])
		panic("dim botch in cms");
	if (minsize[otherdim] > par->re_minsize[otherdim])
		panic("odim botch in cms");
}

void
re_set_callbacks(region_id, draw_proc, input_proc, destroy_proc)
region_id_t region_id;
re_draw_proc_t draw_proc;
re_input_proc_t input_proc;
re_destroy_proc_t destroy_proc;
{
	region_t *re;

	re = (region_t *)region_id;
	re->re_draw_proc = draw_proc;
	re->re_input_proc = input_proc;
	re->re_destroy_proc = destroy_proc;
}

region_id_t
re_make_region(wn)
int wn;
{
	return (region_id_t)add_region((region_t *)NULL, wn, 1.0, 0);
}

static void
check_margin_range(n)
int n;
{
	if (n < 0 || n > 255)
		panic("bad margin value in cmr");
}

void
re_set_margins(region_id, left, right, top, bottom)
region_id_t region_id;
int left, right, top, bottom;
{
	region_t *re;
	int ldelta, rdelta, tdelta, bdelta;

	check_margin_range(left);
	check_margin_range(right);
	check_margin_range(top);
	check_margin_range(bottom);

	re = (region_t *)region_id;

	ldelta = left - re->re_left_margin;
	rdelta = right - re->re_right_margin;
	tdelta = top - re->re_top_margin;
	bdelta = bottom - re->re_bottom_margin;

	re->re_flags |= RF_SIZE_CHANGED;
	wn_adjust_win_size(re->re_wn, ldelta, tdelta,
						ldelta + rdelta, tdelta + bdelta);

	re->re_left_margin = left;
	re->re_right_margin = right;
	re->re_top_margin = top;
	re->re_bottom_margin = bottom;
}

static region_t *
add_region(par, wn, prop, fixed_size)
region_t *par;
int wn;
double prop;
int fixed_size;
{
	static const char *nullcaps[] = { "", "", "", "" };
	region_t *new_re;

	new_re = new_region();

	if (par == NULL)
		new_re->re_wn = wn;
	else {
		region_t *re;

		for (re = par; re != NULL; re = re->re_parent)
			check_minsizes(re);
		new_re->re_wn = wn_create_subwin(wn, 0, 0, 0, 0, WN_INPUT_OUTPUT);
	}


	/*  We set the minimum size to zero intially, so we can use
	 *  re_set_minsize to set the actual values as a delta.
	 */
	new_re->re_minsize[WIDTH] = new_re->re_minsize[HEIGHT] = 0;

	new_re->re_size[WIDTH] = new_re->re_size[HEIGHT] = 0;

	new_re->re_left_margin = new_re->re_right_margin = 0;
	new_re->re_top_margin = new_re->re_bottom_margin = 0;

	new_re->re_flags = RF_SIZE_CHANGED;
	new_re->re_cursor = CU_DEAD;
	new_re->re_mhcaps = nullcaps;

	new_re->re_subregs = NULL;
	new_re->re_prop = prop;
	
	new_re->re_draw_proc = NULL;
	new_re->re_input_proc = NULL;
	new_re->re_destroy_proc = NULL;

	new_re->re_data = NULL;
	new_re->re_subregs = NULL;
	new_re->re_next = NULL;
	new_re->re_parent = par;

	if (prop == 0.0)
		new_re->re_flags |= RF_FIXED_SIZE;

	wn_set_win_data(new_re->re_wn, (long)new_re);

	if (par != NULL) {
		region_t *re, *last;
		int dim;

		dim = (par->re_flags & RF_VSPLIT) ? WIDTH : HEIGHT;

		last = NULL;
		for (re = par->re_subregs; re != NULL; re = re->re_next)
			last = re;
		if (last == NULL) {
			if (par->re_minsize[dim] != 0)
				panic("par ms botch");
			par->re_subregs = new_re;
		}
		else
			last->re_next = new_re;
		
		if (prop != 0.0)
			par->re_flags |= RF_REDO_PROPS;

		if (fixed_size != 0) {
			int minsize[2];

			minsize[dim] = fixed_size;
			minsize[1 - dim] = 0;
			set_minsize(new_re, minsize);
		}
	}

	{
		region_t *re;

		for (re = new_re; re != NULL; re = re->re_parent)
			check_minsizes(re);
	}

	return new_re;
}

/*  Scale the re_prop fields of all the variable size children
 *  of par so that the total is reasonably near to 1.0
 */
static void
renormalise_props(par, skip_minsize_adjustment)
region_t *par;
bool skip_minsize_adjustment;
{
	region_t *re;
	double total_props, scale, average_minsize;
	int total_vpixels, total_minsize, standard_minsize, nvregs, dim;
	bool all_minsizes_equal;

	dim = (par->re_flags & RF_VSPLIT) ? WIDTH : HEIGHT;

	nvregs = total_minsize = 0;
	total_props = 0.0;
	all_minsizes_equal = TRUE;
	standard_minsize = 0;		/* to satisfy gcc */

	for (re = par->re_subregs; re != NULL; re = re->re_next) {
		total_props += re->re_prop;
		if ((re->re_flags & RF_FIXED_SIZE) == 0) {
			++nvregs;
			total_minsize += re->re_minsize[dim];
			if (nvregs == 0)
				standard_minsize = total_minsize;
			else if (re->re_minsize[dim] != standard_minsize)
				all_minsizes_equal = FALSE;
		}
	}

	if (nvregs == 0 || total_props < 0.0001)
		panic("props botch in rp");

	scale = 1.0 / total_props;
	for (re = par->re_subregs; re != NULL; re = re->re_next)
		re->re_prop *= scale;
	
	if (all_minsizes_equal || skip_minsize_adjustment)
		return;

	/*  Compensate for the minumum size allocations by adding
	 *  or subtracting to re_prop an amount proportional to the
	 *  deviation of each min_size from the average min_size.
	 */

	average_minsize = total_minsize / nvregs;
	total_vpixels = par->re_size[dim] - par->re_minsize[dim];

	for (re = par->re_subregs; re != NULL; re = re->re_next) {
		double minsize_dev;

		if (re->re_flags & RF_FIXED_SIZE)
			continue;
		minsize_dev = re->re_minsize[dim] - average_minsize;
		re->re_prop -= minsize_dev / total_vpixels;
		if (re->re_prop < 0.0)
			re->re_prop = 0.0;
	}

	renormalise_props(par, TRUE);
}

/*  Resize the children of par such that the total width and height
 *  of the children matches par->re_size.  Recursively update descendents.
 */
static void
resize(par)
region_t *par;
{
	region_t *re, *maxsize_re;
	int dim, otherdim;
	int total_vpixels, vpixels_left, maxsize_vpixels;

	if (par->re_size[WIDTH] < par->re_minsize[WIDTH])
		panic("width botch in rs");
	if (par->re_size[HEIGHT] < par->re_minsize[HEIGHT])
		panic("height botch in rs");

	if (par->re_subregs == NULL)
		return;

	if (par->re_flags & RF_REDO_PROPS) {
		renormalise_props(par, FALSE);
		par->re_flags &= ~RF_REDO_PROPS;
	}

	dim = (par->re_flags & RF_VSPLIT) ? XPOS_OR_WIDTH : YPOS_OR_HEIGHT;
	otherdim = 1 - dim;

	vpixels_left = total_vpixels = par->re_size[dim] - par->re_minsize[dim];
	
	/*  And each region shall recieve pixels according to its need ...
	 *  Remember the largest variable size region so we can feed it
	 *  any roundoff pixels.
	 */
	maxsize_re = NULL;
	maxsize_vpixels = 0;
	for (re = par->re_subregs; re != NULL; re = re->re_next) {
		int npixels, vpixels;

		npixels = re->re_minsize[dim];

		vpixels = total_vpixels * re->re_prop;
		npixels += vpixels;
		vpixels_left -= vpixels;

		if (vpixels > maxsize_vpixels) {
			maxsize_re = re;
			maxsize_vpixels = vpixels;
		}

		if (re->re_size[dim] != npixels ||
		    re->re_size[otherdim] != par->re_size[otherdim])
			re->re_flags |= RF_SIZE_CHANGED;

		re->re_size[dim] = npixels;
		re->re_size[otherdim] = par->re_size[otherdim];
	}

	/*  BUG: what if vpixels_left is negative and maxsize_re is already
	 *       at its minimum size?
	 */
	if (vpixels_left != 0) {
		if (maxsize_re == NULL)
			panic("vpix botch");
		maxsize_re->re_size[dim] += vpixels_left;
	}
	
	update_window_positions(par, dim);
}

/*  Recalculate positions based on the new sizes and recursively
 *  resize children.
 */
static void
update_window_positions(par, dim)
region_t *par;
int dim;
{
	int pos[2];
	region_t *re;

	pos[XPOS] = pos[YPOS] = 0;
	for (re = par->re_subregs; re != NULL; re = re->re_next) {
		set_win_size(re, pos);
		if (re->re_flags & RF_SIZE_CHANGED)
			resize(re);
		pos[dim] += re->re_size[dim];
	}

	/*  Sanity check
	 */
	if (pos[dim] != par->re_size[dim])
		panic("resize botch");
}

static void
set_win_size(re, pos)
region_t *re;
int *pos;
{
	int x, y, width, height;

	x = pos[XPOS] + re->re_left_margin;
	y = pos[YPOS] + re->re_top_margin;
	width = re->re_size[WIDTH] - (re->re_left_margin + re->re_right_margin);
	height = re->re_size[HEIGHT] - (re->re_top_margin+re->re_bottom_margin);

	wn_change_win(re->re_wn, re->re_parent->re_wn, x, y, width, height);
}

static void
expose(par, expose_reason)
region_t *par;
re_draw_reason_t expose_reason;
{
	region_t *re;
	int width, height;
	re_draw_reason_t draw_reason;

	for (re = par->re_subregs; re != NULL; re = re->re_next)
		expose(re, expose_reason);

	if (par->re_draw_proc == NULL)
		return;

	if ((par->re_flags & RF_SIZE_CHANGED) && expose_reason == RE_EXPOSED) {
		par->re_flags &= ~RF_SIZE_CHANGED;
		draw_reason = RE_RESIZED;
	}
	else
		draw_reason = expose_reason;

	wn_get_window_size(par->re_wn, &width, &height);
	(*par->re_draw_proc)((region_id_t)par, par->re_wn, width, height,
								draw_reason);
}

void
re_set_exit_event_loop_flag()
{
	Quit_event_loop = TRUE;
}

bool
re_get_exit_event_loop_flag()
{
	return Quit_event_loop;
}

void
re_set_keyboard_focus(region_id)
region_id_t region_id;
{
	Keyboard_focus_region_id = region_id;
}

void
re_set_rootwin(region_id)
region_id_t region_id;
{
	Root_region_id = region_id;
}

void
re_redraw_root(event_type, clear_first)
unsigned long event_type;
bool clear_first;
{
	if (event_type == EV_WINDOW_RESIZED) {
		await_big_enough_window((region_t *)Root_region_id);
		resize((region_t *)Root_region_id);
	}

	re_expose(Root_region_id, clear_first);
}

static void
await_big_enough_window(re)
region_t *re;
{
	font_t *font;

	font = wn_get_sysfont();

	for (;;) {
		bool wide_enough, deep_enough;
		int win_width, win_height, mesg_width, mesg_height;
		const char *mesg;
		event_t event;

		wn_get_window_size(re->re_wn, &win_width, &win_height);

		wide_enough = win_width >= re->re_minsize[WIDTH];
		deep_enough = win_height >= re->re_minsize[HEIGHT];

		if (wide_enough && deep_enough) {
			re->re_size[WIDTH] = win_width;
			re->re_size[HEIGHT] = win_height;
			break;
		}

		if (wide_enough)
			mesg = "Window too short";
		else if (deep_enough)
			mesg = "Window too narrow";
		else
			mesg = "Window too small";
		
		mesg_width = wn_strwidth(mesg, font);
		mesg_height = font->ft_height;

		wn_set_area(re->re_wn, 0, 0, win_width, win_height, WN_BG);
		wn_tputs(re->re_wn, mesg, (win_width - mesg_width) / 2,
				          (win_height - mesg_height) / 2);
		wn_next_event(re->re_wn, EV_WINDOW_RESIZED | EV_WINDOW_EXPOSED,
									&event);
	}
}

void
re_event_loop(region_id)
region_id_t region_id;
{
	const int evmask = EV_BUTTON_UP | EV_BUTTON_DOWN | EV_MOUSE_MOVED | EV_KEY |
			   EV_INTERRUPT | EV_WINDOW_EXPOSED | EV_WINDOW_RESIZED;
	region_t *root, *keyboard_re;
	int last_cursor;
	static const char **last_mhcaps;

	root = (region_t *)region_id;
	last_cursor = -1;
	keyboard_re = (region_t *)Keyboard_focus_region_id;

	for (Quit_event_loop = FALSE; !Quit_event_loop; ) {
		unsigned long resize_event;
		int cursor;
		region_t *re;
		event_t event;

		resize_event = wn_get_resize_event(root->re_wn);
		if (resize_event != 0) {
			re_redraw_root(resize_event, FALSE);
			td_record_refresh();
			continue;
		}

		wn_next_event(WN_ANY, evmask, &event);

		/*  Cancel any displayed error message on a mouse press
		 *  or release, or key press.
		 */
		if (event.ev_type & (EV_BUTTON_DOWN | EV_BUTTON_UP | EV_KEY))
			clear_message();

		/*  We want key presses anywhere in the window to go to the
		 *  typing line.
		 */
		if (keyboard_re != NULL && (event.ev_type == EV_KEY ||
			(event.ev_type == EV_BUTTON_DOWN &&
			 (event.ev_buttons & (B_ANY|B_SHIFT_KEY|B_CONTROL_KEY)) ==
							(B_MIDDLE|B_SHIFT_KEY)))) {
			re = keyboard_re;
		}
		else {
			re = (region_t *)wn_get_win_data(event.ev_wn);

			cursor = (re != NULL) ? re->re_cursor : CU_DEAD;
			if (cursor != last_cursor) {
				set_bm_cursor(re->re_wn, cursor);
				last_cursor = cursor;
			}
			if (re->re_mhcaps != last_mhcaps) {
				mhdraw(re->re_mhcaps);
				last_mhcaps = re->re_mhcaps;
			}
		}

		if (re != NULL) {
			const int swmask = B_SHIFT_KEY | B_CONTROL_KEY;

			if (event.ev_type == EV_BUTTON_DOWN &&
					(event.ev_buttons & swmask) == swmask)
				swap_regions(re, &event);
			else if (re->re_input_proc != NULL)
				(*re->re_input_proc)((region_id_t)re, &event);
		}
	}
}

void
re_expose(region_id, clear_first)
region_id_t region_id;
bool clear_first;
{
	region_t *re;

	re = (region_t *)region_id;
	if (clear_first)
		wn_set_area(re->re_wn, 0, 0,
				   re->re_size[WIDTH], re->re_size[HEIGHT], WN_BG);
	expose(re, RE_EXPOSED);
}

#ifdef __STDC__
void
re_divide(region_id_t region_id, re_orientation_t orientation, ...)
{
	va_list ap;
	region_t *re;

	va_start(ap, orientation);
#else
void
re_divide(va_alist)
va_dcl
{
	va_list ap;
	region_id_t region_id;
	re_orientation_t orientation;
	region_t *re;

	va_start(ap);
	region_id = va_arg(ap, region_id_t);
	orientation = va_arg(ap, re_orientation_t);
#endif

	re = (region_t *)region_id;

	switch (orientation) {
	case RE_VERTICAL:
		if (!(re->re_flags & RF_VSPLIT)) {
			if (re->re_subregs != NULL)
				panic("tried to change region split type");
			re->re_flags |= RF_VSPLIT;
		}
		break;
	case RE_HORIZONTAL:
		if (re->re_flags & RF_VSPLIT) {
			if (re->re_subregs != NULL)
				panic("tried to change region split type");
			re->re_flags &= ~RF_VSPLIT;
		}
		break;
	default:
		panic("bad orientation in re_divide");
	}

	for(;;) {
		region_id_t *p_region_id;
		region_t *new_re;
		int size;
		double prop;

		p_region_id = va_arg(ap, region_id_t *);
		if (p_region_id == NULL)
			break;

		size = va_arg(ap, int);
		prop = va_arg(ap, double);

		new_re = add_region(re, re->re_wn, prop, size);
		if (*p_region_id != NULL) {
			region_t *old_re;

			old_re = (region_t *)*p_region_id;
			new_re->re_draw_proc = old_re->re_draw_proc;
			new_re->re_input_proc = old_re->re_input_proc;
			new_re->re_destroy_proc = old_re->re_destroy_proc;
			new_re->re_cursor = old_re->re_cursor;
			new_re->re_mhcaps = old_re->re_mhcaps;
		}
		*p_region_id = (region_id_t)new_re;
	}

	va_end(ap);
}

static region_t *
prev_re(lim)
region_t *lim;
{
	region_t *re, *prev;

	prev = NULL;
	for (re = lim->re_parent->re_subregs; re != lim; re = re->re_next)
		prev = re;
	return prev;
}

static int
avail_pixels(start, lim, dim)
region_t *start, *lim;
int dim;
{
	region_t *re;
	int avail;

	avail = 0;
	for (re = start; re != lim; re = re->re_next)
		avail += re->re_size[dim] - re->re_minsize[dim];

	return avail;
}

#ifdef HH
static void
grey_out(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	switch (draw_reason) {
	case RE_EXPOSED:
	case RE_RESIZED:
		wn_shade_area(wn, 0, 0, width, height, WN_GREY2, R_RPL);
		break;
	case RE_RESIZING:
	case RE_UNDRAW:
		break;
	default:
		panic("bad dr in ld");
	}
}
#endif

static void
swap_regions(sel_re, ev)
region_t *sel_re;
event_t *ev;
{
	int buttons;

	buttons = ev->ev_buttons & B_ANY;

	if (buttons == 0)
		return;
	wn_next_event(WN_ANY, EV_BUTTON_UP, ev);
	if (ev->ev_wn != sel_re->re_wn)
		return;

#ifdef HH
	if (buttons == B_MIDDLE && (Debug_flags & DBFLAG_MISC) != 0) {
		static region_t *last_middle_re;

		/*  The DECstation 3100 compiler objects to the following
		 *  comparison unless the (noop) cast is in place ...
		 */
		if (sel_re->re_draw_proc == (re_draw_proc_t)grey_out)
			return;

		if (sel_re == last_middle_re) {
			errf("I did say not to press this button again ...");
			sel_re->re_draw_proc = grey_out;
			sel_re->re_input_proc = NULL;
			sel_re->re_destroy_proc = NULL;
			re_expose((region_id_t)sel_re, TRUE);
		}
		else {
			errf("Please do not press this button again");
			last_middle_re = sel_re;
		}
	}
#endif /* HH */

	if (buttons & (B_LEFT | B_RIGHT))
		rswap_regions(sel_re, ev, buttons == B_LEFT);
}

static void
rswap_regions(sel_re, ev, going_up)
region_t *sel_re;
event_t *ev;
bool going_up;
{
	region_t *re, *par;
	int dim;

	if ((par = sel_re->re_parent) == NULL)
		return;
	dim = (par->re_flags & RF_VSPLIT) ? XPOS_OR_WIDTH : YPOS_OR_HEIGHT;

	re = sel_re;
	do {
		re = going_up ? prev_re(re) : re->re_next;
	} while (re != NULL && re->re_size[dim] <= 1);

	if (re == NULL)
		rswap_regions(par, ev, going_up);
	else {
		swap_children(re, sel_re);
		wn_swap_wins(re->re_wn, sel_re->re_wn);
		update_window_positions(par, dim);
		re_expose((region_id_t)par, TRUE);
		wn_warp_mouse(ev->ev_wn, ev->ev_x, ev->ev_y);
	}
}

static void
swap_children(re1, re2)
region_t *re1, *re2;
{
	region_t **tab, *re, *par;
	int nregs, i, re1pos, re2pos;
	
	par = re1->re_parent;
	if (re2->re_parent != par)
		panic("par botch in sc");

	re1pos = re2pos = 0;	/* to satisfy gcc */
	nregs = 0;
	for (re = par->re_subregs; re != NULL; re = re->re_next) {
		if (re == re1)
			re1pos = nregs;
		if (re == re2)	
			re2pos = nregs;
		++nregs;
	}

	tab = (region_t **)e_malloc(nregs * sizeof(region_t *));
	for (i = 0, re = par->re_subregs; re != NULL; ++i, re = re->re_next)
		tab[i] = re;
	
	re = tab[re1pos];
	tab[re1pos] = tab[re2pos];
	tab[re2pos] = re;

	re = NULL;
	for (i = nregs - 1; i >= 0; --i) {
		tab[i]->re_next = re;
		re = tab[i];
	}
	par->re_subregs = re;

	free((char *)tab);
}

int
re_change_position(region_id, delta)
region_id_t region_id;
int delta;
{
	region_t *re, *exp_re, *mv_re, *par, *first_re, *last_re;
	int dim, avail, total_vpixels, needed;
	bool going_up;

	mv_re = (region_t *)region_id;
	par = mv_re->re_parent;

	dim = (par->re_flags & RF_VSPLIT) ? XPOS_OR_WIDTH : YPOS_OR_HEIGHT;

	going_up = delta < 0;
	if (going_up)
		delta = -delta;

	/*  Check that we have pixels to spare in the area that's
	 *  getting smaller.
	 */
	if (going_up)
		avail = avail_pixels(par->re_subregs, mv_re, dim);
	else
		avail = avail_pixels(mv_re->re_next, (region_t *)NULL, dim);
	if (avail < delta)
		delta = avail;

	/*  Find a variable size region in the expanded area to put the
	 *  extra pixels.
	 */
	exp_re = mv_re;
	do {
		exp_re = going_up ? exp_re->re_next : prev_re(exp_re);
	} while (exp_re != NULL && (exp_re->re_flags & RF_FIXED_SIZE) != 0);

	if (exp_re == NULL || delta == 0)
		return 0;
	
	/*  Find out how far we have to go to get the pixels we want in
	 *  the area that's getting smaller.  We do this before doing
	 *  any actual size changes as we want to undraw (with the old
	 *  sizes) the regions that are going to move.
	 */
	re = mv_re;
	needed = delta;
	while (needed > 0) {
		int spare;

		re = going_up ? prev_re(re) : re->re_next;
		if (re == NULL)
			panic("pixel famine");

		spare = re->re_size[dim] - re->re_minsize[dim];
		if (spare != 0 && (re->re_flags & RF_FIXED_SIZE) == 0)
			needed -= spare;
	}

	if (going_up) {
		first_re = re;
		last_re = exp_re->re_next;
	}
	else {
		first_re = exp_re;
		last_re = re->re_next;
	}

	/*  OK, we have some pixels and we know which regions are going to
	 *  change.  We are definitely going to munge the display, so do the
	 *  undraw now.
	 */
	wn_updating_off(par->re_wn);
	for (re = first_re; re != last_re; re = re->re_next)
		expose(re, RE_UNDRAW);
	 
	total_vpixels = par->re_size[dim] - par->re_minsize[dim];

	/*  First do the compressed area.  We search back for a region with spare
	 *  pixels, and take all the spare pixels or delta, whichever is greater.
	 *  We keep going like this until we have all the pixels we want.
	 */
	re = mv_re;
	needed = delta;
	while (needed != 0) {
		int spare;

		re = going_up ? prev_re(re) : re->re_next;
		if (re == NULL)
			panic("pixel famine");

		spare = re->re_size[dim] - re->re_minsize[dim];
		if (spare == 0 || (re->re_flags & RF_FIXED_SIZE))
			continue;
		
		if (spare > needed)
			spare = needed;
		
		/*  Steal the pixels from the region, and change re_prop
		 *  to that the new proportions will be preserved on a
		 *  window size change.
		 */
		re->re_size[dim] -= spare;
		re->re_prop -= (double)spare / total_vpixels;
		re->re_flags |= RF_SIZE_CHANGED;

		needed -= spare;
	}

	/*  Add delta pixels to the region to be expanded and munge it's
	 *  proportion to match.
	 */
	exp_re->re_size[dim] += delta;
	exp_re->re_prop += (double)delta / total_vpixels;
	exp_re->re_flags |= RF_SIZE_CHANGED;

	/*  Update the window positions.
	 */
	update_window_positions(par, dim);

	for (re = first_re; re != last_re; re = re->re_next)
		expose(re, RE_RESIZING);
	wn_updating_on(par->re_wn);

	return going_up ? -delta : delta;
}

region_id_t
re_get_parent(region_id)
region_id_t region_id;
{
	return (region_id_t)((region_t *)region_id)->re_parent;
}
