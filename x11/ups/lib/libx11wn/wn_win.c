/* wn_win.c - window creation and desruction */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_win_c_sccsid[] = "@(#)wn_win.c	1.22 25/4/92 (UKC)";

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_init.h"
#include "wn_icon.h"
#include "wn_event.h"
#include "wn_misc.h"
#include "wn_win.h"

static int find_wn PROTO((int wn));
static void create_subwin_batch PROTO((int parwn, batchwin_t *wins, int nwins));
static swin_t *make_w PROTO((int wn, const char *name));
static void rchange_win PROTO((swin_t *par, int x_delta, int y_delta, int width, int height));
static void delete_wtree PROTO((swin_t *par));
static swin_t *new_swin PROTO((void));
static void free_swin PROTO((swin_t *w));

swin_t **_wn_Windowtab;
int _wn_Nwin = 0;

static int Want_own_icons = FALSE;

#define CHUNK	64

static swin_t *Head_free_swin = NULL;

static swin_t *
new_swin()
{
	swin_t *ptr, *lim;

	if (Head_free_swin == NULL) {
		ptr = (swin_t *)wn__e_malloc(CHUNK * sizeof(swin_t));
		Head_free_swin = ptr;

		lim = ptr + CHUNK - 1;
		for (; ptr < lim; ptr++)
			ptr->w_next = ptr + 1;
		ptr->w_next = NULL;
	}

	ptr = Head_free_swin;
	Head_free_swin = ptr->w_next;

	return ptr;
}

static void
free_swin(ptr)
swin_t *ptr;
{
	ptr->w_next = Head_free_swin;
	Head_free_swin = ptr;
}

static int
find_wn(wn)
int wn;
{
	int i;

	if (_wn_Nwin == 0) {
		_wn_Nwin = 16;
		_wn_Windowtab = (swin_t **) wn__e_malloc(_wn_Nwin * sizeof(swin_t *));
		for (i = 0; i < _wn_Nwin; i++)
			_wn_Windowtab[i] = NULL;
	}

	if (wn != WN_ANY) {
		if (wn < 0 || wn >= _wn_Nwin)
			wn__panic(wn__Badmesg);
		return (_wn_Windowtab[wn] != NULL) ? -1 : wn;
	}

	/*  Search for free slot.
	 */
	for (wn = 0; wn < _wn_Nwin; wn++) {
		if (_wn_Windowtab[wn] == NULL)
			return wn;
	}

	/*  Search for free slot.
	 */
	_wn_Nwin *= 2;
	_wn_Windowtab = (swin_t **) realloc((char *)_wn_Windowtab,
					(size_t)_wn_Nwin * sizeof(swin_t *));
	if (_wn_Windowtab == NULL)
		wn__panic("realloc failed in find_wn");
	for (i = wn; i < _wn_Nwin; i++)
		_wn_Windowtab[i] = NULL;
	return wn;
}

int
wn_from_wid(wid)
int wid;
{
	register swin_t **wt;
	
	for (wt = _wn_Windowtab; wt < _wn_Windowtab + _wn_Nwin; wt++) {
		if (*wt != NULL && (int)MENU_ID(*wt) == wid)
			return _wn_Windowtab - wt;
	}
	if (wid >= 0 && wid < _wn_Nwin && _wn_Windowtab[wid] != NULL)
		return wid;
	
	return -1;
}

static void
create_subwin_batch(parwn, wins, nwins)
int parwn;
batchwin_t *wins;
int nwins;
{
	swin_t *parw;
	register swin_t *w;
	register batchwin_t *wb;
	int x_offs, y_offs;

	W_CHECK(parwn);
	parw = WN_TO_W(parwn);
	wb = wins + nwins;
	while (--wb >= wins) {
		if (wb->wb_type != WN_OUTPUT_ONLY && wb->wb_type != WN_INPUT_OUTPUT)
			wn__panic("bad type in wn_create_subwin");
		w = new_swin();
		wb->wb_wn = w->w_wn = find_wn(WN_ANY);
		_wn_Windowtab[w->w_wn] = w;
		w->w_bw = parw->w_bw;
		x_offs = wb->wb_x + parw->w_x_offs;
		y_offs = wb->wb_y + parw->w_y_offs;
#ifdef X11
		w->w_win = parw->w_win;
#endif /* X11 */
#ifdef SUNVIEW
		w->w_pw = parw->w_pw;
#endif /* SUNVIEW */
		w->w_user_data = 0;
		w->w_x_offs = x_offs;
		w->w_y_offs = y_offs;
		w->w_width = wb->wb_width;
		w->w_height = wb->wb_height;
		w->w_type = wb->wb_type;

		w->w_base = parw->w_base;
		w->w_next = parw->w_child;
		w->w_parent = parw;
		w->w_child = NULL;
		parw->w_child = w;
	}
}

int
wn_create_subwin(parwn, x, y, width, height, type)
int parwn;
int x, y, width, height, type;
{
	batchwin_t wbbuf;

	wbbuf.wb_x = x;
	wbbuf.wb_y = y;
	wbbuf.wb_width = width;
	wbbuf.wb_height = height;
	wbbuf.wb_type = type;
	create_subwin_batch(parwn, &wbbuf, 1);
	return wbbuf.wb_wn;
}

long
wn_set_win_data(wn, data)
int wn;
long data;
{
	swin_t *w;
	long res;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	res = w->w_user_data;
	w->w_user_data = data;
	return res;
}

long
wn_get_win_data(wn)
int wn;
{
	W_CHECK(wn);
	return WN_TO_W(wn)->w_user_data;
}

int
wn_get_wm_type()
{
#ifdef X11
	return WN_X11;
#endif /* X11 */
#ifdef SUNVIEW
	return WN_SUNVIEW;
#endif /* SUNVIEW */
}

int
wn_trans_coords(oldwn, x, y, newwn, p_x, p_y)
int oldwn, x, y, newwn;
int *p_x, *p_y;
{
	int res;
	swin_t *ow, *nw;

	W_CHECK(oldwn);
	ow = WN_TO_W(oldwn);
	W_CHECK(newwn);
	nw = WN_TO_W(newwn);

	x += ow->w_x_offs - nw->w_x_offs;
	y += ow->w_y_offs - nw->w_y_offs;
	res = x >= 0 && y >= 0 && x < nw->w_width && y < nw->w_height;
	if (p_x != NULL)
		*p_x = x;
	if (p_y != NULL)
		*p_y = y;
	return res;
}

/*  Allocate space for an swin_t structure and its associated bwin_t.
 *  Initialise the fields, apart from most of the window system dependent ones.
 */
static swin_t *
make_w(wn, name)
int wn;
const char *name;
{
	register swin_t *w;

	w = (swin_t *)wn__e_malloc(sizeof(swin_t));
	w->w_bw = (bwin_t *) wn__e_malloc(sizeof(bwin_t));
	w->w_wn = wn;
	w->w_bw->bw_name = strcpy(wn__e_malloc(strlen(name) + 1), name);
	w->w_bw->bw_inmode = WN_REQUEST;
#ifdef X11
	w->w_bw->bw_is_mapped = TRUE;
#endif
	w->w_bw->bw_buttons = 0;
	w->w_bw->bw_mouse_x = w->w_bw->bw_mouse_y = 0;
	w->w_bw->bw_char = -1;
	w->w_bw->bw_winch_event = 0;
	w->w_bw->bw_had_lost_selection_event = FALSE;
	w->w_bw->bw_upd_level = 0;
	w->w_bw->bw_cursor = NULL;
	w->w_bw->bw_draw_icon_func = _wn_draw_icon_window;
	w->w_bw->bw_deiconise_func = wn_deiconise;
	w->w_user_data = 0;
	w->w_x_offs = w->w_y_offs = 0;
	w->w_base = w;
	w->w_child = w->w_next = w->w_parent = NULL;
	w->w_bw->bw_have_sc = w->w_bw->bw_sc_drawn = FALSE;
	return w;
}

int
wn_install_wid(wid)
long wid;
{
	int wn;
	swin_t *w;
#ifdef SUNVIEW
	struct rect wr;
#endif /* SUNVIEW */

	if ((wn = find_wn(WN_STDWIN)) != WN_STDWIN)
		return -1;
	w = make_w(wn, "Unknown name");
#ifdef SUNVIEW
	w->w_pw = (struct pixwin *) wid;
	win_getsize(w->w_pw->pw_windowfd, &wr);
	w->w_width = wr.r_width;
	w->w_height = wr.r_height;
	w->w_bw->bw_resized = FALSE;
#endif /* SUNVIEW */
#ifdef X11
	w->w_win = (Window) wid;
	wn__update_size(w);
#endif /* X11 */
	_wn_Windowtab[wn] = w;
	return wn;
}

void
_wn_want_own_icon()
{
	Want_own_icons = TRUE;
}

int
wn_create_window(name)
const char *name;
{
	static int first_call = TRUE;
#ifdef X11
	swin_t *iw;
	int iwn;
#endif /* X11 */
	int wn;
	swin_t *w;

	if (first_call) {
		if (_wn_init() == -1)
			return -1;
	}

	if ((wn = find_wn(first_call ? WN_STDWIN : WN_ANY)) == -1)
		return -1;
	w = make_w(wn, name);
	if (_wn_make_window(w, name, first_call) != 0) {
		free((char *)w->w_bw);
		free((char *)w);
		return -1;
	}
	_wn_Windowtab[wn] = w;
	
	wn__setmode(wn);
#ifdef X11
	if (Want_own_icons) {
		if ((iwn = find_wn(WN_ANY)) == -1) {
			wn_close_window(wn);
			return -1;
		}
		iw = make_w(iwn, name);
		if (_wn_create_icon_win(iw, w, name) == -1) {
			wn_close_window(wn);
			return -1;
		}
		_wn_Windowtab[iwn] = iw;
		iw->w_bw->bw_assocw = w;
		iw->w_bw->bw_is_icon = TRUE;
	}
	else
		iw = NULL;
	w->w_bw->bw_assocw = iw;
	w->w_bw->bw_is_icon = FALSE;
	
#ifdef X11
	_wn_map_X_window(w, iw);
#endif /* X11 */
#endif /* X11 */
#ifdef SUNVIEW
	wn_set_area(wn, 0, 0, w->w_width, w->w_height, WN_BG);
#endif /* SUNVIEW */
	first_call = FALSE;
	return wn;
}

int
wn_open_rootwindow()
{
#ifndef X11
	return -1;
#else
	int rwn;
	swin_t *rw;

	if ((rwn = find_wn(WN_ANY)) == -1)
		return -1;
	rw = make_w(rwn, "Root window");
	rw->w_win = RootWindow(wn__Dpy, DefaultScreen(wn__Dpy));
	wn__update_size(rw);
	_wn_Windowtab[rwn] = rw;
	return rwn;
#endif /* X11 */
}

#ifdef X11
void
wn__update_size(w)
register swin_t *w;
{
	XWindowAttributes infobuf;

	if (XGetWindowAttributes(wn__Dpy, w->w_win, &infobuf) == 0)
		wn__panic("wn__update_size");
	w->w_width = infobuf.width;
	w->w_height = infobuf.height;
}
#endif /* X11 */

long
wn_get_window_handle(wn)
int wn;
{
	return (int)MENU_ID(WN_TO_W(wn));
}

#ifdef SUNVIEW
int
wn_windowfd(wn)
int wn;
{
	return WN_TO_W(wn)->w_pw->pw_windowfd;
}
#endif

static void
rchange_win(par, x_delta, y_delta, width, height)
register swin_t *par;
int x_delta, y_delta, width, height;
{
	swin_t *w;

	par->w_x_offs += x_delta;
	par->w_y_offs += y_delta;
	par->w_width = width;
	par->w_height = height;
	if (x_delta != 0 || y_delta != 0)
		for (w = par->w_child; w != NULL; w = w->w_next)
			rchange_win(w, x_delta, y_delta, w->w_width, w->w_height);
}

void
wn_swap_wins(wn1, wn2)
int wn1, wn2;
{
	swin_t **tab, *w, *par, *w1, *w2;
	int nwins, i, w1pos, w2pos;

	W_CHECK(wn1);
	W_CHECK(wn2);

	w1 = WN_TO_W(wn1);
	w2 = WN_TO_W(wn2);
	par = w1->w_parent;

	/*  The two windows must be distinct children of the same parent.
	 */
	if (w1 == w2 || w2->w_parent != par)
		wn__panic("swap_wins botch");

	w1pos = w2pos = 0;	/* to satisfy gcc */
	nwins = 0;
	for (w = par->w_child; w != NULL; w = w->w_next) {
		if (w == w1)
			w1pos = nwins;
		if (w == w2)	
			w2pos = nwins;
		++nwins;
	}

	tab = (swin_t **)wn__e_malloc(nwins * sizeof(swin_t *));
	for (i = 0, w = par->w_child; w != NULL; ++i, w = w->w_next)
		tab[i] = w;
	
	w = tab[w1pos];
	tab[w1pos] = tab[w2pos];
	tab[w2pos] = w;

	w = NULL;
	for (i = nwins - 1; i >= 0; --i) {
		tab[i]->w_next = w;
		w = tab[i];
	}
	par->w_child = w;

	free((char *)tab);
}

void
wn_change_win(wn, parwn, x, y, width, height)
int wn, parwn;
int x, y, width, height;
{
	swin_t *w, *wp;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	W_CHECK(parwn);
	wp = WN_TO_W(parwn);

	rchange_win(w, (wp->w_x_offs + x) - w->w_x_offs,
		       (wp->w_y_offs + y) - w->w_y_offs,
		       width, height);
}

void
wn_adjust_win_size(wn, x_delta, y_delta, w_delta, h_delta)
window_t wn;
int x_delta, y_delta, w_delta, h_delta;
{
	swin_t *w;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	rchange_win(w, x_delta, y_delta,
		       w->w_width + w_delta, w->w_height + h_delta);
}

void
wn_set_win_size(wn, width, height)
window_t wn;
int width, height;
{
	W_CHECK(wn);
	rchange_win(WN_TO_W(wn), 0, 0, width, height);
}


int
wn_get_width(wn)
int wn;
{
	return WN_TO_W(wn)->w_width;
}

int
wn_get_height(wn)
int wn;
{
	return WN_TO_W(wn)->w_height;
}

const char *
wn_get_window_name(wn)
int wn;
{
	return WN_TO_W(wn)->w_bw->bw_name;
}

void
wn_get_window_size(wn,p_width,p_height)
int wn;
int *p_width, *p_height;
{
	swin_t *w = WN_TO_W(wn);

	*p_width = w->w_width;
	*p_height = w->w_height;
}

int
wn_is_open(wn)
int wn;
{
	return wn >= 0 && wn < _wn_Nwin && WN_TO_W(wn) != NULL;
}

static void
delete_wtree(par)
swin_t *par;
{
	swin_t *w, *next;

	for (w = par->w_child; w != NULL; w = next) {
		next = w->w_next;
		delete_wtree(w);
	}
	_wn_Windowtab[par->w_wn] = NULL;
	free_swin(par);
}

void
wn_close_window(wn)
int wn;
{
	swin_t *w, *w2, *par;
#ifdef SUNVIEW
	int fd;
	extern swin_t *_wn_Fdtab[];
#endif /* SUNVIEW */

	W_CHECK(wn);
	w = WN_TO_W(wn);
	par = w->w_parent;
	if (par != NULL) {
		if (par->w_child == w)
			par->w_child = w->w_next;
		else {
			for (w2 = par->w_child; w2->w_next != w; w2 = w2->w_next)
				;
			w2->w_next = w->w_next;
		}
	}
#ifdef SUNVIEW
	fd = IS_BASEWIN(w) ? w->w_pw->pw_windowfd : -1;
#endif /* SUNVIEW */
#ifdef X11
	if (IS_BASEWIN(w)) {
		if (w->w_win == wn__Last_event_window)
			wn__Last_event_window = 0;
		XDestroyWindow(wn__Dpy, w->w_win);
		XFlush(wn__Dpy);
	}
	delete_wtree(w);
#endif /* X11 */
#ifdef SUNVIEW
	if (fd != -1) {
		/*  Something in SunView looks at the main window fd when
		 *  exit() is called, so leave it open.
		 */
		if (wn != WN_STDWIN)
			close(fd);
		_wn_Fdtab[fd] = NULL;
		_wn_change_wn_fdmask(fd);
	}
#endif /* SUNVIEW */
}

void
wn__setmode(wn)
int wn;
{
#ifdef X11
	_wn_set_inmode(WN_TO_W(wn), WN_REQUEST);
#endif /* X11 */

#ifdef SUNVIEW
	Inputmask mask;
	
	input_imnull(&mask);
	mask.im_flags |= IM_ASCII|IM_NEGEVENT;
	win_setinputcodebit(&mask,MS_LEFT);
	win_setinputcodebit(&mask,MS_MIDDLE);
	win_setinputcodebit(&mask,MS_RIGHT);
	win_setinputcodebit(&mask,LOC_MOVE);
	win_setinputcodebit(&mask,LOC_STILL);
	win_setinputcodebit(&mask,LOC_WINEXIT);
	win_setinputcodebit(&mask,LOC_WINENTER);
	win_set_pick_mask(WN_TO_W(wn)->w_pw->pw_windowfd,&mask);
#endif /* SUNVIEW */
}
