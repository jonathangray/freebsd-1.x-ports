/* wn_icon.c - X icon handling code (see comment below) */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_icon_c_sccsid[] = "@(#)wn_icon.c	1.16 25/4/92 (UKC)";

/* The stuff in this file is mostly undocumented, and exists only to
 * support the ICL version of guide with multiple stacked icons.
 *
 * It should not be used by other applications.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <fcntl.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_win.h"
#include "wn_misc.h"
#include "wn_icon.h"

static void zoom PROTO((int wn, int nstep, int ox, int oy, int ow, int oh, int nx, int ny, int nw, int nh));

static int Zoom_enabled = FALSE;

#ifdef X11
extern char *_wn_Icon_geometry;

#define MAPPED(xwa)		((xwa).map_state != IsUnmapped)

/*  Height of icon window in pixels.
 */
#define ICON_HEIGHT 32

#define BLOB_WIDTH	20
#define BLOB_HEIGHT	20

#define NAME_MARG	8

int
_wn_create_icon_win(w, main_w, name)
swin_t *w, *main_w;
const char *name;
{
	int icon_x, icon_y, icon_width;
	Window icon_win;
	XWindowAttributes info;
#ifdef X11
	unsigned junk;
#endif /* X11 */

	icon_width = BLOB_WIDTH + NAME_MARG * 3 + wn_strwidth(name, (font_t *)NULL);
	XGetWindowAttributes(wn__Dpy, main_w->w_win, &info);
	icon_x = info.x + (info.width - icon_width) / 2;
	icon_y = info.y + (info.height - ICON_HEIGHT) / 2;

	/*  We shouldn't need to do this - it should be handled by setting
	 *  icon_position in the window manager hints in _wn_map_X_window().
	 *  It doesn't seem to work though.
	 */
	if (_wn_Icon_geometry != NULL)
		(void) XParseGeometry(_wn_Icon_geometry,
						&icon_x, &icon_y, &junk, &junk);

#ifdef X11
	icon_win = XCreateSimpleWindow(wn__Dpy,
				  RootWindow(wn__Dpy, DefaultScreen(wn__Dpy)),
				  icon_x, icon_y,
				  (unsigned)icon_width, (unsigned)ICON_HEIGHT,
				  (unsigned)info.border_width,
				  (xpixel_t)_wn_Fg_pixel, (xpixel_t)_wn_Bg_pixel);
#endif /* X11 */
	if (icon_win == 0)
		return -1;
	XSelectInput(wn__Dpy, icon_win, ICON_MASK);
	w->w_win = icon_win;
	wn__update_size(w);
	return 0;
}

static void
zoom(wn, nstep, ox, oy, ow, oh, nx, ny, nw, nh)
int wn, nstep, ox, oy, ow, oh, nx, ny, nw, nh;
{
	int tlx, tly, brx, bry, lx, ly, lw, lh;
	int t, i, done_one;

	lx = ly = lw = lh = 0;	/* to satisfy gcc and lint */
	done_one = FALSE;
	wn_updating_off(wn);
	for (i = 1; i < nstep; i++) {
		tlx = ox + (t = (nx - ox) * i) / nstep;
		tly = oy + (t = (ny - oy) * i) / nstep;
		brx = ox + ow + (t = ((nx + nw) - (ox + ow)) * i) / nstep;
		bry = oy + oh + (t = ((ny + nh) - (oy + oh)) * i) / nstep;
		if (done_one)
			wn_box_round(wn, lx, ly, lw, lh, WN_INVERT);
		lx = tlx;
		ly = tly;
		lw = brx - tlx;
		lh = bry - tly;
		wn_box_round(wn, lx, ly, lw, lh, WN_INVERT);
		wn_show_updates(wn);
		done_one = TRUE;
	}
	if (done_one)
		wn_box_round(wn, lx, ly, lw, lh, WN_INVERT);
	wn_updating_on(wn);
}

/*  BUG: this was hacked in for the ICL version of guide.  It should
 *       be hacked out again.
 */
void
_wn_wzoom(oldwin, newwin)
Window oldwin, newwin;
{
	XWindowAttributes oi, ni;
	int wn;

	if (Zoom_enabled) {
		if (XGetWindowAttributes(wn__Dpy, oldwin, &oi) == 0)
			wn__panic("wn__update_size");
		if (XGetWindowAttributes(wn__Dpy, newwin, &ni) == 0)
			wn__panic("wn__update_size");
		if (MAPPED(oi) && !MAPPED(ni)) {
			wn = wn_open_rootwindow();
			XGrabServer(wn__Dpy);
			zoom(wn, 12, oi.x, oi.y, oi.width, oi.height,
					ni.x, ni.y, ni.width, ni.height);
			XUnmapWindow(wn__Dpy, oldwin);
			XMapWindow(wn__Dpy, newwin);
			wn_close_window(wn);
			XUngrabServer(wn__Dpy);
		}
	}
	else {
		XUnmapWindow(wn__Dpy, oldwin);
		XMapWindow(wn__Dpy, newwin);
	}
}
#endif /* X11 */

int
wn_is_iconised(wn)
int wn;
{
#ifdef X11
	swin_t *w;
	XWindowAttributes wa;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	if (XGetWindowAttributes(wn__Dpy, w->w_win, &wa) == 0)
		wn__panic("_wn_is_iconised");
	return !MAPPED(wa);
#else
	return FALSE;
#endif /* !X11 */
}

void
_wn_enable_zoom()
{
	Zoom_enabled = TRUE;
}

void
wn_iconise(wn)
int wn;
{
#ifdef X11
	swin_t *w;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	w->w_bw->bw_is_mapped = FALSE;
	w->w_bw->bw_assocw->w_bw->bw_is_mapped = TRUE;
	_wn_wzoom(w->w_win, w->w_bw->bw_assocw->w_win);
	wn__do_xflush();
#endif /* X11 */
}

typedef int (*deiconise_func_t)PROTO((int wn));

deiconise_func_t
wn_set_deiconise_func(wn, func)
int wn;
deiconise_func_t func;
{
	deiconise_func_t old;
	swin_t *w;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	old = w->w_bw->bw_deiconise_func;
	w->w_bw->bw_deiconise_func = func;
	return old;
}

int
wn_deiconise(wn)
int wn;
{
#ifdef X11
	swin_t *w;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	w->w_bw->bw_is_mapped = TRUE;
	w->w_bw->bw_assocw->w_bw->bw_is_mapped = FALSE;
	_wn_wzoom(w->w_bw->bw_assocw->w_win, w->w_win);
	wn__do_xflush();
	return TRUE;
#endif /* X11 */
}

void
wn_configure_icon_window(wn, x, y, width, height)
int wn, x, y, width, height;
{
#ifdef X11
#ifdef X11
	XSetWindowAttributes xwa;
#endif /* X11 */
	swin_t *iw;

	W_CHECK(wn);
	iw = WN_TO_W(wn)->w_bw->bw_assocw;
#ifdef X11
	xwa.override_redirect = TRUE;
	XChangeWindowAttributes(wn__Dpy, iw->w_win, CWOverrideRedirect, &xwa);
#endif /* X11 */
	XMoveResizeWindow(wn__Dpy, iw->w_win, x, y, (xdim_t)width, (xdim_t)height);
	wn__update_size(iw);
#endif /* X11 */
}

/*  BUG: only works for outer windows and under X windows.
 */
void
wn_get_root_wpos(wn, p_x, p_y)
int wn, *p_x, *p_y;
{
	swin_t *w;
#ifdef X11
	XWindowAttributes infobuf;
#endif /* X11 */

	W_CHECK(wn);
	w = WN_TO_W(wn);
#ifdef X11

	if (XGetWindowAttributes(wn__Dpy, w->w_win, &infobuf) == 0)
		wn__panic("wn__update_size");
	*p_x = infobuf.x;
	*p_y = infobuf.y;
#else
	wn__panic("wn_get_root_wpos not yet implemented");
#endif
}
	
typedef void (*draw_icon_func_t)PROTO((int wn));

draw_icon_func_t
wn_set_draw_icon_func(wn, func)
int wn;
draw_icon_func_t func;
{
	swin_t *w;
	draw_icon_func_t oldfunc;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	oldfunc = w->w_bw->bw_draw_icon_func;
	w->w_bw->bw_draw_icon_func = func;
	return oldfunc;
}

void
_wn_draw_icon_window(wn)
int wn;
{
#ifdef X11
	int x, y;

	x = NAME_MARG;
	y = (ICON_HEIGHT - BLOB_HEIGHT) / 2;
	wn_set_area(wn, x, y, BLOB_WIDTH, BLOB_HEIGHT, WN_FG);
	
	x = NAME_MARG + BLOB_WIDTH + NAME_MARG;
	y = (ICON_HEIGHT - wn_get_sysfont()->ft_height) / 2;
	wn_tputs(wn, wn_get_window_name(wn), x, y);
#endif /* X11 */
}

