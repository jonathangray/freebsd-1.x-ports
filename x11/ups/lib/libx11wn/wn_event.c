/* wn_event.c - input event processing */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_event_c_sccsid[] = "@(#)wn_event.c	1.39 1/7/92 (UKC)";

/*  Some versions of the select() macros use bzero, so map it to memset
 */
#define bzero(s, count)		memset(s, 0, count)

#ifdef SVR4
#include <sys/fcntl.h>
#endif

#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_sc.h"
#include "wn_win.h"
#include "wn_misc.h"
#include "wn_event.h"
#include "wn_replay.h"

#ifdef X11
#include <X11/keysym.h>
#endif

static void get_event PROTO((int wn, event_t *ev, int *p_xyset));
static int select_on_mask PROTO((long win_mask, int sample, long *p_resmask));
static void set_event_win PROTO((event_t *ev));
static void catch_sigio PROTO((int ignored_sig));
static void set_intr_event_mask PROTO((swin_t *w));
static void set_normal_event_mask PROTO((swin_t *w));

#ifdef SUNVIEW
static void repair_window PROTO((swin_t *w));
static swin_t *sun_new_size PROTO((void));
static void sun_to_wn_event PROTO((Event *sun_event, event_t *ev));
#endif /* SUNVIEW */

#ifdef X11
static bool event_wanted PROTO((Display *display, XEvent *x_event, char *arg));
#endif

/*  Space for a single pushed back event.
 *  See wn_pushback_event() and wn_next_event().
 */
static event_t Pushedback_event;

#ifdef X11
/*  See extern declaration in wn_event.h.
 */
Window wn__Last_event_window;
#endif

/*  BUG: this typedef means the user can only set the first 32 fds.
 */
typedef long fdmask_t;

#define MAX_FDS		(sizeof(fdmask_t) * 8)

static long Wn_fd_mask = 0;
static long User_fd_mask = 0;

#ifdef SUNVIEW
swin_t *_wn_Fdtab[MAX_FDS];
static int Wakeup_fd = -1;

void
_wn_set_sunview_wakeup_fd(fd)
int fd;
{
	Wakeup_fd = fd;
}
#endif /* SUNVIEW */

void
_wn_change_wn_fdmask(fd)
int fd;
{
	Wn_fd_mask ^= 1 << fd;
}

fdmask_t
wn_get_wn_fds()
{
	return Wn_fd_mask;
}

fdmask_t
wn_get_fd_mask()
{
	return User_fd_mask;
}

void
wn_set_fd_mask(mask)
fdmask_t mask;
{
	User_fd_mask = mask;
}

static int
select_on_mask(win_mask, sample, p_resmask)
fdmask_t win_mask;
int sample;
fdmask_t *p_resmask;
{
	static int first_call = TRUE;
	static struct timeval zero_timeout = { 0, 0 };
	register struct timeval *timeout;
	fd_set mask;
	int sel_res;

	if (first_call) {
		if (getenv("TIMEOUT") != NULL)
			zero_timeout.tv_usec = atoi(getenv("TIMEOUT")) * 1000;
		first_call = FALSE;
	}
		
	timeout = sample ? &zero_timeout : NULL;

	for (;;) {
		FD_ZERO(&mask);
		mask.fds_bits[0] = win_mask | User_fd_mask;
#ifdef SUNVIEW
		if (Wakeup_fd != -1)
			FD_SET(Wakeup_fd, &mask);
#endif

		sel_res = select(32, &mask, (fd_set *)NULL, (fd_set *)NULL, timeout);
		switch(sel_res) {
		case -1:
			if (errno != EINTR)
				wn__panic("select failed in wn_next_event");
			return EV_INTERRUPT;
		case 0:
			return EV_MOUSE_MOVED;
		default:
#ifdef SUNVIEW
			if (Wakeup_fd != -1 && FD_ISSET(Wakeup_fd, &mask)) {
				char junkch;

				read(Wakeup_fd, &junkch, 1);
				FD_CLR(Wakeup_fd, &mask);
				if (wn__Lost_selection)
					return EV_OTHER;
			}
#endif
			if (mask.fds_bits[0] & win_mask) {
				*p_resmask = mask.fds_bits[0] & win_mask;
				return EV_OTHER;
			}
			if (mask.fds_bits[0] & User_fd_mask) {
				*p_resmask = mask.fds_bits[0] & User_fd_mask;
				return EV_OTHER_INPUT;
			}
		}
	}
	/* NOTREACHED */
}

#ifdef SUNVIEW
static int Got_sigwinch = FALSE;

/*  Refresh a window's contents from its backing pixrect.
 */
static void
repair_window(w)
swin_t *w;
{
	if (w->w_bw->bw_upd_level != 0)
		pw_batch_off(w->w_pw);
	pw_damaged(w->w_pw);
	pw_repairretained(w->w_pw);
	pw_donedamaged(w->w_pw);
	if (w->w_bw->bw_upd_level != 0)
		pw_batch_on(w->w_pw);
}

void
_wn_catch_sigwinch()
{
	register swin_t **wt;

	for (wt = _wn_Windowtab; wt < _wn_Windowtab + _wn_Nwin; wt++)
		if (*wt != NULL && IS_BASEWIN(*wt) && (*wt)->w_bw->bw_can_refresh)
			repair_window(*wt);
	Got_sigwinch = TRUE;
}

static swin_t *
sun_new_size()
{
	Rect r;
	swin_t **wt, *resized_w;
	register swin_t *w;

	resized_w = NULL;
	for (wt = _wn_Windowtab; wt < _wn_Windowtab + _wn_Nwin; wt++) {
		w = *wt;
		if (w == NULL || !IS_BASEWIN(w))
			continue;
		repair_window(w);
		win_getsize(w->w_pw->pw_windowfd,&r);
		if (r.r_width != w->w_width || r.r_height != w->w_height) {
			_wn_make_retained_pixrect(w->w_pw, r.r_width, r.r_height);
			w->w_width = r.r_width;
			w->w_height = r.r_height;
			w->w_bw->bw_resized = TRUE;
			resized_w = w;
		}
	}
	return resized_w;
}

static void
sun_to_wn_event(sun_event,ev)
register Event *sun_event;
register event_t *ev;
{
	int flipped;

	ev->ev_x = event_x(sun_event);
	ev->ev_y = event_y(sun_event);
	flipped = 0;
	switch(sun_event->ie_code) {
	case LOC_WINEXIT:
		ev->ev_buttons = 0;
		ev->ev_type = EV_WINDOW_DESELECTED;
		break;
	case LOC_WINENTER:
		ev->ev_type = EV_WINDOW_SELECTED;
		break;
	case LOC_MOVE:
	case LOC_STILL:
		ev->ev_type = EV_MOUSE_MOVED;
		break;
	case MS_LEFT:
		flipped = B_LEFT;
		break;
	case MS_MIDDLE:
		flipped = B_MIDDLE;
		break;
	case MS_RIGHT:
		flipped = B_RIGHT;
		break;
	default:
		ev->ev_type = event_is_ascii(sun_event) ? EV_KEY : EV_OTHER;
		if (ev->ev_type == EV_KEY)
			ev->ev_char = sun_event->ie_code;
		break;
	}
	if (flipped != 0) {
		ev->ev_flags = flipped;
		if (win_inputnegevent(sun_event)) {
			ev->ev_type = EV_BUTTON_UP;
			ev->ev_buttons &= ~flipped;
		}
		else {
			ev->ev_type = EV_BUTTON_DOWN;
			ev->ev_buttons |= flipped;
		}

		ev->ev_buttons &= ~(B_SHIFT_KEY | B_CONTROL_KEY);
		if (event_shift_is_down(sun_event))
			ev->ev_buttons |= B_SHIFT_KEY;
		if (event_ctrl_is_down(sun_event))
			ev->ev_buttons |= B_CONTROL_KEY;
	}
}

/* sun_get_event()
 */
static void
get_event(wn, ev, p_xyset)
int wn;
register event_t *ev;
int *p_xyset;
{
	Event sun_event;
	int sample;
	fdmask_t mask, resmask;
	register int fd;
	register swin_t *w;
	swin_t *resized_w;

	if (wn == WN_ANY) {
		w = NULL;
		sample = FALSE;
		mask = Wn_fd_mask;
	}
	else {
		W_CHECK(wn);
		w = WN_TO_W(wn)->w_base;
		sample = w->w_bw->bw_inmode == WN_SAMPLE;
		mask = 1 << w->w_pw->pw_windowfd;
	}

	if (Got_sigwinch) {
		resized_w = sun_new_size();
		Got_sigwinch = FALSE;
	}
	else
		resized_w = NULL;

	if (resized_w != NULL) {
		resized_w->w_bw->bw_resized = FALSE;
		ev->ev_wn = resized_w->w_wn;
		ev->ev_type = EV_WINDOW_RESIZED;
		return;
	}

#ifdef SUNVIEW
	if (wn__Lost_selection) {
		ev->ev_type = EV_LOST_SELECTION;
		wn__Lost_selection = FALSE;
	} else
#endif /* SUNVIEW */
	{
		ev->ev_type = select_on_mask(mask, sample, &resmask);
	}
#ifdef SUNVIEW
	if (wn__Lost_selection) {
		ev->ev_type = EV_LOST_SELECTION;
		wn__Lost_selection = FALSE;
	}
#endif /* SUNVIEW */

	if (w != NULL)
		fd = w->w_pw->pw_windowfd;
	else {
		if (ev->ev_type == EV_OTHER) {
			for (fd = 0; (resmask & 1) == 0; resmask >>= 1, ++fd)
				;
			w = _wn_Fdtab[fd];
		}
		else {
			w = WN_TO_W(WN_STDWIN);
			fd = w->w_pw->pw_windowfd;
		}
	}
	ev->ev_wn = w->w_wn;

	ev->ev_buttons = w->w_bw->bw_buttons;
	if (ev->ev_type == EV_INTERRUPT && Got_sigwinch)
		ev->ev_type = 0;
	if (ev->ev_type == EV_OTHER) {
		if (input_readevent(fd, &sun_event) == 0) {
			sun_to_wn_event(&sun_event,ev);
			*p_xyset = TRUE;
		}
		else if (errno != EINTR)
			wn__panic("input_readevent failed in get_event");
	}
	else if (ev->ev_type == EV_OTHER_INPUT)
		ev->ev_fdmask = resmask;
}
#endif /* SUNVIEW */

/*  This and the following function are hacks for ICL guide.
 *  See get_event below.
 */
static int Self_deiconise_enabled = FALSE;

void
_wn_enable_self_deiconise()
{
	Self_deiconise_enabled = TRUE;
}

#ifdef X11
#define SAMPLE_MASK	((REQUEST_MASK) & ~(PointerMotionMask))

static bool
event_wanted(display, x_event, arg)
Display *display;
XEvent *x_event;
char *arg;
{
	return ((XAnyEvent *)x_event)->window == (Window)arg ||
	       x_event->type == SelectionRequest ||
	       x_event->type == SelectionClear;
}

/* x_get_event()
 */
static void
get_event(wn, ev, p_xyset)
int wn;
register event_t *ev;
int *p_xyset;
{
	static swin_t *last_w;
	XEvent x_event;
	int sample;
	int xbstate, xbflipped, buttons, flipped, modflags;
	int old_width, old_height, xbuttons, nbytes;
	char ch;
	char *keystr;
	register swin_t *w;
	extern int _wn_Autoraise;

	if (wn == WN_ANY) {
		w = NULL;
		sample = FALSE;
	}
	else {
		W_CHECK(wn);
		w = WN_TO_W(wn)->w_base;
		sample = w->w_bw->bw_inmode == WN_SAMPLE;
	}

	if (!sample && QLength(wn__Dpy) == 0) {
		fdmask_t mask, resmask;

		mask = 1 << ConnectionNumber(wn__Dpy);
		ev->ev_type = select_on_mask(mask, sample, &resmask);
		if (ev->ev_type == EV_MOUSE_MOVED) {
			ev->ev_wn = w->w_wn;
			ev->ev_buttons = w->w_bw->bw_buttons;
			return;
		}
		if (ev->ev_type != EV_OTHER) {
			ev->ev_wn = WN_STDWIN;
			ev->ev_buttons = WN_TO_W(WN_STDWIN)->w_bw->bw_buttons;
			if (ev->ev_type == EV_OTHER_INPUT)
				ev->ev_fdmask = resmask;
			return;
		}
	}
	if (sample) {
		if (!XCheckWindowEvent(wn__Dpy, w->w_win, REQUEST_MASK, &x_event)) {
			ev->ev_type = EV_MOUSE_MOVED;
			ev->ev_wn = w->w_wn;
			ev->ev_buttons = w->w_bw->bw_buttons;
			return;
		}
	}
	else {
		if (w != NULL) {
			Window win;

			if (w->w_bw->bw_is_mapped)
				win = w->w_win;
			else
				win = w->w_bw->bw_assocw->w_win;
			XIfEvent(wn__Dpy, &x_event, event_wanted, (char *)win);
		}
		else
			XNextEvent(wn__Dpy, &x_event);
	}

	if (((XAnyEvent *)&x_event)->window == wn__Last_event_window)
		w = last_w;
	else {
		extern swin_t **_wn_Windowtab;
		extern int _wn_Nwin;
		register swin_t **wp;

		for (wp = _wn_Windowtab; wp < _wn_Windowtab + _wn_Nwin; wp++)
			if (*wp != NULL &&
				    (*wp)->w_win == ((XAnyEvent *)&x_event)->window)
				break;
		if (wp == _wn_Windowtab + _wn_Nwin) {
			ev->ev_type = 0;
			return;
		}

		last_w = w = *wp;
		wn__Last_event_window = ((XAnyEvent *)&x_event)->window;
	}

	ev->ev_wn = w->w_wn;
	ev->ev_buttons = w->w_bw->bw_buttons;
	if (w->w_bw->bw_is_icon) {
		swin_t *main_w;

		main_w = w->w_bw->bw_assocw;
		if (x_event.type == Expose)
			(*main_w->w_bw->bw_draw_icon_func)(w->w_wn);

		/*  This stuff is a hack for ICL guide.
		 */
		if (Self_deiconise_enabled && x_event.type == ButtonPress) {
			xbuttons = ((XButtonPressedEvent *)&x_event)->button;
			if (xbuttons & Button1) {
				XSelectInput(wn__Dpy, w->w_win, (unsigned long)0);
				(*main_w->w_bw->bw_deiconise_func)(main_w->w_wn);
				XSelectInput(wn__Dpy, w->w_win, ICON_MASK);
			}
		}

		ev->ev_type = EV_OTHER;
		return;
	}
	switch(x_event.type) {
	case EnterNotify:
		ev->ev_type = EV_WINDOW_SELECTED;
		ev->ev_x = ((XEnterWindowEvent *)&x_event)->x;
		ev->ev_y = ((XEnterWindowEvent *)&x_event)->y;
		*p_xyset = TRUE;
		if (_wn_Autoraise) {
			XRaiseWindow(wn__Dpy, w->w_win);
			XFlush(wn__Dpy);
		}
		break;
	case LeaveNotify:
		ev->ev_type = EV_WINDOW_DESELECTED;
		ev->ev_x = ((XLeaveWindowEvent *)&x_event)->x;
		ev->ev_y = ((XLeaveWindowEvent *)&x_event)->y;
		*p_xyset = TRUE;
		break;
	case Expose:
#ifdef X11
		if (((XExposeEvent *)&x_event)->count != 0) {
			ev->ev_type = 0;
			return;
		}
#endif /* X11 */
		old_width = w->w_width;
		old_height = w->w_height;
		wn__update_size(w);
		if (w->w_width == old_width && w->w_height == old_height)
			ev->ev_type = EV_WINDOW_EXPOSED;
		else
			ev->ev_type = EV_WINDOW_RESIZED;
		break;
	case MotionNotify:
		/*  Discard all except the last MotionNotify event.
		 */
		for(;;) {
			XEvent peek_event;

			if (QLength(wn__Dpy) == 0)
				break;
			XPeekEvent(wn__Dpy, &peek_event);
			if (peek_event.type != MotionNotify)
				break;
			XNextEvent(wn__Dpy, &x_event);
		}
		ev->ev_x = ((XMotionEvent *)&x_event)->x;
		ev->ev_y = ((XMotionEvent *)&x_event)->y;
		*p_xyset = TRUE;
		ev->ev_type = EV_MOUSE_MOVED;
		break;
	case ButtonPress:
	case ButtonRelease:
		ev->ev_type = (x_event.type == ButtonPress) ?
						EV_BUTTON_DOWN : EV_BUTTON_UP;
		ev->ev_x = ((XButtonPressedEvent *)&x_event)->x;
		ev->ev_y = ((XButtonPressedEvent *)&x_event)->y;
		*p_xyset = TRUE;
		xbstate = ((XButtonPressedEvent *)&x_event)->state;
		xbflipped = ((XButtonPressedEvent *)&x_event)->button;

		buttons = 0;
		if (xbstate & Button1Mask)
			buttons |= B_LEFT;
		if (xbstate & Button2Mask)
			buttons |= B_MIDDLE;
		if (xbstate & Button3Mask)
			buttons |= B_RIGHT;

		modflags = 0;
		if (xbstate & ShiftMask)
			modflags |= B_SHIFT_KEY;
		if (xbstate & ControlMask)
			modflags |= B_CONTROL_KEY;

		switch(xbflipped) {
		case Button1:
			flipped = B_LEFT;
			break;
		case Button2:
			flipped = B_MIDDLE;
			break;
		case Button3:
			flipped = B_RIGHT;
			break;
		default:
			flipped = 0;
			ev->ev_type = EV_MOUSE_MOVED;
			break;
		}
		ev->ev_buttons = (buttons ^ flipped) | modflags;
		ev->ev_flags = flipped;
		break;
	case KeyPress:
		keystr = &ch;
		nbytes = XLookupString((XKeyPressedEvent *)&x_event, keystr, 1,
					(KeySym *)NULL, (XComposeStatus *)NULL);
		if (nbytes == 0) {
			int keysym;

			keysym = XLookupKeysym((XKeyEvent *)&x_event, 0);
			nbytes = 1;
			switch (keysym) {
				case XK_Left:	ch = WN_CH_LEFT_ARROW;	break;
				case XK_Right:	ch = WN_CH_RIGHT_ARROW;	break;
				case XK_Up:	ch = WN_CH_UP_ARROW;	break;
				case XK_Down:	ch = WN_CH_DOWN_ARROW;	break;
				case XK_F1:	ch = WN_CH_FUNCKEY(1);	break;
				case XK_F2:	ch = WN_CH_FUNCKEY(2);	break;
				case XK_F3:	ch = WN_CH_FUNCKEY(3);	break;
				case XK_F4:	ch = WN_CH_FUNCKEY(4);	break;
				case XK_F5:	ch = WN_CH_FUNCKEY(5);	break;
				case XK_F6:	ch = WN_CH_FUNCKEY(6);	break;
				case XK_F7:	ch = WN_CH_FUNCKEY(7);	break;
				case XK_F8:	ch = WN_CH_FUNCKEY(8);	break;
				case XK_F9:	ch = WN_CH_FUNCKEY(9);	break;
				case XK_F10:	ch = WN_CH_FUNCKEY(10);	break;
				case XK_F11:	ch = WN_CH_FUNCKEY(11);	break;
				case XK_F12:	ch = WN_CH_FUNCKEY(12);	break;
				default:	nbytes = 0;		break;
			}
		}
				
		ev->ev_type = (nbytes > 0) ? EV_KEY : EV_OTHER;
		if (nbytes > 0) /* BUG: what if nbytes > 1 ? */
			ev->ev_char = *keystr;

		if (((XKeyEvent *)&x_event)->state & ShiftMask)
			ev->ev_buttons |= B_SHIFT_KEY;
		if (((XKeyEvent *)&x_event)->state & ControlMask)
			ev->ev_buttons |= B_CONTROL_KEY;

		break;

	case SelectionRequest:
		wn__send_selection(&x_event);
		ev->ev_type = EV_OTHER;
		break;
	
	case SelectionClear:
		ev->ev_type = EV_LOST_SELECTION;
		break;

	default:
		ev->ev_type = 0;
	}
}

#endif /* X11 */

int
wn_inmode(wn,mode)
int wn;
int mode;
{
	register swin_t *w = WN_TO_W(wn);
	int oldmode;

	W_CHECK(wn);
	oldmode = w->w_bw->bw_inmode;
	if (mode != oldmode)
		_wn_set_inmode(w, mode);
	return oldmode;
}

void
_wn_set_inmode(w, mode)
swin_t *w;
int mode;
{
#ifdef X11
	switch(mode) {
	case WN_REQUEST:
		XSelectInput(wn__Dpy, w->w_win, REQUEST_MASK);
		break;
	case WN_SAMPLE:
		/* XSelectInput(wn__Dpy, w->w_win, SAMPLE_MASK); */
		break;
	case WN_NOINPUT:
		XSelectInput(wn__Dpy, w->w_win, ExposureMask);
		break;
	case _WN_MININPUT:
		XSelectInput(wn__Dpy, w->w_win, NoEventMask);
		break;
	default:
		wn__panic("bad mode in _wn_set_inmode");
	}
	XSync(wn__Dpy, FALSE);
#endif /* X11 */
	w->w_bw->bw_inmode = mode;
}

static void
set_event_win(ev)
event_t *ev;
{
	register int x, y;
	register swin_t *w, *par;

	w = WN_TO_W(ev->ev_wn);
	x = ev->ev_x + w->w_x_offs;
	y = ev->ev_y + w->w_y_offs;
	par = w->w_base;
	for (;;) {
		for (w = par->w_child; w != NULL; w = w->w_next) {
			if (w->w_type == WN_INPUT_OUTPUT &&
			    x >= w->w_x_offs && y >= w->w_y_offs &&
			    x < w->w_x_offs + w->w_width &&
			    y < w->w_y_offs + w->w_height)
			break;
		}
		if (w == NULL) {
			ev->ev_wn = par->w_wn;
			ev->ev_x = x - par->w_x_offs;
			ev->ev_y = y - par->w_y_offs;
			return;
		}
		par = w;
	}
}

void
wn_warp_mouse(wn, x, y)
int wn;
int x, y;
{
	register swin_t *w;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	ADJ_COORDS(w, x, y);
#ifdef X11
	XWarpPointer(wn__Dpy, w->w_win, w->w_win, 0, 0, 0, 0, x, y);
	X_UPDATE(w);
#endif /* X11 */
#ifdef SUNVIEW
	win_setmouseposition(w->w_pw->pw_windowfd, x, y);
#endif /* SUNVIEW */
}

static wn_event_handler_func_t Event_handler = NULL;

wn_event_handler_func_t
wn_interpose_event_handler(func)
wn_event_handler_func_t func;
{
	wn_event_handler_func_t old;

	old = Event_handler;
	Event_handler = func;
	return old;
}

void
wn_next_event(wn, mask, ev)
int wn;
int mask;
event_t *ev;
{
	register swin_t *w;
	register bwin_t *bw;
	int new_x, new_y, xyset;

	for (;;) {
		do {
			ev->ev_flags = 0;
			xyset = FALSE;
			if (Pushedback_event.ev_type != 0) {
				*ev = Pushedback_event;
				xyset = TRUE;
				Pushedback_event.ev_type = 0;
			}
			else {
				if (_wn_Replaying) {
					if (wn_get_recorded_event(ev) == 0)
						xyset = TRUE;
					else
						get_event(wn, ev, &xyset);
				}
				else
					get_event(wn, ev, &xyset);
			}
		} while ((mask & ev->ev_type) == 0);

		w = WN_TO_W(ev->ev_wn);
		bw = w->w_bw;
		if (!xyset) {
			ev->ev_x = bw->bw_mouse_x + w->w_x_offs;
			ev->ev_y = bw->bw_mouse_y + w->w_y_offs;
		}

		if (_wn_Recording && (mask & ev->ev_type) != 0)
			wn_record_event(ev);

		new_x = ev->ev_x + w->w_x_offs;
		new_y = ev->ev_y + w->w_y_offs;

		if (wn == WN_ANY) {
			set_event_win(ev);
			w = WN_TO_W(ev->ev_wn);
		}
		else {
			/*  If the caller asked for a particular window
			 *  and the event is in the base window of the
			 *  requested window, the translate the event
			 *  into the requested window.
			 */
			if (ev->ev_wn != wn && w->w_base->w_wn == ev->ev_wn) {
				w = WN_TO_W(wn);
				ev->ev_wn = wn;
				ev->ev_x = new_x - w->w_x_offs;
				ev->ev_y = new_y - w->w_y_offs;
			}
		}

		if (new_x != bw->bw_mouse_x || new_y != bw->bw_mouse_y) {
			SC_REFRESH(w->w_wn,new_x,new_y);
			bw->bw_mouse_x = new_x;
			bw->bw_mouse_y = new_y;
		}
		bw->bw_buttons = ev->ev_buttons;

		if (ev->ev_type == EV_WINDOW_RESIZED)
			bw->bw_winch_event = EV_WINDOW_RESIZED;
		else if (ev->ev_type == EV_WINDOW_EXPOSED && bw->bw_winch_event == 0)
			bw->bw_winch_event = EV_WINDOW_EXPOSED;
		else if (ev->ev_type == EV_LOST_SELECTION)
			bw->bw_had_lost_selection_event = TRUE;

		if (Event_handler == NULL || (*Event_handler)(ev) == 0)
			break;
	}
}

int
wn_lost_selection(wn)
int wn;
{
	register swin_t *w = WN_TO_W(wn);
	bool res;

	W_CHECK(wn);
	res = w->w_bw->bw_had_lost_selection_event;
	w->w_bw->bw_had_lost_selection_event = FALSE;
	return res;
}

unsigned long
wn_get_resize_event(wn)
int wn;
{
	register swin_t *w = WN_TO_W(wn);
	int res;

	W_CHECK(wn);
	res = w->w_bw->bw_winch_event;
	w->w_bw->bw_winch_event = 0;
	return res;
}

/*  Push back an event.  We make the event window the main window
 *  to avoid problems with the event window being deleted between
 *  here and the next call of wn_next_event().
 */
void
wn_pushback_event(ev)
event_t *ev;
{
	swin_t *w;

	W_CHECK(ev->ev_wn);
	w = WN_TO_W(ev->ev_wn);
	if (ev->ev_type == EV_WINDOW_RESIZED)
		w->w_bw->bw_winch_event = EV_WINDOW_RESIZED;
	else if (ev->ev_type == EV_WINDOW_EXPOSED && w->w_bw->bw_winch_event == 0)
		w->w_bw->bw_winch_event = EV_WINDOW_EXPOSED;
	else if (ev->ev_type == EV_LOST_SELECTION)
		w->w_bw->bw_had_lost_selection_event = TRUE;
	ev->ev_x += w->w_x_offs;
	ev->ev_y += w->w_y_offs;
	ev->ev_wn = w->w_base->w_wn;
	if ((ev->ev_type & (EV_BUTTON_UP | EV_BUTTON_DOWN)) == 0)
		ev->ev_buttons = w->w_bw->bw_buttons;
	Pushedback_event = *ev;
}

/*   Return zero if the are no characters queued on the standard input,
 *   otherwise return non-zero and set *p_ch to the next character read.
 *   p_ch must be a *character* pointer.
 */
int
wn_getc(wn,p_ch)
int wn;
char *p_ch;
{
	register swin_t *w = WN_TO_W(wn);

	W_CHECK(wn);

	if (w->w_bw->bw_char == -1)
		return FALSE;
	*p_ch = w->w_bw->bw_char;
	w->w_bw->bw_char = -1;
	return TRUE;
}

#define GP_MASK (EV_KEY | EV_BUTTONS | EV_MOUSE_MOVED | EV_WINDOW_RESIZED | EV_WINDOW_EXPOSED | EV_INTERRUPT | EV_LOST_SELECTION)

void
wn_await_window_size_change(wn)
int wn;
{
	event_t evbuf;

	wn_next_event(wn, EV_WINDOW_RESIZED, &evbuf);
	(void) wn_get_resize_event(wn);
}

int
wn_getpuck(wn, p_xpos, p_ypos)
int wn;
int *p_xpos, *p_ypos;
{
	event_t evbuf;

	wn_next_event(wn, GP_MASK, &evbuf);
	if (evbuf.ev_type == EV_KEY)
		WN_TO_W(wn)->w_bw->bw_char = evbuf.ev_char;
	*p_xpos = evbuf.ev_x;
	*p_ypos = evbuf.ev_y;
	return evbuf.ev_buttons | evbuf.ev_type |
					(evbuf.ev_flags & EV_WAS_PUSHED_BACK);
}

void
wn_ungetpuck(wn,x,y,buttons)
int wn;
int x,y,buttons;
{
	event_t evbuf;

	W_CHECK(wn);
	evbuf.ev_flags = EV_WAS_PUSHED_BACK;
	evbuf.ev_type = EV_MOUSE_MOVED;
	evbuf.ev_x = x;
	evbuf.ev_y = y;
	evbuf.ev_buttons = buttons & (B_ANY | B_SHIFT_KEY | B_CONTROL_KEY);
	evbuf.ev_wn = wn;
	wn_pushback_event(&evbuf);
}

void
wn_wait_for_release_of(wn, buttons)
int wn;
int buttons;
{
	event_t evbuf;
	
	W_CHECK(wn);
	do {
		wn_next_event(wn,  GP_MASK, &evbuf);
	} while (evbuf.ev_buttons & buttons);
}


static wn_abort_func_t Abort_func = NULL;
static swin_t *Abort_w;

static void
catch_sigio(unused_sig)
int unused_sig;
{
	(*Abort_func)();
}

wn_abort_func_t
wn_set_abort_func(wn,func)
int wn;
wn_abort_func_t func;
{
	static void (*old_io_func)PROTO((int sig));
	wn_abort_func_t old_abort_func;
	int fd;
	swin_t *w;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	old_abort_func = Abort_func;
	Abort_func = func;
	Abort_w = w;

#ifdef X11
	fd = ConnectionNumber(wn__Dpy);
#endif
#ifdef SUNVIEW
	fd = w->w_pw->pw_windowfd;
#endif

	if (func != NULL) {
		set_intr_event_mask(w);
		old_io_func = signal(SIGIO, catch_sigio);
		if (fcntl(fd, F_SETFL, FASYNC) != 0)
			wn__panic("FASYNC on botch");
	}
	else {
		(void) signal(SIGIO, old_io_func);
		if (fcntl(fd, F_SETFL, 0) != 0)
			wn__panic("FASYNC off botch");
		set_normal_event_mask(w);
	}

	return old_abort_func;
}

static void
set_intr_event_mask(w)
swin_t *w;
{
#ifdef X11
	XSelectInput(wn__Dpy, w->w_win,
		     ButtonPressMask | ButtonReleaseMask | ExposureMask);
	XSync(wn__Dpy, TRUE);
#endif
#ifdef SUNVIEW
	Inputmask mask;
	
	input_imnull(&mask);
	mask.im_flags |= IM_ASCII|IM_NEGEVENT;
	win_setinputcodebit(&mask, MS_LEFT);
	win_setinputcodebit(&mask, MS_MIDDLE);
	win_setinputcodebit(&mask, MS_RIGHT);
	win_set_pick_mask(w->w_pw->pw_windowfd, &mask);
#endif
}

static void
set_normal_event_mask(w)
swin_t *w;
{
#ifdef X11
	XSelectInput(wn__Dpy, w->w_win,
		     (w->w_bw->bw_inmode == WN_SAMPLE) ? SAMPLE_MASK : REQUEST_MASK);
	XFlush(wn__Dpy);
#endif
#ifdef SUNVIEW
	wn__setmode(w->w_wn);
#endif
}
