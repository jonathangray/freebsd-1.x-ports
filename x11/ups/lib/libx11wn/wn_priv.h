/* wn_priv.h - private header file for the wn library */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn_priv.h	1.15 25/4/92 (UKC) */

#ifdef X11
#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef unsigned long xpixel_t;
typedef unsigned long xplanemask_t;
typedef unsigned xdim_t;
#endif /* X11 */

#ifdef SUNVIEW
#include <sys/time.h>
#include <fcntl.h>
#include <errno.h>
#include <pixrect/pixrect.h>
#include <pixrect/memvar.h>
#include <pixrect/pixfont.h>
#include <sunwindow/defaults.h>
#include <sunwindow/rect.h>
#include <sunwindow/rectlist.h>
#include <sunwindow/pixwin.h>
#include <sunwindow/win_input.h>
#include <sunwindow/win_cursor.h>
#include <sunwindow/win_struct.h>
#include <sys/file.h>
struct pixrect *mem_create();
#endif /* SUNVIEW */

#ifndef FD_SET
typedef struct { int fds_bits[1]; } my_fd_set;
#define fd_set	my_fd_set
#undef FD_CLR
#undef FD_ISSET
#undef FD_ZERO
#define FD_SET(n, p)	((p)->fds_bits[0] |= (1 << (n)))
#define FD_CLR(n, p)	((p)->fds_bits[0] &= ~(1 << (n)))
#define FD_ISSET(n, p)	((p)->fds_bits[0] & (1 << (n)))
#define FD_ZERO(p)	((p)->fds_bits[0] = 0)
#endif

extern int errno;

/*  Structure describing a special cursor.  Used only in wn_sc.c
 */
typedef struct scst {
	unsigned short sc_flags;
	void (*sc_func)PROTO((int wn, ...));
	short sc_x1;
	short sc_y1;
	short sc_x2;
	short sc_y2;
	short sc_cx1;
	short sc_cy1;
	short sc_cx2;
	short sc_cy2;
	short sc_last_x;
	short sc_last_y;
} sc_t;

/*  Structure describing a base window (as opposed to a subwindow)
 */
typedef struct bwinst {
#ifdef X11
	struct swinst *bw_assocw;
	short bw_is_icon;
	short bw_is_mapped;
	short bw_last_rop_was_damaged;
#endif /* X11 */
#ifdef SUNVIEW
	short bw_resized;
	short bw_can_refresh;
	int bw_planes;
#endif /* SUNVIEW */
	const char *bw_name;
	bitmap_t *bw_cursor;
	cursor_t bw_cid;
	short bw_inmode;
	short bw_upd_level;
	short bw_winch_event;
	short bw_had_lost_selection_event;
	short bw_mouse_x;
	short bw_mouse_y;
	short bw_buttons;
	short bw_char;
	void (*bw_draw_icon_func)PROTO((int wn));
	int (*bw_deiconise_func)PROTO((int wn));
	sc_t bw_scbuf;
	int bw_have_sc;
	int bw_sc_drawn;
} bwin_t;

typedef struct swinst {
	bwin_t *w_bw;
#ifdef X11
	Window w_win;
#endif /* X11 */
#ifdef SUNVIEW
	struct pixwin *w_pw;
#endif /* SUNVIEW */
	long w_user_data;
	short w_wn;
	short w_type;	/* WN_OUTPUT_ONLY or WN_INPUT_OUTPUT */
	short w_x_offs;
	short w_y_offs;
	short w_width;
	short w_height;
#ifndef xwindowsNOTYET
	struct swinst *w_next;
	struct swinst *w_base;
	struct swinst *w_parent;
	struct swinst *w_child;
#endif /* !xwindowsNOTYET */
} swin_t;

#define WN_TO_W(wn)		(_wn_Windowtab[wn])

#define IS_BASEWIN(w)		((w)->w_parent == NULL)

/*  Under X windows and Suntools, the system handles coordinate adjustment
 *  for us.  On other systems, we must do it.
 */
#if (defined(X11) && defined(XSUBWINS)) || (defined(SUNVIEW) && defined(SUNSUBWINS))
#define ADJ_COORDS(w,x,y)	/* No action */
#else
#define ADJ_COORDS(w,x,y)	{ x += w->w_x_offs; y += w->w_y_offs; }
#endif

#ifdef X11
extern Display *wn__Dpy;
extern GC _wn_Gc;
#ifdef X11
extern Colormap _wn_Cmap;
#endif /* X11 */
#define REQUEST_MASK (KeyPressMask | ButtonPressMask | ButtonReleaseMask | \
			PointerMotionMask | \
			ExposureMask | EnterWindowMask | LeaveWindowMask)

#define ICON_MASK	(ButtonPressMask | ExposureMask)

#define MENU_ID(w)		((w)->w_win)
#define X_UPDATE(w) 		{ if ((w)->w_bw->bw_upd_level==0) wn__do_xflush(); }

#endif /* X11 */

#ifdef SUNVIEW
#define MENU_ID(w)		((w)->w_pw)
#endif /* SUNVIEW */

#ifdef X11
extern xplanemask_t _wn_Planemask;
#endif /* X11 */
#ifdef SUNVIEW
extern int _wn_Planemask;
#endif /* SUNVIEW */

/*  Macros for special cursor stuff.
 */
void _wn_sc_refresh PROTO((int wn, int x, int y));
void _wn_sc_undraw PROTO((int wn));
void _wn_sc_redraw PROTO((int wn));

#define SC_REFRESH(wn,x,y)	{ if (WN_TO_W(wn)->w_bw->bw_have_sc) \
							_wn_sc_refresh(wn,x,y); }
#define SC_MUST_UNDRAW(wn)	(WN_TO_W(wn)->w_bw->bw_sc_drawn)
#define SC_UNDRAW(wn)		{ if (WN_TO_W(wn)->w_bw->bw_sc_drawn) \
								_wn_sc_undraw(wn); }
#define SC_REDRAW(wn)		{ if (WN_TO_W(wn)->w_bw->bw_have_sc) \
								_wn_sc_redraw(wn); }

extern int _wn_Nwin;

#define BADWIN(wn)		((wn)<0 || (wn) >= _wn_Nwin || WN_TO_W(wn) == NULL)

extern char wn__Badmesg[];

#define W_CHECK(wn)		{ if (BADWIN(wn)) wn__panic(wn__Badmesg); }

/*  The table of open windows.
 */
extern swin_t **_wn_Windowtab;

/*  The table translating wn rasterop functions to machine dependent ones.
 */
extern int _wn_Roptab[];

/*  Structure to save a bitmap format in.
 */
typedef struct formatst {
	short fm_used;
	char fm_bit_order;
	char fm_byte_order;
	char fm_pixel_format;
	int fm_lineinc;
} format_t;


/*  Flag bits in bm_pflags
 */
#define B_LINEINC_OK		0x1	/* bm_lineinc ok for direct machine use */
#define B_BIT_ORDER_OK		0x2	/* bit order ok for direct machine use */
#define B_DISP_BYTE_ORDER	0x4	/* byte order ok for display */
#define B_XROP_BYTE_ORDER	0x8	/* byte order ok for our rasterop */
#define B_PIXEL_FORMAT_OK      0x10	/* pixel format ok for direct machine use */
#define B_DATA_ALIGNED	       0x20	/* bm_data address suitably aligned */

#define COMMON_FLAGS		(B_DATA_ALIGNED | B_LINEINC_OK | \
					B_BIT_ORDER_OK | B_PIXEL_FORMAT_OK)

#define DISP_OK		(COMMON_FLAGS | B_DISP_BYTE_ORDER)
#define XROP_OK		(COMMON_FLAGS | B_XROP_BYTE_ORDER)

#define FORMAT_WRONG(bm, flags)	(((bm)->bm_pflags & (flags)) != (flags))

void _wn_set_machine_format PROTO((bitmap_t *bm, format_t *fm, int flags));
void _wn_restore_format PROTO((bitmap_t *bm, format_t *fm));

#define SAVE_FORMAT(bm, format, flags)	\
		{ \
			if (FORMAT_WRONG(bm, flags)) \
				_wn_set_machine_format(bm, &format, flags); \
			else \
				format.fm_used = FALSE; \
		}

#define RESTORE_FORMAT(bm, format) \
		{ \
			if (format.fm_used && !((bm)->bm_flags & BM_CHOOSE_FORMAT)) \
				_wn_restore_format(bm, &format); \
		}
