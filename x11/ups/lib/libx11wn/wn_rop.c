/* wn_rop.c - on screen rasterops and line drawing */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_rop_c_sccsid[] = "@(#)wn_rop.c	1.17 20/5/92 (UKC)";

#include <sys/types.h>
#include <string.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_sc.h"
#include "wn_xrop.h"
#include "wn_bm.h"
#include "wn_misc.h"
#include "wn_rop.h"

#ifdef X11
static Bool get_gexpose_event PROTO((Display *unused_display, XEvent *xev, char *cwin));
#endif

/*  Rasterop lookup tables for the systems wn runs over.
 */
int _wn_Roptab[] = {
#ifdef X11
	GXcopy,				/* R_RPL	*/
	GXcopyInverted,			/* R_NOT	*/
	GXand,				/* R_AND	*/
	GXandInverted,			/* R_ANDNOT	*/
	GXor,				/* R_OR		*/
	GXorInverted,			/* R_ORNOT	*/
	GXxor,				/* R_XOR	*/
	GXequiv,			/* R_XNOR	*/
#endif /* X11 */
#ifdef SUNVIEW
	PIX_SRC,			/* R_RPL	*/
	PIX_NOT(PIX_SRC),		/* R_NOT	*/
	PIX_SRC & PIX_DST,		/* R_AND	*/
	PIX_NOT(PIX_SRC) & PIX_DST,	/* R_ANDNOT	*/
	PIX_SRC | PIX_DST,		/* R_OR		*/
	PIX_NOT(PIX_SRC) | PIX_DST,	/* R_ORNOT	*/
	PIX_SRC ^ PIX_DST,		/* R_XOR	*/
	PIX_NOT(PIX_SRC) ^ PIX_DST	/* R_XNOR	*/
#endif /* SUNVIEW */
};

#ifdef X11
/*  On a monochrome display we have no choice about pixel to colour
 *  mapping - a set bit is a white pixel and a clear bit is black.
 *  To get round this, we use a rather peculiar rasterop function
 *  mapping, which is set up below.
 */
void
_wn_fix_ropfuncs()
{
	static int munged_roptab[] = {
		GXcopy,				/* R_RPL	*/
		GXcopyInverted,			/* R_NOT	*/
		GXor,				/* R_AND	*/
		GXorInverted,			/* R_ANDNOT	*/
		GXand,				/* R_OR		*/
		GXandInverted,			/* R_ORNOT	*/
		GXequiv,			/* R_XOR	*/
		GXxor,				/* R_XNOR	*/
	};
	int i;

	for (i = 0; i < 8; i++)
		_wn_Roptab[i] = munged_roptab[i];
}
#endif /* X11 */

void
wn_tile_area(wn, x, y, width, height, bm, ropfunc)
int wn;
int x, y, width, height;
bitmap_t *bm;
int ropfunc;
{
	swin_t *w;
	static int first_call = -1;
	static unsigned short last[16];
	format_t format;
	int changed;
#ifdef X11
	static Pixmap grey_pixmap = 0;
#endif /* X11 */
#ifdef SUNVIEW
	static struct pixrect *grey_pr;
#endif /* SUNVIEW */

	W_CHECK(wn);
	if (bm->bm_width != 16 || bm->bm_height != 16 || bm->bm_nplanes != 1)
		wn__panic("bitmap not 16 x 16 x 1 in wn_tile_area");
	if (width <= 0 || height <= 0)
		wn__panic("Bad width or height in shade");

	w = WN_TO_W(wn);
	ADJ_COORDS(w, x, y);
	SAVE_FORMAT(bm, format, DISP_OK);
	if (first_call) {
		first_call = FALSE;
		changed = TRUE;
	}
	else
		changed = memcmp((char *)last, (char *)bm->bm_data, sizeof(last)) != 0;
	if (changed)
		memcpy((char *)last, (char *)bm->bm_data, sizeof(last));
	SC_UNDRAW(wn);

#ifdef X11
	if (changed) {
		if (grey_pixmap != 0)
			XFreePixmap(wn__Dpy, grey_pixmap);
		grey_pixmap = XCreatePixmap(wn__Dpy, w->w_win,
				16, 16, 
				(unsigned)DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy)));
		_wn_send_bm(bm, 0, 0, 16, 16, grey_pixmap, _wn_Gc, 0, 0, R_RPL,
								WN_FG, WN_BG);
	}
	XSetTile(wn__Dpy, _wn_Gc, grey_pixmap);
	XSetState(wn__Dpy, _wn_Gc, (xpixel_t)_wn_Fg_pixel, (xpixel_t)_wn_Bg_pixel,
				      _wn_Roptab[ropfunc], (xplanemask_t)AllPlanes);
	XSetFillStyle(wn__Dpy, _wn_Gc, FillTiled);
	XFillRectangle(wn__Dpy, w->w_win, _wn_Gc, x, y,
						(xdim_t)width, (xdim_t)height);
	XSetFillStyle(wn__Dpy, _wn_Gc, FillSolid);
	X_UPDATE(w);
#endif /* X11 */

#ifdef SUNVIEW
	if (changed) {
		if (grey_pr != NULL)
			pr_destroy(grey_pr);
		grey_pr = mem_point(16, 16, 1, last);
	}
	w->w_bw->bw_can_refresh = FALSE;
	pw_replrop(w->w_pw, x, y, width, height, _wn_Roptab[ropfunc], grey_pr, 0, 0);
	w->w_bw->bw_can_refresh = TRUE;
#endif /* SUNVIEW */

	SC_REDRAW(wn);
	RESTORE_FORMAT(bm, format);
}

void
wn_shade_area(wn, x, y, width, height, orig_shade, ropfunc)
int wn;
int x, y, width, height;
int orig_shade;
int ropfunc;
{
	bitmap_t *bm;
	unsigned short data[16], *sptr;
	int i, j, shade; 
	register unsigned short pat;

	sptr = data;
	for (i = 0; i < 4; i++) {
		shade = orig_shade;
		for (j = 0; j < 4; j++) {
			pat = shade & 0xf;
			pat |= pat << 4;
			pat |= pat << 8;
			*sptr++ = pat;
			shade >>= 4;
		}
	}
	bm = wn_make_bitmap_from_data(16, 16, 1, data,
						BM_BIT0_RIGHT, BM_XY_PIXELS, 2);
	wn_tile_area(wn, x, y, width, height, bm, ropfunc);
	wn_free_bitmap(bm);
}

void
wn_mono_rop(wn, x, y, width, height, new_x, new_y, ropfunc)
int wn;
int x, y, width, height, new_x, new_y, ropfunc;
{
	register swin_t *w = WN_TO_W(wn);
#ifdef X11
	XEvent xevent;
#endif /* X11 */

	W_CHECK(wn);
	ADJ_COORDS(w, x, y);
	ADJ_COORDS(w, new_x, new_y);
	SC_UNDRAW(wn);
#ifdef X11
	XSetState(wn__Dpy, _wn_Gc,
			   (xpixel_t)_wn_Fg_pixel, (xpixel_t)_wn_Bg_pixel,
			   _wn_Roptab[ropfunc], (xplanemask_t)_wn_Planemask);
	XSetGraphicsExposures(wn__Dpy, _wn_Gc, x != new_x || y != new_y);
	XCopyArea(wn__Dpy, w->w_win, w->w_win, _wn_Gc, x, y,
				(xdim_t)width, (xdim_t)height, new_x, new_y);
	if (x != new_x || y != new_y) {
		XIfEvent(wn__Dpy, &xevent, get_gexpose_event, (char *)w->w_win);
		w->w_bw->bw_last_rop_was_damaged = xevent.type == GraphicsExpose;
	}
	else
		w->w_bw->bw_last_rop_was_damaged = FALSE;
	X_UPDATE(w);
#endif /* X11 */
#ifdef SUNVIEW
	w->w_bw->bw_can_refresh = FALSE;

	if (w->w_pw->pw_pixrect->pr_depth != 1) {
		pw_putattributes(w->w_pw, &_wn_Planemask);
		pr_putattributes(w->w_pw->pw_prretained, &_wn_Planemask);
	}
	pw_copy(w->w_pw, new_x, new_y, width, height,
					_wn_Roptab[ropfunc], w->w_pw, x, y);
	if (w->w_pw->pw_pixrect->pr_depth != 1) {
		pw_putattributes(w->w_pw, &w->w_bw->bw_planes);
		pr_putattributes(w->w_pw->pw_prretained, &w->w_bw->bw_planes);
	}

	w->w_bw->bw_can_refresh = TRUE;
#endif /* SUNVIEW */
	SC_REDRAW(wn);
}

#ifdef X11
/* BUG: no check on cwin
 */
/* ARSUSED */
static Bool
get_gexpose_event(unused_display, xev, cwin)
Display *unused_display;
XEvent *xev;
char *cwin;
{
	if (xev->type == GraphicsExpose || xev->type == NoExpose) {
		return True;
	}
	else {
		return False;
	}
}
#endif /* X11 */

int
wn_last_rop_was_damaged(wn)
int wn;
{
	W_CHECK(wn);
#ifdef X11
	return WN_TO_W(wn)->w_bw->bw_last_rop_was_damaged;
#else
	return FALSE;
#endif /* !X11 */
}

/*   Copy area (x,y,width,height) of the window to (new_x,new_y). Use
 *   ROP function R_RPL
 */
void
wn_rop(wn,x,y,width,height,new_x,new_y)
int wn;
int x,y,width,height,new_x,new_y;
{
	register swin_t *w = WN_TO_W(wn);
#ifdef X11
	XEvent xevent;
#endif /* X11 */

	W_CHECK(wn);
	if (width <= 0 || height <= 0)
		return;
	ADJ_COORDS(w, x, y);
	ADJ_COORDS(w, new_x, new_y);
	SC_UNDRAW(wn);
#ifdef X11
	XSetState(wn__Dpy, _wn_Gc,
			   (xpixel_t)_wn_Fg_pixel, (xpixel_t)_wn_Bg_pixel,
			   _wn_Roptab[R_RPL], (xplanemask_t)AllPlanes);
	if (x != new_x || y != new_y)
		XSetGraphicsExposures(wn__Dpy, _wn_Gc, TRUE);
	XCopyArea(wn__Dpy, w->w_win, w->w_win, _wn_Gc, x, y,
				(xdim_t)width, (xdim_t)height, new_x, new_y);
	if (x != new_x || y != new_y) {
		XIfEvent(wn__Dpy, &xevent, get_gexpose_event, (char *)w->w_win);
		w->w_bw->bw_last_rop_was_damaged = xevent.type == GraphicsExpose;
	}
	else
		w->w_bw->bw_last_rop_was_damaged = FALSE;
	X_UPDATE(w);
#endif /* X11 */
#ifdef SUNVIEW
	w->w_bw->bw_can_refresh = FALSE;
	pw_copy(w->w_pw, new_x, new_y, width,height,
						_wn_Roptab[R_RPL], w->w_pw, x, y);
	w->w_bw->bw_can_refresh = TRUE;
#endif /* SUNVIEW */
	SC_REDRAW(wn);
}

#define ABS(x) ((x) > 0 ? (x) : -(x))

void
wn_invert_line(wn, x1, y1, x2, y2)
int wn;
int x1, y1, x2, y2;
{
	register swin_t *w = WN_TO_W(wn);

	W_CHECK(wn);
	SC_UNDRAW(wn);
	ADJ_COORDS(w, x1, y1);
	ADJ_COORDS(w, x2, y2);
#ifdef X11
	XSetState(wn__Dpy, _wn_Gc, (xpixel_t)_wn_Fg_pixel, (xpixel_t)_wn_Bg_pixel,
				_wn_Roptab[R_XOR], (xplanemask_t)_wn_Planemask);
	XDrawLine(wn__Dpy, w->w_win, _wn_Gc, x1, y1, x2, y2);
#endif /* X11 */
#ifdef X11
	X_UPDATE(w);
#endif /* X11 */
#ifdef SUNVIEW
	w->w_bw->bw_can_refresh = FALSE;

	if (w->w_pw->pw_pixrect->pr_depth != 1)
		pw_putattributes(w->w_pw, &_wn_Planemask);
	pw_vector(w->w_pw, x1, y1, x2, y2, PIX_SRC ^ PIX_DST, 1);
	if (w->w_pw->pw_pixrect->pr_depth != 1)
		pw_putattributes(w->w_pw, &w->w_bw->bw_planes);

	w->w_bw->bw_can_refresh = TRUE;
#endif /* SUNVIEW */
	SC_REDRAW(wn);
}

void
wn_draw_line(wn,x1,y1,x2,y2,colour)
int wn;
int x1,y1,x2,y2,colour;
{
	register swin_t *w = WN_TO_W(wn);

	if (colour == WN_INVERT) {
		wn_invert_line(wn, x1, y1, x2, y2);
		return;
	}

	W_CHECK(wn);
	ADJ_COORDS(w, x1, y1);
	ADJ_COORDS(w, x2, y2);
	SC_UNDRAW(wn);
#ifdef X11
#ifdef X11
	XSetState(wn__Dpy, _wn_Gc, (xpixel_t)colour, (xpixel_t)_wn_Bg_pixel,
					_wn_Roptab[R_RPL], (xplanemask_t)AllPlanes);
	XDrawLine(wn__Dpy, w->w_win, _wn_Gc, x1, y1, x2, y2);
#endif /* X11 */
	X_UPDATE(w);
#endif /* X11 */
#ifdef SUNVIEW
	w->w_bw->bw_can_refresh = FALSE;
	pw_vector(w->w_pw, x1, y1, x2, y2, PIX_SRC, colour);
	w->w_bw->bw_can_refresh = TRUE;
#endif /* SUNVIEW */
	SC_REDRAW(wn);
}
	
/*   Move an area (x,y,width,height) of the window to (new_x,new_y) and set
 *   the vacated area to colour. Useful for scrolling
 */
void
wn_move_area(wn, x, y, width, height, new_x, new_y, colour)
int wn;
int x, y, width, height, new_x, new_y, colour;
{
	int dist, new;
	
	SC_UNDRAW(wn);
	wn_rop(wn, x, y, width, height, new_x, new_y);
	if (x != new_x) {
		dist  = x - new_x;
		new = (dist < 0) ? x : x + width - dist;
		wn_set_area(wn, new, y, ((dist < 0) ? -dist : dist), height, colour);
	}
	if (y != new_y) {
		dist  = y - new_y;
		new = (dist < 0) ? y : y + height - dist;
		wn_set_area(wn, x, new, width, ((dist<0) ? -dist : dist), colour);
	}
	SC_REDRAW(wn);
}

void
wn_set_area(wn,x,y,width,height,colour)
int wn;
int x,y,width,height,colour;
{
	swin_t *w;

	if (colour == WN_INVERT) {
		wn_invert_area(wn, x, y, width, height);
		return;
	}
	W_CHECK(wn);
	w = WN_TO_W(wn);
	ADJ_COORDS(w, x, y);
	SC_UNDRAW(wn);
#ifdef X11
	XSetState(wn__Dpy, _wn_Gc, (xpixel_t)colour, (xpixel_t)_wn_Bg_pixel,
					_wn_Roptab[R_RPL], (xplanemask_t)AllPlanes);
	XFillRectangle(wn__Dpy, w->w_win, _wn_Gc, x, y,
						(xdim_t)width, (xdim_t)height);
	X_UPDATE(w);
#endif /* X11 */
#ifdef SUNVIEW
	pw_rop(w->w_pw, x, y, width,height, PIX_SRC | PIX_COLOR(colour),
						(struct pixrect *)NULL, 0, 0);
#endif /* SUNVIEW */
	SC_REDRAW(wn);
}

/*   draw rectangle with origin x-1,y-1, width width+2, height height+2
 */
void
wn_box_round(wn,x,y,width,height,colour)
int wn;
int x,y,width,height,colour;
{
	SC_UNDRAW(wn);
	wn_draw_line(wn,	x-1,	 y-1,	   x+width-1, y-1,        colour);
	wn_draw_line(wn,	x+width, y-1,	   x+width,   y+height-1, colour);
	wn_draw_line(wn,	x+width, y+height, x,  	      y+height,   colour);
	wn_draw_line(wn,	x-1,	 y+height, x-1,       y,          colour);
	SC_REDRAW(wn);
}

/*  Like wn_box_round(), but draw inverting lines - see wn_invert_line().
 */
void
wn_invert_box(wn, x, y, width, height)
int wn;
int x, y, width, height;
{
	wn_invert_line(wn,	x-1,	 y-1,	   x+width-1, y-1	);
	wn_invert_line(wn,	x+width, y-1,	   x+width,   y+height-1);
	wn_invert_line(wn,	x+width, y+height, x,  	      y+height	);
	wn_invert_line(wn,	x-1,	 y+height, x-1,       y		);
}
