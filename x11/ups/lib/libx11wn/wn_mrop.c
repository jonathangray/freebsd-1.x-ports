/* wn_mrop.c - public functions for rasterops to and from memory */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_mrop_c_sccsid[] = "@(#)wn_mrop.c	1.12 25/4/92 (UKC)";

#include <sys/types.h>
#include <stdlib.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_misc.h"
#include "wn_bm.h"
#include "wn_xrop.h"

/*  Macro used for clipping - ensure that v is non negative, and adjust
 *  n and v2 if v is changed.
 */
#define MIN_ZERO2(v,n,v2) { if (v < 0) { n += v; v2 -= v; v = 0; } }

/*  Clip a width or height (v) to the width or height (lim) of a bitmap.
 */
#define CLIP_DIM(v,d,lim) { if (d + v > lim) { v = lim - d; } }

/*  Rasterop region (sx,sy,width,height) of bitmap sbm to
 *  (dx,dy) in dbm, using rasterop function ropfunc.
 *
 *  The region is clipped to fit both bitmaps.
 */
void
wn_copy_image(sbm,sx,sy,width,height,dbm,dx,dy,ropfunc)
bitmap_t *sbm;
int sx, sy, width, height;
bitmap_t *dbm;
int dx, dy, ropfunc;
{
#ifdef SUNVIEW
	struct mpr_data mdbuf;
	struct pixrect *dpr, *pr, sprbuf;
	struct pixrect *_wn_bm_to_pixrect();
#endif /* SUNVIEW */
	format_t sformat, dformat;

	if (sbm->bm_nplanes != dbm->bm_nplanes)
		wn__panic("wn_copy_image: src and dst bm have different #planes");

	/*  Clip to the source and destination.
	 */
	MIN_ZERO2(sx, width,  dx);
	MIN_ZERO2(sy, height, dy);
	MIN_ZERO2(dx, width,  sx);
	MIN_ZERO2(dy, height, sy);
	CLIP_DIM(width,  sx, sbm->bm_width);
	CLIP_DIM(height, sy, sbm->bm_height);
	CLIP_DIM(width,  dx, dbm->bm_width);
	CLIP_DIM(height, dy, dbm->bm_height);
	if (width <= 0 || height <= 0)
		return;

	SAVE_FORMAT(sbm, sformat, XROP_OK);
	if (sbm != dbm)
		SAVE_FORMAT(dbm, dformat, XROP_OK);

	if (sbm->bm_pixel_format != dbm->bm_pixel_format)
		wn__panic("bitmaps must have same pixel format in wn_copy_image");
#ifdef X11
	if (sbm->bm_pixel_format == BM_XY_PIXELS)
		_wn_xy_rasterop(sbm,sx,sy,width,height,dbm,dx,dy,ropfunc);
	else
		_wn_z_rasterop(sbm,sx,sy,width,height,dbm,dx,dy,ropfunc);
#endif /* X11 */

#ifdef SUNVIEW
	/*  _wn_bm_to_pixrect returns a pointer to a static buffer, so we must
	 *  copy both returned structures as we want to use two pixrects at
	 *  once.
	 */
	pr = _wn_bm_to_pixrect(sbm);
	mdbuf = *(struct mpr_data *)pr->pr_data;
	sprbuf = *pr;
	sprbuf.pr_data = (caddr_t) &mdbuf;
	dpr = (sbm == dbm) ? &sprbuf : _wn_bm_to_pixrect(dbm);

	pr_rop(dpr,dx,dy,width,height,_wn_Roptab[ropfunc],&sprbuf,sx,sy);
#endif /* SUNVIEW */

	/*  Restore the original format.
	 */
	RESTORE_FORMAT(sbm, sformat);
	if (sbm != dbm)
		RESTORE_FORMAT(dbm, dformat);
}


#ifdef X11
#define N_PLANES(w)	DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy))
#endif /* X11 */
#ifdef SUNVIEW
#define N_PLANES(w)	((w)->w_pw->pw_pixrect->pr_depth)
#endif /* SUNVIEW */

/*  Rasterop the region (sx,sy,width,height) from the bitmap sbm
 *  to dx,dy in dbm.
 */
void
wn_put_image(bm,sx,sy,width,height,wn,dx,dy,ropfunc,fg,bg)
bitmap_t *bm;
int sx, sy, width, height, wn, dx, dy, ropfunc;
int fg, bg;
{
	register swin_t *w = WN_TO_W(wn);
	format_t format;

	W_CHECK(wn);
	ADJ_COORDS(w, dx, dy);
	if (bm->bm_nplanes != 1 && bm->bm_nplanes != N_PLANES(w))
		wn__panic("bitmap nplanes not 1 or dpy planes");
	MIN_ZERO2(sx, width,  dx);
	MIN_ZERO2(sy, height, dy);
	CLIP_DIM(width,  sx, bm->bm_width);
	CLIP_DIM(height, sy, bm->bm_height);
	if (width <= 0 || height <= 0)
		return;
	SAVE_FORMAT(bm, format, DISP_OK);

#ifdef X11
	_wn_send_bm(bm, sx, sy, width, height, w->w_win, _wn_Gc, dx, dy,
								ropfunc, fg, bg);
	X_UPDATE(w);
#endif /* X11 */

#ifdef SUNVIEW
	SC_UNDRAW(wn);
	w->w_bw->bw_can_refresh = FALSE;
	if (bg != WN_BG && bm->bm_nplanes == 1) {
		pw_writebackground(w->w_pw, dx, dy, width, height,
					 _wn_Roptab[ropfunc] | PIX_COLOR(bg));
		if (ropfunc == R_RPL) {
			pw_stencil(w->w_pw, dx, dy, width, height,
				   _wn_Roptab[ropfunc] | PIX_COLOR(fg),
				   _wn_bm_to_pixrect(bm),
				   sx, sy,
				   (struct pixrect *)NULL, 0, 0);
		}
		else {
			pw_write(w->w_pw,
				 dx, dy, width, height,
				 (PIX_SRC ^ PIX_DST) | PIX_COLOR(fg ^ bg),
				 _wn_bm_to_pixrect(bm),
				 sx, sy);
		}
	}
	else {
		pw_write(w->w_pw,
			 dx, dy, width, height,
			 _wn_Roptab[ropfunc] | PIX_COLOR(fg),
			 _wn_bm_to_pixrect(bm),
			 sx, sy);
	}
	w->w_bw->bw_can_refresh = TRUE;
	SC_REDRAW(wn);
#endif /* SUNVIEW */

	RESTORE_FORMAT(bm, format);
}

/*  Copy a region (sx,sy,width,height) from window wn to (dx,dy)
 *  in bitmap bm.
 */
void
wn_get_image(wn,sx,sy,width,height,bm,dx,dy,ropfunc)
int wn, sx, sy, width, height;
bitmap_t *bm;
int dx, dy, ropfunc;
{
	register swin_t *w = WN_TO_W(wn);
	format_t format;
#ifdef X11
	bitmap_t *tbm;
	XImage *xi;
#endif /* X11 */

	W_CHECK(wn);
	ADJ_COORDS(w, sx, sy);
	if (bm->bm_nplanes != N_PLANES(w))
		wn__panic("wn_get_image: bitmap nplanes");
	MIN_ZERO2(dx, width,  sx);
	MIN_ZERO2(dy, height, sy);
	CLIP_DIM(width,  dx, bm->bm_width);
	CLIP_DIM(height, dy, bm->bm_height);
	if (width <= 0 || height <= 0)
		return;

	SAVE_FORMAT(bm, format, DISP_OK);

#ifdef X11
	xi = XGetImage(wn__Dpy, w->w_win,
		       sx, sy, (unsigned)width, (unsigned)height,
		       (unsigned)AllPlanes,
		       (bm->bm_pixel_format == BM_XY_PIXELS) ? XYPixmap : ZPixmap);
	tbm = wn_make_bitmap_from_data(width, height,
			     DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy)),
			     (unsigned short *)xi->data,
			     (xi->bitmap_bit_order == LSBFirst) ? BM_BIT0_LEFT
								: BM_BIT0_RIGHT,
			     bm->bm_pixel_format,
			     xi->bytes_per_line);
	tbm->bm_byte_order = (xi->byte_order == LSBFirst) ? BM_LSB_FIRST
							  : BM_MSB_FIRST;
	wn_copy_image(tbm, 0, 0, width, height, bm, dx, dy, ropfunc);
	wn_free_bitmap(tbm);
	XDestroyImage(xi);
#endif /* X11 */

#ifdef SUNVIEW
	pw_read(_wn_bm_to_pixrect(bm),dx,dy,width,height,_wn_Roptab[ropfunc],
								w->w_pw,sx,sy);
#endif /* !SUNVIEW */

	RESTORE_FORMAT(bm, format);
}

/*  Structure describing a saved region.
 */
typedef struct sast {
	int sa_wn;
	int sa_x, sa_y, sa_width, sa_height;
#ifdef X11
	Pixmap sa_pixmap;
#else
#ifdef SUNVIEW
	struct pixrect *sa_pr;
#else
	bitmap_t *sa_bm;
#endif /* !SUNVIEW */
#endif /* !X11*/
} sa_t;

/*  Save the area x,y,width,height of the screen to a private off screen area.
 *  Return a handle for this saved area.  The only thing that can be done with
 *  the saved handle is to restore it to the screen with wn_restore_area().
 */
long
wn_save_area(wn,x,y,width,height)
int wn;
int x,y,width,height;
{
	sa_t *sa;
	register swin_t *w;

	/*  Do everything in the base window.
	 */
	W_CHECK(wn);
	w = WN_TO_W(wn);
	ADJ_COORDS(w, x, y);
	w = w->w_base;
	wn = w->w_wn;

	sa = (sa_t *) wn__e_malloc(sizeof(sa_t));
	sa->sa_wn = wn;
#ifdef X11
	if (x < 0) {
		width += x;
		x = 0;
	}
	if (y < 0) {
		height += y;
		y = 0;
	}
#endif /* X11 */
	sa->sa_x = x;
	sa->sa_y = y;
	sa->sa_width = width;
	sa->sa_height = height;
	if (width < 0 || height < 0) {
#ifdef X11
		sa->sa_pixmap = None;
#else
#ifdef SUNVIEW
		sa->sa_pr = NULL;
#else
		sa->sa_bm = NULL;
#endif /* !SUNVIEW */
#endif /* !X11*/
		return (long)sa;
	}
#ifdef X11
	sa->sa_pixmap = XCreatePixmap(wn__Dpy, w->w_win,
				      (unsigned)width, (unsigned)height,
				      (unsigned)DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy)));
	if (sa->sa_pixmap == None)
		wn__panic("XCreatePixmap failed");
	
	XSetState(wn__Dpy, _wn_Gc, (xpixel_t)_wn_Fg_pixel, (xpixel_t)_wn_Bg_pixel,
				      GXcopy, (xplanemask_t)AllPlanes);
	XSetGraphicsExposures(wn__Dpy, _wn_Gc, FALSE);
	XCopyArea(wn__Dpy, w->w_win, sa->sa_pixmap, _wn_Gc, x, y,
					(xdim_t)width, (xdim_t)height, 0, 0);
#else
#ifdef SUNVIEW
	sa->sa_pr = mem_create(width, height,
					WN_TO_W(wn)->w_pw->pw_prretained->pr_depth);
	pr_rop(sa->sa_pr, 0,  0, width, height, PIX_SRC,
					WN_TO_W(wn)->w_pw->pw_prretained, x, y);
#else
	sa->sa_bm = wn_make_bitmap(width,height,wn_get_nplanes(),
				BM_NATURAL_BIT_ORDER, BM_NATURAL_PIXEL_FORMAT);
	wn_get_image(wn,x,y,width,height,sa->sa_bm,0,0,R_RPL);
#endif /* !SUNVIEW */
#endif /* !X11 */
	return (long)sa;
}

/*  Restore screen area saved in sa and free the resources of the sa.
 */
void
wn_restore_area(lsa)
long lsa;
{
#ifdef X11
	register sa_t *sa;
	register swin_t *w;

	sa = (sa_t *) lsa;
	w = WN_TO_W(sa->sa_wn);
	W_CHECK(sa->sa_wn);
	if (sa->sa_pixmap != None) {
		XSetState(wn__Dpy, _wn_Gc, (xpixel_t)_wn_Fg_pixel,
				 	   (xpixel_t)_wn_Bg_pixel,
					   GXcopy, (xplanemask_t)AllPlanes);
		XSetGraphicsExposures(wn__Dpy, _wn_Gc, FALSE);
		XCopyArea(wn__Dpy, sa->sa_pixmap, w->w_win, _wn_Gc, 0, 0,
					(xdim_t)sa->sa_width, (xdim_t)sa->sa_height,
					sa->sa_x, sa->sa_y);
		X_UPDATE(w);
	}
#else
	register sa_t *sa;
#ifdef SUNVIEW
	struct pixwin *pw;
#endif /* SUNVIEW */

	sa = (sa_t *) lsa;
#ifdef SUNVIEW
	if (sa->sa_pr != NULL)
		pw_write(WN_TO_W(sa->sa_wn)->w_pw,
				sa->sa_x,sa->sa_y,sa->sa_width,sa->sa_height,
								PIX_SRC,
								sa->sa_pr,0,0);
#else
	if (sa->sa_bm != NULL)
		wn_put_image(sa->sa_bm,
			     0, 0, sa->sa_width, sa->sa_height,
			     sa->sa_wn,
			     sa->sa_x, sa->sa_y,
			     R_RPL, WN_FG, WN_BG);
#endif /* !SUNVIEW */
#endif /* !X11 */
}

void
wn_free_area(lsa)
long lsa;
{
	sa_t *sa;

	sa = (sa_t *) lsa;
#ifdef X11
	if (sa->sa_pixmap != None)
		XFreePixmap(wn__Dpy, sa->sa_pixmap);
#else
#ifdef SUNVIEW
	if (sa->sa_pr != NULL)
		pr_destroy(sa->sa_pr);
#else
	if (sa->sa_bm != NULL)
		wn_free_bitmap(sa->sa_bm);
#endif /* !SUNVIEW */
#endif /* !X11 */
	free((char *)sa);
}
