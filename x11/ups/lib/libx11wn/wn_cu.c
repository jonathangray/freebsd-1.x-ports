/* wn_cu.c - setting of ordinary cursors for windows */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_cu_c_sccsid[] = "@(#)wn_cu.c	1.13 25/4/92 (UKC)";

#include <sys/types.h>
#include <string.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"

#include "wn_xrop.h"
#include "wn_misc.h"
#include "wn_bm.h"
#include "wn_cu.h"

#ifdef X11
#define NULL_CID	0

static Pixmap bm_to_pixmap PROTO((bitmap_t *bm));
static void toggle_shorts PROTO((unsigned short *s, int count));
#endif /* X11 */


#ifdef X11
static XColor Cu_fg_color, Cu_bg_color;

void
wn__set_x11_cursor_colors(fg_color, bg_color)
XColor *fg_color, *bg_color;
{
	if (fg_color != NULL)
		Cu_fg_color = *fg_color;
	if (bg_color != NULL)
		Cu_bg_color = *bg_color;
}

static Pixmap
bm_to_pixmap(bm)
bitmap_t *bm;
{
	format_t fm;
	Pixmap pixmap;
	Window rootwin;

	if (bm == NULL)
		return None;

	if (bm->bm_flags & BM_CHOOSE_FORMAT)
		fm.fm_lineinc = 0;
	else {
		fm.fm_bit_order = bm->bm_bit_order;
		fm.fm_byte_order = bm->bm_byte_order;
		fm.fm_pixel_format = bm->bm_pixel_format;
		fm.fm_lineinc = bm->bm_lineinc;
	}

	wn_change_bitmap_format(bm, BM_BIT0_LEFT, BM_LSB_FIRST,
					    BM_XY_PIXELS,
					    (bm->bm_width + 7) / 8);
	rootwin = RootWindow(wn__Dpy, DefaultScreen(wn__Dpy));
	pixmap = XCreateBitmapFromData(wn__Dpy, rootwin,
						(char *)bm->bm_data,
						(unsigned)bm->bm_width,
						(unsigned)bm->bm_height);
	
	if (pixmap == 0)
		wn__panic("Can't make pixmap for cursor");
	
	if (fm.fm_lineinc != 0)
		wn_change_bitmap_format(bm, fm.fm_bit_order, fm.fm_byte_order,
						fm.fm_pixel_format, fm.fm_lineinc);
	
	return pixmap;
}

static void
toggle_shorts(s, count)
unsigned short *s;
int count;
{
	unsigned short *lim;

	for (lim = s + count; s < lim; ++s)
		*s = ~*s;
}
#endif /* X11 */

/*  Create a cursor and return a handle on it.
 */
cursor_t
wn_create_cursor(cbm, mask_bm)
bitmap_t *cbm, *mask_bm;
{
#ifdef SUNVIEW
	struct cursor *sun_cursor;
	struct pixrect *spr, *dpr;
	int width, height;
	struct pixrect *_wn_bm_to_pixrect();
	format_t c_format;

	sun_cursor = (struct cursor *) wn__e_malloc(sizeof(struct cursor));
	bzero((char *)sun_cursor, sizeof(struct cursor));

	width = (cbm->bm_width > 16) ? 16 : cbm->bm_width;
	height = (cbm->bm_height > 16) ? 16 : cbm->bm_height;
	SAVE_FORMAT(cbm, c_format, DISP_OK);
	spr = _wn_bm_to_pixrect(cbm);
	dpr = mem_create(width, height, 1);
	pr_rop(dpr, 0, 0, width, height, PIX_SRC, spr, 0, 0);
	RESTORE_FORMAT(cbm, c_format);

	sun_cursor->cur_shape = dpr;
	sun_cursor->cur_xhot = cbm->bm_xhot;
	sun_cursor->cur_yhot = cbm->bm_yhot;
	sun_cursor->cur_function = PIX_SRC ^ PIX_DST;
	return (cursor_t)sun_cursor;
#endif /* SUNVIEW */
#ifdef X11
	Pixmap cursor_pixmap, mask_pixmap;
	int width, height;
	Cursor cid;
	static bool first_call = TRUE, invert_cursors;

	width = cbm->bm_width;
	height = cbm->bm_height;

	/*  The DECwindows server on the color DS3100 (at least) is broken -
	 *  it displays inverted cursors.  We try to work around this.
	 */
	if (first_call) {
		static char decstring[] = "DECWINDOWS DigitalEquipmentCorp.";

		invert_cursors =
			ServerVendor(wn__Dpy) != NULL &&
			DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy)) == 8 &&
			strcmp(ServerVendor(wn__Dpy), decstring) == 0 &&
			VendorRelease(wn__Dpy) == 3;

		first_call = FALSE;
	}

	if (invert_cursors) {
		toggle_shorts(cbm->bm_data, (cbm->bm_lineinc / 2) * cbm->bm_height);
		cursor_pixmap = bm_to_pixmap(cbm);
		toggle_shorts(cbm->bm_data, (cbm->bm_lineinc / 2) * cbm->bm_height);

		/*  We can't just use cursor_pixmap for the mask when mask_bm
		 *  is NULL because cursor_pixmap is inverted.
		 */
		mask_pixmap = bm_to_pixmap((mask_bm != NULL) ? mask_bm : cbm);
	}
	else {
		cursor_pixmap = bm_to_pixmap(cbm);
		mask_pixmap = (mask_bm != NULL) ? bm_to_pixmap(mask_bm)
						: cursor_pixmap;
	}

	cid = XCreatePixmapCursor(wn__Dpy, cursor_pixmap, mask_pixmap,
					   &Cu_fg_color, &Cu_bg_color,
					   (unsigned)cbm->bm_xhot,
					   (unsigned)cbm->bm_yhot);

	XFreePixmap(wn__Dpy, cursor_pixmap);
	if (mask_pixmap != cursor_pixmap)
		XFreePixmap(wn__Dpy, mask_pixmap);

	if (cid == 0)
		wn__panic("XCreateCursor failed in wn_set_cursor");

	return (cursor_t)cid;
#endif /* X11 */
}

void
wn_free_cursor(cid)
cursor_t cid;
{
#ifdef SUNVIEW
	pr_destroy(((struct cursor *)cid)->cur_shape);
	free((char *)(struct cursor *)cid);
#endif /* SUNVIEW */
#ifdef X11
	XFreeCursor(wn__Dpy, (Cursor)cid);
#endif /* X11 */
}

/*  Make the cursor cid the cursor image for window wn.
 */
void
wn_define_cursor(wn, cid)
int wn;
cursor_t cid;
{
	swin_t *w;
#ifdef SUNVIEW
	struct cursor *cur;
	int save_func;
#endif /* SUNVIEW */
	
	W_CHECK(wn);
	w = WN_TO_W(wn);
#ifdef SUNVIEW
	cur = (struct cursor *) cid;
	if (w->w_pw->pw_pixrect->pr_depth == 8) {
		cur->cur_function = PIX_SRC | PIX_DST;
		win_setcursor(w->w_pw->pw_windowfd, cur);
		cur->cur_function = PIX_SRC ^ PIX_DST;
	}
	else
		win_setcursor(w->w_pw->pw_windowfd, cur);
#endif /* SUNVIEW */
#ifdef X11
	XDefineCursor(wn__Dpy, w->w_win, (unsigned)cid);
	wn__do_xflush();
#endif /* X11 */
	w->w_bw->bw_cid = cid;
}

cursor_t
wn_get_window_cursor(wn)
int wn;
{
	W_CHECK(wn);
	return WN_TO_W(wn)->w_bw->bw_cid;
}

/*  Set the cursor for window wn from bitmap cbm.
 *  This function is obsolete, and is present only for backwards compatibility.
 */
bitmap_t *
wn_set_cursor(wn, cbm)
int wn;
bitmap_t *cbm;
{
	bitmap_t *oldcbm;
	cursor_t cid;
	swin_t *w;

	W_CHECK(wn);
	w = WN_TO_W(wn);
	oldcbm = w->w_bw->bw_cursor;
	w->w_bw->bw_cursor = cbm;
	cid = wn_create_cursor(cbm, (bitmap_t *)NULL);
	wn_define_cursor(wn, cid);
	wn_free_cursor(cid);
	return oldcbm;
}

/*  Return the cursor for window wn.
 *  This function is obsolete, and is present only for backwards compatibility.
 */
bitmap_t *
wn_get_cursor(wn)
int wn;
{
	W_CHECK(wn);
	return WN_TO_W(wn)->w_bw->bw_cursor;
}

void
wn_combine_cursors(dcbm, scbm, ropfunc)
bitmap_t *dcbm, *scbm;
int ropfunc;
{
	wn_rop_mem_to_mem(scbm, 0, 0, scbm->bm_width, scbm->bm_height,
							dcbm, 0, 0, ropfunc);
	if (ropfunc == R_RPL || ropfunc == R_NOT) {
		dcbm->bm_xhot = scbm->bm_xhot;
		dcbm->bm_yhot = scbm->bm_yhot;
	}
}

void
wn_print_cursor(wn,cbm,x,y,ropfunc)
int wn;
bitmap_t *cbm;
int x,y,ropfunc;
{
	wn_rop_from_mem(cbm, 0, 0, cbm->bm_width, cbm->bm_height,
				wn, x - cbm->bm_xhot, y - cbm->bm_yhot, ropfunc);
}
