/* wn_xrop.c - private X rasterop functions */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_xrop_c_sccsid[] = "@(#)wn_xrop.c	1.12 25/4/92 (UKC)";

#include <stdio.h>
#include <sys/types.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_misc.h"
#include "wn_xrop.h"

#ifdef X11
/*  Macro to rasterop bits in s to bits in d by rop function ropfunc.
 *  Used in x_rasterop below.
 *  Note that s and d are each referenced only once - we rely on
 *  this in z_rasterop where we use post increment on both.
 */
#define DO_ROP(s,d,func) \
	switch(func) { \
		case R_RPL:	d  =  s;	break; \
		case R_NOT:	d  = ~s;	break; \
		case R_AND:	d &=  s;	break; \
		case R_ANDNOT:	d &= ~s;	break; \
		case R_OR:	d |=  s;	break; \
		case R_ORNOT:	d |= ~s;	break; \
		case R_XOR:	d ^=  s;	break; \
		case R_XNOR:	d ^= ~s;	break; \
	}

void
_wn_z_rasterop(sbm,src_x,src_y,width,height,dbm,dst_x,dst_y,ropfunc)
bitmap_t *sbm;
int src_x, src_y, width, height;
bitmap_t *dbm;
int dst_x, dst_y, ropfunc;
{
	unsigned char *sdata, *ddata;
	register unsigned char *src, *dst, *lim;

	if (sbm->bm_nplanes != 8 || dbm->bm_nplanes != 8)
		wn__panic("nplanes must be 8 for src and dst bitmaps for rop");
	sdata = (unsigned char *)sbm->bm_data + src_y * sbm->bm_lineinc + src_x;
	ddata = (unsigned char *)dbm->bm_data + dst_y * dbm->bm_lineinc + dst_x;
	while (--height >= 0) {
		src = sdata;
		dst = ddata;
		lim = src + width;
		switch(ropfunc) {
		case R_RPL:
			while (src < lim)
				*dst++ = *src++;
			break;
		case R_NOT:
			while (src < lim)
				*dst++ = ~*src++;
			break;
		case R_AND:
			while (src < lim)
				*dst++ &= *src++;
			break;
		case R_ANDNOT:
			while (src < lim)
				*dst++ &= ~*src++;
			break;
		case R_OR:
			while (src < lim)
				*dst++ |= *src++;
			break;
		case R_ORNOT:
			while (src < lim)
				*dst++ |= ~*src++;
			break;
		case R_XOR:
			while (src < lim)
				*dst++ ^= *src++;
			break;
		case R_XNOR:
			while (src < lim)
				*dst++ ^= ~*src++;
			break;
		}
		sdata += sbm->bm_lineinc;
		ddata += dbm->bm_lineinc;
	}
}

/*  Macro to do the shifting and masking to get bits out of src when the
 *  source is not word aligned.
 *  Used in x_rasterop below.
 */
#define GET_SBITS(src,ls,rs,bit0_right) \
	((ls == 0) ? *src \
		   : (bit0_right ? ((*src << ls) | (src[1] >> rs)) \
				 : ((*src >> ls) | (src[1] << rs))))

/*  Software rasterop for X.
 *
 *  BUG: it doesn't handle overlapping source and destination in all cases
 */
void
_wn_xy_rasterop(sbm,src_x,src_y,width,height,dbm,dst_x,dst_y,ropfunc)
bitmap_t *sbm;
int src_x, src_y, width, height;
bitmap_t *dbm;
int dst_x, dst_y, ropfunc;
{
	static unsigned short bit0_right_masks[17] = {
									0x0000,
		0x0001, 0x0003, 0x0007, 0x000f, 0x001f, 0x003f, 0x007f, 0x00ff,
		0x01ff, 0x03ff, 0x07ff, 0x0fff, 0x1fff, 0x3fff, 0x7fff, 0xffff,
	};
	static unsigned short bit0_left_masks[17] = {
									0x0000,
		0x8000, 0xc000, 0xe000, 0xf000, 0xf800, 0xfc00, 0xfe00, 0xff00,
		0xff80, 0xffc0, 0xffe0, 0xfff0, 0xfff8, 0xfffc, 0xfffe, 0xffff,
	};
#ifdef DEBUG
	static int do_dump = -1;
	int orig_src_x, orig_dst_x, orig_width;
#endif /* DEBUG */
	register unsigned short *src, *dst, *lim;
	register unsigned short sbits, dbits, d_lmask, d_rmask;
	register int bit0_right;
	unsigned short *masks, *sdata_base, *ddata_base;
	unsigned short *sdata, *ddata;
	unsigned short d_lbits, d_rbits, s_lshift, s_rshift;
	int sdata_offset, ddata_offset;
	int plane, h, shorts_per_line;

#ifdef DEBUG
	if (do_dump == -1)
		do_dump = getenv("DUMP") != NULL;
#endif /* DEBUG */

	if (sbm->bm_bit_order != dbm->bm_bit_order)
		wn__panic("bitmaps have different bit orders in _wn_xy_rop");
	bit0_right = sbm->bm_bit_order == BM_BIT0_RIGHT;
	masks = bit0_right ? bit0_right_masks : bit0_left_masks;
#ifdef DEBUG
	if (do_dump)
		printf("sx:%d sy:%d w:%d h:%d dx:%d dy:%d bit0right:%d\n",
				src_x,src_y,width,height,dst_x,dst_y,bit0_right);
#endif /* DEBUG */
	/*  Get the masks for the part words at the left and right of
	 *  the destination.
	 */
	d_lbits = dst_x % 16;
#ifdef DEBUG
	if (do_dump)
		printf("d_lbits:%d", d_lbits);
#endif /* DEBUG */

#ifdef DEBUG
	orig_src_x = src_x;
	orig_dst_x = dst_x;
	orig_width = width;
#endif /* DEBUG */
	/*  Adjust src_x,dst_x and width if necessary to get src_x onto
	 *  a word boundary.
	 */
	if (d_lbits != 0) {
		dst_x -= d_lbits;
		src_x -= d_lbits;
		width += d_lbits;
#ifdef DEBUG
		if (do_dump)
			printf("-> [sx:%d dx:%d w:%d] ",src_x,dst_x,width);
#endif /* DEBUG */
	}

	/*  Now that we have dst_x aligned on a word boundary, calculate
	 *  the number of whole words we need to copy, and the number of
	 *  bits in the right hand part word.
	 */
	if (d_lbits == 0 || width > 16) {
		d_rbits = width % 16;
		d_lmask = masks[16 - d_lbits];
	}
	else {
		unsigned short t;

		d_rbits = 0;
		t = ~masks[16 - (width-d_lbits)];
		d_lmask = bit0_right ? t >> d_lbits : t << d_lbits;
	}
	d_rmask = ~masks[16 - d_rbits];
	shorts_per_line = width / 16 - ((d_lbits != 0) ? 1 : 0);

#ifdef DEBUG
	if (do_dump)
		printf("spl:%d d_rbits:%d d_lmask:0x%x d_rmask:0x%x ",
					shorts_per_line,d_rbits,d_lmask,d_rmask);
#endif /* DEBUG */

	/*  Set up the shifting and masking necessary to get the source
	 *  words out.
	 */
	s_lshift = (src_x + 16) % 16;
	s_rshift = 16 - s_lshift;

	sdata_base = sbm->bm_data;
	ddata_base = dbm->bm_data;
	sdata_offset = src_y * (sbm->bm_lineinc >> 1) + (src_x + 16) / 16 - 1;
	ddata_offset = dst_y * (dbm->bm_lineinc >> 1) + dst_x / 16;

#ifdef DEBUG
	if (do_dump)
		printf("s_lshift:%d s_rshift:%d sdata:%d ddata:%d\n",
				s_lshift,s_rshift,
				sdata_base-sbm->bm_data, ddata_base-dbm->bm_data);
	if (do_dump)
		_wn_dump_bits(stdout, dbm, orig_dst_x, dst_y, orig_width, height, 16);
#endif /* DEBUG */
	for (plane = 0; plane < sbm->bm_nplanes; plane++) {
		sdata = sdata_base + sdata_offset;
		ddata = ddata_base + ddata_offset;
		for (h = 0; h < height; h++) {
			src = sdata;
			dst = ddata;
			if (d_lbits != 0) {
				sbits = GET_SBITS(src,s_lshift,s_rshift,bit0_right);
				src++;
				dbits = *dst;
				DO_ROP(sbits,dbits,ropfunc);
				*dst = (*dst & ~d_lmask) | (dbits & d_lmask);
				dst++;
			}
			lim = dst + shorts_per_line;
			while (dst < lim) {
				sbits = GET_SBITS(src,s_lshift,s_rshift,bit0_right);
				src++;
				dbits = *dst;
				DO_ROP(sbits,dbits,ropfunc);
				*dst++ = dbits;
			}
			if (d_rbits != 0) {
				sbits = GET_SBITS(src,s_lshift,s_rshift,bit0_right);
				dbits = *dst;
				DO_ROP(sbits,dbits,ropfunc);
				*dst = (*dst & ~d_rmask) | (dbits & d_rmask);
			}
			sdata += sbm->bm_lineinc >> 1;
			ddata += dbm->bm_lineinc >> 1;
		}
		sdata_base += (sbm->bm_lineinc >> 1) * sbm->bm_height;
		ddata_base += (dbm->bm_lineinc >> 1) * dbm->bm_height;
	}
#ifdef DEBUG
	if (do_dump)
		_wn_dump2_bits(stdout,sbm,orig_src_x,src_y,dbm,orig_dst_x,dst_y,
							orig_width,height,16);
#endif /* DEBUG */

}

/*  Maximum request size. Conservative estimate.
 */
#define MAX_RQ_SIZE	(64 * 1024)

/*  Send a bitmap to the server for display.  There is a limit to the
 *  size of a single message to the server, so we fragment the bitmap
 *  if necessary.
 */ 
void
_wn_send_bm(bm, sx, sy, width, height, win, gc, dx, dy, ropfunc, fg, bg)
bitmap_t *bm;
int sx, sy, width, height;
Window win;
GC gc;
int dx, dy;
int ropfunc, fg, bg;
{
	XImage xibuf;

	xibuf.width = bm->bm_width;
	xibuf.height = bm->bm_height;
	xibuf.xoffset = 0;
	if (bm->bm_pixel_format == BM_XY_PIXELS && bm->bm_nplanes == 1)
		xibuf.format = XYBitmap;
	else
		xibuf.format = (bm->bm_pixel_format == BM_XY_PIXELS) ? XYPixmap
								     : ZPixmap;
	xibuf.data = (char *)bm->bm_data;
	xibuf.byte_order = (bm->bm_byte_order == BM_MSB_FIRST) ? MSBFirst
							       : LSBFirst;
	xibuf.bitmap_bit_order = BitmapBitOrder(wn__Dpy);
	if (bm->bm_pixel_format == BM_BYTE_PIXELS) {
		xibuf.bitmap_unit = 8;
		xibuf.bitmap_pad = 8;
	}
	else {
		if (bm->bm_lineinc % 2 == 0) {
			xibuf.bitmap_unit = 16;
			xibuf.bitmap_pad = 16;
		}
		else {
			xibuf.bitmap_unit = 32;
			xibuf.bitmap_pad = 32;
		}
	}

	xibuf.depth = bm->bm_nplanes;
	xibuf.bytes_per_line = bm->bm_lineinc;
	xibuf.bits_per_pixel = bm->bm_nplanes;
	xibuf.red_mask = xibuf.blue_mask = xibuf.green_mask = 0;
	xibuf.obdata = NULL;

	XSetState(wn__Dpy, gc, (xpixel_t)fg, (xpixel_t)bg,
				      _wn_Roptab[ropfunc], (xplanemask_t)AllPlanes);
	XPutImage(wn__Dpy, win, gc, &xibuf, sx, sy, dx, dy,
					(unsigned)width, (unsigned)height);
}
#endif /* X11 */
