/* wn_bm.c - bitmap creation and format munging */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_bm_c_sccsid[] = "@(#)wn_bm.c	1.11 25/4/92 (UKC)";

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_misc.h"
#include "wn_bm.h"

static void dump_row PROTO((FILE *fp, bitmap_t *bm, int x1, int y, int width, int margin));
static void set_lineinc PROTO((bitmap_t *bm, int lineinc));
static void flip_bit_order PROTO((bitmap_t *bm, int bit_order));
static void flip_byte_order PROTO((bitmap_t *bm, int byte_order));
static void z_to_xy_pixels PROTO((bitmap_t *bm, int lineinc));
static void xy_to_z_pixels PROTO((bitmap_t *bm, int lineinc));

#define BIT_ORDER	(BM_BIT0_LEFT | BM_BIT0_RIGHT)

/*  Bitmaps must have lineinc such that is an exact multiple of
 *  Lineinc_align.
 *  If a bitmap does not satisfy this constraint, wn_change_bitmap_format() will
 *  fix it.
 */
static int Lineinc_align = 0;
static int Display_bit_order;
static int Display_byte_order;
static int Display_pixel_format;
static int Machine_byte_order;

void
_wn_define_machine_bitmap_format()
{
	static unsigned short test = 0x1122;

	Machine_byte_order = (*((char *)&test) == 0x11) ? BM_MSB_FIRST
							: BM_LSB_FIRST;
#ifdef SUNVIEW
	Lineinc_align = 16;
	Display_pixel_format = BM_BYTE_PIXELS;
#ifdef sun386
	Display_bit_order = BM_BIT0_LEFT;
#else
	Display_bit_order = BM_BIT0_RIGHT;
#endif /* sun386 */
	Display_byte_order = Machine_byte_order;
#endif /* SUNVIEW */
#ifdef X11
	Lineinc_align = BitmapPad(wn__Dpy);
	Display_pixel_format = BM_BYTE_PIXELS;
	Display_bit_order = (BitmapBitOrder(wn__Dpy) == LSBFirst) ? BM_BIT0_LEFT :
								    BM_BIT0_RIGHT;
	Display_byte_order = (ImageByteOrder(wn__Dpy) == LSBFirst) ? BM_LSB_FIRST :
								     BM_MSB_FIRST;
#endif /* X11 */
}

#ifdef SUNVIEW
/*  Return a pixrect, given a bitmap bm in BM_BIT0_RIGHT format.
 *  Multiplane bitmaps must have byte pixels (we only support 8 plane).
 *  The returned value points to static storage, so we can only have
 *  one of these at a time.
 */
struct pixrect *
_wn_bm_to_pixrect(bm)
bitmap_t *bm;
{
	extern struct pixrectops mem_ops;
	static struct mpr_data mdbuf = { UNSET, UNSET, { 0, 0 }, FALSE, 0 };
	static struct pixrect prbuf = { &mem_ops,UNSET,UNSET,UNSET,(caddr_t)&mdbuf };

	if (FORMAT_WRONG(bm, DISP_OK))
		wn__panic("bm_to_pixrect passed a bitmap with wrong bit order");
	
	mdbuf.md_linebytes = bm->bm_lineinc;
	mdbuf.md_image = (short *) bm->bm_data;
#ifdef sun386
	mdbuf.md_flags |= MP_I386; /* see section 2.2 in 4.0 Pixrect manual */
#endif /* sun386 */
	prbuf.pr_size.x = bm->bm_width;
	prbuf.pr_size.y = bm->bm_height;
	prbuf.pr_depth = bm->bm_nplanes;
	return &prbuf;
}
#endif /* SUNVIEW */

static void
dump_row(fp, bm, x1, y, width, margin)
FILE *fp;
bitmap_t *bm;
int x1, y, width, margin;
{
	int x;

	for (x = x1 - margin; x < x1 + width + 2 * margin; x++) {
		if (x == x1)
			putc('[', fp);
		if (x % 16 == 0)
			putc('|', fp);
		putc(wn_getbit(bm, x, y) ? '#' : '.', fp);
		if (x == x1 + width - 1)
			putc(']', fp);
	}
}

/*  Dump an ascii representation of the specified region of a bitmap.
 */
void
_wn_dump_bits(fp, bm, orig_x, orig_y, width, height, margin)
FILE *fp;
bitmap_t *bm;
int orig_x, orig_y, width, height, margin;
{
	int y;

	for (y = orig_y; y < orig_y + height; y++) {
		putc('\t', fp);
		dump_row(fp, bm, orig_x, y, width, margin);
		putc('\n', fp);
	}
	putc('\n', fp);
}

/*  Like dump_bits above, but dump two bitmaps side by side.
 */
void
_wn_dump2_bits(fp, bm1, x1, y1, bm2, x2, y2, width, height, margin)
FILE *fp;
bitmap_t *bm1;
int x1, y1;
bitmap_t *bm2;
int x2, y2, width, height, margin;
{
	int h;

	for (h = 0; h < height; h++) {
		putc('\t', fp);
		dump_row(fp, bm1, x1, y1+h, width, margin);
		fputs("       ", fp);
		dump_row(fp, bm2, x2, y2+h, width, margin);
		putc('\n', fp);
	}
	putc('\n', fp);
}

void
wn_dump_bitmap(fp, bm, x, y, width, height)
FILE *fp;
bitmap_t *bm;
int x, y, width, height;
{
	if (x < 0) {
		width += x;
		x = 0;
	}
	if (y < 0) {
		height += y;
		y = 0;
	}
	if (width > bm->bm_width)
		width = bm->bm_width;
	if (height > bm->bm_height)
		height = bm->bm_height;
	_wn_dump_bits(fp, bm, x, y, width, height, 0);
	fflush(fp);
}

/*  This table and the macro that uses it copied from ~X/libsun/font.c
 */
unsigned short _wn_byteflip_tab[256] = {
    0x00,0x80,0x40,0xc0,0x20,0xa0,0x60,0xe0,0x10,0x90,0x50,0xd0,0x30,0xb0,0x70,0xf0, 
    0x08,0x88,0x48,0xc8,0x28,0xa8,0x68,0xe8,0x18,0x98,0x58,0xd8,0x38,0xb8,0x78,0xf8, 
    0x04,0x84,0x44,0xc4,0x24,0xa4,0x64,0xe4,0x14,0x94,0x54,0xd4,0x34,0xb4,0x74,0xf4, 
    0x0c,0x8c,0x4c,0xcc,0x2c,0xac,0x6c,0xec,0x1c,0x9c,0x5c,0xdc,0x3c,0xbc,0x7c,0xfc, 
    0x02,0x82,0x42,0xc2,0x22,0xa2,0x62,0xe2,0x12,0x92,0x52,0xd2,0x32,0xb2,0x72,0xf2,
    0x0a,0x8a,0x4a,0xca,0x2a,0xaa,0x6a,0xea,0x1a,0x9a,0x5a,0xda,0x3a,0xba,0x7a,0xfa, 
    0x06,0x86,0x46,0xc6,0x26,0xa6,0x66,0xe6,0x16,0x96,0x56,0xd6,0x36,0xb6,0x76,0xf6, 
    0x0e,0x8e,0x4e,0xce,0x2e,0xae,0x6e,0xee,0x1e,0x9e,0x5e,0xde,0x3e,0xbe,0x7e,0xfe, 
    0x01,0x81,0x41,0xc1,0x21,0xa1,0x61,0xe1,0x11,0x91,0x51,0xd1,0x31,0xb1,0x71,0xf1, 
    0x09,0x89,0x49,0xc9,0x29,0xa9,0x69,0xe9,0x19,0x99,0x59,0xd9,0x39,0xb9,0x79,0xf9, 
    0x05,0x85,0x45,0xc5,0x25,0xa5,0x65,0xe5,0x15,0x95,0x55,0xd5,0x35,0xb5,0x75,0xf5, 
    0x0d,0x8d,0x4d,0xcd,0x2d,0xad,0x6d,0xed,0x1d,0x9d,0x5d,0xdd,0x3d,0xbd,0x7d,0xfd,
    0x03,0x83,0x43,0xc3,0x23,0xa3,0x63,0xe3,0x13,0x93,0x53,0xd3,0x33,0xb3,0x73,0xf3,
    0x0b,0x8b,0x4b,0xcb,0x2b,0xab,0x6b,0xeb,0x1b,0x9b,0x5b,0xdb,0x3b,0xbb,0x7b,0xfb,
    0x07,0x87,0x47,0xc7,0x27,0xa7,0x67,0xe7,0x17,0x97,0x57,0xd7,0x37,0xb7,0x77,0xf7, 
    0x0f,0x8f,0x4f,0xcf,0x2f,0xaf,0x6f,0xef,0x1f,0x9f,0x5f,0xdf,0x3f,0xbf,0x7f,0xff
};

static void
set_lineinc(bm, lineinc)
bitmap_t *bm;
int lineinc;
{
	int l, min_lineinc;
	unsigned char *sdata, *ddata, *dbuf;
	register unsigned char *src, *dst, *lim;

	if (bm->bm_pixel_format == BM_XY_PIXELS)
		l = bm->bm_height * bm->bm_nplanes;
	else
		l = bm->bm_height;
	dbuf = (unsigned char *) malloc((size_t)l * lineinc);
	if (dbuf == NULL)
		wn__panic("out of memory in wn_change_bitmap_format");
	memset((char *)dbuf, '\0', l * lineinc);
	ddata = dbuf;
	sdata = (unsigned char *) bm->bm_data;
	min_lineinc = (lineinc < bm->bm_lineinc) ? lineinc : bm->bm_lineinc;
	while (--l >= 0) {
		src = sdata;
		dst = ddata;
		lim = src + min_lineinc;
		while (src < lim)
			*dst++ = *src++;
		sdata += bm->bm_lineinc;
		ddata += lineinc;
	}
	if (bm->bm_flags & BM_CAN_FREE_DATA)
		free((char *)bm->bm_data);
	bm->bm_lineinc = lineinc;
	bm->bm_data = (unsigned short *) dbuf;
	bm->bm_flags |= BM_CAN_FREE_DATA;
}

static void
flip_bit_order(bm, bit_order)
bitmap_t *bm;
int bit_order;
{
	register short *src, *lim;

	src = (short *) bm->bm_data;
	lim = src + bm->bm_nplanes * (bm->bm_lineinc >> 1) * bm->bm_height;
	for (; src < lim; src++)
		*src = WN_FLIP_SHORT(*src);
	bm->bm_bit_order = bit_order;
}

static void
flip_byte_order(bm, byte_order)
bitmap_t *bm;
int byte_order;
{
	register unsigned char *cptr, t;
	register int nshorts;

	cptr = (unsigned char *) bm->bm_data;
	nshorts = (bm->bm_lineinc * bm->bm_height) / 2;

	while (--nshorts >= 0) {
		t = cptr[0];
		cptr[0] = cptr[1];
		cptr[1] = t;
		cptr += 2;
	}

	bm->bm_byte_order = byte_order;
}

static void
z_to_xy_pixels(bm, lineinc)
bitmap_t *bm;
int lineinc;
{
	register unsigned planemask;
	register unsigned short dmask;
	register unsigned short *dptr;
	unsigned short *xy_data;
	char *z_data;
	register char *sptr;
	unsigned short *data_base;
	register int i;
	int y;

	xy_data = (unsigned short *)
			malloc((size_t)lineinc * bm->bm_height * bm->bm_nplanes);
	if (xy_data == NULL)
		wn__panic("malloc failed in z_to_xy_pixels");
	z_data = (char *) bm->bm_data;
	data_base = xy_data;
	for (planemask = 0x80; planemask != 0; planemask >>= 1) {
		sptr = z_data;
		for (y = 0; y < bm->bm_height; y++) {
			dptr = data_base + y * lineinc / 2;
			dmask = 0x1;
			i = bm->bm_width;
			while (--i >= 0) {
				if (*sptr++ & planemask)
					*dptr |= dmask;
				dmask <<= 1;
				if (dmask == 0) {
					dmask = 0x1;
					dptr++;
				}
			}
			sptr += lineinc - bm->bm_width;
		}
		data_base += bm->bm_lineinc / 2 * bm->bm_height;
	}
	if (bm->bm_flags & BM_CAN_FREE_DATA)
		free((char *)z_data);
	bm->bm_data = xy_data;
	bm->bm_lineinc = lineinc;
	bm->bm_pixel_format = BM_XY_PIXELS;
}

/* ARGSUSED */
static void
xy_to_z_pixels(bm, lineinc)
bitmap_t *bm;
int lineinc;
{
	wn__panic("xy_to_z_pixels not yet implemented");
}

void
_wn_set_machine_format(bm, fm, flags)
bitmap_t *bm;
register format_t *fm;
int flags;
{
	int byte_order;

	if ((flags & B_DISP_BYTE_ORDER) && bm->bm_pixel_format != BM_BYTE_PIXELS)
		byte_order = Display_byte_order;
	else
		byte_order = Machine_byte_order;

	fm->fm_used = TRUE;
	fm->fm_bit_order = bm->bm_bit_order;
	fm->fm_byte_order = bm->bm_byte_order;
	fm->fm_pixel_format = bm->bm_pixel_format;
	fm->fm_lineinc = bm->bm_lineinc;
	wn_change_bitmap_format(bm, BM_NATURAL_BIT_ORDER, byte_order,
				    BM_NATURAL_PIXEL_FORMAT, BM_NATURAL_LINEINC);
}

void
_wn_restore_format(bm, fm)
bitmap_t *bm;
format_t *fm;
{
	wn_change_bitmap_format(bm, fm->fm_bit_order, fm->fm_byte_order,
					fm->fm_pixel_format, fm->fm_lineinc);
}

/*  Set the format and lineinc of bitmap bm to the given values.
 */
void
wn_change_bitmap_format(bm, bit_order, byte_order, pixel_format, lineinc)
register bitmap_t *bm;
int bit_order, byte_order, pixel_format;
int lineinc;
{
	int natural_lineinc, temp;

	if (Lineinc_align == 0)
		_wn_define_machine_bitmap_format();

	if (byte_order == BM_MACHINE_BYTE_ORDER)
		byte_order = Machine_byte_order;

	if (bm->bm_byte_order == BM_MACHINE_BYTE_ORDER)
		bm->bm_byte_order = Machine_byte_order;

	if (pixel_format == BM_NATURAL_PIXEL_FORMAT)
		pixel_format = (bm->bm_nplanes == 1) ? BM_XY_PIXELS
						     : Display_pixel_format;
	if (pixel_format == BM_BYTE_PIXELS && bm->bm_nplanes != 8)
		wn__panic("change_bitmap_format: byte pixels on 8 plane bitmaps only");
	
	/*  Get the `natural' lineinc for a bitmap of this width and pixel format
	 *  on this machine.
	 */
	if (pixel_format == BM_XY_PIXELS) {
		temp = (bm->bm_width + Lineinc_align - 1) / Lineinc_align;
		natural_lineinc = temp * Lineinc_align / 8;
	}
	else if (pixel_format == BM_BYTE_PIXELS) {
#ifdef SUNVIEW
		natural_lineinc = bm->bm_width + (bm->bm_width & 1);
#else
		natural_lineinc = bm->bm_width;
#if 0
		lineinc_bytes = Lineinc_align / 8;
		temp = (bm->bm_width + lineinc_bytes - 1) / lineinc_bytes;
		natural_lineinc = temp * lineinc_bytes;
#endif
#endif /* !SUNVIEW */
	}
	else {
		wn__panic("unknown pixel format in wn_change_bitmap_format");
		natural_lineinc = 0; /* to satisfy gcc */
	}

	/*  Convert BM_NATURAL_{BIT,BYTE}_ORDER and BM_NATURAL_LINEINC to the machine
	 *  dependent values.
	 */
	if (bit_order == BM_NATURAL_BIT_ORDER)
		bit_order = Display_bit_order;
	if (byte_order == BM_NATURAL_BYTE_ORDER)
		byte_order = Display_byte_order;
	if (byte_order != BM_MSB_FIRST && byte_order != BM_LSB_FIRST)
		wn__panic("unknown byte order in wn_change_bitmap_format");
	if (bit_order != BM_BIT0_RIGHT && bit_order != BM_BIT0_LEFT)
		wn__panic("unknown bit order in wn_change_bitmap_format");
	if (lineinc == BM_NATURAL_LINEINC)
		lineinc = natural_lineinc;

	/*  Convert the pixel format if necessary.
	 */
	if (pixel_format != bm->bm_pixel_format) {
		if (pixel_format == BM_XY_PIXELS)
			z_to_xy_pixels(bm, lineinc);
		else
			xy_to_z_pixels(bm, lineinc);
	}

	/*  Set the bit order if necessary for XY bitmaps.
	 */
	if (bm->bm_pixel_format == BM_XY_PIXELS && bit_order != bm->bm_bit_order)
		flip_bit_order(bm, bit_order);
	
	/*  Change the byte order if necessary.
	 */
	if (byte_order != bm->bm_byte_order)
		flip_byte_order(bm, byte_order);

	/*  Set the lineinc if necessary.
	 */
	if (lineinc != bm->bm_lineinc)
		set_lineinc(bm, lineinc);

	bm->bm_pflags &= ~(COMMON_FLAGS | B_DISP_BYTE_ORDER | B_XROP_BYTE_ORDER);

	/*  Set the various flags for things that are ok.
	 */
	if (pixel_format == BM_BYTE_PIXELS || bit_order == Display_bit_order)
		bm->bm_pflags |= B_BIT_ORDER_OK;
	if (byte_order == Display_byte_order)
		bm->bm_pflags |= B_DISP_BYTE_ORDER;
	if (byte_order == Machine_byte_order)
		bm->bm_pflags |= B_XROP_BYTE_ORDER;
#ifdef X11
	/*  We grok either pixel format in X.
	 */
	bm->bm_pflags |= B_PIXEL_FORMAT_OK;
#else
	if (bm->bm_nplanes == 1 || pixel_format == Display_pixel_format)
		bm->bm_pflags |= B_PIXEL_FORMAT_OK;
#endif
	if (lineinc == natural_lineinc)
		bm->bm_pflags |= B_LINEINC_OK;
	bm->bm_pflags |= B_DATA_ALIGNED;
}

/*  Return a pointer to a bitmap with nplanes planes of
 *  (width * height) pixels in the format given.
 */
bitmap_t *
wn_make_bitmap(width, height, nplanes, bit_order, pixel_format)
int width, height, nplanes, bit_order, pixel_format;
{
	int nbytes;
	int lineinc;
	bitmap_t *bm;
	unsigned short *data;

	if (Lineinc_align == 0)
		_wn_define_machine_bitmap_format();
	if (bit_order == BM_NATURAL_BIT_ORDER)
		bit_order = Display_bit_order;
	if (pixel_format == BM_NATURAL_PIXEL_FORMAT)
		pixel_format = (nplanes == 8) ? BM_BYTE_PIXELS : BM_XY_PIXELS;
	if (pixel_format == BM_BYTE_PIXELS) {
		lineinc = width;
#ifdef SUNVIEW
		lineinc += width & 1;
#endif /* SUNVIEW */
		nbytes = lineinc * height;
	}
	else {
		lineinc = (width + (Lineinc_align - 1)) / Lineinc_align;
		lineinc *= Lineinc_align / 8;
		nbytes = lineinc * height * nplanes;
	}
	data = (unsigned short *)malloc((size_t)nbytes);
	if (data == NULL)
		return NULL;
	bm = wn_make_bitmap_from_data(width, height, nplanes, data,
						bit_order, pixel_format, lineinc);
	if (bm == NULL)
		free((char *)data);
	else {
		memset((char *)data, '\0', nbytes);
		bm->bm_flags |= BM_CAN_FREE_DATA;
	}
	return bm;
}

bitmap_t *
wn_make_bitmap_from_data(width,height,nplanes,data,bit_order,pixel_format,lineinc)
int width, height, nplanes;
unsigned short *data;
int bit_order, pixel_format, lineinc;
{
	register bitmap_t *bm;

	if (Lineinc_align == 0)
		_wn_define_machine_bitmap_format();
	if (width <= 0 || height <= 0 || nplanes <= 0)
		wn__panic("arg < 1 in wn_make_bitmap_from_data");
	if ((bm = (bitmap_t *) malloc(sizeof(bitmap_t))) == NULL)
		return NULL;

	bm->bm_data = data;

	bm->bm_bit_order = bit_order;
	bm->bm_byte_order = Machine_byte_order;
	bm->bm_pixel_format = pixel_format;

	bm->bm_flags = BM_CAN_FREE_HEADER;
	bm->bm_pflags = 0;

	bm->bm_width = width;
	bm->bm_height = height;
	bm->bm_nplanes = nplanes;

	bm->bm_xhot = 0;
	bm->bm_yhot = 0;

	bm->bm_lineinc = lineinc;
	bm->bm_colormap = NULL;
	bm->bm_cache_data = 0;

	return bm;
}

/*  Free a bitmap.
 */
void 
wn_free_bitmap(bm)
bitmap_t *bm;
{
	if (bm->bm_flags & BM_CAN_FREE_DATA)
		free((char *)bm->bm_data);
	if (bm->bm_flags & BM_CAN_FREE_HEADER)
		free((char *)bm);
}
