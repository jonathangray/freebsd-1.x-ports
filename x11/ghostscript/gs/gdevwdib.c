/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gdevwdib.c */
/* MS Windows 3.n driver for Ghostscript using a DIB for buffering. */
#include "dos_.h"
#include "gdevmswn.h"
#include "gxdevmem.h"

/* Make sure we cast to the correct structure type. */
typedef struct gx_device_win_dib_s gx_device_win_dib;
#undef wdev
#define wdev ((gx_device_win_dib *)dev)

/* Device procedures */

/* See gxdevice.h for the definitions of the procedures. */
private dev_proc_open_device(win_dib_open);
private dev_proc_get_initial_matrix(win_dib_get_initial_matrix);
private dev_proc_close_device(win_dib_close);
private dev_proc_map_rgb_color(win_dib_map_rgb_color);
private dev_proc_fill_rectangle(win_dib_fill_rectangle);
private dev_proc_copy_mono(win_dib_copy_mono);
private dev_proc_copy_color(win_dib_copy_color);
/* Windows-specific procedures */
private win_proc_copy_to_clipboard(win_dib_copy_to_clipboard);
private win_proc_repaint(win_dib_repaint);
private win_proc_alloc_bitmap(win_dib_alloc_bitmap);
private win_proc_free_bitmap(win_dib_free_bitmap);

/* The device descriptor */
struct gx_device_win_dib_s {
	gx_device_common;
	gx_device_win_common;

	/* The following help manage the division of the DIB */
	/* into 64K segments.  Each block of y_block scan lines */
	/* starting at y_base mod 64K falls in a single segment. */
	/* Since the raster is a power of 2, y_block is a power of 2. */

	int y_block;
	int y_base;
	int y_mask;		/* y_block - 1 */

	HGLOBAL hmdata;
	gx_device_memory mdev;
};
private gx_device_procs win_dib_procs = {
	win_dib_open,
	win_dib_get_initial_matrix,
	win_sync_output,
	win_output_page,
	win_dib_close,
	win_dib_map_rgb_color,
	win_map_color_rgb,
	win_dib_fill_rectangle,
	gx_default_tile_rectangle,
	win_dib_copy_mono,
	win_dib_copy_color,
	gx_default_draw_line,
	gx_default_get_bits,
	gx_default_get_props,
	win_put_props,
	gx_default_map_cmyk_color,
	win_get_xfont_procs
};
gx_device_win_dib gs_mswin_device = {
	sizeof(gx_device_win_dib),
	&win_dib_procs,
	"mswin",
	INITIAL_WIDTH, INITIAL_HEIGHT, 	/* win_open() fills these in later */
	INITIAL_RESOLUTION, INITIAL_RESOLUTION,	/* win_open() fills these in later */
	no_margins,
	dci_black_and_white,
	0,		/* not open yet */
	2,		/* nColors */
	win_dib_copy_to_clipboard,
	win_dib_repaint,
	win_dib_alloc_bitmap,
	win_dib_free_bitmap
};


/* Open the win_dib driver */
private int
win_dib_open(gx_device *dev)
{
	int code = win_open(dev);
	if ( code < 0 ) return code;

	if ( gdev_mem_device_for_bits(dev->color_info.depth) == 0 )
	{	win_close(dev);
		return gs_error_rangecheck;
	}
	return win_dib_alloc_bitmap((gx_device_win *)dev, dev);
}

/* Get the initial matrix.  DIBs, unlike most displays, */
/* put (0,0) in the lower left corner. */
private void
win_dib_get_initial_matrix(gx_device *dev, gs_matrix *pmat)
{	pmat->xx = dev->x_pixels_per_inch / 72.0;
	pmat->xy = 0;
	pmat->yx = 0;
	pmat->yy = dev->y_pixels_per_inch / 72.0;
	pmat->tx = 0;
	pmat->ty = 0;
}

/* Close the win_dib driver */
private int
win_dib_close(gx_device *dev)
{	win_dib_free_bitmap((gx_device_win *)dev);
	return win_close(dev);
}

/* Map a r-g-b color to the colors available under Windows */
private gx_color_index
win_dib_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	int i = wdev->nColors;
	gx_color_index color = win_map_rgb_color(dev, r, g, b);
	if ( color != i ) return color;

	return color;
}

/* The drawing routines must all be careful not to cross */
/* a segment boundary. */

#define wmdev ((gx_device *)&wdev->mdev)
#define wmproc(proc) (*wdev->mdev.procs->proc)

#define single_block(y, h)\
  !(((y - wdev->y_base) ^ (y - wdev->y_base + h - 1)) & ~wdev->y_mask)

#define BEGIN_BLOCKS\
{	int by, bh, left = h;\
	for ( by = y; left > 0; by += bh, left -= bh )\
	{	bh = wdev->y_block - (by & wdev->y_mask);\
		if ( bh > left ) bh = left;
#define END_BLOCKS\
	}\
}

/* Fill a rectangle. */
private int
win_dib_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	if ( single_block(y, h) )
	{	wmproc(fill_rectangle)(wmdev, x, y, w, h, color);
	}
	else
	{	/* Divide the transfer into blocks. */
		BEGIN_BLOCKS
			wmproc(fill_rectangle)(wmdev, x, by, w, bh, color);
		END_BLOCKS
	}
	win_update((gx_device_win *)dev);
	return 0;
}

/* Copy a monochrome bitmap.  The colors are given explicitly. */
/* Color = gx_no_color_index means transparent (no effect on the image). */
private int
win_dib_copy_mono(gx_device *dev,
  const byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h,
  gx_color_index zero, gx_color_index one)
{	if ( single_block(y, h) )
	{	wmproc(copy_mono)(wmdev, base, sourcex, raster, id,
					 x, y, w, h, zero, one);
	}
	else
	{	/* Divide the transfer into blocks. */
		const byte *source = base;
		BEGIN_BLOCKS
			wmproc(copy_mono)(wmdev, source, sourcex, raster,
					  gx_no_bitmap_id, x, by, w, bh,
					  zero, one);
			source += bh * raster;
		END_BLOCKS
	}
	win_update((gx_device_win *)dev);
	return 0;
}

/* Copy a color pixel map.  This is just like a bitmap, except that */
/* each pixel takes 8 or 4 bits instead of 1 when device driver has color. */
private int
win_dib_copy_color(gx_device *dev,
  const byte *base, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	if ( single_block(y, h) )
	{	wmproc(copy_color)(wmdev, base, sourcex, raster, id,
					  x, y, w, h);
	}
	else
	{	/* Divide the transfer into blocks. */
		const byte *source = base;
		BEGIN_BLOCKS
			wmproc(copy_color)(wmdev, source, sourcex, raster,
					   gx_no_bitmap_id, x, by, w, bh);
			source += by * raster;
		END_BLOCKS
	}
	win_update((gx_device_win *)dev);
	return 0;
}

/* ------ Windows-specific device procedures ------ */


/* Repaint a section of the window. */
private void
win_dib_repaint(gx_device_win *dev, HDC hdc, int dx, int dy, int wx, int wy,
  int sx, int sy)
{	struct bmi_s {
		BITMAPINFOHEADER h;
		ushort pal_index[256];
	} bmi;
	int i;

	bmi.h.biSize = sizeof(bmi.h);
	bmi.h.biWidth = wdev->mdev.width;
	bmi.h.biHeight = wy;
	bmi.h.biPlanes = 1;
	bmi.h.biBitCount = dev->color_info.depth;
	bmi.h.biCompression = 0;
	bmi.h.biSizeImage = 0;			/* default */
	bmi.h.biXPelsPerMeter = 0;		/* default */
	bmi.h.biYPelsPerMeter = 0;		/* default */
	bmi.h.biClrUsed = wdev->nColors;
	bmi.h.biClrImportant = wdev->nColors;
	for ( i = 0; i < wdev->nColors; i++ )
		bmi.pal_index[i] = i;

	SetDIBitsToDevice(hdc, dx, dy, wx, wy,
		sx, 0, 0, wy,
		wdev->mdev.line_ptrs[wdev->height - (sy + wy)],
		(BITMAPINFO FAR *)&bmi, DIB_PAL_COLORS);
}


/* This makes a DIB that contains all or part of the bitmap. */
/* The bitmap pixel orgx must start on a byte boundary. */
private HGLOBAL
win_dib_make_dib(gx_device_win *dev, int orgx, int orgy, int wx, int wy)
{
#define xwdev ((gx_device_win_dib *)dev)
	gx_color_value prgb[3];
	HGLOBAL hglobal;
	BYTE FAR *pDIB;
	BITMAPINFOHEADER FAR *pbmih;
	RGBQUAD FAR *pColors;
	BYTE huge *pBits;
	BYTE huge *pLine;
	ulong bitmapsize;
	int i;
	int loffset; 		/* byte offset to start of line */
	UINT lwidth;		/* line width in bytes rounded up to multiple of 4 bytes */
	UINT lseg;		/* bytes remaining in this segment */

	if (orgx + wx > wdev->width)
		wx = wdev->width - orgx;
	if (orgy + wy > wdev->height)
		wy = wdev->height - orgy;

	loffset = orgx * wdev->color_info.depth / 8;
	lwidth =  ((wx * wdev->color_info.depth + 31) & ~31) >> 3;
	bitmapsize = (long)lwidth * wy;

	hglobal = GlobalAlloc(GHND | GMEM_SHARE, sizeof(BITMAPINFOHEADER) 
		+ sizeof(RGBQUAD)*wdev->nColors + bitmapsize);
	if (hglobal == (HGLOBAL)NULL) {
		MessageBeep(-1);
		return(HGLOBAL)NULL;
	}
	pDIB = (BYTE FAR *)GlobalLock(hglobal);
	if (pDIB == (BYTE FAR *)NULL) {
		MessageBeep(-1);
		return(HGLOBAL)NULL;
	}
	pbmih = (BITMAPINFOHEADER FAR *)(pDIB); 
	pColors = (RGBQUAD FAR *)(pDIB + sizeof(BITMAPINFOHEADER));
	pBits = (BYTE huge *)(pDIB + sizeof(BITMAPINFOHEADER) + sizeof(RGBQUAD)*wdev->nColors);

	pbmih->biSize = sizeof(BITMAPINFOHEADER);
	pbmih->biWidth = wx;
	pbmih->biHeight = wy;
	pbmih->biPlanes = 1;
	pbmih->biBitCount = wdev->color_info.depth;
	pbmih->biCompression = 0;
	pbmih->biSizeImage = 0;			/* default */
	pbmih->biXPelsPerMeter = 0;		/* default */
	pbmih->biYPelsPerMeter = 0;		/* default */
	pbmih->biClrUsed = wdev->nColors;
	pbmih->biClrImportant = wdev->nColors;

	for ( i = 0; i < wdev->nColors; i++ ) {
		win_map_color_rgb((gx_device *)wdev, (gx_color_index)i, prgb);
		pColors[i].rgbRed   = win_color_value(prgb[0]);
		pColors[i].rgbGreen = win_color_value(prgb[1]);
		pColors[i].rgbBlue  = win_color_value(prgb[2]);
		pColors[i].rgbReserved = 0;
	}


	pLine = pBits;
	for ( i = orgy; i < orgy + wy; i++ ) {
		/* Window 3.1 has hmemcpy, but 3.0 doesn't */
		lseg = (UINT)(-OFFSETOF(pLine));   /* remaining bytes in this segment */
		if (lseg >= lwidth) { 
		   _fmemcpy(pLine, xwdev->mdev.line_ptrs[i] + loffset, lwidth);
		}
	        else { /* break up transfer to avoid crossing segment boundary */
		   _fmemcpy(pLine, xwdev->mdev.line_ptrs[i] + loffset, lseg);
		   _fmemcpy(pLine+lseg, xwdev->mdev.line_ptrs[i] + loffset + lseg, lwidth - lseg);
		}
		pLine += lwidth;
	}

	GlobalUnlock(hglobal);
	return hglobal;
}


/* Copy the bitmap to the clipboard. */
private void
win_dib_copy_to_clipboard(gx_device_win *dev)
{
	HGLOBAL hglobal;
	hglobal = win_dib_make_dib(dev, 0, 0, wdev->width, wdev->height);
	if (hglobal == (HGLOBAL)NULL) {
	    MessageBox(wdev->hwndimg, "Not enough memory to Copy to Clipboard", 
		szAppName, MB_OK | MB_ICONEXCLAMATION);
	    return;
	}
	OpenClipboard(dev->hwndimg);
	EmptyClipboard();
	SetClipboardData(CF_DIB, hglobal);
	SetClipboardData(CF_PALETTE, CreatePalette(wdev->limgpalette));
	CloseClipboard();
}


/* Allocate the backing bitmap. */
private int
win_dib_alloc_bitmap(gx_device_win *dev, gx_device *param_dev)
{
	int width;
	gx_device_memory mdev;
	HGLOBAL hmdata;
	byte huge *base;
	byte huge *ptr_base;
	ulong data_size;
	uint ptr_size;
	uint raster;

	/* Round up the width so that the scan line size is a power of 2. */
	/* Only works if bits per pixel is a power of 2, i.e., */
	/* doesn't work for 24-bit devices.  We'll fix this someday.... */

	width = param_dev->width - 1;
	while ( width & (width + 1) ) width |= width >> 1;
	width++;

	/* Finish initializing the DIB. */

	mdev = *gdev_mem_device_for_bits(dev->color_info.depth);
	mdev.width = width;
	mdev.height = param_dev->height;
	mdev.target = (gx_device *)dev;
	raster = gdev_mem_raster(&mdev);
	data_size = (ulong)raster * mdev.height;
	ptr_size = sizeof(byte **) * mdev.height;
	hmdata = GlobalAlloc(0, raster + data_size + ptr_size * 2);
	if ( hmdata == 0 )
		return win_nomemory();

	/* Nothing can go wrong now.... */

	wdev->hmdata = hmdata;
	base = GlobalLock(hmdata);
	/* Adjust base so scan lines, and the pointer table, */
	/* don't cross a segment boundary. */
	base += (-PTR_OFF(base) & (raster - 1));
	ptr_base = base + data_size;
	if ( PTR_OFF(ptr_base + ptr_size) < ptr_size )
		base += (uint)-PTR_OFF(ptr_base);
	wdev->mdev = mdev;
	wdev->mdev.base = (byte *)base;
	wdev->y_block = 0x10000L / raster;
	wdev->y_mask = wdev->y_block - 1;
	if ( (wdev->y_base = PTR_OFF(base)) != 0 )
		wdev->y_base = -(PTR_OFF(base) / raster);
	(*wdev->mdev.procs->open_device)((gx_device *)&wdev->mdev);
	return 0;
}


/* Free the backing bitmap. */
private void
win_dib_free_bitmap(gx_device_win *dev)
{	HGLOBAL hmdata = wdev->hmdata;
	GlobalUnlock(hmdata);
	GlobalFree(hmdata);
}
