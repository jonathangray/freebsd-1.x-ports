/* Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.  All rights reserved.

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

/*
gdevln03.c
Ghostscript driver for DEC LN03 printer

Ulrich Mueller, Div. PPE, CERN, CH-1211 Geneva 23 <ulm@vsnhd1.cern.ch>
This code is subject to the GNU General Public License

ulm 91-02-13 created as driver for gs 2.1.1
ulm 91-07-23 adapted to gs 2.2
ulm 91-08-21 changed memory allocation to gs_malloc,
	     ported to VMS (contributed by Martin Stiftinger, TU Vienna)
lpd 91-11-24 sped up by removing multiplies from inner loop
ijmp 92-04-14 add support for la75/la50 (macphed@dvinci.usask.ca)
ulm 92-09-25 support letter size paper (8.5" x 11")
*/

#include "gdevprn.h"

/* Forward references */
private int sixel_print_page(P3(gx_device_printer *pdev,
			       FILE *prn_stream, const char *init));

/* The device descriptor */
private dev_proc_output_page(sixel_output_page);
private dev_proc_print_page(ln03_print_page);
/* We have to supply our own procs, since we have to intercept */
/* output_page so we can open the printer in text mode. */
private gx_device_procs sixel_procs =
  prn_procs(gdev_prn_open, sixel_output_page, gdev_prn_close);

#ifdef A4
#  define BOTTOM_MARGIN 0.5
#else
#  define BOTTOM_MARGIN 0.4
#endif
gx_device_printer gs_ln03_device =
    prn_device(sixel_procs, "ln03",
	       DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
	       300, 300,		/* x_dpi, y_dpi */
	       0, BOTTOM_MARGIN, 0, 0,	/* left, bottom, right, top margin */
	       1, ln03_print_page);

/*
 * Initialization string: switch to graphics mode, 300 dpi
 * <ESC>[!p	    DECSTR	soft terminal reset
 * <ESC>[11h	    PUM		select unit of measurement
 * <ESC>[7 I	    SSU		select pixel as size unit
 * <ESC>[?52h	    DECOPM	origin is upper-left corner
 * <ESC>[0t	    DECSLPP	set maximum form length
 * <ESC>[1;2475s    DECSLRM	set left and right margins
 * <ESC>P0;0;1q			select sixel graphics mode
 * "1;1		    DECGRA	aspect ratio (1:1)
 */

#define LN03_INIT \
 "\033[!p\033[11h\033[7 I\033[?52h\033[0t\033[1;2475s\033P0;0;1q\"1;1"

private int
ln03_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
    return (sixel_print_page(pdev,prn_stream,LN03_INIT));
}

/*
 * LA75 dot matrix printer device.
 * This uses North American 8.5 x 11 inch paper size.
 */
private dev_proc_print_page(la75_print_page);
gx_device_printer gs_la75_device =
    prn_device(sixel_procs, "la75",
	       85,
	       110,
	       144, 72,
	       0, 0, 0.5, 0,
	       1, la75_print_page);

#define LA75_INIT "\033P0;0;0q"

private int
la75_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
    return (sixel_print_page(pdev,prn_stream,LA75_INIT));
}

/*
 * LA50 dot matrix printer device.
 * This uses North American 8.5 x 11 inch paper size.
 */
private dev_proc_print_page(la50_print_page);
gx_device_printer gs_la50_device =
    prn_device(sixel_procs, "la50",
	       85,
	       110,
	       144, 72,
	       0, 0, 0.5, 0,
	       1, la50_print_page);
/* LA50's use a very primitive form of initialization */

#define LA50_INIT "\033Pq"

private int
la50_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
    return (sixel_print_page(pdev,prn_stream,LA50_INIT));
}

/* ------ Internal routines ------ */

/* Open the printer in text mode before gdev_prn_output_page */
/* opens it in binary mode. */
private int
sixel_output_page(gx_device *pdev, int num_copies, int flush)
{	int code = gdev_prn_open_printer(pdev, 0);
	if ( code < 0 )
		return code;
	return gdev_prn_output_page(pdev, num_copies, flush);
}

/* Send the page to the printer. */
private int
sixel_print_page(gx_device_printer *pdev, FILE *prn_stream, const char *init)
{
    byte *in, *inp;
    int lnum, lcount, l, count, empty, mask, c, oldc, line_size, in_size;

    line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
    in_size = line_size * 6;
    in = (byte *)gs_malloc(in_size, 1, "sixel_print_page");

    /* Check allocation */
    if (!in) return(-1);

    fputs(init,prn_stream);

    /* Print lines of graphics */
    for (lnum = lcount = 0; lnum < pdev->height; lnum+=6, lcount++) {
	gdev_prn_copy_scan_lines(pdev, lnum, inp = in, line_size * 6);

	mask = 0200;
	oldc = 077;
	empty = 1;

	for (l = pdev->width, count = 0; --l >= 0; count++) {
	    /* transpose 6*8 rectangle */
	    register byte *iptr = inp;
	    c = 077;
	    if (*iptr & mask)
		c += 1;
	    if (*(iptr += line_size) & mask)
		c += 2;
	    if (*(iptr += line_size) & mask)
		c += 4;
	    if (*(iptr += line_size) & mask)
		c += 010;
	    if (*(iptr += line_size) & mask)
		c += 020;
	    if (*(iptr += line_size) & mask)
		c += 040;
	    if (!(mask >>= 1)) {
		mask = 0200;
		inp++;
	    }

	    if (c != oldc) {
		if (empty) {
		    while (--lcount >= 0) {
			/*
			 * Terminate record for VMS STREAM-LF file.
			 * This LF is ignored by the LN03.
			 */
			fputc('\n', prn_stream);
			/* Terminate previous line. */
			fputc('-', prn_stream);
		    }
		    empty = lcount = 0;
		}
		if (count > 3)
		    /* use run length encoding */
		    fprintf(prn_stream, "!%d%c", count, oldc);
		else
		    while (--count >= 0)
			fputc(oldc, prn_stream);
		oldc = c;
		count = 0;
	    }
	}
	if (c != 077) {
	    if (count > 3)
		/* use run length encoding */
		fprintf(prn_stream, "!%d%c", count, c);
	    else
		while (--count >= 0)
		    fputc(c, prn_stream);
	}
    }

    /* leave sixel graphics mode, eject page
       <ESC>\		ST	string terminator
       <FF>		FF	form feed */
    fputs("\033\\\f", prn_stream);
    fflush(prn_stream);

    gs_free((char *)in, in_size, 1, "sixel_print_page");

    return(0);
}
