/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.

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

/* gdevbit.c */
/* Fake bitmapped device to estimate rendering time. */
#include "gdevprn.h"

/* Define the device parameters. */
#ifndef X_DPI
#  define X_DPI 72
#endif
#ifndef Y_DPI
#  define Y_DPI 72
#endif

/* The device descriptor */
private dev_proc_print_page(bit_print_page);
gx_device_printer far_data gs_bit_device =
  prn_device(prn_std_procs, "bit",
	DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	1, bit_print_page);

/* Send the page to the printer. */
private int
bit_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	/* Just dump the bits on the file. */
	/* If the file is 'nul', don't even do the writes. */
	int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
	int lnum;
	byte *in = (byte *)gs_malloc(line_size, 1, "bit_print_page(in)");
	byte *data;
	int nul = !strcmp(pdev->fname, "nul");
	if ( in == 0 )
		return_error(gs_error_VMerror);
	for ( lnum = 0; lnum < pdev->height; lnum++ )
	   {	gdev_prn_get_bits(pdev, lnum, in, &data);
		if ( !nul )
			fwrite(data, 1, line_size, prn_stream);
	   }
	gs_free((char *)in, line_size, 1, "bit_print_page(in)");
	return 0;
}
