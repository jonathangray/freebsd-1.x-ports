/* Copyright (C) 1989-1993 Aladdin Enterprises.  All rights reserved.

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

/* This is a bare bones driver I developed for my apple Dot Matrix Printer.
 * This code originally was from the epson driver, but I removed a lot
 * of stuff that was not needed.
 *
 * The Dot Matrix Printer was a predecessor to the apple Imagewriter.  Its
 * main difference being that it was parallel.
 *
 * This code should work fine on Imagewriters, as they have a superset
 * of commands compared to the DMP printer.
 *
 * This driver does not produce the smalles output files possible.  To
 * do that, it should look through the output strings and find repeat
 * occurances of characters, and use the escape sequence that allows
 * printing repeat sequences.  However, as I see it, this the limiting
 * factor in printing is not transmission speed to the printer itself,
 * but rather, how fast the print head can move.  This is assuming the
 * printer is set up with a reasonable speed (9600 bps)
 *
 * WHAT THE CODE DOES AND DOES NOT DO:
 *
 * To print out images, it sets the printer for unidirection printing
 * and 15 cpi (120 dpi). IT sets line feed to 1/9 of an inch (72 dpi).
 * When finished, it sets things back to bidirection print, 1/8" line
 * feeds, and 12 cpi.  There does not appear to be a way to reset
 * things to initial values.
 *
 * This code does not set for 8 bit characters (which is required). It
 * also assumes that carriage return/newline is needed, and not just
 * carriage return.  These are all switch settings on the DMP, and
 * I have configured them for 8 bit data and cr only.
 *
 * You can search for the strings Init and Reset to find the strings
 * that set up the printer and clear things when finished, and change
 * them to meet your needs.
 *
 * Also, you need to make sure that the printer daemon (assuming unix)
 * doesn't change the data as it is being printed.  I have set my
 * printcap file (sunos 4.1.1) with the string:
 * ms=pass8,-opost
 * and it works fine.
 *
 * Feel free to improve this code if you want.  However, please make
 * sure that the old DMP will still be supported by any changes.  This
 * may mean making an imagewriter device, and just copying this file
 * to something like gdevimage.c.
 *
 * The limiting factor of the DMP is the vertical resolution.  However, I
 * see no way to do anything about this.  Horizontal resolution could
 * be increased by using 17 cpi (136 dpi).  I believe the Imagewriter
 * supports 24 cpi (192 dpi).  However, the higher dpi, the slower
 * the printing.
 *
 * Dot Matrix Code by Mark Wedel (master@cats.ucsc.edu)
 */


#include "gdevprn.h"

#ifndef X_DPI
#  define X_DPI 120
#endif

#ifndef Y_DPI
#  define Y_DPI 72
#endif

/* The device descriptors */
private dev_proc_print_page(dmp_print_page);

/* Standard DMP device */
gx_device_printer gs_dmp_device =
  prn_device(prn_std_procs, "dmp",
	85,				/* width_10ths, 8.5" */
	110,				/* height_10ths, 11" */
	X_DPI, Y_DPI,
	0, 0, 0.5, 0,		/* margins */
	1, dmp_print_page);


/* ------ Internal routines ------ */


/* Send the page to the printer. */
private int
dmp_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	

	int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
	/* Note that in_size is a multiple of 8. */
	int in_size = line_size * 8;
	byte *buf1 = (byte *)gs_malloc(in_size, 1, "eps_print_page(buf1)");
	byte *buf2 = (byte *)gs_malloc(in_size, 1, "eps_print_page(buf2)");
	byte *in = buf1;
	byte *out = buf2;
	int lnum = 0;

	/* Check allocations */
	if ( buf1 == 0 || buf2 == 0 )
	{	if ( buf1 ) 
		  gs_free((char *)buf1, in_size, 1, "eps_print_page(buf1)");
		if ( buf2 ) 
		  gs_free((char *)buf2, in_size, 1, "eps_print_page(buf2)");
		return_error(gs_error_VMerror);
	}

	/* Initialize the printer and reset the margins. */
	fwrite("\r\n\033>\033q\033T16", 1, 10, prn_stream);

	/* Print lines of graphics */
	while ( lnum < pdev->height )
	{	
		byte *inp;
		byte *in_end;
		byte *out_end;
		int lcnt;
		register byte *out_blk;

/* The apple DMP printer seems to be odd in that the bit order on
 * each line is reverse what might be expected.  Meaning, an
 * underscore would be done as a series of 0x80, while on overscore
 * would be done as a series of 0x01.  So we get each
 * scan line in reverse order.
 */

		for (lcnt=0; lcnt<8; lcnt++) {
			if ((lnum+lcnt)>pdev->height) 
				memset(in+lcnt*line_size, 0, line_size);
			else
				gdev_prn_copy_scan_lines(pdev, lnum+lcnt,
					in + line_size*(7 - lcnt), line_size);
		}

	    out_end = out;
	    inp = in;
	    in_end = inp + line_size;
    
                   for ( ; inp < in_end; inp++, out_end += 8 )
   	                { 
    		            gdev_prn_transpose_8x8(inp, 
					           line_size, out_end, 1);
		    	}
			/* Remove trailing 0s. */
 		    while ( out_end > out && out_end[-1] == 0 )
	        	{
		       	    out_end--;
			}
		for (out_blk = out; out_blk< out_end; ) {
			/* skip leading 0s */
			if (*out_blk) break;
			out_blk ++;
		}
		/* write out however many blank's as we got nulls.
		 * In fact, because of the overhead, this is only really
		 * efficient if you have 8 or more leading 0's.
		 */
		if ((out_blk!=out) && ((out_blk-out)>7))
			fprintf(prn_stream,"\033V%04d%c", (int) (out_blk-out), 0);
		else
			out_blk=out;

		if ((int)(out_end-out_blk)) {
			fprintf(prn_stream,"\033G%04d",(int)(out_end - out_blk));
			fwrite(out_blk, 1, (int)(out_end-out_blk), prn_stream);
		}
		fprintf(prn_stream,"\r\n");
	
		lnum += 8 ;
	}

	/* Formfeed and Reset printer */
	fputs("\f\033<\033B\033E", prn_stream);
	fflush(prn_stream);

	gs_free((char *)buf2, in_size, 1, "eps_print_page(buf2)");
	gs_free((char *)buf1, in_size, 1, "eps_print_page(buf1)");
	return 0;
}
