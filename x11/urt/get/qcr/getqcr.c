/* 
 * getqcr.c - Image display for the Matrix QCR-Z
 * 
 * Author:	John W. Peterson
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Wed Jan 20 1988
 * Copyright (c) 1988, University of Utah
 *
 * BUGS
 *  verbose flag should turn on "reading red..." type messages.
 *  Should handle single channel images as black and white.
 */

#include <stdio.h>
#include "qcr.h"
#include "rle.h"

main (argc, argv)
int argc;
char ** argv;
{
    rle_pixel * camera_data = NULL, * cdptr;
    rle_pixel ** rows;
    char * filename;
    int color = 0;
    int y, i, ysize;
    int verbose_flag = 0, double_flag = 0, exposures = 1, exp_flag = 0;
    int center_flag = 0, pos_flag = 0, fourK = 0, xstart = 0, ystart = 0;

    if (! scanargs( argc, argv,
		   "% v%- d%- c%- f%- p%-xpos!dyposn!d e%-num!d infile!s",
		   &verbose_flag, &double_flag, &center_flag,
		   &fourK, &pos_flag, &xstart, &ystart,
		   &exp_flag, &exposures, &filename ))
	exit(-1);

    rle_dflt_hdr.ncolors = 1;	/* Assume at least one */
    init_qcr( verbose_flag );

    if (double_flag)
	exposures = 2;

    while( color < rle_dflt_hdr.ncolors )
    {
	rle_dflt_hdr.rle_file = rle_open_f( "getqcr", filename, "r" );
	if ( rle_dflt_hdr.ncolors > 1 &&
	     rle_dflt_hdr.rle_file == stdin &&
	     fseek( rle_dflt_hdr.rle_file, 0L, 0 ) < 0 )
	{
	    fprintf( "Can't pipe color data to getqcr.\n" );
	    exit( -1 );
	}

	rle_get_setup_ok( &rle_dflt_hdr, "getqcr", filename );

	if (! pos_flag)
	{
	    xstart = rle_dflt_hdr.xmin;
	    ystart = rle_dflt_hdr.ymin;
	}

	rle_dflt_hdr.xmax -= rle_dflt_hdr.xmin;
	rle_dflt_hdr.xmin = 0;
	ysize = (rle_dflt_hdr.ymax - rle_dflt_hdr.ymin + 1);

	if (! camera_data)	/* Only allocate once */
	{
	    rows = (rle_pixel **) malloc( rle_dflt_hdr.ncolors
					 * sizeof( rle_pixel * ));
	    camera_data = (rle_pixel *)
		malloc( (rle_dflt_hdr.xmax + 1) * ysize);

	    if (! (rows && camera_data))
	    {
		fprintf(stderr, "getqcr: out of memory\n");
		exit(-2);
	    }
	}

	/* Only read one color at a time, never alpha */
	for (i = -rle_dflt_hdr.alpha; i < rle_dflt_hdr.ncolors; i++)
	    RLE_CLR_BIT( rle_dflt_hdr, i );
	RLE_SET_BIT( rle_dflt_hdr, color );

	rows[color] = camera_data;

	if (center_flag)
	    set_up_qcr( rle_dflt_hdr.xmax + 1, ysize, ysize, 0 );
	else
	    set_up_qcr_nc( xstart, ystart,
			   rle_dflt_hdr.xmax + 1, ysize, fourK );

	for (y = rle_dflt_hdr.ymin; y <= rle_dflt_hdr.ymax; y++)
	{
	    rle_getrow( &rle_dflt_hdr, rows );
	    rows[color] = &(rows[color][rle_dflt_hdr.xmax+1]);
	}

	for ( i = 0; i < exposures; i++)
	    send_pixel_image( color, camera_data,
			      (rle_dflt_hdr.xmax + 1) * ysize);

	fclose( rle_dflt_hdr.rle_file );
	color++;
    }
    exit( 0 );
}
