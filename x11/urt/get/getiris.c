/* 
 * getiris.c - Whoop up an rle with 24 bits ( no mex ).
 * 
 * Author:	Glenn McMinn
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Tue Feb  3 1987
 * Copyright (c) 1987 Glenn McMinn
 * 
 */


#define MIN(a,b) ( a<b ? a : b)

#include "gl.h"
#include "device.h"
#include "stdio.h"
#include "rle.h"
#ifdef USE_STDLIB_H
#include <stdlib.h>
#else

#ifdef VOID_STAR
extern void *malloc();
#else
extern char *malloc();
#endif
extern void free();

#endif /* USE_STDLIB_H */

main(argc,argv)
int argc;
char **argv;
{
        int x_len, y_len;
	int i;
	unsigned char **scan;
	Device val;

	/* This program runs without mex running. */
	if ( ismex())
	{
		fprintf( stderr, "getiris:  can't run under mex!\n");
		exit(0);
	}
	ginit();

	/* Turn off cursor so that picture doesn't have a glich. */
	cursoff();
	RGBmode();
	gconfig();
	RGBcolor(0, 0, 0);
	clear();

	/* Setup mouse buttons so that they are queued. */
	qdevice( LEFTMOUSE );
	qdevice( RIGHTMOUSE );
	qdevice( MIDDLEMOUSE );
	qreset();

	/* Take input from file argument or stdin. */ 	
	rle_dflt_hdr.rle_file = rle_open_f("getiris", argv[1], "r");
	rle_get_setup( &rle_dflt_hdr );

	x_len = rle_dflt_hdr.xmax - rle_dflt_hdr.xmin + 1;
	y_len = rle_dflt_hdr.ymax - rle_dflt_hdr.ymin + 1;
	rle_dflt_hdr.xmax -= rle_dflt_hdr.xmin;
	rle_dflt_hdr.xmin = 0;

	/* Grab a scanline. */
	scan = (unsigned char **) malloc( (rle_dflt_hdr.ncolors +
				       rle_dflt_hdr.alpha) *
				      sizeof( unsigned char * ) );
	for ( i = 0; i < rle_dflt_hdr.ncolors + rle_dflt_hdr.alpha; i++ )
	    scan[i] = (unsigned char *)malloc( x_len );

	if ( rle_dflt_hdr.alpha )
	{
	    scan++;
	}

	/* Display each scanline. */
        for ( i = 0; i < MIN(y_len,768) ; i ++)
	{
		rle_getrow(&rle_dflt_hdr, scan );

		cmov2i(0, i);
		writeRGB(x_len, scan[0], scan[1], scan[2]);
	}

	/* Wait for a mouse button push. */
	qread( &val);

	/* Set the iris back up so that it is not scrogged. */
	singlebuffer();
	gconfig();
	clear();
	gexit();
}
