/*
 * This software is copyrighted as noted below.  It may be freely copied,
 * modified, and redistributed, provided that the copyright notice is 
 * preserved on all copies.
 * 
 * There is no warranty or other guarantee of fitness for this software,
 * it is provided solely "as is".  Bug reports or fixes may be sent
 * to the author, who may or may not act on them as he desires.
 *
 * You may not include this software in a program or other software product
 * without supplying the source, or without informing the end-user that the 
 * source is available for no extra charge.
 *
 * If you modify this software, you should include a notice giving the
 * name of the person performing the modification, the date of modification,
 * and the reason for such modification.
 */
/* 
 * rlepatch.c - Patch images over a larger image.
 * 
 * Author:	John W. Peterson
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Sun Nov 29 1987
 * Copyright (c) 1987, University of Utah
 *
 * This was a quick hack.  It should be changed to use the "raw"
 * routines someday - this would run MUCH faster for sparse patches.
 */

#include <stdio.h>
#include <rle.h>

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

#define CHECK_MALLOC( ptr ) \
	{ if (! (ptr)) {fprintf( stderr, "%s: Unable to alloc memory\n", \
				 cmd_name( argv ) ); exit(-2);}}
#define IN_WINDOW(y,wind) ((y >= wind.ymin) && (y <= wind.ymax))

void
main( argc, argv )
int argc;
char **argv;
{
    rle_hdr im_hdr, *patch_hdr, out_hdr;
    CONST_DECL char *imfilename = NULL, *outfilename = NULL;
    CONST_DECL char **patchnames;
    FILE *outfile = stdout;
    rle_pixel ** im_rows, **patch_rows, ** outrows;
    int stdin_used = 0;
    int patches = 0, oflag = 0, i, y, c, xlen, width;
    int rle_cnt, rle_err;

    if (! scanargs( argc, argv, "% o%-outfile!s infile!s patchfiles%*s",
		    &oflag, &outfilename, &imfilename, &patches, &patchnames ))
	exit( -1 );

    patch_hdr = (rle_hdr *) malloc( sizeof( rle_hdr )
						* patches );
    CHECK_MALLOC( patch_hdr );

    /* Setup the main image data structures. */

    im_hdr.rle_file = rle_open_f( cmd_name( argv ), imfilename, "r" );
    if ( im_hdr.rle_file == stdin )
    {
	imfilename = "Standard input";	/* For error messages */
	stdin_used++;
    }

    for( i = 0; i < patches; i++ )
    {
	patch_hdr[i].rle_file = rle_open_f( cmd_name( argv ),
					    patchnames[i], "r" );
	if ( patch_hdr[i].rle_file == stdin )
	{
	    patchnames[i] = "Standard input";
	    if ( stdin_used )
	    {
		fprintf(stderr, "%s: Only use stdin (-) once.\n",
			cmd_name( argv ));
		exit(-1);
	    }
	    stdin_used++;
	}
    }

    for ( rle_cnt = 0;
	  (rle_err = rle_get_setup( &im_hdr )) == RLE_SUCCESS;
	  rle_cnt++ )
    {
	/* Get the patch files set up */
	for( i = 0; i < patches; i++ )
	{
	    rle_get_setup_ok( &patch_hdr[i], cmd_name( argv ), patchnames[i] );

	    /* Sanity checks. */

	    if ((patch_hdr[i].xmin < im_hdr.xmin) ||
		(patch_hdr[i].xmax > im_hdr.xmax) ||
		(patch_hdr[i].ymin < im_hdr.ymin) ||
		(patch_hdr[i].ymax > im_hdr.ymax))
	    {
		fprintf( stderr, "%s: file %s is outside %s\n",
			 cmd_name( argv ), patchnames[i], imfilename );
		exit( -2 );
	    }

	    if ((patch_hdr[i].ncolors != im_hdr.ncolors) ||
		(patch_hdr[i].alpha != im_hdr.alpha))
	    {
		fprintf( stderr,
			 "%s: file %s doesn't have the same channels as %s\n",
			 cmd_name( argv ), patchnames[i], imfilename );
		exit(-2);
	    }
	}	

	if (rle_row_alloc( &im_hdr, &im_rows ) ||
	    rle_row_alloc( &im_hdr, &patch_rows ))
	    CHECK_MALLOC( 0 );

	/* Setup output */
	out_hdr = im_hdr;
	if ( rle_cnt == 0 )
	    outfile = rle_open_f( cmd_name( argv ), outfilename, "w" );
	out_hdr.rle_file = outfile;

	rle_addhist( argv, &im_hdr, &out_hdr );

	rle_put_setup( &out_hdr );

	/*
	 * Allocate pointers to the output rows.  Note that rle_putrow
	 * expects the pointers to start at xmin, instead of at zero
	 * like rle_getrow (Weird Spencerism).
	 */
	outrows = (rle_pixel**) malloc( sizeof( rle_pixel** ) *
					(im_hdr.alpha + im_hdr.ncolors));
	CHECK_MALLOC( outrows );
	if (im_hdr.alpha)
	    outrows++;		/* Put alpha at -1 index */

	xlen = out_hdr.xmax - out_hdr.xmin + 1;
	for( i = -im_hdr.alpha; i < im_hdr.ncolors; i++ )
	    outrows[i] = &(im_rows[i][im_hdr.xmin]);

	/* Process the images. */

	for (y = im_hdr.ymin; y <= im_hdr.ymax; y++)
	{
	    rle_getrow( &im_hdr, im_rows );

	    for (i = 0; i < patches; i++)
	    {
		if (IN_WINDOW( y, patch_hdr[i]))
		{
		    rle_getrow( &(patch_hdr[i]), patch_rows );
		    width = patch_hdr[i].xmax - patch_hdr[i].xmin + 1;
		    for( c = -im_hdr.alpha; c < im_hdr.ncolors; c++)
			bcopy( &(patch_rows[c][patch_hdr[i].xmin]),
			       &(im_rows[c][patch_hdr[i].xmin]), width );
		}
	    }
	    rle_putrow( outrows, xlen, &out_hdr );
	}
    
	rle_puteof( &out_hdr );

	/* Release memory. */
	rle_row_free( &im_hdr, im_rows );
	rle_row_free( &im_hdr, patch_rows );
	if ( im_hdr.alpha )
	    outrows--;
	free( outrows );
    }
    exit( 0 );
}
