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
 * rletorast.c - Convert RLE to sun rasterfile.
 * 
 * Author:	Rod Bogart, John W. Peterson & Ed Falk (SMI)
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Tue Nov  4 1986
 * Copyright (c) 1986 Rod Bogart
 * 
 * Based on "tobw.c" by Spencer Thomas, and 
 * "rps" by Marc Majka (UBC Vision lab)
 *
 * If an input file isn't specified, it reads from stdin.  An output file
 * can be specified with -o (otherwise it writes to stdout).
 */
#include <stdio.h>
#include <math.h>
#include <pixrect/pixrect_hs.h>
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

typedef	enum { GREY, GREY_ALPHA, COLOR, COLOR_ALPHA } Input_Type ;

void
main( argc, argv )
int argc;
char **argv;
{
    char       *infname = NULL, 
    	       *outfname = NULL;
    FILE       *outfile = stdout;
    int 	oflag = 0;
    int 	y, ncol, nscan, nchan, nochan, i;
    unsigned char **scan;
    unsigned char *buffer;
    struct rasterfile header ;
    unsigned char *image ;
    Input_Type 	input_type ;
    int 	linebytes ;

    if ( scanargs( argc, argv,
	    "% o%-outfile!s infile%s",
	    &oflag, &outfname, &infname ) == 0 )
	exit( 1 );

    /* Open RLE file and get header information */
    rle_dflt_hdr.rle_file = rle_open_f( "rletorast", infname, "r" );
    rle_get_setup_ok( &rle_dflt_hdr, "rletorast", infname );

    if ( ( rle_dflt_hdr.ncolors != 3 ) && ( rle_dflt_hdr.ncolors != 1))
    {
	fprintf( stderr, "%s is not RGB or b&w image",
		    infname ? infname : "stdin" );
	exit( 0 );
    }

    outfile = rle_open_f( "rletorast", outfname, "w" );

    /* 
     * Spencer trick: save space by sliding the input image over to the
     * left margin.
     */
    rle_dflt_hdr.xmax -= rle_dflt_hdr.xmin ;
    rle_dflt_hdr.xmin = 0 ;
    ncol = rle_dflt_hdr.xmax + 1;
    nscan = rle_dflt_hdr.ymax - rle_dflt_hdr.ymin + 1;
    nchan = rle_dflt_hdr.ncolors + (rle_dflt_hdr.alpha ? 1 : 0) ;

    if( rle_dflt_hdr.alpha )
    {
      if( rle_dflt_hdr.ncolors == 1 )
	input_type = GREY_ALPHA ;
      else
	input_type = COLOR_ALPHA ;
    }
    else
    {
      if( rle_dflt_hdr.ncolors == 1 )
	input_type = GREY ;
      else
	input_type = COLOR ;
    }

    /* Allocate scanline memory */
    /*
     * buffer = space for one scanline of one color
     * scan = space for pointers to ncolors+alpha buffers
     * scan[i] = pointer to space for one scanline of one color
     */

    buffer = (unsigned char *)malloc( ncol );
    rle_row_alloc( &rle_dflt_hdr, &scan );

    header.ras_magic = RAS_MAGIC ;
    header.ras_width = ncol ;
    header.ras_height = nscan ;
    switch(input_type)
    {
      case GREY: nochan = 1 ; break ;
      case COLOR: nochan = 3 ; break ;
      case GREY_ALPHA: case COLOR_ALPHA: nochan = 4 ; break ;
      default: nochan = 4; break;
    }
    header.ras_depth = 8 * nochan ;
    linebytes = header.ras_width * nochan ;
    linebytes += linebytes % 2 ;
    header.ras_length = linebytes * header.ras_height ;
    header.ras_type = RT_STANDARD ;
    if( rle_dflt_hdr.ncmap != 0 )
    {
      header.ras_maptype = RMT_EQUAL_RGB ;
      header.ras_maplength = (1 << rle_dflt_hdr.cmaplen) * 3 ;
    }
    else
    {
      header.ras_maptype = RMT_NONE ;
      header.ras_maplength = 0 ;
    }

    fwrite( &header, sizeof(header), 1, outfile );

    if( header.ras_maptype == RMT_EQUAL_RGB )
    {
      register rle_map *iptr ;
      unsigned char out ;
      register int chan;
      int nch, nco ;

      if( rle_dflt_hdr.ncolors == 1 )
      {
	nch = 3 ;
	nco = (1 << rle_dflt_hdr.cmaplen) ;
      }
      else
      {
	nch = 1 ;
	nco = header.ras_maplength ;
      }

      for(chan=0; chan<nch; ++chan)
      {
	iptr = rle_dflt_hdr.cmap ;
	for(i=0; i<nco; ++i)
	{
	  out = (*iptr++ + 0x80) >> 8 ;
	  fwrite(&out, 1,1, outfile) ;
	}
      }
    }

    if( rle_dflt_hdr.comments != NULL )
    {
      register CONST_DECL char **comment = rle_dflt_hdr.comments ;
      while(*comment != NULL)
      {
	fprintf(stderr,"%s\n", *comment) ;
	++comment ;
      }
    }

  image = (unsigned char *) malloc(linebytes * header.ras_height) ;

  if( rle_dflt_hdr.background != 0 )
  {
    register int row,col,chan ;
    register unsigned char *ptr ;
    register int *bptr ;
    for(row=0; row<nscan; ++row)
    {
      ptr = image + row*linebytes ;
      for(col=ncol; --col >= 0; )
      {
	if( rle_dflt_hdr.alpha )
	  *ptr++ = 0 ;
	bptr = rle_dflt_hdr.bg_color ;
	for(chan=rle_dflt_hdr.ncolors; --chan >= 0; )
	  *ptr++ = *bptr++ ;
      }
    }
  }



  while ( (y = rle_getrow( &rle_dflt_hdr, scan )) <=
	  rle_dflt_hdr.ymax )
  {
    register unsigned char *optr, *a,*r,*g,*b ;
    register int col ;

    optr = image + (nscan-y+rle_dflt_hdr.ymin)*linebytes ;
    switch( input_type )
    {
      case GREY:
	r = scan[RLE_RED] ;
	for(col=ncol; --col >= 0; )
	  *optr++ = *r++ ;
	break ;

      case GREY_ALPHA:
	a = scan[RLE_ALPHA] ;
	r = scan[RLE_RED] ;
	for(col=ncol; --col >= 0; )
	{
	  *optr++ = *a++ ;
	  *optr++ = *optr++ = *optr++ = *r++ ;
	}
	break ;

      case COLOR:
	r = scan[RLE_RED] ;
	g = scan[RLE_GREEN] ;
	b = scan[RLE_BLUE] ;
	for(col=ncol; --col >= 0; )
	{
	  *optr++ = *r++ ;
	  *optr++ = *g++ ;
	  *optr++ = *b++ ;
	}
	break ;

      case COLOR_ALPHA:
	a = scan[RLE_ALPHA] ;
	r = scan[RLE_RED] ;
	g = scan[RLE_GREEN] ;
	b = scan[RLE_BLUE] ;
	for(col=ncol; --col >= 0; )
	{
	  *optr++ = *a++ ;
	  *optr++ = *r++ ;
	  *optr++ = *g++ ;
	  *optr++ = *b++ ;
	}
	break ;
    }
  }
  fwrite(image, linebytes * header.ras_height, 1, outfile) ;

  exit( 0 );
}

