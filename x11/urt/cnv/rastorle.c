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
 * rastorle  --
 *   convert Sun Rasterfile to Utah RLE format 
 *   reads Sun rasterfile or stdin and writes stdout.
 *
 * Author:         Berry Kercheval (berry@mordor.s1.gov)
 *                 Lawrence Livermore National Laboratory
 *                 Livermore, CA.
 * Date:           9 March 1987
 * History:  
 *   27 March 1987:  bbk: make it understand depth one (B&W) rasterfiles.
 *   6 September 1990: Clark: make it understand RT_BYTE_ENCODED rasterfiles,
 *			      add flag so alpha channel is done only if desired,
 *                            and output a colormap for images of depth 8.
 *
 * Usage is:
 *   rastorle [-a] [-o outfile.rle] [ infile.ras ]
 * 		(optionally) | rleflip -v > out.rle
 *
 * -a		Fake an alpha channel.
 *
 *   (The flip is necessary because Sun numbers the rows differently.)
 *
 * This will be difficult to compile on a non-sun, as it uses the pixrect
 * library include files.
 */

#include <stdio.h>
#include <pixrect/pixrect_hs.h>
#include <sysexits.h>
#include "rle.h"

#define MAXLINE        1280	/* max width of a line    */
#define MAXVAL          255	/* max value for pixel */
#define CMAPSIZE	256	/* Max size of color map  */
#define ESCAPE		128	/* Escape value for RT_BYTE_ENCODED rasterfiles */
/* #define DEBUG                /* debug output turned on */

int getbit();

static rle_map out_map[3*(1<<8)];

unsigned char *outrows[4];	/* array of rows for RLE output */
unsigned char redline[MAXLINE], /* rle red row values */
              grnline[MAXLINE],
	      bluline[MAXLINE], 
	      alfline[MAXLINE];	/* alpha channel values */

unsigned char red[CMAPSIZE],	/* red colormap entries */
            green[CMAPSIZE],	/* Green ditto */
	    blue[CMAPSIZE];	/* see a pattern? */

void
main(argc, argv)
int argc;
char *argv[];
{
    char	     *outfname = NULL;
    char	     *infname = NULL;
    int		      flag = 0, aflag = 0, oflag = 0;
    FILE             *rasfile;	/* where input comes from */
    FILE             *outfile;	/* where output goes to */
    int               i;	/* useful index */
    int               p;	/* pixel value read from a file -- 
				 * used to index rasterfile color map. */
    int               count;    /* Holds current byte count for rasterfiles
				 * of type RT_BYTE_ENCODED */
    int               h;	/* index for looping along a row */
    struct rasterfile rashdr;	/* standard header of a Sun rasterfile*/
    
    if ( scanargs( argc, argv, "% a%- o%-outfile!s infile%s",
		   &aflag, &oflag, &outfname, &infname ) == 0 )
	exit( EX_USAGE );

    rasfile = rle_open_f( "rastorle", infname, "r" );
    outfile = rle_open_f( "rastorle", outfname, "w" );

    rle_addhist( argv, (rle_hdr *)NULL, &rle_dflt_hdr );

    /* first read the rasterfile header */
    if(fread(&rashdr, sizeof(rashdr), 1, rasfile) != 1)
    {
	fprintf(stderr, "Can't read rasterfile header.\n");
	exit(EX_DATAERR);
    }
    /* it has to start with the magic number... */
    if (rashdr.ras_magic != RAS_MAGIC)
    {
	fprintf(stderr, "Error: \"%s\" is not a rasterfile.\n", 
		rasfile==stdin?"stdin":argv[1]);
	exit(EX_DATAERR);
    }
#ifdef DEBUG
    fprintf (stderr, "rasterfile width     =  %d\n", rashdr.ras_width);
    fprintf (stderr, "rasterfile height    =  %d\n", rashdr.ras_height);
    fprintf (stderr, "rasterfile depth     =  %d\n", rashdr.ras_depth);
    fprintf (stderr, "rasterfile length    =  %d\n", rashdr.ras_length);
    fprintf (stderr, "rasterfile type      =  %d\n", rashdr.ras_type);
    fprintf (stderr, "rasterfile maplength =  %d\n", rashdr.ras_maplength);
#endif

    /* read in color map */
    switch(rashdr.ras_maptype)
    {
      case RMT_NONE:
#ifdef DEBUG
	fprintf (stderr, "No color map\n");
#endif
	for (i = 0; i < 256; i++)
	  red[i] = green[i] = blue[i] = i ;
	break;
      case RMT_RAW:
#ifdef DEBUG
	fprintf (stderr, "Raw color map\n");
#endif
	for (i = 0; i < 256; i++)
	  red[i] = green[i] = blue[i] = i ;
	for (i = 0; i < rashdr.ras_maplength; i++)
	  getc(rasfile);
	break;
      case RMT_EQUAL_RGB:
#ifdef DEBUG
	fprintf (stderr, "RGB color map\n");
#endif
	/* read red */
	for (i = 0; i < rashdr.ras_maplength/3; i++)
	  red[i] = getc(rasfile);
	/* read green */
	for (i = 0; i < rashdr.ras_maplength/3; i++)
	  green[i] = getc(rasfile);
	/* read blue */
	for (i = 0; i < rashdr.ras_maplength/3; i++)
	  blue[i] = getc(rasfile);
	break;
      default:
	fprintf (stderr, "Unknown color map type (%d)\n", rashdr.ras_maptype);
	exit (EX_DATAERR);
    }
    /* we need to work on this... */
    switch(rashdr.ras_depth)
    {
      default:
	fprintf(stderr,
	  "Sorry, I can't deal with a rasterfile depth %d\n",rashdr.ras_depth);
	break;

      case 1:			/* black & white */
	/* next few lines stolen from painttorle.c */
	RLE_SET_BIT(rle_dflt_hdr, RLE_RED);
	RLE_SET_BIT(rle_dflt_hdr, RLE_GREEN);
	RLE_SET_BIT(rle_dflt_hdr, RLE_BLUE);
	if ( aflag )
            RLE_SET_BIT(rle_dflt_hdr, RLE_ALPHA );
	else
            RLE_CLR_BIT(rle_dflt_hdr, RLE_ALPHA );
	rle_dflt_hdr.rle_file = outfile;
	rle_dflt_hdr.xmin = 0 ;
	rle_dflt_hdr.xmax = rashdr.ras_width-1;
	rle_dflt_hdr.ymin = 0 ;
	rle_dflt_hdr.ymax = rashdr.ras_height-1;
	rle_dflt_hdr.ncmap = 0 ;
	if ( aflag )
            rle_dflt_hdr.alpha = 1;
        else 
            rle_dflt_hdr.alpha = 0;
	outrows[0] = alfline;
	/* all three can have the same value since it's B&W */
	outrows[1] = redline;
	outrows[2] = redline;
	outrows[3] = redline;
	
	rle_put_setup( &rle_dflt_hdr );
	for (i=0; i<rashdr.ras_height; i++) 
	{
	    for(h = 0; h < rashdr.ras_width; h++)
	    {
		p = getbit(rasfile, 0);
		redline[h] = p?0:MAXVAL;
		if ( aflag )
		    alfline[h] = p?0:MAXVAL;
	    }
	    /* write a line to the rle file */
	    rle_putrow(&outrows[1], rashdr.ras_width, &rle_dflt_hdr);
	    getbit(NULL, 1);
	}
	break; /* end case 1: */

      case 8:
      case 24:
      case 32:
	/* next few lines stolen from painttorle.c */
	RLE_SET_BIT(rle_dflt_hdr, RLE_RED);
	RLE_SET_BIT(rle_dflt_hdr, RLE_GREEN);
	RLE_SET_BIT(rle_dflt_hdr, RLE_BLUE);
	if ( aflag || rashdr.ras_depth == 32 )
            RLE_SET_BIT(rle_dflt_hdr, RLE_ALPHA );
        else 
            RLE_CLR_BIT(rle_dflt_hdr, RLE_ALPHA );
	rle_dflt_hdr.rle_file = outfile;
	rle_dflt_hdr.xmin = 0 ;
	rle_dflt_hdr.xmax = rashdr.ras_width-1;
	rle_dflt_hdr.ymin = 0 ;
	rle_dflt_hdr.ymax = rashdr.ras_height-1;
	if ( aflag || rashdr.ras_depth == 32 )
            rle_dflt_hdr.alpha = 1;
        else 
            rle_dflt_hdr.alpha = 0;
	if (rashdr.ras_depth == 8)
	{
	    rle_dflt_hdr.ncolors =  1;
	    rle_dflt_hdr.ncmap = 3;
	    rle_dflt_hdr.cmaplen = 8;
	    rle_dflt_hdr.cmap = out_map;
	    for (i=0;i<(1<<8);i++)
	    {
        	out_map[i+(0<<8)] = red[i] << 8;
        	out_map[i+(1<<8)] = green[i] << 8;
        	out_map[i+(2<<8)] = blue[i] << 8;
	    }
        }
 	
	outrows[0] = alfline;
	outrows[1] = redline;
	outrows[2] = grnline;
	outrows[3] = bluline;
	
	rle_put_setup( &rle_dflt_hdr );
	/* now just loop through the rows */
	
	if (rashdr.ras_type == RT_BYTE_ENCODED)
	    count = 9999;
	for (i=0; i<rashdr.ras_height; i++)
	{
	    /* read a line from the rasterfile */
	    switch(rashdr.ras_depth)
	    {
	      case 8:
		for (h=0; h < rashdr.ras_width; h++)
		{
		    if (rashdr.ras_type != RT_BYTE_ENCODED)
			p = getc(rasfile);
		    else
		    {
			if (count == 9999)
			{
			    p = getc(rasfile);
			    if (p == ESCAPE)
			    {
				count = getc(rasfile);
				if (count == 0)
				    count = 9999;
				else
				    p = getc(rasfile);
			    }
			} 
			else
			{
			    if (--count == 0)
				count = 9999;
			}
		    }
		    redline[h] = p;
		    grnline[h] = p;
		    bluline[h] = p;
		    /* fake up an alpha channel */
		    if ( aflag || rashdr.ras_depth == 32 )
		    {
            		if (redline[h] || grnline[h] || bluline[h])
		  	    alfline[h] = 255;
			else
			    alfline[h] = 0;
		    }
		}
		/* Since records are an even number of bytes */
		if (rashdr.ras_width & 1) p = getc(rasfile);
		break ;

	      case 24:
		for (h=0; h < rashdr.ras_width; h++)
		{
		    register int r,g,b ;
		    r = getc(rasfile);
		    g = getc(rasfile);
		    b = getc(rasfile);
		    redline[h] = red[r];
		    grnline[h] = green[g];
		    bluline[h] = blue[b];
		    /* fake up an alpha channel */
		    if ( aflag )
		    {
            		if (redline[h] || grnline[h] || bluline[h])
		  	    alfline[h] = 255;
			else
			    alfline[h] = 0;
		    }
		}
		break ;

	      case 32:
		for (h=0; h < rashdr.ras_width; h++)
		{
		    register int r,g,b,a ;
		    a = getc(rasfile);
		    r = getc(rasfile);
		    g = getc(rasfile);
		    b = getc(rasfile);
		    redline[h] = red[r];
		    grnline[h] = green[g];
		    bluline[h] = blue[b];
		    alfline[h] = a;
		}
		break ;
	    }
	    /* write a line to the rle file */
	    rle_putrow(&outrows[1], rashdr.ras_width, &rle_dflt_hdr);
	}
	break; /* end case 8: */
    } /* end switch */
}

/*
 * get a bit from the file fp.  Read a byte and return its bits
 * one at a time.  (actually returns zero or non-zero).
 */
int
getbit(fp, force)
FILE *fp;
int force;			/* true if we should read a byte anyway */
{
    static int c;		/* the char we are picking apart */
    static unsigned int mask
      = 0x0;			/* mask to get the next bit -- init to huge
				 * so that we get a byte the first time */
    int val;			/* value to be returned */
    if (force)			/* read a new byte next time */
    {
	mask = 0x0;
	return 0;
    }
    if(mask == 0)		/* time to get the next byte */
    {
	c = getc(fp);
	mask = 0x80;		/* reset the mask */
    }
    val = c & mask;		/* true if this bit on */
    mask >>= 1;			/* shift mask over one bit */
    return val;			/* the bit we saved goes back to caller */
}
