/* tifftorle.c */
/* main program for converting 24-bit TIFF files to 24-bit
 * Utah Raster Toolkit RLE files.
 * by Bailey Brown, Jr.  21 June 1990
 */
 
 
#include "rle.h"
#undef TIFF			/* Defined by rle_config.h. */
#include <stdio.h>
#include "tiffio.h"
#ifndef USE_STRING_H
#include <strings.h>
#else
#include <string.h>
#endif

#ifdef USE_PROTOTYPES
void error(CONST_DECL char *s);
#else
void error();
#endif
void get_scanlines();

static rle_hdr the_hdr;
TIFF *tif;
unsigned char *tiffbuf;
/* needed to read all scanlines before converting
        to tiff (rle is upside down) */
static rle_pixel **scan_red;
static rle_pixel **scan_green;
static rle_pixel **scan_blue;
 
int flip;
char * my_name = NULL;
 
void
main(argc, argv)
int argc;
char *argv[];
{
        unsigned short imagelength, imagewidth, shortval;
        int row, i;
        rle_pixel *rows[4];
	char *infname = NULL, *outfname = NULL;
 
        flip = 0;
	my_name = cmd_name( argv );
	if ( scanargs( argc, argv, "% o%-outfile.rle!s infile.tiff!s",
		       &i, &outfname, &infname ) == 0 )
	    exit( 1 );
        tif = TIFFOpen(infname, "rb");
        if (!tif) error("can't open input file");
        the_hdr.rle_file = rle_open_f(cmd_name(argv), outfname, "w");
        TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &shortval);
        if (shortval != 2) error("tiff file must be 24-bit RGB");
        TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &shortval );
        if (shortval != 1) error("can't handle planar config");
        TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &imagewidth);
        TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &imagelength);
 
        RLE_SET_BIT(the_hdr, RLE_RED);
        RLE_SET_BIT(the_hdr, RLE_GREEN);
        RLE_SET_BIT(the_hdr, RLE_BLUE);
        the_hdr.ncolors =  3;
 
        the_hdr.bg_color = NULL;
        the_hdr.alpha = 0;
        the_hdr.background = 0;
        the_hdr.xmin = 0;
        the_hdr.xmax = imagewidth - 1;
        the_hdr.ymin = 0;
        the_hdr.ymax = imagelength - 1;
        the_hdr.ncmap = 0;
        the_hdr.cmaplen = 0;
        the_hdr.cmap = 0;
 
	rle_addhist( argv, (rle_hdr *)NULL, &the_hdr );
        rle_put_setup( &the_hdr );
 
        tiffbuf = (unsigned char*)malloc(TIFFScanlineSize(tif));
        if (!tiffbuf) error("can't allocate tiff scanline buffer");
        get_scanlines();
        rows[0] = NULL;
        for ( row = 0; row < the_hdr.ymax + 1; row++) {
            rows[1] = scan_red[row];
            rows[2] = scan_green[row];
            rows[3] = scan_blue[row];
            rle_putrow(rows+1, the_hdr.xmax+1, &the_hdr);
        }
}
 
void 
get_scanlines()
{
    int i,j,k,n;
 
    scan_red = (rle_pixel**)malloc(sizeof(rle_pixel*)*(the_hdr.ymax+1));
    scan_green = (rle_pixel**)malloc(sizeof(rle_pixel*)*(the_hdr.ymax+1));
    scan_blue = (rle_pixel**)malloc(sizeof(rle_pixel*)*(the_hdr.ymax+1));
    if (!(scan_red && scan_green && scan_blue))
        error("can't allocate a scan buffer");
    if (flip) {
         for (i = 0; i < the_hdr.ymax+1; i++) {
              TIFFReadScanline(tif, tiffbuf, i, 0);
              scan_red[i] = (unsigned char*)malloc(the_hdr.xmax+1);
              scan_green[i] = (unsigned char*)malloc(the_hdr.xmax+1);
              scan_blue[i] = (unsigned char*)malloc(the_hdr.xmax+1);
              if (!(scan_red[i] && scan_green[i] && scan_blue[i]))
              error("can't allocate scan buffer");
              for (k = j = 0; j < the_hdr.xmax+1; k +=3, j++) {
                   scan_red[i][j] = tiffbuf[k];
                   scan_green[i][j] = tiffbuf[k+1];
                   scan_blue[i][j] = tiffbuf[k+2];
              }
         }
    } else {
        for (i = 0; i < the_hdr.ymax+1; i++) {
              n = the_hdr.ymax - i;
              TIFFReadScanline(tif, tiffbuf, i, 0);
              scan_red[n] = (unsigned char*)malloc(the_hdr.xmax+1);
              scan_green[n] = (unsigned char*)malloc(the_hdr.xmax+1);
              scan_blue[n] = (unsigned char*)malloc(the_hdr.xmax+1);
              if (!(scan_red[n] && scan_green[n] && scan_blue[n]))
              error("can't allocate scan buffer");
              for (k = j = 0; j < the_hdr.xmax+1; k+= 3, j++) {
                   scan_red[n][j] = tiffbuf[k];
                   scan_green[n][j] = tiffbuf[k+1];
                   scan_blue[n][j] = tiffbuf[k+2];
              }
         }
    }
}
 
void 
error(s)
CONST_DECL char *s;
{
    fprintf(stderr,"%s: %s\n", my_name, s);
    exit(2);
}
