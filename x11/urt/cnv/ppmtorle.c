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
 * ppmtorle - A program which will convert pbmplus/ppm images
 *            to Utah's "rle" image format.
 *
 * Author:      Wesley C. Barris
 *              AHPCRC
 *              Minnesota Supercomputer Center, Inc.
 * Date:        Fri July 20 1990
 * Copyright (c) Wesley C. Barris
 */
/*-----------------------------------------------------
 * System includes.
 */
#include <stdio.h>
#include <ppm.h>
#include <rle.h>

#ifdef USE_STDLIB_H
#include <stdlib.h>
#else

#ifdef USE_STRING_H
#include <string.h>
#else
#include <strings.h>
#define strrchr rindex
#endif

#ifdef VOID_STAR
extern void *malloc();
#else
extern char *malloc();
#endif
extern void free();

#endif /* USE_STDLIB_H */

#define VPRINTF if (verbose || header) fprintf

typedef unsigned char U_CHAR;
/*
 * Global variables.
 */
FILE	*fp;
int	format;
int	width, height;
int	verbose = 0, header = 0, do_alpha = 0;
gray	maxval;
/*-----------------------------------------------------------------------------
 *                                        Read the Wavefront image file header.
 */
void read_ppm_header()
{
   ppm_readppminit(fp, &width, &height, &maxval, &format);
   VPRINTF(stderr, "Image type: 24 bit true color\n");
   VPRINTF(stderr, "Full image: %dx%d\n", width, height);
   VPRINTF(stderr, "Maxval:     %d\n", maxval);
   if (do_alpha)
      VPRINTF(stderr, "Computing alpha channel...\n");
}
/*-----------------------------------------------------------------------------
 *                                             Write the rle image file header.
 */
void write_rle_header()
{
   rle_dflt_hdr.xmin    = 0;
   rle_dflt_hdr.xmax    = width-1;
   rle_dflt_hdr.ymin    = 0;
   rle_dflt_hdr.ymax    = height-1;
   rle_dflt_hdr.ncolors = 3;
   rle_dflt_hdr.background = 0;
   if (do_alpha) {
      rle_dflt_hdr.alpha = 1;
      RLE_SET_BIT(rle_dflt_hdr, RLE_ALPHA);
      }
   RLE_SET_BIT(rle_dflt_hdr, RLE_RED);
   RLE_SET_BIT(rle_dflt_hdr, RLE_GREEN);
   RLE_SET_BIT(rle_dflt_hdr, RLE_BLUE);
   rle_put_setup(&rle_dflt_hdr);
}
/*-----------------------------------------------------------------------------
 *                                      Write the rle data portion of the file.
 */
void write_rle_data()
{
   register int		x;
   register int		scan;
   register pixel	*pixelrow, *pP;
   rle_pixel		**scanline;
/*
 * Allocate some memory.
 */
   pixelrow = ppm_allocrow(width);
   if (rle_row_alloc(&rle_dflt_hdr, &scanline)) {
      fprintf(stderr, "Unable to malloc space for pixels\n");
      exit(-1);
      }
/*
 * Loop through the ppm files image window, write blank lines outside
 * active window.
 */
   for (scan = 0; scan < height; scan++) {
      ppm_readppmrow(fp, pixelrow, width, maxval, format);
      for (x = 0, pP = pixelrow; x < width; x++, pP++) {
         scanline[RLE_RED][x]   = PPM_GETR(*pP);
         scanline[RLE_GREEN][x] = PPM_GETG(*pP);
         scanline[RLE_BLUE][x]  = PPM_GETB(*pP);
         if (do_alpha) {
            scanline[RLE_ALPHA][x] = (scanline[RLE_RED][x] ||
                                     scanline[RLE_GREEN][x] ||
                                     scanline[RLE_BLUE][x] ? 255 : 0);
            }
         }
      rle_putrow(scanline, width, &rle_dflt_hdr);
      }
   VPRINTF(stderr, "Done -- write oef to RLE data.\n");
   rle_puteof(&rle_dflt_hdr);
}
/*-----------------------------------------------------------------------------
 *                      Convert an Wavefront image file into an rle image file.
 */
int
main(argc, argv)
int argc;
char **argv;
{
   char		*periodP, *ppmname = NULL, *outname = NULL;
   static char	filename[BUFSIZ];
   int		oflag;
/*
 * Get those options.
 */
   if (!scanargs(argc,argv,
       "% v%- h%- a%- o%-outfile!s infile.ppm%s",
       &verbose,
       &header,
       &do_alpha,
       &oflag, &outname,
       &ppmname))
      exit(-1);
/*
 * Open the file.
 */
   if (ppmname == NULL) {
      strcpy(filename, "stdin");
      fp = stdin;
      }
   else {
      periodP = strrchr(ppmname, '.');
      strcpy(filename, ppmname);
      if (periodP) {
         if (strcmp(periodP, ".ppm")) /* does not end in ppm */
            strcat(filename, ".ppm");
         }
      else				/* no ext -- add one */
         strcat(filename, ".ppm");
      if (!(fp = fopen(filename, "r"))) {
         fprintf(stderr, "Cannot open %s for reading.\n", filename);
         exit(-1);
         }
      }
/*
 * Read the Wavefront file file header.
 */
   read_ppm_header();
   if (header)
      exit(0);
/*
 * Write the rle file header.
 */
   rle_dflt_hdr.rle_file = rle_open_f( cmd_name( argv ), outname, "w" );
   rle_addhist(argv, (rle_hdr *)NULL, &rle_dflt_hdr);
   write_rle_header();
/*
 * Write the rle file data.
 */
   write_rle_data();
   fclose(fp);

   return 0;
}
