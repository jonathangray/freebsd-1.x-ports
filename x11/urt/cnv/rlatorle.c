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
 * rlatorle - A program which will convert Wavefront's "rlb" images
 *            to Utah's "rle" image format.
 *
 * Author:      Wesley C. Barris
 *              AHPCRC
 *              Minnesota Supercomputer Center, Inc.
 * Date:        Thu June 20 1990
 * Copyright (c) Wesley C. Barris
 */
/*-----------------------------------------------------------------------------
 * System includes.
 */
#include <stdio.h>
#include "rlb_header.h"
#include "rle.h"

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
RLB_HEADER	rlb_head;
/*static rle_map	color_map[3*(1<<8)];*/
/*int		maplen = 0;*/
int		scan;
FILE		*fp;
int		act_x_res, act_y_res;
int		width, height;
int		verbose = 0, header = 0, do_matte = 0;
/*-----------------------------------------------------------------------------
 *                                        Read the Wavefront image file header.
 */
void read_rlb_header(act_x_res, act_y_res, width, height)
int *act_x_res;
int *act_y_res;
int *width;
int *height;
{
   if (fread(&rlb_head, 740, 1, fp) != 1) {
      fprintf(stderr, "Error reading rlb file header!\n");
      exit(-2);
      }
   *act_x_res = rlb_head.active_window.right-rlb_head.active_window.left+1;
   *act_y_res = rlb_head.active_window.top-rlb_head.active_window.bottom+1;
   *width     = rlb_head.window.right-rlb_head.window.left+1;
   *height    = rlb_head.window.top-rlb_head.window.bottom+1;
   VPRINTF(stderr, "Full image:         %dx%d\n", *width, *height);
   VPRINTF(stderr, "Active window:      %dx%d\n", *act_x_res, *act_y_res);
   VPRINTF(stderr, "Number of channels: %d\n", rlb_head.num_chan);
   VPRINTF(stderr, "Number of mattes:   %d\n", rlb_head.num_matte);
   VPRINTF(stderr, "Image gamma:        %s\n", rlb_head.gamma);
   VPRINTF(stderr, "Original filename:  %s\n", rlb_head.name);
   VPRINTF(stderr, "Description:        %s\n", rlb_head.desc);
   VPRINTF(stderr, "Machine:            %s\n", rlb_head.machine);
   VPRINTF(stderr, "User:               %s\n", rlb_head.user);
   VPRINTF(stderr, "Date:               %s\n", rlb_head.date);
   VPRINTF(stderr, "Aspect:             %s\n", rlb_head.aspect);
   VPRINTF(stderr, "Aspect ratio:       %s\n", rlb_head.aspect_ratio);
   VPRINTF(stderr, "Channel color space %s\n", rlb_head.chan);
   VPRINTF(stderr, "Interlaced?         %d\n", rlb_head.filter_type);
   if (do_matte)
      VPRINTF(stderr, "Converting matte channel only...\n");
}
/*-----------------------------------------------------------------------------
 *                                             Write the rle image file header.
 */
void write_rle_header()
{
   rle_dflt_hdr.xmin    = rlb_head.window.left;
   rle_dflt_hdr.xmax    = rlb_head.window.right;
   rle_dflt_hdr.ymin    = rlb_head.window.bottom;
   rle_dflt_hdr.ymax    = rlb_head.window.top;
   rle_dflt_hdr.alpha   = (rlb_head.num_matte && !do_matte ? 1 : 0);
   rle_dflt_hdr.ncolors = (do_matte ? 1 : rlb_head.num_chan);
   /*rle_dflt_hdr.ncmap   = (map ? 3 : 0);*/
   /*rle_dflt_hdr.cmaplen = (map ? 8 : 0);*/
   /*rle_dflt_hdr.cmap    = (map ? color_map : NULL);*/
   rle_putcom(rlb_head.desc, &rle_dflt_hdr);
   rle_dflt_hdr.rle_file    = stdout;
   rle_dflt_hdr.background = 0;
   if (rle_dflt_hdr.alpha)
      RLE_SET_BIT(rle_dflt_hdr, RLE_ALPHA);
   if (!do_matte) {
      RLE_SET_BIT(rle_dflt_hdr, RLE_RED);
      RLE_SET_BIT(rle_dflt_hdr, RLE_GREEN);
      RLE_SET_BIT(rle_dflt_hdr, RLE_BLUE);
      }
   rle_put_setup(&rle_dflt_hdr);
}
/*-----------------------------------------------------------------------------
 *                                        Add a color triplet to the color map.
 */
/*int add_to_map(red, green, blue)
U_CHAR red;
U_CHAR green;
U_CHAR blue;
{
   int i;

   for (i = 0; i <maplen; i++)
      if (  red == color_map[i] >> 8 &&
          green == color_map[i+(1<<8)] >> 8 &&
           blue == color_map[i+(2<<8)] >> 8)
         return i;*/
/*
 * This color was not found in the map -- add it.
 */
   /*if (maplen > 255) {
      fprintf(stderr, "The upper limit of 256 colors has been surpassed at scan line number %d!\n", scan);
      fprintf(stderr, "A color map cannot be created for this image.\n");
      exit(-1);
      }
   color_map[maplen]        = red   << 8;
   color_map[maplen+(1<<8)] = green << 8;
   color_map[maplen+(2<<8)] = blue  << 8;
   return maplen++;
}*/
/*-----------------------------------------------------------------------------
 *                                            Decode run length encoded pixels.
 */
void
decode(c_in, c_out, len)
U_CHAR	*c_in;
U_CHAR	*c_out;
int	len;
{
   int	ct;

   while(len > 0) {
      ct = *c_in++;
      len--;
      if (ct < 128) {
/*
 * Repeat pixel value ct+1 times.
 */
         while (ct-- >= 0)
            *c_out++ = *c_in;
         c_in++;
         len--;
         }
      else {
/*
 * Copy ct unencoded values.
 */
         for (ct = 256-ct; ct-- > 0; len--)
            *c_out++ = *c_in++;
         }
      }
}
/*-----------------------------------------------------------------------------
 *                                      Write the rle data portion of the file.
 */
void write_rle_data()
{
   int		*offset;
   int		x;
   int		bottom;
   int		left;
   int		scan;
   /*U_CHAR	*blank;*/
   U_CHAR	*red, *green, *blue, *matte;
   U_CHAR	*buf;
   short	len;
   rle_pixel	*scanline[4];
/*
 * Read scanline offset table.
 */
   if (!(offset = (int *)malloc(sizeof(int) * act_y_res))) {
      fprintf(stderr, "Offset malloc failed!\n");
      exit(-3);
      }
   if (fread(offset, sizeof(int), act_y_res, fp) != act_y_res) {
      fprintf(stderr, "Offset table read failed!\n");
      exit(-4);
      }
/*
 * Allocate some memory.
 */
   /*if (!(blank = (U_CHAR *)malloc(sizeof(U_CHAR) * width * 4))) {
      fprintf(stderr, "Unsigned scanline malloc failed!\n");
      exit(-6);
      }*/
   if (!(buf = (U_CHAR *)malloc(sizeof(U_CHAR) * width * 2))) {
      fprintf(stderr, "Buf malloc failed!\n");
      exit(-7);
      }
   if (!(red = (U_CHAR *)malloc(sizeof(U_CHAR) * width * 4))) {
      fprintf(stderr, "Red scanline malloc failed!\n");
      exit(-8);
      }
   green = &red[width];
   blue  = &green[width];
   matte = &blue[width];
   /*bzero((char *)blank, width*4);*/
   if (((scanline[0]=(rle_pixel *)malloc(width*sizeof(rle_pixel)))==NULL) ||
       ((scanline[1]=(rle_pixel *)malloc(width*sizeof(rle_pixel)))==NULL) ||
       ((scanline[2]=(rle_pixel *)malloc(width*sizeof(rle_pixel)))==NULL) ||
       ((scanline[3]=(rle_pixel *)malloc(width*sizeof(rle_pixel)))==NULL)) {
      fprintf(stderr, "Unable to malloc space for pixels\n");
      exit(-1);
      }
/*
 * Loop through the rla files image window, write blank lines outside
 * active window.
 */
   bottom = rlb_head.active_window.bottom;
   left   = rlb_head.active_window.left;
   for (scan = rlb_head.window.bottom; scan <= rlb_head.window.top; scan++) {
/*
 * Check for black regions outside active window.
 */
      if ((scan < rlb_head.active_window.bottom) ||
          (scan > rlb_head.active_window.top))
         rle_skiprow(&rle_dflt_hdr, 1);
      else {
         if (fseek(fp, (long)offset[scan-bottom], 0)) {
            fprintf(stderr, "rlb file incomplete!\n");
            exit(-9);
            }
/*
 * Red scanline.
 */
         fread(&len, sizeof(short), 1, fp);
         fread(buf, sizeof(U_CHAR), (int)len, fp);
         decode(buf, red, (int)len);
/*
 * Green scanline.
 */
         fread(&len, sizeof(short), 1, fp);
         fread(buf, sizeof(U_CHAR), (int)len, fp);
         decode(buf, green, (int)len);
/*
 * Blue scanline.
 */
         fread(&len, sizeof(short), 1, fp);
         fread(buf, sizeof(U_CHAR), (int)len, fp);
         decode(buf, blue, (int)len);
/*
 * Matte scanline.
 */
         fread(&len, sizeof(short), 1, fp);
         fread(buf, sizeof(U_CHAR), (int)len, fp);
         decode(buf, matte, (int)len);
/*
 * Write out RGBM for each pixel.
 */
         for (x=rlb_head.window.left; x<=rlb_head.window.right; x++) {
            if ((x < rlb_head.active_window.left) ||
                (x > rlb_head.active_window.right)) {
               scanline[0][x] = 0;
               scanline[1][x] = 0;
               scanline[2][x] = 0;
               scanline[3][x] = 0;
               }
            else
               if (do_matte) {
                  /*scanline[1][x] = add_to_map(red[x-left],
                                              green[x-left],
                                              blue[x-left]);*/
                  scanline[0][x] = (red[x-left] ||
                                  green[x-left] ||
                                   blue[x-left] ? 255 : 0);
                  }
               else {
                  scanline[0][x] = matte[x-left];
                  scanline[1][x] = red[x-left];
                  scanline[2][x] = green[x-left];
                  scanline[3][x] = blue[x-left];
                  }
            }
         if (do_matte)
            rle_putrow(scanline, width, &rle_dflt_hdr);
         else
            rle_putrow(scanline + 1, width, &rle_dflt_hdr);
         } /* end of if scan is within active y */
      } /* end of for every scanline */
   VPRINTF(stderr, "Done -- write oef to RLE data.\n");
   rle_puteof(&rle_dflt_hdr);
/*
 * Free up some stuff.
 */
   free(offset);
   /*free(blank);*/
   free(buf);
   free(red);
   free(scanline[0]);
   free(scanline[1]);
   free(scanline[2]);
   free(scanline[3]);
}
/*-----------------------------------------------------------------------------
 *                      Convert an Wavefront image file into an rle image file.
 */
int
main(argc, argv)
int argc;
char **argv;
{
   char		*periodP, *rlaname = NULL, *outname = NULL;
   static char	filename[BUFSIZ];
   int		oflag = 0;
/*
 * Get those options.
 */
   if (!scanargs(argc,argv,
       "% v%- h%- m%- o%-outfile!s infile.rla%s",
       &verbose,
       &header,
       &do_matte,
       &oflag, &outname,
       &rlaname))
      exit(-1);
/*
 * Open the file.
 */
   if (rlaname == NULL) {
      strcpy(filename, "stdin");
      fp = stdin;
      }
   else {
      periodP = strrchr(rlaname, '.');
      strcpy(filename, rlaname);
      if (periodP) {
         if (strcmp(periodP, ".rla")) /* does not end in rla */
            strcat(filename, ".rla");
         }
      else				/* no ext -- add one */
         strcat(filename, ".rla");
      if (!(fp = fopen(filename, "r"))) {
         fprintf(stderr, "Cannot open %s for reading.\n", filename);
         exit(-1);
         }
      }
/*
 * Read the Wavefront file file header.
 */
   read_rlb_header(&act_x_res, &act_y_res, &width, &height);
   if (header)
      exit(0);
/*
 * Write the rle file header.
 */
   rle_dflt_hdr.rle_file = rle_open_f( cmd_name(argv), outname, "w" );
   rle_addhist(argv, (rle_hdr *)NULL, &rle_dflt_hdr);
   write_rle_header();
/*
 * Write the rle file data.
 */
   write_rle_data();
   fclose(fp);

   return 0;
}
