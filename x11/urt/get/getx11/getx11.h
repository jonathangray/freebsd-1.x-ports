/*
 * This software is copyrighted as noted below.  It may be freely copied,
 * modified, and redistributed, provided that the copyright notices are 
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
 * getx11.h - Declaration for image_information structure...  (tote a global)
 * 
 * Author:	Martin R. Friedmann 
 * 		Dept of Electrical Engineering and Computer Science
 *		University of Michigan
 * Date:	Tue, Dec 10, 1989
 * Copyright (c) 1989, University of Michigan
 */

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <rle.h>

#define COUNT_OF(_array_)	( sizeof (_array_) / sizeof (_array_[0]) )
#define IMAGE_BORDERWIDTH	3

#ifdef USE_STDLIB_H
#include <stdlib.h>
#else

#ifdef USE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

extern char	*getenv ();
#ifndef VOID_STAR
extern char	*malloc (), *realloc();
#else
extern void *malloc(), *realloc();
#endif
extern void free();

#endif /* USE_STDLIB_H */

typedef int Boolean;
typedef unsigned int Pixel;

#define MALLOC_FAILURE 3
#define FILE_FAILURE 2
#define FATAL_FAILURE 1
#define SUCCESS 0

#define VPRINTF if (verbose_flag) fprintf
#define DPRINTF if (debug_flag) fprintf

#define SHIFT_MASK_PIXEL(r, g, b) \
    (( ( (r) << red_shift  ) & red_mask )\
    | ( ( (g) << green_shift) & green_mask )\
    | ( ( (b) << blue_shift ) & blue_mask ))

#define SHIFT_MASK_PIXEL_32(r, g, b) \
    (( (r) << red_shift  ) | ( (g) << green_shift) | ( (b) << blue_shift ))


typedef void VOID_FUNCTION(); 
typedef int array16[16];

extern double		display_gamma;
extern int 		iflag;

extern char 		*progname;
extern Display 		*dpy;
extern Window		root_window;
extern int		screen;

extern Boolean		debug_flag;	/* set if debug mode -D */
extern Boolean		verbose_flag;	/* set if verbose mode -v */
extern int 		stingy_flag;
extern int              specified_levels;

/* X11/NeWS server bug workaround. */
extern int no_color_ref_counts;

/* 
 * Color map, gamma correction map, and lookup tables 
 */

extern int		red_shift;
extern int		green_shift;
extern int		blue_shift;
extern Pixel		red_mask;
extern Pixel		green_mask;
extern Pixel		blue_mask;
extern Pixel		pixel_base;
extern Pixel		black_pixel;
extern Pixel		white_pixel;

typedef struct _image_info_struct
{
    Window window, icn_window;	/* X windows and pixmaps */
    Window pix_info_window;
    Pixmap pixmap, icn_pixmap, mag_pixmap, refresh_pixmap;
    GC gc, icn_gc;			/* And GC's and XImages */
    XImage *image, *icn_image;
    Colormap colormap;
    int visual_class;
    Visual *dpy_visual;
    int dpy_depth;
    Boolean pixmap_failed;

    CONST_DECL char *filename;		/* file that Image came from.  */
    char *title;			/* title for this image...     */
    int   img_num;			/* Number of image within file. */
    FILE *fd;
    unsigned char *scan_data;           /* a[img_h][img_w][img_clr_channels] */
    int img_channels;			/* # of color channels in file       */
    int dpy_channels;			/* # of channels we will display     */
    VOID_FUNCTION *map_scanline;	/* map_scanline routine to use       */
    VOID_FUNCTION *MAG_scanline;	/* MAG_scanline routine to use       */
    float gamma;
    float dpy_gamma;
    
    int x, y;				/* Original origin of image	     */
    int w, h;				/* width and height of image         */
    int icn_w, icn_h;			/* width and height of icon          */
    int icn_factor;			/* divide factor from img -> icon    */

    int mag_x, mag_y, mag_w, mag_h;	/* image rect currently being viewed */
    int mag_fact, save_mag_fact;	/* current magnification factor      */
    Boolean mag_mode;			/* are we display magnified image?   */
    int save_mag_x, save_mag_y;
    int save_mag_w, save_mag_h;
    
    Boolean binary_img;			/* will we make it 2 color? (-W)     */
    Boolean mono_img;			/* do we make it grey scale? (-w)    */
    Boolean dither_img;			/* do we dither it? (-a)             */
    Boolean rw_cmap;			/* is the colormap writable?         */
    Boolean sep_colors;			/* is the visual True or DirectColor?*/
    Boolean mono_color;			/* a one channel color image (mcut)  */
    Boolean color_dpy;			/* False if we are FORCED to b/w     */

    rle_pixel **in_cmap;
    int ncmap;				/* rle_hdr.ncmap 		     */
    int cmlen;				/* Comment `color_map_length` in file*/

    int *modN;				/* dither arrays, all of them */
    int *divN;
    array16 *dm16;			
    Pixel *pixel_table;
    int lvls, lvls_squared;
} image_information;

/* pointer arithmetic... gack!  */
/* Returns Y'th row in our saved data array.  Works around problem with */
/* rle_getrow by adding 1 to y.  We waste the first line of this array  */
/* SAVED_RLE_ROW(img, -1) == img->scan_data, and is never used...       */
#define SAVED_RLE_ROW( img, y ) \
    ((img)->scan_data + (((y) + 1) * (img)->w * (img)->dpy_channels)) 


#define duff8(counter, block) {\
  while (counter >= 8) {\
     { block; } \
     { block; } \
     { block; } \
     { block; } \
     { block; } \
     { block; } \
     { block; } \
     { block; } \
     counter -= 8;\
  } \
  switch (counter & 7) { \
     case 7:    { block; } \
     case 6:    { block; } \
     case 5:    { block; } \
     case 4:    { block; } \
     case 3:    { block; } \
     case 2:    { block; } \
     case 1:    { block; } \
     case 0:    counter = 0;\
     }\
}
