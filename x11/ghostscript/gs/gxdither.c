/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.

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

/* gxdither.c */
#include "gx.h"
#include "gxfixed.h"
#include "gxlum.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"
#include "gzcolor.h"
#include "gzht.h"

/* 
 *	Improved dithering for Ghostscript.  The underlying device imaging 
 *	model supports dithering between two colors to generate intermediate
 *	shades.  
 *	
 *	The strategy is to first see if the color is either pure white or
 *	pure black.  In this case the problem is trivial.
 *
 *	Next, if the device has high quality colors (at least 32 values
 *	per axis), we ask it to map the color directly.
 *
 *	Next, if the device is black and white, or the color happens
 *	to be achromatic, we perform simple B/W dithering.
 *	
 *	Otherwise, things are a bit more complicated.  If the device 
 * 	supports N shades of each R, G and B independently, there are a total 
 *	of N*N*N colors.  These colors form a 3-D grid in a cubical color 
 *	space.  The following dithering technique works by locating the 
 *	color we want in this 3-D color grid and finding the eight colors 
 * 	that surround it.  In the case of dithering into 8 colors with 1 
 *	bit for each red, green and blue, these eight colors will always 
 *	be the same.
 *
 *	Now we consider all possible diagonal paths between the eight colors
 *	and chose the path that runs closest to our desired color in 3-D
 *	color space.  There are 28 such paths.  Then we find the position
 *	on the path that is closest to our color.
 *
 *	The search is made faster by always reflecting our color into
 *	the bottom octant of the cube and comparing it to 7 paths.
 *	After the best path and the best position on that path are found,
 *	the results are reflected back into the original color space.
 *
 *	NOTE: This code has been tested for B/W and Color imaging with
 *	1, 2, 3 and 8 bits per component.
 *
 *	--- original code by Paul Haeberli @ Silicon Graphics - 1990
 *	--- extensively revised by L. Peter Deutsch, Aladdin Enterprises
 */

extern void gx_color_load(P2(gx_device_color *, gs_state *));

#define	WEIGHT1		(unsigned long)(100)	/* 1.0 			*/
#define	WEIGHT2		(unsigned long)(71)	/* 1/sqrt(2.0) 		*/
#define	WEIGHT3		(unsigned long)(62)	/* 1/sqrt(3.0)+tad 	*/

#define	DIAG_R		(0x1)
#define	DIAG_G		(0x2)
#define	DIAG_B		(0x4)
#define	DIAG_RG		(0x3)
#define	DIAG_GB		(0x6)
#define	DIAG_BR		(0x5)
#define	DIAG_RGB	(0x7)

private const unsigned short lum_w[8] = {
    (0*lum_blue_weight+0*lum_green_weight+0*lum_red_weight),
    (0*lum_blue_weight+0*lum_green_weight+1*lum_red_weight),
    (0*lum_blue_weight+1*lum_green_weight+0*lum_red_weight),
    (0*lum_blue_weight+1*lum_green_weight+1*lum_red_weight),
    (1*lum_blue_weight+0*lum_green_weight+0*lum_red_weight),
    (1*lum_blue_weight+0*lum_green_weight+1*lum_red_weight),
    (1*lum_blue_weight+1*lum_green_weight+0*lum_red_weight),
    (1*lum_blue_weight+1*lum_green_weight+1*lum_red_weight),
};

/* Compute a fractional color, the correctly rounded quotient of */
/* f * max_gx_color_value / maxv. */
#define _fc(f, maxv)\
  (gx_color_value)(((f) * (0xffffL * 2) + maxv) / (maxv * 2))
/* We have to split up the following because of a bug in the IBM AIX 3.2 */
/* C compiler. */
private const gx_color_value
  q0[] = { 0 };
private const gx_color_value
  q1[] = { 0, _fc(1,1) };
private const gx_color_value
  q2[] = { 0, _fc(1,2), _fc(2,2) };
private const gx_color_value
  q3[] = { 0, _fc(1,3), _fc(2,3), _fc(3,3) };
private const gx_color_value
  q4[] = { 0, _fc(1,4), _fc(2,4), _fc(3,4), _fc(4,4) };
private const gx_color_value
  q5[] = { 0, _fc(1,5), _fc(2,5), _fc(3,5), _fc(4,5), _fc(5,5) };
private const gx_color_value
  q6[] = { 0, _fc(1,6), _fc(2,6), _fc(3,6), _fc(4,6), _fc(5,6), _fc(6,6) };
private const gx_color_value
  q7[] = { 0, _fc(1,7), _fc(2,7), _fc(3,7), _fc(4,7), _fc(5,7), _fc(6,7), _fc(7,7) };
private const gx_color_value _ds *color_quo[8] =
 { q0, q1, q2, q3, q4, q5, q6, q7 };
#define fractional_color(f, maxv)\
  ((maxv) <= 7 ? color_quo[maxv][f] : _fc(f, maxv))

/* Render a gray, possibly by halftoning. */
void
gx_render_gray(frac gray, gx_device_color *pdevc, gs_state *pgs)
{	device *pdev = pgs->device;
	gx_device *dev;
	uint max_value;
	unsigned long hsize;
	dev_proc_map_rgb_color((*map_rgb_color));

/* Make a special check for black and white. */
	if ( gray == frac_0 )
	   {	pdevc->color2 = pdevc->color1 = pdev->black;
		pdevc->halftone_level = 0; /* pure color */
		return;
	   }
	else if ( gray == frac_1 )
	   {	pdevc->color2 = pdevc->color1 = pdev->white;
		pdevc->halftone_level = 0; /* pure color */
		return;
	   }

/* get a few handy values */
	dev = pdev->info;
	map_rgb_color = dev->procs->map_rgb_color;
	hsize = (unsigned)pgs->halftone->order_size;
	max_value = dev->color_info.dither_gray - 1;
	   {	unsigned long nshades = hsize * max_value + 1;
		unsigned long lx = (nshades * gray) / (frac_1_long + 1);
		uint v = lx / hsize;
		gx_color_value lum = fractional_color(v, max_value);
		pdevc->halftone_level = lx % hsize;
		pdevc->color1 = (*map_rgb_color)(dev, lum, lum, lum);
		if ( pdevc->halftone_level == 0 )
		   {	/* Close enough to a pure color, */
			/* no dithering needed. */
			pdevc->color2 = pdevc->color1;
		   }
		else
		   {	lum = fractional_color(v+1, max_value);
			pdevc->color2 =
				(*map_rgb_color)(dev, lum, lum, lum);
		   }
		gx_color_load(pdevc, pgs);
	   }
	return;
}

/* Render RGB, possibly by halftoning. */
void
gx_render_rgb(frac red, frac green, frac blue,
  gx_device_color *pdevc, gs_state *pgs)
{	device *pdev = pgs->device;
	gx_device *dev = pdev->info;
	uint max_value = dev->color_info.dither_rgb - 1;
	unsigned long hsize = (unsigned)pgs->halftone->order_size;
	dev_proc_map_rgb_color((*map_rgb_color)) = dev->procs->map_rgb_color;
	frac rem_r = red;
	frac rem_g = green;
	frac rem_b = blue;
	uint r, g, b;
	int adjust_r, adjust_b, adjust_g;
	frac amax;
	unsigned long dmax;
	int axisc, diagc;
	unsigned short lum_invert;
	unsigned long dot1, dot2, dot3;
	int level;

	/* Compute the quotient and remainder of each color component */
	/* with the actual number of available colors. */
	switch ( max_value )
	   {
	case 1:			/* 8 colors */
		if ( rem_r == frac_1 ) rem_r = 0, r = 1;
		else r = 0;
		if ( rem_g == frac_1 ) rem_g = 0, g = 1;
		else g = 0;
		if ( rem_b == frac_1 ) rem_b = 0, b = 1;
		else b = 0;
		break;
	default:
	   {	unsigned long want_r = (ulong)max_value * rem_r;
		unsigned long want_g = (ulong)max_value * rem_g;
		unsigned long want_b = (ulong)max_value * rem_b;
		/* We observe that if M = 2^n-1 and V < M^2, then */
		/*	V / M = (V + (V >> n) + 1) >> n	*/
		/*	V % M = (V + (V / M)) & M	*/
		r = ((want_r >> frac_bits) + want_r + 1) >> frac_bits;
		g = ((want_g >> frac_bits) + want_g + 1) >> frac_bits;
		b = ((want_b >> frac_bits) + want_b + 1) >> frac_bits;
		rem_r = ((uint)want_r + r) & frac_1;
		rem_g = ((uint)want_g + g) & frac_1;
		rem_b = ((uint)want_b + b) & frac_1;
	   }
	   }

	/* Check for no dithering required */
	if ( !(rem_r | rem_g | rem_b) )
	   {	pdevc->color2 = pdevc->color1 =
			(*map_rgb_color)(dev, fractional_color(r, max_value),
					 fractional_color(g, max_value),
					 fractional_color(b, max_value));
		pdevc->halftone_level = 0;
		return;
	   }

	if_debug9('c', "[c]rgb=%x,%x,%x -->\n   %x+%x,%x+%x,%x+%x -->\n",
		  (unsigned)red, (unsigned)green, (unsigned)blue,
		  (unsigned)r, (unsigned)rem_r, (unsigned)g, (unsigned)rem_g,
		  (unsigned)b, (unsigned)rem_b);

/* flip the remainder color into the 0, 0, 0 octant */
	lum_invert = 0;
#define half (frac_1/2)
	if ( rem_r > half )
		rem_r = frac_1 - rem_r,
		  adjust_r = -1, r++, lum_invert += lum_red_weight * 2;
	else
		adjust_r = 1;
	if ( rem_g > half )
		rem_g = frac_1 - rem_g,
		  adjust_g = -1, g++, lum_invert += lum_green_weight * 2;
	else
		adjust_g = 1;
	if ( rem_b > half )
		rem_b = frac_1 - rem_b,
		  adjust_b = -1, b++, lum_invert += lum_blue_weight * 2;
	else
		adjust_b = 1;
	pdevc->color1 = (*map_rgb_color)(dev, fractional_color(r, max_value),
					 fractional_color(g, max_value),
					 fractional_color(b, max_value));
/* 
 * Dot the color with each axis to find the best one of 7;
 * find the color at the end of the axis chosen.
 */
	if ( rem_g > rem_r )
	   {	if ( rem_b > rem_g )
			amax = rem_b, axisc = DIAG_B;
		else
			amax = rem_g, axisc = DIAG_G;
		if ( rem_b > rem_r )
			dmax = (unsigned long)rem_g+rem_b, diagc = DIAG_GB;
		else
			dmax = (unsigned long)rem_r+rem_g, diagc = DIAG_RG;
	   }
	else
	   {	if ( rem_b > rem_r )
			amax = rem_b, axisc = DIAG_B;
		else
			amax = rem_r, axisc = DIAG_R;
		if ( rem_b > rem_g )
			dmax = (unsigned long)rem_b+rem_r, diagc = DIAG_BR;
		else
			dmax = (unsigned long)rem_r+rem_g, diagc = DIAG_RG;
	   }

	dot1 = amax*WEIGHT1;
	dot2 = dmax*WEIGHT2;
	dot3 = (ulong)rem_r+rem_g+rem_b;	/* rgb axis */
	if ( dot1 > dot2 )
	   {	if ( dot3*WEIGHT3 > dot1 )
			diagc = DIAG_RGB,
			  level = (hsize * dot3) / (3 * frac_1_long);
		else
			diagc = axisc,
			  level = (hsize * amax) / frac_1_long;
	   }
	else
	   {	if ( dot3*WEIGHT3 > dot2 )
			diagc = DIAG_RGB,
			  level = (hsize * dot3) / (3 * frac_1_long);
		else
			level = (hsize * dmax) / (2 * frac_1_long);
	   };

	if_debug9('c', "   %x+%x,%x+%x,%x+%x; adjust=%d,%d,%d;\n",
		  (unsigned)r, (unsigned)rem_r, (unsigned)g, (unsigned)rem_g,
		  (unsigned)b, (unsigned)rem_b,
		  adjust_r, adjust_g, adjust_b);

	if ( (pdevc->halftone_level = level) == 0 )
		pdevc->color2 = pdevc->color1;
	else
	   {	gx_color_index color2;
/* construct the second color, inverting back to original space if needed */
		if (diagc & DIAG_R) r += adjust_r;
		if (diagc & DIAG_G) g += adjust_g;
		if (diagc & DIAG_B) b += adjust_b;
/* get the second device color, sorting by luminance */
		color2 = (*map_rgb_color)(dev, fractional_color(r, max_value),
					  fractional_color(g, max_value),
					  fractional_color(b, max_value));
		if ( lum_w[diagc] < lum_invert )
		   {	pdevc->color2 = pdevc->color1;
			pdevc->color1 = color2;
			pdevc->halftone_level = level = hsize - level;
		   }
		else
			pdevc->color2 = color2;
		gx_color_load(pdevc, pgs);
	   }

	if_debug5('c', "[c]diagc=%d; color1=%lx, color2=%lx, level=%d/%d\n",
		  diagc, (ulong)pdevc->color1, (ulong)pdevc->color2,
		  level, (unsigned)hsize);

}
