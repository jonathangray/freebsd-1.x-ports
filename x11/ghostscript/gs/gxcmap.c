/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gxcmap.c */
/* Color mapping and conversion for Ghostscript */
#include "gx.h"
#include "gserrors.h"
#include "gscspace.h"
#include "gxarith.h"
#include "gxfrac.h"
#include "gxlum.h"
#include "gxcolor.h"
#include "gxdevice.h"
#include "gzcolor.h"
#include "gzstate.h"
/* Brought back from a later release.... */
#define color_set_pure(pdc, color)\
  ((pdc)->color1 = (pdc)->color2 = (color), (pdc)->halftone_level = 0)

/* Convert a frac to a gx_color_value. */
/* This is needed because map_rgb_color still uses gx_color_value. */
#define _cv_bits (sizeof(gx_color_value) * 8)
#define frac2cv(fr)\
  ( ((fr) << (_cv_bits - frac_bits)) +\
    ((fr) >> (frac_bits * 2 - _cv_bits)) )
#define cv2frac(cv) ((frac)((cv) >> (_cv_bits - frac_bits)))

/* Note: the color model conversion algorithms are taken from */
/* Rogers, Procedural Elements for Computer Graphics, pp. 401-403. */

/* ------ Conversion between HSB and RGB ------ */

/* Convert RGB to HSB. */
void
color_rgb_to_hsb(floatp r, floatp g, floatp b, float hsb[3])
{	frac red = float2frac(r), green = float2frac(g), blue = float2frac(b);
#define rhue hsb[0]
#define rsat hsb[1]
#define rbri hsb[2]
	if ( red == green && green == blue )
	   {	rhue = 0;	/* arbitrary */
		rsat = 0;
		rbri = r;	/* pick any one */
	   }
	else
	   {	/* Convert rgb to hsb */
		frac V, Temp;
		long diff, H;
		V = (red > green ? red : green);
		if ( blue > V ) V = blue;
		Temp = (red > green ? green : red);
		if ( blue < Temp ) Temp = blue;
		diff = V - Temp;
		if ( V == red )
		    H = (green - blue) * frac_1_long / diff;
		else if ( V == green )
		    H = (blue - red) * frac_1_long / diff + 2 * frac_1_long;
		else /* V == blue */
		    H = (red - green) * frac_1_long / diff + 4 * frac_1_long;
		if ( H < 0 ) H += 6 * frac_1_long;
		rhue = H / (frac_1 * 6.0);
		rsat = diff / (float)V;
		rbri = frac2float(V);
	   }
#undef rhue
#undef rsat
#undef rbri
}

/* Convert HSB to RGB. */
void
color_hsb_to_rgb(floatp hue, floatp saturation, floatp brightness, float rgb[3])
{	if ( saturation == 0 )
	   {	rgb[0] = rgb[1] = rgb[2] = brightness;
	   }
	else
	   {	/* Convert hsb to rgb. */
		/* We rely on the fact that the product of two */
		/* fracs fits into an unsigned long. */
		floatp h6 = hue * 6;
		ulong V = float2frac(brightness);	/* force arithmetic to long */
		frac S = float2frac(saturation);
	        int I = (int)h6;
		ulong F = float2frac(h6 - I);		/* ditto */
		/* M = V*(1-S), N = V*(1-S*F), K = V*(1-S*(1-F)) = M-N+V */
		frac M = V * (frac_1_long - S) / frac_1_long;
		frac N = V * (frac_1_long - S * F / frac_1_long) / frac_1_long;
		frac K = M - N + V;
		frac R, G, B;
		switch ( I )
		   {
		default: R = V; G = K; B = M; break;
		case 1: R = N; G = V; B = M; break;
		case 2: R = M; G = V; B = K; break;
		case 3: R = M; G = N; B = V; break;
		case 4: R = K; G = M; B = V; break;
		case 5: R = V; G = M; B = N; break;
		   }
		rgb[0] = frac2float(R);
		rgb[1] = frac2float(G);
		rgb[2] = frac2float(B);
#ifdef DEBUG
if ( debug_c('c') )
{		dprintf7("[c]hsb(%g,%g,%g)->VSFI(%ld,%d,%ld,%d)->\n",
			 hue, saturation, brightness, V, S, F, I);
		dprintf6("   RGB(%d,%d,%d)->rgb(%g,%g,%g)\n",
			 R, G, B, rgb[0], rgb[1], rgb[2]);
}
#endif
	   }
}

/* ------ Color space conversion ------ */

/* Only 4 of the 6 conversions are implemented here; */
/* the other 2 (Gray to RGB/CMYK) are trivial. */
/* The CMYK to RGB algorithms specified by Adobe are, e.g., */
/*	R = 1.0 - min(1.0, C + K)	*/
/* but we get much better results with */
/*	R = (1.0 - C) * (1.0 - K)	*/

/* Convert RGB to Gray. */
frac
color_rgb_to_gray(frac r, frac g, frac b, const gs_state *pgs)
{	return (r * (unsigned long)lum_red_weight +
		g * (unsigned long)lum_green_weight +
		b * (unsigned long)lum_blue_weight +
		(lum_all_weights / 2))
	    / lum_all_weights;
}

/* Convert RGB to CMYK. */
/* Note that this involves black generation and undercolor removal. */
void
color_rgb_to_cmyk(frac r, frac g, frac b, const gs_state *pgs,
  frac cmyk[4])
{	frac c = frac_1 - r, m = frac_1 - g, y = frac_1 - b;
	frac k = (c < m ? min(c, y) : min(m, y));
	/* The default UCR and BG functions are pretty arbitrary.... */
	frac bg =
		(pgs->black_generation == NULL ? frac_0 :
		 float2frac((*pgs->black_generation)(pgs, frac2float(k))));
	signed_frac ucr =
		(pgs->undercolor_removal == NULL ? frac_0 :
		 float2frac((*pgs->undercolor_removal)(pgs, frac2float(k))));
	/* Adobe specifies, e.g., */
	/*	C = max(0.0, min(1.0, 1 - R - UCR)) */
	/* but in order to match our improved CMYK->RGB mapping, we use */
	/*	C = max(0.0, min(1.0, 1 - R / (1 - UCR)) */
	if ( ucr == frac_1 )
		cmyk[0] = cmyk[1] = cmyk[2] = 0;
	else
	{	float denom = frac2float(frac_1 - ucr);	/* unscaled */
		float v;
		v = (float)frac_1 - r / denom;	/* unscaled */
		cmyk[0] =
		  (is_fneg(v) ? frac_0 : v >= (float)frac_1 ? frac_1 : (frac)v);
		v = (float)frac_1 - g / denom;	/* unscaled */
		cmyk[1] =
		  (is_fneg(v) ? frac_0 : v >= (float)frac_1 ? frac_1 : (frac)v);
		v = (float)frac_1 - b / denom;	/* unscaled */
		cmyk[2] =
		  (is_fneg(v) ? frac_0 : v >= (float)frac_1 ? frac_1 : (frac)v);
	}
	cmyk[3] = bg;
	if_debug7('c', "[c]RGB 0x%x,0x%x,0x%x -> CMYK 0x%x,0x%x,0x%x,0x%x\n",
		  r, g, b, cmyk[0], cmyk[1], cmyk[2], cmyk[3]);
}

/* Convert CMYK to Gray. */
frac
color_cmyk_to_gray(frac c, frac m, frac y, frac k, const gs_state *pgs)
{	frac not_gray = color_rgb_to_gray(c, m, y, pgs);
	return (not_gray > frac_1 - k ?		/* gray + k > 1.0 */
		frac_0 : frac_1 - (not_gray + k));
}

/* Convert CMYK to RGB. */
void
color_cmyk_to_rgb(frac c, frac m, frac y, frac k, const gs_state *pgs,
  frac rgb[3])
{	switch ( k )
	{
	case frac_0:
		rgb[0] = frac_1 - c;
		rgb[1] = frac_1 - m;
		rgb[2] = frac_1 - y;
		break;
	case frac_1:
		rgb[0] = rgb[1] = rgb[2] = frac_0;
		break;
	default:
	{	ulong not_k = frac_1 - k;
		/* Compute not_k * (frac_1 - v) / frac_1 efficiently. */
		ulong prod;
#define deduct_black(v)\
  (prod = (frac_1 - (v)) * not_k,\
   (prod + (prod >> frac_bits) + 1) >> frac_bits)
		rgb[0] = deduct_black(c);
		rgb[1] = deduct_black(m);
		rgb[2] = deduct_black(y);
	}
	}
	if_debug7('c', "[c]CMYK 0x%x,0x%x,0x%x,0x%x -> RGB 0x%x,0x%x,0x%x\n",
		  c, m, y, k, rgb[0], rgb[1], rgb[2]);
}

/* ------ Device color rendering ------ */

private cmap_proc_gray(cmap_gray_halftoned);
private cmap_proc_gray(cmap_gray_direct);
private cmap_proc_gray(cmap_gray_to_rgb);
private cmap_proc_gray(cmap_gray_to_cmyk);
#define cmap_rgb_halftoned cmap_rgb_direct
private cmap_proc_rgb(cmap_rgb_direct);
private cmap_proc_rgb(cmap_rgb_to_gray);
private cmap_proc_rgb(cmap_rgb_to_cmyk);
#define cmap_cmyk_halftoned cmap_cmyk_direct
private cmap_proc_cmyk(cmap_cmyk_direct);
private cmap_proc_cmyk(cmap_cmyk_to_gray);
private cmap_proc_cmyk(cmap_cmyk_to_rgb);

private const gx_color_map_procs
	cmap_gray_few =
		{ cmap_gray_halftoned, cmap_rgb_to_gray, cmap_cmyk_to_gray },
	cmap_gray_many =
		{ cmap_gray_direct, cmap_rgb_to_gray, cmap_cmyk_to_gray },
	cmap_rgb_few =
		{ cmap_gray_to_rgb, cmap_rgb_halftoned, cmap_cmyk_to_rgb },
	cmap_rgb_many =
		{ cmap_gray_to_rgb, cmap_rgb_direct, cmap_cmyk_to_rgb },
	cmap_cmyk_few =
		{ cmap_gray_to_cmyk, cmap_rgb_to_cmyk, cmap_cmyk_halftoned },
	cmap_cmyk_many =
		{ cmap_gray_to_cmyk, cmap_rgb_to_cmyk, cmap_cmyk_direct };

const gx_color_map_procs *cmap_procs_default = &cmap_gray_many;

private const gx_color_map_procs _ds *cmap_few[] = {
	0, &cmap_gray_few, 0, &cmap_rgb_few, &cmap_cmyk_few
};

private const gx_color_map_procs _ds *cmap_many[] = {
	0, &cmap_gray_many, 0, &cmap_rgb_many, &cmap_cmyk_many
};

/* Set the color mapping procedures in the graphics state. */
void
gx_set_cmap_procs(gs_state *pgs)
{	gx_device *dev = gs_currentdevice(pgs);
	pgs->cmap_procs =
		((gx_device_has_color(dev) ? dev->color_info.max_rgb :
		  dev->color_info.max_gray) >= 31 ? cmap_many : cmap_few)
		 [dev->color_info.num_components];
}

/* Remap the color in the graphics state. */
int
gx_remap_color(gs_state *pgs)
{	const gs_color_space *pcs = pgs->color_space;
	return (*pcs->type->remap_color)(pgs->ccolor, pcs,
				pgs->dev_color, pgs);
}
/* Color remappers for the standard color spaces. */
#define unit_frac(v)\
  (ftemp = (v),\
   (is_fneg(ftemp) ? frac_0 : ftemp >= 1.0 ? frac_1 : float2frac(ftemp)))
int
gx_remap_DeviceGray(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, gs_state *pgs)
{	float ftemp;
	(*pgs->cmap_procs->map_gray)
		(unit_frac(pc->paint.values[0]),
		 pdc, pgs);
	return 0;
}
int
gx_remap_DeviceRGB(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, gs_state *pgs)
{	float ftemp;
	(*pgs->cmap_procs->map_rgb)
		(unit_frac(pc->paint.values[0]),
		 unit_frac(pc->paint.values[1]),
		 unit_frac(pc->paint.values[2]),
		 pdc, pgs);
	return 0;
}
int
gx_remap_DeviceCMYK(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, gs_state *pgs)
{	float ftemp;
	(*pgs->cmap_procs->map_cmyk)
		(unit_frac(pc->paint.values[0]),
		 unit_frac(pc->paint.values[1]),
		 unit_frac(pc->paint.values[2]),
		 unit_frac(pc->paint.values[3]),
		 pdc, pgs);
	return 0;
}
#undef unit_frac

/* Render Gray color. */

private void
cmap_gray_direct(frac gray, gx_device_color *pdc, const gs_state *pgs)
{	gx_device *dev = gs_currentdevice(pgs);
	frac mgray = gx_map_color_frac(pgs, gray, gray);
	gx_color_value cv_gray = frac2cv(mgray);
	gx_color_index color =
		(*dev->procs->map_rgb_color)(dev, cv_gray, cv_gray, cv_gray);
	if ( color == gx_no_color_index )
	{	gx_render_gray(mgray, pdc, pgs);
		return;
	}
	color_set_pure(pdc, color);
}

private void
cmap_gray_halftoned(frac gray, gx_device_color *pdc, const gs_state *pgs)
{	gx_render_gray(gx_map_color_frac(pgs, gray, gray), pdc, pgs);
}

private void
cmap_gray_to_rgb(frac gray, gx_device_color *pdc, const gs_state *pgs)
{	(*pgs->cmap_procs->map_rgb)(gray, gray, gray, pdc, pgs);
}

private void
cmap_gray_to_cmyk(frac gray, gx_device_color *pdc, const gs_state *pgs)
{	(*pgs->cmap_procs->map_cmyk)(frac_0, frac_0, frac_0, gray, pdc, pgs);
}

/* Render RGB color. */

/*
 * This code should test r == g and g == b and then use the gray
 * rendering procedures.  The Adobe documentation allows this:
 * conversion between color spaces occurs before the transfer function
 * and halftoning.  However, output from FrameMaker (mis)uses the
 * transfer function to provide the equivalent of indexed color;
 * it requires the color components to be passed through unchanged.
 * For this reason, we have to make the check after the transfer
 * function rather than before.
 */

private void
cmap_rgb_direct(frac r, frac g, frac b, gx_device_color *pdc,
  const gs_state *pgs)
{	gx_device *dev = gs_currentdevice(pgs);
	frac mred = gx_map_color_frac(pgs, r, red);
	frac mgreen = gx_map_color_frac(pgs, g, green);
	frac mblue = gx_map_color_frac(pgs, b, blue);
	/* We make a test for direct vs. halftoned, rather than */
	/* duplicating most of the code of this procedure. */
	if ( dev->color_info.max_rgb >= 31 )
	{	gx_color_index color =
			(*dev->procs->map_rgb_color)(dev,
				frac2cv(mred), frac2cv(mgreen),
				frac2cv(mblue));
		if ( color != gx_no_color_index )
		{	color_set_pure(pdc, color);
			return;
		}
	}
	if ( mred == mgreen && mred == mblue )	/* gray shade */
		gx_render_gray(mred, pdc, pgs);
	else
		gx_render_rgb(mred, mgreen, mblue, pdc, pgs);
}

private void
cmap_rgb_to_gray(frac r, frac g, frac b, gx_device_color *pdc,
  const gs_state *pgs)
{	(*pgs->cmap_procs->map_gray)(color_rgb_to_gray(r, g, b, pgs), pdc, pgs);
}

private void
cmap_rgb_to_cmyk(frac r, frac g, frac b, gx_device_color *pdc,
  const gs_state *pgs)
{	frac cmyk[4];
	color_rgb_to_cmyk(r, g, b, pgs, cmyk);
	(*pgs->cmap_procs->map_cmyk)(cmyk[0], cmyk[1], cmyk[2], cmyk[3], pdc, pgs);
}

/* Render CMYK color. */

/* See above under RGB for why we can't use a shortcut for gray. */

private void
cmap_cmyk_direct(frac c, frac m, frac y, frac k, gx_device_color *pdc,
  const gs_state *pgs)
{	gx_device *dev = gs_currentdevice(pgs);
	frac mcyan = gx_map_color_frac(pgs, c, red);
	frac mmagenta = gx_map_color_frac(pgs, m, green);
	frac myellow = gx_map_color_frac(pgs, y, blue);
	frac mblack = gx_map_color_frac(pgs, k, gray);
	/* We make a test for direct vs. halftoned, rather than */
	/* duplicating most of the code of this procedure. */
	if ( dev->color_info.max_rgb >= 31 )
	{	gx_color_index color =
			(*dev->procs->map_cmyk_color)(dev,
				frac2cv(mcyan), frac2cv(mmagenta),
				frac2cv(myellow), frac2cv(mblack));
		if ( color != gx_no_color_index )
		{	color_set_pure(pdc, color);
			return;
		}
	}
	/* CMYK halftones are not implemented yet. */
	{	frac rgb[3];
		color_cmyk_to_rgb(c, m, y, k, pgs, rgb);
		cmap_rgb_halftoned(rgb[0], rgb[1], rgb[2], pdc, pgs);
	}
}

private void
cmap_cmyk_to_gray(frac c, frac m, frac y, frac k, gx_device_color *pdc, const gs_state *pgs)
{	(*pgs->cmap_procs->map_gray)(color_cmyk_to_gray(c, m, y, k, pgs), pdc, pgs);
}

private void
cmap_cmyk_to_rgb(frac c, frac m, frac y, frac k, gx_device_color *pdc, const gs_state *pgs)
{	frac rgb[3];
	color_cmyk_to_rgb(c, m, y, k, pgs, rgb);
	(*pgs->cmap_procs->map_rgb)(rgb[0], rgb[1], rgb[2], pdc, pgs);
}

/* ------ Transfer function mapping ------ */

/* Map a color fraction through a transfer map. */
frac
gx_color_frac_map(frac cv, const frac *values)
{
#define cp_frac_bits (frac_bits - log2_transfer_map_size)
	int cmi = cv >> cp_frac_bits;
	frac mv = values[cmi];
	int rem, mdv;
	/* Interpolate between two adjacent values if needed. */
	rem = (cv & ((1 << cp_frac_bits) - 1)) - (cv >> (frac_bits - cp_frac_bits));
	if ( rem == 0 ) return mv;
	else if ( rem > 0 ) mdv = values[cmi + 1] - mv;
	else mdv = mv - values[cmi - 1];
#if arch_ints_are_short
	/* Only use long multiplication if necessary. */
	if ( mdv > 1 << (16 - cp_frac_bits) )
		return mv + (uint)(((ulong)rem * mdv) >> cp_frac_bits);
#endif
	return mv + ((rem * mdv) >> cp_frac_bits);
#undef cp_frac_bits
}

/* ------ Default device color mapping ------ */

gx_color_index
gx_default_map_rgb_color(gx_device *dev,
  gx_color_value r, gx_color_value g, gx_color_value b)
{	/* Map values >= 1/2 to 1, < 1/2 to 0. */
	return ((r | g | b) > gx_max_color_value / 2 ?
		(gx_color_index)1 : (gx_color_index)0);
}

int
gx_default_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	/* Map 1 to max_value, 0 to 0. */
	prgb[0] = prgb[1] = prgb[2] = -(gx_color_value)color;
	return 0;
}

gx_color_index
gx_default_map_cmyk_color(gx_device *dev,
  gx_color_value c, gx_color_value m, gx_color_value y, gx_color_value k)
{	/* Convert to RGB */
	frac rgb[3];
	color_cmyk_to_rgb(cv2frac(c), cv2frac(m), cv2frac(y), cv2frac(k),
			  NULL, rgb);
	return (*dev->procs->map_rgb_color)(dev,
		frac2cv(rgb[0]), frac2cv(rgb[1]), frac2cv(rgb[2]));
}
