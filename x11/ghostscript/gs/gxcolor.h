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

/* gxcolor.h */
/* Client color structure definition */
#include "gxfrac.h"
#include "gsuid.h"

struct gs_client_color_s;
typedef struct gs_client_color_s gs_client_color;

/* Paint (non-pattern) colors (Device, CIE, Indexed, Separation) */
typedef struct gs_paint_color_s {
	float values[4];
} gs_paint_color;

/* Pattern */
typedef struct gs_pattern_s {
	gs_uid uid;		/* XUID or nothing */
	int PaintType;
	int TilingType;
	gs_rect BBox;
	float XStep;
	float YStep;
	int (*PaintProc)(P2(gs_client_color *, gs_state *));
} gs_pattern;

/* General colors */
struct gs_client_color_s {
	gs_paint_color paint;		/* also color for uncolored pattern */
	gs_pattern pattern;
};

/* Color representation conversion routines */
void color_hsb_to_rgb(P4(floatp h, floatp s, floatp b, float rgb[3]));
void color_rgb_to_hsb(P4(floatp r, floatp g, floatp b, float hsb[3]));
/* Color space conversion routines */
frac color_rgb_to_gray(P4(frac r, frac g, frac b,
  const gs_state *pgs));
void color_rgb_to_cmyk(P5(frac r, frac g, frac b,
  const gs_state *pgs, frac cmyk[4]));
frac color_cmyk_to_gray(P5(frac c, frac m, frac y, frac k,
  const gs_state *pgs));
void color_cmyk_to_rgb(P6(frac c, frac m, frac y, frac k,
  const gs_state *pgs, frac rgb[3]));
