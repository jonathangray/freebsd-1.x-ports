/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gzcolor.h */
/* Private definition of color representation for Ghostscript */
#include "gscolor.h"			/* client interface */
#include "gxfmap.h"
#include "gxlum.h"

/*
 * The following parameters are computed from client color specifications,
 * and kept current through changes in transfer function or device.
 * If the halftone is a colored tile, color1 == color2 == gx_no_color_index,
 * and halftone_level == -1.  (Colored tiles are not currently used.)
 */
typedef struct gx_device_color_s gx_device_color;
struct gx_device_color_s {
	gx_color_index color1;		/* device color, or */
					/* darker color for halftoning */
	gx_color_index color2;		/* lighter color for halftoning */
	int halftone_level;		/* number of spots to whiten */
					/* when halftoning, 0 if */
					/* halftoning not needed, */
					/* <0 if color halftone */
	struct gx_bitmap_s *tile;	/* pointer to cached halftone */
};
#define color_is_pure(pdevc)\
  ((pdevc)->halftone_level == 0)
#define color_is_color_halftone(pdevc)\
  ((pdevc)->halftone_level < 0)

/* Procedures for rendering colors specified by fractions. */

#define cmap_proc_gray(proc)\
  void proc(P3(frac, gx_device_color *, const gs_state *))
#define cmap_proc_rgb(proc)\
  void proc(P5(frac, frac, frac, gx_device_color *, const gs_state *))
#define cmap_proc_cmyk(proc)\
  void proc(P6(frac, frac, frac, frac, gx_device_color *, const gs_state *))

typedef struct gx_color_map_procs_s {
	cmap_proc_gray((*map_gray));
	cmap_proc_rgb((*map_rgb));
	cmap_proc_cmyk((*map_cmyk));
} gx_color_map_procs;

/* A fast version of gz_fill_rectangle. */
/* Note that it takes additional arguments. */
#define gz_fill_rectangle_open(dev, xi, yi, w, h, fill_proc, tile_proc, pdevc, pgs)\
  (color_is_pure(pdevc) ?\
    (*fill_proc)(dev, xi, yi, w, h, pdevc->color1) :\
    (*tile_proc)(dev, pdevc->tile, xi, yi, w, h,\
	pdevc->color1, pdevc->color2,\
	pgs->phase_mod.x, pgs->phase_mod.y) )

/* A color transfer function and cache. */
/* log2... must not be greater than frac_bits. */
#define log2_transfer_map_size 8
#define transfer_map_size (1 << log2_transfer_map_size)
typedef struct gx_transfer_map_s {
	frac_map(log2_transfer_map_size);
} gx_transfer_map;

/* Map a color fraction through a transfer map. */
extern frac gx_color_frac_map(P2(frac, const frac *));
#define gx_map_color_frac(pgs,cf,m)\
  gx_color_frac_map(cf, &pgs->transfer.m->values[0])
/****************
#if log2_transfer_map_size <= 8
#  define byte_to_tmx(b) ((b) >> (8 - log2_transfer_map_size))
#else
#  define byte_to_tmx(b)\
	(((b) << (log2_transfer_map_size - 8)) +\
	 ((b) >> (16 - log2_transfer_map_size)))
#endif
#define gx_map_color_frac_byte(pgs,b,m)\
 (pgs->transfer->m.values[byte_to_tmx(b)])
 ****************/
