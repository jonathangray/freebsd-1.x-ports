/* Copyright (C) 1993 Aladdin Enterprises.  All rights reserved.

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

/* gxcht.c */
/* Color halftone setup for Ghostscript imaging library */
/****** NOT USED YET, DON'T TAKE TOO SERIOUSLY ******/
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"			/* for gxdevice.h */
#include "gzstate.h"
#include "gzdevice.h"
#include "gzcolor.h"			/* requires gxdevice.h */
#include "gzht.h"

#include "gxdevmem.h"

/*
 * We construct color halftone tiles out of 3 or 4 "planes".
 * Each plane specifies halftoning for one component (R/G/B or C/M/Y/K).
 */

void
set_color_ht(
	gx_bitmap *ctile,	/* the output tile; data, raster, size are set */
	int px,		/* the initial phase of the output tile */
	int py,
	int w,		/* how much of the tile to set */
	int h,
	gx_device *dev,	/* for color mapping */
	gx_device_color planes[4],	/* actually nplanes */
	int nplanes	/* 3 or 4 */
)
{	/* Note that the planes are specified in the order RGB or CMYK, but */
	/* the indices used for the internal colors array are BGR or KYMC. */

	gx_color_index colors[16];	/* the actual colors for the tile */

	/* Set up the tile colors. */
	{	gx_color_value r0 = planes[0].color1;
		gx_color_value r1 = planes[0].color2;
		gx_color_value g0 = planes[1].color1;
		gx_color_value g1 = planes[1].color2;
		gx_color_value b0 = planes[2].color1;
		gx_color_value b1 = planes[2].color2;
#define map8(m)\
  m(0, r0, g0, b0); m(1, r1, g0, b0);\
  m(2, r0, g1, b0); m(3, r1, g1, b0);\
  m(4, r0, g0, b1); m(5, r1, g0, b1);\
  m(6, r0, g1, b1); m(7, r1, g1, b1)
		if ( nplanes == 3 )
		{	dev_proc_map_rgb_color((*map)) =
				dev->procs->map_rgb_color;
#define mapc(i, r, g, b)\
  colors[i] = (*map)(dev, r, g, b)
			map8(mapc);
#undef mapc
		}
		else
		{	dev_proc_map_cmyk_color((*map)) =
				dev->procs->map_cmyk_color;
			gx_color_value k0 = planes[3].color1;
			gx_color_value k1 = planes[3].color2;
#define mapc(i, r, g, b)\
  colors[i] = (*map)(dev, r, g, b, k0);\
  colors[i+8] = (*map)(dev, r, g, b, k1)
			map8(mapc);
#undef mapc
		}
#undef map8
	}

	/* Construct the actual tile. */
	{	int x, y;
		struct tile_cursor_s {
			int xoffset;
			int xshift;
			const byte *row;
			const byte *tdata;
			uint raster;
			const byte *data;
			uint shifter;
		} cursor[4];
		/* Note that depth must be at least 4 */
		/* (4, 8, 16, 24, 32). */
		int depth = dev->color_info.depth;
		int dbytes = depth >> 3;
		uint dest_raster = ctile->raster;
		byte *dest_row =
		  ctile->data + dest_raster * (h - 1) + (w * depth - 1) / 8;
		{	int lastx = w - 1 + px;
			int lasty = h - 1 + py;
#define set_start(c, i)\
{ gx_bitmap *btile = planes[i].tile;\
  int bx = lastx % btile->size.x;\
  int by = lasty % btile->size.y;\
  c.xoffset = bx >> 3;\
  c.xshift = ~bx & 7;\
  c.tdata = btile->data;\
  c.raster = btile->raster;\
  c.row = c.tdata + by * c.raster;\
}
			set_start(cursor[0], 0);
			set_start(cursor[1], 1);
			set_start(cursor[2], 2);
			if ( nplanes == 4 )
				set_start(cursor[3], 3);
#undef set_start
		}
		for ( y = h; --y >= 0; dest_row -= dest_raster )
		{	byte *dest = dest_row;
#define set_row(c, i)\
  {	c.data = c.row + c.xoffset;\
	c.shifter = ((*c.data + 0x100) >> c.xshift) << i;\
  }
			set_row(cursor[0], 0);
			set_row(cursor[1], 1);
			set_row(cursor[2], 2);
			if ( nplanes == 4 )
			{	set_row(cursor[3], 3);
			}
			else
				cursor[3].shifter = 0;
#undef set_row
			for ( x = w; --x >= 0; )
			{	int c =
					(cursor[0].shifter & 1) +
					(cursor[1].shifter & 2) +
					(cursor[2].shifter & 4);
				gx_color_index tcolor;
#define step_plane(c, i)\
  if ( (c.shifter >>= 1) < (2 << i) )\
  {	if ( c.data > c.row )\
		c.shifter = (*--c.data + 0x100) << i;\
	else\
	{	int tw1 = planes[i].tile->size.x - 1;\
		c.data = c.row + (tw1 >> 3);\
		c.shifter = ((*c.data + 0x100) >> (~tw1 & 7)) << i;\
	}\
  }
				step_plane(cursor[0], 0);
				step_plane(cursor[1], 1);
				step_plane(cursor[2], 2);
				if ( nplanes == 4 )
				{	c += cursor[3].shifter & 8;
					step_plane(cursor[3], 3);
				}
#undef step_plane
				tcolor = colors[c];
				switch ( dbytes )
				{
				case 0:			/* 4 */
					if ( x & 1 )
						*dest = (byte)tcolor;
					else
						*dest-- += (byte)tcolor << 4;
					break;
				case 4:			/* 32 */
					dest -= 4;
					dest[3] = (byte)tcolor;
					dest[2] = (byte)((uint)tcolor >> 8);
					tcolor >>= 16;
					goto c2;
				case 3:			/* 24 */
					dest -= 3;
					dest[2] = (byte)tcolor;
					dest[1] = (byte)((uint)tcolor >> 8);
					tcolor >>= 16;
					goto c2;
				case 2:			/* 16 */
					dest -= 2;
c2:					dest[1] = (byte)tcolor;
c1:					dest[0] = (byte)((uint)tcolor >> 8);
					break;
				case 1:			/* 8 */
					*dest-- = (byte)tcolor;
				}
			}
#define step_row(c, i)\
  if ( c.row > c.tdata )\
	c.row -= c.raster;\
  else\
	c.row += c.raster * (planes[i].tile->size.y - 1)
			step_row(cursor[0], 0);
			step_row(cursor[1], 1);
			step_row(cursor[2], 2);
			if ( nplanes == 4)
				step_row(cursor[3], 3);
#undef step_row
		}
	}

}
