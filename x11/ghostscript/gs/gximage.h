/* Copyright (C) 1989, 1990, 1991, 1992 Aladdin Enterprises.  All rights reserved.

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

/* gximage.h */
/* Internal definitions for image rendering */
/* Requires gxcpath.h, gxdevmem.h, gzcolor.h, gzpath.h */
#include "gscspace.h"
#include "gsimage.h"

/* Interface for routine used to unpack and shuffle incoming samples. */
/* The Unix C compiler can't handle typedefs for procedure */
/* (as opposed to pointer-to-procedure) types, */
/* so we have to do it with macros instead. */
#define iunpack_proc(proc)\
  void proc(P6(const gs_image_enum *, sample_map *, byte *, const byte *, uint, uint))
/* Interface for routine used to render a (source) scan line. */
#define irender_proc(proc)\
  int proc(P4(gs_image_enum *, byte *, uint, int))

/*
 * Incoming samples may go through two different transformations:
 *
 *	- N-to-8-bit expansion may involve a lookup map.  Currently this
 *	map is either an identity function or a subtraction from 1
 *	(inversion).
 *
 *	- The 8-bit sample may undergo decoding (a linear transformation)
 *	before being handed off to the color mapping machinery.
 *
 * If the decoding function's range is [0..1], we fold it into the
 * expansion lookup; otherwise we must compute it separately.
 * For speed, we distinguish 3 different cases of the decoding step:
 */
typedef enum {
	sd_none,		/* decoded during expansion */
	sd_lookup,		/* use lookup_decode table */
	sd_compute		/* compute using base and factor */
} sample_decoding;
typedef struct sample_map_s {

	/* The following union implements the expansion of sample */
	/* values from N bits to 8, and a possible inversion. */

	union {

		ulong lookup4x1to32[16];	/* 1 bit/sample */
		ushort lookup2x2to16[16];	/* 2 bits/sample */
		/* lookup8 is also used for 1 or 2 bits/sample */
		/* if we are spreading out the samples. */
		byte lookup8[256];		/* 4 bits/sample [16] */
						/* 8 or 12 bits/sample */

	} table;

	/* If an 8-bit fraction doesn't represent the decoded value */
	/* accurately enough, but the samples have 4 bits or fewer, */
	/* we precompute the decoded values into a table. */
	/* Different entries are used depending on bits/sample: */
	/*	1,8,12 bits/sample: 0,15	*/
	/*	2 bits/sample: 0,5,10,15	*/
	/*	4 bits/sample: all	*/

	float decode_lookup[16];
#define decode_base decode_lookup[0]
#define decode_max decode_lookup[15]

	/* In the worst case, we have to do the decoding on the fly. */
	/* The value is base + sample * factor, where the sample is */
	/* an 8-bit (unsigned) integer. */

	float decode_factor;

	sample_decoding decoding;

} sample_map;

/* Decode an 8-bit sample into a floating point color component. */
/* penum points to the gs_image_enum structure. */
#define decode_sample(sample_value, cc, i)\
  switch ( penum->map[i].decoding )\
  {\
  case sd_none:\
    cc.paint.values[i] = (sample_value) * (1.0 / 255.0);  /* faster than / */\
    break;\
  case sd_lookup:	/* <= 4 significant bits */\
    cc.paint.values[i] =\
      penum->map[i].decode_lookup[(sample_value) >> 4];\
    break;\
  case sd_compute:\
    cc.paint.values[i] =\
      penum->map[i].decode_base + (sample_value) * penum->map[i].decode_factor;\
  }

/* Main state structure */
struct gs_image_enum_s {
	/* We really want the map structure to be long-aligned, */
	/* so we choose shorter types for some flags. */
	/* Following are set at structure initialization */
	int width;
	int height;
	int bps;			/* bits per sample: 1, 2, 4, 8, 12 */
	int spp;			/* samples per pixel: 1, 3, or 4 */
	int spread;			/* spp if colors are separated, */
					/* 1 otherwise */
	int masked;			/* 0 = [color]image, 1 = imagemask */
	fixed fxx, fxy, fyx, fyy;	/* fixed version of matrix */
	iunpack_proc((*unpack));
	irender_proc((*render));
	gs_state *pgs;
	gs_fixed_rect clip_box;		/* pgs->clip_path.path->bbox, */
					/* possibly translated */
	byte *buffer;			/* for expanding to 8 bits/sample */
	uint buffer_size;
	byte *line;			/* buffer for an output scan line */
	uint line_size;
	uint line_width;		/* width of line in device pixels */
	uint bytes_per_row;		/* # of input bytes per row */
					/* (per plane, if spp == 1 and */
					/* spread > 1) */
	byte never_clip;		/* true if entire image fits */
	byte skewed;			/* true if image is skewed */
					/* or rotated */
	byte slow_loop;			/* true if !(skewed | */
					/* imagemask with a halftone) */
	byte device_color;		/* true if device color space and */
					/* standard decoding */
	fixed adjust;			/* adjustment when rendering */
					/* characters */
	gx_device_clip clip_dev;	/* clipping device (if needed) */
	/* Following are updated dynamically */
	byte *planes[4];		/* separated color data */
	int plane_index;		/* current plane index, [0..spp) */
	uint plane_size;		/* size of data in each plane */
	uint byte_in_row;		/* current input byte position in row */
	fixed xcur, ycur;		/* device x, y of current row */
	int yci, hci;			/* integer y & height of row */
					/* (if no skew) */
	int y;
	/* The maps are set at initialization.  We put them here */
	/* so that the scalars will have smaller offsets. */
	sample_map map[4];
	/* Entries 0 and 255 of the following are set at initialization */
	/* for monochrome images; other entries are updated dynamically. */
	gx_device_color dev_colors[256];
#define icolor0 dev_colors[0]
#define icolor1 dev_colors[255]
};
