/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.

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

/* gsimage2.c */
/* Additional image procedures for Ghostscript library */
/* This file is logically part of gsimage.c; we have split it out */
/* to reduce the code working set. */
#include "gx.h"
#include "memory_.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gscspace.h"
#include "gspaint.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate.h */
#include "gzcolor.h"			/* requires gxdevice.h */
#include "gzpath.h"
#include "gxcolor.h"
#include "gxcpath.h"
#include "gxdevmem.h"
#include "gximage.h"

/* ------ Unpacking procedures ------ */

void
image_unpack_1_spread(const gs_image_enum *penum, const sample_map *pmap,
  register byte *bufp, register const byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	int left = dsize;
	register const byte *map = &pmap->table.lookup8[0];
	while ( left-- )
	   {	register uint b = *data++;
		*bufp = map[b >> 7]; bufp += spread;
		*bufp = map[(b >> 6) & 1]; bufp += spread;
		*bufp = map[(b >> 5) & 1]; bufp += spread;
		*bufp = map[(b >> 4) & 1]; bufp += spread;
		*bufp = map[(b >> 3) & 1]; bufp += spread;
		*bufp = map[(b >> 2) & 1]; bufp += spread;
		*bufp = map[(b >> 1) & 1]; bufp += spread;
		*bufp = map[b & 1]; bufp += spread;
	   }
}

void
image_unpack_2_spread(const gs_image_enum *penum, const sample_map *pmap,
  register byte *bufp, register const byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	int left = dsize;
	register const byte *map = &pmap->table.lookup8[0];
	while ( left-- )
	   {	register unsigned b = *data++;
		*bufp = map[b >> 6]; bufp += spread;
		*bufp = map[(b >> 4) & 3]; bufp += spread;
		*bufp = map[(b >> 2) & 3]; bufp += spread;
		*bufp = map[b & 3]; bufp += spread;
	   }
}

void
image_unpack_8_spread(const gs_image_enum *penum, const sample_map *pmap,
  register byte *bufp, register const byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	register int left = dsize;
	register const byte *map = &pmap->table.lookup8[0];
	while ( left-- )
	   {	*bufp = map[*data++]; bufp += spread;
	   }
}

void
image_unpack_12(const gs_image_enum *penum, const sample_map *pmap,
  register byte *bufp, register const byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	register int left = dsize;
	register const byte *map = &pmap->table.lookup8[0];
	/* We have to deal with the 3 cases of inpos % 3 separately. */
	/* (In fact, this is the only reason inpos is passed to */
	/* the unpacking procedures at all.) */
	/****** DOESN'T DO MAPPING RIGHT. ******/
	/* Let N = inpos / 3. */
	switch ( inpos % 3 )
	   {
	case 1:
		/* bufp points to byte 2N, which was already filled */
		/* with the leftover byte from the previous call. */
		bufp += spread;
		*bufp = *data++ << 4;
		if ( !--left ) return;
	case 2:
		/* bufp points to byte 2N+1, which was half-filled */
		/* with the second leftover byte from the previous call. */
		*bufp = map[*bufp + (*data++ >> 4)];
		--left;
	case 0:
		/* Nothing special to do. */
		;
	   }
	/* Just drop the low 4 bits of each 12. */
	while ( left >= 3 )
	   {	*bufp = map[*data];
		bufp += spread;
		*bufp = map[(data[1] << 4) + (data[2] >> 4)];
		bufp += spread;
		data += 3;
	   }
	switch ( left )
	   {
	case 2:				/* dddddddd xxxxdddd */
		bufp[1] = data[1] << 4;
	case 1:				/* dddddddd */
		*bufp = map[*data];
	case 0:				/* Nothing more to do. */
		;
	   }
}

/* ------ Rendering procedures ------ */

/* Rendering procedure for handling color images. */
typedef union {
	byte v[4];
	ulong all;
} color_samples;
int
image_render_color(gs_image_enum *penum, byte *buffer, uint w, int h)
{	gs_state *pgs = penum->pgs;
	fixed	dxx = penum->fxx, dxy = penum->fxy,
		dyx = penum->fyx, dyy = penum->fyy;
	int skew = penum->skewed;
	fixed xt = penum->xcur;
	fixed ytf = penum->ycur;
	int yt = penum->yci, iht = penum->hci;
	gs_color_space *pcs = pgs->color_space;
	cs_proc_remap_color((*remap_color)) = pcs->type->remap_color;
	gs_client_color cc;
	int device_color = penum->device_color;
	cmap_proc_rgb((*map_rgb)) = pgs->cmap_procs->map_rgb;
	cmap_proc_cmyk((*map_cmyk)) = pgs->cmap_procs->map_cmyk;
	gx_device_color devc1, devc2;
	gx_device_color _ss *spdevc = &devc1;
	gx_device_color _ss *spdevc_next = &devc2;
#define pdevc ((gx_device_color *)spdevc)
#define pdevc_next ((gx_device_color *)spdevc_next)
	int spp = penum->spp;
	int i;
	fixed xl = xt;
	const byte *psrc = buffer;
	fixed xrun = xt;		/* x at start of run */
	int irun = fixed2int_var_rounded(xrun);	/* int xrun */
	fixed yrun = ytf;		/* y ditto */
	color_samples run;		/* run value */
	color_samples next;		/* next sample value */
	byte *bufend = buffer + w;
	bufend[0] = ~bufend[-spp];	/* force end of run */
	if_debug5('b', "[b]y=%d w=%d xt=%f yt=%f yb=%f\n",
		  penum->y, w,
		  fixed2float(xt), fixed2float(ytf), fixed2float(ytf + dyy));
	run.all = 0;
	next.all = 0;
	for ( i = 0; i < spp; i++ )
		cc.paint.values[i] = penum->map[i].decode_base;
	(*remap_color)(&cc, pcs, pdevc, pgs);
	while ( psrc <= bufend )	/* 1 extra iteration */
				/* to handle final run */
	{	next.v[0] = psrc[0];
		next.v[1] = psrc[1];
		next.v[2] = psrc[2];
		if ( spp == 4 )		/* cmyk */
		{	next.v[3] = psrc[3];
			psrc += 4;
			if ( next.all == run.all ) goto inc;
			if ( device_color )
			{	(*map_cmyk)(byte2frac(next.v[0]),
					byte2frac(next.v[1]),
					byte2frac(next.v[2]),
					byte2frac(next.v[3]),
					pdevc_next, pgs);
				goto f;
			}
			else
			{	decode_sample(next.v[3], cc, 3);
			}
		}
		else			/* rgb */
		{	psrc += 3;
			if ( next.all == run.all ) goto inc;
			if ( device_color )
			{	(*map_rgb)(byte2frac(next.v[0]),
					byte2frac(next.v[1]),
					byte2frac(next.v[2]),
					pdevc_next, pgs);
				goto f;
			}
		}
		decode_sample(next.v[0], cc, 0);
		decode_sample(next.v[1], cc, 1);
		decode_sample(next.v[2], cc, 2);
		(*remap_color)(&cc, pcs, pdevc_next, pgs);
f:		if_debug7('B', "[B]0x%x,0x%x,0x%x,0x%x -> %ld,%ld,%d\n",
			next.v[0], next.v[1], next.v[2], next.v[3],
			pdevc_next->color1, pdevc_next->color2,
			pdevc_next->halftone_level);
		/* Even though the supplied colors don't match, */
		/* the device colors might. */
		if ( devc1.color1 != devc2.color1 ||
		     devc1.halftone_level != devc2.halftone_level ||
		     (devc1.halftone_level &&
		      devc1.color2 != devc2.color2) ||
		     psrc > bufend	/* force end of last run */
		   )
		{	/* Fill the region between */
			/* xrun/irun and xl */
			gx_device_color _ss *sptemp;
			int code;
			if ( skew )
			{	/* Parallelogram */
				code = gz_fill_pgram_fixed(xrun, yrun,
					xl - xrun, ytf - yrun, dyx, dyy,
					pdevc, pgs);
				xrun = xl;
				yrun = ytf;
			}
				else
			{	/* Rectangle */
				int xi = irun;
				int wi = (irun = fixed2int_var_rounded(xl)) - xi;
				if ( wi < 0 ) xi += wi, wi = -wi;
				code = gz_fill_rectangle(xi, yt, wi, iht, pdevc, pgs);
			}
			if ( code < 0 )
				return code;
			return_if_interrupt();
			sptemp = spdevc;
			spdevc = spdevc_next;
			spdevc_next = sptemp;
		}
		run.all = next.all;
inc:		xl += dxx;
		ytf += dxy;		/* harmless if no skew */
	}
	return 1;
}
