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

/* gsht.c */
/* Halftone operators for Ghostscript library */
#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gzstate.h"
#include "gzht.h"

/* Halftone enumeration structure */
struct gs_screen_enum_s {
	halftone_params ht;		/* constructed here */
	gs_matrix mat;		/* for mapping device x,y to rotated cell */
	int x, y;
	gs_state *pgs;
};

/* Exported values */
const uint gs_screen_enum_sizeof = sizeof(gs_screen_enum);

/* Forward declarations */
private void set_phase(P1(gs_state *));
private void gx_sort_ht_order(P2(ht_bit *, uint));

/* setscreen */
int
gs_setscreen(gs_state *pgs,
  floatp freq, floatp angle, float (*proc)(P2(floatp, floatp)))
{	gs_screen_enum senum;
	gs_point pt;
	int code = gs_screen_init(&senum, pgs, freq, angle);
	if ( code < 0 ) return code;
	while ( (code = gs_screen_currentpoint(&senum, &pt)) == 0 )
		if ( (code = gs_screen_next(&senum, (*proc)(pt.x, pt.y))) < 0 )
			return code;
	if ( code < 0 ) return code;
	pgs->ht_proc = proc;
	set_phase(pgs);
	return 0;
}

/* currentscreen */
int
gs_currentscreen(gs_state *pgs,
  float *pfreq, float *pangle, float (**pproc)(P2(floatp, floatp)))
{	halftone_params *pht = pgs->halftone;
	*pfreq = pht->frequency;
	*pangle = pht->angle;
	*pproc = pgs->ht_proc;
	return 0;
}

/* sethalftonephase */
int
gs_sethalftonephase(gs_state *pgs, int x, int y)
{	pgs->ht_phase.x = x;
	pgs->ht_phase.y = y;
	set_phase(pgs);
	return 0;
}

/* currenthalftonephase */
int
gs_currenthalftonephase(gs_state *pgs, gs_int_point *pphase)
{	*pphase = pgs->ht_phase;
	return 0;
}

/* ------ Halftone sampling ------ */

/* Set up for halftone sampling */
typedef struct rat_s { int num, denom; } rat_t;
private float adjust_screen_angle(P2(floatp, rat_t *));
/* There may be a fmod function and/or macro defined.... */
#define fmodu(a, b) ((a) - floor((a) / (b)) * (b))
int
gs_screen_init(gs_screen_enum *penum, gs_state *pgs,
  floatp freq, floatp angle)
{	int cell_width, cell_height;
	int tile_width, tile_height;
	int code;
	ht_bit *order;
	rat_t arat;
	float copies;
	if ( freq < 0.1 ) return_error(gs_error_rangecheck);
	/* Convert the frequency to cell width and height */
	   {	float cell_size = 72.0 / freq;
		gs_point pcwh;
		gs_matrix imat;
		gs_deviceinitialmatrix(gs_currentdevice(pgs), &imat);
		if ( (code = gs_distance_transform(cell_size, cell_size,
						   &imat, &pcwh)) < 0
		    ) return code;
		/* It isn't clear to me whether we should round the */
		/* width and height, truncate them, or do something */
		/* more complicated.  All the problems arise from devices */
		/* whose X and Y resolutions aren't the same: */
		/* the halftone model isn't really designed for this. */
		/* For the moment, truncate and hope for the best. */
#define abs_round(z) (z < 0 ? -(int)(z) : (int)(z))
/*#define abs_round(z) (z < 0 ? -(int)(z - 0.5) : (int)(z + 0.5))*/
		cell_width = abs_round(pcwh.x);
		cell_height = abs_round(pcwh.y);
#undef abs_round
	   }
	/* Force a halfway reasonable cell size. */
	if ( cell_width <= 4 ) cell_width = 4;
	if ( cell_height <= 4 ) cell_height = 4;
	angle = adjust_screen_angle(angle, &arat);
	copies = hypot((float)arat.num, (float)arat.denom);
	tile_width = cell_width * copies;
	tile_height = cell_height * copies;
	if ( tile_width > max_ushort / tile_height )
		return_error(gs_error_limitcheck);
	order = (ht_bit *)gs_malloc(tile_width * tile_height, sizeof(ht_bit),
				    "halftone samples");
	if ( order == 0 ) return_error(gs_error_VMerror);
	penum->ht.frequency = freq;
	penum->ht.angle = angle;
	penum->ht.order = order;
	penum->ht.width = tile_width;
	penum->ht.height = tile_height;
	penum->ht.order_size = tile_width * tile_height;
	penum->x = penum->y = 0;
	penum->pgs = pgs;
	/* The transformation matrix must include normalization to the */
	/* interval (-1..1), and rotation by the negative of the angle. */
	   {	float xscale = 2.0 / cell_width;
		float yscale = 2.0 / cell_height;
		gs_make_rotation(-angle, &penum->mat);
		penum->mat.xx *= xscale, penum->mat.xy *= xscale;
		penum->mat.yx *= yscale, penum->mat.yy *= yscale;
		penum->mat.tx = -1.0;
		penum->mat.ty = -1.0;
		if_debug8('h', "[h]Screen: %dx%d -> %dx%d [%f %f %f %f]\n",
			  cell_width, cell_height, tile_width, tile_height,
			  penum->mat.xx, penum->mat.xy,
			  penum->mat.yx, penum->mat.yy);
	   }
	return 0;
}

/* Adjust the angle to one with a rational tangent with */
/* small numerator and denominator. */
private float
adjust_screen_angle(floatp fang, rat_t *prat)
{	float tang = fmodu(fang, 90) * degrees_to_radians;
	int quadrant = (int)fang / 90 % 4;
	const rat_t *ptrat;
	float best_diff, best_ang;
	static const rat_t rattab[9] =
	   { {0,1}, {1,3}, {1,2}, {2,3}, {1,1}, {3,2}, {2,1}, {3,1}, {1,0} };
	for ( ptrat = rattab, best_diff = M_PI; ptrat->denom != 0; ptrat++ )
	   {	float rang = atan2((double)ptrat->num, (double)ptrat->denom);
		float diff = fabs(tang - rang);
		if ( diff < best_diff )
			best_diff = diff, best_ang = rang, *prat = *ptrat;
	   }
	/* If we are in an odd quadrant, swap num and denom. */
	if ( quadrant & 1 )
	   {	int temp = prat->num;
		prat->num = prat->denom;
		prat->denom = temp;
	   }
	return best_ang * radians_to_degrees + quadrant * 90;
}

/* Report current point for sampling */
private int gx_screen_finish(P1(gs_screen_enum *));
int
gs_screen_currentpoint(gs_screen_enum *penum, gs_point *ppt)
{	gs_point pt;
	int code;
	if ( penum->y >= penum->ht.height )	/* all done */
		return gx_screen_finish(penum);
	if ( (code = gs_point_transform(penum->x + 0.5, penum->y + 0.5, &penum->mat, &pt)) < 0 )
		return code;
	while ( pt.x < -1.0 ) pt.x += 2.0;
	while ( pt.x >= 1.0 ) pt.x -= 2.0;
	while ( pt.y < -1.0 ) pt.y += 2.0;
	while ( pt.y >= 1.0 ) pt.y -= 2.0;
	*ppt = pt;
	return 0;
}

/* Record next halftone sample */
int
gs_screen_next(gs_screen_enum *penum, floatp value)
{	ushort sample;
	if ( value < -1.0 || value > 1.0 )
	  return_error(gs_error_rangecheck);
	/* The following statement was split into two */
	/* to work around a bug in the Siemens C compiler. */
	sample = (ushort)(value * (float)(int)(max_ushort >> 1));
	sample += (max_ushort >> 1);	/* convert from signed to biased */
#ifdef DEBUG
if ( gs_debug['h'] )
   {	gs_point pt;
	gs_screen_currentpoint(penum, &pt);
	dprintf6("[h]sample x=%d y=%d (%f,%f): %f -> %u\n",
		 penum->x, penum->y, pt.x, pt.y, value, sample);
   }
#endif
	penum->ht.order[penum->y * penum->ht.width + penum->x].mask = sample;
	if ( ++(penum->x) >= penum->ht.width )
		penum->x = 0, ++(penum->y);
	return 0;
}

/* All points have been sampled. */
/* Finish constructing the halftone. */
private int
gx_screen_finish(gs_screen_enum *penum)
{	ht_bit *order = penum->ht.order;
	uint size = penum->ht.order_size;
	uint i;
	int code;
	/* Label each element with its ordinal position. */
	for ( i = 0; i < size; i++ )
		order[i].offset = i;
	/* Sort the samples in increasing order by value. */
	gx_sort_ht_order(order, size);
	/* Set up the actual halftone description. */
	code = gx_ht_construct_order(order, penum->ht.width, penum->ht.height);
	if ( code < 0 ) return code;
	gx_ht_install(penum->pgs, &penum->ht);
	code = gx_remap_color(penum->pgs);
	if ( code < 0 ) return code;
	return 1;			/* all done */
}

/* ------ Internal routines ------ */

/* Compute the negated halftone phase mod the tile size. */
/* This is the displacement of the tile relative to the device coordinates. */
private void
set_phase(gs_state *pgs)
{	halftone_params *pht = pgs->halftone;
	if ( pht->width == 0 )
		pgs->phase_mod.x = 0;
	else
	   {	if ( (pgs->phase_mod.x = -pgs->ht_phase.x % pht->width) < 0 )
			pgs->phase_mod.x += pht->width;
	   }
	if ( pht->height == 0 )
		pgs->phase_mod.y = 0;
	else
	   {	if ( (pgs->phase_mod.y = -pgs->ht_phase.y % pht->height) < 0 )
			pgs->phase_mod.y += pht->height;
	   }
}

/* Heapsort (algorithm 5.2.3H, Knuth vol. 2, p. 146), */
/* modified for 0-origin indexing. */
private void
gx_sort_ht_order(ht_bit *recs, uint N)
{	uint l = N >> 1;
	uint r = N - 1;
	uint j;
	ht_bit R;
	if ( N <= 1 ) return;
#define key(m) recs[m].mask
#define K R.mask
	while ( 1 )
	   {	if ( l > 0 )
			R = recs[--l];
		else
		   {	R = recs[r];
			recs[r] = recs[0];
			if ( --r == 0 )
			   {	recs[0] = R;
				break;
			   }
		   }
		j = l;
		while ( 1 )
		   {	uint i = j;
			j = j + j + 1;
			if ( j < r )
				if ( key(j) < key(j + 1) ) j++;
			if ( j > r || K >= key(j) )
			   {	recs[i] = R;
				break;	/* to outer loop */
			   }
			recs[i] = recs[j];
		   }
	   }
}
