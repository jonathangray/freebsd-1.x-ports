/* Copyright (C) 1990, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gxhint2.c */
/* Character level hints for Type 1 fonts. */
#include "gx.h"
#include "gserrors.h"
#include "gxarith.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"			/* for gxchar */
#include "gxdevmem.h"			/* ditto */
#include "gxchar.h"
#include "gxfont.h"
#include "gxtype1.h"
#include "gxop1.h"

/* Define the tolerance for testing whether a point is in a zone, */
/* in device pixels.  (Maybe this should be variable?) */
#define stem_tolerance float2fixed(0.05)

/* Forward references */

private stem_hint *near type1_stem(P3(stem_hint_table *, fixed, fixed));
private fixed near find_snap(P3(fixed, const stem_snap_table *, const pixel_scale *));
private alignment_zone *near find_zone(P3(gs_type1_state *, fixed, fixed));

/* Reset the stem hints. */
void
reset_stem_hints(register gs_type1_state *pis)
{	pis->hstem_hints.count = 0;
	pis->hstem_hints.current = &pis->hstem_hints.data[0];
	pis->vstem_hints.count = 0;
	pis->vstem_hints.current = &pis->vstem_hints.data[0];
}

/* ------ Add hints ------ */

#define c_fixed(d, c) m_fixed(d, c, pis->fc, max_coeff_bits)

/* Add a horizontal stem hint. */
void
type1_hstem(register gs_type1_state *pis, fixed y, fixed dy)
{	stem_hint *psh;
	alignment_zone *pz;
	fixed v, dv, adj_dv;
	fixed vtop, vbot;
	fixed center, diff_v, diff2_dv;
	if ( !pis->fh.use_y_hints ) return;
	y += pis->lsb.y;
	if ( pis->fh.axes_swapped )
		v = pis->vs_offset.x + c_fixed(y, yx) +
			pis->pgs->ctm.tx_fixed,
		dv = c_fixed(dy, yx);
	else
		v = pis->vs_offset.y + c_fixed(y, yy) +
			pis->pgs->ctm.ty_fixed,
		dv = c_fixed(dy, yy);
	if ( dy < 0 )
		vbot = v + dv, vtop = v;
	else
		vbot = v, vtop = v + dv;
	if ( dv < 0 ) v += dv, dv = -dv;
	psh = type1_stem(&pis->hstem_hints, v, dv);
	if ( psh == 0 ) return;
	adj_dv = find_snap(dv, &pis->fh.snap_h, &pis->scale);
	pz = find_zone(pis, vbot, vtop);
	if ( pz != 0 )
	{	/* Use the alignment zone to align the outer stem edge. */
		int inverted =
		  (pis->fh.axes_swapped ? pis->fh.x_inverted : pis->fh.y_inverted);
		int adjust_v1 =
		  (inverted ? !pz->is_top_zone : pz->is_top_zone);
		fixed flat_v = pz->flat;
		fixed overshoot =
			(pz->is_top_zone ? vtop - flat_v : flat_v - vbot);
		fixed pos_over =
			(inverted ? -overshoot : overshoot);
		fixed ddv = adj_dv - dv;
		fixed shift = scaled_rounded(flat_v, &pis->scale) - flat_v;
		if ( pos_over > 0 )
		{
		  if ( pos_over < pis->fh.blue_shift || pis->fh.suppress_overshoot )
		  {	/* Character is small, suppress overshoot. */
			if_debug0('y', "[y]suppress overshoot\n");
			if ( pz->is_top_zone )
				shift -= overshoot;
			else
				shift += overshoot;
		  }
		  else
		  if ( pos_over < pis->scale.unit )
		  {	/* Enforce overshoot. */
			if_debug0('y', "[y]enforce overshoot\n");
			if ( overshoot < 0 )
				overshoot = -pis->scale.unit - overshoot;
			else
				overshoot = pis->scale.unit - overshoot;
			if ( pz->is_top_zone )
				shift += overshoot;
			else
				shift -= overshoot;
		  }
		}
		if ( adjust_v1 )
			psh->dv1 = shift, psh->dv0 = shift - ddv;
		else
			psh->dv0 = shift, psh->dv1 = shift + ddv;
		if_debug4('y', "[y]flat_v = %g, overshoot = %g, dv=%g,%g\n",
			  fixed2float(flat_v), fixed2float(overshoot),
			  fixed2float(psh->dv0), fixed2float(psh->dv1));
		return;
	}
	/* Align the stem so its edges fall on pixel boundaries, */
	/* moving the center as little as possible. */
	center = v + arith_rshift_1(dv);
	if ( adj_dv & pis->scale.unit )
	{	/* Odd width, align center on half-pixel. */
		center += pis->scale.half;
	}
	diff_v = scaled_rounded(center, &pis->scale) - center;
	diff2_dv = arith_rshift_1(adj_dv - dv);
	psh->dv0 = diff_v - diff2_dv;
	psh->dv1 = diff_v + diff2_dv;
	if_debug6('y', "[y]hstem %g,%g -> %g,%g ; d = %g,%g\n",
		  fixed2float(y), fixed2float(dy),
		  fixed2float(v), fixed2float(dv),
		  fixed2float(psh->dv0), fixed2float(psh->dv1));
}

/* Add a vertical stem hint. */
void
type1_vstem(register gs_type1_state *pis, fixed x, fixed dx)
{	stem_hint *psh;
	fixed v, dv, adj_dv;
	fixed center, diff_v, diff2_dv;
	if ( !pis->fh.use_x_hints ) return;
	x += pis->lsb.x;
	if ( pis->fh.axes_swapped )
		v = pis->vs_offset.y + c_fixed(x, xy) +
			pis->pgs->ctm.ty_fixed,
		dv = c_fixed(dx, xy);
	else
		v = pis->vs_offset.x + c_fixed(x, xx) +
			pis->pgs->ctm.tx_fixed,
		dv = c_fixed(dx, xx);
	if ( dv < 0 ) v += dv, dv = -dv;
	psh = type1_stem(&pis->vstem_hints, v, dv);
	if ( psh == 0 ) return;
	adj_dv = find_snap(dv, &pis->fh.snap_v, &pis->scale);
	if ( pis->pdata->ForceBold && adj_dv < pis->scale.unit )
		adj_dv = pis->scale.unit;
	/* Align the stem so its edges fall on pixel boundaries, */
	/* moving the center as little as possible. */
	center = v + arith_rshift_1(dv);
	if ( adj_dv & pis->scale.unit )
	{	/* Odd width, align center on half-pixel. */
		center += pis->scale.half;
	}
	diff_v = scaled_rounded(center, &pis->scale) - center;
	diff2_dv = arith_rshift_1(adj_dv - dv);
	psh->dv0 = diff_v - diff2_dv;
	psh->dv1 = diff_v + diff2_dv;
	if_debug6('y', "[y]vstem %g,%g -> %g,%g ; d = %g,%g\n",
		  fixed2float(x), fixed2float(dx),
		  fixed2float(v), fixed2float(dv),
		  fixed2float(psh->dv0), fixed2float(psh->dv1));
}

/* Adjust the character center for a vstem3. */
/****** NEEDS UPDATING FOR SCALE ******/
void
center_vstem(gs_type1_state *pis, fixed x0, fixed dx)
{	fixed x1 = x0 + dx;
	gs_fixed_point pt0, pt1, width;
	fixed center, int_width;
	fixed *psxy;
	gs_point_transform2fixed(&pis->pgs->ctm,
		fixed2float(x0), 0.0, &pt0);
	gs_point_transform2fixed(&pis->pgs->ctm,
		fixed2float(x1), 0.0, &pt1);
	width.x = pt0.x - pt1.x;
	if ( width.x < 0 ) width.x = - width.x;
	width.y = pt0.y - pt1.y;
	if ( width.y < 0 ) width.y = - width.y;
	if ( width.y < float2fixed(0.05) )
	{	/* Vertical on device */
		center = arith_rshift_1(pt0.x + pt1.x);
		int_width = fixed2int_rounded(width.x);
		psxy = &pis->vs_offset.x;
	}
	else
	{	/* Horizontal on device */
		center = arith_rshift_1(pt0.y + pt1.y);
		int_width = fixed2int_rounded(width.y);
		psxy = &pis->vs_offset.y;
	}
	if ( int_width == 0 || (int_width & 1) == 0 )
	{	/* Odd width, center stem over pixel. */
		*psxy = fixed_floor(center) + fixed_half - center;
	}
	else
	{	/* Even width, center stem between pixels. */
		*psxy = fixed_rounded(center) - center;
	}
	/* We can't fix up the current point here, */
	/* but we can fix up everything else. */
	/****** TO BE COMPLETED ******/
}

/* Add a stem hint, keeping the table sorted. */
/* Return the stem hint pointer, or 0 if the table is full. */
private stem_hint *near
type1_stem(stem_hint_table *psht, fixed v0, fixed d)
{	stem_hint *bot = &psht->data[0];
	stem_hint *top = bot + psht->count;
	if ( psht->count >= max_stems ) return 0;
	while ( top > bot && v0 < top[-1].v0 )
	   {	*top = top[-1];
		top--;
	   }
	/* Add a little fuzz for insideness testing. */
	top->v0 = v0 - stem_tolerance;
	top->v1 = v0 + d + stem_tolerance;
	psht->count++;
	return top;
}

/* Compute the adjusted width of a stem. */
/* The value returned is always a multiple of scale.unit. */
private fixed near
find_snap(fixed dv, const stem_snap_table *psst, const pixel_scale *pps)
{	fixed best = pps->half;
	fixed adj_dv;
	int i;
	for ( i = 0; i < psst->count; i++ )
	{	fixed diff = psst->data[i] - dv;
		if ( any_abs(diff) < any_abs(best) )
		{	if_debug3('Y', "[Y]possibly snap %g to [%d]%g\n",
				  fixed2float(dv), i,
				  fixed2float(psst->data[i]));
			best = diff;
		}
	}
	adj_dv = scaled_rounded((any_abs(best) < pps->half ? dv + best : dv),
				pps);
	if ( adj_dv == 0 )
		adj_dv = pps->unit;
#ifdef DEBUG
	if ( adj_dv == dv )
		if_debug1('Y', "[Y]no snap %g\n", fixed2float(dv));
	else
		if_debug2('Y', "[Y]snap %g to %g\n",
			  fixed2float(dv), fixed2float(adj_dv));
#endif
	return adj_dv;
}

/* Find the applicable alignment zone for a stem, if any. */
/* vbot and vtop are the bottom and top of the stem, */
/* but without interchanging if the y axis is inverted. */
private alignment_zone *near
find_zone(gs_type1_state *pis, fixed vbot, fixed vtop)
{	alignment_zone *pz;
	for ( pz = &pis->fh.a_zones[pis->fh.a_zone_count];
	      --pz >= &pis->fh.a_zones[0];
	    )
	{	fixed v = (pz->is_top_zone ? vtop : vbot);
		if ( v >= pz->v0 && v <= pz->v1 )
		{	if_debug2('Y', "[Y]stem crosses %s-zone %d\n",
				  (pz->is_top_zone ? "top" : "bottom"),
				  (int)(pz - &pis->fh.a_zones[0]));
			return pz;
		}
	}
	return 0;
}

/* ------ Apply hints ------ */

private stem_hint *near search_hints(P2(stem_hint_table *, fixed));

/* Adjust a point according to the relevant hints. */
/* x and y are the current point in device space after moving; */
/* dx and dy are the delta components in character space. */
void
find_stem_hints(gs_type1_state *pis, fixed x, fixed y,
  fixed dx, fixed dy, gs_fixed_point *ppt)
{	ppt->x = x, ppt->y = y;
	if ( pis->in_dotsection ) return;
	/* Note that if use_x/y_hints is false, */
	/* no entries ever get made in the stem hint tables, */
	/* so we don't have to check those flags here. */
	/* Check the vertical stem hints. */
	if ( pis->vstem_hints.count )
	{	fixed *pv = (pis->fh.axes_swapped ? &ppt->y : &ppt->x);
		stem_hint *ph = search_hints(&pis->vstem_hints, *pv);
		if ( ph != 0 )
		{	/* Decide which side of the stem we are on. */
			/* If we're moving horizontally, just use the */
			/* x coordinate; otherwise, assume outside */
			/* edges move clockwise and inside edges move */
			/* counter-clockwise.  (This algorithm was */
			/* taken from the IBM X11R5 rasterizer; I'm not */
			/* sure I believe it.) */
#define adjust_stem(pv, dxy, ph, inverted)\
  *pv +=\
    ((/*dxy == 0 ?*/ *pv < arith_rshift_1(ph->v0 +\
      ph->v1) /*: inverted ? dxy < 0 : dxy > 0*/) ?\
     ph->dv0 : ph->dv1)
			if_debug2('Y', "[Y]use vstem %d: %g",
				  (int)(ph - &pis->vstem_hints.data[0]),
				  fixed2float(*pv));
			adjust_stem(pv, dy, ph, pis->fh.y_inverted);
			if_debug1('Y', " -> %g\n", fixed2float(*pv));
		}
	}
	/* Check the horizontal stem hints. */
	if ( pis->hstem_hints.count )
	{	fixed *pv = (pis->fh.axes_swapped ? &ppt->x : &ppt->y);
		stem_hint *ph = search_hints(&pis->hstem_hints, *pv);
		if ( ph != 0 )
		{	if_debug2('Y', "[Y]use hstem %d: %g",
				  (int)(ph - &pis->hstem_hints.data[0]),
				  fixed2float(*pv));
			adjust_stem(pv, dx, ph, pis->fh.x_inverted);
			if_debug1('Y', " -> %g\n", fixed2float(*pv));
		}
	}
	return;
#undef adjust_stem
}

/* Search one hint table for an adjustment. */
private stem_hint *near
search_hints(stem_hint_table *psht, fixed v)
{	stem_hint *ph = psht->current;
	if ( v >= ph->v0 && v <= ph->v1 ) return ph;
	/* We don't bother with binary or even up/down search, */
	/* because there won't be very many hints. */
	for ( ph = &psht->data[psht->count]; --ph >= &psht->data[0]; )
	  if ( v >= ph->v0 && v <= ph->v1 )
	    return (psht->current = ph);
	return 0;
}

