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

/* gscolor2.c */
/* Level 2 color and halftone operators for Ghostscript library */
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"			/* ditto */
#include "gxmatrix.h"			/* for gzstate.h */
#include "gxdevice.h"			/* for gx_color_index */
#include "gxrefct.h"
#include "gxcolor.h"			/* for gscolor2.h, gscie.h */
#include "gscspace.h"			/* for gscolor2.h */
#include "gscolor2.h"			/* for gscie.h */
#include "gzstate.h"
#include "gzcolor.h"
#include "gscie.h"

/*
 * NOTE: Ghostscript does not currently implement the Pattern color space.
 * This is the module where that limitation is enforced,
 * since setcolorspace is the only way that a non-Device color space
 * can be made current (other than specialized operators such as setpattern.)
 */

/* Define the Level 2 color space types. */
#define cs_procs(scope, remap, install, adjust)\
  scope cs_proc_remap_color(remap);\
  scope cs_proc_install_cspace(install);\
  scope cs_proc_adjust_count(adjust)
cs_procs(extern, gx_remap_CIEBasedABC, gx_install_CIEBasedABC,
  gx_adjust_CIEBasedABC);
cs_procs(extern, gx_remap_CIEBasedA, gx_install_CIEBasedA,
  gx_adjust_CIEBasedA);
cs_procs(private, gx_remap_Separation, gx_install_Separation,
  gx_adjust_Separation);
cs_procs(private, gx_remap_Indexed, gx_install_Indexed,
  gx_adjust_Indexed);
cs_procs(private, gx_remap_Pattern, gx_install_Pattern,
  gx_adjust_Pattern);
const gs_color_space_type
	gs_color_space_type_CIEBasedABC =
	 { gs_color_space_index_CIEBasedABC, 3,
	   gx_remap_CIEBasedABC, gx_install_CIEBasedABC, gx_adjust_CIEBasedABC
	 },
	gs_color_space_type_CIEBasedA =
	 { gs_color_space_index_CIEBasedA, 1,
	   gx_remap_CIEBasedA, gx_install_CIEBasedA, gx_adjust_CIEBasedA
	 },
	gs_color_space_type_Separation =
	 { gs_color_space_index_Separation, 1,
	   gx_remap_Separation, gx_install_Separation, gx_adjust_Separation
	 },
	gs_color_space_type_Indexed =
	 { gs_color_space_index_Indexed, 1,
	   gx_remap_Indexed, gx_install_Indexed, gx_adjust_Indexed
	 },
	gs_color_space_type_Pattern =
	 { gs_color_space_index_Pattern, -1,
	   gx_remap_Pattern, gx_install_Pattern, gx_adjust_Pattern
	 };

/* setcolorspace */
int
gs_setcolorspace(gs_state *pgs, gs_color_space *pcs)
{	int code;
	gs_color_space *pcs_old = pgs->color_space;
	gs_client_color c;
	if ( pgs->in_cachedevice )
		return_error(gs_error_undefined);
	switch ( pcs->type->index )
	{
	case gs_color_space_index_DeviceCMYK:
		c.paint.values[3] = 1.0;
	case gs_color_space_index_DeviceRGB:
	case gs_color_space_index_CIEBasedABC:
		c.paint.values[2] = c.paint.values[1] = 0.0;
	case gs_color_space_index_DeviceGray:
	case gs_color_space_index_CIEBasedA:
	case gs_color_space_index_Indexed:
		c.paint.values[0] = 0.0;
		break;
	case gs_color_space_index_Separation:
		c.paint.values[0] = 1.0;
		break;
	/*case gs_color_space_index_Pattern:*/	/****** NYI ******/
	default:
		return gs_error_undefined;
	}
	if ( (code = (*pcs->type->adjust_count)(pcs, pgs, 1)) < 0 ||
	     (code = (*pcs_old->type->adjust_count)(pcs_old, pgs, -1)) < 0 ||
	     (*pcs_old = *pcs,
	      (code = (*pcs->type->install_cspace)(pcs, pgs)) < 0)
	   )
		return code;
	return gs_setcolor(pgs, &c);
}

/* currentcolorspace */
const gs_color_space *
gs_currentcolorspace(const gs_state *pgs)
{	return pgs->color_space;
}

/* setcolor */
int
gs_setcolor(gs_state *pgs, const gs_client_color *pcc)
{	const gs_color_space *pcs = pgs->color_space;
	int code;
	if ( pgs->in_cachedevice )
		return_error(gs_error_undefined);
	code = (*pcs->type->remap_color)(pcc, pcs, pgs->dev_color, pgs);
	if ( code < 0 ) return code;
	*pgs->ccolor = *pcc;
	return 0;
}

/* currentcolor */
const gs_client_color *
gs_currentcolor(const gs_state *pgs)
{	return pgs->ccolor;
}

/* setoverprint */
void
gs_setoverprint(gs_state *pgs, int ovp)
{	pgs->overprint = ovp;
}

/* currentoverprint */
int
gs_currentoverprint(const gs_state *pgs)
{	return pgs->overprint;
}

/* ------ Internal routines ------ */

/* Color remapping for Level 2 color spaces. */
/* The CIE-based spaces are handled in gscie.c. */

private int
gx_remap_Separation(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, gs_state *pgs)
{	float tint = pc->paint.values[0];
	int code;
	gs_client_color cc;
	if ( tint < 0 ) tint = 0;
	else if ( tint > 1 ) tint = 1;
	code = (*pcs->params.separation.tint_transform)(tint, &cc.paint.values[0]);
	if ( code < 0 ) return code;
	return (*pcs->params.separation.alt_space.type->remap_color)(&cc,
		(const gs_color_space *)&pcs->params.separation.alt_space,
		 pdc, pgs);
}

private int
gx_remap_Indexed(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, gs_state *pgs)
{	float value = pc->paint.values[0];
	int index =
		(value < 0 ? 0 :
		 value > pcs->params.indexed.hival ?
		   pcs->params.indexed.hival :
		 (int)value);
	gs_client_color cc;
	if ( pcs->params.indexed.use_proc )
	{	int code = (*pcs->params.indexed.lookup.proc)(index, &cc.paint.values[0]);
		if ( code < 0 ) return code;
	}
	else
	{	int m = pcs->params.indexed.base_space.type->num_components;
		const byte *pcomp = pcs->params.indexed.lookup.table + m * index;
		int i;
		for ( i = 0; i < m; i++, pcomp++ )
			cc.paint.values[i] = *pcomp * (1.0 / 255.0);
	}
	return (*pcs->params.indexed.base_space.type->remap_color)(&cc,
		(const gs_color_space *)&pcs->params.indexed.base_space,
		 pdc, pgs);
}

private int
gx_remap_Pattern(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, gs_state *pgs)
{	return gs_error_rangecheck;	/* NYI */
}

/* Color space installation ditto. */

private int
gx_install_Separation(gs_color_space *pcs, gs_state *pgs)
{	return (*pcs->params.separation.alt_space.type->install_cspace)
		((gs_color_space *)&pcs->params.separation.alt_space, pgs);
}

private int
gx_install_Indexed(gs_color_space *pcs, gs_state *pgs)
{	return (*pcs->params.indexed.base_space.type->install_cspace)
		((gs_color_space *)&pcs->params.indexed.base_space, pgs);
}

private int
gx_install_Pattern(gs_color_space *pcs, gs_state *pgs)
{	if ( !pcs->params.pattern.has_base_space )
		return 0;
	return (*pcs->params.pattern.base_space.type->install_cspace)
		((gs_color_space *)&pcs->params.pattern.base_space, pgs);
}

/* Reference count adjustment ditto. */

private int
gx_adjust_Separation(gs_color_space *pcs, gs_state *pgs, int delta)
{	return (*pcs->params.separation.alt_space.type->adjust_count)
		((gs_color_space *)&pcs->params.separation.alt_space, pgs, delta);
}

private int
gx_adjust_Indexed(gs_color_space *pcs, gs_state *pgs, int delta)
{	return (*pcs->params.indexed.base_space.type->adjust_count)
		((gs_color_space *)&pcs->params.indexed.base_space, pgs, delta);
}

private int
gx_adjust_Pattern(gs_color_space *pcs, gs_state *pgs, int delta)
{	/****** SHOULD ALSO REFCT Implementation ******/
	if ( !pcs->params.pattern.has_base_space )
		return 0;
	return (*pcs->params.pattern.base_space.type->adjust_count)
		((gs_color_space *)&pcs->params.pattern.base_space, pgs, delta);
}
