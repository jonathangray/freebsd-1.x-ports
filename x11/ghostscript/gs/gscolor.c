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

/* gscolor.c */
/* Color and halftone operators for Ghostscript library */
#include "gx.h"
#include "gserrors.h"
#include "gscspace.h"
#include "gxcolor.h"
#include "gxdevice.h"			/* for gx_color_index */
#include "gxrefct.h"
#include "gzstate.h"
#include "gzcolor.h"

/* Define the standard color space types. */
extern cs_proc_remap_color(gx_remap_DeviceGray);
extern cs_proc_remap_color(gx_remap_DeviceRGB);
extern cs_proc_remap_color(gx_remap_DeviceCMYK);
const gs_color_space_type
	gs_color_space_type_DeviceGray =
	 { gs_color_space_index_DeviceGray, 1,
	   gx_remap_DeviceGray, gx_no_install_cspace, gx_no_adjust_count
	 },
	gs_color_space_type_DeviceRGB =
	 { gs_color_space_index_DeviceRGB, 3,
	   gx_remap_DeviceRGB, gx_no_install_cspace, gx_no_adjust_count
	 },
	gs_color_space_type_DeviceCMYK =
	 { gs_color_space_index_DeviceCMYK, 4,
	   gx_remap_DeviceCMYK, gx_no_install_cspace, gx_no_adjust_count
	 };

/* Null color space installation procedure. */
int
gx_no_install_cspace(gs_color_space *pcs, gs_state *pgs)
{	return 0;
}

/* Null reference count adjustment procedure. */
int
gx_no_adjust_count(gs_color_space *pcs, gs_state *pgs, int delta)
{	return 0;
}

/* Force a parameter into the range [0.0..1.0]. */
#define force_unit(p) (p < 0.0 ? 0.0 : p > 1.0 ? 1.0 : p)
frac gx_color_unit_param(P1(floatp));

/* Forward declarations */
private void load_transfer_map(P2(gs_state *, gx_transfer_map *));

/* setgray */
int
gs_setgray(gs_state *pgs, floatp gray)
{	int code;
	if ( pgs->in_cachedevice ) return_error(gs_error_undefined);
	code = cs_adjust_count(pgs, -1);
	if ( code < 0 ) return code;
	pgs->ccolor->paint.values[0] = gray;
	pgs->color_space->type = &gs_color_space_type_DeviceGray;
	return gx_remap_color(pgs);
}

/* currentgray */
float
gs_currentgray(const gs_state *pgs)
{	gs_client_color *pcc = pgs->ccolor;
	switch ( pgs->color_space->type->index )
	{
	case gs_color_space_index_DeviceGray:
		return pcc->paint.values[0];
	case gs_color_space_index_DeviceRGB:
		return frac2float(color_rgb_to_gray(
			float2frac(pcc->paint.values[0]),
			float2frac(pcc->paint.values[1]),
			float2frac(pcc->paint.values[2]),
			pgs));
	case gs_color_space_index_DeviceCMYK:
		return frac2float(color_cmyk_to_gray(
			float2frac(pcc->paint.values[0]),
			float2frac(pcc->paint.values[1]),
			float2frac(pcc->paint.values[2]),
			float2frac(pcc->paint.values[3]),
			pgs));
	default:
		return 0.0;
	}
}

/* sethsbcolor */
int
gs_sethsbcolor(gs_state *pgs, floatp h, floatp s, floatp b)
{	float rgb[3];
	color_hsb_to_rgb(force_unit(h), force_unit(s), force_unit(b), rgb);
	return gs_setrgbcolor(pgs, rgb[0], rgb[1], rgb[2]);
}

/* currenthsbcolor */
int
gs_currenthsbcolor(const gs_state *pgs, float pr3[3])
{	float rgb[3];
	gs_currentrgbcolor(pgs, rgb);
	color_rgb_to_hsb(rgb[0], rgb[1], rgb[2], pr3);
	return 0;
}

/* setrgbcolor */
int
gs_setrgbcolor(gs_state *pgs, floatp r, floatp g, floatp b)
{	int code;
	gs_client_color *pcc = pgs->ccolor;
	if ( pgs->in_cachedevice ) return_error(gs_error_undefined);
	code = cs_adjust_count(pgs, -1);
	if ( code < 0 ) return code;
	pcc->paint.values[0] = r;
	pcc->paint.values[1] = g;
	pcc->paint.values[2] = b;
	pgs->color_space->type = &gs_color_space_type_DeviceRGB;
	return gx_remap_color(pgs);
}

/* currentrgbcolor */
int
gs_currentrgbcolor(const gs_state *pgs, float pr3[3])
{	gs_client_color *pcc = pgs->ccolor;
	switch ( pgs->color_space->type->index )
	{
	case gs_color_space_index_DeviceGray:
		pr3[0] = pr3[1] = pr3[2] = pcc->paint.values[0];
		break;
	case gs_color_space_index_DeviceRGB:
		pr3[0] = pcc->paint.values[0];
		pr3[1] = pcc->paint.values[1];
		pr3[2] = pcc->paint.values[2];
		break;
	case gs_color_space_index_DeviceCMYK:
	{	frac frgb[3];
		color_cmyk_to_rgb(
			float2frac(pcc->paint.values[0]),
			float2frac(pcc->paint.values[1]),
			float2frac(pcc->paint.values[2]),
			float2frac(pcc->paint.values[3]),
			pgs, frgb);
		pr3[0] = frac2float(frgb[0]);
		pr3[1] = frac2float(frgb[1]);
		pr3[2] = frac2float(frgb[2]);
	}	break;
	default:
		pr3[0] = pr3[1] = pr3[2] = 0.0;
	}
	return 0;
}

/* setcmykcolor */
int
gs_setcmykcolor(gs_state *pgs, floatp c, floatp m, floatp y, floatp k)
{	int code;
	gs_client_color *pcc = pgs->ccolor;
	if ( pgs->in_cachedevice ) return_error(gs_error_undefined);
	code = cs_adjust_count(pgs, -1);
	if ( code < 0 ) return code;
	pcc->paint.values[0] = c;
	pcc->paint.values[1] = m;
	pcc->paint.values[2] = y;
	pcc->paint.values[3] = k;
	pgs->color_space->type = &gs_color_space_type_DeviceCMYK;
	return gx_remap_color(pgs);
}

/* currentcmykcolor */
int
gs_currentcmykcolor(const gs_state *pgs, float pr4[4])
{	gs_client_color *pcc = pgs->ccolor;
	switch ( pgs->color_space->type->index )
	{
	case gs_color_space_index_DeviceGray:
		pr4[0] = pr4[1] = pr4[2] = 0.0;
		pr4[3] = 1.0 - pcc->paint.values[0];
		break;
	case gs_color_space_index_DeviceRGB:
	{	frac fcmyk[4];
		color_rgb_to_cmyk(
			float2frac(pcc->paint.values[0]),
			float2frac(pcc->paint.values[1]),
			float2frac(pcc->paint.values[2]),
			pgs, fcmyk);
		pr4[0] = frac2float(fcmyk[0]);
		pr4[1] = frac2float(fcmyk[1]);
		pr4[2] = frac2float(fcmyk[2]);
		pr4[3] = frac2float(fcmyk[3]);
	}	break;
	case gs_color_space_index_DeviceCMYK:
		pr4[0] = pcc->paint.values[0];
		pr4[1] = pcc->paint.values[1];
		pr4[2] = pcc->paint.values[2];
		pr4[3] = pcc->paint.values[3];
		break;
	default:
		pr4[0] = pr4[1] = pr4[2] = 0.0;
		pr4[3] = 1.0;
	}
	return 0;
}

/* setblackgeneration */
int
gs_setblackgeneration(gs_state *pgs, gs_mapping_proc proc)
{	/****** INCOMPLETE ******/
	pgs->black_generation = proc;
	return 0;
}

/* currentblackgeneration */
gs_mapping_proc
gs_currentblackgeneration(const gs_state *pgs)
{	return pgs->black_generation;
}

/* setundercolorremoval */
int
gs_setundercolorremoval(gs_state *pgs, gs_mapping_proc proc)
{	/****** INCOMPLETE ******/
	pgs->undercolor_removal = proc;
	return 0;
}

/* currentundercolorremoval */
gs_mapping_proc
gs_currentundercolorremoval(const gs_state *pgs)
{	return pgs->undercolor_removal;
}

/* settransfer */
/* Remap=0 is used by the interpreter. */
int
gs_settransfer_remap(gs_state *pgs, gs_mapping_proc tproc, int remap)
{	gx_transfer *ptran = &pgs->transfer;
	/* We can safely decrement the reference counts */
	/* of the non-gray transfer maps, because */
	/* if any of them get freed, the rc_unshare can't fail. */
	rc_decrement(ptran->red, pgs->memory_procs, "gs_settransfer");
	rc_decrement(ptran->green, pgs->memory_procs, "gs_settransfer");
	rc_decrement(ptran->blue, pgs->memory_procs, "gs_settransfer");
	rc_unshare(ptran->gray, gx_transfer_map, pgs->memory_procs,
		   goto fail, "gs_settransfer");
	ptran->gray->proc = tproc;
	ptran->red = ptran->gray;
	ptran->green = ptran->gray;
	ptran->blue = ptran->gray;
	ptran->gray->rc.ref_count += 3;
	if ( remap )
	{	load_transfer_map(pgs, ptran->gray);
		return gx_remap_color(pgs);
	}
	else
		return 0;
fail:	rc_increment(ptran->red);
	rc_increment(ptran->green);
	rc_increment(ptran->blue);
	return gs_error_VMerror;
}
int
gs_settransfer(gs_state *pgs, gs_mapping_proc tproc)
{	return gs_settransfer_remap(pgs, tproc, 1);
}

/* currenttransfer */
gs_mapping_proc
gs_currenttransfer(const gs_state *pgs)
{	return pgs->transfer.gray->proc;
}

/* setcolortransfer */
/* Remap=0 is used by the interpreter. */
int
gs_setcolortransfer_remap(gs_state *pgs, gs_mapping_proc red_proc,
  gs_mapping_proc green_proc, gs_mapping_proc blue_proc,
  gs_mapping_proc gray_proc, int remap)
{	gx_transfer *ptran = &pgs->transfer;
	gx_transfer old;
	old = *ptran;
	rc_unshare(ptran->gray, gx_transfer_map, pgs->memory_procs,
		   goto fgray, "gs_setcolortransfer");
	rc_unshare(ptran->red, gx_transfer_map, pgs->memory_procs,
		   goto fred, "gs_setcolortransfer");
	rc_unshare(ptran->green, gx_transfer_map, pgs->memory_procs,
		   goto fgreen, "gs_setcolortransfer");
	rc_unshare(ptran->blue, gx_transfer_map, pgs->memory_procs,
		   goto fblue, "gs_setcolortransfer");
	ptran->red->proc = red_proc;
	ptran->green->proc = green_proc;
	ptran->blue->proc = blue_proc;
	ptran->gray->proc = gray_proc;
	if ( remap )
	{	load_transfer_map(pgs, ptran->red);
		load_transfer_map(pgs, ptran->green);
		load_transfer_map(pgs, ptran->blue);
		load_transfer_map(pgs, ptran->gray);
		return gx_remap_color(pgs);
	}
	else
		return 0;
fblue:	rc_assign(ptran->green, old.green, pgs->memory_procs, "setcolortransfer");
fgreen:	rc_assign(ptran->red, old.red, pgs->memory_procs, "setcolortransfer");
fred:	rc_assign(ptran->gray, old.gray, pgs->memory_procs, "setcolortransfer");
fgray:	return gs_error_VMerror;
}
int
gs_setcolortransfer(gs_state *pgs, gs_mapping_proc red_proc,
  gs_mapping_proc green_proc, gs_mapping_proc blue_proc,
  gs_mapping_proc gray_proc)
{	return gs_setcolortransfer_remap(pgs, red_proc, green_proc,
					 blue_proc, gray_proc, 1);
}

/* currentcolortransfer */
void
gs_currentcolortransfer(const gs_state *pgs, gs_mapping_proc procs[4])
{	const gx_transfer *ptran = &pgs->transfer;
	procs[0] = ptran->red->proc;
	procs[1] = ptran->green->proc;
	procs[2] = ptran->blue->proc;
	procs[3] = ptran->gray->proc;
}

/* ------ Non-operator routines ------ */

/* Set up black for writing into the character cache. */
void
gx_set_black(gs_state *pgs)
{	gx_device_color *pdc = pgs->dev_color;
	gs_client_color *pc = pgs->ccolor;
	pc->paint.values[0] = 0.0;
	pgs->color_space->type = &gs_color_space_type_DeviceGray;
	pdc->color1 = pdc->color2 = 1;
	pdc->halftone_level = 0;
}

/* Force a parameter into the range [0..1], */
/* and convert to a frac. */
frac
gx_color_unit_param(floatp fval)
{	if ( fval <= 0.0 )
		return frac_0;
	else if ( fval >= 1.0 )
		return frac_1;
	else
		return float2frac(fval);
}

/* ------ Internal routines ------ */

/* Load one cached transfer map. */
private void
load_transfer_map(gs_state *pgs, gx_transfer_map *pmap)
{	gs_mapping_proc proc = pmap->proc;
	frac *values = pmap->values;
	int i;
	for ( i = 0; i < transfer_map_size; i++ )
	  values[i] = gx_color_unit_param((*proc)(pgs, (float)i / (transfer_map_size - 1)));
}
