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

/* zcspace2.c */
/* Level 2 color space operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gscolor.h"
#include "gxcolor.h"
#include "gscspace.h"
#include "gscolor2.h"
#include "dict.h"
#include "dparam.h"
#include "estack.h"
#include "iname.h"		/* for name_eq */
#include "state.h"
#include "store.h"

/* Imported from gscolor2.c */
extern const gs_color_space_type
	gs_color_space_type_Separation,
	gs_color_space_type_Indexed,
	gs_color_space_type_Pattern;

/* Imported from zcie.c */
extern int
	zcolorspace_CIEBasedABC(P3(const ref *, gs_color_space *, ref_cie_procs *)),
	zcolorspace_CIEBasedA(P3(const ref *, gs_color_space *, ref_cie_procs *));

/* Forward references */
typedef enum { cs_allow_base, cs_allow_paint, cs_allow_all } cs_allowed;
private int cspace_param(P4(const ref *, gs_color_space *, ref_color_procs *, cs_allowed));

/* Names of color spaces: */
static ref color_space_names[8];
#define name_DeviceGray color_space_names[(int)gs_color_space_index_DeviceGray]
#define name_DeviceRGB color_space_names[(int)gs_color_space_index_DeviceRGB]
#define name_DeviceCMYK color_space_names[(int)gs_color_space_index_DeviceCMYK]
#define name_CIEBasedABC color_space_names[(int)gs_color_space_index_CIEBasedABC]
#define name_CIEBasedA color_space_names[(int)gs_color_space_index_CIEBasedA]
#define name_Separation color_space_names[(int)gs_color_space_index_Separation]
#define name_Indexed color_space_names[(int)gs_color_space_index_Indexed]
#define name_Pattern color_space_names[(int)gs_color_space_index_Pattern]

/* Initialization */
private void
zcspace2_init(void)
{	static const names_def csn[] = {

	/* Create the names of the color spaces. */
	   { "DeviceGray", &name_DeviceGray },
	   { "DeviceRGB", &name_DeviceRGB },
	   { "DeviceCMYK", &name_DeviceCMYK },
	   { "CIEBasedABC", &name_CIEBasedABC },
	   { "CIEBasedA", &name_CIEBasedA },
	   { "Separation", &name_Separation },
	   { "Indexed", &name_Indexed },
	   { "Pattern", &name_Pattern },

	/* Mark the end of the initialized name list. */
	   names_def_end
	};

	init_names(csn);
}


/* currentcolorspace */
int
zcurrentcolorspace(register os_ptr op)
{	push(1);
	if ( r_has_type(&istate.colorspace.array, t_null) )
	{	/* Create the 1-element array on the fly. */
		const gs_color_space *pcs = gs_currentcolorspace(igs);
		make_tasv(op, t_array, a_readonly, 1, refs,
			  &color_space_names[(int)pcs->type->index]);
	}
	else
		*op = istate.colorspace.array;
	return 0;
}

/* setcolorspace */
int
zsetcolorspace(register os_ptr op)
{	gs_color_space cs;
	ref_color_procs procs;
	ref_colorspace cspace_old;
	es_ptr ep = esp;
	int code;
	procs = istate.colorspace.procs;
	code = cspace_param((const ref *)op, &cs, &procs, cs_allow_all);
	if ( code < 0 )
	{	esp = ep;
		return code;
	}
	/* The color space installation procedure may refer to */
	/* istate.colorspace.procs. */
	cspace_old = istate.colorspace;
	if ( r_has_type(op, t_name) )
		make_null(&istate.colorspace.array);	/* no params */
	else
		istate.colorspace.array = *op;
	istate.colorspace.procs = procs;
	code = gs_setcolorspace(igs, &cs);
	if ( code < 0 )
	{	istate.colorspace = cspace_old;
		esp = ep;
		return code;
	}
	pop(1);
	return (esp == ep ? 0 : o_push_estack);
}

/* ------ Initialization procedure ------ */

op_def zcspace2_op_defs[] = {
	{"0currentcolorspace", zcurrentcolorspace},
	{"1setcolorspace", zsetcolorspace},
	op_def_end(zcspace2_init)
};

/* ------ Internal procedures ------ */

/* Dummy indexed lookup procedure. */
private int
null_indexed_lookup_proc(int index, float *values)
{	return 0;
}

/* Extract the parameters for a color space. */
private int
cspace_param(const ref *pcsref, gs_color_space *pcs,
  ref_color_procs *pcprocs, cs_allowed allow)
{	const ref *pcsa, *pcsn;
	uint asize;
	int csi;
	int code;
	if ( r_has_type(pcsref, t_array) )
	{	check_read(*pcsref);
		pcsa = pcsref->value.const_refs;
		asize = r_size(pcsref);
		if ( asize == 0 )
			return_error(e_rangecheck);
	}
	else
	{	pcsa = pcsref;
		asize = 1;
	}
	pcsn = pcsa++;
	asize--;
	check_type(*pcsn, t_name);
	for ( csi = 0; !name_eq(pcsn, &color_space_names[csi]); )
	{	if ( ++csi == countof(color_space_names) )
			return_error(e_rangecheck);
	}
	/* Note: to avoid having to undo allocations, we should make all */
	/* checks before any recursive calls of cspace_params. */
	/* Unfortunately, we can't do this in the case of Indexed spaces. */
	switch ( (gs_color_space_index)csi )
	{
	case gs_color_space_index_DeviceGray:
		if ( asize != 0 )
			return_error(e_rangecheck);
		pcs->type = &gs_color_space_type_DeviceGray;
		break;
	case gs_color_space_index_DeviceRGB:
		if ( asize != 0 )
			return_error(e_rangecheck);
		pcs->type = &gs_color_space_type_DeviceRGB;
		break;
	case gs_color_space_index_DeviceCMYK:
		if ( asize != 0 )
			return_error(e_rangecheck);
		pcs->type = &gs_color_space_type_DeviceCMYK;
		break;
	case gs_color_space_index_CIEBasedABC:
		if ( asize != 1 )
			return_error(e_rangecheck);
		code = zcolorspace_CIEBasedABC(pcsa, pcs, &pcprocs->cie);
		if ( code < 0 )
			return code;
		/*pcs->type = &gs_color_space_type_CIEBasedABC;*/	/* set by zcolorspace... */
		break;
	case gs_color_space_index_CIEBasedA:
		if ( asize != 1 )
			return_error(e_rangecheck);
		code = zcolorspace_CIEBasedA(pcsa, pcs, &pcprocs->cie);
		if ( code < 0 )
			return code;
		/*pcs->type = &gs_color_space_type_CIEBasedA;*/	/* set by zcolorspace... */
		break;
	case gs_color_space_index_Separation:
		if ( allow == cs_allow_base )
			return_error(e_rangecheck);
		if ( asize != 3 )
			return_error(e_rangecheck);
		check_type(*pcsa, t_name);
		check_proc(pcsa[2]);
		code = cspace_param(pcsa + 1, (gs_color_space *)&pcs->params.separation.alt_space, pcprocs, cs_allow_base);
		if ( code < 0 )
			return code;
		pcprocs->special.separation.layer_name = pcsa[0];
		pcprocs->special.separation.tint_transform = pcsa[2];
		pcs->type = &gs_color_space_type_Separation;
		break;
	case gs_color_space_index_Indexed:
		if ( allow == cs_allow_base )
			return_error(e_rangecheck);
		if ( asize != 3 )
			return_error(e_rangecheck);
		check_type(pcsa[1], t_integer);
		if ( pcsa[1].value.intval < 0 || pcsa[1].value.intval > 4095 )
			return_error(e_rangecheck);
		pcs->params.indexed.hival = pcsa[1].value.intval;
		/* We must get the base space now, rather than later, */
		/* so we can check the length of the table against */
		/* the num_components of the base space. */
		code = cspace_param(pcsa, (gs_color_space *)&pcs->params.indexed.base_space, pcprocs, cs_allow_base);
		if ( code < 0 )
			return code;
		if ( r_has_type(&pcsa[2], t_string) )
		{	check_read(pcsa[2]);
			if ( r_size(&pcsa[2]) !=
			      (pcs->params.indexed.hival + 1) *
			      pcs->params.indexed.base_space.type->num_components
			   )
				return_error(e_rangecheck);
			pcs->params.indexed.lookup.table =
				pcsa[2].value.const_bytes;
			pcs->params.indexed.use_proc = 0;
			make_null(&pcprocs->special.index_proc);
		}
		else
		{	check_proc(pcsa[2]);
			pcs->params.indexed.lookup.proc =
				null_indexed_lookup_proc;
			pcs->params.indexed.use_proc = 1;
			pcprocs->special.index_proc = pcsa[2];
		}
		pcs->type = &gs_color_space_type_Indexed;
		break;
	case gs_color_space_index_Pattern:
		if ( allow != cs_allow_all )
			return_error(e_rangecheck);
		switch ( asize )
		{
		case 0:		/* no base space */
			pcs->params.pattern.has_base_space = 0;
			break;
		default:
			return_error(e_rangecheck);
		case 1:
			pcs->params.pattern.has_base_space = 1;
			code = cspace_param(pcsa,
			  (gs_color_space *)&pcs->params.pattern.base_space,
			  pcprocs, cs_allow_paint);
			if ( code < 0 )
				return code;
		}
		pcs->type = &gs_color_space_type_Pattern;
		break;
	default:
		return_error(e_typecheck);
	}
	return 0;
}
