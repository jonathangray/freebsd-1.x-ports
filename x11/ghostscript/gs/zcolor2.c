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

/* zcolor2.c */
/* Level 2 color operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gscolor.h"
#include "gxcolor.h"
#include "gscspace.h"
#include "gscolor2.h"
#include "gsmatrix.h"
#include "dict.h"
#include "dparam.h"
#include "iname.h"		/* for name_eq */
#include "state.h"
#include "store.h"

/* Forward references */
private int store_color_params(P3(os_ptr, const gs_paint_color *, const gs_color_space *));
private int load_color_params(P3(os_ptr, gs_paint_color *, const gs_color_space *));

/* Names of keys in pattern dictionaries: */
static ref name_PatternType;
static ref name_PaintType;
static ref name_TilingType;
static ref name_BBox;
static ref name_XStep;
static ref name_YStep;
static ref name_PaintProc;
static ref name_Implementation;

/* Initialization */
private void
zcolor2_init(void)
{	static const names_def patn[] = {

	/* Create the names of the known entries in */
	/* a pattern dictionary. */
	   { "PatternType", &name_PatternType },
	   { "PaintType", &name_PaintType },
	   { "TilingType", &name_TilingType },
	   { "BBox", &name_BBox },
	   { "XStep", &name_XStep },
	   { "YStep", &name_YStep },
	   { "PaintProc", &name_PaintProc },
	   { "Implementation", &name_Implementation },

	/* Mark the end of the initialized name list. */
	   names_def_end
	};

	init_names(patn);
}


/* - currentcolor <param1> ... <paramN> */
int
zcurrentcolor(register os_ptr op)
{	const gs_client_color *pc = gs_currentcolor(igs);
	const gs_color_space *pcs = gs_currentcolorspace(igs);
	int n;
	check_ostack(4);
	if ( pcs->type->index == gs_color_space_index_Pattern )
	{	n = 1;
		if ( pc->pattern.PaintType == 2 )
			n += store_color_params(op, &pc->paint,
				(const gs_color_space *)&pcs->params.pattern.base_space);
		op[n] = istate.pattern;
	}
	else
		n = store_color_params(op, &pc->paint, pcs);
	push(n);
	return 0;
}

/* - currentoverprint <bool> */
int
zcurrentoverprint(register os_ptr op)
{	push(1);
	make_bool(op, gs_currentoverprint(igs));
	return 0;
}

/* <pattern> <matrix> makepattern <newpattern> */
int
zmakepattern(os_ptr op)
{	os_ptr op1 = op - 1;
	int code;
	gs_matrix mat;
	int PatternType;
	float BBox[4];
	gs_pattern pattern;
	ref *pPaintProc;
	check_type(*op1, t_dictionary);
	check_dict_read(*op1);
	if ( (code = read_matrix(op, &mat)) < 0 ||
	     (code = dict_int_param(op1, &name_PatternType, 1, 1, 0, &PatternType)) < 0 ||
	     (code = dict_int_param(op1, &name_PaintType, 1, 2, 0, &pattern.PaintType)) < 0 ||
	     (code = dict_int_param(op1, &name_TilingType, 1, 3, 0, &pattern.TilingType)) < 0 ||
	     (code = dict_float_array_param(op1, &name_BBox, 4, BBox, NULL)) != 4 ||
	     (code = dict_float_param(op1, &name_XStep, 0.0, &pattern.XStep)) != 0 ||
	     pattern.XStep == 0.0 ||
	     (code = dict_float_param(op1, &name_YStep, 0.0, &pattern.YStep)) != 0 ||
	     pattern.YStep == 0.0 ||
	     (code = dict_find(op1, &name_PaintProc, &pPaintProc)) <= 0
	   )
		return_error((code < 0 ? code : e_rangecheck));
	check_proc(*pPaintProc);
	pattern.BBox.p.x = min(BBox[0], BBox[2]);
	pattern.BBox.p.y = min(BBox[1], BBox[3]);
	pattern.BBox.q.x = max(BBox[0], BBox[2]);
	pattern.BBox.q.y = max(BBox[1], BBox[3]);
	/****** FINISH ******/
	NYI("makepattern");
	pop(1);
	return 0;
}

/* <param1> ... <paramN> setcolor - */
int
zsetcolor(register os_ptr op)
{	gs_client_color c;
	const gs_color_space *pcs = gs_currentcolorspace(igs);
	int n, code;
	if ( pcs->type->index == gs_color_space_index_Pattern )
	{	/* Make sure *op is a real Pattern. */
		ref *pImpl;
		const gs_pattern *ppat;
		if ( dict_find(op, &name_Implementation, &pImpl) <= 0 ||
		     !r_has_type(pImpl, t_string) ||
		     r_size(pImpl) != sizeof(gs_pattern)
		   )
			return_error(e_rangecheck);
		ppat = (const gs_pattern *)pImpl->value.const_bytes;
		c.pattern = *ppat;
		if ( c.pattern.PaintType == 2 )
		{	n = load_color_params(op - 1, &c.paint,
				(const gs_color_space *)&pcs->params.pattern.base_space);
			if ( n < 0 ) return n;
			n++;
		}
		else
			n = 1;
	}
	else
		n = load_color_params(op, &c.paint, pcs);
	if ( n < 0 ) return n;
	code = gs_setcolor(igs, &c);
	if ( code < 0 ) return code;
	pop(n);
	return 0;
}

/* <bool> setoverprint - */
int
zsetoverprint(register os_ptr op)
{	check_type(*op, t_boolean);
	gs_setoverprint(igs, op->value.index);
	pop(1);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zcolor2_op_defs[] = {
	{"0currentcolor", zcurrentcolor},
	{"0currentoverprint", zcurrentoverprint},
	{"2makepattern", zmakepattern},
	{"1setcolor", zsetcolor},
	{"1setoverprint", zsetoverprint},
	op_def_end(zcolor2_init)
};

/* ------ Internal procedures ------ */

/* Store non-pattern color values on the operand stack. */
/* Return the number of values stored. */
private int
store_color_params(os_ptr op, const gs_paint_color *pc,
  const gs_color_space *pcs)
{	int n = pcs->type->num_components;
	make_reals(op + 1, pc->values, n);
	return n;
}

/* Load non-pattern color values from the operand stack. */
/* Return the number of values stored. */
private int
load_color_params(os_ptr op, gs_paint_color *pc,
  const gs_color_space *pcs)
{	int n = pcs->type->num_components;
	int code = num_params(op, n, pc->values);
	if ( code < 0 ) return code;
	return n;
}
