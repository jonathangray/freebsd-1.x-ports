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

/* zcie.c */
/* CIE color operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gxrefct.h"			/* early for gscie.h */
#include "gxcolor.h"			/* for gscolor2.h */
#include "gscspace.h"
#include "gscolor2.h"
#include "gscie.h"
#include "alloc.h"
#include "dict.h"
#include "dparam.h"
#include "estack.h"
#include "save.h"
#include "state.h"
#include "store.h"		/* for make_null */

/* There are actually only two CIE-specific operators, */
/* but CIE color dictionaries are so complex that */
/* we handle the CIE case of setcolorspace here as well. */

/* Forward references */
private int cache_colorrendering(P3(gs_cie_render *,
  const ref_cie_render_procs *, gs_state *));
private int cache_common(P4(gs_cie_common *, const ref_cie_procs *,
  const ref_cie_render_procs *, gs_state *));

/* Elements in a CIEBasedABC color space dictionary: */
static ref name_RangeABC;
static ref name_DecodeABC;
static ref name_MatrixABC;
static ref name_RangeLMN;
static ref name_DecodeLMN;
static ref name_MatrixLMN;
static ref name_WhitePoint;
static ref name_BlackPoint;
/* Additional or alternate elements in a CIEBasedA c.s. dictionary: */
static ref name_RangeA;
static ref name_DecodeA;
static ref name_MatrixA;
/* (Additional) elements in a CIE color rendering dictionary: */
static ref name_ColorRenderingType;
static ref name_EncodeLMN;
static ref name_EncodeABC;
static ref name_MatrixPQR;
static ref name_RangePQR;
static ref name_TransformPQR;
static ref name_RenderTable;

/* Empty procedures */
static ref empty_procs[3];

/* Original CIE color space types */
extern const gs_color_space_type
	gs_color_space_type_CIEBasedABC,
	gs_color_space_type_CIEBasedA;
/* Redefined CIE color space types (that load the cache when installed) */
gs_color_space_type
	cs_type_zCIEBasedABC,
	cs_type_zCIEBasedA;
private cs_proc_install_cspace(cs_install_zCIEBasedABC);
private cs_proc_install_cspace(cs_install_zCIEBasedA);

/* Initialization */
private void
zcie_init(void)
{	static const names_def cien[] = {

	/* Elements in a CIEBasedABC color space dictionary: */
	   { "RangeABC", &name_RangeABC },
	   { "DecodeABC", &name_DecodeABC },
	   { "MatrixABC", &name_MatrixABC },
	   { "RangeLMN", &name_RangeLMN },
	   { "DecodeLMN", &name_DecodeLMN },
	   { "MatrixLMN", &name_MatrixLMN },
	   { "WhitePoint", &name_WhitePoint },
	   { "BlackPoint", &name_BlackPoint },

	/* Additional or alternate elements in a CIEBasedA c.s. dictionary: */
	   { "RangeA", &name_RangeA },
	   { "DecodeA", &name_DecodeA },
	   { "MatrixA", &name_MatrixA },

	/* (Additional) elements in a CIE color rendering dictionary: */
	   { "ColorRenderingType", &name_ColorRenderingType },
	   { "EncodeLMN", &name_EncodeLMN },
	   { "EncodeABC", &name_EncodeABC },
	   { "MatrixPQR", &name_MatrixPQR },
	   { "RangePQR", &name_RangePQR },
	   { "TransformPQR", &name_TransformPQR },
	   { "RenderTable", &name_RenderTable },

	/* Mark the end of the initialized name list. */
	   names_def_end
	};

	init_names(cien);

	/* Make the null (default) transformation procedures. */
	make_const_array(&empty_procs[0], a_readonly + a_executable, 0, NULL);
	make_const_array(&empty_procs[1], a_readonly + a_executable, 0, NULL);
	make_const_array(&empty_procs[2], a_readonly + a_executable, 0, NULL);

	/* Create the modified color space types. */
	cs_type_zCIEBasedABC = gs_color_space_type_CIEBasedABC;
	cs_type_zCIEBasedABC.install_cspace = cs_install_zCIEBasedABC;
	cs_type_zCIEBasedA = gs_color_space_type_CIEBasedA;
	cs_type_zCIEBasedA.install_cspace = cs_install_zCIEBasedA;

}

/* ------ CIE setcolorspace ------ */

/* Get a 3-element range parameter from a dictionary. */
#define dict_range3_param(op, pname, prange)\
	dict_float_array_param(op, pname, 6, (float *)prange, (float *)&Range3_default)
#define range3_ok 6

/* Get a 3x3 matrix parameter from a dictionary. */
#define dict_matrix3_param(op, pname, pmat)\
	dict_float_array_param(op, pname, 9, (float *)pmat, (float *)&Matrix3_default)
#define matrix3_ok 9

/* Get an array of procedures from a dictionary. */
/* We know count <= 3. */
private int
dict_proc_array_param(const ref *pdict, const ref *pname,
  uint count, ref *pparray)
{	ref *pvalue;
	if ( dict_find(pdict, pname, &pvalue) > 0 )
	{	uint i;
		check_array(*pvalue);
		if ( r_size(pvalue) != count )
			return_error(e_rangecheck);
		for ( i = 0; i < count; i++ )
		{	ref proc;
			array_get(pvalue, (long)i, &proc);
			check_proc(proc);
		}
		*pparray = *pvalue;
	}
	else
		make_const_array(pparray, a_readonly, count, &empty_procs[0]);
	return 0;
}

/* Get 3 procedures from a dictionary. */
#define dict_proc3_param(op, pname, pparray)\
	dict_proc_array_param(op, pname, 3, pparray)
		
/* Shared code for getting WhitePoint and BlackPoint values. */
private int
cie_points_param(const ref *pdref, gs_cie_wb *pwb)
{	int code;
	if ( (code = dict_float_array_param(pdref, &name_WhitePoint, 3, (float *)&pwb->WhitePoint, NULL)) != 3 ||
	     (code = dict_float_array_param(pdref, &name_BlackPoint, 3, (float *)&pwb->BlackPoint, (float *)&BlackPoint_default)) != 3
	   )
		return (code < 0 ? code : e_rangecheck);
	if ( pwb->WhitePoint.u <= 0 ||
	     pwb->WhitePoint.v != 1 ||
	     pwb->WhitePoint.w <= 0 ||
	     pwb->BlackPoint.u < 0 ||
	     pwb->BlackPoint.v < 0 ||
	     pwb->BlackPoint.w < 0
	   )
		return_error(e_rangecheck);
	return 0;
}

/* Common code for the CIEBasedA[BC] cases of setcolorspace. */
private int
cie_lmnp_param(const ref *pdref, gs_cie_common *pcie, ref_cie_procs *pcprocs)
{	int code;
	if ( (code = dict_range3_param(pdref, &name_RangeLMN, &pcie->RangeLMN)) != range3_ok ||
	     (code = dict_proc3_param(pdref, &name_DecodeLMN, &pcprocs->DecodeLMN)) < 0 ||
	     (code = dict_matrix3_param(pdref, &name_MatrixLMN, &pcie->MatrixLMN)) != matrix3_ok ||
	     (code = cie_points_param(pdref, &pcie->points)) < 0
	   )
		return (code < 0 ? code : e_rangecheck);
	pcie->DecodeLMN = DecodeLMN_default;
	return 0;
}

/* Get the parameters of a CIEBasedABC color space. */
/* This doesn't actually implement setcolorspace, */
/* since it has to be usable for the base color space */
/* of Separation, Indexed, and Pattern spaces as well. */
int
zcolorspace_CIEBasedABC(const ref *pdref, gs_color_space *pcs,
  ref_cie_procs *pcprocs)
{	gs_cie_abc *pcie;
	int code;
	check_type(*pdref, t_dictionary);
	check_dict_read(*pdref);
	rc_alloc_struct_0(pcie, gs_cie_abc, &alloc_memory_procs,
			  return_error(e_VMerror),
			  "setcolorspace(CIEBasedABC)");
	if ( (code = dict_range3_param(pdref, &name_RangeABC, &pcie->RangeABC)) != range3_ok ||
	     (code = dict_proc3_param(pdref, &name_DecodeABC, &pcprocs->Decode.ABC)) < 0 ||
	     (code = dict_matrix3_param(pdref, &name_MatrixABC, &pcie->MatrixABC)) != matrix3_ok ||
	     (code = cie_lmnp_param(pdref, &pcie->common, pcprocs)) < 0
	   )
	{	rc_free(pcie, &alloc_memory_procs,
			"setcolorspace(CIEBasedABC)");
		return (code < 0 ? code : e_rangecheck);
	}
	pcie->DecodeABC = DecodeABC_default;
	pcs->params.abc = pcie;
	pcs->type = &cs_type_zCIEBasedABC;
	return 0;	/* installation will load the caches */
}

/* Get the parameters of a CIEBasedA color space. */
/* See above. */
int
zcolorspace_CIEBasedA(const ref *pdref, gs_color_space *pcs,
  ref_cie_procs *pcprocs)
{	gs_cie_a *pcie;
	int code;
	ref *pproc;
	check_type(*pdref, t_dictionary);
	check_dict_read(*pdref);
	if ( (code = dict_find(pdref, &name_DecodeA, &pproc)) < 0 )
		make_array(&pcprocs->Decode.A, a_readonly + a_executable, 0, NULL);
	else
	{	check_proc(*pproc);
		pcprocs->Decode.A = *pproc;
	}
	rc_alloc_struct_0(pcie, gs_cie_a, &alloc_memory_procs,
			  return_error(e_VMerror),
			  "setcolorspace(CIEBasedA)");
	if ( (code = dict_float_array_param(pdref, &name_RangeA, 2, (float *)&pcie->RangeA, (float *)&RangeA_default)) != 2 ||
	     (code = dict_float_array_param(pdref, &name_MatrixA, 3, (float *)&pcie->MatrixA, (float *)&MatrixA_default)) != 3 ||
	     (code = cie_lmnp_param(pdref, &pcie->common, pcprocs)) < 0
	   )
	{	rc_free(pcie, &alloc_memory_procs,
			"setcolorspace(CIEBasedA)");
		return (code < 0 ? code : e_rangecheck);
	}
	pcie->DecodeA = DecodeA_default;
	pcs->params.a = pcie;
	pcs->type = &cs_type_zCIEBasedA;
	return 0;	/* installation will load the caches */
}

/* ------ CIE rendering dictionary ------ */

/* - currentcolorrendering <dict> */
private int
zcurrentcolorrendering(register os_ptr op)
{	push(1);
	*op = istate.colorrendering.dict;
	return 0;
}

/* <dict> setcolorrendering - */
private int zsetcolorrendering_internal(P3(os_ptr, gs_cie_render *, ref_cie_render_procs *));
private int
zsetcolorrendering(register os_ptr op)
{	int code;
	es_ptr ep = esp;
	gs_cie_render *pcie;
	ref_cie_render_procs procs_old;
	check_read_type(*op, t_dictionary);
	check_dict_read(*op);
	rc_alloc_struct_0(pcie, gs_cie_render, &alloc_memory_procs,
			  return_error(e_VMerror),
			  "setcolorrendering");
	/* gs_setcolorrendering may refer to istate.colorrendering.procs. */
	procs_old = istate.colorrendering.procs;
	code = zsetcolorrendering_internal(op, pcie, &istate.colorrendering.procs);
	if ( code < 0 )
	{	rc_free(pcie, &alloc_memory_procs, "setcolorrendering");
		istate.colorrendering.procs = procs_old;
		esp = ep;
		return code;
	}
	istate.colorrendering.dict = *op;
	pop(1);
	return (esp == ep ? 0 : o_push_estack);
}
private int
zsetcolorrendering_internal(os_ptr op, gs_cie_render *pcie,
  ref_cie_render_procs *pcprocs)
{	int code;
	int discard;
	ref *pRT;
	if ( (code = dict_int_param(op, &name_ColorRenderingType, 1, 1, 0, &discard)) < 0 ||
	     (code = dict_matrix3_param(op, &name_MatrixLMN, &pcie->MatrixLMN)) != matrix3_ok ||
	     (code = dict_proc3_param(op, &name_EncodeLMN, &pcprocs->EncodeLMN)) < 0 ||
	     (code = dict_range3_param(op, &name_RangeLMN, &pcie->RangeLMN)) != range3_ok ||
	     (code = dict_matrix3_param(op, &name_MatrixABC, &pcie->MatrixABC)) != matrix3_ok ||
	     (code = dict_proc3_param(op, &name_EncodeABC, &pcprocs->EncodeABC)) < 0 ||
	     (code = dict_range3_param(op, &name_RangeABC, &pcie->RangeABC)) != range3_ok ||
	     (code = cie_points_param(op, &pcie->points)) < 0 ||
	     (code = dict_matrix3_param(op, &name_MatrixPQR, &pcie->MatrixPQR)) != matrix3_ok ||
	     (code = dict_range3_param(op, &name_RangePQR, &pcie->RangePQR)) != range3_ok ||
	     (code = dict_proc3_param(op, &name_TransformPQR, &pcprocs->TransformPQR)) < 0
	   )
		return (code < 0 ? code : e_rangecheck);
#define rRT pcie->RenderTable
	if ( dict_find(op, &name_RenderTable, &pRT) > 0 )
	{	const ref *prte;
		int i;
		uint n2;
		const ref *strings;
		check_read_type(*pRT, t_array);
		prte = pRT->value.const_refs;
		check_type(prte[0], t_integer);
		check_type(prte[1], t_integer);
		check_type(prte[2], t_integer);
		check_read_type(prte[3], t_array);
		check_type(prte[4], t_integer);
		if ( prte[0].value.intval <= 1 ||
		     prte[1].value.intval <= 1 ||
		     prte[2].value.intval <= 1 ||
		     !(prte[4].value.intval == 3 || prte[4].value.intval == 4)
		   )
			return_error(e_rangecheck);
		rRT.NA = prte[0].value.intval;
		rRT.NB = prte[1].value.intval;
		rRT.NC = prte[2].value.intval;
		rRT.m = prte[4].value.intval;
		n2 = rRT.m * rRT.NB * rRT.NC;
		if ( r_size(pRT) != rRT.m + 5 || r_size(&prte[3]) != rRT.NA )
			return_error(e_rangecheck);
		strings = prte[3].value.const_refs;
		for ( i = 0; i < rRT.NA; i++ )
		{	const ref *prt2 = strings + i;
			check_read_type(*prt2, t_string);
			if ( r_size(prt2) != n2 )
				return_error(e_rangecheck);
		}
		prte += 5;
		for ( i = 0; i < rRT.m; i++ )
		{	const ref *prt2 = prte + i;
			check_proc(*prt2);
		}
		rRT.table = (const byte **)alloc(rRT.NA, sizeof(byte *),
						 "setcolorrendering(table)");
		if ( rRT.table == 0 )
			return_error(e_VMerror);
		for ( i = 0; i < rRT.NA; i++ )
			rRT.table[i] = strings[i].value.bytes;
		make_const_array(&pcprocs->RenderTableT, a_readonly, rRT.m, prte + 5);
	}
	else
	{	rRT.table = 0;
		make_null(&pcprocs->RenderTableT);
	}
#undef rRT
	pcie->EncodeLMN = Encode_default;
	pcie->EncodeABC = Encode_default;
	pcie->TransformPQR = TransformPQR_default;
	pcie->RenderTable.T = RenderTableT_default;
	code = cache_colorrendering(pcie, pcprocs, igs);
	if ( code < 0 )
		return code;
	return gs_setcolorrendering(igs, pcie);
}

/* ------ Internal routines ------ */

/* Import operators. */
extern int
  zfor(P1(os_ptr)),
  zcvx(P1(os_ptr));
extern int zexec(P1(os_ptr));
/* Import accessors. */
extern gx_cie_joint_caches *gx_currentciecaches(P1(gs_state *));
extern char /*void*/ *gs_state_client_data(P1(gs_state *));

/* Forward declarations */
private int
  cie_cache_finish(P1(os_ptr)),
  cie_exec_tpqr(P1(os_ptr)),
  cie_tpqr_finish(P1(os_ptr));

/* Transform a set of ranges. */
private void
cie_transform_range(const gs_range3 *in, const gs_vector3 *col,
  gs_range *out)
{	float umin = col->u * in->u.rmin, umax = col->u * in->u.rmax;
	float vmin = col->v * in->v.rmin, vmax = col->v * in->v.rmax;
	float wmin = col->w * in->w.rmin, wmax = col->w * in->w.rmax;
	float temp;
#define swap(x, y) temp = x, x = y, y = temp
	if ( umin > umax ) swap(umin, umax);
	if ( vmin > vmax ) swap(vmin, vmax);
	if ( wmin > wmax ) swap(wmin, wmax);
	out->rmin = umin + vmin + wmin;
	out->rmax = umax + vmax + wmax;
#undef swap
}
private void
cie_transform_range3(const gs_range3 *in, const gs_matrix3 *mat,
  gs_range3 *out)
{	cie_transform_range(in, &mat->cu, &out->u);
	cie_transform_range(in, &mat->cv, &out->v);
	cie_transform_range(in, &mat->cw, &out->w);
}

/* Prepare to cache the values for one or more procedures. */
private int
cie_prepare_caches(const gs_range *domain, const ref *proc,
  gx_cie_cache *pcache, int n)
{	check_estack(n * 8);
	for ( ; --n >= 0; domain++, proc++, pcache++, esp += 8 )
	{	float diff = domain->rmax - domain->rmin;
		float delta = diff / (gx_cie_cache_size - 1);
		register es_ptr ep = esp;
		/* Zero out the cache, since the gs level will try to */
		/* access it before it has been filled. */
		{	register float *pcv = &pcache->values[0];
			register int i;
			for ( i = 0; i < gx_cie_cache_size; i++, pcv++ )
				*pcv = 0.0;
		}
		pcache->base = domain->rmin - delta / 2;	/* so lookup will round */
		pcache->factor = (delta == 0 ? 0 : 1 / delta);
		pcache->is_identity = r_size(proc) == 0;
		if_debug3('c', "[c]cache 0x%lx base=%g, factor=%g\n",
			  (ulong)pcache, pcache->base, pcache->factor);
		make_real(ep + 8, domain->rmin);
		make_real(ep + 7, delta);
		make_real(ep + 6, domain->rmax + delta / 2);
		ep[5] = *proc;
		r_clear_attrs(ep + 5, a_executable);
		make_op_estack(ep + 4, zcvx);
		make_op_estack(ep + 3, zfor);
		make_op_estack(ep + 2, cie_cache_finish);
		make_string(ep + 1, 0, sizeof(*pcache), (byte *)pcache);
	}
	return o_push_estack;
}
/* Prepare to cache the values for 3 procedures. */
#define cie_prepare_cache3(d3,p3,c3)\
  cie_prepare_caches((const gs_range *)(d3), p3, c3, 3)

/* Store the result of caching one procedure. */
private int
cie_cache_finish(os_ptr op)
{	gx_cie_cache *pcache = (gx_cie_cache *)esp->value.bytes;
	int code = num_params(op, gx_cie_cache_size, &pcache->values[0]);
	if_debug3('c', "[c]cache 0x%lx base=%g, factor=%g:\n",
		  (ulong)pcache, pcache->base, pcache->factor);
	if ( code < 0 ) return code;
#ifdef DEBUG
	if ( debug_c('c') )
	{	int i;
		for ( i = 0; i < gx_cie_cache_size; i++ )
		  dprintf2("[c]cache[%3d]=%g\n", i, pcache->values[i]);
	}
#endif
	pop(gx_cie_cache_size);
	esp--;				/* pop pointer to cache */
	return o_pop_estack;
}

/* Install a CIE-based color space. */

private int
cs_install_zCIEBasedABC(gs_color_space *pcs, gs_state *pgs)
{	es_ptr ep = esp;
	gs_cie_abc *pcie = pcs->params.abc;
	const int_gstate *pigs = (int_gstate *)gs_state_client_data(pgs);
	const ref_cie_procs *pcprocs = &pigs->colorspace.procs.cie;
	int code = gx_install_CIEBasedABC(pcs, pgs);	/* former routine */
	if ( code < 0 ) return code;
	code = cie_prepare_cache3(&pcie->RangeABC, pcprocs->Decode.ABC.value.const_refs, &pcie->caches.DecodeABC[0]);
	if ( code < 0 ||
	     (code = cache_common(&pcie->common, pcprocs, &pigs->colorrendering.procs, pgs)) < 0
	   )
	{	esp = ep;
		return code;
	}
	return o_push_estack;
}

private int
cs_install_zCIEBasedA(gs_color_space *pcs, gs_state *pgs)
{	es_ptr ep = esp;
	gs_cie_a *pcie = pcs->params.a;
	const int_gstate *pigs = (int_gstate *)gs_state_client_data(pgs);
	const ref_cie_procs *pcprocs = &pigs->colorspace.procs.cie;
	int code = gx_install_CIEBasedA(pcs, pgs);	/* former routine */
	if ( code < 0 ) return code;
	code = cie_prepare_caches(&pcie->RangeA, &pcprocs->Decode.A, &pcie->caches.DecodeA, 1);
	if ( code < 0 ||
	     (code = cache_common(&pcie->common, pcprocs, &pigs->colorrendering.procs, pgs)) < 0
	   )
	{	esp = ep;
		return code;
	}
	return o_push_estack;
}

/* Cache the results of the color rendering procedures. */
private int
cache_colorrendering(gs_cie_render *pcie,
  const ref_cie_render_procs *pcrprocs, gs_state *pgs)
{	gs_range3 DomainLMN;
	gs_range3 DomainABC;
	es_ptr ep = esp;
	static const gs_range ranges_01[4] =
		{ {0,1}, {0,1}, {0,1}, {0,1} };
	int code = gs_cie_render_init(pcie);	/* sets PQR'*LMN */
	if ( code < 0 ) return code;
	cie_transform_range3(&pcie->RangePQR, &pcie->MatrixPQR_inverse_LMN, &DomainLMN);
	cie_transform_range3(&pcie->RangeLMN, &pcie->MatrixABC, &DomainABC);
	if ( (code = cie_prepare_cache3(&DomainLMN, pcrprocs->EncodeLMN.value.const_refs, &pcie->caches.EncodeLMN[0])) < 0 ||
	     (code = cie_prepare_cache3(&DomainABC, pcrprocs->EncodeABC.value.const_refs, &pcie->caches.EncodeABC[0])) < 0 ||
	     (pcie->RenderTable.table != 0 &&
	      (code = cie_prepare_caches(ranges_01, pcrprocs->RenderTableT.value.const_refs, &pcie->caches.RenderTableT[0], pcie->RenderTable.m)) < 0)
	   )
	{	esp = ep;
		return code;
	}
	/* gs_setcolorrendering reinstalls the color space, */
	/* which reloads the joint caches if needed. */
	return o_push_estack;
}

/* Common cache code */
private int
cache_common(gs_cie_common *pcie, const ref_cie_procs *pcprocs,
  const ref_cie_render_procs *pcrprocs, gs_state *pgs)
{	int code = cie_prepare_cache3(&pcie->RangeLMN,
				      pcprocs->DecodeLMN.value.const_refs,
				      &pcie->caches.DecodeLMN[0]);
	const gs_cie_render *pcier = gs_currentcolorrendering(pgs);
	/* The former installation procedures have allocated */
	/* the joint caches and filled in points_sd. */
	gx_cie_joint_caches *pjc = gx_currentciecaches(pgs);
	ref pqr_procs;
#define pqr_refs pqr_procs.value.refs
	int i;
	if ( code < 0 ) return code;
	if ( pcier == 0 ) return 0;	/* cache is not used */
	check_estack(2);
	code = alloc_array(&pqr_procs, a_readonly, 3*(1+3+4*6), "cie_cache_common");
	if ( code < 0 ) return code;
	/* Make sure we deallocate the procs when we're done. */
	push_op_estack(cie_tpqr_finish);
	*++esp = pqr_procs;
	for ( i = 0; i < 3; i++ )
	{	ref *p = pqr_refs + 3 + (3+4*6) * i;
		const float *ppt = (float *)&pjc->points_sd;
		int j;
		make_array(pqr_refs + i, a_readonly + a_executable, 3, p);
		make_array(p, a_readonly, 4*6, p + 3);
		p[1] = pcrprocs->TransformPQR.value.refs[i];
		make_oper(p + 2, 0, cie_exec_tpqr);
		for ( j = 0, p += 3; j < 4*6; j++, p++, ppt++ )
			make_real(p, *ppt);
	}
	return cie_prepare_cache3(&pcier->RangePQR,
				  pqr_procs.value.const_refs,
				  &pjc->TransformPQR[0]);
}

/* Private operator to shuffle arguments for the TransformPQR procedure: */
/* v [ws wd bs bd] proc -> ws wd bs bd v proc + exec */
private int
cie_exec_tpqr(register os_ptr op)
{	const ref *ppt = op[-1].value.const_refs;
	int i;
	push(3);
	*op = op[-3];		/* proc */
	op[-1] = op[-5];	/* v */
	for ( i = 0; i < 4; i++ )
		make_const_array(op - 5 + i, a_readonly, 6, ppt + i * 6);
	return zexec(op);
}

/* Private operator to free procs array. */
private int
cie_tpqr_finish(register os_ptr op)
{	alloc_free_array(op, "cie_tpqr_finish");
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zcie_op_defs[] = {
	{"0currentcolorrendering", zcurrentcolorrendering},
	{"1setcolorrendering", zsetcolorrendering},
	{"0%cie_cache_finish", cie_cache_finish},
	{"3%cie_exec_tpqr", cie_exec_tpqr},
	{"1%cie_tpqr_finish", cie_tpqr_finish},
	op_def_end(zcie_init)
};
