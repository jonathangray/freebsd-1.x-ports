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

/* gscie.c */
/* CIE color rendering for Ghostscript */
#include "gx.h"
#include "gserrors.h"
#include "gxrefct.h"			/* early for gscie.h */
#include "gxcolor.h"			/* early for gscolor2.h */
#include "gscspace.h"
#include "gscolor2.h"			/* for gs_set/currentcolorrendering */
#include "gscie.h"
#include "gxarith.h"
#include "gxdevice.h"			/* for gs_color_index */
#include "gzcolor.h"
#include "gzstate.h"

/* Forward references */
private void near cie_mult3(P3(const gs_vector3 *, const gs_matrix3 *, gs_vector3 *));
private void near cie_matrix_mult3(P3(const gs_matrix3 *, const gs_matrix3 *, gs_matrix3 *));
private void near cie_invert3(P2(const gs_matrix3 *, gs_matrix3 *));
private void near cie_restrict3(P3(const gs_vector3 *, const gs_range3 *, gs_vector3 *));
private void near cie_lookup3(P3(const gs_vector3 *, const gx_cie_cache *, gs_vector3 *));
private void near cie_matrix_init(P1(gs_matrix3 *));

#define lookup(vin, pcache, vout)\
  if ( (pcache)->is_identity ) vout = (vin);\
  else vout = (pcache)->values[(int)(((vin) - (pcache)->base) * (pcache)->factor)]

#define restrict(vin, range, vout)\
  if ( (vin) < (range).rmin ) vout = (range).rmin;\
  else if ( (vin) > (range).rmax ) vout = (range).rmax;\
  else vout = (vin)

/* ------ Default values for CIE dictionary elements ------ */

/* Default transformation procedures. */

private int
a_identity(const float *in, const gs_cie_a *pcie, float *out)
{	*out = *in;
	return 0;
}
private int
abc_identity(const gs_vector3 *in, const gs_cie_abc *pcie, gs_vector3 *out)
{	*out = *in;
	return 0;
}
private int
common_identity(const gs_vector3 *in, const gs_cie_common *pcie, gs_vector3 *out)
{	*out = *in;
	return 0;
}
private int
render_identity(const gs_vector3 *in, const gs_cie_render *pcie, gs_vector3 *out)
{	*out = *in;
	return 0;
}
private int
tpqr_identity(const gs_vector3 *in, const gs_cie_wbsd *pwbsd, const gs_cie_render *pcie, gs_vector3 *out)
{	*out = *in;
	return 0;
}
private int
render_table_identity(const byte *in, int m, const gs_cie_render *pcie, float *out)
{	int j;
	for ( j = 0; j < m; j++ ) out[j] = in[j] / 255.0;
	return 0;
}

/* Default vectors and matrices. */

const gs_range3 Range3_default = { {0,1}, {0,1}, {0,1} };
const gs_cie_abc_proc3 DecodeABC_default = abc_identity;
const gs_cie_common_proc3 DecodeLMN_default = common_identity;
const gs_matrix3 Matrix3_default = { {1,0,0}, {0,1,0}, {0,0,1}, 1 };
const gs_range RangeA_default = {0,1};
const gs_cie_a_proc DecodeA_default = a_identity;
const gs_vector3 MatrixA_default = { 1, 1, 1 };
const gs_vector3 BlackPoint_default = { 0, 0, 0 };
const gs_cie_render_proc3 Encode_default = render_identity;
const gs_cie_transform_proc3 TransformPQR_default = tpqr_identity;
const gs_cie_render_table_proc RenderTableT_default = render_table_identity;

/* setcolorrendering */
int
gs_setcolorrendering(gs_state *pgs, gs_cie_render *pcie)
{	int code = gs_cie_render_init(pcie);
	if ( code < 0 ) return code;
	rc_assign(pgs->cie_render, pcie, pgs->memory_procs,
		  "gs_setcolorrendering");
	/* Initialize the joint caches, if needed, */
	/* by re-installing the color space. */
	(*pgs->color_space->type->install_cspace)(pgs->color_space, pgs);
	return gx_remap_color(pgs);
}

/* currentcolorrendering */
const gs_cie_render *
gs_currentcolorrendering(const gs_state *pgs)
{	return pgs->cie_render;
}

/* Get the joint caches, to avoid having to import gzstate.h */
gx_cie_joint_caches *
gx_currentciecaches(gs_state *pgs)
{	return pgs->cie_joint_caches;
}

/* ------ Complete a rendering structure ------ */

int
gs_cie_render_init(gs_cie_render *pcie)
{	gs_matrix3 PQR_inverse;
	cie_matrix_init(&pcie->MatrixLMN);
	cie_matrix_init(&pcie->MatrixABC);
	cie_matrix_init(&pcie->MatrixPQR);
	cie_invert3(&pcie->MatrixPQR, &PQR_inverse);
	cie_matrix_mult3(&PQR_inverse, &pcie->MatrixLMN, &pcie->MatrixPQR_inverse_LMN);
	cie_mult3(&pcie->points.WhitePoint, &pcie->MatrixPQR, &pcie->wdpqr);
	cie_mult3(&pcie->points.BlackPoint, &pcie->MatrixPQR, &pcie->bdpqr);
	/****** FINISH ******/
	return 0;
}

/* ------ Fill in the joint cache ------ */

int
gx_cie_joint_caches_init(gx_cie_joint_caches *pjc,
  const gs_cie_common *pcie, const gs_cie_render *pcier)
{	pjc->points_sd.ws.xyz = pcie->points.WhitePoint;
	cie_mult3(&pjc->points_sd.ws.xyz, &pcier->MatrixPQR, &pjc->points_sd.ws.pqr);
	pjc->points_sd.bs.xyz = pcie->points.BlackPoint;
	cie_mult3(&pjc->points_sd.bs.xyz, &pcier->MatrixPQR, &pjc->points_sd.bs.pqr);
	pjc->points_sd.wd.xyz = pcier->points.WhitePoint;
	pjc->points_sd.wd.pqr = pcier->wdpqr;
	pjc->points_sd.bd.xyz = pcier->points.BlackPoint;
	pjc->points_sd.bd.pqr = pcier->bdpqr;
	/****** FINISH ******/
	return 0;
}

/* ------ Remap (render) a CIE color (using the caches). ------ */

private int near cie_remap_finish(P4(const gs_vector3 *,
  const gs_cie_common *, gx_device_color *, gs_state *));

/* Render a CIEBasedABC color. */
int
gx_remap_CIEBasedABC(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, gs_state *pgs)
{	const gs_cie_abc *pcie = pcs->params.abc;
	gs_vector3 abc, lmn;
	cie_restrict3((const gs_vector3 *)&pc->paint.values[0], &pcie->RangeABC, &abc);
		/* (*pcie->DecodeABC)(&abc, pcie, &abc); */
	cie_lookup3(&abc, &pcie->caches.DecodeABC[0], &abc);
	cie_mult3(&abc, &pcie->MatrixABC, &lmn);
	return cie_remap_finish(&lmn, &pcie->common, pdc, pgs);
}

/* Render a CIEBasedA color. */
int
gx_remap_CIEBasedA(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, gs_state *pgs)
{	const gs_cie_a *pcie = pcs->params.a;
	const gx_cie_cache *pcache = &pcie->caches.DecodeA;
	float a;
	gs_vector3 lmn;
	restrict(pc->paint.values[0], pcie->RangeA, a);
		/* (*pcie->DecodeA)(&a, pcie, &a); */
	lookup(a, pcache, a);
	lmn.u = a * pcie->MatrixA.u;
	lmn.v = a * pcie->MatrixA.v;
	lmn.w = a * pcie->MatrixA.w;
	return cie_remap_finish(&lmn, &pcie->common, pdc, pgs);
}

/* Common rendering code. */
private int near
cie_remap_finish(const gs_vector3 *plmn, const gs_cie_common *pcommon,
  gx_device_color *pdc, gs_state *pgs)
{	const gs_cie_render *pcie = pgs->cie_render;
	const byte **table;
	gs_vector3 abc, lmn, xyz, pqr;
	gs_client_color cc;
	gs_color_space cs;

		/* Finish decoding. */

	cie_restrict3(plmn, &pcommon->RangeLMN, &lmn);
		/* (*pcommon->DecodeLMN)(&lmn, pcommon, &lmn); */
	cie_lookup3(&lmn, &pcommon->caches.DecodeLMN[0], &lmn);
	cie_mult3(&lmn, &pcommon->MatrixLMN, &xyz);

		/* Render. */

	if ( pcie == 0 )		/* default rendering */
	{	abc = xyz;
		table = 0;
	}
	else
	{	const gx_cie_joint_caches *pjc = pgs->cie_joint_caches;
		cie_mult3(&xyz, &pcie->MatrixPQR, &pqr);
		cie_restrict3(&pqr, &pcie->RangePQR, &pqr);
			/* (*pcie->TransformPQR)(&pqr, &pjc->points_sd, pcie, &pqr); */
		cie_lookup3(&pqr, &pjc->TransformPQR[0], &pqr);
		cie_mult3(&pqr, &pcie->MatrixPQR_inverse_LMN, &lmn);
			/* (*pcie->EncodeLMN)(&lmn, pcie, &lmn); */
		cie_lookup3(&lmn, &pcie->caches.EncodeLMN[0], &lmn);
		cie_restrict3(&lmn, &pcie->RangeLMN, &lmn);
		cie_mult3(&lmn, &pcie->MatrixABC, &abc);
			/* (*pcie->EncodeABC)(&abc, pcie, &abc); */
		cie_lookup3(&abc, &pcie->caches.EncodeABC[0], &abc);
		cie_restrict3(&abc, &pcie->RangeABC, &abc);
		table = pcie->RenderTable.table;
	}
	if ( table == 0 )
	{	/* No further transformation */
		cc.paint.values[0] = abc.u;
		cc.paint.values[1] = abc.v;
		cc.paint.values[2] = abc.w;
		cs.type = &gs_color_space_type_DeviceRGB;
	}
	else
	{	/* Use the RenderTable. */
		int m = pcie->RenderTable.m;
#define ri(s,n)\
  (int)((abc.s - pcie->RangeABC.s.rmin) * (pcie->RenderTable.n - 1) /\
	(pcie->RangeABC.s.rmax - pcie->RangeABC.s.rmin) + 0.5)
		int ia = ri(u, NA);
		int ib = ri(v, NB);
		int ic = ri(w, NC);
		const byte *prtc =
		  table[ia] + m * (ib * pcie->RenderTable.NC + ic);
			/* (*pcie->RenderTable.T)(prtc, m, pcie, &cc.paint.values[0]); */
#define shift_in(b) gx_cie_byte_to_cache_index(b)
#define rtc(i) (pcie->caches.RenderTableT[i])
		cc.paint.values[0] = rtc(0).values[shift_in(prtc[0])];
		cc.paint.values[1] = rtc(1).values[shift_in(prtc[1])];
		cc.paint.values[2] = rtc(2).values[shift_in(prtc[2])];
		if ( m == 3 )
		{	cs.type = &gs_color_space_type_DeviceRGB;
		}
		else
		{	cc.paint.values[3] = rtc(3).values[shift_in(prtc[3])];
			cs.type = &gs_color_space_type_DeviceCMYK;
		}
#undef rtc
#undef shift_in
	}
	return (*cs.type->remap_color)(&cc, &cs, pdc, pgs);
}

/* ------ Adjust reference counts for a CIE color space ------ */

int
gx_adjust_CIEBasedABC(gs_color_space *pcs, gs_state *pgs, int delta)
{	rc_adjust(pcs->params.abc, delta, pgs->memory_procs,
		  "gx_adjust_CIEBasedABC");
	return 0;
}

int
gx_adjust_CIEBasedA(gs_color_space *pcs, gs_state *pgs, int delta)
{	rc_adjust(pcs->params.a, delta, pgs->memory_procs,
		  "gx_adjust_CIEBasedA");
	return 0;
}

/* ------ Install a CIE color space ------ */
/* These routines should load the cache, but they don't. */

int
gx_install_CIEBasedABC(gs_color_space *pcs, gs_state *pgs)
{	gs_cie_abc *pcie = pcs->params.abc;
	cie_matrix_init(&pcie->common.MatrixLMN);
	cie_matrix_init(&pcie->MatrixABC);
	if ( pgs->cie_render == 0 )
		return 0;
	rc_unshare(pgs->cie_joint_caches, gx_cie_joint_caches,
		   pgs->memory_procs, return gs_error_VMerror,
		   "gx_install_CIEBasedABC");
	return gx_cie_joint_caches_init(pgs->cie_joint_caches,
		&pcie->common, pgs->cie_render);
}

int
gx_install_CIEBasedA(gs_color_space *pcs, gs_state *pgs)
{	gs_cie_a *pcie = pcs->params.a;
	cie_matrix_init(&pcie->common.MatrixLMN);
	if ( pgs->cie_render == 0 )
		return 0;
	rc_unshare(pgs->cie_joint_caches, gx_cie_joint_caches,
		   pgs->memory_procs, return gs_error_VMerror,
		   "gx_install_CIEBasedA");
	return gx_cie_joint_caches_init(pgs->cie_joint_caches,
		&pcie->common, pgs->cie_render);
}

/* ------ Utilities ------ */

#define if_debug_vector3(str, vec)\
  if_debug4('c', "%s[%g %g %g]\n", str, vec->u, vec->v, vec->w)
#define if_debug_matrix3(str, mat)\
  if_debug10('c', "%s[%g %g %g / %g %g %g / %g %g %g]\n", str,\
    mat->cu.u, mat->cu.v, mat->cu.w, mat->cv.u, mat->cv.v, mat->cv.w,\
    mat->cw.u, mat->cw.v, mat->cw.w)

/* Multiply a vector by a matrix. */
/* Optimizing this routine is the justification for is_identity! */
private void near
cie_mult3(const gs_vector3 *in, register const gs_matrix3 *mat, gs_vector3 *out)
{	if_debug_vector3("[c]mult", in);
	if_debug_matrix3("      *", mat);
	if ( mat->is_identity )
		*out = *in;
	else
	{	float u = in->u, v = in->v, w = in->w;
		out->u = (u * mat->cu.u) + (v * mat->cu.v) + (w * mat->cu.w);
		out->v = (u * mat->cv.u) + (v * mat->cv.v) + (w * mat->cv.w);
		out->w = (u * mat->cw.u) + (v * mat->cw.v) + (w * mat->cw.w);
	}
	if_debug_vector3("      =", out);
}

/* Multiply two matrices.  We assume the result is not an alias for */
/* either of the operands. */
private void near
cie_matrix_mult3(const gs_matrix3 *ma, const gs_matrix3 *mb, gs_matrix3 *mc)
{	gs_vector3 row_in, row_out;
	if_debug_matrix3("[c]matrix_mult", ma);
	if_debug_matrix3("             *", mb);
#define mult_row(e)\
  row_in.u = ma->cu.e, row_in.v = ma->cv.e, row_in.w = ma->cw.e;\
  cie_mult3(&row_in, mb, &row_out);\
  mc->cu.e = row_out.u, mc->cv.e = row_out.v, mc->cw.e = row_out.w
	mult_row(u);
	mult_row(v);
	mult_row(w);
#undef mult_row
	cie_matrix_init(mc);
	if_debug_matrix3("             =", mc);
}

/* Invert a matrix. */
/* The output must not be an alias for the input. */
private void near
cie_invert3(register const gs_matrix3 *in, register gs_matrix3 *out)
{	/* This is a brute force algorithm; maybe there are better. */
	/* We label the array elements */
	/*   [ A B C ]   */
	/*   [ D E F ]   */
	/*   [ G H I ]   */
#define A cu.u
#define B cv.u
#define C cw.u
#define D cu.v
#define E cv.v
#define F cw.v
#define G cu.w
#define H cv.w
#define I cw.w
	double coA = in->E * in->I - in->F * in->H; 
	double coB = in->F * in->G - in->D * in->I; 
	double coC = in->D * in->H - in->E * in->G;
	double det = in->A * coA + in->B * coB + in->C * coC;
	if_debug_matrix3("[c]invert", in);
	out->A = coA / det;
	out->D = coB / det;
	out->G = coC / det;
	out->B = (in->C * in->H - in->B * in->I) / det;
	out->E = (in->A * in->I - in->C * in->G) / det;
	out->H = (in->B * in->G - in->A * in->H) / det;
	out->C = (in->B * in->F - in->C * in->E) / det;
	out->F = (in->C * in->D - in->A * in->F) / det;
	out->I = (in->A * in->E - in->B * in->D) / det;
	if_debug_matrix3("        =", out);
#undef A
#undef B
#undef C
#undef D
#undef E
#undef F
#undef G
#undef H
#undef I
	out->is_identity = in->is_identity;
}

/* Force values within bounds. */
private void near
cie_restrict3(const gs_vector3 *in, const gs_range3 *range, gs_vector3 *out)
{	float temp;
	temp = in->u; restrict(temp, range->u, out->u);
	temp = in->v; restrict(temp, range->v, out->v);
	temp = in->w; restrict(temp, range->w, out->w);
}

private void near
cie_lookup3(const gs_vector3 *in, const gx_cie_cache *pc /*[3]*/, gs_vector3 *out)
{	if_debug4('c', "[c]lookup 0x%lx [%g %g %g]\n", (ulong)pc,
		  in->u, in->v, in->w);
	lookup(in->u, pc, out->u); pc++;
	lookup(in->v, pc, out->v); pc++;
	lookup(in->w, pc, out->w);
	if_debug_vector3("        =", out);
}

/* Set the is_identity flag that accelerates multiplication. */
private void near
cie_matrix_init(register gs_matrix3 *mat)
{	mat->is_identity =
		mat->cu.u == 1.0 && is_fzero2(mat->cu.v, mat->cu.w) &&
		mat->cv.v == 1.0 && is_fzero2(mat->cv.u, mat->cv.w) &&
		mat->cw.w == 1.0 && is_fzero2(mat->cw.u, mat->cw.v);
}
