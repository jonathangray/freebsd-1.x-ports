/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.

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

/* gscolor.h */
/* Client interface to color routines for Ghostscript library */

#ifndef gscolor_INCLUDED
#  define gscolor_INCLUDED

/* Common definition for mapping procedures */
typedef float (*gs_mapping_proc)(P2(const gs_state *, floatp));

/* Color and gray interface */
int	gs_setgray(P2(gs_state *, floatp));
float	gs_currentgray(P1(const gs_state *));
int	gs_sethsbcolor(P4(gs_state *, floatp, floatp, floatp)),
	gs_currenthsbcolor(P2(const gs_state *, float [3])),
	gs_setrgbcolor(P4(gs_state *, floatp, floatp, floatp)),
	gs_currentrgbcolor(P2(const gs_state *, float [3])),
	gs_setcmykcolor(P5(gs_state *, floatp, floatp, floatp, floatp)),
	gs_currentcmykcolor(P2(const gs_state *, float [4]));
int	gs_setblackgeneration(P2(gs_state *, gs_mapping_proc));
gs_mapping_proc	gs_currentblackgeneration(P1(const gs_state *));
int	gs_setundercolorremoval(P2(gs_state *, gs_mapping_proc));
gs_mapping_proc	gs_currentundercolorremoval(P1(const gs_state *));
/* Transfer function */
int	gs_settransfer(P2(gs_state *, gs_mapping_proc)),
	gs_settransfer_remap(P3(gs_state *, gs_mapping_proc, int));
gs_mapping_proc	gs_currenttransfer(P1(const gs_state *));
int	gs_setcolortransfer(P5(gs_state *, gs_mapping_proc /*red*/,
			gs_mapping_proc /*green*/, gs_mapping_proc /*blue*/,
			gs_mapping_proc /*gray*/)),
	gs_setcolortransfer_remap(P6(gs_state *, gs_mapping_proc /*red*/,
			gs_mapping_proc /*green*/, gs_mapping_proc /*blue*/,
			gs_mapping_proc /*gray*/, int));
void	gs_currentcolortransfer(P2(const gs_state *, gs_mapping_proc [4]));

#endif					/* gscolor_INCLUDED */
