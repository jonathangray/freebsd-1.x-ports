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

/* gscolor2.h */
/* Client interface to Level 2 color routines for Ghostscript library */
/* (requires gscspace.h, gxcolor.h) */

/* Note: clients should use rc_alloc_struct_0 (in gxrefct.h) to allocate */
/* CIE color spaces or rendering structures. */

/* General color routines */
const gs_color_space *gs_currentcolorspace(P1(const gs_state *));
int	gs_setcolorspace(P2(gs_state *, gs_color_space *));
const gs_client_color *gs_currentcolor(P1(const gs_state *));
int	gs_setcolor(P2(gs_state *, const gs_client_color *));
int	gs_currentoverprint(P1(const gs_state *));
void	gs_setoverprint(P2(gs_state *, int));

/* CIE-specific routines */
struct gs_cie_render_s;
typedef struct gs_cie_render_s gs_cie_render;
const gs_cie_render *gs_currentcolorrendering(P1(const gs_state *));
int	gs_setcolorrendering(P2(gs_state *, gs_cie_render *));
