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

/* gsimage.h */
/* Image painting interface for Ghostscript library */
/* Requires gscspace.h, gsstate.h, and gsmatrix.h */

/* Image painting.  This is done in an enumeration style: */
/* the client initializes an enumerator, then supplies data incrementally. */
typedef struct gs_image_enum_s gs_image_enum;
extern const uint gs_image_enum_sizeof;
/* image_init and imagemask_init return 1 for an empty image, */
/* 0 normally, <0 on error. */
int	gs_image_init(P9(gs_image_enum *penum, gs_state *pgs,
			 int width, int height, int bits_per_component,
			 int spread, const gs_color_space *pcs,
			 const float *decode, gs_matrix *pmat));
int	gs_imagemask_init(P7(gs_image_enum *penum, gs_state *pgs,
			     int width, int height, int invert,
			     gs_matrix *pmat, int adjust));
int	gs_image_next(P3(gs_image_enum *penum, byte *dbytes, uint dsize));
/* Clean up after processing an image. */
void	gs_image_cleanup(P1(gs_image_enum *penum));
