/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.

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

/* gzstate.h */
/* Private graphics state definition for Ghostscript library */
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gsstate.h"

/* Composite components of the graphics state. */
struct gx_transfer_map_s;
typedef struct gx_transfer_s {
	struct gx_transfer_map_s *red;
	struct gx_transfer_map_s *green;
	struct gx_transfer_map_s *blue;
	struct gx_transfer_map_s *gray;
} gx_transfer;

/* Graphics state structure.  See pp. 59-60 of the PostScript manual. */
struct gs_state_s {
	gs_state *saved;		/* previous state from gsave */
	const gs_memory_procs *memory_procs;
/* Transformation: */
	gs_matrix_fixed ctm;
#define ctm_only(pgs) *(gs_matrix *)&(pgs)->ctm
	gs_matrix ctm_inverse;
	int inverse_valid;		/* true if ctm_inverse = ctm^-1 */
/* Paths: */
	struct gx_path_s *path;
	struct gx_clip_path_s *clip_path;
	int clip_rule;
/* Lines: */
	struct line_params_s *line_params;
/* Halftone screen: */
	struct halftone_params_s *halftone;
	float (*ht_proc)(P2(floatp, floatp));
	gs_int_point ht_phase;
	gs_int_point phase_mod;		/* negated phase mod tile size */
	struct gx_ht_cache_s *ht_cache;	/* shared by all GCs */
/* Color (device-independent): */
	struct gs_color_space_s *color_space;
	struct gs_client_color_s *ccolor;
/* Color (device-dependent): */
	struct gs_cie_render_s *cie_render;
	int overprint;
	float (*black_generation)(P2(const gs_state *, floatp));
	float (*undercolor_removal)(P2(const gs_state *, floatp));
	gx_transfer transfer;
/* Color caches: */
	struct gx_device_color_s *dev_color;
	struct gx_cie_joint_caches_s *cie_joint_caches;
	const struct gx_color_map_procs_s *cmap_procs;
/* Font: */
	struct gs_font_s *font;
	gs_matrix_fixed char_tm;	/* font matrix * ctm */
#define char_tm_only(pgs) *(gs_matrix *)&(pgs)->char_tm
	int char_tm_valid;		/* true if char_tm is valid */
	byte in_cachedevice;		/* true after a setcachedevice */
	byte in_charpath;		/* 0 if not in charpath, */
					/* 1 if false charpath, */
					/* 2 if true charpath */
					/* (see charpath_flag in */
					/* gs_show_enum_s) */
	gs_state *show_gstate;		/* gstate when show was invoked */
					/* (so charpath can append to path) */
/* Other stuff: */
	int level;			/* incremented by 1 per gsave */
	float flatness;
	fixed fill_adjust;		/* fattening for fill */
	int stroke_adjust;
	struct device_s *device;
	int device_is_shared;		/* true if device is shared, */
					/* so don't deallocate at grestore */
/* Client data: */
	char/*void*/ *client_data;
	gs_state_client_procs client_procs;
};
