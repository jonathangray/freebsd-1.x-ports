/* Copyright (C) 1991, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gscspace.h */
/* Client interface to color spaces for Ghostscript library */

#ifndef gscspace_INCLUDED
#  define gscspace_INCLUDED

/* Color space type indices */
typedef enum {
		/* Supported in all configurations */
	gs_color_space_index_DeviceGray = 0,
	gs_color_space_index_DeviceRGB,
	gs_color_space_index_DeviceCMYK,
		/* Supported in Level 2 only */
	gs_color_space_index_CIEBasedABC,
	gs_color_space_index_CIEBasedA,
	gs_color_space_index_Separation,
	gs_color_space_index_Indexed,
	gs_color_space_index_Pattern
} gs_color_space_index;

/*
 * Color spaces are complicated because different spaces involve
 * different kinds of parameters, as follows:

Space		Space parameters		Color parameters
-----		----------------		----------------
DeviceGray	(none)				1 real [0-1]
DeviceRGB	(none)				3 reals [0-1]
DeviceCMYK	(none)				4 reals [0-1]
CIEBasedABC	dictionary			3 reals
CIEBasedA	dictionary			1 real
Separation	name, alt_space, tint_xform	1 real [0-1]
Indexed		base_space, hival, lookup	1 int [0-hival]
Pattern		colored: (none)			dictionary
		uncolored: base_space		dictionary + base space params

Space		Underlying or alternate space
-----		-----------------------------
Separation	Device, CIE
Indexed		Device, CIE
Pattern		Device, CIE, Separation, Indexed

 */

/* Color space type objects */
struct gs_client_color_s;
struct gs_color_space_s;
struct gx_device_color_s;
/* Map a color to a device color. */
#define cs_proc_remap_color(proc)\
  int proc(P4(const struct gs_client_color_s *,\
	      const struct gs_color_space_s *,\
	      struct gx_device_color_s *,\
	      gs_state *))
/* Install the color space in a graphics state. */
#define cs_proc_install_cspace(proc)\
  int proc(P2(struct gs_color_space_s *, gs_state *))
cs_proc_install_cspace(gx_no_install_cspace);
/* Adjust reference counts of indirect components. */
#define cs_proc_adjust_count(proc)\
  int proc(P3(struct gs_color_space_s *, gs_state *, int))
cs_proc_adjust_count(gx_no_adjust_count);
#define cs_adjust_count(pgs, delta)\
  (*(pgs)->color_space->type->adjust_count)((pgs)->color_space, pgs, delta)
/* Color space types (classes): */
typedef struct gs_color_space_type_s {
	gs_color_space_index index;
	int num_components;		/* # of components in a color */
					/* in this space, -1 if variable */
	cs_proc_remap_color((*remap_color));
	cs_proc_install_cspace((*install_cspace));
	cs_proc_adjust_count((*adjust_count));
} gs_color_space_type;

/* Standard color space types */
extern const gs_color_space_type
	gs_color_space_type_DeviceGray,
	gs_color_space_type_DeviceRGB,
	gs_color_space_type_DeviceCMYK;

	/* Base color spaces (Device and CIE) */

typedef struct gs_cie_abc_s gs_cie_abc;
typedef struct gs_cie_a_s gs_cie_a;
#define gs_base_cspace_params\
	gs_cie_abc *abc;\
	gs_cie_a *a
typedef struct gs_base_color_space_s {
	const gs_color_space_type *type;
	union {
		gs_base_cspace_params;
	} params;
} gs_base_color_space;

	/* Paint (non-pattern) color spaces (base + Separation + Indexed) */

typedef ulong gs_separation_name;		/* BOGUS */

typedef struct gs_separation_params_s {
	gs_separation_name sname;
	gs_base_color_space alt_space;
	int (*tint_transform)(P2(floatp, float *));
} gs_separation_params;
typedef struct gs_indexed_params_s {
	gs_base_color_space base_space;
	int hival;
	union {
		const byte *table;
		int (*proc)(P2(int, float *));
	} lookup;
	int use_proc;		/* 0 = use table, 1 = use proc */
} gs_indexed_params;
#define gs_paint_cspace_params\
	gs_base_cspace_params;\
	gs_separation_params separation;\
	gs_indexed_params indexed
typedef struct gs_paint_color_space_s {
	const gs_color_space_type *type;
	union {
		gs_paint_cspace_params;
	} params;
} gs_paint_color_space;

	/* General color spaces (including patterns) */

typedef struct gs_pattern_params_s {
	int has_base_space;
	gs_paint_color_space base_space;
} gs_pattern_params;
struct gs_color_space_s {
	const gs_color_space_type *type;
	union {
		gs_paint_cspace_params;
		gs_pattern_params pattern;
	} params;
};
typedef struct gs_color_space_s gs_color_space;

#endif					/* gscspace_INCLUDED */
