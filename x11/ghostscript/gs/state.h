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

/* state.h */
/* Ghostscript interpreter graphics state definition */

/* Note that from the interpreter's point of view, */
/* the graphics state is opaque, i.e. the interpreter is */
/* just another client of the library. */

/* The interpreter requires additional items in the graphics state. */
/* These are "client data" from the library's point of view. */
/* Note that they are all refs. */

typedef struct int_gstate_s int_gstate;

/* Most of the complexity in the interpreter state comes from */
/* the parameters associated with the various Level 2 color spaces. */

/* CIE transformation procedures */
typedef struct ref_cie_procs_s {
	union {
		ref ABC;
		ref A;
	} Decode;
	ref DecodeLMN;
} ref_cie_procs;
/* CIE rendering transformation procedures */
typedef struct ref_cie_render_procs_s {
	ref TransformPQR, EncodeLMN, EncodeABC, RenderTableT;
} ref_cie_render_procs;

/* Separation name and tint transform */
typedef struct ref_separation_params_s {
	ref layer_name, tint_transform;
} ref_separation_params;

/* All color space parameters. */
/* All of these are optional. */
/* Note that they may actually be the parameters for an underlying or */
/* alternate space for a special space. */
typedef struct ref_color_procs_s {
	ref_cie_procs cie;
	union {
		ref_separation_params separation;
		ref index_proc;
	} special;
} ref_color_procs;
typedef struct ref_colorspace_s {
	ref array;		/* color space (array), */
		/* only relevant if the current */
		/* color space has parameters associated with it. */
	ref_color_procs procs;	/* associated procedures/parameters, */
		/* only relevant for CIE, Separation, Indexed/CIE, */
		/* or a Pattern with one of the above */
} ref_colorspace;

struct int_gstate_s {
		/* Screen_procs are only relevant if setscreen was */
		/* executed more recently than sethalftone */
		/* (for this graphics context). */
	struct {
		ref red, green, blue, gray;
	} screen_procs,			/* halftone screen procedures */
	  transfer_procs;		/* transfer procedures */
	ref black_generation;		/* (procedure) */
	ref undercolor_removal;		/* (procedure) */
	ref font;			/* font object (dictionary) */
	ref_colorspace colorspace;
		/* Pattern is only relevant if the current color space */
		/* is a pattern space. */
	ref pattern;			/* pattern (dictionary) */
	struct {
		ref dict;		/* CIE color rendering dictionary */
		ref_cie_render_procs procs;	/* (see above) */
	} colorrendering;
		/* Halftone is only relevant if sethalftone was executed */
		/* more recently than setscreen for this graphics context. */
		/* setscreen sets it to null. */
	ref halftone;			/* halftone (dictionary) */
};
/* Enumerate the refs in an int_gstate. */
/* Since all the elements of an int_gstate are refs, this is simple. */
#define int_gstate_map_refs(p,m)\
 { register ref *rp_ = (ref *)(p);\
   register int i = sizeof(int_gstate) / sizeof(ref);\
   do { m(rp_); ++rp_; } while ( --i );\
 }

/* The current instances. */
extern int_gstate istate;
extern gs_state *igs;
