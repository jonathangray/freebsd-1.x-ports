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

/* gsstate.c */
/* Miscellaneous graphics state operators for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gxrefct.h"			/* early for gscie.h */
#include "gxcolor.h"			/* for gscolor2.h, gscie.h */
#include "gzstate.h"
#include "gzdevice.h"
#include "gscspace.h"
#include "gscolor2.h"
#include "gscie.h"
#include "gzcolor.h"			/* requires gxdevice.h */
#include "gzht.h"
#include "gzline.h"
#include "gzpath.h"

/* Imported values */
/* The following should include a 'const', but */
/* the Watcom compiler won't accept it. */
extern /*const*/ gx_color_map_procs *cmap_procs_default;

/* Forward references */
private gs_state *alloc_gstate(P3(const gs_memory_procs *, const gs_state *,
  const char *));
private int alloc_gstate_contents(P1(gs_state *));
private void free_gstate_contents(P1(gs_state *));
private void copy_gstate_contents(P2(gs_state *pto, const gs_state *pfrom));

/* The structure for allocating (most of) the contents of a gstate */
/* all at once. */
typedef struct gs_state_contents_s {
	gx_path path;
	gx_clip_path clip_path;
	line_params line_params;
	halftone_params halftone;
	gs_color_space color_space;
	gs_client_color ccolor;
	gx_device_color dev_color;
} gs_state_contents;
#define gstate_contents(pgs) ((gs_state_contents *)((pgs)->path))

/* Allocate and free a structure */
#define alloc_struct(pgs,typ,cname)\
  (typ *)(*(pgs)->memory_procs->alloc)(1, sizeof(typ), cname)
#define free_struct(pgs,ptr,cname)\
  (*(pgs)->memory_procs->free)((char *)(ptr), 1, sizeof(*(ptr)), cname)

/* ------ Operations on the entire graphics state ------ */

/* Allocate and initialize a graphics state. */
private float
null_transfer(const gs_state *pgs, floatp gray)
{	return gray;
}
gs_state *
gs_state_alloc(const gs_memory_procs *mprocs)
{	register gs_state *pgs =
		alloc_gstate(mprocs, (gs_state *)0, "gs_state_alloc");
	if ( pgs == 0 ) return 0;
	pgs->saved = 0;
	/* Initialize things not covered by initgraphics */
	gx_path_init(pgs->path, pgs->memory_procs);
	gx_cpath_init(pgs->clip_path, pgs->memory_procs);
	gx_alloc_ht_cache(pgs);
	pgs->halftone->width = pgs->halftone->height =
		pgs->halftone->order_size = 0;
	gs_sethalftonephase(pgs, 0, 0);
	/* Initialize things so that gx_remap_color won't crash. */
	gx_set_black(pgs);
	pgs->overprint = 0;
	pgs->black_generation = 0;
	pgs->undercolor_removal = 0;
	pgs->cmap_procs = cmap_procs_default;
	{	gx_transfer *ptran = &pgs->transfer;
		rc_alloc_struct_n(ptran->gray, gx_transfer_map, mprocs,
				  return 0, "gs_state_alloc", 4);
		ptran->red = ptran->green = ptran->blue = ptran->gray;
		ptran->gray->proc = null_transfer;
		ptran->gray->values[0] = frac_0;
	}
	gs_nulldevice(pgs);
	gs_settransfer(pgs, null_transfer);
	gs_setflat(pgs, 1.0);
	gs_setstrokeadjust(pgs, 1);
	/****** What about the font ? ******/
	pgs->in_cachedevice = pgs->in_charpath = 0;
	pgs->show_gstate = 0;
	pgs->level = 0;
	pgs->fill_adjust = float2fixed(0.25);
	pgs->client_data = 0;
	if ( gs_initgraphics(pgs) < 0 )
	   {	/* Something went very wrong */
		return 0;
	   }
	return pgs;
}

/* Set the client data in a graphics state. */
/* This should only be done to a newly created state. */
void
gs_state_set_client(gs_state *pgs, char/*void*/ *pdata,
  const gs_state_client_procs *pprocs)
{	pgs->client_data = pdata;
	pgs->client_procs = *pprocs;
}

/* Get the client data from a graphics state. */
char/*void*/ *
gs_state_client_data(gs_state *pgs)
{	return pgs->client_data;
}

/* Free a graphics state */
int
gs_state_free(gs_state *pgs)
{	free_gstate_contents(pgs);
	free_struct(pgs, pgs, "gs_state_free");
	return 0;
}

/* Save the graphics state */
int
gs_gsave(gs_state *pgs)
{	gs_state *pnew = alloc_struct(pgs, gs_state, "gs_gsave");
	if ( pnew == 0 )
		return_error(gs_error_VMerror);
	*pnew = *pgs;
	if ( alloc_gstate_contents(pgs) < 0 )
	   {	*pgs = *pnew;		/* undo partial alloc */
		free_struct(pgs, pnew, "gs_gsave");
		return_error(gs_error_VMerror);
	   }
	copy_gstate_contents(pgs, pnew);
	/* Make pnew, not pgs, have the newly-allocated client data. */
	{	char/*void*/ *pdata = pgs->client_data;
		pgs->client_data = pnew->client_data;
		pnew->client_data = pdata;
	}
	pgs->saved = pnew;
	if ( pgs->show_gstate == pgs )
		pgs->show_gstate = pnew->show_gstate = pnew;
	pgs->level++;
	if_debug2('g', "[g]gsave -> 0x%lx, level = %d\n",
		  (ulong)pnew, pgs->level);
	return 0;
}

/* Restore the graphics state. */
int
gs_grestore(gs_state *pgs)
{	gs_state *saved = pgs->saved;
	char/*void*/ *pdata = pgs->client_data;
	char/*void*/ *sdata;
	if_debug2('g', "[g]grestore 0x%lx, level was %d\n",
		  (ulong)saved, pgs->level);
	if ( !saved ) return gs_gsave(pgs);	/* shouldn't happen */
	sdata = saved->client_data;
	/* Swap back the client data pointers. */
	pgs->client_data = sdata;
	saved->client_data = pdata;
	if ( pdata != 0 && sdata != 0 )
		(*pgs->client_procs.copy)(pdata, sdata);
	free_gstate_contents(pgs);
	*pgs = *saved;
	if ( pgs->show_gstate == saved )
		pgs->show_gstate = pgs;
	free_struct(pgs, saved, "gs_grestore");
	return (pgs->saved == 0 ? gs_gsave(pgs) : 0);
}

/* Restore to the bottommost graphics state. */
int
gs_grestoreall(gs_state *pgs)
{	if ( !pgs->saved ) return gs_gsave(pgs);	/* shouldn't happen */
	while ( pgs->saved->saved ) gs_grestore(pgs);
	return gs_grestore(pgs);
}

/* Allocate and return a new graphics state. */
gs_state *
gs_gstate(gs_state *pgs)
{	gs_state *pnew = alloc_gstate(pgs->memory_procs, pgs, "gs_gstate");
	if ( pnew == 0 ) return 0;
	copy_gstate_contents(pnew, pgs);
	pnew->saved = 0;
	return pnew;
}

/* Copy one previously allocated graphics state to another. */
int
gs_copygstate(gs_state *pto, const gs_state *pfrom)
{	/* This is the same as currentgstate. */
	return gs_currentgstate(pto, pfrom);
}

/* Copy the current graphics state to a previously allocated one. */
int
gs_currentgstate(gs_state *pto, const gs_state *pgs)
{	/* We have to copy both the scalar and composite parts */
	/* of the state. */
	gs_state sgs;
	sgs = *pto;
	*pto = *pgs;
	/* Put back the composite part pointers. */
#define gcopy(element)\
    pto->element = sgs.element
	gcopy(path);
	gcopy(clip_path);
	gcopy(line_params);
	gcopy(halftone);
	gcopy(color_space);
	gcopy(ccolor);
	gcopy(dev_color);
	gcopy(cie_render);
	gcopy(transfer.red);
	gcopy(transfer.green);
	gcopy(transfer.blue);
	gcopy(transfer.gray);
	gcopy(device);
	gcopy(client_data);
#undef gcopy
	copy_gstate_contents(pto, pgs);
	return 0;
}

/* Restore the current graphics state from a previously allocated one. */
int
gs_setgstate(gs_state *pgs, const gs_state *pfrom)
{	/* The implementation is the same as currentgstate, */
	/* except we must preserve the saved pointer and the level. */
	gs_state *saved = pgs->saved;
	int level = pgs->level;
	int code = gs_currentgstate(pgs, pfrom);
	if ( code < 0 ) return code;
	pgs->saved = saved;
	pgs->level = level;
	return 0;
}

/* Swap the saved pointer of the graphics state. */
/* This is provided only for save/restore. */
gs_state *
gs_state_swap_saved(gs_state *pgs, gs_state *new_saved)
{	gs_state *saved = pgs->saved;
	pgs->saved = new_saved;
	return saved;
}

/* ------ Operations on components ------ */

/* Reset most of the graphics state */
int
gs_initgraphics(register gs_state *pgs)
{	int code;
	gs_initmatrix(pgs);
	if (	(code = gs_newpath(pgs)) < 0 ||
		(code = gs_initclip(pgs)) < 0 ||
		(code = gs_setlinewidth(pgs, 1.0)) < 0 ||
		(code = gs_setlinecap(pgs, gs_cap_butt)) < 0 ||
		(code = gs_setlinejoin(pgs, gs_join_miter)) < 0 ||
		(code = gs_setdash(pgs, (float *)0, 0, 0.0)) < 0 ||
		(code = gs_setgray(pgs, 0.0)) < 0 ||
		(code = gs_setmiterlimit(pgs, 10.0)) < 0
	   ) return code;
	return 0;
}

/* setflat */
int
gs_setflat(gs_state *pgs, floatp flat)
{	if ( flat <= 0.2 ) flat = 0.2;
	else if ( flat > 100 ) flat = 100;
	pgs->flatness = flat;
	return 0;
}

/* currentflat */
float
gs_currentflat(const gs_state *pgs)
{	return pgs->flatness;
}

/* setstrokeadjust */
int
gs_setstrokeadjust(gs_state *pgs, int stroke_adjust)
{	pgs->stroke_adjust = stroke_adjust;
	return 0;
}

/* currentstrokeadjust */
int
gs_currentstrokeadjust(const gs_state *pgs)
{	return pgs->stroke_adjust;
}

/* ------ Internal routines ------ */

/* Allocate a graphics state object and its contents, */
/* optionally initializing it from an existing object. */
/* Return 0 if the allocation fails. */
private gs_state *
alloc_gstate(const gs_memory_procs *mprocs, const gs_state *pold,
  const char *cname)
{	gs_state *pgs =
		(gs_state *)(*mprocs->alloc)(1, sizeof(gs_state), cname);
	if ( pgs == 0 ) return 0;
	if ( pold != 0 )
		*pgs = *pold;
	else
		pgs->transfer.red = pgs->transfer.green =
		  pgs->transfer.blue = pgs->transfer.gray = 0,
		pgs->cie_render = 0,
		pgs->cie_joint_caches = 0,
		pgs->client_data = 0;
	pgs->memory_procs = mprocs;
	if ( alloc_gstate_contents(pgs) < 0 )
	   {	free_struct(pgs, pgs, cname);
		return 0;
	   }
	return pgs;
}

/* Allocate the contents of a graphics state object. */
/* Return -1 if the allocation fails. */
/* Note that the contents have been smashed in this case. */
private int
alloc_gstate_contents(register gs_state *pgs)
{	gs_proc_alloc_t palloc = pgs->memory_procs->alloc;
	static const char cname[] = "alloc_gstate_contents";
	gs_state_contents *cont =
	  (gs_state_contents *)(*palloc)(1, sizeof(gs_state_contents), cname);
	if ( cont == 0 ) return -1;
#define gset(element)\
  pgs->element = &cont->element;
	gset(path);
	gset(clip_path);
	gset(line_params);
	gset(halftone);
	gset(color_space);
	cont->color_space.type = &gs_color_space_type_DeviceGray;	/* for cs_adjust_count */
	gset(ccolor);
	gset(dev_color);
#undef gset
#define galloc(element,type,fail)\
  if ( (pgs->element = (type *)(*palloc)(1, sizeof(type), cname)) == 0 )\
    goto fail
#define gtalloc(element,fail)\
  rc_allocate_struct(pgs->element, gx_transfer_map, pgs->memory_procs,\
	      goto fail, cname);
	gtalloc(transfer.red, utr);
	gtalloc(transfer.green, utg);
	gtalloc(transfer.blue, utb);
	gtalloc(transfer.gray, uty);
	galloc(device, device, ud);
#undef galloc
#undef gtalloc
	if ( pgs->client_data != 0 )
	{	if ( (pgs->client_data =
		       (*pgs->client_procs.alloc)(pgs->memory_procs)) == 0
		   )
			goto ucd;
	}
	rc_increment(pgs->cie_render);
	rc_increment(pgs->cie_joint_caches);
	pgs->device_is_shared = 0;
	return 0;
	/* Undo partial allocations if an allocation failed. */
#define gunalloc(element) free_struct(pgs, pgs->element, cname)
ucd:	gunalloc(device);
ud:	gunalloc(transfer.gray);
uty:	gunalloc(transfer.blue);
utb:	gunalloc(transfer.green);
utg:	gunalloc(transfer.red);
utr:	free_struct(pgs, cont, cname);
	return -1;
#undef gunalloc
}

/* Free the contents of a graphics state, but not the state itself. */
private void
free_gstate_contents(gs_state *pgs)
{	gs_proc_free_t pfree = pgs->memory_procs->free;
	static const char cname[] = "free_gstate_contents";
	if ( pgs->client_data != 0 )
	    (*pgs->client_procs.free)(pgs->client_data, pgs->memory_procs);
#define gfree(element)\
    (*pfree)((char *)pgs->element, 1, sizeof(*pgs->element), cname)
	gx_cpath_release(pgs->clip_path);
	gx_path_release(pgs->path);
	if ( !pgs->device_is_shared )
		gfree(device);
	rc_decrement(pgs->transfer.gray, pgs->memory_procs, cname);
	rc_decrement(pgs->transfer.blue, pgs->memory_procs, cname);
	rc_decrement(pgs->transfer.green, pgs->memory_procs, cname);
	rc_decrement(pgs->transfer.red, pgs->memory_procs, cname);
	rc_decrement(pgs->cie_render, pgs->memory_procs, cname);
	rc_decrement(pgs->cie_joint_caches, pgs->memory_procs, cname);
	cs_adjust_count(pgs, -1);
	(*pfree)((char *)gstate_contents(pgs), 1, sizeof(gs_state_contents),
		 cname);
#undef gfree
}

/* Copy the composite parts of a graphics state. */
private void
copy_gstate_contents(gs_state *pto, const gs_state *pfrom)
{	static const char cname[] = "copy_gstate_contents";
	cs_adjust_count(pto, -1);
	*gstate_contents(pto) = *gstate_contents(pfrom);
	cs_adjust_count(pto, 1);
#define gcopy(element)\
    *pto->element = *pfrom->element
#define rccopy(element)\
    rc_assign(pto->element, pfrom->element, pto->memory_procs, cname);
	rccopy(transfer.gray);
	rccopy(transfer.blue);
	rccopy(transfer.green);
	rccopy(transfer.red);
	rccopy(cie_render);
	rccopy(cie_joint_caches);
	gcopy(device);
#undef gcopy
#undef rccopy
	if ( pfrom->client_data != 0 )
	    (*pfrom->client_procs.copy)(pto->client_data, pfrom->client_data);
	gx_path_share(pto->path);
	gx_cpath_share(pto->clip_path);
}
