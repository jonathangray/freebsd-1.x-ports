/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gsfont.c */
/* Font operators for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"			/* must precede gxdevice */
#include "gxdevice.h"			/* must precede gxfont */
#include "gschar.h"
#include "gxfont.h"
#include "gxfdir.h"

/* Imported procedures */
void	gs_purge_font_from_char_caches(P2(gs_font_dir *, const gs_font *));

/* Size of cache structures */
extern const uint cached_char_sizeof;
extern const uint cached_fm_pair_sizeof;

/* Define the sizes of the various aspects of the font/character cache. */
/*** Big memory machines ***/
#define smax_LARGE 50		/* smax - # of scaled fonts */
#define bmax_LARGE 500000	/* bmax - space for cached chars */
#define mmax_LARGE 200		/* mmax - # of cached font/matrix pairs */
#define cmax_LARGE 5000		/* cmax - # of cached chars */
#define blimit_LARGE 2500	/* blimit/upper - max size of a single cached char */
/*** Small memory machines ***/
#define smax_SMALL 20		/* smax - # of scaled fonts */
#define bmax_SMALL 25000	/* bmax - space for cached chars */
#define mmax_SMALL 40		/* mmax - # of cached font/matrix pairs */
#define cmax_SMALL 500		/* cmax - # of cached chars */
#define blimit_SMALL 100	/* blimit/upper - max size of a single cached char */

/* Allocate a font directory */
gs_font_dir *
gs_font_dir_alloc(const gs_memory_procs *mprocs)
{	/* Try allocating a very large cache. */
	/* If this fails, allocate a small one. */
#if !arch_ints_are_short
	gs_font_dir *pdir;
	pdir = gs_font_dir_alloc_limits(mprocs,
					smax_LARGE, bmax_LARGE, mmax_LARGE,
					cmax_LARGE, blimit_LARGE);
	if ( pdir != 0 ) return pdir;
#endif
	return gs_font_dir_alloc_limits(mprocs,
					smax_SMALL, bmax_SMALL, mmax_SMALL,
					cmax_SMALL, blimit_SMALL);
}
gs_font_dir *
gs_font_dir_alloc_limits(const gs_memory_procs *mprocs,
  uint smax, uint bmax, uint mmax, uint cmax, uint upper)
{	register gs_font_dir *pdir = (gs_font_dir *)(*mprocs->alloc)(1, sizeof(gs_font_dir), "font_dir_alloc(dir)");
	uint chsize = (cmax / 5) | 31;		/* a guess */
	cached_fm_pair *mdata;
	cached_char **chars;
	if ( pdir == 0 ) return 0;
	/* Round up chsize to a power of 2. */
	while ( chsize & (chsize + 1) ) chsize |= chsize >> 1;
	chsize++;
	mdata = (cached_fm_pair *)(*mprocs->alloc)(mmax, cached_fm_pair_sizeof, "font_dir_alloc(mdata)");
	chars = (cached_char **)(*mprocs->alloc)(chsize, sizeof(cached_char *), "font_dir_alloc(chars)");
	if ( mdata == 0 || chars == 0 )
	   {	if ( chars != 0 ) (*mprocs->free)((char *)chars, chsize, sizeof(cached_char *), "font_dir_alloc(chars)");
		if ( mdata != 0 ) (*mprocs->free)((char *)mdata, mmax, cached_fm_pair_sizeof, "font_dir_alloc(mdata)");
		(*mprocs->free)((char *)pdir, 1, sizeof(gs_font_dir), "font_dir_alloc(dir)");
		return 0;
	   }
	memset((char *)pdir, 0, sizeof(gs_font_dir));	/* easiest to clear everything first */
	pdir->mprocs = mprocs;
	pdir->smax = smax;
	pdir->fmcache.mmax = mmax;
	pdir->fmcache.mdata = mdata;
	pdir->ccache.mprocs = mprocs;
	pdir->ccache.bmax = bmax;
	pdir->ccache.cmax = cmax;
	pdir->ccache.lower = upper / 10;
	pdir->ccache.upper = upper;
	pdir->ccache.chars = chars;
	pdir->ccache.chars_mask = chsize - 1;
	gx_char_cache_init(pdir);
	return pdir;
}

/* Macro for linking an element at the head of a chain */
#define link_first(first, elt)\
  if ( (elt->next = first) != NULL ) first->prev = elt;\
  elt->prev = 0;\
  first = elt

/* definefont */
/* Use this only for original (unscaled) fonts! */
int
gs_definefont(gs_font_dir *pdir, gs_font *pfont)
{	link_first(pdir->orig_fonts, pfont);
	pfont->dir = pdir;
	pfont->base = pfont;
	return 0;
}

/* scalefont */
int
gs_scalefont(gs_font_dir *pdir, const gs_font *pfont, floatp scale,
  gs_font **ppfont, gs_font **pdfont)
{	gs_matrix mat;
	gs_make_scaling(scale, scale, &mat);
	return gs_makefont(pdir, pfont, &mat, ppfont, pdfont);
}

/* makefont */
int
gs_makefont(gs_font_dir *pdir, const gs_font *pfont, const gs_matrix *pmat,
  gs_font **ppfont, gs_font **pdfont)
{	int code;
	gs_font *prev = 0;
	gs_font *pf_out = pdir->scaled_fonts;
	gs_matrix newmat;
	*pdfont = 0;
	gs_make_identity(&newmat);	/* fill in tags */
	if ( (code = gs_matrix_multiply(&pfont->FontMatrix, pmat, &newmat)) < 0 )
	  return code;
	/* Check for the font already being in the scaled font cache. */
	/* Only attempt to share fonts if the current font has */
	/* a valid UniqueID or XUID. */
#ifdef DEBUG
if ( gs_debug['m'] )
   {	if ( pfont->data.base.UID.size == 0 )	/* UniqueID */
	  dprintf1("[m]UniqueID=%ld", pfont->data.base.UID.u.id);
	else
	  dprintf1("[m]XUID(%d)", pfont->data.base.UID.size);
	dprintf7(", FontType=%d,\n[m]  new FontMatrix=[%g %g %g %g %g %g]\n",
	  pfont->FontType,
	  pmat->xx, pmat->xy, pmat->yx, pmat->yy,
	  pmat->tx, pmat->ty);
   }
#endif
	if ( uid_is_valid(&pfont->data.base.UID) )
	  for ( ; pf_out != 0; prev = pf_out, pf_out = pf_out->next )
	    if ( uid_equal(&pf_out->data.base.UID, &pfont->data.base.UID) &&
		 pf_out->base == pfont->base &&
		 pf_out->FontType == pfont->FontType &&
		 pf_out->FontMatrix.xx == newmat.xx &&
		 pf_out->FontMatrix.xy == newmat.xy &&
		 pf_out->FontMatrix.yx == newmat.yx &&
		 pf_out->FontMatrix.yy == newmat.yy &&
		 pf_out->FontMatrix.tx == newmat.tx &&
		 pf_out->FontMatrix.ty == newmat.ty
	       )
		{	*ppfont = pf_out;
			if_debug1('m', "[m]found font=%lx\n", (ulong)pf_out);
			return 0;
		}
	pf_out = (gs_font *)(*pdir->mprocs->alloc)(1, sizeof(gs_font), "gs_makefont");
	if ( !pf_out ) return_error(gs_error_VMerror);
	*pf_out = *pfont;
	pf_out->FontMatrix = newmat;
	pf_out->client_data = 0;
	if ( uid_is_valid(&pfont->data.base.UID) )
	{	if ( pdir->ssize == pdir->smax )
		{	/* Must discard a cached scaled font. */
			/* prev points to the last (oldest) font. */
			if_debug1('m', "[m]discarding font %lx\n",
				  (ulong)prev);
			*pdfont = prev;
			prev->prev->next = 0;
		}
		else
			pdir->ssize++;
		link_first(pdir->scaled_fonts, pf_out);
	}
	pf_out->dir = pdir;
	pf_out->base = pfont->base;
	*ppfont = pf_out;
	if_debug1('m', "[m]new font=%lx\n", (ulong)pf_out);
	return 1;
}

/* setfont */
int
gs_setfont(gs_state *pgs, gs_font *pfont)
{	pgs->font = pfont;
	pgs->char_tm_valid = 0;
	return 0;
}

/* currentfont */
gs_font *
gs_currentfont(const gs_state *pgs)
{	return pgs->font;
}

/* cachestatus */
void
gs_cachestatus(register const gs_font_dir *pdir, register uint pstat[7])
{	pstat[0] = pdir->ccache.bsize;
	pstat[1] = pdir->ccache.bmax;
	pstat[2] = pdir->fmcache.msize;
	pstat[3] = pdir->fmcache.mmax;
	pstat[4] = pdir->ccache.csize;
	pstat[5] = pdir->ccache.cmax;
	pstat[6] = pdir->ccache.upper;
}

/* setcachelimit */
int
gs_setcachelimit(gs_font_dir *pdir, uint size)
{	pdir->ccache.upper = size;
	return 0;
}

/* setcacheparams */
int
gs_setcachelower(gs_font_dir *pdir, uint size)
{	pdir->ccache.lower = size;
	return 0;
}
int
gs_setcacheupper(gs_font_dir *pdir, uint size)
{	pdir->ccache.upper = size;
	return 0;
}

/* currentcacheparams */
uint
gs_currentcachelower(const gs_font_dir *pdir)
{	return pdir->ccache.lower;
}
uint
gs_currentcacheupper(const gs_font_dir *pdir)
{	return pdir->ccache.upper;
}

/* Dummy (ineffective) BuildChar/BuildGlyph procedure */
int
gs_no_build_char_proc(struct gs_show_enum_s *penum, gs_state *pgs,
  gs_font *pfont, gs_char chr, gs_glyph glyph)
{	return 1;			/* failure, but not error */
}

/* Dummy character encoding procedure */
gs_glyph
gs_no_encode_char_proc(struct gs_show_enum_s *penum,
  gs_font *pfont, gs_char *pchr)
{	return gs_no_glyph;
}

/* Purge a font from all font- and character-related tables. */
/* This is only used by restore (and, someday, the GC). */
void
gs_purge_font(const gs_font *pfont)
{	gs_font_dir *pdir = pfont->dir;
	gs_font *pf;

	/* Remove the font from its list (orig_fonts or scaled_fonts). */
	gs_font *prev = pfont->prev;
	gs_font *next = pfont->next;
	if ( next != 0 )
		next->prev = prev;
	if ( prev != 0 )
		prev->next = next;
	else if ( pdir->orig_fonts == pfont )
		pdir->orig_fonts = next;
	else if ( pdir->scaled_fonts == pfont )
		pdir->scaled_fonts = next;
	else
		/* Shouldn't happen! */
		;
	if ( pfont->base != pfont )
	{	/* I.e., this is a scaled font. */
		pdir->ssize--;
	}

	/* Purge the font from the scaled font cache. */
	for ( pf = pdir->scaled_fonts; pf != 0; )
	{	if ( pf->base == pfont )
		{	gs_purge_font(pf);
			pf = pdir->scaled_fonts; /* start over */
		}
		else
			pf = pf->next;
	}

	/* Purge the font from the font/matrix pair cache, */
	/* including all cached characters rendered with that font. */
	gs_purge_font_from_char_caches(pdir, pfont);

}
