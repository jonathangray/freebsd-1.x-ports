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

/* gxccman.c */
/* Character cache management routines for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate.h */
#include "gzcolor.h"
#include "gzpath.h"
#include "gxcpath.h"
#include "gxdevmem.h"
#include "gxchar.h"
#include "gxcache.h"
#include "gxfont.h"
#include "gxfdir.h"
#include "gxxfont.h"

extern ulong gs_next_ids(P1(uint));

/* Export the size of the cache structures. */
const uint cached_char_sizeof = sizeof_cached_char;
const uint cached_fm_pair_sizeof = sizeof(cached_fm_pair);

/* Forward references */
void gs_purge_fm_pair(P3(gs_font_dir *, cached_fm_pair *, int));
private gx_xfont *lookup_xfont_by_name(P6(gx_device *, gx_xfont_procs *, gs_font_name *, int, const cached_fm_pair *, const gs_matrix *));
private cached_char *alloc_char_in_chunk(P4(gs_font_dir *, ulong, char_cache_chunk *, uint));
private void shorten_cached_char(P3(gs_font_dir *, cached_char *, uint));
private uint compress_scaled_bits(P4(byte *, uint, uint, int));

/* ====== Initialization ====== */

/* Initialize the character cache. */
void
gx_char_cache_init(register gs_font_dir *dir)
{	int i;
	cached_fm_pair *pair;
	dir->fmcache.msize = 0;
	dir->fmcache.mnext = 0;
	dir->ccache.bsize = 0;
	dir->ccache.csize = 0;
	dir->ccache.bspace = 0;
	dir->ccache.chunks = dir->ccache.initial_chunk.next =
		&dir->ccache.initial_chunk;
	dir->ccache.initial_chunk.size = 0;
	dir->ccache.cnext = 0;
	memset((char *)dir->ccache.chars, 0,
	       (dir->ccache.chars_mask + 1) * sizeof(cached_char *));
	for ( i = dir->fmcache.mmax, pair = dir->fmcache.mdata; --i >= 0; pair++ )
	  fm_pair_set_free(pair);
}

/* ====== Font-level routines ====== */

/* Add a font/matrix pair to the cache. */
/* (This is only exported for gxccache.c.) */
cached_fm_pair *
gx_add_fm_pair(register gs_font_dir *dir, gs_font *font, const gs_uid *puid,
  const gs_state *pgs)
{	register cached_fm_pair *pair =
		dir->fmcache.mdata + dir->fmcache.mnext;
	cached_fm_pair *mend =
		dir->fmcache.mdata + dir->fmcache.mmax;
	if ( dir->fmcache.msize == dir->fmcache.mmax ) /* cache is full */
	{	/* Prefer an entry with num_chars == 0, if any. */
		int count;
		for ( count = dir->fmcache.mmax;
		      --count >= 0 && pair->num_chars != 0;
		    )
			if ( ++pair == mend ) pair = dir->fmcache.mdata;
		gs_purge_fm_pair(dir, pair, 0);
	}
	else
	{	/* Look for an empty entry.  (We know there is one.) */
		while ( !fm_pair_is_free(pair) )
			if ( ++pair == mend ) pair = dir->fmcache.mdata;
	}
	dir->fmcache.msize++;
	dir->fmcache.mnext = pair + 1 - dir->fmcache.mdata;
	if ( dir->fmcache.mnext == dir->fmcache.mmax )
		dir->fmcache.mnext = 0;
	pair->font = font;
	pair->UID = *puid;
	pair->mxx = pgs->char_tm.xx, pair->mxy = pgs->char_tm.xy;
	pair->myx = pgs->char_tm.yx, pair->myy = pgs->char_tm.yy;
	pair->num_chars = 0;
	pair->xfont_tried = 0;
	pair->xfont = 0;
	if_debug6('k', "[k]adding pair 0x%lx: 0x%lx [%g %g %g %g]\n",
		  (ulong)pair, (ulong)font,
		  pair->mxx, pair->mxy, pair->myx, pair->myy);
	return pair;
}

/* Look up the xfont for a font/matrix pair. */
/* (This is only exported for gxccache.c.) */
void
gx_lookup_xfont(const gs_state *pgs, cached_fm_pair *pair, int encoding_index)
{	gx_device *dev = pgs->device->info;
	gx_device *fdev = (*dev->procs->get_xfont_device)(dev);
	gs_font *font = pair->font;
	gx_xfont_procs *procs = (*fdev->procs->get_xfont_procs)(fdev);
	gx_xfont *xf = 0;
	if ( procs != 0 )
	{	gs_matrix mat;
		mat.xx = pair->mxx, mat.xy = pair->mxy;
		mat.yx = pair->myx, mat.yy = pair->myy;
		/* xfonts can outlive their invocations, */
		/* but restore purges them properly. */
		pair->mprocs = pgs->memory_procs;
		if ( font->key_name.size != 0 )
			xf = lookup_xfont_by_name(fdev, procs,
				&font->key_name, encoding_index,
				pair, &mat);
#define font_name_eq(pfn1,pfn2)\
  ((pfn1)->size == (pfn2)->size &&\
   !memcmp((char *)(pfn1)->chars, (char *)(pfn2)->chars, (pfn1)->size))
		if ( xf == 0 && font->font_name.size != 0 &&
			     /* Avoid redundant lookup */
		     !font_name_eq(&font->font_name, &font->key_name)
		   )
			xf = lookup_xfont_by_name(fdev, procs,
				&font->font_name, encoding_index,
				pair, &mat);
		if ( xf == 0 & font->FontType != ft_composite &&
		     uid_is_valid(&font->data.base.UID)
		   )
		{	/* Look for an original font with the same UID. */
			gs_font_dir *pdir = font->dir;
			gs_font *pfont;
			for ( pfont = pdir->orig_fonts; pfont != 0;
			      pfont = pfont->next
			    )
			{	if ( pfont->FontType != ft_composite &&
				     uid_equal(&pfont->data.base.UID, &font->data.base.UID) &&
				     pfont->key_name.size != 0 &&
				     !font_name_eq(&font->key_name,
					           &pfont->key_name)
				   )
				{	xf = lookup_xfont_by_name(fdev, procs,
						&pfont->key_name,
						encoding_index, pair, &mat);
					if ( xf != 0 )
						break;
				}
			}
		}
	}
	pair->xfont = xf;
}

/* ------ Internal routines ------ */

/* Purge from the caches all references to a given font/matrix pair, */
/* or just character that depend on its xfont. */
void
gs_purge_fm_pair(gs_font_dir *dir, cached_fm_pair *pair, int xfont_only)
{	int chi;
	if_debug2('k', "[k]purging pair 0x%lx%s\n",
		  (ulong)pair, (xfont_only ? " (xfont only)" : ""));
	if ( pair->xfont != 0 )
	{	(*pair->xfont->common.procs->release)(pair->xfont,
			pair->mprocs);
		pair->xfont = 0;
		pair->xfont_tried = 0;
	}
	for ( chi = dir->ccache.chars_mask;
	      pair->num_chars != 0 && chi >= 0;
	    )
	{	cached_char **pcc = dir->ccache.chars + chi--;
		while ( *pcc != 0 )
		{	cached_char *cc = *pcc;
			if ( cc->head.pair == pair && (!xfont_only ||
			       (pair->xfont == 0 && !cc_has_bits(cc)))
			   )
			{	cached_char *ccnext = cc->next;
				gx_free_cached_char(dir, cc);
				*pcc = ccnext;
			}
			else
				pcc = &cc->next;
		}
	}
	if ( !xfont_only )
	{
#ifdef DEBUG
		if ( pair->num_chars != 0 )
		{	lprintf1("Error in gs_purge_fm_pair: num_chars =%d\n",
				 pair->num_chars);
		}
#endif
		fm_pair_set_free(pair);
		dir->fmcache.msize--;
	}
}

/* Look up an xfont by name. */
/* The caller must already have done get_xfont_device to get the proper */
/* device to pass as the first argument to lookup_font. */
private gx_xfont *
lookup_xfont_by_name(gx_device *fdev, gx_xfont_procs *procs,
  gs_font_name *pfstr, int encoding_index, const cached_fm_pair *pair,
  const gs_matrix *pmat)
{	gx_xfont *xf;
	if_debug5('k', "[k]lookup xfont %s [%g %g %g %g]\n",
		  pfstr->chars, pmat->xx, pmat->xy, pmat->yx, pmat->yy);
	xf = (*procs->lookup_font)(fdev,
		&pfstr->chars[0], pfstr->size,
		encoding_index, &pair->UID,
		pmat, pair->mprocs);
	if_debug1('k', "[k]... xfont=0x%lx\n", (ulong)xf);
	return xf;
}

/* ====== Character-level routines ====== */

/* Allocate storage for caching a rendered character. */
/* If dev != NULL set up the memory device; */
/* if dev == NULL, this is an xfont-only entry. */
/* Return the cached_char if OK, 0 if too big. */
cached_char *
gx_alloc_char_bits(gs_font_dir *dir, gx_device_memory *dev,
  ushort iwidth, ushort iheight)
{	ulong isize, icdsize;
	uint iraster;
	char_cache_chunk *cck;
	cached_char *cc;
	gx_device_memory mdev;
	gx_device_memory *pdev = dev;
	if ( dev == NULL )
	{	mdev = mem_mono_device;
		pdev = &mdev;
	}
	pdev->width = iwidth;
	pdev->height = iheight;
	iraster = gdev_mem_raster(pdev);
	if ( iraster != 0 && iheight > dir->ccache.upper / iraster )
		return 0;		/* too big */
	isize = gdev_mem_bitmap_size(pdev);
	icdsize = isize + cached_char_sizeof;
	/* Try allocating at the current position first. */
	cck = dir->ccache.chunks;
	cc = alloc_char_in_chunk(dir, icdsize, cck, dir->ccache.cnext);
	if ( cc == 0 )
	{	if ( dir->ccache.bspace < dir->ccache.bmax )
		{	/* Allocate another chunk. */
			char_cache_chunk *cck_prev = cck;
			uint cksize = dir->ccache.bmax / 5 + 1;
			uint tsize = dir->ccache.bmax - dir->ccache.bspace;
			byte *cdata;
			if ( cksize > tsize )
				cksize = tsize;
			if ( icdsize + sizeof(cached_char_head) > cksize )
				return 0;		/* wouldn't fit */
			cck = (char_cache_chunk *)gs_malloc(1, sizeof(*cck),
							"char cache chunk");
			if ( cck == 0 )
				return 0;
			cdata = (byte *)gs_malloc(cksize, 1,
						  "char cache chunk");
			if ( cdata == 0 )
			{	gs_free((char *)cck, 1, sizeof(*cck),
					"char cache chunk");
				return 0;
			}
			cck->data = cdata;
			cck->size = cksize;
			cck->next = cck_prev->next;
			cck_prev->next = cck;
			dir->ccache.bspace += cksize;
			((cached_char_head *)cdata)->size = cksize;
			((cached_char_head *)cdata)->pair = 0;
			cc = alloc_char_in_chunk(dir, icdsize, cck, 0);
		}
		else
		{	/* Cycle through chunks. */
			char_cache_chunk *cck_init = cck;
			while ( (cck = cck->next) != cck_init )
			{	cc = alloc_char_in_chunk(dir, icdsize, cck, 0);
				if ( cc != 0 ) break;
			}
			if ( cc == 0 )
				cc = alloc_char_in_chunk(dir, icdsize, cck, 0);
		}
		if ( cc == 0 )
			return 0;
		dir->ccache.chunks = cck;	/* update roving pointer */
	}
	if_debug4('k', "[k]adding 0x%lx:%u(%u,%u)\n",
		  (ulong)cc, (uint)icdsize, iwidth, iheight);
	cc->xglyph = gx_no_xglyph;
	cc->width = iwidth;
	cc->height = iheight;
	cc->raster = iraster;
	cc->head.pair = 0;	/* not linked in yet */
	cc->id = gx_no_bitmap_id;
	if ( dev != NULL )
		gx_open_cache_device(dev, cc);
	return cc;
}

/* Open the cache device. */
void
gx_open_cache_device(gx_device_memory *dev, cached_char *cc)
{	byte *bits = cc_bits(cc);
	dev->width = cc->width;
	dev->height = cc->height;
	memset((char *)bits, 0, (uint)gdev_mem_bitmap_size(dev));
	dev->base = bits;
	(*dev->procs->open_device)((gx_device *)dev);	/* initialize */
}

/* Remove a character from the cache. */
void
gx_free_cached_char(gs_font_dir *dir, cached_char *cc)
{	char_cache_chunk *cck = cc->chunk;
	dir->ccache.chunks = cck;
	dir->ccache.cnext = (byte *)cc - cck->data;
	dir->ccache.csize--;
	dir->ccache.bsize -= cc->head.size;
	if ( cc->head.pair != 0 )
	   {	/* might be allocated but not added to table yet */
		cc->head.pair->num_chars--;
	   }
	if_debug2('k', "[k]freeing 0x%lx, pair=0x%lx\n",
		  (ulong)cc, (ulong)cc->head.pair);
	cc_set_free(cc);
}

/* Add a character to the cache */
void
gx_add_cached_char(gs_font_dir *dir, gx_device_memory *dev,
  cached_char *cc, cached_fm_pair *pair, int scale)
{	if_debug3('k', "[k]chaining 0x%lx: glyph=0x%lx, wmode=%d\n",
		  (ulong)cc, (ulong)cc->code, cc->wmode);
	if ( dev != NULL )
		gx_add_char_bits(dir, dev, cc, scale);
	/* Add the new character at the tail of its chain. */
	{	register cached_char **head =
		  chars_head(dir, cc->code, pair);
		while ( *head != 0 ) head = &(*head)->next;
		*head = cc;
		cc->next = 0;
		cc->head.pair = pair;
		pair->num_chars++;
	}
}

/* Adjust the bits of a newly-rendered character, by unscaling */
/* and/or compressing. */
void
gx_add_char_bits(gs_font_dir *dir, gx_device_memory *dev,
  cached_char *cc, int scale)
{	uint raster, bsize;
	byte *bits = cc_bits(cc);
	/* Make sure the bits are in the right order */
	/* to use as a source. */
	gdev_mem_ensure_byte_order(dev);
	/* If the character was oversampled, compress it now. */
	if ( scale != 1 )
	{	cc->raster = compress_scaled_bits(bits, cc->width,
						  cc->height, scale);
		cc->width /= scale;
		cc->height /= scale;
	}
	raster = cc->raster;
	bsize = raster * cc->height;
	/* Compress the character in place. */
	/* For now, just discard leading and trailing blank rows. */
	if ( raster != 0 )
	{	/* Discard trailing blank rows. */
		register byte *p = bits + bsize;
		register uint n = bsize;
		while ( n && !p[-1] ) --n, --p;
		bsize = (n + raster - 1) / raster * raster;
		cc->height = bsize / raster;
	}
	if ( bsize )
	{	/* Discard leading blank rows. */
		int offset;
		register byte *p = bits;
		while ( !*p ) ++p;
		offset = (p - bits) / raster;
		if ( offset )
		{	uint diff = offset * raster;
			bsize -= diff;
			memcpy((char *)bits, (char *)bits + diff, bsize);
			cc->offset.y -= int2fixed(offset);
			cc->height = bsize / raster;
		}
	}
	/* Discard the memory device overhead that follows the bits, */
	/* and any space reclaimed from unscaling or blank rows. */
	{	uint diff = round_down(gdev_mem_bitmap_size(dev) - bsize,
					align_cached_char_mod);
		if ( diff >= sizeof(cached_char_head) )
		{	shorten_cached_char(dir, cc, diff);
			dir->ccache.bsize -= diff;
			if_debug2('K', "[K]shortening 0x%lx by %u (mdev overhead)\n",
				  (ulong)cc, diff);
		}
	}
	/* Assign a bitmap id. */
	cc->id = gs_next_ids(1);
}

/* Purge from the caches all references to a given font. */
void
gs_purge_font_from_char_caches(gs_font_dir *dir, const gs_font *font)
{	cached_fm_pair *pair = dir->fmcache.mdata;
	int count = dir->fmcache.mmax;
	if_debug1('k', "[k]purging font 0x%lx\n",
		  (ulong)font);
	while ( count-- )
	{	if ( pair->font == font )
		{	if ( uid_is_valid(&pair->UID) )
			{	/* Keep the entry. */
				pair->font = 0;
			}
			else
				gs_purge_fm_pair(dir, pair, 0);
		}
		pair++;
	}
}

/* ------ Internal routines ------ */

/* Allocate a character in a given chunk, which the caller will make */
/* (or ensure) current. */
private cached_char *
alloc_char_in_chunk(gs_font_dir *dir, ulong icdsize,
  char_cache_chunk *cck, uint cnext)
{	uint cdsize;
	cached_char_head *cch;
#define hcc ((cached_char *)cch)
	cached_char *cc;
	uint fsize = 0;
	if ( icdsize + sizeof(cached_char_head) > cck->size - cnext )
	{	/* Not enough room to allocate here. */
		return 0;
	}
	cdsize = (uint)icdsize;
	/* Look for and/or free enough space. */
	cch = (cached_char_head *)(cck->data + cnext);
	cc = hcc;
	while ( !(fsize == cdsize ||
		  fsize >= cdsize + sizeof(cached_char_head))
	      )
	{	if ( !cc_head_is_free(cch) )
		{	/* Free the character */
			cached_char **pcc =
				chars_head(dir, hcc->code, cch->pair);
			while ( *pcc != hcc )
				pcc = &(*pcc)->next;
			*pcc = hcc->next; /* remove from chain */
			gx_free_cached_char(dir, hcc);
		}
		fsize += cch->size;
		if_debug2('K', "[K]merging free 0x%lx(%u)\n",
			  (ulong)cch, cch->size);
		cc->head.size = fsize;
		cch = (cached_char_head *)((byte *)cc + fsize);
	}
#undef hcc
	cc->chunk = cck;
	if ( fsize > cdsize )
	  { shorten_cached_char(dir, cc, fsize - cdsize);
	    if_debug2('K', "[K]shortening 0x%lx by %u (initial)\n",
		      (ulong)cc, fsize - cdsize);
	  }
	dir->ccache.csize++;
	dir->ccache.bsize += cdsize;
	dir->ccache.cnext = (byte *)cc + cdsize - cck->data;
	return cc;
}

/* Shorten a cached character. */
/* diff >= sizeof(cached_char_head). */
private void
shorten_cached_char(gs_font_dir *dir, cached_char *cc, uint diff)
{	char_cache_chunk *cck = cc->chunk;
	cached_char_head *next;
	if ( (byte *)cc + cc->head.size == cck->data + dir->ccache.cnext &&
	     cck == dir->ccache.chunks
	   )
		dir->ccache.cnext -= diff;
	cc->head.size -= diff;
	next = (cached_char_head *)((byte *)cc + cc->head.size);
	if_debug2('K', "[K]shortening creates free block 0x%lx(%u)\n",
		  (ulong)next, diff);
	cc_head_set_free(next);
	next->size = diff;
}

/* ------ Oversampling/scaling ------ */

/* Count the number of 1-bits in a half-byte. */
static const byte half_byte_1s[16] = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4};

/* Compress a 4x4- or 2x2-oversampled bitmap to 1x1 by counting 1-bits. */
/* Width and height reflect the oversampling; both are multiples of scale. */
/* Return the raster of the compressed bitmap. */
private uint
compress_scaled_bits(byte *data, uint width, uint height, int scale)
{	uint threshold = scale * scale / 2;
	uint sraster = ((width + 31) >> 5) << 2;
	uint sskip = sraster * scale;
	uint dwidth = width / scale;
	uint draster = ((dwidth + 31) >> 5) << 2;
	uint dskip = -((dwidth + 7) >> 3) & 3;
	uint mask = (1 << scale) - 1;
	byte *srow = data;
	byte *d = data;
	uint h;
	for ( h = height; h; h -= scale )
	{	byte *s = srow;
		byte out_bit = 0x80;
		byte out = 0;
		int in_shift = 8 - scale;
		uint w;
		for ( w = width; w; w -= scale )
		{	uint count = 0;
			uint index;
			for ( index = 0; index != sskip; index += sraster )
				count += half_byte_1s[(s[index] >> in_shift) & mask];
			if ( count >= threshold )
				out += out_bit;
			if ( (in_shift -= scale) < 0 )
				s++, in_shift += 8;
			if ( !(out_bit >>= 1) )
				*d++ = out, out_bit = 0x80, out = 0;
		}
		if ( out_bit != 0x80 )
			*d++ = out;
		switch ( dskip )
		{
		case 3: *d++ = 0;
		case 2: *d++ = 0;
		case 1: *d++ = 0;
		}
		srow += sskip;
	}
	return draster;
}
