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

/* gdevmem1.c */
/* Generic and monobit "memory" (stored bitmap) device */
/* for Ghostscript library. */
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gxdevmem.h"			/* semi-public definitions */
#include "gdevmem.h"			/* private definitions */

/* Define masks for little-endian operation. */
const ushort gdev_mem_swapped_left_masks[17] = {
	0xffff, 0xff7f, 0xff3f, 0xff1f, 0xff0f, 0xff07, 0xff03, 0xff01,
	0xff00, 0x7f00, 0x3f00, 0x1f00, 0x0f00, 0x0700, 0x0300, 0x0100,
	0x0000
};

/* ------ Generic code ------ */

/* Return the appropriate memory device for a given */
/* number of bits per pixel (0 if none suitable). */
const gx_device_memory *
gdev_mem_device_for_bits(int bits_per_pixel)
{	switch ( bits_per_pixel )
	   {
	case 1: return &mem_mono_device;
	case 2: return &mem_mapped2_color_device;
	case 4: return &mem_mapped4_color_device;
	case 8: return &mem_mapped8_color_device;
	case 16: return &mem_true16_color_device;
	case 24: return &mem_true24_color_device;
	case 32: return &mem_true32_color_device;
	default: return 0;
	   }
}

/* Compute the size of the bitmap storage, */
/* including the space for the scan line pointer table. */
/* Note that scan lines are padded to a multiple of 4 bytes, */
/* and additional padding may be needed if the pointer table */
/* must be aligned 0 mod 8. */
private ulong
mem_bitmap_bits_size(const gx_device_memory *dev)
{	return round_up((ulong)dev->height * gdev_mem_raster(dev),
			max(4, arch_align_long_mod));
}
ulong
gdev_mem_bitmap_size(const gx_device_memory *dev)
{        return mem_bitmap_bits_size(dev) +
		(ulong)dev->height * sizeof(byte *);
}

/* Open a memory device, allocating the data area if appropriate, */
/* and create the scan line table. */
int
mem_open(gx_device *dev)
{	byte *scan_line;
	uint raster = mdev->raster = gdev_mem_raster(mdev);
	byte **pptr;
	byte **pend;
	if ( mdev->memory_procs != 0 )
	{	/* Allocate the data now. */
		ulong size = gdev_mem_bitmap_size(mdev);
		if ( (uint)size != size )
			return gs_error_limitcheck;
		mdev->base = (byte *)(*mdev->memory_procs->alloc)(1, (uint)size, "mem_open");
		if ( mdev->base == 0 )
			return gs_error_VMerror;
	}
	scan_line = mdev->base;
        pptr = (byte **)byte_ptr_add(scan_line, mem_bitmap_bits_size(mdev));
	pend = pptr + dev->height;
	mdev->line_ptrs = pptr;
	while ( pptr < pend )
	   {	*pptr++ = scan_line;
		scan_line = byte_ptr_add(scan_line, raster);
	   }
	return 0;
}

/* Return the initial transformation matrix */
void
mem_get_initial_matrix(gx_device *dev, gs_matrix *pmat)
{	pmat->xx = mdev->initial_matrix.xx;
	pmat->xy = mdev->initial_matrix.xy;
	pmat->yx = mdev->initial_matrix.yx;
	pmat->yy = mdev->initial_matrix.yy;
	pmat->tx = mdev->initial_matrix.tx;
	pmat->ty = mdev->initial_matrix.ty;
}

/* Test whether a device is a memory device */
int
gs_device_is_memory(const gx_device *dev)
{	/* We can't just compare the procs, or even an individual proc, */
	/* because we might be tracing.  Compare the device name, */
	/* and hope for the best. */
	const char *name = dev->dname;
	int i;
	for ( i = 0; i < 6; i++ )
	  if ( name[i] != "image("[i] ) return 0;
	return 1;
}

/* Ensure that the data bytes are in big-endian order. */
/* This is no longer needed. */
void
gdev_mem_ensure_byte_order(gx_device_memory *dev)
{
}

/* Close a memory device, freeing the data area if appropriate. */
int
mem_close(gx_device *dev)
{	if ( mdev->memory_procs != 0 )
	  (*mdev->memory_procs->free)((char *)mdev->base,
		1, (uint)gdev_mem_bitmap_size(mdev), "mem_close");
	return 0;
}

/* Copy a scan line to a client. */
#undef chunk
#define chunk byte
int
mem_get_bits(gx_device *dev, int y, byte *str, byte **actual_data)
{	byte *src;
	if ( y < 0 || y >= dev->height )
		return gs_error_rangecheck;
	src = scan_line_base(mdev, y);
	if ( actual_data == 0 )
		memcpy(str, src, gx_device_raster(dev, 0));
	else
		*actual_data = src;
	return 0;
}

/* Return the xfont procedure vector. */
gx_xfont_procs *
mem_get_xfont_procs(gx_device *dev)
{	gx_device *target = mdev->target;
	return (target == 0 ? gx_default_get_xfont_procs(dev) :
		(*target->procs->get_xfont_procs)(target));
}

/* Return the xfont device. */
gx_device *
mem_get_xfont_device(gx_device *dev)
{	gx_device *target = mdev->target;
	return (target == 0 ? gx_default_get_xfont_device(dev) :
		(*target->procs->get_xfont_device)(target));
}

/* ------ Monochrome ------ */

/* Procedures */
private dev_proc_copy_mono(mem_mono_copy_mono);
private dev_proc_fill_rectangle(mem_mono_fill_rectangle);

/* The device descriptor. */
private gx_device_procs mem_mono_procs =
  mem_procs(gx_default_map_rgb_color, gx_default_map_color_rgb,
    mem_mono_copy_mono, gx_default_copy_color, mem_mono_fill_rectangle);

/* The instance is public. */
const gx_device_memory mem_mono_device =
  mem_device("image(mono)", 1, mem_mono_procs);

/* Convert x coordinate to byte offset in scan line. */
#define x_to_byte(x) ((x) >> 3)

/* Fill a rectangle with a color. */
#undef chunk
#define chunk mono_chunk
private int
mem_mono_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	uint bit;
	chunk right_mask;
	byte fill;
	declare_scan_ptr(dest);
	fit_fill(dev, x, y, w, h);
	setup_rect(dest);
#define write_loop(stat)\
 { int line_count = h;\
   chunk *ptr = dest;\
   do { stat; inc_chunk_ptr(ptr, draster); }\
   while ( --line_count );\
 }
#define write_partial(msk)\
   if ( fill ) write_loop(*ptr |= msk)\
   else write_loop(*ptr &= ~msk)
	switch ( color )
	   {
	case 0: fill = mdev->invert; break;
	case 1: fill = ~mdev->invert; break;
	case gx_no_color_index: return 0;		/* transparent */
	default: return -1;		/* invalid */
	   }
	bit = x & chunk_align_bit_mask;
	if ( bit + w <= chunk_bits )
	   {	/*
		 * Only one word. We have to split following statement
		 * because of a bug in the Xenix C compiler (it produces
		 * a signed rather than an unsigned shift if we don't
		 * split).
		 */
		set_mono_thin_mask(right_mask, w, bit);
	   }
	else
	   {	int byte_count;
		if ( bit )
		   {	chunk mask;
			set_mono_left_mask(mask, bit);
			write_partial(mask);
			dest++;
			w += bit - chunk_bits;
		   }
		set_mono_right_mask(right_mask, w & chunk_bit_mask);
		if ( (byte_count = (w >> 3) & -chunk_bytes) != 0 )
		   {	write_loop(memset(ptr, fill, byte_count));
			inc_chunk_ptr(dest, byte_count);
		   }
	   }
	if ( right_mask )
		write_partial(right_mask);
	return 0;
}

/* Copy a monochrome bitmap. */

/* Fetch a chunk from the source. */
/* The source data are always stored big-endian. */
/* Note that the macros always cast cptr, */
/* so it doesn't matter what the type of cptr is. */
/* cshift = chunk_bits - shift. */
#undef chunk
#if arch_is_big_endian
#  define chunk uint
#  define cfetch_right(cptr, shift, cshift)\
	(cfetch_aligned(cptr) >> shift)
#  define cfetch_left(cptr, shift, cshift)\
	(cfetch_aligned(cptr) << shift)
/* Fetch a chunk that straddles a chunk boundary. */
#  define cfetch2(cptr, cskew, skew)\
    (cfetch_left(cptr, cskew, skew) +\
     cfetch_right((chunk *)(cptr) + 1, skew, cskew))
#else				/* little-endian */
#  define chunk ushort
private const ushort right_masks2[9] = {
	0xffff, 0x7f7f, 0x3f3f, 0x1f1f, 0x0f0f, 0x0707, 0x0303, 0x0101, 0x0000
};
private const ushort left_masks2[9] = {
	0xffff, 0xfefe, 0xfcfc, 0xf8f8, 0xf0f0, 0xe0e0, 0xc0c0, 0x8080, 0x0000
};
#  define ccont(cptr, off) (((chunk *)(cptr))[off])
#  define cfetch_right(cptr, shift, cshift)\
	((shift) < 8 ?\
	 ((ccont(cptr, 0) >> (shift)) & right_masks2[shift]) +\
	  (ccont(cptr, 0) << (cshift)) :\
	 ((chunk)*(byte *)(cptr) << (cshift)) & 0xff00)
#  define cfetch_left(cptr, shift, cshift)\
	((shift) < 8 ?\
	 ((ccont(cptr, 0) << (shift)) & left_masks2[shift]) +\
	  (ccont(cptr, 0) >> (cshift)) :\
	 ((ccont(cptr, 0) & 0xff00) >> (cshift)) & 0xff)
/* Fetch a chunk that straddles a chunk boundary. */
/* We can avoid testing the shift amount twice */
/* by expanding the cfetch_left/right macros in-line. */
#  define cfetch2(cptr, cskew, skew)\
	((cskew) < 8 ?\
	 ((ccont(cptr, 0) << (cskew)) & left_masks2[cskew]) +\
	  (ccont(cptr, 0) >> (skew)) +\
	  (((chunk)(((byte *)(cptr))[2]) << (cskew)) & 0xff00) :\
	 (((ccont(cptr, 0) & 0xff00) >> (skew)) & 0xff) +\
	  ((ccont(cptr, 1) >> (skew)) & right_masks2[skew]) +\
	   (ccont(cptr, 1) << (cskew)))
#endif
/* Since source and destination are both always big-endian, */
/* fetching an aligned chunk never requires byte swapping. */
#  define cfetch_aligned(cptr)\
	(*(chunk *)(cptr))

/* copy_function and copy_shift get added together for dispatch */
typedef enum {
	copy_or = 0, copy_store, copy_and, copy_funny
} copy_function;
/* copy_right/left is not an enum, because compilers complain about */
/* an enumeration clash when these are added to a copy_function. */
#define copy_right ((copy_function)0)
#define copy_left ((copy_function)4)
typedef struct {
	short invert;
	ushort op;			/* copy_function */
} copy_mode;
/* Map from <c0,c1,invert> to copy_mode. */
#define cm(i,op) { i, (ushort)op }
private copy_mode copy_modes[9*2] = {
	cm(-1, copy_funny),		/* NN */
	cm(-1, copy_and),		/* N0 */
	cm(0, copy_or),			/* N1 */
	cm(0, copy_and),		/* 0N */
	cm(0, copy_funny),		/* 00 */
	cm(0, copy_store),		/* 01 */
	cm(-1, copy_or),		/* 1N */
	cm(-1, copy_store),		/* 10 */
	cm(0, copy_funny),		/* 11 */
	cm(-1, copy_funny),		/* NNi */
	cm(0, copy_or),			/* N1i */
	cm(-1, copy_and),		/* N0i */
	cm(-1, copy_or),		/* 1Ni */
	cm(0, copy_funny),		/* 11i */
	cm(-1, copy_store),		/* 10i */
	cm(0, copy_and),		/* 0Ni */
	cm(0, copy_store),		/* 01i */
	cm(0, copy_funny)		/* 00i */
};
private int
mem_mono_copy_mono(gx_device *dev,
  const byte *base, int sourcex, int sraster, gx_bitmap_id id,
  int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{	register const byte *bptr;		/* actually chunk * */
	int dbit, wleft;
	uint mask;
	copy_mode mode;
#define function (copy_function)(mode.op)
	declare_scan_ptr_as(dbptr, byte *);
#define optr ((chunk *)dbptr)
	register int skew;
	register uint invert;
	fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
#if gx_no_color_index_value != -1		/* hokey! */
	if ( zero == gx_no_color_index ) zero = -1;
	if ( one == gx_no_color_index ) one = -1;
#endif
#define izero (int)zero
#define ione (int)one
	mode =
	  copy_modes[(mdev->invert & 9) + izero + izero + izero + ione + 4];
#undef izero
#undef ione
	invert = (uint)(int)mode.invert;	/* load register */
	setup_rect_as(dbptr, byte *);
	bptr = base + ((sourcex & ~chunk_align_bit_mask) >> 3);
	dbit = x & chunk_align_bit_mask;
	skew = dbit - (sourcex & chunk_align_bit_mask);
/* Macros for writing partial chunks. */
/* The destination pointer is always named optr, */
/* and must be declared as chunk *. */
/* cinvert may be temporarily redefined. */
#define cinvert(bits) ((bits) ^ invert)
#define write_or_masked(bits, mask, off)\
  optr[off] |= (cinvert(bits) & mask)
#define write_store_masked(bits, mask, off)\
  optr[off] = ((optr[off] & ~mask) | (cinvert(bits) & mask))
#define write_and_masked(bits, mask, off)\
  optr[off] &= (cinvert(bits) | ~mask)
/* Macros for writing full chunks. */
#define write_or(bits)  *optr |= cinvert(bits)
#define write_store(bits) *optr = cinvert(bits)
#define write_and(bits) *optr &= cinvert(bits)
/* Macro for incrementing to next chunk. */
#define next_x_chunk\
  bptr += chunk_bytes; dbptr += chunk_bytes
/* Common macro for the end of each scan line. */
#define end_y_loop(sdelta, ddelta)\
  if ( --h == 0 ) break;\
  bptr += sdelta; dbptr += ddelta
	if ( (wleft = w + dbit - chunk_bits) <= 0 )
	   {	/* The entire operation fits in one (destination) chunk. */
		set_mono_thin_mask(mask, w, dbit);
#define write_single(wr_op, src)\
  for ( ; ; )\
   { wr_op(src, mask, 0);\
     end_y_loop(sraster, draster);\
   }
#define write1_loop(src)\
  switch ( function ) {\
    case copy_or: write_single(write_or_masked, src); break;\
    case copy_store: write_single(write_store_masked, src); break;\
    case copy_and: write_single(write_and_masked, src); break;\
    default: goto funny;\
  }
		if ( skew >= 0 )	/* single -> single, right/no shift */
		   {	int cskew = chunk_bits - skew;
			write1_loop(cfetch_right(bptr, skew, cskew));
		   }
		else if ( wleft <= skew )	/* single -> single, left shift */
		   {	int cskew = chunk_bits + skew;
			skew = -skew;
			write1_loop(cfetch_left(bptr, skew, cskew));
		   }
		else			/* double -> single */
		   {	int cskew = -skew;
			skew += chunk_bits;
			write1_loop(cfetch2(bptr, cskew, skew));
		   }
#undef write1_loop
#undef write_single
	   }
	else if ( wleft <= skew )
	   {	/* 1 source chunk -> 2 destination chunks. */
		/* This is an important special case for */
		/* both characters and halftone tiles. */
		register uint bits;
		uint rmask;
		int cskew = chunk_bits - skew;
		set_mono_left_mask(mask, dbit);
		set_mono_right_mask(rmask, wleft);
#undef cinvert
#define cinvert(bits) (bits)		/* pre-inverted here */
#if arch_is_big_endian			/* no byte swapping */
#  define write_1to2(wr_op)\
  for ( ; ; )\
   { bits = cfetch_aligned(bptr) ^ invert;\
     wr_op(bits >> skew, mask, 0);\
     wr_op(bits << cskew, rmask, 1);\
     end_y_loop(sraster, draster);\
   }
#else					/* byte swapping */
#  define write_1to2(wr_op)\
  for ( ; ; )\
   { wr_op(cfetch_right(bptr, skew, cskew) ^ invert, mask, 0);\
     wr_op(cfetch_left(bptr, cskew, skew) ^ invert, rmask, 1);\
     end_y_loop(sraster, draster);\
   }
#endif
		switch ( function )
		   {
		case copy_or: write_1to2(write_or_masked); break;
		case copy_store: write_1to2(write_store_masked); break;
		case copy_and: write_1to2(write_and_masked); break;
		default: goto funny;
		   }
#undef cinvert
#define cinvert(bits) ((bits) ^ invert)
#undef write_1to2
	   }
	else
	   {	/* More than one source chunk and more than one */
		/* destination chunk are involved. */
		uint rmask;
		int words = (wleft & ~chunk_bit_mask) >> 3;
		uint sskip = sraster - words;
		uint dskip = draster - words;
		register uint bits;
		set_mono_left_mask(mask, dbit);
		set_mono_right_mask(rmask, wleft & chunk_bit_mask);
		if ( skew == 0 )	/* optimize the aligned case */
		   {
#define write_aligned(wr_op, wr_op_masked)\
  for ( ; ; )\
   { int count = wleft;\
     /* Do first partial chunk. */\
     wr_op_masked(cfetch_aligned(bptr), mask, 0);\
     /* Do full chunks. */\
     while ( (count -= chunk_bits) >= 0 )\
      { next_x_chunk; wr_op(cfetch_aligned(bptr)); }\
     /* Do last chunk */\
     if ( count > -chunk_bits )\
      { wr_op_masked(cfetch_aligned(bptr + chunk_bytes), rmask, 1); }\
     end_y_loop(sskip, dskip);\
   }
			switch ( function )
			  {
			  case copy_or:
			    write_aligned(write_or, write_or_masked);
			    break;
			  case copy_store:
			    write_aligned(write_store, write_store_masked);
			    break;
			  case copy_and:
			    write_aligned(write_and, write_and_masked);
			    break;
			  default:
			    goto funny;
			  }
#undef write_aligned
		   }
		else			/* not aligned */
		   {	int ccase =
			  (skew >= 0 ? copy_right :
			   ((bptr += chunk_bytes), copy_left))
			  + (int)function;
			int cskew = -skew & chunk_bit_mask;
			skew &= chunk_bit_mask;
			for ( ; ; )
			   {	int count = wleft;
#define prefetch_right\
  bits = cfetch_right(bptr, skew, cskew)
#define prefetch_left\
  bits = cfetch2(bptr - chunk_bytes, cskew, skew)
#define write_unaligned(wr_op, wr_op_masked)\
  wr_op_masked(bits, mask, 0);\
  /* Do full chunks. */\
  while ( count >= chunk_bits )\
    { bits = cfetch2(bptr, cskew, skew);\
      next_x_chunk; wr_op(bits); count -= chunk_bits;\
    }\
  /* Do last chunk */\
  if ( count > 0 )\
    { bits = cfetch_left(bptr, cskew, skew);\
      if ( count > skew ) bits += cfetch_right(bptr + chunk_bytes, skew, cskew);\
      wr_op_masked(bits, rmask, 1);\
    }
				switch ( ccase )
				  {
				  case copy_or + copy_left:
				    prefetch_left; goto uor;
				  case copy_or + copy_right:
				    prefetch_right;
uor:				    write_unaligned(write_or, write_or_masked);
				    break;
				  case copy_store + copy_left:
				    prefetch_left; goto ustore;
				  case copy_store + copy_right:
				    prefetch_right;
ustore:				    write_unaligned(write_store, write_store_masked);
				    break;
				  case copy_and + copy_left:
				    prefetch_left; goto uand;
				  case copy_and + copy_right:
				    prefetch_right;
uand:				    write_unaligned(write_and, write_and_masked);
				    break;
				  default:
				    goto funny;
				  }
				end_y_loop(sskip, dskip);
#undef write_unaligned
#undef prefetch_left
#undef prefetch_right
			   }
		   }
	   }
#undef end_y_loop
#undef next_x_chunk
	return 0;
	/* Handle the funny cases that aren't supposed to happen. */
funny:	return (invert ? -1 : mem_mono_fill_rectangle(dev, x, y, w, h, zero));
#undef optr
}
