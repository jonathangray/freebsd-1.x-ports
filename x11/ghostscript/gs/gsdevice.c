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

/* gsdevice.c */
/* Device operators for Ghostscript library */
#include "math_.h"			/* for fabs */
#include "memory_.h"			/* for memcpy */
#include "gx.h"
#include "gserrors.h"
#include "gsprops.h"
#include "gsutil.h"
#include "gxarith.h"
#include "gzstate.h"
#include "gzdevice.h"
#include "gxdevmem.h"

/* Import the device list from gdevs.c */
extern gx_device *gx_device_list[];

/* Device definitions */
/* Following defines the null device */
private dev_proc_fill_rectangle(null_fill_rectangle);
private dev_proc_copy_mono(null_copy_mono);
private dev_proc_get_xfont_procs(null_get_xfont_procs);
private dev_proc_get_xfont_device(null_get_xfont_device);

/* The null device procedure record is also used to fill in */
/* NULL procedures in actual devices, so it must be complete. */
private gx_device_procs null_procs = {
	gx_default_open_device,
	gx_default_get_initial_matrix,
	gx_default_sync_output,
	gx_default_output_page,
	gx_default_close_device,
	gx_default_map_rgb_color,
	gx_default_map_color_rgb,
	null_fill_rectangle,
	gx_default_tile_rectangle,
	null_copy_mono,
	gx_default_copy_color,
	gx_default_draw_line,
	gx_default_get_bits,
	gx_default_get_props,
	gx_default_put_props,
	gx_default_map_cmyk_color,
	null_get_xfont_procs,
	null_get_xfont_device
};
gx_device_null gs_null_device = {
	sizeof(gx_device),
	&null_procs,
	"null",
	0, 0,
	72, 72,
	no_margins,
	dci_black_and_white,
	1,
	0				/* target */
};

/* Fill in a single procedure */
#define fill_default(procs, p, dproc)\
  if ( (procs)->p == 0 ) (procs)->p = dproc
#define fill_proc(procs, p)\
  fill_default(procs, p, null_procs.p)

/* Fill in NULL procedures in a device procedure record. */
void
gx_device_procs_complete(register gx_device_procs *procs)
{	fill_proc(procs, open_device);
	fill_proc(procs, get_initial_matrix);
	fill_proc(procs, sync_output);
	fill_proc(procs, output_page);
	fill_proc(procs, close_device);
	fill_proc(procs, map_rgb_color);
	fill_proc(procs, map_color_rgb);
	/* NOT fill_rectangle */
	fill_proc(procs, tile_rectangle);
	/* NOT copy_mono */
	fill_proc(procs, copy_color);		/* Bogus? */
	fill_proc(procs, draw_line);
	fill_proc(procs, get_bits);
	fill_proc(procs, get_props);
	fill_proc(procs, put_props);
	fill_proc(procs, map_cmyk_color);
	fill_default(procs, get_xfont_procs, gx_default_get_xfont_procs);
	fill_default(procs, get_xfont_device, gx_default_get_xfont_device);
}
void
gx_device_complete_procs(gx_device *dev)
{	gx_device_procs_complete(dev->procs);
}

/* Flush buffered output to the device */
int
gs_flushpage(gs_state *pgs)
{	gx_device *dev = pgs->device->info;
	return (*dev->procs->sync_output)(dev);
}

/* Make the device output the accumulated page description */
int
gs_copypage(gs_state *pgs)
{	return gs_output_page(pgs, 1, 0);
}
int
gs_output_page(gs_state *pgs, int num_copies, int flush)
{	gx_device *dev = pgs->device->info;
	return (*dev->procs->output_page)(dev, num_copies, flush);
}

/* Copy scan lines from an image device */
int
gs_copyscanlines(gx_device *dev, int start_y, byte *data, uint size,
  int *plines_copied, uint *pbytes_copied)
{	uint line_size = gx_device_raster(dev, 0);
	uint count = size / line_size;
	uint i;
	byte *dest = data;
	for ( i = 0; i < count; i++, dest += line_size )
	{	int code = (*dev->procs->get_bits)(dev, start_y + i, dest, NULL);
		if ( code < 0 )
		{	/* Might just be an overrun. */
			if ( start_y + i == dev->height ) break;
			return_error(code);
		}
	}
	if ( plines_copied != NULL )
	  *plines_copied = i;
	if ( pbytes_copied != NULL )
	  *pbytes_copied = i * line_size;
	return 0;
}

/* Get the current device from the graphics state */
gx_device *
gs_currentdevice(const gs_state *pgs)
{	return pgs->device->info;
}

/* Get the name of a device */
const char *
gs_devicename(const gx_device *dev)
{	return dev->dname;
}

/* Get the initial matrix of a device. */
void
gs_deviceinitialmatrix(gx_device *dev, gs_matrix *pmat)
{	fill_proc(dev->procs, get_initial_matrix);
	(*dev->procs->get_initial_matrix)(dev, pmat);
}

/* Get the N'th device from the known device list */
gx_device *
gs_getdevice(int index)
{	int i;
	for ( i = 0; gx_device_list[i] != 0; i++ )
	   {	if ( i == index ) return gx_device_list[i];
	   }
	return 0;			/* index out of range */
}

/* Clone an existing device. */
int
gs_copydevice(gx_device **pnew_dev, const gx_device *dev, const gs_memory_procs *mprocs)
{	register gx_device *new_dev;
	new_dev = (gx_device *)(*mprocs->alloc)(1, dev->params_size, "gs_copydevice");
	if ( new_dev == 0 ) return_error(gs_error_VMerror);
	memcpy(new_dev, dev, dev->params_size);
	new_dev->is_open = 0;
	*pnew_dev = new_dev;
	return 0;
}

/* Make a memory (image) device. */
/* If num_colors = -16, -24, or -32, this is a true-color device; */
/* otherwise, num_colors is the number of elements in the palette */
/* (2^N or 3*2^N). */
int
gs_makeimagedevice(gx_device **pnew_dev, const gs_matrix *pmat,
  uint width, uint height, const byte *colors, int num_colors,
  const gs_memory_procs *mprocs)
{	const gx_device_memory *old_dev;
	register gx_device_memory *new_dev;
	int palette_size = num_colors;
	int bpp = 1;
	int pcount;
	int bits_per_pixel;
	float x_pixels_per_unit, y_pixels_per_unit;
	byte palette[256 * 3];
	byte *dev_palette;
	int has_color;
	if ( width <= 0 || height <= 0 ) return_error(gs_error_rangecheck);
	switch ( num_colors )
	   {
	case 3*2:
		palette_size = 2; bpp = 3;
	case 2:
		bits_per_pixel = 1; break;
	case 3*4:
		palette_size = 4; bpp = 3;
	case 4:
		bits_per_pixel = 2; break;
	case 3*16:
		palette_size = 16; bpp = 3;
	case 16:
		bits_per_pixel = 4; break;
	case 3*256:
		palette_size = 256; bpp = 3;
	case 256:
		bits_per_pixel = 8; break;
	case -16:
		bits_per_pixel = 16; palette_size = 0; break;
	case -24:
		bits_per_pixel = 24; palette_size = 0; break;
	case -32:
		bits_per_pixel = 32; palette_size = 0; break;
	default:
		return_error(gs_error_rangecheck);
	   }
	old_dev = gdev_mem_device_for_bits(bits_per_pixel);
	if ( old_dev == 0 )		/* no suitable device */
		return_error(gs_error_rangecheck);
	pcount = palette_size * 3;
	/* Check to make sure the palette contains white and black, */
	/* and, if it has any colors, the six primaries. */
	if ( bits_per_pixel <= 8 )
	   {	const byte *p;
		byte *q;
		int primary_mask = 0;
		int i;
		has_color = 0;
		for ( i = 0, p = colors, q = palette;
		      i < palette_size; i++, q += 3
		    )
		   {	int mask = 1;
			switch ( bpp )
			   {
			case 1:			/* gray */
				q[0] = q[1] = q[2] = *p++;
				break;
			default:		/* bpp == 3, colored */
				q[0] = p[0], q[1] = p[1], q[2] = p[2];
				p += 3;
			   }
#define shift_mask(b,n)\
  switch ( b ) { case 0xff: mask <<= n; case 0: break; default: mask = 0; }
			shift_mask(q[0], 4);
			shift_mask(q[1], 2);
			shift_mask(q[2], 1);
#undef shift_mask
			primary_mask |= mask;
			if ( q[0] != q[1] || q[0] != q[2] )
				has_color = 1;
		   }
		switch ( primary_mask )
		   {
		case 129:		/* just black and white */
			if ( has_color )	/* color but no primaries */
				return_error(gs_error_rangecheck);
		case 255:		/* full color */
			break;
		default:
			return_error(gs_error_rangecheck);
		   }
	   }
	else
		has_color = 1;
	/*
	 * The initial transformation matrix must map 1 user unit to
	 * 1/72".  Let W and H be the width and height in pixels, and
	 * assume the initial matrix is of the form [A 0 0 B X Y].
	 * Then the size of the image in user units is (W/|A|,H/|B|),
	 * hence the size in inches is ((W/|A|)/72,(H/|B|)/72), so
	 * the number of pixels per inch is
	 * (W/((W/|A|)/72),H/((H/|B|)/72)), or (|A|*72,|B|*72).
	 * Similarly, if the initial matrix is [0 A B 0 X Y] for a 90
	 * or 270 degree rotation, the size of the image in user
	 * units is (W/|B|,H/|A|), so the pixels per inch are
	 * (|B|*72,|A|*72).  We forbid non-orthogonal transformation
	 * matrices.
	 */
	if ( is_fzero2(pmat->xy, pmat->yx) )
		x_pixels_per_unit = pmat->xx, y_pixels_per_unit = pmat->yy;
	else if ( is_fzero2(pmat->xx, pmat->yy) )
		x_pixels_per_unit = pmat->yx, y_pixels_per_unit = pmat->xy;
	else
		return_error(gs_error_undefinedresult);
	/* All checks done, allocate the device. */
	new_dev = (gx_device_memory *)(*mprocs->alloc)(1, old_dev->params_size, "gs_makeimagedevice(device)");
	if ( new_dev == 0 ) return_error(gs_error_VMerror);
	*new_dev = *old_dev;
	new_dev->initial_matrix = *pmat;
	new_dev->width = width;
	new_dev->height = height;
	new_dev->x_pixels_per_inch = fabs(x_pixels_per_unit) * 72;
	new_dev->y_pixels_per_inch = fabs(y_pixels_per_unit) * 72;
	if ( !has_color )
		new_dev->color_info.max_rgb = 0,
		new_dev->color_info.dither_rgb = 0;
	dev_palette = (byte *)(*mprocs->alloc)(pcount, 1, "gs_makeimagedevice(palette)");
	if ( dev_palette == 0 ) return_error(gs_error_VMerror);
	new_dev->invert = (palette[0] | palette[1] | palette[2] ? -1 : 0);	/* bogus */
	new_dev->palette_size = palette_size;
	new_dev->palette = dev_palette;
	memcpy(dev_palette, palette, pcount);
	/* The bitmap will be allocated when the device is opened. */
	new_dev->memory_procs = mprocs;
	new_dev->is_open = 0;
	*pnew_dev = (gx_device *)new_dev;
	return 0;
}

/* Set the device in the graphics state */
int
gs_setdevice(gs_state *pgs, gx_device *dev)
{	register device *pdev = pgs->device;
	int was_open = dev->is_open;
	int code;
	/* Initialize the device */
	if ( !was_open )
	{	gx_device_complete_procs(dev);
		if ( gs_device_is_memory(dev) )
		{	/* Set the target to the current device. */
			gx_device *odev = pdev->info;
			while ( odev != 0 && gs_device_is_memory(odev) )
				odev = ((gx_device_memory *)odev)->target;
			((gx_device_memory *)dev)->target = odev;
		}
		code = (*dev->procs->open_device)(dev);
		if ( code < 0 ) return_error(code);
		dev->is_open = 1;
	}
	/* Compute device white and black codes */
	pdev->black = (*dev->procs->map_cmyk_color)(dev, 0, 0, 0, gx_max_color_value);
	pdev->white = (*dev->procs->map_cmyk_color)(dev, 0, 0, 0, 0);
	pdev->info = dev;
	gx_set_cmap_procs(pgs);
	if (	(code = gs_initmatrix(pgs)) < 0 ||
		(code = gs_initclip(pgs)) < 0
	   )
		return code;
	if ( !was_open )
		if ( (code = gs_erasepage(pgs)) < 0 )
			return code;
	return gx_remap_color(pgs);
}

/* Select the null device.  This is just a convenience. */
void
gs_nulldevice(gs_state *pgs)
{	gs_setdevice(pgs, (gx_device *)&gs_null_device);
}

/* Close a device.  The client is responsible for ensuring that */
/* this device is not current in any graphics state. */
int
gs_closedevice(gx_device *dev)
{	int code = 0;
	if ( dev->is_open )
	   {	code = (*dev->procs->close_device)(dev);
		if ( code < 0 ) return_error(code);
		dev->is_open = 0;
	   }
	return code;
}

/* Install enough of a null device to suppress graphics output */
/* during the execution of stringwidth. */
void
gx_device_no_output(gs_state *pgs)
{	pgs->device->info = (gx_device *)&gs_null_device;
}

/* Just set the device without reinitializing. */
/* (For internal use only.) */
void
gx_set_device_only(gs_state *pgs, gx_device *dev)
{	pgs->device->info = dev;
}

/* Compute the size of one scan line for a device, */
/* with or without padding to a word boundary. */
uint
gx_device_raster(const gx_device *dev, int pad)
{	ulong bits = (ulong)dev->width * dev->color_info.depth;
	return (pad ?
		(uint)((bits + (align_bitmap_mod * 8 - 1))
			 >> (log2_align_bitmap_mod + 3))
			<< log2_align_bitmap_mod :
		(uint)((bits + 7) >> 3));
}

/* Adjust the resolution for devices that only have a fixed set of */
/* geometries, so that the apparent size in inches remains constant. */
/* If fit=1, the resolution is adjusted so that the entire image fits; */
/* if fit=0, one dimension fits, but the other one is clipped. */
int
gx_device_adjust_resolution(gx_device *dev,
  int actual_width, int actual_height, int fit)
{	double width_ratio = (double)actual_width / dev->width ;
	double height_ratio = (double)actual_height / dev->height ;
	double ratio =
		(fit ? min(width_ratio, height_ratio) :
		 max(width_ratio, height_ratio));
	dev->x_pixels_per_inch *= ratio;
	dev->y_pixels_per_inch *= ratio;
	dev->width = actual_width;
	dev->height = actual_height;
	return 0;
}

/* ------ The null device ------ */

private int
null_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	return 0;
}
private int
null_copy_mono(gx_device *dev, const byte *data,
  int dx, int raster, gx_bitmap_id id, int x, int y, int w, int h,
  gx_color_index zero, gx_color_index one)
{	return 0;
}
private gx_xfont_procs *
null_get_xfont_procs(gx_device *dev)
{	gx_device *target = ((gx_device_null *)dev)->target;
	return (target == 0 ? NULL :
		(*target->procs->get_xfont_procs)(target));
}
private gx_device *
null_get_xfont_device(gx_device *dev)
{	gx_device *target = ((gx_device_null *)dev)->target;
	return (target == 0 ? dev :
		(*target->procs->get_xfont_device)(target));
}

/* ------ Default device procedures ------ */

int
gx_default_open_device(gx_device *dev)
{	return 0;
}

void
gx_default_get_initial_matrix(register gx_device *dev, register gs_matrix *pmat)
{	pmat->xx = dev->x_pixels_per_inch / 72.0;
	pmat->xy = 0;
	pmat->yx = 0;
	pmat->yy = dev->y_pixels_per_inch / -72.0;
	pmat->tx = 0;
	pmat->ty = dev->height;	/****** WRONG for devices with ******/
				/****** arbitrary initial matrix ******/
}

int
gx_default_sync_output(gx_device *dev)
{	return 0;
}

int
gx_default_output_page(gx_device *dev, int num_copies, int flush)
{	return (*dev->procs->sync_output)(dev);
}

int
gx_default_close_device(gx_device *dev)
{	return 0;
}

int
gx_default_copy_color(gx_device *dev, const byte *data,
  int data_x, int raster, gx_bitmap_id id,
  int x, int y, int width, int height)
{	return (*dev->procs->copy_mono)(dev, data, data_x, raster, id,
		x, y, width, height, (gx_color_index)0, (gx_color_index)1);
}

int
gx_default_get_bits(gx_device *dev, int y, byte *data, byte **actual_data)
{	return -1;
}

gx_xfont_procs *
gx_default_get_xfont_procs(gx_device *dev)
{	return NULL;
}

gx_device *
gx_default_get_xfont_device(gx_device *dev)
{	return dev;
}

/* Standard device properties */

private const gs_prop_item props_std[] = {
		/* Following can be set, but will close and */
		/* reopen the device if necessary. */
	prop_def("HWResolution", prt_float_array),
	prop_def("HWSize", prt_int_array),
		/* Following cannot be set yet */
	prop_def("InitialMatrix", prt_float_array),
		/* Following cannot be set */
	prop_def("Name", prt_string),
		/* Slots for arrays */
	prop_float, prop_float,
	prop_int, prop_int,
	prop_float, prop_float, prop_float, prop_float,
	  prop_float, prop_float
};

/* Get the size of the device properties. */
int
gs_getdeviceprops_size(gx_device *dev)
{	fill_proc(dev->procs, get_props);
	return (*dev->procs->get_props)(dev, NULL);
}

/* Get the device properties. */
int
gs_getdeviceprops(gx_device *dev, gs_prop_item *plist)
{	fill_proc(dev->procs, get_props);
	return (*dev->procs->get_props)(dev, plist);
}

/* Get standard properties. */
int
gx_default_get_props(register gx_device *dev, register gs_prop_item *plist)
{	if ( plist != 0 )
	   {	register gs_prop_item *pi;
		gs_matrix mat;
		memcpy(plist, props_std, sizeof(props_std));
		plist[0].value.a.size = 2;
		plist[1].value.a.size = 2;
		plist[2].value.a.size = 6;
		plist[3].value.a.p.s = (char *)dev->dname;
		plist[3].value.a.size = -1;
		pi = &plist[4];
			/* resolution array */
		plist[0].value.a.p.v = pi;
		pi[0].value.f = dev->x_pixels_per_inch;
		pi[1].value.f = dev->y_pixels_per_inch;
		pi += 2;
			/* width/height array */
		plist[1].value.a.p.v = pi;
		pi[0].value.i = dev->width;
		pi[1].value.i = dev->height;
		pi += 2;
			/* matrix */
		plist[2].value.a.p.v = pi;

		fill_proc(dev->procs, get_initial_matrix);
		(*dev->procs->get_initial_matrix)(dev, &mat);
		pi[0].value.f = mat.xx;
		pi[1].value.f = mat.xy;
		pi[2].value.f = mat.yx;
		pi[3].value.f = mat.yy;
		pi[4].value.f = mat.tx;
		pi[5].value.f = mat.ty;
		pi += 6;
	   }
	return sizeof(props_std) / sizeof(gs_prop_item);
}

/* Set the device properties. */
/* If the device was open and the put_props procedure closed it, */
/* return 1; otherwise, return 0 or an error code as usual. */
int
gs_putdeviceprops(gx_device *dev, gs_prop_item *plist, int count)
{	int was_open = dev->is_open;
	int code;
	fill_proc(dev->procs, put_props);
	code = (*dev->procs->put_props)(dev, plist, count);
	return (code < 0 ? code : was_open && !dev->is_open ? 1 : code);
}

/* Set standard properties. */
/* Note that setting the size or resolution closes the device. */
int
gx_default_put_props(gx_device *dev, gs_prop_item *plist, int count)
{	gs_prop_item *known[2];
	int code = 0;
	gx_device temp_dev;
	props_extract(plist, count, props_std, 2, known, 1);
	temp_dev = *dev;
	if ( known[1] != 0 )
	   {	if ( known[1]->value.a.size != 2 )
			known[1]->status = pv_typecheck,
			code = gs_error_typecheck;
		else
		   {	gs_prop_item *ap = known[1]->value.a.p.v;
			if ( ap[0].value.i <= 0 || ap[1].value.i <= 0 )
				known[1]->status = pv_rangecheck,
				code = gs_error_rangecheck;
#define max_coord min(max_int, fixed2long(max_fixed))
			else if ( ap[0].value.i > max_coord ||
			     ap[1].value.i > max_coord
			   )
				known[1]->status = pv_limitcheck,
				code = gs_error_limitcheck;
#undef max_coord
			else
			   {	temp_dev.width = ap[0].value.i;
				temp_dev.height = ap[1].value.i;
			   }
			if ( code == 0 ) code = 1;
		   }
	   }
	if ( known[0] != 0 )
	   {	if ( known[0]->value.a.size != 2 )
			known[0]->status = pv_typecheck,
			code = gs_error_typecheck;
		else
		   {	gs_prop_item *ap = known[0]->value.a.p.v;
			if ( ap[0].value.f <= 0 || ap[1].value.f <= 0 )
				known[0]->status = pv_rangecheck,
				code = gs_error_rangecheck;
			else
			   {	temp_dev.x_pixels_per_inch = ap[0].value.f;
				temp_dev.y_pixels_per_inch = ap[1].value.f;
			   }
			if ( code == 0 ) code = 1;
		   }
	   }
	if ( code < 0 )
		return_error(code);
	/* Close the device; gs_putdeviceprops will reopen it. */
	if ( dev->is_open && code )
	{	int ccode = gs_closedevice(dev);
		if ( ccode < 0 ) return ccode;
	}
	dev->x_pixels_per_inch = temp_dev.x_pixels_per_inch;
	dev->y_pixels_per_inch = temp_dev.y_pixels_per_inch;
	dev->width = temp_dev.width;
	dev->height = temp_dev.height;
	return code;
}
