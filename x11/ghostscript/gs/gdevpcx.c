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

/* gdevpcx.c */
/* PCX file format devices for Ghostscript */
#include "gdevprn.h"
#include "gdevpccm.h"
#include "gxlum.h"

/* Thanks to Phil Conrad for donating the original version */
/* of these drivers to Aladdin Enterprises. */

/* ------ The device descriptors ------ */

/*
 * Default X and Y resolution.
 */
#define X_DPI 72
#define Y_DPI 72

/* Monochrome. */

private dev_proc_print_page(pcxmono_print_page);

gx_device_printer far_data gs_pcxmono_device =
  prn_device(prn_std_procs, "pcxmono",
	DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	1, pcxmono_print_page);

/* Chunky 8-bit gray scale. */

private dev_proc_print_page(pcx256_print_page);

private dev_proc_map_rgb_color(pcxgray_map_rgb_color);
private dev_proc_map_color_rgb(pcxgray_map_color_rgb);

private gx_device_procs pcxgray_procs =
  prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
    pcxgray_map_rgb_color, pcxgray_map_color_rgb);
gx_device_printer far_data gs_pcxgray_device =
  prn_device(pcxgray_procs, "pcxgray",
	DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	8, pcx256_print_page);

/* 4-bit planar (EGA/VGA-style) color. */

private dev_proc_print_page(pcx16_print_page);

private gx_device_procs pcx16_procs =
  prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
    pc_4bit_map_rgb_color, pc_4bit_map_color_rgb);
gx_device_printer far_data gs_pcx16_device =
  prn_device(pcx16_procs, "pcx16",
	DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	4, pcx16_print_page);

/* Chunky 8-bit (SuperVGA-style) color. */
/* (Uses a fixed palette of 3,3,2 bits.) */

private gx_device_procs pcx256_procs =
  prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
    pc_8bit_map_rgb_color, pc_8bit_map_color_rgb);
gx_device_printer far_data gs_pcx256_device =
  prn_device(pcx256_procs, "pcx256",
	DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	8, pcx256_print_page);

/* ------ Private definitions ------ */

/* All two-byte quantities are stored LSB-first! */
#if arch_is_big_endian
#  define assign_ushort(a,v) a = ((v) >> 8) + ((v) << 8)
#else
#  define assign_ushort(a,v) a = (v)
#endif

typedef struct pcx_header_s {
	byte 	manuf;		/* always 0x0a */
	byte	version;	/* version info = 0,2,3,5 */
	byte	encoding;	/* 1=RLE */
	byte	bpp;		/* bits per pixel */
	ushort	x1;		/* picture dimensions */
	ushort	y1;
	ushort	x2;
	ushort	y2;
	ushort	hres;		/* horz. resolution */
	ushort	vres;		/* vert. resolution */
	byte	palette[16*3];	/* color palette */
	byte	reserved;
	byte	nplanes;	/* number of color planes */
	ushort	bpl;		/* number of bytes per line (uncompressed) */
	ushort	palinfo;	/* palette info 1=color, 2=grey */
	byte	xtra[58];	/* fill out header to 128 bytes */
} pcx_header;

/* 
** version info for PCX is as follows 
**
** 0 == 2.5
** 2 == 2.8 w/palette info
** 3 == 2.8 without palette info
** 5 == 3.0 (includes palette)
**
*/

/* Forward declarations */
private void pcx_write_rle(P3(const byte *, const byte *, FILE *));
private int pcx_write_page(P4(gx_device_printer *, FILE *, pcx_header _ss *, int));

/* Write a monochrome PCX page. */
private int
pcxmono_print_page(gx_device_printer *pdev, FILE *file)
{	pcx_header header;
	header.bpp = 1;
	header.nplanes = 1;
	/* Set the first two entries of the short palette. */
	memcpy((byte *)header.palette, "\377\377\377\000\000\000", 6);
	return pcx_write_page(pdev, file, &header, 0);
}

/* Write an "old" PCX page. */
static const byte ega_palette[16*3] = {
  0x00,0x00,0x00,  0x00,0x00,0xaa,  0x00,0xaa,0x00,  0x00,0xaa,0xaa,
  0xaa,0x00,0x00,  0xaa,0x00,0xaa,  0xaa,0xaa,0x00,  0xaa,0xaa,0xaa,
  0x55,0x55,0x55,  0x55,0x55,0xff,  0x55,0xff,0x55,  0x55,0xff,0xff,
  0xff,0x55,0x55,  0xff,0x55,0xff,  0xff,0xff,0x55,  0xff,0xff,0xff
};
private int
pcx16_print_page(gx_device_printer *pdev, FILE *file)
{	pcx_header header;
	header.bpp = 1;
	header.nplanes = 4;
	/* Fill the EGA palette appropriately. */
	memcpy((byte *)header.palette, ega_palette, sizeof(ega_palette));
	return pcx_write_page(pdev, file, &header, 1);
}

/* Write a "new" PCX page. */
private int
pcx256_print_page(gx_device_printer *pdev, FILE *file)
{	pcx_header header;
	int code;
	header.bpp = 8;
	header.nplanes = 1;
	/* Clear the EGA palette */
	memset((byte *)header.palette, 0, sizeof(header.palette));
	code = pcx_write_page(pdev, file, &header, 0);
	if ( code >= 0 )
	{	/* Write out the palette. */
		fputc(0x0c, file);
		code = pc_write_palette((gx_device *)pdev, 256, file);
	}
	return code;
}

/* Write out a page in PCX format. */
/* This routine is used for all four formats (monochrome, gray scale, */
/* planar "8-bit" actually 4-bit color, and chunky 8-bit color.) */
private int
pcx_write_page(gx_device_printer *pdev, FILE *file, pcx_header _ss *phdr,
  int planar)
{	int orig_raster = gdev_prn_raster(pdev);
	int raster = round_up(orig_raster, 2);	/* PCX format requires even */
	uint rsize = round_up((pdev->width + 7) >> 3, 2);	/* ditto */
	int height = pdev->height;
	int depth = pdev->color_info.depth;
	byte *line = (byte *)gs_malloc(raster + rsize, 1, "pcx file buffer");
	byte *plane = line + raster;
	int y;
	int code = 0;			/* return code */
	if ( line == 0 )		/* can't allocate line buffer */
		return_error(gs_error_VMerror);

	/* Set up the header struct. */

	phdr->manuf = 10;
	phdr->version = 5;
	phdr->encoding = 1;	/* 1 for rle 8-bit encoding */
	/* bpp was set by the caller */
	phdr->x1 = 0;
	phdr->y1 = 0;
	assign_ushort(phdr->x2, pdev->width-1);
	assign_ushort(phdr->y2, height-1);
	assign_ushort(phdr->hres, (int)pdev->x_pixels_per_inch);
	assign_ushort(phdr->vres, (int)pdev->y_pixels_per_inch);
	phdr->reserved = 0;
	/* nplanes was set by the caller */
	assign_ushort(phdr->bpl, (planar && depth > 1 ? rsize : raster));
	assign_ushort(phdr->palinfo, (gx_device_has_color(pdev) ? 1 : 2));

	/* Write the header. */

	if ( fwrite((const char *)phdr, 1, 128, file) < 128 )
	   {	code = gs_error_ioerror;
		goto pcx_done;
	   }

	/* Dump the contents of the image. */
	for ( y = 0; y < height; y++ )
	{	byte *row;
		byte *end;
		int code = gdev_prn_get_bits(pdev, y, line, &row);
		if ( code < 0 ) break;
		end = row + raster;
		/* Clear an odd trailing byte. */
		if ( orig_raster & 1 )
			end[-1] = 0;
		switch ( depth )
		   {
		case 1:
		   {	/* PCX assumes 0=black, 1=white. */
			register byte *bp;
			for ( bp = row; bp < end; bp++ )
				*bp = ~*bp;
			pcx_write_rle(row, end, file);
		   }
			break;
		case 8:
		   {	register int shift;

			if ( !gx_device_has_color(pdev) )
			   {	/* Can't map gray scale */
				code = gs_error_undefinedresult;
				goto pcx_done;
			   }

			if ( !planar )
			   {	/* Just write the bits */
				pcx_write_rle(row, end, file);
				break;
			   }

			for ( shift = 0; shift < 4; shift++ )
			{	register byte *from, *to;
				register int bmask = 1 << shift;
				for ( from = row, to = plane;
				      from < end; from += 8
				    )
				{	*to++ =
					  ((((uint)from[0] & bmask) << 7) +
					   (((uint)from[1] & bmask) << 6) +
					   (((uint)from[2] & bmask) << 5) +
					   (((uint)from[3] & bmask) << 4) +
					   (((uint)from[4] & bmask) << 3) +
					   (((uint)from[5] & bmask) << 2) +
					   (((uint)from[6] & bmask) << 1) +
					   (((uint)from[7] & bmask)))
					  >> shift;
				}
				pcx_write_rle(plane, plane + rsize, file);
			}
		   }
			break;

		default:
			  code = gs_error_undefinedresult;
			  goto pcx_done;

		   }
	}

pcx_done:
	gs_free((char *)line, raster + rsize, 1, "pcx file buffer");

	return code;
}

/* ------ Internal routines ------ */

/* Map an RGB color to a gray value. */
private gx_color_index
pcxgray_map_rgb_color(gx_device *dev, ushort r, ushort g, ushort b)
{	/* We round the value rather than truncating it. */
	gx_color_value gray =
		((r * (ulong)lum_red_weight) +
		 (g * (ulong)lum_green_weight) +
		 (b * (ulong)lum_blue_weight) +
		 (lum_all_weights / 2)) / lum_all_weights
		* dev->color_info.max_gray / gx_max_color_value;
	return gray;
}

/* Map a gray value back to an RGB color. */
private int
pcxgray_map_color_rgb(gx_device *dev, gx_color_index color, ushort prgb[3])
{	gx_color_value gray =
		color * gx_max_color_value / dev->color_info.max_gray;
	prgb[0] = gray;
	prgb[1] = gray;
	prgb[2] = gray;
	return 0;
}

/* Write one line in PCX run-length-encoded format. */
private void
pcx_write_rle(const byte *from, const byte *end, FILE *file)
{	while ( from < end )
	{	byte data = *from++;
		int count = 1;
		while ( (from < end) && (*from == data) )
			count++, from++;
		while ( count > 63 )
		{	putc(0xff, file);
			putc(data, file);
			count -= 63;
		}
		if ( count != 1 || data >= 0xc0 )
		{	putc(count | 0xc0, file);
		}
		putc(data, file);
	}
}
