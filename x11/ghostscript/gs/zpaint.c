/* Copyright (C) 1989, 1990, 1991 Aladdin Enterprises.  All rights reserved.

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

/* zpaint.c */
/* Painting operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "estack.h"			/* for image[mask] */
#include "ilevel.h"
#include "store.h"
#include "gscspace.h"
#include "gsmatrix.h"
#include "gsimage.h"
#include "gspaint.h"
#include "state.h"
#include "stream.h"

/* Forward references */
/* zimage_setup is used by zimage2.c */
int zimage_setup(P10(int width, int height, gs_matrix *pmat,
  ref *sources, int bits_per_component,
  int spread, const gs_color_space *pcs, int masked,
  const float *decode, int npop));
private int image_opaque_setup(P4(os_ptr, int, const gs_color_space_type *, int));
private int image_setup(P7(os_ptr, int, int, const gs_color_space_type *, int, const float *, int));
private int image_read(P2(ref *, ref *));
private int image_continue(P1(os_ptr));
private int image_cleanup(P1(os_ptr));

/* - erasepage - */
int
zerasepage(register os_ptr op)
{	return gs_erasepage(igs);
}

/* - fill - */
int
zfill(register os_ptr op)
{	return gs_fill(igs);
}

/* - eofill - */
int
zeofill(register os_ptr op)
{	return gs_eofill(igs);
}

/* - stroke - */
int
zstroke(register os_ptr op)
{	return gs_stroke(igs);
}

/* Standard decoding maps for images. */
static const float decode_01[8] = { 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 };
static const float decode_10[8] = { 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0 };

/* <width> <height> <bits/comp> <matrix> */
/*	true <datasrc_0> ... <datasrc_ncomp-1> <ncomp> colorimage - */
/*	false <datasrc> <ncomp> colorimage - */
int
zcolorimage(register os_ptr op)
{	int spp;			/* samples per pixel */
	int npop = 7;
	os_ptr procp = op - 2;
	static const gs_color_space_type *apcst[] = {
		NULL, &gs_color_space_type_DeviceGray, NULL,
		&gs_color_space_type_DeviceRGB,
		&gs_color_space_type_DeviceCMYK
	};
	int spread = 0;
	check_type(*op, t_integer);	/* ncolors */
	check_type(op[-1], t_boolean);	/* multiproc */
	if ( (ulong)(op->value.intval) > 4 )
		return_error(e_rangecheck);
	switch ( (spp = (int)(op->value.intval)) )
	{
	case 1:
		break;
	case 3: case 4:
		if ( op[-1].value.index )	/* planar format */
			npop += spp - 1,
			procp -= spp - 1,
			spread = 1;
		break;
	default:
		return_error(e_rangecheck);
	}
	return image_opaque_setup(procp, spread, apcst[spp], npop);
}

/* <width> <height> <bits/sample> <matrix> <datasrc> image - */
int
zimage(register os_ptr op)
{	return image_opaque_setup(op, 0, &gs_color_space_type_DeviceGray, 5);
}

/* <width> <height> <paint_1s> <matrix> <datasrc> imagemask - */
int
zimagemask(register os_ptr op)
{	check_type(op[-2], t_boolean);
	return image_setup(op, 1, 0, &gs_color_space_type_DeviceGray, 1,
			   (op[-2].value.index ? decode_01 : decode_10), 5);
}

/* Common setup for image and colorimage. */
private int
image_opaque_setup(register os_ptr op, int spread,
  const gs_color_space_type *pcst, int npop)
{	check_type(op[-2], t_integer);	/* bits/sample */
	if ( (ulong)(op[-2].value.intval) > 8 )
		return_error(e_rangecheck);
	return image_setup(op, (int)op[-2].value.intval, spread, pcst, 0, decode_01, npop);
}

/* Common setup for [color]image and imagemask. */
private int
image_setup(register os_ptr op, int bps, int spread,
  const gs_color_space_type *pcst, int masked, const float *decode, int npop)
{	gs_matrix mat;
	int code;
	gs_color_space cs;
	check_type(op[-4], t_integer);	/* width */
	check_type(op[-3], t_integer);	/* height */
	if ( op[-4].value.intval <= 0 || op[-3].value.intval < 0 )
		return_error(e_rangecheck);
	if ( (code = read_matrix(op - 1, &mat)) < 0 )
		return code;
	cs.type = pcst;
	return zimage_setup((int)op[-4].value.intval,
			    (int)op[-3].value.intval,
			    &mat, op, bps, spread, &cs, masked, decode, npop);
}

/* Common setup for Level 1 image/imagemask/colorimage and */
/* the Level 2 dictionary form of image/imagemask. */
int
zimage_setup(int width, int height, gs_matrix *pmat,
  ref *sources, int bits_per_component,
  int spread, const gs_color_space *pcs,
  int masked, const float *decode, int npop)
{	int code;
	gs_image_enum *penum;
	int px;
	ref *pp;
	int num_sources = (spread ? pcs->type->num_components : 1);
	ref source1;
	/* We push on the estack: */
	/*	Control mark, 4 procs, last plane index, */
	/*	enumeration structure (as bytes). */
#define inumpush 7
	check_estack(inumpush + 2);	/* stuff above, + continuation + proc */
	/* Note that the "procedures" might not be procedures, */
	/* but might be strings or files (Level 2 only). */
	for ( px = 0, pp = sources; px < num_sources; px++, pp++ )
	{	switch ( r_type(pp) )
		{
		case t_file:
			if ( !level2_enabled )
				return_error(e_typecheck);
		case t_string:
			check_read(*pp);
			break;
		default:
			check_proc(*pp);
		}
	}
	if ( height == 0 )
		return 0;	/* empty image */
	if ( masked )
	{	/* Make sure decode is 0..1 or 1..0 */
		if ( decode[0] == 0.0 && decode[1] == 1.0 )
			masked = 1;
		else if ( decode[0] == 1.0 && decode[1] == 0.0 )
			masked = -1;
		else
			return_error(e_rangecheck);
	}
	if ( (penum = (gs_image_enum *)alloc(1, gs_image_enum_sizeof, "image_setup")) == 0 )
		return_error(e_VMerror);
	code = (masked != 0 ?
		gs_imagemask_init(penum, igs, width, height,
				  masked < 0, pmat, 1) :
		gs_image_init(penum, igs, width, height, bits_per_component,
			      spread, pcs, decode, pmat) );
	if ( code < 0 )
	{	alloc_free((char *)penum,
			   1, gs_image_enum_sizeof, "image_setup");
		return code;
	}
	mark_estack(es_other, image_cleanup);
	++esp;
	for ( px = 0, pp = sources; px < 4; esp++, px++, pp++ )
	  if ( px < num_sources )
		*esp = *pp;
	  else
		make_null(esp);
	make_int(esp, 0);		/* current plane */
	r_set_size(esp, num_sources);
	++esp;
	make_tasv(esp, t_string, 0, gs_image_enum_sizeof, bytes, (byte *)penum);
	code = image_read(sources, &source1);
	switch ( code )
	{
	case 1:			/* string or file */
		pop(npop - 1);
		*osp = source1;
		return image_continue(osp);
	case 0:			/* procedure */
		pop(npop);
		return o_push_estack;
	default:
		return code;
	}
}
/* Continuation procedure.  Hand the string to the enumerator. */
private int
image_continue(register os_ptr op)
{	gs_image_enum *penum = (gs_image_enum *)esp->value.bytes;
	int code;
	if ( !r_has_type(op, t_string) )
	   {	/* Procedure didn't return a string.  Quit. */
		esp -= inumpush;
		image_cleanup(op);
		return_error(e_typecheck);
	   }
top:	code = gs_image_next(penum, op->value.bytes, r_size(op));
	if ( r_size(op) == 0 || code != 0 )	/* stop now */
	   {	esp -= inumpush;
		image_cleanup(op);
		if ( code < 0 ) return code;
		code = o_pop_estack;
	   }
	else
	   {	int px = (int)++(esp[-1].value.intval);
		es_ptr pproc = esp - 5;
		if ( px == r_size(esp - 1) )
			esp[-1].value.intval = px = 0;
		code = image_read(pproc + px, op);
		switch ( code )
		{
		case 1:			/* string or file */
			goto top;
		case 0:			/* procedure */
			code = o_push_estack;
			break;
		default:
			return code;
		}
	   }
	pop(1);
	return code;
}
/* Prepare to read from an image data source. */
/* Return 1 for a string (and store the string at *psrc), */
/* 0 for a procedure (push on the estack), <0 for error. */
private int
image_read(ref *psref, ref *psrc)
{	switch ( r_type(psref) )
	{
	case t_string:
		*psrc = *psref;
		return 1;
	case t_file:
	{	stream *s = psref->value.pfile;
		int avail;
		while ( (avail = sbufavailable(s)) == 0 )
		{	int next = sgetc(s);
			if ( next == EOFC )		/* end of data, */
						/* break with avail == 0 */
				break;
			if ( next == ERRC )
				return_error(e_ioerror);
			sputback(s);
		}
		make_string(psrc, a_readonly, avail, sbufptr(s));
		sskip(s, avail);
	}	return 1;
	default:
		push_op_estack(image_continue);
		*++esp = *psref;
		return 0;
	}
}
/* Clean up after enumerating an image */
private int
image_cleanup(os_ptr op)
{	gs_image_enum *penum = (gs_image_enum *)esp[inumpush].value.bytes;
	gs_image_cleanup(penum);
	alloc_free((char *)penum, 1, gs_image_enum_sizeof, "image_cleanup");
	return 0;
}

/* ------ Ghostscript-specific operators ------ */

/* <width> <height> <data> .imagepath - */
int
zimagepath(register os_ptr op)
{	int code;
	check_type(op[-2], t_integer);
	check_type(op[-1], t_integer);
	check_read_type(*op, t_string);
	if ( r_size(op) < ((op[-2].value.intval + 7) >> 3) * op[-1].value.intval )
		return_error(e_rangecheck);
	code = gs_imagepath(igs,
		(int)op[-2].value.intval, (int)op[-1].value.intval,
		op->value.const_bytes);
	if ( code == 0 ) pop(3);
	return code;
}

/* ------ Initialization procedure ------ */

op_def zpaint_op_defs[] = {
	{"0eofill", zeofill},
	{"0erasepage", zerasepage},
	{"0fill", zfill},
	{"7colorimage", zcolorimage},
	{"5image", zimage},
	{"5imagemask", zimagemask},
	{"3.imagepath", zimagepath},
	{"0stroke", zstroke},
		/* Internal operators */
	{"0%image_continue", image_continue},
	op_def_end(0)
};
