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

/* zfont2.c */
/* Font creation utilities for Ghostscript */
#include "memory_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gxfixed.h"
#include "gsmatrix.h"
#include "gxdevice.h"
#include "gschar.h"
#include "gxfont.h"
#include "alloc.h"
#include "bfont.h"
#include "dict.h"
#include "dparam.h"
#include "ilevel.h"
#include "iname.h"
#include "packed.h"
#include "save.h"		/* for alloc_array */
#include "store.h"

/* Global font-related objects */
/* Names of system-known keys in font dictionaries: */
static ref name_FontType;
static ref name_FontName;
static ref name_WMode;
static ref name_Encoding;
static ref name_FontBBox;
static ref name_UniqueID;
static ref name_XUID;
static ref name_BuildChar;
static ref name_BuildGlyph;
/* Bitmap fonts */
static ref name_BitmapWidths;
static ref name_ExactSize;
static ref name_InBetweenSize;
static ref name_TransformedChar;

/* Registered encodings.  See font.h for documentation. */
ref registered_Encodings[registered_Encodings_countof];

/* Initialize the font building operators */
private void
zfont2_init(void)
{	static const names_def fnd2[] = {

	/* Create the names of the standard elements of */
	/* a font dictionary. */
	   { "FontType", &name_FontType },
	   { "FontName", &name_FontName },
	   { "WMode", &name_WMode },
	   { "Encoding", &name_Encoding },
	   { "FontBBox", &name_FontBBox },
	   { "UniqueID", &name_UniqueID },
	   { "XUID", &name_XUID },
	   { "BuildChar", &name_BuildChar },
	   { "BuildGlyph", &name_BuildGlyph },
	   { "BitmapWidths", &name_BitmapWidths },
	   { "ExactSize", &name_ExactSize },
	   { "InBetweenSize", &name_InBetweenSize },
	   { "TransformedChar", &name_TransformedChar },

	/* Mark the end of the initialized name list. */
	   names_def_end
	};

	init_names(fnd2);

	/* Initialize the registered Encodings. */
	{	int i;
		for ( i = 0; i < registered_Encodings_countof; i++ )
			make_array(&registered_Encodings[i], 0, 0, NULL);
	}
}

/* <string|name> <font_dict> .buildfont3 <string|name> <font> */
/* Build a type 3 (user-defined) font. */
int
zbuildfont3(os_ptr op)
{	int ccode, gcode, code;
	build_proc_refs build;
	ref pnull;
	gs_font *pfont;
	check_type(*op, t_dictionary);
	ccode = dict_find(op, &name_BuildChar, &build.pBuildChar);
	gcode = dict_find(op, &name_BuildGlyph, &build.pBuildGlyph);
	make_null(&pnull);
	if ( ccode <= 0 )
	{	if ( gcode <= 0 )
			return_error(e_invalidfont);
		build.pBuildChar = &pnull;
	}
	else
	{	check_proc(*build.pBuildChar);
	}
	if ( gcode <= 0 )
		build.pBuildGlyph = &pnull;
	else
	{	check_proc(*build.pBuildGlyph);
	}
	code = build_gs_simple_font(op, &pfont, ft_user_defined, &build);
	if ( code < 0 )
		return code;
	return define_gs_font(pfont);
}

/* <int> <packedarray> .registerencoding - */
private int
zregisterencoding(register os_ptr op)
{	long i;
	check_type(op[-1], t_integer);
	check_read_type(*op, t_shortarray);
	i = op[-1].value.intval;
	if ( i >= 0 && i < registered_Encodings_countof )
	{	ref_assign_old(&registered_Encodings[i], op,
			       ".registerencoding");
	}
	pop(2);
	return 0;
}

/* Encode a character. */
/* (This is very inefficient right now; we can speed it up later.) */
private gs_glyph
zfont_encode_char(gs_show_enum *penum, gs_font *pfont, gs_char *pchr)
{	const ref *pencoding =
		&((font_data *)(pfont->client_data))->Encoding;
	ulong index = *pchr;	/* work around VAX widening bug */
	ref cname;
	int code = array_get(pencoding, (long)index, &cname);
	if ( code < 0 || !r_has_type(&cname, t_name) )
		return gs_no_glyph;
	return (gs_glyph)name_index(&cname);
}

/* Get the name of a glyph. */
/* The following typedef is needed to work around a bug in */
/* some AIX C compiler. */
typedef const char *const_chars;
private const_chars
zfont_glyph_name(gs_glyph index, uint *plen)
{	ref nref, sref;
	name_index_ref(index, &nref);
	name_string_ref(&nref, &sref);
	*plen = r_size(&sref);
	return (const char *)sref.value.const_bytes;
}

/* ------ Initialization procedure ------ */

op_def zfont2_op_defs[] = {
	{"2.buildfont3", zbuildfont3},
	{"2.registerencoding", zregisterencoding},
	op_def_end(zfont2_init)
};

/* ------ Subroutines ------ */

/* Do the common work for building a font of any non-composite FontType. */
/* The caller guarantees that *op is a dictionary. */
int
build_gs_simple_font(os_ptr op, gs_font **ppfont, font_type ftype,
  const build_proc_refs *pbuild)
{	ref *pbbox;
	float bbox[4];
	gs_uid uid;
	int code;
	gs_font *pfont;
	/* Pre-clear the bbox in case it's invalid. */
	/* The Red Books say that FontBBox is required, */
	/* but the Adobe interpreters don't require it, */
	/* and a few user-written fonts don't supply it, */
	/* or supply one of the wrong size (!). */
	bbox[0] = bbox[1] = bbox[2] = bbox[3] = 0.0;
	if ( dict_find(op, &name_FontBBox, &pbbox) > 0 )
	{	if ( !r_is_array(pbbox) )
			return_error(e_invalidfont);
		if ( r_size(pbbox) == 4 )
		{	const ref_packed *pbe = pbbox->value.packed;
			ref rbe[4];
			int i;
			for ( i = 0; i < 4; i++ )
			{	packed_get(pbe, rbe + i);
				pbe = packed_next(pbe);
			}
			if ( num_params(rbe + 3, 4, bbox) < 0 )
				return_error(e_invalidfont);
		}
	}
	code = get_gs_font_uid(op, &uid);
	if ( code < 0 ) return code;
	code = build_gs_font(op, ppfont, ftype, pbuild);
	if ( code != 0 ) return code;	/* invalid or scaled font */
	pfont = *ppfont;
	pfont->data.base.FontBBox.p.x = bbox[0];
	pfont->data.base.FontBBox.p.y = bbox[1];
	pfont->data.base.FontBBox.q.x = bbox[2];
	pfont->data.base.FontBBox.q.y = bbox[3];
	pfont->data.base.UID = uid;
	{	const ref *pfe =
			&((font_data *)(pfont->client_data))->Encoding;
		int index;
		for ( index = registered_Encodings_countof; --index >= 0; )
		  if ( obj_eq(pfe, &registered_Encodings[index]) )
		    break;
		pfont->data.base.encoding_index = index;
		if ( index < 0 )
		{	/* Look for an encoding that's "close". */
			int near_index = -1;
			uint esize = r_size(pfe);
			uint best = esize / 3;	/* must match at least this many */
			for ( index = registered_Encodings_countof; --index >= 0; )
			{	const ref *pre = &registered_Encodings[index];
				uint match = esize;
				int i;
				if ( r_size(pre) != esize )
					continue;
				for ( i = esize; --i >= 0; )
				{	ref fe, re;
					array_get(pfe, (long)i, &fe);
					array_get(pre, (long)i, &re);
					if ( !obj_eq(&fe, &re) )
						match--;
				}
				if ( match > best )
					best = match,
					near_index = index;
			}
			index = near_index;
		}
		pfont->data.base.nearest_encoding_index = index;
	}
	return 0;
}

/* Get the UniqueID or XUID from a font dictionary. */
int
get_gs_font_uid(os_ptr op, gs_uid *puid)
{	ref *puniqueid;
	/* In a Level 2 environment, check for XUID first. */
	if ( level2_enabled && dict_find(op, &name_XUID, &puniqueid) > 0 )
	{	long *xvalues;
		uint size, i;
		check_array_else(*puniqueid, e_invalidfont);
		size = r_size(puniqueid);
		if ( size == 0 )
			return_error(e_invalidfont);
		xvalues = (long *)alloc(size, sizeof(long), "get XUID");
		if ( xvalues == 0 )
			return_error(e_VMerror);
		/* Get the values from the XUID array. */
		for ( i = 0; i < size; i++ )
		{	const ref *pvalue = puniqueid->value.const_refs + i;
			if ( !r_has_type(pvalue, t_integer) )
			{	alloc_free((char *)xvalues, size, sizeof(long), "get XUID");
				return_error(e_invalidfont);
			}
			xvalues[i] = pvalue->value.intval;
		}
		uid_set_XUID(puid, xvalues, size);
		return 0;
	}
	/* If no UniqueID entry, set the UID to invalid, */
	/* because UniqueID need not be present in all fonts, */
	/* and if it is, the legal range is 0 to 2^24-1. */
	if ( dict_find(op, &name_UniqueID, &puniqueid) <= 0 )
		uid_set_invalid(puid);
	else
	   {	if ( !r_has_type(puniqueid, t_integer) ||
		     puniqueid->value.intval < 0 ||
		     puniqueid->value.intval > ((1L << 24) - 1)
		   )
			return_error(e_invalidfont);
		/* Apparently fonts created by Fontographer often have */
		/* a UniqueID of 0, contrary to Adobe's specifications. */
		/* Treat 0 as equivalent to -1 (no UniqueID). */
		if ( puniqueid->value.intval == 0 )
			uid_set_invalid(puid);
		else
			uid_set_UniqueID(puid, puniqueid->value.intval);
	   }
	return 0;
}

/* Do the common work for building a font of any FontType. */
/* The caller guarantees that *op is a dictionary. */
/* op[-1] must be the key under which the font is being registered */
/* in FontDirectory, normally a name or string. */
/* Return 0 for a new font, 1 for a font made by makefont or scalefont, */
/* or a negative error code. */
private void get_font_name(P2(ref *, const ref *));
private void copy_font_name(P2(gs_font_name *, const ref *));
int
build_gs_font(os_ptr op, gs_font **ppfont, font_type ftype,
  const build_proc_refs *pbuild)
{	ref kname, fname;		/* t_string */
	ref *pftype;
	ref *pfontname;
	ref *pmatrix;
	gs_matrix mat;
	ref *pencoding;
	int bitmapwidths, exactsize, inbetweensize, transformedchar;
	int wmode;
	int code;
	gs_font *pfont;
	ref *pfid;
	ref *aop = dict_access_ref(op);
	get_font_name(&kname, op - 1);
	if ( dict_find(op, &name_FontType, &pftype) <= 0 ||
	    !r_has_type(pftype, t_integer) ||
	    pftype->value.intval != (int)ftype ||
	    dict_find(op, &name_FontMatrix, &pmatrix) <= 0 ||
	    read_matrix(pmatrix, &mat) < 0 ||
	    dict_find(op, &name_Encoding, &pencoding) <= 0 ||
	    !r_is_array(pencoding)
	   )
		return_error(e_invalidfont);
	if ( dict_find(op, &name_FontName, &pfontname) > 0 )
		get_font_name(&fname, pfontname);
	else
		make_string(&fname, a_readonly, 0, NULL);
	if ( (code = dict_int_param(op, &name_WMode, 0, 1, 0, &wmode)) < 0 ||
	     (code = dict_bool_param(op, &name_BitmapWidths, 0, &bitmapwidths)) < 0 ||
	     (code = dict_int_param(op, &name_ExactSize, 0, 2, fbit_use_bitmaps, &exactsize)) < 0 ||
	     (code = dict_int_param(op, &name_InBetweenSize, 0, 2, fbit_use_outlines, &inbetweensize)) < 0 ||
	     (code = dict_int_param(op, &name_TransformedChar, 0, 2, fbit_use_outlines, &transformedchar)) < 0
	   )
		return code;
	code = dict_find(op, &name_FID, &pfid);
	if ( r_has_attr(aop, a_write) )
	   {	/* Assume this is a new font */
		ref daref;
		font_data *pdata;
		if ( code > 0 )
			return_error(e_invalidfont);	/* has FID already */
		if ( (pfont = (gs_font *)alloc(1, sizeof(gs_font), "buildfont(font)")) == 0 )
			return_error(e_VMerror);
		code = alloc_array(&daref, 0, sizeof(font_data) / sizeof(ref), "buildfont(data)");
		if ( code < 0 )
			return code;
		if ( (code = add_FID(op, pfont)) < 0 )
			return code;
		pdata = (font_data *)daref.value.refs;
		ref_assign_new(&pdata->dict, op);
		ref_assign_new(&pdata->BuildChar, pbuild->pBuildChar);
		ref_assign_new(&pdata->BuildGlyph, pbuild->pBuildGlyph);
		ref_assign_new(&pdata->Encoding, pencoding);
		pfont->base = pfont;
		pfont->dir = ifont_dir;
		pfont->client_data = (char *)pdata;
		pfont->FontType = ftype;
		pfont->FontMatrix = mat;
		pfont->BitmapWidths = bitmapwidths;
		pfont->ExactSize = exactsize;
		pfont->InBetweenSize = inbetweensize;
		pfont->TransformedChar = transformedchar;
		pfont->WMode = wmode;
		pfont->build_char_proc = gs_no_build_char_proc;
		pfont->encode_char_proc = zfont_encode_char;
		pfont->glyph_name_proc = zfont_glyph_name;
	   }
	else
	   {	/* Assume this was made by makefont or scalefont */
		if ( code <= 0 || !r_has_type(pfid, t_fontID) )
			return_error(e_invalidfont);
		pfont = pfid->value.pfont;
	   }
	copy_font_name(&pfont->key_name, &kname);
	copy_font_name(&pfont->font_name, &fname);
	*ppfont = pfont;
	return 0;
}

/* Get the string corresponding to a font name. */
/* If the font name isn't a name or a string, return an empty string. */
private void
get_font_name(ref *pfname, const ref *op)
{	switch ( r_type(op) )
	{
	case t_string:
		*pfname = *op;
		break;
	case t_name:
		name_string_ref(op, pfname);
		break;
	default:
		/* This is weird, but legal.... */
		make_string(pfname, a_readonly, 0, NULL);
	}
}

/* Copy a font name into the gs_font structure. */
private void
copy_font_name(gs_font_name *pfstr, const ref *pfname)
{	uint size = r_size(pfname);
	if ( size > gs_font_name_max )
		size = gs_font_name_max;
	memcpy(&pfstr->chars[0], pfname->value.const_bytes, size);
	pfstr->size = size;
}

/* Finish building a font, by calling gs_definefont if needed. */
int
define_gs_font(gs_font *pfont)
{	return (pfont->base == pfont ?		/* i.e., original font */
		gs_definefont(ifont_dir, pfont) :
		0);
}
