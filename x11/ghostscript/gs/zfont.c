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

/* zfont.c */
/* Font operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gzstate.h"		/* must precede gxdevice */
#include "gxdevice.h"		/* must precede gxfont */
#include "gschar.h"
#include "gxfont.h"
#include "gxfdir.h"
#include "gxcache.h"
#include "alloc.h"
#include "bfont.h"
#include "dict.h"
#include "iname.h"
#include "packed.h"
#include "save.h"
#include "state.h"
#include "store.h"

/* Imported operators */
extern int zcleartomark(P1(os_ptr));

/* Forward references */
private int font_param(P2(os_ptr, gs_font **));
private int make_font(P3(os_ptr, const gs_matrix *, const ref *));
private void make_uint_array(P3(os_ptr, const uint *, int));

/* The (global) font directory */
gs_font_dir *ifont_dir = 0;		/* needed for buildfont */

/* Names of system-known keys in font dictionaries: */
ref name_FontMatrix;			/* needed for buildfont */
ref name_FID;
private ref name_OrigFont;		/* needed for scalefont/makefont */
private ref name_ScaleMatrix;		/* ditto */

/* Initialize the font operators */
private void
zfont_init(void)
{	static const names_def fnd[] = {

	/* Create the names of the standard elements of */
	/* a font dictionary. */
	   { "FontMatrix", &name_FontMatrix },
	   { "FID", &name_FID },
	   { "OrigFont", &name_OrigFont },
	   { "ScaleMatrix", &name_ScaleMatrix },

	/* Mark the end of the initialized name list. */
	   names_def_end
	};

	ifont_dir = gs_font_dir_alloc(&alloc_memory_procs);
	init_names(fnd);
}

/* <font> <scale> scalefont <new_font> */
int
zscalefont(register os_ptr op)
{	int code;
	float scale;
	gs_matrix mat;
	if ( (code = num_params(op, 1, &scale)) < 0 ) return code;
	if ( (code = gs_make_scaling(scale, scale, &mat)) < 0 ) return code;
	return make_font(op, &mat, NULL);
}

/* <font> <matrix> makefont <new_font> */
int
zmakefont(register os_ptr op)
{	int code;
	gs_matrix mat;
	if ( (code = read_matrix(op, &mat)) < 0 ) return code;
	return make_font(op, &mat, op);
}

/* <font> setfont - */
int
zsetfont(register os_ptr op)
{	gs_font *pfont;
	int code = font_param(op, &pfont);
	if ( code < 0 || (code = gs_setfont(igs, pfont)) < 0 )
	  return code;
	istate.font = *op;
	pop(1);
	return code;
}

/* - currentfont <font> */
int
zcurrentfont(register os_ptr op)
{	push(1);
	*op = istate.font;
	return 0;
}

/* - cachestatus <mark> <bsize> <bmax> <msize> <mmax> <csize> <cmax> <blimit> */
int
zcachestatus(register os_ptr op)
{	uint status[7];
	gs_cachestatus(ifont_dir, status);
	push(7);
	make_uint_array(op - 6, status, 7);
	return 0;
}

/* <blimit> setcachelimit - */
int
zsetcachelimit(register os_ptr op)
{	long limit;
	check_type(*op, t_integer);
	limit = op->value.intval;
	if ( (ulong)limit > max_uint )	/* also covers limit < 0 */
		return_error(e_rangecheck);
	gs_setcachelimit(ifont_dir, (uint)limit);
	pop(1);
	return 0;
}

/* <mark> <size> <lower> <upper> setcacheparams - */
int
zsetcacheparams(register os_ptr op)
{	uint params[2];
	int i, code;
	for ( i = 0; i < 2 && !r_has_type(op - i, t_mark) ; i++ )
	   {	long parm;
		check_type(op[-i], t_integer);
		parm = op[-i].value.intval;
		if ( (ulong)parm > max_uint )	/* covers parm < 0 */
			return_error(e_rangecheck);
		params[i] = parm;
	   }
	switch ( i )
	   {
	case 2:
		if ( (code = gs_setcachelower(ifont_dir, params[1])) < 0 )
			return code;
	case 1:
		if ( (code = gs_setcacheupper(ifont_dir, params[0])) < 0 )
			return code;
	case 0: ;
	   }
	return zcleartomark(op);
}

/* - currentcacheparams <mark> <size> <lower> <upper> */
int
zcurrentcacheparams(register os_ptr op)
{	uint params[2];
	params[0] = gs_currentcachelower(ifont_dir);
	params[1] = gs_currentcacheupper(ifont_dir);
	push(3);
	make_tv(op - 2, t_mark, intval, 0);
	make_uint_array(op - 1, params, 2);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zfont_op_defs[] = {
	{"0currentfont", zcurrentfont},
	{"2makefont", zmakefont},
	{"2scalefont", zscalefont},
	{"1setfont", zsetfont},
	{"0cachestatus", zcachestatus},
	{"1setcachelimit", zsetcachelimit},
	{"1setcacheparams", zsetcacheparams},
	{"0currentcacheparams", zcurrentcacheparams},
	op_def_end(zfont_init)
};

/* ------ Subroutines ------ */

/* Validate a font parameter. */
private int
font_param(os_ptr fp, gs_font **pfont)
{	/* Check that fp is a read-only dictionary, */
	/* and that it has a FID entry. */
	ref *pid;
	check_type(*fp, t_dictionary);
	if ( dict_find(fp, &name_FID, &pid) <= 0 )
		return_error(e_invalidfont);
	*pfont = pid->value.pfont;
	if ( *pfont == 0 )
		return_error(e_invalidfont); /* unregistered font */
	return 0;
}

/* Add the FID entry to a font dictionary. */
int
add_FID(ref *fp /* t_dictionary */,  gs_font *pfont)
{	ref fid;
	make_tv_new(&fid, t_fontID, pfont, pfont);
	return dict_put(fp, &name_FID, &fid);
}

/* Make a transformed font (common code for makefont/scalefont). */
private int
make_font(os_ptr op, const gs_matrix *pmat, const ref *pmref)
{	os_ptr fp = op - 1;
	gs_font *oldfont, *newfont, *ffont;
	int code;
	if ( (code = font_param(fp, &oldfont)) < 0 ||
	     (code = gs_makefont(ifont_dir, oldfont, pmat,
				 &newfont, &ffont)) < 0
	   )
	  return code;
	/* Check whether the scaled font was already cached. */
	if ( newfont->client_data == 0 )	/* not in the cache */
	{	ref newdict, newdata, newmat;
		ref mref;
#define font_data_refs (sizeof(font_data) / sizeof(ref))
		uint data_len = font_data_refs + 6;
		uint dlen = dict_maxlength(fp);
		/* Ensure room for FontID, OrigFont, ScaleMatrix. */
		uint mlen = dict_length(fp) + 3;
		if ( pmref == NULL )
		{	/* Prepare to create the matrix implied by scalefont. */
			data_len += 6;
		}
		if ( dlen < mlen )
			dlen = mlen;
		if ( (code = dict_create(dlen, &newdict)) < 0 ||
		     (code = dict_copy(fp, &newdict)) < 0 ||
		     (code = alloc_array(&newdata, a_all, data_len, "make_font")) < 0
		   )
		  return code;
		if ( pmref == NULL )
		{	/* Create the scaling matrix now. */
			data_len -= 6;
			refcpy_to_new(newdata.value.refs + data_len,
				      (const ref *)pmat, 6);
			r_set_size(&newdata, data_len);
			make_array(&mref, a_readonly, 6,
				   newdata.value.refs + data_len);
			pmref = &mref;
		}
		make_array(&newmat, a_readonly, 6, newdata.value.refs + font_data_refs);
#undef font_data_refs
		if ( (code = dict_put(&newdict, &name_FontMatrix, &newmat)) < 0 ||
		     (code = dict_put(&newdict, &name_OrigFont, fp)) < 0 ||
		     (pmref != NULL && (code = dict_put(&newdict, &name_ScaleMatrix, pmref)) < 0) ||
		     (code = add_FID(&newdict, newfont)) < 0
		   )
			return code;
		newfont->client_data = (char *)newdata.value.refs;
		*(font_data *)newdata.value.refs =
			*(font_data *)(oldfont->client_data);
		((font_data *)newdata.value.refs)->dict = newdict;
		*(gs_matrix *)newmat.value.refs = newfont->FontMatrix;
		r_clear_attrs(dict_access_ref(&newdict), a_write);
	}
	*fp = ((font_data *)(newfont->client_data))->dict;
	if ( ffont )
	  { /****** SHOULD DECREMENT REFCT ******/
	  }
	pop(1);
	return 0;
}

/* Convert an array of (unsigned) integers to stack form. */
private void
make_uint_array(register os_ptr op, const uint *intp, int count)
{	int i;
	for ( i = 0; i < count; i++, op++, intp++ )
		make_int(op, *intp);
}

/* Remove scaled font and character cache entries that would be */
/* invalidated by a restore. */
void
font_restore(const alloc_save *save)
{	gs_font_dir *pdir = ifont_dir;
	if ( pdir == 0 )		/* never initialized */
		return;

	/* Purge original (unscaled) fonts. */

	{	gs_font *pfont;
otop:		for ( pfont = pdir->orig_fonts; pfont != 0;
		      pfont = pfont->next
		    )
		{ if ( alloc_is_since_save((char *)pfont, save) )
		   { gs_purge_font(pfont); goto otop; }
		}
	}

	/* Purge scaled fonts. */

	{	gs_font *pfont;
top:		for ( pfont = pdir->scaled_fonts; pfont != 0;
		      pfont = pfont->next
		    )
		{ if ( alloc_is_since_save((char *)pfont, save) )
		   { gs_purge_font(pfont); goto top; }
		}
	}

	/* Purge xfonts. */

	{	cached_fm_pair *pair;
		uint n;
		for ( pair = pdir->fmcache.mdata, n = pdir->fmcache.mmax;
		      n > 0; pair++, n--
		    )
		{	if ( !fm_pair_is_free(pair) &&
			     pair->xfont != 0 &&
			     alloc_is_since_save((char *)pair->xfont, save)
			   )
			  gs_purge_fm_pair(pdir, pair, 1);
		}
	}
}
