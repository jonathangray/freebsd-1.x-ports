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

/* iccfont.c */
/* Initialization support for compiled fonts */
#include "string_.h"
#include "ghost.h"
#include "alloc.h"
#include "ccfont.h"
#include "dict.h"
#include "dstack.h"
#include "errors.h"
#include "font.h"
#include "iname.h"
#include "iutil.h"
#include "oper.h"
#include "save.h"			/* for alloc_refs */
#include "store.h"

/* Forward references */
private int huge
	cfont_ref_dict_create(P4(ref *, const cfont_dict_keys *,
				 cfont_string_array, const ref *)),
	cfont_string_dict_create(P4(ref *, const cfont_dict_keys *,
				    cfont_string_array, cfont_string_array)),
	cfont_num_dict_create(P4(ref *, const cfont_dict_keys *,
				 cfont_string_array, const float *)),
	cfont_name_array_create(P3(ref *, cfont_string_array, int)),
	cfont_string_array_create(P4(ref *, cfont_string_array, int, uint)),
	cfont_name_create(P2(ref *, const char *));

/* Procedure vector passed to font initialization procedures. */
private const cfont_procs ccfont_procs = {
	cfont_ref_dict_create,
	cfont_string_dict_create,
	cfont_num_dict_create,
	cfont_name_array_create,
	cfont_string_array_create,
	cfont_name_create
};

/* ------ Initialization tables ------ */

/*
 * The file gconfigf.h contains a line
 *	font_("0.font_xxx", gsf_xxx, zf_xxx)
 * for each compiled font.
 */

/* Generate an operator procedure for each font. */
#ifdef __PROTOTYPES__
#  define zfproto(proc) proc(os_ptr op)
#else
#  define zfproto(proc) proc(op) os_ptr op;
#endif
#define font_(fname, fproc, zfproc)\
extern int huge fproc(P2(const cfont_procs *, ref *));\
private int zfproto(zfproc)\
{	int code = fproc(&ccfont_procs, (ref *)(op + 1));\
	if ( code < 0 ) return code;\
	push(1);\
	return 0;\
}
#include "gconfigf.h"

/* Generate the operator initialization table. */
#undef font_
#define font_(fname, fproc, zfproc)\
	{fname, zfproc},
op_def ccfonts_op_defs[] = {
#include "gconfigf.h"
	op_def_end(0)
};

/* ------ Procedural code ------ */

typedef struct {
	const char *str_array;
	const byte *str;
	uint len;
} str_enum;
#define init_str_enum(sep, ksa)\
  (sep)->str_array = ksa
typedef struct {
	cfont_dict_keys keys;
	str_enum strings;
} key_enum;
#define init_key_enum(kep, kp, ksa)\
  (kep)->keys = *kp, init_str_enum(&(kep)->strings, ksa)

/* Check for reaching the end of the keys. */
#define more_keys(kep) ((kep)->keys.num_enc_keys | (kep)->keys.num_str_keys)

/* Get the next string from a string array. */
/* Return true if it was a string, false if it was a null. */
private int near
cfont_next_string(str_enum _ss *pse)
{	const char *str = pse->str_array;
	uint len = (((const byte *)str)[0] << 8) + ((const byte *)str)[1];
	int was_string = 1;
	if ( len == 0xffff )
	{	len = 0;
		was_string = 0;
	}
	pse->str = (const byte *)str + 2;
	pse->len = len;
	pse->str_array = str + 2 + len;
	return was_string;
}

/* Put the next entry into a dictionary. */
/* We know that more_keys(kp) is true. */
private int near
cfont_put_next(ref *pdict, key_enum _ss *kep, const ref *pvalue)
{	ref kname;
	int code;
#define kp (&kep->keys)
	if ( pdict->value.pdict == 0 )
	{	/* First time, create the dictionary. */
		code = dict_create(kp->num_enc_keys + kp->num_str_keys + kp->extra_slots, pdict);
		if ( code < 0 )
			return code;
	}
	if ( kp->num_enc_keys )
	{	const charindex *skp = kp->enc_keys++;
		code = array_get(&registered_Encodings[skp->encx], (long)(skp->charx), &kname);
		kp->num_enc_keys--;
	}
	else		/* must have kp->num_str_keys != 0 */
	{	cfont_next_string(&kep->strings);
		code = name_ref(kep->strings.str, kep->strings.len, &kname, 0);
		kp->num_str_keys--;
	}
	if ( code < 0 )
		return code;
	return dict_put(pdict, &kname, pvalue);
#undef kp
}

/* ------ Routines called from compiled font initialization ------ */

/* Create a dictionary with general ref values. */
private int huge
cfont_ref_dict_create(ref *pdict, const cfont_dict_keys *kp,
  cfont_string_array ksa, const ref *values)
{	key_enum kenum;
	const ref *vp = values;
	init_key_enum(&kenum, kp, ksa);
	pdict->value.pdict = 0;
	while ( more_keys(&kenum) )
	{	const ref *pvalue = vp++;
		int code = cfont_put_next(pdict, &kenum, pvalue);
		if ( code < 0 ) return code;
	}
	return 0;
}

/* Create a dictionary with string/null values. */
private int huge
cfont_string_dict_create(ref *pdict, const cfont_dict_keys *kp,
  cfont_string_array ksa, cfont_string_array kva)
{	key_enum kenum;
	str_enum senum;
	uint attrs = kp->value_attrs;
	init_key_enum(&kenum, kp, ksa);
	init_str_enum(&senum, kva);
	pdict->value.pdict = 0;
	while ( more_keys(&kenum) )
	{	ref vstring;
		int code;
		if ( cfont_next_string(&senum) )
		{	make_const_string(&vstring, attrs, senum.len,
					  (const byte *)senum.str);
		}
		else
			make_null(&vstring);
		code = cfont_put_next(pdict, &kenum, &vstring);
		if ( code < 0 ) return code;
	}
	return 0;
}

/* Create a dictionary with number values. */
private int huge
cfont_num_dict_create(ref *pdict, const cfont_dict_keys *kp,
  cfont_string_array ksa, const float *values)
{	key_enum kenum;
	const float *vp = values;
	ref vnum;
	init_key_enum(&kenum, kp, ksa);
	pdict->value.pdict = 0;
	while ( more_keys(&kenum) )
	{	float val = *vp++;
		int code;
		if ( val == (int)val )
			make_int(&vnum, (int)val);
		else
			make_real(&vnum, val);
		code = cfont_put_next(pdict, &kenum, &vnum);
		if ( code < 0 ) return code;
	}
	return 0;
}

/* Create an array with name values. */
private int huge
cfont_name_array_create(ref *parray, cfont_string_array ksa, int size)
{	int code = alloc_array(parray, a_readonly, size, "cfont_name_array_create");
	ref *aptr = parray->value.refs;
	int i;
	str_enum senum;
	if ( code < 0 ) return code;
	init_str_enum(&senum, ksa);
	for ( i = 0; i < size; i++, aptr++ )
	{	ref nref;
		cfont_next_string(&senum);
		code = name_ref((const byte *)senum.str, senum.len, &nref, 0);
		if ( code < 0 ) return code;
		ref_assign_new(aptr, &nref);
	}
	return 0;
}

/* Create an array with string/null values. */
private int huge
cfont_string_array_create(ref *parray, cfont_string_array ksa,
  int size, uint attrs)
{	int code = alloc_array(parray, a_readonly, size, "cfont_string_array_create");
	ref *aptr = parray->value.refs;
	int i;
	str_enum senum;
	if ( code < 0 ) return code;
	init_str_enum(&senum, ksa);
	for ( i = 0; i < size; i++, aptr++ )
	{	if ( cfont_next_string(&senum) )
		{	make_tasv_new(aptr, t_string, attrs, senum.len,
				      const_bytes, (const byte *)senum.str);
		}
		else
			make_null_new(aptr);
	}
	return 0;
}

/* Create a name. */
private int huge
cfont_name_create(ref *pnref, const char *str)
{	return name_ref((const byte *)str, strlen(str), pnref, 0);
}
