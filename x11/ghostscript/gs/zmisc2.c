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

/* zmisc2.c */
/* Miscellaneous Level 2 operators for Ghostscript */
#include "memory_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gsfont.h"
#include "dict.h"
#include "dparam.h"
#include "dstack.h"
#include "estack.h"
#include "ilevel.h"
#include "iname.h"		/* for dict_find_name */
#include "store.h"

/* The (global) font directory */
extern gs_font_dir *ifont_dir;		/* in zfont.c */

/* Import the Level 2 definitions directory from iinit.c. */
extern ref ref_level2dict;
#define level2dict (&ref_level2dict)

/* Import the Level 1 'where' operator from zdict.c. */
extern int zwhere(P1(os_ptr));

/* The system parameter password */
#define max_password 64			/* must be at least 11 */
typedef struct password_s {
	uint size;
	byte data[max_password];
} password;
private password SystemParamsPassword = { 0 };

/* Forward references */
private int set_language_level(P1(int));
private void names_to_stack(P3(os_ptr, const ref _ds * _ds *, int));
private int dict_password_param(P3(const ref *, const ref *, password *));

/* Names used for the 'where' hack. */
static ref name_setcolor;
static ref name_FreeHandDict;

/* User parameter names. */
static ref name_MaxFontItem;
static ref name_MinFontCompress;
static ref name_MaxOpStack;
static ref name_MaxDictStack;
static ref name_MaxExecStack;
static ref name_MaxLocalVM;

/* System parameter names. */
static ref name_Password;		/* (only in incoming dictionaries) */
static ref name_SystemParamsPassword;
static ref name_BuildTime;
static ref name_ByteOrder;
static ref name_RealFormat;
static ref name_MaxFontCache;
static ref name_CurFontCache;

/* Initialization */
private void
zmisc2_init(void)
{
	static const names_def uspn[] = {
		/* needed for 'where' */
	   { "setcolor", &name_setcolor },
	   { "FreeHandDict", &name_FreeHandDict },
		/* User parameters */
	   { "MaxFontItem", &name_MaxFontItem },
	   { "MinFontCompress", &name_MinFontCompress },
	   { "MaxOpStack", &name_MaxOpStack },
	   { "MaxDictStack", &name_MaxDictStack },
	   { "MaxExecStack", &name_MaxExecStack },
	   { "MaxLocalVM", &name_MaxLocalVM },
		/* System parameters */
	   { "Password", &name_Password },
	   { "SystemParamsPassword", &name_SystemParamsPassword },
	   { "BuildTime", &name_BuildTime },
	   { "ByteOrder", &name_ByteOrder },
	   { "RealFormat", &name_RealFormat },
	   { "MaxFontCache", &name_MaxFontCache },
	   { "CurFontCache", &name_CurFontCache },
	   names_def_end
	};
	init_names(uspn);
}

/* ------ Language level operators ------ */

/* - .languagelevel <1 or 2> */
private int
zlanguagelevel(register os_ptr op)
{	push(1);
	ref_assign(op, &ref_language_level);
	return 0;
}

/* <1 or 2> .setlanguagelevel - */
private int
zsetlanguagelevel(register os_ptr op)
{	int code = 0;
	check_type(*op, t_integer);
	if ( op->value.intval < 1 || op->value.intval > 2 )
		return_error(e_rangecheck);
	if ( op->value.intval != ref_language_level.value.intval )
	{	code = set_language_level((int)op->value.intval);
		if ( code < 0 ) return code;
	}
	pop(1);
	ref_assign_old(&ref_language_level, op, "setlanguagelevel");
	return code;
}

/* ------ User and system parameters ------ */

/* <dict> setsystemparams - */
private int
zsetsystemparams(register os_ptr op)
{	int code;
	int ival;
	password pass;
	check_read_type(*op, t_dictionary);
	if ( SystemParamsPassword.size != 0 )
	{	code = dict_password_param(op, &name_Password, &pass);
		if ( code ) return (code < 0 ? code : e_invalidaccess);
		if ( pass.size != SystemParamsPassword.size ||
		     bytes_compare(&pass.data[0], pass.size,
			     &SystemParamsPassword.data[0],
			     SystemParamsPassword.size) != 0
		   )
			return_error(e_invalidaccess);
	}
	code = dict_password_param(op, &name_SystemParamsPassword, &pass);
	if ( code <= 0 )
	{	if ( code < 0 ) return code;
		SystemParamsPassword = pass;
	}
	code = dict_int_param(op, &name_MaxFontCache, 0, max_int, 0, &ival);
	switch ( code )
	{
	default:			/* invalid */
		return code;
	case 1:				/* missing */
		break;
	case 0:
		/****** NOT IMPLEMENTED YET ******/
		;
	}
	pop(1);
	return 0;
}

/* - .currentsystemparams <name1> <value1> ... */
private int
zcurrentsystemparams(os_ptr op)
{	register os_ptr rop = op;
#if arch_floats_are_IEEE
	static const char rfs[] = "IEEE";
#else
	static const char rfs[] = "not IEEE";
#endif
	static const ref _ds *spn[] = {
		&name_ByteOrder, &name_RealFormat, &name_BuildTime,
		&name_MaxFontCache, &name_CurFontCache
	};
#define num_sp countof(spn)
	uint cstat[7];
	push(num_sp * 2);
	names_to_stack(rop, spn, num_sp);
	rop += 2;
	make_bool(rop, !arch_is_big_endian);
	rop += 2;
	make_const_string(rop, a_readonly, sizeof(rfs) - 1,
			  (const byte *)rfs);
	rop += 2;
	make_int(rop, 0);			/* BOGUS */
	gs_cachestatus(ifont_dir, cstat);
	rop += 2;
	make_int(rop, cstat[1]);
	rop += 2;
	make_int(rop, cstat[0]);
#undef num_sp
	return 0;
}

/* <dict> setuserparams - */
private int
zsetuserparams(register os_ptr op)
{	int code;
	int ival;
	check_read_type(*op, t_dictionary);
	code = dict_int_param(op, &name_MaxFontItem, 0, max_int, 0, &ival);
	switch ( code )
	{
	default:			/* invalid */
		return code;
	case 1:				/* missing */
		break;
	case 0:
		if ( (code = gs_setcacheupper(ifont_dir, ival)) < 0 )
			return code;
	}
	code = dict_int_param(op, &name_MinFontCompress, 0, max_int, 0, &ival);
	switch ( code )
	{
	default:			/* invalid */
		return code;
	case 1:				/* missing */
		break;
	case 0:
		if ( (code = gs_setcachelower(ifont_dir, ival)) < 0 )
			return code;
	}
	pop(1);
	return 0;
}

/* - .currentuserparams <name1> <value1> ... */
private int
zcurrentuserparams(os_ptr op)
{	register os_ptr rop = op;
	long cur_vm, max_vm;
	static const ref _ds *upn[] = {
		&name_MaxFontItem, &name_MinFontCompress,
		&name_MaxOpStack, &name_MaxDictStack, &name_MaxExecStack,
		&name_MaxLocalVM
	};
#define num_up countof(upn)
	push(num_up * 2);
	names_to_stack(rop, upn, num_up);
	rop += 2;
	make_int(rop, gs_currentcacheupper(ifont_dir));
	rop += 2;
	make_int(rop, gs_currentcachelower(ifont_dir));
	rop += 2;
	make_int(rop, ostop - osbot + 1);
	rop += 2;
	make_int(rop, dstop - dsbot + 1);
	rop += 2;
	make_int(rop, estop - esbot + 1);
	alloc_status(&cur_vm, &max_vm);
	rop += 2;
	make_int(rop, max_vm);
	return 0;
}

/* ------ The 'where' hack ------ */

private int
z2where(register os_ptr op)
{	/*
	 * Aldus Freehand versions 2.x check for the presence of the
	 * setcolor operator, and if it is missing, substitute a procedure.
	 * Unfortunately, the procedure takes different parameters from
	 * the operator.  As a result, files produced by this application
	 * cause an error if the setcolor operator is actually defined.
	 * Aldus fixed this bug in Freehand 3.0, but there are a lot of
	 * files created by the older versions still floating around.
	 * Therefore, at Adobe's suggestion, we implement the following
	 * dreadful hack in the 'where' operator:
	 *	If the key is /setcolor,
	 *	 there is a dictionary named FreeHandDict, and
	 *	 currentdict is that dictionary,
	 *	then "where" consults only that dictionary and not any other
	 *	 dictionaries on the dictionary stack.
	 */
	const ref *pdref = dsp;
	ref *pvalue;
	if ( !obj_eq(op, &name_setcolor) ||
	     (pvalue = dict_find_name(&name_FreeHandDict)) == 0 ||
	     !obj_eq(pvalue, pdref)
	   )
		return zwhere(op);
	check_dict_read(*pdref);
	if ( dict_find(pdref, op, &pvalue) > 0 )
	{	ref_assign(op, pdref);
		push(1);
		make_true(op);
	}
	else
		make_false(op);
	return 0;
}

/* ------ Initialization procedure ------ */

/* The level setting ops are recognized even in Level 1 mode. */
op_def zmisc2_level_op_defs[] = {
	{"0.languagelevel", zlanguagelevel},
	{"1.setlanguagelevel", zsetlanguagelevel},
	op_def_end(0)
};
op_def zmisc2_op_defs[] = {
	{"0.currentsystemparams", zcurrentsystemparams},
	{"0.currentuserparams", zcurrentuserparams},
	{"1setsystemparams", zsetsystemparams},
	{"1setuserparams", zsetuserparams},
/* Note that this overrides the definition in zdict.c. */
	{"1where", z2where},
	op_def_end(zmisc2_init)
};

/* ------ Internal procedures ------ */

/* Adjust the interpreter for a change in language level. */
/* This is used for the setlanguage level operator, */
/* and after a restore. */
private int
set_language_level(int level)
{	if ( level == 2 )		/* from Level 1 to Level 2 */
	{	/* Insert globaldict in the dictionary stack. */
		/* memcpy isn't guaranteed to work top-to-bottom. */
		ref ndict;
		ref *pdict;
		int code;
		register ds_ptr dp = dsp;
		if ( dsp == dstop )
			return_error(e_dictstackoverflow);
		code = name_ref((const byte *)"globaldict", 10, &ndict, -1);
		if ( code < 0 ) return code;
		code = dict_find(level2dict, &ndict, &pdict);
		if ( code <= 0 )
			return_error(e_undefined);
		if ( !r_has_type(pdict, t_dictionary) )
			return_error(e_typecheck);
		while ( dp > dsbot ) dp[1] = *dp, dp--;
		dsbot[1] = *pdict;
		min_dstack_size++;
		dsp++;
		/* Set other flags for Level 2 operation. */
		dict_auto_expand = 1;
	}
	else				/* from Level 1 to Level 1 */
	{	/* Remove globaldict from the dictionary stack. */
		memcpy((char *)(dsbot + 1), (const char *)(dsbot + 2),
		       (int)((char *)dsp - (char *)(dsbot + 1)));
		min_dstack_size--;
		dsp--;
		/* Set other flags for Level 1 operation. */
		dict_auto_expand = 0;
	}
	/* Swap the contents of level2dict and systemdict. */
	{	int index = dict_first(level2dict);
		ref elt[2];		/* key, value */
		ref *pvalue;
		ref old_value;
		while ( (index = dict_next(level2dict, index, &elt[0])) >= 0 )
		{	int found = dict_find(systemdict, &elt[0], &pvalue);
			switch ( found )
			{
			default:		/* <0, error */
				return found;
			case 0:			/* missing */
				make_null(&old_value);
				break;
			case 1:			/* present */
				old_value = *pvalue;
			}
			if ( r_has_type(&elt[1], t_null) )
				dict_undef(systemdict, &elt[0]);
			else
				dict_put(systemdict, &elt[0], &elt[1]);
			dict_put(level2dict, &elt[0], &old_value);
		}
	}
	return 0;
}

/* Copy names to the stack. */
private void
names_to_stack(os_ptr rop, const ref _ds * _ds * pnames, int count)
{	for ( ++rop; count; rop += 2, pnames++, count-- )
		*rop = **pnames;
}

/* Get a password from a dictionary. */
/* Return 0 if present, 1 if absent, or an error code. */
private int
dict_password_param(const ref *pdict, const ref *pname, password *ppass)
{	ref *rpass;
	int code = dict_find(pdict, pname, &rpass);
	if ( code < 0 ) return 1;
	switch ( r_type(rpass) )
	{
	case t_integer:
		obj_cvs(rpass, &ppass->data[0], max_password, &ppass->size);
		break;
	case t_string:
	{	uint size = r_size(rpass);
		check_read(*rpass);
		if ( size > max_password )
			return_error(e_limitcheck);
		memcpy(&ppass->data[0], rpass->value.const_bytes, size);
		ppass->size = size;
	}	break;
	default:
		return_error(e_typecheck);
	}
	return 0;
}
