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

/* iinit.c */
/* Initialize internally known objects for Ghostscript interpreter */
#include "string_.h"
#include "ghost.h"
#include "alloc.h"
#include "dict.h"
#include "dstack.h"
#define INCLUDE_ERROR_NAMES		/* see errors.h */
#include "errors.h"
#include "ilevel.h"
#include "iname.h"
#include "oper.h"
#include "save.h"			/* for alloc_refs */
#include "store.h"

/* Define various parameters of this interpreter: */
const char *gs_copyright =
	"Copyright (C) 1990-1993 Aladdin Enterprises, Menlo Park, CA.\n";
const char *gs_product =
	"Ghostscript";
const int gs_revision =
	261;		/* release number x 100 + the sub-release. */
const long gs_revisiondate =
	930528;		/* year x 10000 + month x 100 + day. */
const long gs_serialnumber =
	42;		/* a well-known number */

/* Implementation parameters. */
/* The sizes of systemdict can be set in the makefile. */
/* We want the sizes to be prime numbers large enough to cover */
/* all the operators, plus everything in the init files, */
/* even if all the optional features are selected. */
#ifndef SYSTEMDICT_SIZE
#  define SYSTEMDICT_SIZE 479
#endif
#ifndef SYSTEMDICT_LEVEL2_SIZE
#  define SYSTEMDICT_LEVEL2_SIZE 547
#endif
/* The size of level2dict, if applicable, can be set in the makefile. */
#ifndef LEVEL2DICT_SIZE
#  define LEVEL2DICT_SIZE 109
#endif
/* The number of permanent dstack entries can be set in the makefile. */
#ifndef MIN_DSTACK_SIZE
#  define MIN_DSTACK_SIZE 2
#endif
#define op_array_table_size 100		/* arbitrary */

/* The dictionary that holds Level 2 definitions when not installed. */
ref ref_level2dict;
#define level2dict (&ref_level2dict)
/* Standard dictionaries */
ref name_errordict;
/* Error names */
ref name_ErrorNames;

/* The operator tables */
/* Because of a bug in Sun's SC1.0 compiler, */
/* we have to spell out the typedef for op_def_ptr here: */
const op_def _ds **op_def_table;
uint op_def_count;
ref op_array_table;	/* t_array, definitions of `operator' procedures */
ushort *op_array_nx_table;		/* name indices for same */
uint op_array_count;

/* Enter a name and value into systemdict */
void
initial_enter_string(const char *nstr, uint len, ref *pref)
{	ref nref;
	if ( name_ref((const byte *)nstr, len, &nref, 0) < 0 ||
	     dict_put(systemdict, &nref, pref) < 0
	   )
		lprintf("initial_enter failed!\n"),
		gs_exit(1);
}
void
initial_enter_name(const char *nstr, ref *pref)
{	initial_enter_string(nstr, strlen(nstr), pref);
}

/* Initialize the operators. */
/* Optional operators must come after standard ones, */
/* so they can replace them. */
	/* Non-graphics operators */
extern op_def
  zarith_op_defs[], zarray_op_defs[], zcontrol_op_defs[], zdict_op_defs[],
  zfile_op_defs[], zfiledev_op_defs[], zfileio_op_defs[],
  zfilter_op_defs[], zgeneric_op_defs[],
  zmath_op_defs[], zmisc_op_defs[], zpacked_op_defs[], zprops_op_defs[],
  zrelbit_op_defs[], zstack_op_defs[], zstring_op_defs[],
  ztype_op_defs[], zvmem_op_defs[],
	/* Graphics operators */
  zchar_op_defs[], zcolor_op_defs[], zdevice_op_defs[],
  zfont_op_defs[], zfont1_op_defs[], zfont2_op_defs[],
  zgstate_op_defs[], zht_op_defs[],
  zmatrix_op_defs[], zpaint_op_defs[], zpath_op_defs[],
  zpath2_op_defs[],
	/* Optional operators */
#define oper_(defs) defs[],
#define oper2_(defs) defs[],
#include "gconfig.h"
#undef oper_
#undef oper2_
	/* Interpreter operators */
  interp_op_defs[];
private op_def_ptr op_defs_all[] = {
	/* Non-graphics operators */
  zarith_op_defs, zarray_op_defs, zcontrol_op_defs, zdict_op_defs,
  zfile_op_defs, zfiledev_op_defs, zfileio_op_defs,
  zfilter_op_defs, zgeneric_op_defs,
  zmath_op_defs, zmisc_op_defs, zpacked_op_defs, zprops_op_defs,
  zrelbit_op_defs, zstack_op_defs, zstring_op_defs,
  ztype_op_defs, zvmem_op_defs,
	/* Graphics operators */
  zchar_op_defs, zcolor_op_defs, zdevice_op_defs,
  zfont_op_defs, zfont1_op_defs, zfont2_op_defs,
  zgstate_op_defs, zht_op_defs,
  zmatrix_op_defs, zpaint_op_defs, zpath_op_defs,
  zpath2_op_defs,
	/* Optional operators */
#define oper_(defs) defs,
#include "gconfig.h"
#undef oper_
	/* Interpreter operators */
  interp_op_defs,
	/* Optional Level 2 operators */
#define oper2_(defs) defs,
#include "gconfig.h"
#undef oper2_
	/* end marker */
  (op_def_ptr)0
};
/* Detect whether we have any Level 2 operators. */
#define have_level2\
  (op_defs_all[countof(op_defs_all) - 2] != interp_op_defs)

/* Initialize the stacks (in interp.c) */
extern void interp_init(P0());

/* Initialize objects other than operators */
void
obj_init(void)
{
	interp_init();

	/* Initialize the language level. */
	make_int(&ref_language_level, 1);

	/* Create the built-in dictionaries. */
	/* Only systemdict has non-zero maxlength. */
	dict_create((have_level2 ? SYSTEMDICT_LEVEL2_SIZE : SYSTEMDICT_SIZE),
		    systemdict);
	min_dstack_size = MIN_DSTACK_SIZE;
	{ int i;
	  for ( i = 1; i < MIN_DSTACK_SIZE; i++ )
	    dict_create(0, dsbot + i);
	}

	/* Initialize the predefined names other than operators */
	{	ref vtemp;
		make_null(&vtemp);
		initial_enter_name("null", &vtemp);
		make_const_string(&vtemp, a_readonly, strlen(gs_copyright),
				  (const byte *)gs_copyright);
		initial_enter_name("copyright", &vtemp);
		make_const_string(&vtemp, a_readonly, strlen(gs_product),
				  (const byte *)gs_product);
		initial_enter_name("product", &vtemp);
		make_int(&vtemp, gs_revision);
		initial_enter_name("revision", &vtemp);
		make_int(&vtemp, gs_revisiondate);
		initial_enter_name("revisiondate", &vtemp);
		initial_enter_name("systemdict", systemdict);
	}

	/* Create other system-known names */
	name_enter("errordict", &name_errordict);
	name_enter("ErrorNames", &name_ErrorNames);

	/* Create the error name table */
	   {	int n = sizeof(gs_error_names) / sizeof(char _ds *) - 1;
		int i;
		ref era;
		alloc_array(&era, a_readonly, n, "obj_init(ErrorNames)");
		for ( i = 0; i < n; i++ )
		  name_enter((char *)gs_error_names[i], era.value.refs + i);
		dict_put(systemdict, &name_ErrorNames, &era);
	   }
}

/* Run the initialization procedures of the individual operator files. */
void
zop_init(void)
{	op_def_ptr _ds *tptr;
	/* Because of a bug in Sun's SC1.0 compiler, */
	/* we have to spell out the typedef for op_def_ptr here: */
	const op_def _ds *def;
	for ( tptr = op_defs_all; *tptr != 0; tptr++ )
	   {	for ( def = *tptr; def->oname != 0; def++ ) ;
		if ( def->proc != 0 )
			((void (*)(P0()))(def->proc))();
	   }
}
/* Initialize the operator table. */
void
op_init(void)
{	int count = 1;
	int interp_count;
	op_def_ptr _ds *tptr;
	/* Because of a bug in Sun's SC1.0 compiler, */
	/* we have to spell out the typedef for op_def_ptr here: */
	const op_def _ds *def;
	const char _ds *nstr;

	/* Do a first pass just to count the operators. */
	/* Note the value of count after interp_op_defs, because */
	/* that is where the Level 2 operators start. */

	for ( tptr = op_defs_all; *tptr != 0; tptr ++ )
	{	for ( def = *tptr; def->oname != 0; count++, def++ )
			;
		if ( *tptr == interp_op_defs )
			interp_count = count;
	}
	
	if ( interp_count != count )
	{	/* Create level2dict for the Level 2 definitions. */
		dict_create(LEVEL2DICT_SIZE, level2dict);
		initial_enter_name("level2dict", level2dict);
	}

	/* Do a second pass to construct the operator table, */
	/* and enter the operators in systemdict or level2dict. */

	/* Because of a bug in Sun's SC1.0 compiler, */
	/* we have to spell out the typedef for op_def_ptr here: */
	op_def_table =
		(const op_def _ds **)alloc(count, sizeof(op_def_ptr),
					   "op_init(op_def_table)");
	op_def_count = count;
	count = 1;
	for ( tptr = op_defs_all; *tptr != 0; tptr ++ )
	 for ( def = *tptr; (nstr = def->oname) != 0; count++, def++ )
	   {	ref nref, oper;
		make_oper(&oper, count, (dummy_op_proc_p)(def->proc));
		interp_fix_op(&oper);		/* optimize if possible */
		/* The first character of the name is a digit */
		/* giving the minimum acceptable number of operands. */
		/* For now, we just skip over it. */
		nstr++;
		/* Don't enter internal operators into systemdict. */
		if ( *nstr == '%' )
			name_enter(nstr, &nref);
		else
		{	ref nref;
			ref *pdict =
			  (count >= interp_count ? level2dict : systemdict);
			name_enter(nstr, &nref);
			dict_put(pdict, &nref, &oper);
		}
		op_def_table[count] = def;
	   }

	/* Allocate the table for `operator' procedures. */

	alloc_array(&op_array_table, a_readonly, op_array_table_size,
		    "op_array table");
	refset_null(op_array_table.value.refs, op_array_table_size);
	op_array_nx_table =
	  (ushort *)alloc(op_array_table_size, sizeof(ushort),
			  "op_array nx table");
	op_array_count = 0;

}

/* Initialize variables that hold name constants. */
void
init_names(register const names_def _ds *pnd)
{	for ( ; pnd->vname != 0; pnd++ )
		name_enter(pnd->vname, pnd->pvref);
}
