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

/* iref.h */
/* Object structure and type definitions for Ghostscript */

/* The typedef for object references */
typedef struct ref_s ref;

/*
 * Object types.  The types marked with + use the read/write/execute
 * attributes; the rest only use the executable attribute.
 * The types marked with # are composite and hence use the a_local attribute;
 * objects of all other types must have a_local cleared.
 */
typedef enum {

		/* NOTE: The first 4 types must be kept together */
		/* for the sake of r_is_array and r_is_proc (see below). */

#define _t_array_span 4
	t_array,		/* # + value.refs, uses size */
		/* The following are the two implementations of */
		/* packed arrays. */
	t_mixedarray,		/* # + value.packed, uses size */
	t_shortarray,		/* # + value.packed, uses size */
	  t_unused_array_,	/*     (an unused array type) */

	t_boolean,		/*     value.index */
	t_condition,		/* #   value.pcond */
	t_dictionary,		/* # + value.pdict */
	t_file,			/* # + value.pfile, uses size for id */
	t_fontID,		/* #   value.pfont */
	t_gstate,		/* #   value.pgstate */
	t_integer,		/*     value.intval */
	t_lock,			/* #   value.plock */
	t_mark,			/*       (no value) */
	t_name,			/*     value.pname, uses size for index */
	t_null,			/*     (value.opproc, uses size for mark */
				/*       type, on e-stack only) */
	t_operator,		/*     value.opproc, uses size for index */
	t_real,			/*     value.realval */
	t_save,			/* #   value.psave */
	t_string,		/* # + value.bytes, uses size */
/*
 * The following are extensions to the PostScript type set.
 * When adding new types, be sure to edit the table in gs_init.ps
 * (==only operator), the printing routine in idebug.c, the dispatch
 * in interp.c, obj_eq in iutil.c, restore_check_stack in zvmem.c,
 * and also type_name_strings and type_print_strings below.
 */
	t_device,		/* #    value.pdevice */
	t_oparray,		/* #      (no value), uses size for index */
	t_next_index	/*** first available index ***/
} ref_type;
/*
 * The interpreter uses types starting at t_next_index for representing
 * a few high-frequency operators.
 * Since there are no operations specifically on operators,
 * there is no need for any operators to check specifically for these
 * types.  The r_btype macro takes care of the conversion when required.
 */
extern const int tx_next_index;
/*
 * Define the types that use the size field.
 */
#define case_types_with_size\
  case t_array: case t_file: case t_name: case t_operator: case t_string:\
  case t_mixedarray: case t_shortarray: case t_oparray
/*
 * Define the type names for debugging printout.
 * All names must be the same length, so that columns will line up.
 */
#define type_print_strings\
  "arry","mpry","spry","u?ry",\
  "bool","cond","dict","file",\
  "font","gsta","int ","lock","mark",\
  "name","null","oper","real","save",\
  "str ","devc","opry"
/*
 * Define the type names for the type operator.
 */
#define type_name_strings\
  "arraytype","packedarraytype","packedarraytype","arraytype",\
  "booleantype","conditiontype","dicttype","filetype",\
  "fonttype","gstatetype","integertype","locktype","marktype",\
  "nametype","nulltype","operatortype","realtype","savetype",\
  "stringtype","devicetype","operatortype"

/*
 * The following factors affect the encoding of attributes:
 *
 *	- The packed array format requires the high-order bits of the
 *	  type/attributes field to be 0.  (see packed.h)
 *
 *	- The interpreter wants the type, executable bit, and execute
 *	  permission to be adjacent, and in that order from high to low.
 *
 *	- Type testing is most efficient if the type is in a byte by itself.
 *
 * The layout given below results in the most efficient code overall.
 */

/* Location attributes. */
/* Note that these are associated with the *location*, not with the */
/* ref that is *stored* in that location. */
#define l_mark 2			/* mark for garbage collector */
					/* (not used yet) */
#define l_new 4				/* stored into since last save */
/* Attributes visible at the PostScript language level. */
#define a_local 8			/* object allocated in local VM */
#define a_write 0x10
#define a_read 0x20
#define a_execute 0x40
#define a_executable 0x80
#define a_readonly (a_read+a_execute)
#define a_all (a_write+a_read+a_execute)
#define r_type_shift 8
#define r_type_bits 6

/* Define the attribute names for debugging printout. */
#define attr_print_string "?mnlwrxe......??"

/* Abstract types */
typedef struct dict_s dict;
typedef struct name_s name;
/* We define a dummy type for op_proc_p so that */
/* we don't have to include oper.h. */
typedef int (*dummy_op_proc_p)();
#define real_opproc(pref) (*(op_proc_p *)&(pref)->value)

/* Object reference */
/*
 * Note that because of the way packed arrays are represented,
 * the type_attrs member must be the first one in the ref structure.
 */
struct stream_s;
struct gs_font_s;
struct gs_condition_s;
struct gs_lock_s;
struct gx_device_s;
struct gstate_obj_s;
struct vm_save_s;
struct tas_s {
	ushort type_attrs;
	ushort rsize;
};
struct ref_s {

	struct tas_s tas;

#define r_size(rp) ((rp)->tas.rsize)
#define r_inc_size(rp,inc) ((rp)->tas.rsize += (inc))
#define r_set_size(rp,siz) ((rp)->tas.rsize = (siz))
/* type_attrs is a single element for fast dispatching in the interpreter */
#if r_type_shift == 8
#  if arch_is_big_endian
#    define r_type(rp) (((byte *)&((rp)->tas.type_attrs))[sizeof(ushort)-2])
#  else
#    define r_type(rp) (((byte *)&((rp)->tas.type_attrs))[1])
#  endif
#  define r_has_type(rp,typ) (r_type(rp) == (typ))
#else
#  define r_type(rp) ((rp)->tas.type_attrs >> r_type_shift)
#  define r_has_type(rp,typ) r_has_type_attrs(rp,typ,0)	/* see below */
#endif
/* A special macro for testing arrayhood. */
#define r_is_array(rp) _r_has_masked_type_attrs(rp,t_array,_t_array_span,0)
#define r_set_type(rp,typ) ((rp)->tas.type_attrs = (typ) << r_type_shift)
#define r_btype(rp)\
 ((rp)->tas.type_attrs >= (t_next_index << r_type_shift) ?\
  t_operator : r_type(rp))
#define type_xe(tas) ((tas) >> (r_type_shift - 2))
#define r_type_xe(rp) type_xe((rp)->tas.type_attrs)
#define type_xe_value(t,xe) type_xe(((t) << r_type_shift) + (xe))
#define r_type_attrs(rp) ((rp)->tas.type_attrs)	/* reading only */
#define r_has_attrs(rp,mask) !(~r_type_attrs(rp) & (mask))
#define r_has_attr(rp,mask1)		/* optimize 1-bit case */\
   (r_type_attrs(rp) & (mask1))
/* The following macro is not for external use. */
#define _r_has_masked_type_attrs(rp,typ,tspan,mask)\
 (((rp)->tas.type_attrs &\
   ((((1 << r_type_bits) - (tspan)) << r_type_shift) + (mask))) ==\
  (((typ) << r_type_shift) + (mask)))
#define r_has_type_attrs(rp,typ,mask)\
  _r_has_masked_type_attrs(rp,typ,1,mask)
/* A special macro for testing procedurehood. */
#define r_is_proc(rp)\
  _r_has_masked_type_attrs(rp,t_array,_t_array_span,a_execute+a_executable)
#define r_set_attrs(rp,mask) ((rp)->tas.type_attrs |= (mask))
#define r_clear_attrs(rp,mask) ((rp)->tas.type_attrs &= ~(mask))
#define r_set_type_attrs(rp,typ,mask)\
 ((rp)->tas.type_attrs = ((typ) << r_type_shift) + (mask))

	union v {			/* name the union to keep gdb happy */
		long intval;
		ushort index;		/* for enumerated things */
		float realval;
		byte *bytes;
		const byte *const_bytes;
		ref *refs;
		const ref *const_refs;
		name *pname;
		const name *const_pname;
		dict *pdict;
		const dict *const_pdict;
		const ushort /*ref_packed*/ *packed;
		dummy_op_proc_p opproc;
		struct stream_s *pfile;
		struct gs_font_s *pfont;
		struct gs_condition_s *pcond;
		struct gs_lock_s *plock;
		struct gx_device_s *pdevice;
		struct gs_state_s *pgstate;
		struct vm_save_s *psave;
	} value;
};
