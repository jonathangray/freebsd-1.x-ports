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

/* zgeneric.c */
/* Array/string/dictionary generic operators for PostScript */
#include "memory_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "dict.h"
#include "estack.h"	/* for forall */
#include "iname.h"
#include "ivmspace.h"
#include "packed.h"
#include "store.h"

/* This file implements copy, get, put, getinterval, putinterval, */
/* length, and forall, which apply generically to */
/* arrays, strings, and dictionaries.  (Copy also has a special */
/* meaning for copying the top N elements of the stack.) */

/* See the comment in opdef.h for an invariant which allows */
/* more efficient implementation of forall. */

/* Imported operators */
extern int zcopy_dict(P1(os_ptr));

/* Forward references */
private int zcopy_invalid(P1(os_ptr));
private int zcopy_integer(P1(os_ptr));
private int zcopy_interval(P1(os_ptr));
private int copy_interval(P4(os_ptr, uint, os_ptr, const char *));

/* The type dispatch table for `copy'. */
/* We export it so that Level 2 can extend it to handle gstates. */
op_proc_p zcopy_procs[t_next_index];

/* Initialize the generic dispatch table. */
private void
zgeneric_init(void)
{	int i;
	for ( i = 0; i < t_next_index; i++ )
		zcopy_procs[i] = zcopy_invalid;
	zcopy_procs[t_integer] = zcopy_integer;
	zcopy_procs[t_array] = zcopy_procs[t_string] = zcopy_interval;
	zcopy_procs[t_dictionary] = zcopy_dict;
}

/* <various1> <various2> copy <various> */
/* <obj1> ... <objn> <int> copy <obj1> ... <objn> <obj1> ... <objn> */
/* Note that this implements copy for arrays and strings, */
/* but not for dictionaries (see zcopy_dict in zdict.c). */
int
zcopy(register os_ptr op)
{	int type = r_type(op);
	if ( type >= t_next_index )
		return_error(e_typecheck);
	return (*zcopy_procs[type])(op);
}
/* <other> copy */
private int
zcopy_invalid(os_ptr op)
{	return_error(e_typecheck);
}
/* <obj1> ... <objn> <int> copy <obj1> ... <objn> <obj1> ... <objn> */
private int
zcopy_integer(register os_ptr op)
{	os_ptr op1 = op - 1;
	int count;
	if ( (ulong)op->value.intval > op - osbot )
		return_error(e_rangecheck);
	count = op->value.intval;
	if ( op1 + count > ostop )
		return_error(e_stackoverflow);
	memcpy((char *)op, (char *)(op - count), count * sizeof(ref));
	push(count - 1);
	return 0;
}
/* <array1> <array2> copy <subarray2> */
/* <string1> <string2> copy <substring2> */
private int
zcopy_interval(register os_ptr op)
{	os_ptr op1 = op - 1;
	int code = copy_interval(op, 0, op1, "copy");
	if ( code < 0 ) return code;
	r_set_size(op, r_size(op1));
	*op1 = *op;
	pop(1);
	return 0;
}

/* <array|dict|name|packedarray|string> length <int> */
int
zlength(register os_ptr op)
{	switch ( r_type(op) )
	   {
	case t_array:
	case t_string:
	case t_mixedarray:
	case t_shortarray:
		check_read(*op);
		make_int(op, r_size(op));
		return 0;
	case t_dictionary:
		check_dict_read(*op);
		make_int(op, dict_length(op));
		return 0;
	case t_name:
	   {	ref str;
		name_string_ref(op, &str);
		make_int(op, r_size(&str));
	   }
		return 0;
	default:
		return_error(e_typecheck);
	   }
}

/* <array|packedarray|string> <index> get <obj> */
/* <dict> <key> get <obj> */
int
zget(register os_ptr op)
{	os_ptr op1 = op - 1;
	ref *pvalue;
	switch ( r_type(op1) )
	   {
	case t_dictionary:
		check_dict_read(*op1);
		if ( dict_find(op1, op, &pvalue) <= 0 )
			return_error(e_undefined);
		op[-1] = *pvalue;
		break;
	case t_string:
		check_type(*op, t_integer);
		check_read(*op1);
		if ( (ulong)(op->value.intval) >= r_size(op1) )
			return_error(e_rangecheck);
		make_int(op1, op1->value.bytes[(uint)op->value.intval]);
		break;
	default:
	   {	int code;
		check_type(*op, t_integer);
		check_read(*op1);
		code = array_get(op1, op->value.intval, op1);
		if ( code < 0 ) return code;
	   }
	   }
	pop(1);
	return 0;
}

/* <array> <index> <obj> put - */
/* <dict> <key> <value> put - */
/* <string> <index> <int> put - */
int
zput(register os_ptr op)
{	os_ptr op1 = op - 1;
	os_ptr op2 = op1 - 1;
	switch ( r_type(op2) )
	   {
	case t_dictionary:
		check_dict_write(*op2);
	   {	int code = dict_put(op2, op1, op);
		if ( code ) return code;	/* error */
	   }
		break;
	case t_array:
		check_type(*op1, t_integer);
		check_write(*op2);
		if ( (ulong)(op1->value.intval) >= r_size(op2) )
			return_error(e_rangecheck);
		if ( !r_is_global(op) && r_is_global(op2) )
			return_error(e_invalidaccess);
	   {	ref *eltp = op2->value.refs + (uint)op1->value.intval;
		ref_assign_old(eltp, op, "put");
	   }	break;
	case t_mixedarray:		/* packed arrays are read-only */
	case t_shortarray:
		return_error(e_invalidaccess);
	case t_string:
		check_type(*op1, t_integer);
		check_write(*op2);
		if ( (ulong)(op1->value.intval) >= r_size(op2) )
			return_error(e_rangecheck);
		check_type(*op, t_integer);
		if ( (ulong)op->value.intval > 0xff )
			return_error(e_rangecheck);
		op2->value.bytes[(uint)op1->value.intval] = (byte)op->value.intval;
		break;
	default:
		return_error(e_typecheck);
	   }
	pop(3);
	return 0;
}

/* <seq:array|packedarray|string> <index> <count> getinterval <subseq> */
int
zgetinterval(register os_ptr op)
{	os_ptr op1 = op - 1;
	os_ptr op2 = op1 - 1;
	uint index;
	uint count;
	check_type(*op1, t_integer);
	check_type(*op, t_integer);
	switch ( r_type(op2) )
	   {
	default:
		return_error(e_typecheck);
	case t_array: case t_string:
	case t_mixedarray:
	case t_shortarray: ;
	   }
	check_read(*op2);
	if ( (ulong)op1->value.intval > r_size(op2) )
		return_error(e_rangecheck);
	index = op1->value.intval;
	if ( (ulong)op->value.intval > r_size(op2) - index )
		return_error(e_rangecheck);
	count = op->value.intval;
	switch ( r_type(op2) )
	   {
	case t_array: op2->value.refs += index; break;
	case t_string: op2->value.bytes += index; break;
	case t_mixedarray:
	   {	const ref_packed *packed = op2->value.packed;
		for ( ; index--; ) packed = packed_next(packed);
		op2->value.packed = packed;
	   }	break;
	case t_shortarray: op2->value.packed += index; break;
	   }
	r_set_size(op2, count);
	pop(2);
	return 0;
}

/* <array1> <index> <array2|packedarray2> putinterval - */
/* <string1> <index> <string2> putinterval - */
int
zputinterval(register os_ptr op)
{	os_ptr opindex = op - 1;
	os_ptr opto = opindex - 1;
	int code;
	check_type(*opindex, t_integer);
	switch ( r_type(opto) )
	   {
	default:
		return_error(e_typecheck);
	case t_mixedarray: case t_shortarray:
		return_error(e_invalidaccess);
	case t_array: case t_string: ;
	   }
	check_write(*opto);
	if ( (ulong)opindex->value.intval > r_size(opto) )
		return_error(e_rangecheck);
	code = copy_interval(opto, (uint)(opindex->value.intval), op, "putinterval");
	if ( code >= 0 ) pop(3);
	return code;
}

/* <array|packedarray|string> <<element> proc> forall - */
/* <dict> <<key> <value> proc> forall - */
private int
  array_continue(P1(os_ptr)),
  dict_continue(P1(os_ptr)),
  string_continue(P1(os_ptr)),
  packedarray_continue(P1(os_ptr));
private int forall_cleanup(P1(os_ptr));
int
zforall(register os_ptr op)
{	os_ptr obj = op - 1;
	register es_ptr ep = esp;
	uint index = 0;			/* only used for dictionaries */
	check_estack(6);
#define cproc (ep + 5)
	switch ( r_type(obj) )
	   {
	default:
		return_error(e_typecheck);
	case t_array:
		check_read(*obj);
		make_op_estack(cproc, array_continue);
		break;
	case t_dictionary:
		check_dict_read(*obj);
		make_op_estack(cproc, dict_continue);
		index = dict_first(obj);
		break;
	case t_string:
		check_read(*obj);
		make_op_estack(cproc, string_continue);
		break;
	case t_mixedarray:
	case t_shortarray:
		check_read(*obj);
		make_op_estack(cproc, packedarray_continue);
		break;
	   }
	check_proc(*op);
	/* Push a mark, the composite object, the iteration index, */
	/* and the procedure, and invoke the continuation operator. */
	make_mark_estack(ep + 1, es_for, forall_cleanup);
	ep[2] = *obj;
	make_int(ep + 3, index);
	ep[4] = *op;
	esp += 4;
	pop(2);  op -= 2;
	return (*real_opproc(cproc))(op);
#undef cproc
}
/* Continuation operator for arrays */
private int
array_continue(register os_ptr op)
{	es_ptr obj = esp - 2;
	if ( r_size(obj) )		/* continue */
	   {	push(1);
		r_inc_size(obj, -1);
		*op = *obj->value.refs;
		obj->value.refs++;
		esp += 2;
		*esp = obj[2];
		return o_push_estack;
	   }
	else				/* done */
	   {	esp -= 4;		/* pop mark, object, index, proc */
		return o_pop_estack;
	   }
}
/* Continuation operator for dictionaries */
private int
dict_continue(register os_ptr op)
{	es_ptr obj = esp - 2;
	int index = (int)esp[-1].value.intval;
	push(2);			/* make room for key and value */
	if ( (index = dict_next(obj, index, op - 1)) >= 0 )	/* continue */
	   {	esp[-1].value.intval = index;
		esp += 2;
		*esp = obj[2];
		return o_push_estack;
	   }
	else				/* done */
	   {	pop(2);			/* undo push */
		esp -= 4;		/* pop mark, object, index, proc */
		return o_pop_estack;
	   }
}
/* Continuation operator for strings */
private int
string_continue(register os_ptr op)
{	es_ptr obj = esp - 2;
	if ( r_size(obj) )		/* continue */
	   {	r_inc_size(obj, -1);
		push(1);
		make_int(op, *obj->value.bytes);
		obj->value.bytes++;
		esp += 2;
		*esp = obj[2];
		return o_push_estack;
	   }
	else				/* done */
	   {	esp -= 4;		/* pop mark, object, index, proc */
		return o_pop_estack;
	   }
}
/* Continuation operator for packed arrays */
private int
packedarray_continue(register os_ptr op)
{	es_ptr obj = esp - 2;
	if ( r_size(obj) )		/* continue */
	   {	const ref_packed *packed = obj->value.packed;
		r_inc_size(obj, -1);
		push(1);
		packed_get(packed, op);
		obj->value.packed = packed_next(packed);
		esp += 2;
		*esp = obj[2];
		return o_push_estack;
	   }
	else				/* done */
	   {	esp -= 4;		/* pop mark, object, index, proc */
		return o_pop_estack;
	   }
}
/* Vacuous cleanup procedure */
private int
forall_cleanup(os_ptr op)
{	return 0;
}

/* ------ Initialization procedure ------ */

op_def zgeneric_op_defs[] = {
	{"1copy", zcopy},
	{"2forall", zforall},
	{"2get", zget},
	{"3getinterval", zgetinterval},
	{"1length", zlength},
	{"3put", zput},
	{"3putinterval", zputinterval},
		/* Internal operators */
	{"0%array_continue", array_continue},
	{"0%dict_continue", dict_continue},
	{"0%packedarray_continue", packedarray_continue},
	{"0%string_continue", string_continue},
	op_def_end(zgeneric_init)
};

/* ------ Shared routines ------ */

/* Copy an interval from one operand to another. */
/* This is used by both putinterval and string/array copy. */
/* One operand is known to be an array or string, */
/* and the starting index is known to be less than or equal to */
/* its length; nothing else has been checked. */
private int
copy_interval(os_ptr prto, uint index, os_ptr prfrom, const char *cname)
{	int fromtype = r_type(prfrom);
	uint fromsize = r_size(prfrom);
	if ( !(fromtype == r_type(prto) || (fromtype == t_shortarray
		|| fromtype == t_mixedarray) && r_type(prto) == t_array)
	   )
		return_error(e_typecheck);
	check_read(*prfrom);
	check_write(*prto);
	if ( fromsize > r_size(prto) - index )
		return_error(e_rangecheck);
	switch ( fromtype )
	   {
	case t_array:
		return refcpy_to_old(prto, index, prfrom->value.refs,
				     fromsize, cname);
	case t_string:
		memcpy(prto->value.bytes + index, prfrom->value.bytes,
		       fromsize);
		break;
	case t_mixedarray:
	case t_shortarray:
	   {	int i;
		const ref_packed *packed = prfrom->value.packed;
		ref *pdest = prto->value.refs + index;
		ref elt;
		for ( i = 0; i < fromsize; i++, pdest++ )
		 { packed_get(packed, &elt);
		   ref_assign_old(pdest, &elt, cname);
		   packed = packed_next(packed);
		 }
	   }	break;
	   }
	return 0;
}
