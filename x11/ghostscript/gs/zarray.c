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

/* zarray.c */
/* Array operators for Ghostscript */
#include "memory_.h"
#include "ghost.h"
#include "alloc.h"
#include "errors.h"
#include "oper.h"
#include "packed.h"
#include "store.h"

/* The generic operators (copy, get, put, getinterval, putinterval, */
/* length, and forall) are implemented in zgeneric.c. */

/* <int> array <array> */
int
zarray(register os_ptr op)
{	uint size;
	int code;
	check_type(*op, t_integer);
	if ( op->value.intval < 0 ||
	     op->value.intval > max_ushort / sizeof(ref) - 1
	   )
		return_error(e_rangecheck);
	size = op->value.intval;
	code = alloc_array((ref *)op, a_all, size, "array");
	if ( code < 0 ) return code;
	refset_null(op->value.refs, size);
	return 0;
}

/* <array> aload <obj_0> ... <obj_n-1> <array> */
int
zaload(register os_ptr op)
{	ref aref;
	ref_assign(&aref, op);
	if ( !r_is_array(&aref) )
		return_error(e_typecheck);
	check_read(aref);
#define asize r_size(&aref)
	if ( asize > ostop - op )
		return_error(e_rangecheck);
	if ( r_has_type(&aref, t_array) )
		memcpy((char *)op, (const char *)aref.value.refs,
		       asize * sizeof(ref));
	else
	   {	register ushort i;
		const ref_packed *packed =
			(const ref_packed *)aref.value.packed;
		os_ptr pdest = op;
		for ( i = 0; i < asize; i++, pdest++ )
			packed_get(packed, pdest),
			packed = packed_next(packed);
	   }
	push(asize);
#undef asize
	ref_assign(op, &aref);
	return 0;
}

/* <obj_0> ... <obj_n-1> <array> astore <array> */
int
zastore(register os_ptr op)
{	uint size;
	int code;
	check_type(*op, t_array);
	check_write(*op);
	size = r_size(op);
	if ( size > op - osbot )
		return_error(e_stackunderflow);
	code = refcpy_to_old(op, 0, op - size, size, "astore");
	if ( code < 0 )
		return code;
	op[-(int)size] = *op;
	pop(size);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zarray_op_defs[] = {
	{"1aload", zaload},
	{"1array", zarray},
	{"1astore", zastore},
	op_def_end(0)
};
