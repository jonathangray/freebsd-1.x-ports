/* Copyright (C) 1989, 1990, 1991, 1992 Aladdin Enterprises.  All rights reserved.

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

/* zstring.c */
/* String operators for Ghostscript */
#include "memory_.h"
#include "ghost.h"
#include "stream.h"
#include "alloc.h"
#include "errors.h"
#include "iname.h"
#include "iscan.h"
#include "iutil.h"
#include "oper.h"
#include "store.h"

/* The generic operators (copy, get, put, getinterval, putinterval, */
/* length, and forall) are implemented in zgeneric.c. */

/* Imported operators */
extern int ztoken_file(P1(os_ptr));

/* <int> string <string> */
int
zstring(register os_ptr op)
{	byte *sbody;
	uint size;
	check_type(*op, t_integer);
	if ( op->value.intval < 0 || (ulong)(op->value.intval) > max_ushort )
		return_error(e_rangecheck);
	size = op->value.intval;
	sbody = (byte *)alloc(size, 1, "string");
	if ( sbody == 0 )
		return_error(e_VMerror);
	make_string(op, a_all, size, sbody);
	memset(sbody, 0, size);
	return 0;
}

/* <string> <pattern> anchorsearch <post> <match> true */
/* <string> <pattern> anchorsearch <string> false */
int
zanchorsearch(register os_ptr op)
{	os_ptr op1 = op - 1;
	uint size = r_size(op);
	check_read_type(*op1, t_string);
	check_read_type(*op, t_string);
	if ( size <= r_size(op1) && !memcmp(op1->value.bytes, op->value.bytes, size) )
	{	os_ptr op0 = op;
		push(1);
		*op0 = *op1;
		r_set_size(op0, size);
		op1->value.bytes += size;
		r_inc_size(op1, -size);
		make_bool(op, 1);
	}
	else
		make_bool(op, 0);
	return 0;
}

/* <string> <pattern> search <post> <match> <pre> true */
/* <string> <pattern> search <string> false */
int
zsearch(register os_ptr op)
{	os_ptr op1 = op - 1;
	uint size = r_size(op);
	uint count;
	byte *ptr;
	check_read_type(*op1, t_string);
	check_read_type(*op, t_string);
	if ( size > r_size(op1) )		/* can't match */
	   {	make_bool(op, 0);
		return 0;
	   }
	count = r_size(op1) - size;
	ptr = op1->value.bytes;
	do
	   {	if ( !memcmp(ptr, op->value.bytes, size) )
		   {	op->tas.type_attrs = op1->tas.type_attrs;
			op->value.bytes = ptr;
			r_set_size(op, size);
			push(2);
			op[-1] = *op1;
			r_set_size(op - 1, ptr - op[-1].value.bytes);
			op1->value.bytes = ptr + size;
			r_set_size(op1, count);
			make_bool(op, 1);
			return 0;
		   }
		ptr++;
	   }
	while ( count-- );
	/* No match */
	make_bool(op, 0);
	return 0;
}

/* <obj> <pattern> .stringmatch <bool> */
int
zstringmatch(register os_ptr op)
{	os_ptr op1 = op - 1;
	int result;
	check_read_type(*op, t_string);
	switch ( r_type(op1) )
	   {
	case t_string:
		check_read(*op1);
		goto cmp;
	case t_name:
		name_string_ref(op1, op1);		/* can't fail */
cmp:		result = string_match(op1->value.const_bytes, r_size(op1),
				      op->value.const_bytes, r_size(op),
				      NULL);
		break;
	default:
		result = (r_size(op) == 1 && *op->value.bytes == '*');
	   }
	make_bool(op1, result);
	pop(1);
	return 0;
}

/* <string|file> token <post> <obj> true */
/* <string|file> token false */
int
ztoken(register os_ptr op)
{	stream st;
	stream *s = &st;
	int code;
	ref token;
	switch ( r_type(op) )
	   {
	default:
		return_error(e_typecheck);
	case t_file:
		return ztoken_file(op);
	case t_string: ;
	   }
	check_read(*op);
	sread_string(s, op->value.bytes, r_size(op));
	switch ( code = scan_token(s, 1, &token) )
	   {
	default:			/* possible error */
		if ( code < 0 ) return code;
					/* read a token */
	   {	uint pos = stell(s);
		op->value.bytes += pos;
		r_inc_size(op, -pos);
	   }
		push(2);
		op[-1] = token;
		make_bool(op, 1);
		return 0;
	case scan_EOF:			/* no tokens */
		make_bool(op, 0);
		return 0;
	   }
}

/* ------ Initialization procedure ------ */

op_def zstring_op_defs[] = {
	{"2anchorsearch", zanchorsearch},
	{"2search", zsearch},
	{"1string", zstring},
	{"2.stringmatch", zstringmatch},
	{"1token", ztoken},
	op_def_end(0)
};
