/* Copyright (C) 1990, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* zbseq.c */
/* Level 2 binary object sequence operators */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "save.h"
#include "store.h"
#include "stream.h"
#include "files.h"
#include "iname.h"
#include "bnum.h"
#include "btoken.h"
#include "bseq.h"

/* Current binary format (in iscan.c) */
extern ref ref_binary_object_format;

/* System and user name arrays. */
ref system_names, user_names;

/* Import the Level 2 scanner extensions. */
extern int scan_binary_token(P3(stream *, ref *, int));
extern int (*scan_btoken_proc)(P3(stream *, ref *, int));
extern void scan_ascii85_setup(P4(stream *, stream *, byte *, uint));
extern void (*scan_ascii85_setup_proc)(P4(stream *, stream *, byte *, uint));

/* Initialize the Level 2 scanning machinery. */
private void
zbseq_init(void)
{	/* Initialize fake system and user name tables. */
	/* PostScript code will install the real ones. */
	make_tasv(&system_names, t_shortarray, a_readonly, 0, packed, NULL);
	make_tasv(&user_names, t_array, a_all, 0, refs, NULL);
	scan_btoken_proc = scan_binary_token;
	scan_ascii85_setup_proc = scan_ascii85_setup;
}

/* <system_names> <user_names> .installnames - */
int
zinstallnames(register os_ptr op)
{	check_read_type(op[-1], t_shortarray);
	check_type(*op, t_array);
	ref_assign_old(&system_names, op - 1, ".installnames");
	ref_assign_old(&user_names, op, ".installnames");
	pop(2);
	return 0;
}

/* - currentobjectformat <int> */
int
zcurrentobjectformat(register os_ptr op)
{	push(1);
	*op = ref_binary_object_format;
	return 0;
}

/* <int> setobjectformat - */
int
zsetobjectformat(register os_ptr op)
{	check_type(*op, t_integer);
	if ( op->value.intval < 0 || op->value.intval > 4 )
		return_error(e_rangecheck);
	ref_assign_old(&ref_binary_object_format, op, "setobjectformat");
	pop(1);
	return 0;
}

/* <file> <tag> <array> .writeobjects - */
/* Internal definitions */
typedef struct { ulong refs, chars; } bin_space;
private int bin_seq_push(P2(os_ptr, bin_space *));
private void bin_seq_write_objects(P6(stream *, const_os_ptr, const_os_ptr,
  byte, ulong, ulong));
private void bin_seq_write_strings(P3(stream *, const_os_ptr, const_os_ptr));
int
zwriteobjects(register os_ptr op)
{	stream *s;
	int code;
	int bin_format = (int)ref_binary_object_format.value.intval - 1;
	bin_space space;
	ulong total;
	byte tag;
	os_ptr op1 = op - 1;
	os_ptr top;
	ulong apos;
	static const int nfs[4] =
	   {	num_float_IEEE + num_msb,
		num_float_IEEE + num_lsb,
		num_float_native + num_msb,
		num_float_native + num_lsb
	   };
	if ( bin_format < 0 )
		return_error(e_undefined);
	check_write_file(s, op - 2);
	check_type(*op1, t_integer);
	if ( op1->value.intval < 0 || op1->value.intval > 255 )
		return_error(e_rangecheck);
	tag = (byte)op1->value.intval;
	check_read_type(*op, t_array);
	space.refs = space.chars = 0;
	code = bin_seq_push(op, &space);
	if ( code < 0 ) return code;
	top = op + code;
	/* Object has been validated, only possible error now is */
	/* ioerror (which we don't check for). */
	total = space.refs * sizeof(bin_seq_obj) + space.chars;
	apos = r_size(op) * (ulong)sizeof(bin_seq_obj);
	s->num_format = nfs[bin_format];
	sputc(s, (byte)bt_seq + bin_format);
	if ( total > 0xffff - 4 )		/* use long format */
	{	sputc(s, 0);
		sputshort(s, r_size(op));
		sputlong(s, total + 8);
	}
	else					/* use short format */
	{	sputc(s, (byte)r_size(op));
		sputshort(s, (ushort)total + 4);
	}
	bin_seq_write_objects(s, op, top, tag, apos, total - space.chars);
	bin_seq_write_strings(s, op, top);
	pop(3);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zbseq_op_defs[] = {
	{"2.installnames", zinstallnames},
	{"0currentobjectformat", zcurrentobjectformat},
	{"1setobjectformat", zsetobjectformat},
	{"3.writeobjects", zwriteobjects},
	op_def_end(zbseq_init)
};

/* ------ Internal routines ------ */

/* Recursively scan arrays breadth-first and push onto the stack. */
/* Compute the space requirements at the same time. */
/* Return the number of arrays on the stack, but don't change osp. */
private int
bin_seq_push(os_ptr op0, bin_space *bsp)
{	register os_ptr op = op0;
	os_ptr top = op;
	while ( op <= top )
	{	uint i = r_size(op);
		const ref *ep = op->value.const_refs;
		bsp->refs += i;
		for ( ; i; i--, ep++ )
		  switch ( r_type(ep) )
		{
		case t_null: case t_integer: case t_real:
		case t_boolean: case t_mark:
			break;
		case t_string:
			check_read(*ep);
			bsp->chars += r_size(ep);
			break;
		case t_name:
		{	ref nstr;
			name_string_ref(ep, &nstr);
			bsp->chars += r_size(&nstr);
		}	break;
		case t_array:
		{	check_read(*ep);
			if ( top == ostop )
				return_error(e_limitcheck);
			top++;
			ref_assign(top, ep);
		}	break;
		default:
			return_error(e_typecheck);
		}
		op++;
	}
	return top - op0;
}

/* Write the objects part of a binary object sequence. */
private void
bin_seq_write_objects(stream *s, const_os_ptr op, const_os_ptr top,
  byte tag, ulong apos, ulong spos)
{	bin_seq_obj ob;
	ref nstr;
	ob.unused = tag;
#define swap_t(a, b) t = a, a = b, b = t
#if arch_is_big_endian
#  define must_swap(s) s_is_lsb(s)
#else
#  define must_swap(s) s_is_msb(s)
#endif
	for ( ; op <= top; op++ )
	{	uint i = r_size(op);
		const ref *ep = op->value.const_refs;
		for ( ; i; i--, ep++ )
		{	switch ( r_type(ep) )
			{
			case t_null:
				ob.tx = (byte)bs_null;
				break;
			case t_mark:
				ob.tx = (byte)bs_mark;
				break;
			case t_integer:
				ob.tx = (byte)bs_integer;
				ob.value.w = ep->value.intval;
num:				ob.size.w = 0;	/* (matters for reals) */
swb:				/* swap bytes of value if needed */
				if ( must_swap(s) )
				{ byte t;
				  swap_t(ob.value.b[0], ob.value.b[3]);
				  swap_t(ob.value.b[1], ob.value.b[2]);
				}
				break;
			case t_real:
				ob.tx = (byte)bs_real;
				ob.value.f = ep->value.realval;
				/***** handle non-IEEE native *****/
				goto num;
			case t_boolean:
				ob.tx = (byte)bs_boolean;
				ob.value.w = ep->value.index;
				goto num;
			case t_array:
				ob.tx = (byte)bs_array;
				if ( r_has_attr(ep, a_executable) )
					ob.tx += (byte)bs_executable;
				ob.size.w = r_size(ep);
				ob.value.w = apos;
				apos += ob.size.w * (ulong)sizeof(bin_seq_obj);
				goto nsa;
			case t_string:
				ob.tx = (byte)bs_string;
				if ( r_has_attr(ep, a_executable) )
					ob.tx += (byte)bs_executable;
				ob.size.w = r_size(ep);
nos:				ob.value.w = spos;
				spos += ob.size.w;
nsa:				if ( must_swap(s) )
				{ byte t;
				  swap_t(ob.size.b[0], ob.size.b[1]);
				}
				goto swb;
			case t_name:
				ob.tx = (byte)bs_name;
				name_string_ref(ep, &nstr);
				ob.size.w = r_size(&nstr);
				goto nos;
			}
			sputs(s, (byte *)&ob, sizeof(bin_seq_obj));
			ob.unused = 0;		/* tag first object only */
		}
	}
}

/* Write the string part of a binary object sequence */
private void
bin_seq_write_strings(stream *s, const_os_ptr op, const_os_ptr top)
{	for ( ; op <= top; op++ )
	{	uint i = r_size(op);
		const ref *ep = op->value.const_refs;
		for ( ; i; i--, ep++ )
		  switch ( r_type(ep) )
		{
		case t_name:
		{	ref nstr;
			name_string_ref(ep, &nstr);
			sputs(s, nstr.value.bytes, r_size(&nstr));
		}	break;
		case t_string:
			sputs(s, ep->value.bytes, r_size(ep));
			break;
		}
	}
}
