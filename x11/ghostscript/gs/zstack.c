/* Copyright (C) 1989, 1991, 1992 Aladdin Enterprises.  All rights reserved.

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

/* zstack.c */
/* Operand stack operators for Ghostscript */
#include "memory_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "store.h"

/* <obj> pop - */
int
zpop(register os_ptr op)
{	check_op(1);
	pop(1);
	return 0;
}

/* <obj1> <obj2> exch <obj2> <obj1> */
int
zexch(register os_ptr op)
{	ref next;
	check_op(2);
	ref_assign_inline(&next, op - 1);
	ref_assign_inline(op - 1, op);
	ref_assign_inline(op, &next);
	return 0;
}

/* <obj> dup <obj> <obj> */
int
zdup(register os_ptr op)
{	check_op(1);
	push(1);
	ref_assign_inline(op, op - 1);
	return 0;
}

/* <obj_n> ... <obj_0> <n> index <obj_n> ... <obj_0> <obj_n> */
int
zindex(register os_ptr op)
{	register os_ptr opn;
	check_type(*op, t_integer);
	if ( (ulong)op->value.intval >= op - osbot )
		return_error(e_rangecheck);
	opn = op + ~(int)op->value.intval;
	ref_assign_inline(op, opn);
	return 0;
}

/* <obj_n-1> ... <obj_0> <n> <i> roll */
/*	<obj_(i-1)_mod_ n> ... <obj_0> <obj_n-1> ... <obj_i_mod_n> */
int
zroll(register os_ptr op)
{	os_ptr op1 = op - 1;
	int count, mod;
	register os_ptr from, to;
	register int n;
	check_type(*op1, t_integer);
	check_type(*op, t_integer);
	if ( (ulong)op1->value.intval > op1 - osbot )
		return_error(e_rangecheck);
	count = op1->value.intval;
	mod = op->value.intval;
	/*
	 * The elegant approach, requiring no extra space, would be to
	 * rotate the elements in chains separated by mod elements.
	 * Instead, we simply check to make sure there is enough space
	 * above op to do the roll in two block moves.
	 * Unfortunately, we can't count on memcpy doing the right thing
	 * in *either* direction.
	 */
	switch ( mod )
	{
	case 1:			/* common special case */
		pop(2);  op -= 2;
		for ( from = op, n = count; n--; from-- )
			ref_assign(from + 1, from);
		ref_assign(from + 1, op + 1);
		return 0;
	case -1:		/* common special case */
		pop(2);  op -= 2;
		to = op - count + 1;
		ref_assign(op + 1, to);
		for ( n = count; n--; to++ )
			ref_assign(to, to + 1);
		return 0;
	}
	if ( count <= 1 )
	{	pop(2);
		return 0;
	}
	if ( mod >= count ) mod %= count;
	else if ( mod < 0 )
	{	mod %= count;
		if ( mod < 0 ) mod += count;  /* can't assume % means mod! */
	}
	if ( mod <= count >> 1)
	{	 /* Move everything up, then top elements down. */
		if ( mod >= ostop - op )
			return_error(e_stackoverflow);
		pop(2);  op -= 2;
		for ( to = op + mod, from = op, n = count; n--; to--, from-- )
			ref_assign(to, from);
		memcpy((char *)(from + 1), (char *)(op + 1), mod * sizeof(ref));
	}
	else
	{	/* Move bottom elements up, then everything down. */
		mod = count - mod;
		if ( mod >= ostop - op )
			return_error(e_stackoverflow);
		pop(2);  op -= 2;
		to = op - count + 1;
		memcpy((char *)(op + 1), (char *)to, mod * sizeof(ref));
		for ( from = to + mod, n = count; n--; to++, from++ )
			ref_assign(to, from);
	}
	return 0;
}

/* |- ... clear |- */
/* The function name is changed, because the IRIS library has */
/* a function called zclear. */
int
zclear_stack(os_ptr op)
{	osp = osbot - 1;
	return 0;
}

/* |- <obj_n-1> ... <obj_0> count <obj_n-1> ... <obj_0> <n> */
int
zcount(register os_ptr op)
{	push(1);
	make_int(op, op - osbot);
	return 0;
}

/* - mark <mark> */
int
zmark(register os_ptr op)
{	push(1);
	make_t(op, t_mark);
	return 0;
}

/* <mark> ... cleartomark */
int
zcleartomark(register os_ptr op)
{	while ( op >= osbot )
	   {	if ( r_has_type(op, t_mark) )
		   {	osp = op - 1;
			return 0;
		   }
		op--;
	   }
	return_error(e_unmatchedmark);
}

/* <mark> <obj_n-1> ... <obj_0> counttomark */
/*	<mark> <obj_n-1> ... <obj_0> <n> */
int
zcounttomark(os_ptr op)
{	register os_ptr mp = op;
	while ( mp >= osbot )
	   {	if ( r_has_type(mp, t_mark) )
		   {	push(1);
			make_int(op, op - mp - 1);
			return 0;
		   }
		mp--;
	   }
	return_error(e_unmatchedmark);
}

/* ------ Initialization procedure ------ */

op_def zstack_op_defs[] = {
	{"0clear", zclear_stack},
	{"0cleartomark", zcleartomark},
	{"0count", zcount},
	{"0counttomark", zcounttomark},
	{"1dup", zdup},
	{"2exch", zexch},
	{"2index", zindex},
	{"0mark", zmark},
	{"1pop", zpop},
	{"2roll", zroll},
	op_def_end(0)
};
