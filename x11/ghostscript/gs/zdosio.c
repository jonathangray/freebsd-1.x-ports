/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.

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

/* zdosio.c */
/* DOS direct I/O operators for Ghostscript. */
/* These should NEVER be included in a released configuration! */
#include "dos_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "store.h"

/* <port> inport <word> */
private int
zinport(register os_ptr op)
{	check_type(*op, t_integer);
	make_int(op, inport((int)op->value.intval));
	return 0;
}

/* <port> inportb <byte> */
private int
zinportb(register os_ptr op)
{	check_type(*op, t_integer);
	make_int(op, inportb((int)op->value.intval));
	return 0;
}

/* <port> <word> outport - */
private int
zoutport(register os_ptr op)
{	check_type(*op, t_integer);
	check_type(op[-1], t_integer);
	outport((int)op[-1].value.intval, (int)op->value.intval);
	pop(1);
	return 0;
}

/* <port> <byte> outportb - */
private int
zoutportb(register os_ptr op)
{	check_type(*op, t_integer);
	check_type(op[-1], t_integer);
	if ( (ulong)op->value.intval > 0xff )
		return_error(e_rangecheck);
	outportb((int)op[-1].value.intval, (byte)op->value.intval);
	pop(1);
	return 0;
}

/* <loc> peek <byte> */
private int
zpeek(register os_ptr op)
{	check_type(*op, t_integer);
	make_int(op, *(byte *)(op->value.intval));
	return 0;
}

/* <loc> <byte> poke - */
private int
zpoke(register os_ptr op)
{	check_type(*op, t_integer);
	check_type(op[-1], t_integer);
	if ( (ulong)op->value.intval > 0xff )
		return_error(e_rangecheck);
	*(byte *)(op[-1].value.intval) = (byte)op->value.intval;
	pop(1);
	return 0;
}

/* ------ Operator initialization ------ */

op_def zdosio_op_defs[] = {
	{"1.inport", zinport},
	{"1.inportb", zinportb},
	{"2.outport", zoutport},
	{"2.outportb", zoutportb},
	{"1.peek", zpeek},
	{"2.poke", zpoke},
	op_def_end(0)
};
