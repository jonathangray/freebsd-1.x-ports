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

/* zvmem2.c */
/* Level 2 "Virtual memory" operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "ivmspace.h"
#include "store.h"

/* <bool> setshared/setglobal - */
int
zsetshared(register os_ptr op)
{	check_type(*op, t_boolean);
	alloc_select_local((op->value.index ? 0 : a_local));
	pop(1);
	return 0;
}

/* <bool> currentshared/currentglobal - */
int
zcurrentshared(register os_ptr op)
{	push(1);
	make_bool(op, (alloc_current_local() ? 0 : 1));
	return 0;
}

/* <any> scheck/gcheck <bool> */
int
zscheck(register os_ptr op)
{	make_bool(op, (r_is_global(op) ? 1 : 0));
	return 0;
}

/* ------ Initialization procedure ------ */
op_def zvmem2_op_defs[] = {
	{"0currentshared", zcurrentshared},
	{"1scheck", zscheck},
	{"1setshared", zsetshared},
	op_def_end(0)
};
