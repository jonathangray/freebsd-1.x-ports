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

/* zcontrol.c */
/* Control operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "estack.h"
#include "iutil.h"
#include "store.h"

/* Check for updating the currentfile cache. */
#define esfile_check(ep)\
  if ( r_has_type_attrs(ep, t_file, a_executable) ) esfile = 0

/* Forward references */
private int no_cleanup(P1(os_ptr));
private es_ptr find_stopped(P0());
private void pop_estack_to(P2(os_ptr, es_ptr));

/* See the comment in opdef.h for an invariant which allows */
/* more efficient implementation of for, loop, and repeat. */

/* <obj> exec - */
int
zexec(register os_ptr op)
{	check_op(1);
	check_estack(1);
	++esp;
	ref_assign(esp, op);
	esfile_check(esp);
	pop(1);
	return o_push_estack;
}

/* <bool> <proc> if - */
int
zif(register os_ptr op)
{	check_type(op[-1], t_boolean);
	if ( op[-1].value.index )		/* true */
	   {	check_estack(1);
		++esp;
		ref_assign(esp, op);
		esfile_check(esp);
	   }
	pop(2);
	return o_push_estack;
}

/* <bool> <proc_true> <proc_false> ifelse - */
int
zifelse(register os_ptr op)
{	check_type(op[-2], t_boolean);
	check_estack(1);
	++esp;
	if ( op[-2].value.index )
	   {	ref_assign(esp, op - 1);
	   }
	else
	   {	ref_assign(esp, op);
	   }
	esfile_check(esp);
	pop(3);
	return o_push_estack;
}

/* <init> <step> <limit> <proc> for - */
private int
  for_pos_int_continue(P1(os_ptr)),
  for_neg_int_continue(P1(os_ptr)),
  for_real_continue(P1(os_ptr));
int
zfor(register os_ptr op)
{	int code;
	float params[3];
	register es_ptr ep;
	check_proc(*op);
	if ( r_has_type(op - 1, t_integer) &&
	     r_has_type(op - 2, t_integer) &&
	     r_has_type(op - 3, t_integer)
	   )
		code = 7;
	else if ( (code = num_params(op - 1, 3, params)) < 0 )
		return code;
	check_estack(7);
	/* Push a mark, the control variable, the initial value, */
	/* the increment, the limit, and the procedure, */
	/* and invoke the continuation operator. */
	mark_estack(es_for, no_cleanup);
	ep = esp += 5;
	if ( (code & 3) == 3 )		/* initial & increment are ints */
	   {	ep[-4] = op[-3];
		ep[-3] = op[-2];
		if ( code == 7 )
			ep[-2] = op[-1];
		else
			make_int(ep - 2, (long)params[2]);
		if ( ep[-3].value.intval >= 0 )
			make_op_estack(ep, for_pos_int_continue);
		else
			make_op_estack(ep, for_neg_int_continue);
	   }
	else
	   {	make_real(ep - 4, params[0]);
		make_real(ep - 3, params[1]);
		make_real(ep - 2, params[2]);
		make_op_estack(ep, for_real_continue);
	   }
	ep[-1] = *op;
	pop(4);
	return o_push_estack;
}
/* Continuation operators for for, separate for positive integer, */
/* negative integer, and real. */
/* Execution stack contains mark, control variable, increment, */
/* limit, and procedure (procedure is topmost.) */
/* Continuation operator for positive integers. */
private int
for_pos_int_continue(register os_ptr op)
{	register es_ptr ep = esp;
	long var = ep[-3].value.intval;
	if ( var > ep[-1].value.intval )
	   {	esp -= 5;	/* pop everything */
		return o_pop_estack;
	   }
	push(1);
	make_int(op, var);
	ep[-3].value.intval = var + ep[-2].value.intval;
	ref_assign(ep + 2, ep);		/* saved proc */
	esp = ep + 2;
	return o_push_estack;
}
/* Continuation operator for negative integers. */
private int
for_neg_int_continue(register os_ptr op)
{	register es_ptr ep = esp;
	long var = ep[-3].value.intval;
	if ( var < ep[-1].value.intval )
	   {	esp -= 5;	/* pop everything */
		return o_pop_estack;
	   }
	push(1);
	make_int(op, var);
	ep[-3].value.intval = var + ep[-2].value.intval;
	ref_assign(ep + 2, ep);		/* saved proc */
	esp = ep + 2;
	return o_push_estack;
}
/* Continuation operator for reals. */
private int
for_real_continue(register os_ptr op)
{	es_ptr ep = esp;
	float var = ep[-3].value.realval;
	float incr = ep[-2].value.realval;
	if ( incr >= 0 ? (var > ep[-1].value.realval) :
		(var < ep[-1].value.realval) )
		   {	esp -= 5;	/* pop everything */
			return o_pop_estack;
		   }
	push(1);
	ref_assign(op, ep - 3);
	ep[-3].value.realval = var + incr;
	esp = ep + 2;
	ref_assign(ep + 2, ep);		/* saved proc */
	return o_push_estack;
}

/* <int> <proc> repeat - */
private int repeat_continue(P1(os_ptr));
int
zrepeat(register os_ptr op)
{	check_type(op[-1], t_integer);
	check_proc(*op);
	if ( op[-1].value.intval < 0 )
		return_error(e_rangecheck);
	check_estack(5);
	/* Push a mark, the count, and the procedure, and invoke */
	/* the continuation operator. */
	mark_estack(es_for, no_cleanup);
	*++esp = op[-1];
	*++esp = *op;
	make_op_estack(esp + 1, repeat_continue);
	pop(2);
	return repeat_continue(op - 2);
}
/* Continuation operator for repeat */
private int
repeat_continue(register os_ptr op)
{	es_ptr ep = esp;		/* saved proc */
	if ( --(ep[-1].value.intval) >= 0 )	/* continue */
	   {	esp += 2;
		ref_assign(esp, ep);
		return o_push_estack;
	   }
	else				/* done */
	   {	esp -= 3;		/* pop mark, count, proc */
		return o_pop_estack;
	   }
}

/* <proc> loop */
private int loop_continue(P1(os_ptr));
int
zloop(register os_ptr op)
{	check_proc(*op);
	check_estack(4);
	/* Push a mark and the procedure, and invoke */
	/* the continuation operator. */
	mark_estack(es_for, no_cleanup);
	*++esp = *op;
	make_op_estack(esp + 1, loop_continue);
	pop(1);
	return loop_continue(op - 1);
}
/* Continuation operator for loop */
private int
loop_continue(register os_ptr op)
{	register es_ptr ep = esp;		/* saved proc */
	ref_assign(ep + 2, ep);
	esp = ep + 2;
	return o_push_estack;
}

/* - exit - */
int
zexit(register os_ptr op)
{	es_ptr ep = esp;
	esfile = 0;		/* be lazy, just clear the cache */
	while ( ep >= esbot )
	   {	if ( r_is_estack_mark(ep) )
			switch ( estack_mark_index(ep--) )
			   {
			case es_for:
				pop_estack_to(op, ep);
				return o_pop_estack;
			case es_stopped:
				return_error(e_invalidexit);	/* not a loop */
			   }
		else
			ep--;
	   }
	/* Return e_invalidexit if there is no mark at all. */
	/* This is different from PostScript, which aborts. */
	/* It shouldn't matter in practice. */
	return_error(e_invalidexit);
}

/* - stop - */
int
zstop(register os_ptr op)
{	es_ptr ep = find_stopped();
	esfile = 0;		/* be lazy, just clear the cache */
	if ( ep )
	{	pop_estack_to(op, ep - 1);
		push(1);
		make_bool(op, 1);
		return o_pop_estack;
	}
	/* Return e_invalidexit if there is no mark at all. */
	/* This is different from PostScript, which aborts. */
	/* It shouldn't matter in practice. */
	return_error(e_invalidexit);
}

/* <obj> stopped <bool> */
int
zstopped(register os_ptr op)
{	check_op(1);
	/* Mark the execution stack, and push a false in case */
	/* control returns normally. */
	check_estack(3);
	mark_estack(es_stopped, no_cleanup);
	++esp; make_false(esp);
	*++esp = *op;			/* execute the operand */
	esfile_check(esp);
	pop(1);
	return o_push_estack;
}

/* - .instopped <bool> */
int
zinstopped(register os_ptr op)
{	push(1);
	make_bool(op, find_stopped() != 0);
	return 0;
}

/* - countexecstack <int> */
int
zcountexecstack(register os_ptr op)
{	push(1);
	make_int(op, esp - esbot + 1);
	return 0;
}

/* <array> execstack <subarray> */
private int execstack_continue(P1(os_ptr));
int
zexecstack(register os_ptr op)
{	/* We can't do this directly, because the interpreter */
	/* might have cached some state.  To force the interpreter */
	/* to update the stored state, we push a continuation on */
	/* the exec stack; the continuation is executed immediately, */
	/* and does the actual transfer. */
	int depth = esp - esbot + 1;
	check_write_type(*op, t_array);
	if ( depth > r_size(op) )
		return_error(e_rangecheck);
	check_estack(1);
	r_set_size(op, depth);
	push_op_estack(execstack_continue);
	return o_push_estack;
}
/* Continuation operator to do the actual transfer */
private int
execstack_continue(register os_ptr op)
{	int depth = r_size(op);		/* was set above */
	return refcpy_to_old(op, 0, esbot, depth, "execstack");
}

/* <int> .quit - */
int
zquit(register os_ptr op)
{	check_type(*op, t_integer);
	return e_Quit;		/* Interpreter will do the exit */
}

/* ------ Non-operator routines ------ */

/* Test whether we are inside a `stopped'. */
/* The top level of the interpreter uses this. */
int
in_stopped(void)
{	return find_stopped() != 0;
}

/* ------ Initialization procedure ------ */

op_def zcontrol_op_defs[] = {
	{"0countexecstack", zcountexecstack},
	{"1exec", zexec},
	{"0execstack", zexecstack},
	{"0exit", zexit},
	{"2if", zif},
	{"3ifelse", zifelse},
	{"0.instopped", zinstopped},
	{"4for", zfor},
	{"1loop", zloop},
	{"1.quit", zquit},
	{"2repeat", zrepeat},
	{"0stop", zstop},
	{"1stopped", zstopped},
		/* Internal operators */
	{"0%execstack_continue", execstack_continue},
	{"0%for_pos_int_continue", for_pos_int_continue},
	{"0%for_neg_int_continue", for_neg_int_continue},
	{"0%for_real_continue", for_real_continue},
	{"0%loop_continue", loop_continue},
	{"0%repeat_continue", repeat_continue},
	op_def_end(0)
};

/* ------ Internal routines ------ */

/* Vacuous cleanup routine */
private int
no_cleanup(os_ptr op)
{	return 0;
}

/* Find a `stopped' mark on the e-stack. */
/* Return the e-stack pointer or 0. */
private es_ptr
find_stopped(void)
{	register es_ptr ep;
	for ( ep = esp; ep >= esbot; --ep )
	  if ( r_is_estack_mark(ep) && estack_mark_index(ep) == es_stopped )
	    return ep;
	return 0;
}

/* Pop the e-stack, executing cleanup procedures as needed. */
private void
pop_estack_to(os_ptr op, es_ptr epstop)
{	register es_ptr ep = esp;
	while ( ep > epstop )
	{	ep--;
		if ( r_is_estack_mark(ep + 1) )
		{	esp = ep;
			(*real_opproc(ep + 1))(op);
		}
	}
	esp = ep;
}
