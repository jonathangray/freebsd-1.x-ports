/* obj_stack.c - functions for displaying the stack */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_obj_stack_c_sccsid[] = "@(#)obj_stack.c	1.12 18/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <signal.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include "objtypes.h"
#include "ups.h"
#include "symtab.h"

#include "obj_stack.h"
#include "proc.h"
#include "core.h"
#include "data.h"
#include "preamble.h"
#include "ui.h"
#include "va.h"
#include "obj_signal.h"
#include "obj_misc.h"
#include "stack.h"
#include "menudata.h"
#include "debug.h"
#include "tdr.h"

/*  Arguments block for label_if_var_addr.
 */
typedef struct label_if_addr_argsst {
	func_t *li_func;
	stack_t *li_stk;
	taddr_t li_addr;
	char *li_buf;
} label_if_var_addr_args_t;

/*  A block of a function in a stack.
 */
typedef struct stackblockst {
	stack_t *sb_stk;
	block_t *sb_block;
} stackblock_t;

#ifdef CHECK_STACK
static void check_stack PROTO((void));
#endif
static char *badfuncname PROTO((taddr_t addr));
static void line_draw PROTO((struct drawst *dets));
static objid_t add_sig_markers PROTO((int lastsig));
static void add_func_vars PROTO((objid_t par));
static int remove_stack_object_if_not_a_func PROTO((objid_t par, fval_t unused_arg));
static void set_func_fields PROTO((func_t *f, int lnum, fval_t *fields));
static void expand_block PROTO((objid_t par, block_t *block));
static void label_if_var_addr PROTO((var_t *v, char *args));
static stack_t *stackpos_to_stk PROTO((stackpos_t pos));
static stack_t *obj_to_stk PROTO((objid_t obj));
static void pathname_draw PROTO((struct drawst *dets));

/*  Pointers to the outermost and innermost frame in the list of stack frames.
 *  NULL if there is no stack trace.
 */
static stack_t *Outer_stack = NULL;
static stack_t *Inner_stack = NULL;

/*  Pointer to the stack frame of the "current function".  This is the
 *  function that next and step refer to, and where local variables
 *  are initially searched for.
 */
static stack_t *Current_stack = NULL;

/*  Attempt to get the argv array of the target from the stack so
 *  we can put the arguments in the command line.  If we succeed,
 *  put as many arguments as will fit in command_line, which is
 *  taken to be command_line_size bytes long, and return 0.
 *
 *  If we fail, return -1 (with no error message).
 *
 *  FRAGILE CODE
 *  
 *  This function assumes the following stack layout, starting from
 *  the top of the stack:
 *
 *	Zero or more words with the value zero
 *	The actual bytes of the argument and environment strings
 *	A zero word
 *	Zero or more pointers to environment strings
 *	A zero word
 *	One or more pointers to argument strings
 *
 *  We get confused if the argument bytes contain a zero word - this
 *  can happen if a command is invoked with four consecutive zero length
 *  arguments.
 *
 *  It's not fatal for this routine to fail - all that happens is that
 *  no arguments are displayed on the target line.
 */
int
get_command_line_from_stack(command_line, command_line_size)
char *command_line;
int command_line_size;
{
	static char dotdotdot[] = " ...";
	enum { INITIAL, IN_STRINGS, IN_ENVP, IN_ARGV, FINAL } state;
	char argbuf[512];
	char *lptr, *lim;
	int i, argc;
	taddr_t *argv;
	taddr_t ibuf[128];
	taddr_t *iptr, word, sp, maxsp;

	argc = 0; /* gcc doesn't understand state machines */
	sp = maxsp = get_max_stack_addr();
	iptr = ibuf;

	for (state = INITIAL; state != FINAL; ) {

		/*  Safety limit - if the stack is bad we don't
		 *  want to read the entire core.
		 */
		if (maxsp - sp > 30000)
			return -1;

		if (iptr == ibuf) {
			if (dread(sp - sizeof(ibuf), (char *)ibuf, sizeof(ibuf)) != 0)
				return -1;
			iptr = ibuf + sizeof(ibuf) / sizeof(ibuf[0]);
		}

		sp -= 4;
		word = *--iptr;

		switch(state) {
		case INITIAL:
			if (word != 0)
				state = IN_STRINGS;
			break;
		case IN_STRINGS:
			if (word == 0)
				state = IN_ENVP;
			break;
		case IN_ENVP:
			if (word == 0) {
				state = IN_ARGV;
				argc = 0;
			}
			break;
		case IN_ARGV:
			if (word == argc)
				state = FINAL;
			else
				++argc;
			break;
		default:
			panic("bad state in gclafs");
		}
	}

	/*  At this point the start of the argument vector is at sp.
	 */
	if (argc == 0)
		return -1;
	argv = (taddr_t *)e_malloc(argc * sizeof(taddr_t));
	if (dread(sp + 4, (char *)argv, argc * sizeof(taddr_t)) != 0)
		return -1;
	
	lptr = command_line;
	lim = command_line + command_line_size - sizeof(dotdotdot);

	for (i = 0; i < argc; ++i) {
		if (dgets(argv[i], argbuf, sizeof(argbuf)) != 0)
			return -1;
		if (lptr + strlen(argbuf) >= lim) {
			(void) strcpy(lptr, dotdotdot);
			break;
		}
		if (i != 0)
			*lptr++ = ' ';
		(void) strcpy(lptr, argbuf);
		lptr += strlen(argbuf);
	}
	return 0;
}

taddr_t
get_outer_fp()
{
	if (Outer_stack == NULL)
		panic("bad stack in gofp");
	return Outer_stack->stk_fp;
}

static void
label_if_var_addr(v, args)
var_t *v;
char *args;
{
	label_if_var_addr_args_t *li;

	li = (label_if_var_addr_args_t *)args;

	if ((v->va_class == CL_REF || v->va_class == CL_ARG) &&
				li->li_stk->stk_ap + v->va_addr == li->li_addr)
		(void) sprintf(li->li_buf, "%s:%s", li->li_func->fu_name,v->va_name);
	if (v->va_class == CL_AUTO && li->li_stk->stk_fp + v->va_addr == li->li_addr)
		(void) sprintf(li->li_buf, "%s:%s", li->li_func->fu_name,v->va_name);
}

/*  If address addr is in the stack and matches a local variable, parameter, etc
 *  then return a label for the address.  This function is used to make
 *  debugging dumps of target stacks more useful.
 *
 *  We return a zero length string if there is no label.
 */
const char *
stack_addr_label(addr, p_is_labelled)
taddr_t addr;
bool *p_is_labelled;
{
	static char buf[100];
	stack_t *stk, *above_stk, *below_stk;
	label_if_var_addr_args_t libuf;
	func_t *f;

	*p_is_labelled = FALSE; /* but may be changed later */

	above_stk = below_stk = NULL;
	for (stk = Outer_stack; stk != NULL; stk = stk->stk_inner) {
		f = stk->stk_func;
		if (stk->stk_fp == addr) {
			(void) sprintf(buf, "[%4x %s:fp]",
							addr & 0xffff, f->fu_name);
			return buf;
		}
		if (stk->stk_ap == addr) {
			(void) sprintf(buf, "%s:<ap>", f->fu_name);
			*p_is_labelled = TRUE;
			return buf;
		}

		*buf = '\0';
		libuf.li_func = f;
		libuf.li_stk = stk;
		libuf.li_addr = addr;
		libuf.li_buf = buf;
		iterate_over_vars_of_block(FU_BLOCKS(f), label_if_var_addr,
								(char *)&libuf);
		if (*buf != '\0') {
			*p_is_labelled = TRUE;
			return buf;
		}

		if (stk->stk_fp > addr)
			above_stk = stk;
		if (stk->stk_fp < addr && below_stk == NULL)
			below_stk = stk;
	}

	if (above_stk != NULL && above_stk->stk_fp - addr < 256)
		(void) sprintf(buf, "%4x+%02x ", above_stk->stk_fp & 0xffff,
					        above_stk->stk_fp - addr);
	else
		(void) sprintf(buf, "%8s", "");

	if (below_stk != NULL && addr - below_stk->stk_fp < 256)
		(void) sprintf(buf + strlen(buf), "%4x-%02x",
						below_stk->stk_fp & 0xffff,
						addr - below_stk->stk_fp);
	else
		(void) sprintf(buf, "%7s", "");

	return buf;
}

/*  Return the "current" function, and the pc and fp values for that
 *  function.
 */
void
get_current_func(p_func, p_fp, p_pc, p_adjusted_pc)
func_t **p_func;
taddr_t *p_fp, *p_pc, *p_adjusted_pc;
{
	if (Current_stack == NULL)
		*p_func = NULL;
	else {
		*p_func = Current_stack->stk_func;
		*p_pc = Current_stack->stk_pc;
		*p_fp = Current_stack->stk_fp;
		*p_adjusted_pc = *p_pc;

		/*  If this is not the innermost function the pc is
		 *  pointing just after a call instruction.  This means
		 *  it may already be pointing at the next line, so
		 *  drop it by one to ensure that it's pointing at the
		 *  right line.
		 */
		if (Current_stack->stk_inner != NULL)
			--*p_adjusted_pc;
	}
}

static char *
badfuncname(addr)
taddr_t addr;
{
	char buf[50];

	(void) sprintf(buf, "<bad text address 0x%x>", addr);
	return strsave(buf);
}

/*  The draw routine for the format character %l.
 */
static void
line_draw(dets)
register struct drawst *dets;
{
	wn_draw_line(dets->dr_wn,
			dets->dr_x,
			dets->dr_y+dets->dr_depth/2,
			dets->dr_x + dets->dr_width,
			dets->dr_y+dets->dr_depth/2,
			WN_FG);
}

fdef_t Fsig_fdefs[] = {
	'l', line_draw, no_edit, NULL,
	'\0', NULL, NULL, NULL,
};

const char Fsig_format[] = "%*100l%15cs%*100l\n";
#define FN_FSIG_SIGNAME 0

int
fsig_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	return td_outf(level, "%s", get_field_value(code, FN_FSIG_SIGNAME));
}

void
free_fsig(obj)
objid_t obj;
{
	free((char *)get_field_value(obj, FN_FSIG_SIGNAME));
}

static objid_t
add_sig_markers(lastsig)
int lastsig;
{
	stack_t *stk, *last;

	last = NULL;
	for (stk = Outer_stack; stk != NULL; stk = stk->stk_inner) {
		last = stk;
		if (stk->stk_siginfo != NULL) {
			siginfo_t *si;
			const char *tag;

			si = stk->stk_siginfo;
			new_object((objid_t)si, OT_FSIG, (objid_t)stk, OBJ_BEFORE);
			if (si->si_signo == -1)
				tag = "<unknown signal number>";
			else
				tag = signame(si->si_signo);
			set_field_value((objid_t)si, FN_FSIG_SIGNAME,
							(fval_t)strsave(tag));
		}
	}

	if (last != NULL && lastsig != 0) {
		const char *tag;

		tag = strsave(signame(lastsig));
		new_object((objid_t)tag, OT_FSIG, (objid_t)last, OBJ_AFTER);
		set_field_value((objid_t)tag, FN_FSIG_SIGNAME, (fval_t)tag);

		return (objid_t)tag;
	}

	return NULL;
}

/*  Delete stack objects other then functions.  This function is passed
 *  to visit_objects() to clean the old stack before building a new one.
 */
static int
remove_stack_object_if_not_a_func(obj, unused_arg)
objid_t obj;
fval_t unused_arg;
{
	int objtype; 

	objtype = get_object_type(obj);
	if (objtype != OT_FUNC && objtype != OT_BADFUNC) {
		remove_object(obj, OBJ_DESCENDENTS);
		remove_object(obj, OBJ_SELF);
	}
	return 0;
}

/*  The draw routine for the format character %p.
 */
static void
pathname_draw(dets)
struct drawst *dets;
{
	int x, len, maxlen;
	bool too_long;
	char *s;
	font_t *font;
	
	font = wn_get_sysfont();

	s = (char *)dets->dr_fval;
	len = strlen(s);
	maxlen = dets->dr_width / font->ft_width;

	too_long = len > maxlen;
	if (too_long) {
		s += len - maxlen;
		len = maxlen;
	}

	x = dets->dr_x;
	if (dets->dr_user_info == NULL || dets->dr_user_info[0] != '-')
		x += dets->dr_width - len * font->ft_width;

	if (too_long) {
		static const char dots[] = "...";

		s += sizeof(dots) - 1;
		if (*s == '/' && s[1] != '/') {
			++s;
			x += font->ft_width;
		}
		wn_ttext(dets->dr_wn, dots, x, dets->dr_y, dets->dr_fg, dets->dr_bg);
		x += (sizeof(dots) - 1) * font->ft_width;
	}

	wn_ttext(dets->dr_wn, s, x, dets->dr_y, dets->dr_fg, dets->dr_bg);
}

fdef_t Func_fdefs[] = {
	'p', pathname_draw, no_edit, NULL,
	'\0', NULL, NULL, NULL,
};

const char Func_format[] = "%[-]32cp%25cp%[-]10cs\n";

#define FN_FUNC_NAME	0
#define FN_FUNC_FILE	1
#define FN_FUNC_LNUM	2
#define FN_FUNC_LAST	3

static void
set_func_fields(f, lnum, fields)
func_t *f;
int lnum;
fval_t *fields;
{
	fields[FN_FUNC_NAME] = (fval_t)f->fu_name;

	/*  The test for lnum==0 is really just to make the test scripts
	 *  easier.  Without this frig you get stack entries like
	 *  `abort.s:0'.  Doing this is easier than putting {?xxx|yyy}
	 *  in all the scripts.
	 */
	if ((f->fu_flags & FU_NOSYM) != 0 || lnum == 0) {
		fields[FN_FUNC_FILE] = (fval_t)"(no symbols)";
		fields[FN_FUNC_LNUM] = (fval_t)strsave("");
	}
	else {
		fields[FN_FUNC_FILE] = (fval_t)f->fu_fil->fi_name;
		fields[FN_FUNC_LNUM] = (fval_t)strf(":%d", lnum);
	}

	fields[FN_FUNC_LAST] = (fval_t) NULL;
}

const char Badfunc_format[] = "%[-]30cs\n";
#define FN_BADFUNC_NAME	0

/*  Add function objects to the display tree.
 */
objid_t
add_functions(proc, par)
proc_t proc;
objid_t par;
{
	stack_t *stk, *old_current_stack, *source_stk, *next_stk, *old_stk;
	stack_t *prev_stk;
	objid_t outer, obj, next, newcode, obj_to_make_visible;
	bool still_matching, old_current_stack_was_innermost;
	fval_t fields[FN_FUNC_LAST + 1];

	if (Dummyfunc.fu_preamble_id == NULL) {
		static preamble_t prbuf;

		Dummyfunc.fu_preamble_id = (preamble_id_t)&prbuf;
	}

	source_stk = NULL;

	visit_objects(par, OBJ_CHILDREN, remove_stack_object_if_not_a_func,
						(fval_t)0, (objid_t *)NULL);

	old_stk = Outer_stack;
	Outer_stack = build_stack_trace(proc);
	outer = NULL;
	still_matching = TRUE;
	old_current_stack = Current_stack;
	old_current_stack_was_innermost = Current_stack == NULL ||
					  Current_stack->stk_inner == NULL;
	Current_stack = NULL;

	/*  Is Current_stack the innermost stack frame possible?  We need
	 *  to know this because if so, we will maintain that - i.e. we
	 *  will move Current_stack in if new inner stack frames appear.
	 *
	 *  We can't just check for Current_stack->stk_inner being NULL
	 *  as there may be stack frames without source below Current_stack.
	 */
	old_current_stack_was_innermost = TRUE;
	if (Current_stack != NULL) {
		for (stk = Current_stack->stk_inner; stk != NULL;
							 stk = stk->stk_inner) {
			if (stk->stk_lnum != 0 &&
					     stk->stk_func->fu_fil != NULL) {
				old_current_stack_was_innermost = FALSE;
				break;
			}
		}
	}
	
	for (stk = Outer_stack; stk != NULL; stk = next_stk) {
		next_stk = stk->stk_inner;
		set_func_fields(stk->stk_func, stk->stk_lnum, fields);
		newcode = NULL;
		if (still_matching) {
			if (old_stk == NULL || old_stk->stk_func != stk->stk_func)
				still_matching = FALSE;
			else {
				*old_stk = *stk;

				if (outer == NULL)
					Outer_stack = old_stk;
				else
					((stack_t *)outer)->stk_inner = old_stk;

				newcode = (objid_t) old_stk;
				if (old_stk == old_current_stack)
					Current_stack = old_stk;
				old_stk = (stack_t *)get_code(newcode, OBJ_NEXT);
#ifdef CHECK_STACK
				if (stk->stk_isfree)
					panic("freeing free stk");
#endif
				free_stk(stk);
			}
		}

		if (newcode != NULL) {
			free((char *)get_field_value(newcode, FN_FUNC_LNUM));
		}
		else {
			newcode = (objid_t) stk;
			if (outer == NULL)
				new_object(newcode, OT_FUNC, par, OBJ_CHILD);
			else
				new_object(newcode, OT_FUNC, outer, OBJ_AFTER);
		}

		stk = (stack_t *) newcode;

		if (stk->stk_lnum != 0 && stk->stk_func->fu_fil != NULL)
			source_stk = stk;

		if (stk->stk_func == &Dummyfunc) {
			change_type(newcode, OT_BADFUNC);
			set_field_value(newcode, FN_BADFUNC_NAME,
						badfuncname(stk->stk_pc));
			free((char *)fields[FN_FUNC_LNUM]);
		}
		else {
			set_all_fields(newcode, fields, (fval_t)NULL);
		}

		outer = newcode;
	}

	obj_to_make_visible = add_sig_markers(proc_get_lastsig(proc));

	if (Current_stack == NULL || old_current_stack_was_innermost)
		Current_stack = source_stk;
	if (source_stk != NULL)
		highlight_source(source_stk->stk_func->fu_fil, source_stk->stk_lnum);
	else
		highlight_source((fil_t *)NULL, 0);

	/*  Remove any objects remaining from the old stack trace.
	 */
	for (obj = (objid_t)old_stk; obj != NULL; obj = next) {
		next = get_code(obj, OBJ_NEXT);
		remove_object(obj, OBJ_DESCENDENTS);
		remove_object(obj, OBJ_SELF);
	}

	/*  If the stack went bad, add an object to indicate that.
	 */
	if (Outer_stack == NULL || Outer_stack->stk_bad) {
		char badstackbuf[50];

		(void) sprintf(badstackbuf, "<bad frame pointer 0x%x>", 
				(Outer_stack != NULL) ? Outer_stack->stk_fp
						      : proc_getreg(proc, REG_FP));
		new_object((objid_t)badstackbuf, OT_BADFRAME, par, OBJ_CHILD);
		set_field_value((objid_t)badstackbuf, 0, strsave(badstackbuf));
	}

	/*  Build the backwards links.
	 */
	prev_stk = NULL;
	for (stk = Outer_stack; stk != NULL; stk = stk->stk_inner) {
		stk->stk_outer = prev_stk;
		prev_stk = stk;
	}
	Inner_stack = prev_stk;

	return obj_to_make_visible;
}

void
update_local_variable_values()
{
	stack_t *stk;

	/*  Refresh the variable values for stack frames which were
	 *  there before.
	 */
	for (stk = Outer_stack; stk != NULL; stk = stk->stk_inner)
		update_vars_of((objid_t)stk, FALSE);
}

int
badstack_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	return td_outf(level, "%s", get_field_value(code, 0));
}

void
free_badfunc(obj)
objid_t obj;
{
	free((char *)get_field_value(obj, FN_BADFUNC_NAME));
}

void
delete_functions(par)
objid_t par;
{
	remove_object(par, OBJ_DESCENDENTS);
	Outer_stack = NULL;
}

const char *
func_getobjname(obj)
objid_t obj;
{
	static char *last = NULL;
	stack_t *stk;
	const char *filename, *funcname, *lnumstr;
	int level;

	filename = get_field_value(obj, FN_FUNC_FILE);
	funcname = get_field_value(obj, FN_FUNC_NAME);
	lnumstr = get_field_value(obj, FN_FUNC_LNUM);

	if (last != NULL)
		free(last);

	level = 0;
	for (stk = (stack_t *)obj; stk != NULL; stk = stk->stk_outer)
		++level;

	last = strf("%d-%s:%s%s", level, filename, funcname, lnumstr);

	return last;
}

void
free_func(obj)
objid_t obj;
{
	stack_t *stk;

	stk = (stack_t *) obj;
	free((char *)get_field_value(obj, FN_FUNC_LNUM));
#ifdef CHECK_STACK
	if (stk->stk_isfree)
		panic("freeing free stk");
#endif
	free_stk(stk);
}

int
func_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	const char *funcname, *filename, *lnumstr;
	
	funcname = get_field_value(code, FN_FUNC_NAME);
	filename = get_field_value(code, FN_FUNC_FILE);
	lnumstr = get_field_value(code, FN_FUNC_LNUM);

	return td_outf(level, "%-20s%25s%-10s", funcname, filename, lnumstr);
}

/*  Process the return from a function line menu.
 *  Add the variables to the display or conceal them.
 *  Tell the editor where we are.
 */
void
do_func(par, command)
objid_t par;
int command;
{
	struct filst *fil;
	stack_t *stk;

	stk = (stack_t *)par;

	switch(command) {
	case MR_ADD_VARS:
		add_func_vars(par);
		break;
	case MR_HIDE_VARS:
		remove_object(par, OBJ_DESCENDENTS);
		break;
	case MR_DISPLAY_SOURCE:
		fil = stk->stk_func->fu_fil;
		if (fil == NULL) {
			errf("`%s' was not compiled with the -g flag",
							stk->stk_func->fu_name);
			return;
		}
		if (highlight_source(fil, stk->stk_lnum) == 0)
			Current_stack = stk;
		break;
	case MR_ADD_EXPRESSION:
		get_fi_vars(stk->stk_func->fu_fil);	/* ensure globals loaded */
		add_expr_object(par, FU_BLOCKS(stk->stk_func),
						stk->stk_func->fu_language);
		break;
	default:
		panic("bad cmd in df");
	}
}

#define FN_BLOCK_NAME 0
const char Block_format[] = "%*50l%15cs%*50l\n";

void
free_block(obj)
objid_t obj;
{
	free((char *)get_field_value(obj, FN_BLOCK_NAME));
	free((stackblock_t *)obj);
}


objid_t
find_block(par, block)
objid_t par;
block_t *block;
{
	objid_t obj;

	obj = get_code(par, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT))
		if (((stackblock_t *)obj)->sb_block == block)
			return obj;
	return NULL;
}

/*  Get the stack from an OT_FUNC or OT_BLOCK object.
 */
static stack_t *
obj_to_stk(obj)
objid_t obj;
{
	stack_t *stk;

	switch(get_object_type(obj)) {
	case OT_FUNC:
		stk = (stack_t *)obj;
		break;
	case OT_BLOCK:
		stk = ((stackblock_t *)obj)->sb_stk;
		break;
	default:
		panic("unknown obj type in ots");
		stk = NULL;	/* to satsfy gcc */
	}
	return stk;
}

objid_t
add_block_object(par, block)
objid_t par;
block_t *block;
{
	objid_t obj, prev;
	stackblock_t *sb;
	char buf[100];

	sb = (stackblock_t *)e_malloc(sizeof(stackblock_t));
	sb->sb_stk = obj_to_stk(par);
	sb->sb_block = block;

	/*  Add the block before any other blocks, but after any variables.
	 */
	prev = NULL;
	obj = get_code(par, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT)) {
		if (get_object_type(obj) == OT_BLOCK &&
		    ((stackblock_t *)obj)->sb_block->bl_start_lnum >
								block->bl_start_lnum)
			break;
		prev = obj;
	}
	if (prev != NULL)
		new_object((objid_t)sb, OT_BLOCK, prev, OBJ_AFTER);
	else
		new_object((objid_t)sb, OT_BLOCK, par, OBJ_FIRST_CHILD);

	(void) sprintf(buf, " lines %d..%d ",
				block->bl_start_lnum, block->bl_end_lnum);
	set_field_value((objid_t)sb, FN_BLOCK_NAME, (fval_t)strsave(buf));

	return (objid_t)sb;
}

const char *
block_getobjname(obj)
objid_t obj;
{
	static char *last = NULL;
	stackblock_t *sb;
	int count, start_lnum, end_lnum;

	sb = (stackblock_t *)obj;
	start_lnum = sb->sb_block->bl_start_lnum;
	end_lnum = sb->sb_block->bl_end_lnum;

	count = 1;
	obj = get_code((objid_t)sb->sb_stk, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT)) {
		stackblock_t *sb2;

		if (get_object_type(obj) != OT_BLOCK)
			continue;

		sb2 = (stackblock_t *)obj;
		if (sb2 == sb)
			break;
		
		if (sb2->sb_block->bl_start_lnum == start_lnum &&
		    sb2->sb_block->bl_end_lnum == end_lnum)
			++count;
	}

	if (last != NULL)
		free(last);

	if (count == 1)
		last = strf("%d..%d", start_lnum, end_lnum);
	else
		last = strf("%d-%d..%d", count, start_lnum, end_lnum);
	
	return last;
}

int
block_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	const char *name;

	name = get_field_value(code, FN_BLOCK_NAME);

	if (*name == ' ')
		++name;

	return td_outf(level, "%s", name);
}

/*  Process the return from a block header menu.
 */
void
do_block(par, command)
objid_t par;
int command;
{
	fil_t *fil;
	stackblock_t *sb;

	sb = (stackblock_t *)par;
	switch(command) {
	case MR_ADD_VARS:
		expand_block(par, sb->sb_block);
		break;
	case MR_HIDE_VARS:
		remove_object(par, OBJ_DESCENDENTS);
		break;
	case MR_DISPLAY_SOURCE:
		fil = sb->sb_stk->stk_func->fu_fil;
		if (fil == NULL)
			errf("No source file known");
		else
			show_source(fil, sb->sb_block->bl_start_lnum, TRUE);
		break;
	case MR_ADD_EXPRESSION:
		get_fi_vars(sb->sb_stk->stk_func->fu_fil); /* get globals loaded */
		add_expr_object(par, sb->sb_block,
						sb->sb_stk->stk_func->fu_language);
		break;
	default:
		panic("bad cmd in db");
	}
}

static void
add_func_vars(par)
objid_t par;
{
	func_t *f;
	block_t *blocks;
	taddr_t junk;

	f = get_stack_func(par, &junk, &junk);
#ifdef ARCH_MIPS
	if (f->fu_language != LANG_C) {
		errf("Sorry, can only show variables of C functions");
		return;
	}
#endif
	if (f->fu_flags & FU_NOSYM) {
		errf("Function %s has no variable information",
								f->fu_name);
		return;
	}

	blocks = FU_BLOCKS(f);
	if (blocks == NULL ||
	    (blocks->bl_vars == NULL && blocks->bl_blocks == NULL)) {
		errf("Function %s has no parameters or local variables",
								f->fu_name);
		return;
	}

	expand_block(par, blocks);
}

static void
expand_block(par, parblock)
objid_t par;
block_t *parblock;
{
	block_t *block;
	var_t *v;

	for (block = parblock->bl_blocks; block != NULL; block = block->bl_next) {
		if (block->bl_vars != NULL || block->bl_blocks != NULL) {
			if (find_block(par, block) == NULL)
				add_block_object(par, block);
		}
	}

	for (v = parblock->bl_vars; v != NULL; v = v->va_next)
		if (find_var(par, v) == NULL)
			(void) add_var_object(par, v, OBJ_FIRST_CHILD);
}

func_t *
get_stack_func(obj, p_fp, p_ap)
objid_t obj;
taddr_t *p_fp, *p_ap;
{
	stack_t *stk;

	stk = obj_to_stk(obj);
	*p_fp = stk->stk_fp;
	*p_ap = stk->stk_ap;
	return stk->stk_func;
}

/*  Map a stackpos_t argument to a stack frame pointer.
 */
static stack_t *
stackpos_to_stk(pos)
stackpos_t pos;
{
	struct selst *sel;
	stack_t *stk;

	switch(pos) {
	case STK_OUTER:
		stk = Outer_stack;
		break;
	case STK_CURRENT:
		stk =  Current_stack;
		break;
	case STK_INNER:
		stk = Inner_stack;
		break;
	case STK_SELECTED:
		if ((sel = get_selection()) != NULL && sel->se_next == NULL &&
		    get_object_type(sel->se_code) == OT_FUNC)
			stk =  (stack_t *)sel->se_code;
		else
			stk =  Current_stack;
		break;
	default:
		panic("bad stackpos in sts");
		stk = NULL;	/* to satisfy gcc */
	}
	return stk;
}

/*  Call func for each stack frame, starting from the frame specified by
 *  start, and ending after func has been called on the frame specified
 *  by lim.  See stackpos_to_stk() for the meanings of the pos arguments.
 *
 *  We always work outwards from the startpos to limpos.  Startpos must
 *  refer to a frame that is inside the frame referred to by endpos.
 *
 *  If (*func)() ever returns TRUE, we stop at that point and return TRUE.
 *  Otherwise we return FALSE.
 */
bool
iterate_over_stack_funcs(func, args, startpos, limpos)
bool (*func)PROTO((objid_t stack_obj, func_t *f, char *a_args));
char *args;
stackpos_t startpos, limpos;
{
	stack_t *stk, *lim;

	stk = stackpos_to_stk(startpos);
	lim = stackpos_to_stk(limpos);

	while (stk != NULL) {
		if ((*func)((objid_t)stk, stk->stk_func, args))
			return TRUE;
		if (stk == lim)
			break;
		stk = stk->stk_outer;
	}
	return FALSE;
}

/*  Find the address at which the register variable has been saved.
 *  par is the parent object of the variable (which must be a function
 *  or block).
 */
taddr_t
get_reg_addr(par, reg)
objid_t par;
int reg;
{
	taddr_t addr;
	stack_t *stk;

	stk = obj_to_stk(par);

	switch (reg) {
	case REG_PC:
		addr = (taddr_t)&stk->stk_pc;
		break;
	case REG_AP:
		addr = (taddr_t)&stk->stk_ap;
		break;
	case REG_FP:
		addr = (taddr_t)&stk->stk_fp;
		break;
	case REG_SP:
		addr = (taddr_t)&stk->stk_sp;
		break;
	default:
		if ((addr = reg_addr(stk->stk_inner, reg)) == 0)
			addr = regno_to_addr(reg);
		break;
	}

	return addr;
}
