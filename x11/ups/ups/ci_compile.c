/* ci_compile.c - routines to compile a parse tree into interpreter code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_compile_c_sccsid[] = "@(#)ci_compile.c	1.17 20/5/92 (UKC)";

#include <string.h>

#include <stdlib.h>
#include <stdio.h>
#include <setjmp.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_types.h"
#include "ci_machine.h"
#include "ci_compile.h"
#include "ci_compile_expr.h"
#include "ci_util.h"

labeldesc_t make_label PROTO((text_t *tx));
static branch_label_t *labeldesc_to_label PROTO((text_t *tx,
						 labeldesc_t labeldesc));
static void add_label_reference PROTO((text_t *tx, labeldesc_t labeldesc,
							    taddr_t location));
void set_label_addr PROTO((text_t *tx, labeldesc_t labeldesc, taddr_t addr));
static void set_var_addrs PROTO((var_t *varlist, taddr_t *p_stack_addr,
				 taddr_t *p_data_addr, taddr_t *p_bss_addr,
				 bool doing_params));
static taddr_t assign_local_addrs PROTO((block_t *blocks, taddr_t base_addr,
						    taddr_t *p_data_addr,
						    taddr_t *p_bss_addr));
static void set_local_addrs PROTO((text_t *tx, func_t *f));
static void adjust_local_addrs PROTO((block_t *par, long delta));
static void compile_func PROTO((text_t *tx, func_t *f));
static void compile_statement PROTO((text_t *tx, statement_t *st));

static void compile_labeled_statement PROTO((text_t *tx, labeled_stm_t *ls));
static void compile_case_labeled_statement PROTO((text_t *tx,
						  case_labeled_stm_t *cs));
static void compile_expression_statement PROTO((text_t *tx, expr_t *expr));
static void compile_compound_statement PROTO((text_t *tx, compound_stm_t *co));
static void compile_if_statement PROTO((text_t *tx, if_stm_t *is));
static void compile_switch_statement PROTO((text_t *tx, switch_stm_t *ss));
static void compile_while_or_do_statement PROTO((text_t *tx,
						 while_stm_t *ws,
						 bool is_while_statement));
static void compile_for_statement PROTO((text_t *tx, for_stm_t *fs));
static void compile_goto_statement PROTO((text_t *tx, goto_label_t *gl));
static void compile_continue_or_break_statement PROTO((text_t *tx,
					             label_type_t label_type));
static void compile_return_statement PROTO((text_t *tx, expr_t *expr));
static bool need_final_return PROTO((statement_t *body_st));
static void push_label PROTO((text_t *tx, label_type_t label_type));
static void pop_label PROTO((text_t *tx, label_type_t label_type));
static void set_parameter_addrs PROTO((func_t *f));
static taddr_t compile_crt0 PROTO((text_t *tx, int argc, const char **argv));
static void do_relocations PROTO((text_t *tx, char *data, char *data_copy));
static void add_lno_entry PROTO((text_t *tx, taddr_t addr, lexinfo_t *lx));
static void report_compile_error PROTO((char *arg, errtype_t errtype,
					lexinfo_t *lx, const char *mesg));

static labelref_t *new_labelref PROTO((void));
static void free_labelref_list PROTO((labelref_t *labelref));
ALLOC_NEW_FREELIST(static,labelref_t,labelref,lr_next)

static label_list_t *new_labellist PROTO((void));
static void free_labellist PROTO((label_list_t *labellist));
ALLOC_NEW_FREE(static,label_list_t,labellist,ll_next)
static taddr_t compile_crt0 PROTO((text_t *tx, int argc, const char **argv));

labeldesc_t
make_label(tx)
text_t *tx;
{
	labeldesc_t labeldesc;
	branch_label_t *lb;

	for (labeldesc = 0; labeldesc < tx->tx_labellim; ++labeldesc) {
		if (tx->tx_labels[labeldesc] == NULL)
			break;
	}
	if (labeldesc == tx->tx_labellim) {
		int i;

		tx->tx_labellim += 50;
		tx->tx_labels = (branch_label_t **)e_realloc((char *)tx->tx_labels,
						tx->tx_labellim * sizeof(branch_label_t *));
		for (i = labeldesc; i < tx->tx_labellim; ++i)
			tx->tx_labels[i] = NULL;
	}

	lb = (branch_label_t *)e_malloc(sizeof(branch_label_t));
	lb->lb_addr = 0;
	lb->lb_refs = NULL;
	tx->tx_labels[labeldesc] = lb;
	return labeldesc;
}

static branch_label_t *
labeldesc_to_label(tx, labeldesc)
text_t *tx;
labeldesc_t labeldesc;
{
	if (labeldesc > MAX_LABELDESC || labeldesc >= tx->tx_labellim ||
						tx->tx_labels[labeldesc] == NULL)
		ci_panic("labeldesc botch in ar");
	return tx->tx_labels[labeldesc];
}

static void
add_label_reference(tx, labeldesc, location)
text_t *tx;
labeldesc_t labeldesc;
taddr_t location;
{
	branch_label_t *lb;
	labelref_t *lr;
	
	lb = labeldesc_to_label(tx, labeldesc);
	lr = new_labelref();
	lr->lr_location = location;
	lr->lr_next = lb->lb_refs;
	lb->lb_refs = lr;
}

void
set_label_addr(tx, labeldesc, addr)
text_t *tx;
labeldesc_t labeldesc;
taddr_t addr;
{
	branch_label_t *lb;

	lb = labeldesc_to_label(tx, labeldesc);
	if (lb->lb_addr != 0)
		ci_panic("multiple calls of sla");
	lb->lb_addr = addr;
}

void
ci_resolve_and_free_label(tx, labeldesc)
text_t *tx;
labeldesc_t labeldesc;
{
	branch_label_t *lb;
	labelref_t *lr;
	taddr_t savepc;

	lb = labeldesc_to_label(tx, labeldesc);
	if (lb->lb_addr == 0)
		lb->lb_addr = tx->tx_pc;
	
	savepc = tx->tx_pc;
	for (lr = lb->lb_refs; lr != NULL; lr = lr->lr_next) {
		long offset;

		tx->tx_pc = lr->lr_location;
		offset = lb->lb_addr - lr->lr_location;
		if (offset != (short)offset)
			ci_panic("jump out of range");
		ci_code_word(tx, (long)(unsigned short)offset);
	}
	tx->tx_pc = savepc;
	
	free_labelref_list(lb->lb_refs);
	free((char *)lb);
	tx->tx_labels[labeldesc] = NULL;
}

labeldesc_t
ci_code_jump_to_label(tx, opcode, labeldesc)
text_t *tx;
opcode_t opcode;
labeldesc_t labeldesc;
{
	if (labeldesc == NO_LABELDESC)
		labeldesc = make_label(tx);

	ci_code_opcode(tx, opcode);
	add_label_reference(tx, labeldesc, tx->tx_pc);
	ci_code_word(tx, (long)labeldesc);

	return labeldesc;
}

static void
push_label(tx, label_type)
text_t *tx;
label_type_t label_type;
{
	label_list_t *ll;

	ll = new_labellist();
	ll->ll_labeldesc = make_label(tx);
	ll->ll_next = tx->tx_label_lists[(int)label_type];
	tx->tx_label_lists[(int)label_type] = ll;
}

static void
pop_label(tx, label_type)
text_t *tx;
label_type_t label_type;
{
	label_list_t *ll;

	ll = tx->tx_label_lists[(int)label_type];
	tx->tx_label_lists[(int)label_type] = ll->ll_next;
	ci_resolve_and_free_label(tx, ll->ll_labeldesc);
	free_labellist(ll);
}

void
ci_add_relocation(tx, reloc_type, location, addr, count)
text_t *tx;
reloc_type_t reloc_type;
taddr_t location, addr;
long count;
{
	reloc_t *rl;

	rl = (reloc_t *)e_malloc(sizeof(reloc_t));
	rl->rl_reloc_type = reloc_type;
	rl->rl_location = location;
	rl->rl_addr = addr;
	rl->rl_count = count;
	rl->rl_next = tx->tx_relocs;
	tx->tx_relocs = rl;
}

static void
do_relocations(tx, data, data_copy)
text_t *tx;
char *data, *data_copy;
{
	char *bss;
	reloc_t *rl, *next;
	taddr_t savepc;

	bss = data + tx->tx_data_addr;

	savepc = tx->tx_pc;
	for (rl = tx->tx_relocs; rl != NULL; rl = next) {
		next = rl->rl_next;
		switch (rl->rl_reloc_type) {
		case RLT_RELOC_BSS:
			tx->tx_pc = rl->rl_location;
			ci_code_long(tx, (long)(rl->rl_addr + (taddr_t)bss));
			break;
		case RLT_RELOC_DATA:
			tx->tx_pc = rl->rl_location;
			ci_code_long(tx, (long)(rl->rl_addr + (taddr_t)data));
			break;
		case RLT_COPY_TO_DATA:
			memcpy(data_copy + rl->rl_addr, (char *)rl->rl_location,
								     rl->rl_count);
			break;
		default:
			ci_panic("bad rtype in dr");
		}
		free((char *)rl);
	}
	tx->tx_pc = savepc;
	tx->tx_relocs = NULL;
}

static void
set_var_addrs(varlist, p_stack_addr, p_data_addr, p_bss_addr, doing_params)
var_t *varlist;
taddr_t *p_stack_addr, *p_data_addr, *p_bss_addr;
bool doing_params;
{
	var_t *v;
	taddr_t *p_addr;

	for (v = varlist; v != NULL; v = v->va_next) {
		long tsize;

		if (v->va_class == CL_DECL ||
					v->va_type->ty_code == DT_FUNC_RETURNING)
			continue;
		
		tsize = ci_typesize(v->va_lexinfo, v->va_type);

		if (IS_LOCAL_CLASS(v->va_class)) {
			p_addr = p_stack_addr;
		}
		else
			p_addr = (v->va_flags & VA_HAS_INITIALISER) ? p_data_addr
								    : p_bss_addr;

		v->va_addr = ci_align_addr_for_type(*p_addr, v->va_type);
		*p_addr = v->va_addr + tsize;

		if (doing_params && tsize < sizeof(stackword_t) &&
						     IS_LOCAL_CLASS(v->va_class)) {
			static int endian = 0;
			static bool big_endian;

			if (endian == 0) {
				endian = 42;
				big_endian = *(char *)&endian != 42;
			}

			if (big_endian)
				v->va_addr += sizeof(stackword_t) - tsize; 

			*p_addr += sizeof(stackword_t) - tsize;
		}
	}
}
			
func_t *
ci_addr_to_func(funclist, addr)
func_t *funclist;
taddr_t addr;
{
	func_t *f;

	for (f = funclist; f != NULL; f = f->fu_next)
		if (f->fu_addr == addr)
			return f;
	return NULL;
}

taddr_t
ci_libvarname_to_addr(tx, lx, name)
text_t *tx;
lexinfo_t *lx;
const char *name;
{
	taddr_t addr;

	if ((*tx->tx_getaddrproc)(name, &addr) == CI_DATA)
		return addr;

	diagf(ET_ERROR, lx, "Undefined external variable %s", name);

	/*  We don't want any furthur error messages for this variable,
	 *  so return a non-zero address.  The error message above will
	 *  ensure that compilation fails.
	 */
	return 1;
}

int
ci_funcname_to_index(tx, lx, name, error_if_not_found)
text_t *tx;
lexinfo_t *lx;
const char *name;
bool error_if_not_found;
{
	func_t *f;
	fil_t *fil;
	libfunc_t *lf;
	taddr_t addr;
	int func_index;

	fil = tx->tx_curfil;
	func_index = 1;
	for (f = tx->tx_funclist; f != NULL; f = f->fu_next) {
		if (strcmp(f->fu_name, name) == 0 &&
				(f->fu_fil == fil || !(f->fu_flags & FU_STATIC)))
			return func_index;
		++func_index;
	}
	
	for (lf = tx->tx_direct_libfuncs; lf != NULL; lf = lf->lf_next)
		if (strcmp(lf->lf_name, name) == 0)
			return lf->lf_index;
	
	for (lf = tx->tx_indirect_libfuncs; lf != NULL; lf = lf->lf_next)
		if (strcmp(lf->lf_name, name) == 0)
			return lf->lf_index;
	
	/*  Not seen before, so query for it with the callback function
	 *  provided by the application.
	 */
	switch ((*tx->tx_getaddrproc)(name, &addr)) {
	case CI_DIRECT_LIBFUNC:
		lf = (libfunc_t *)alloc(tx->tx_alloc_id, sizeof(libfunc_t));
		lf->lf_name = name;
		lf->lf_addr = addr;
		lf->lf_index = tx->tx_num_funcs + tx->tx_num_direct_libfuncs++;
		lf->lf_next = tx->tx_direct_libfuncs;
		tx->tx_direct_libfuncs = lf;
		return lf->lf_index;
	case CI_INDIRECT_LIBFUNC:
		lf = (libfunc_t *)alloc(tx->tx_alloc_id, sizeof(libfunc_t));
		lf->lf_name = name;
		lf->lf_addr = addr;
		lf->lf_index = -tx->tx_num_indirect_libfuncs++; /* negative */
		lf->lf_next = tx->tx_direct_libfuncs;
		tx->tx_direct_libfuncs = lf;
		return lf->lf_index;
	case CI_DATA:
	case CI_UNDEFINED:
		break;
	default:
		ci_panic("bad nametype in fti");
	}

	if (error_if_not_found)
		diagf(ET_ERROR, lx, "Undefined function %s", name);

	return 0;
}

static void
report_compile_error(arg, errtype, lx, mesg)
char *arg;
errtype_t errtype;
lexinfo_t *lx;
const char *mesg;
{
	text_t *tx;

	tx = (text_t *)arg;

	if (tx->tx_report_error_func != NULL) {
		bool shut_up;

		if (lx != NULL)
			shut_up = (*tx->tx_report_error_func)(lx->lx_filename,
						     lx->lx_lnum, lx->lx_cnum, mesg);
		else
			shut_up = (*tx->tx_report_error_func)((const char *)NULL,
							      0, 0, mesg);
		if (shut_up)
			tx->tx_report_error_func = NULL;
	}

	if (errtype == ET_ERROR)
		tx->tx_had_error = TRUE;
}

code_id_t
ci_compile_program(parse_id, report_error, getaddrproc, checkarg_proc,
		   regno_to_addr_proc, get_regaddr_proc, get_regaddr_proc_arg,
		   flags, argc, argv)
parse_id_t parse_id;
ci_report_error_func_t report_error;
ci_getaddrproc_t getaddrproc;
ci_checkarg_proc_t checkarg_proc;
ci_regno_to_addr_proc_t regno_to_addr_proc;
ci_get_regaddr_proc_t get_regaddr_proc;
char *get_regaddr_proc_arg;
unsigned long flags;
int argc;
const char **argv;
{
	extern bool ci_Catching_panics;
	extern jmp_buf ci_Catch_panic_env;
	parse_res_t *pr;
	machine_t *machine;
	text_t textbuf;
	taddr_t entry_point, pclim;
	char *data, *data_copy;
	func_t *f;
	taddr_t max_sp;
	int stack_size;

	pclim = 200;

	pr = (parse_res_t *)parse_id;

	textbuf.tx_alloc_id = alloc_create_pool();

	if ((flags & CI_CP_DONT_PANIC) != 0) {
		if (ci_Catching_panics)
			panic("catch_panics botch");

		ci_Catching_panics = TRUE;
		if (setjmp(ci_Catch_panic_env) != 0) {
			ci_set_diag_handler((diag_handler_func_t)NULL,
								(char *)NULL);
			alloc_free_pool(textbuf.tx_alloc_id);
			ci_Catching_panics = FALSE;
			return NULL;
		}
	}

	textbuf.tx_text = (textword_t *)e_malloc((size_t)(pclim *
							  sizeof(textword_t)));
	textbuf.tx_pclim = pclim;
	textbuf.tx_pc = 0; 		/* avoid conflict with NULL */
	textbuf.tx_data_addr = 1;	/* ditto */
	textbuf.tx_bss_addr = 1;	/* ditto */
	textbuf.tx_labels = NULL;
	textbuf.tx_relocs = NULL;
	textbuf.tx_labellim = 0;
	textbuf.tx_lnos = NULL;
	textbuf.tx_last_lno = NULL;
	textbuf.tx_curfil = NULL;
	textbuf.tx_initlist = pr->pr_block->bl_initlist;
	textbuf.tx_label_lists[(int)LT_BREAK] = NULL;
	textbuf.tx_label_lists[(int)LT_CONTINUE] = NULL;
	textbuf.tx_label_lists[(int)LT_GOTO] = NULL;
	textbuf.tx_funclist = pr->pr_funcs;
	textbuf.tx_varlist = pr->pr_block->bl_vars;
	textbuf.tx_direct_libfuncs = NULL;
	textbuf.tx_num_direct_libfuncs = 0;
	textbuf.tx_indirect_libfuncs = NULL;
	textbuf.tx_num_indirect_libfuncs = 1; /* so the first index is -1 */
	textbuf.tx_report_error_func = report_error;
	textbuf.tx_getaddrproc = getaddrproc;
	textbuf.tx_checkarg_proc = checkarg_proc;
	textbuf.tx_regno_to_addr_proc = regno_to_addr_proc;
	textbuf.tx_had_error = FALSE;
	textbuf.tx_flags = flags;
	textbuf.tx_have_proc_vars = pr->pr_block->bl_parent != NULL;
	textbuf.tx_want_reg_relocs = get_regaddr_proc != NULL;
	textbuf.tx_reg_relocs = NULL;

	set_var_addrs(textbuf.tx_varlist, (taddr_t *)NULL, &textbuf.tx_data_addr,
						    &textbuf.tx_bss_addr, FALSE);

	/*  Make jumps to zero halt execution.
	 *
	 *  We initialise sp and max_sp to avoid complaints from purify
	 *  about uninitialised variables.
	 */
	textbuf.tx_sp = 0;
	textbuf.tx_max_sp = 0;
	ci_code_opcode(&textbuf, OC_TRAP);

	/*  Assign addresses to all the functions.  We start from one
	 *  to avoid having NULL valid function pointers.
	 */
	textbuf.tx_num_funcs = 1;
	for (f = pr->pr_funcs; f != NULL; f = f->fu_next)
		++textbuf.tx_num_funcs;
	
	ci_set_diag_handler(report_compile_error, (char *)&textbuf);

	max_sp = 0;
	for (f = pr->pr_funcs; f != NULL; f = f->fu_next) {
		f->fu_addr = textbuf.tx_pc;
		compile_func(&textbuf, f);
		if (need_final_return((statement_t *)f->fu_statement_id)) {
			opcode_t opcode;

			opcode = (flags & CI_CP_FUNCTRACE) ? OC_TRACERET
							   : OC_RET;
			ci_code_opcode(&textbuf, opcode);
		}
		if (textbuf.tx_max_sp > max_sp)
			max_sp = textbuf.tx_max_sp;
	}

	/*  BUG: the +4 in the expression is to work around an off-by-one
	 *       error in the stack size calculation for the single function
	 *       case.  TODO: find out what's wrong and fix it.
	 */
	textbuf.tx_sp = max_sp + 4;

	entry_point = compile_crt0(&textbuf, argc, argv);

	/*  We now have tx_sp set to the maximum stack space used by
	 *  any one function, plus the space needed by the startup
	 *  code.  If there is only a single function, then that is
	 *  all the stack space we need.  Otherwise we don't know, so
	 *  just give a plausible amount.
	 *
	 *  The one function case will be the normal one for ups breakpoint
	 *  code and display expressions.
	 *
	 *  TODO: work out maximum stack size when there are multiple
	 *        functions - possible if there is no recursion.
	 *
	 *  Note that tx_num_funcs is one higher than the number of
	 *  because it starts from one (to avoid NULL valid function ptrs.
	 */
	stack_size = (textbuf.tx_num_funcs <= 2) ? textbuf.tx_max_sp : 16 * 1024;

	/*  Round up the bss and data sizes to the maximum alignment.
	 *
	 *  This is necessary for data, as bss follows it, and we want bss
	 *  to start on the maximum alignment boundary.  We do bss as well
	 *  for neatness.
	 */
	textbuf.tx_data_addr = ci_align_addr_for_type(textbuf.tx_data_addr,
								(type_t *)NULL);
	textbuf.tx_bss_addr = ci_align_addr_for_type(textbuf.tx_bss_addr,
								(type_t *)NULL);

	data = alloc(textbuf.tx_alloc_id, (size_t)(textbuf.tx_data_addr +
					  textbuf.tx_bss_addr + stack_size));
	data_copy = alloc(textbuf.tx_alloc_id, (size_t)textbuf.tx_data_addr);
	ci_do_static_initialisations(&textbuf, textbuf.tx_initlist,
				     (taddr_t)data,
				     (taddr_t)(data + textbuf.tx_data_addr),
				     data_copy);


	ci_set_diag_handler((diag_handler_func_t)NULL, (char *)NULL);

	if (textbuf.tx_had_error) {
		alloc_free_pool(textbuf.tx_alloc_id);
		machine = NULL;
	}
	else {
		libfunc_t *lf;
		int i, nfuncs;
		taddr_t *funcaddrs;

		nfuncs = textbuf.tx_num_indirect_libfuncs + textbuf.tx_num_funcs +
						textbuf.tx_num_direct_libfuncs;

		funcaddrs = (taddr_t *)alloc(textbuf.tx_alloc_id,
						         nfuncs * sizeof(taddr_t));
		funcaddrs += textbuf.tx_num_indirect_libfuncs;

		for (lf = textbuf.tx_indirect_libfuncs; lf != NULL; lf = lf->lf_next)
			funcaddrs[lf->lf_index] = lf->lf_addr;
		for (i = 1, f = pr->pr_funcs; f != NULL; f = f->fu_next)
			funcaddrs[i++] = f->fu_addr;
		for (lf = textbuf.tx_direct_libfuncs; lf != NULL; lf = lf->lf_next)
			funcaddrs[lf->lf_index] = lf->lf_addr;

		do_relocations(&textbuf, data, data_copy);

		machine = (machine_t *)alloc(textbuf.tx_alloc_id, sizeof(machine_t));
		machine->ma_data = data;
		machine->ma_data_copy = data_copy;
		machine->ma_text = textbuf.tx_text;
		machine->ma_entry_point = entry_point;
		machine->ma_text_size = textbuf.tx_pc;
		machine->ma_data_size = textbuf.tx_data_addr;
		machine->ma_bss_size = textbuf.tx_bss_addr;
		machine->ma_stack_size = stack_size;
		machine->ma_funcaddrs = funcaddrs;
		machine->ma_nfuncs = textbuf.tx_num_funcs;
		machine->ma_get_regaddr_proc = get_regaddr_proc;
		machine->ma_get_regaddr_proc_arg = get_regaddr_proc_arg;
		machine->ma_reg_relocs = textbuf.tx_reg_relocs;
		machine->ma_alloc_id = textbuf.tx_alloc_id;

		ci_initialise_code((code_id_t)machine, TRUE);
	}

	ci_Catching_panics = FALSE;

	return (code_id_t)machine;
}

void
ci_free_code_id(code_id)
code_id_t code_id;
{
	free(((machine_t *)code_id)->ma_text);
	alloc_free_pool(((machine_t *)code_id)->ma_alloc_id);
}

ci_opcode_t
ci_install_trap_instruction(code_id, location)
code_id_t code_id;
long location;
{
	machine_t *machine;
	int res;

	machine = (machine_t *)code_id;

	res = machine->ma_text[location];
	machine->ma_text[location] = (int)OC_TRAP;
	return res;
}

void
ci_uninstall_trap_instruction(code_id, location, opcode)
code_id_t code_id;
long location;
ci_opcode_t opcode;
{
	machine_t *machine;

	if (opcode >= (int)OC_LAST_OPCODE)
		ci_panic("opcode botch in cuti");

	machine = (machine_t *)code_id;

	if (machine->ma_text[location] != (int)OC_TRAP)
		ci_panic("missing bpt in cuti");	

	machine->ma_text[location] = (int)opcode;
}

static taddr_t
compile_crt0(tx, argc, argv)
text_t *tx;
int argc;
const char **argv;
{
	int main_index, exit_index;
	unsigned long save_check_sp;
	taddr_t entry_point;

	main_index = ci_funcname_to_index(tx, (lexinfo_t *)NULL, "$start", FALSE);
	if (main_index != 0)
		exit_index = 0;
	else {
		main_index = ci_funcname_to_index(tx,(lexinfo_t *)NULL, "main",TRUE);
		exit_index = ci_funcname_to_index(tx,(lexinfo_t *)NULL, "exit",TRUE);
	}

	entry_point = tx->tx_pc;

	/*  Don't want sp checking here, as the frame pointer is not
	 *  yet set up.
	 */
	save_check_sp = tx->tx_flags & CI_CP_CHECKSP;
	tx->tx_flags &= ~CI_CP_CHECKSP;

	ci_code_constpush(tx, (long)argv);
	ci_code_constpush(tx, (long)argc);
	ci_code_generic_opcode(tx, OC_CALL_B, (stackword_t)main_index);
	ci_code_byte(tx, 2);

	if (exit_index != 0) {
		ci_code_opcode(tx, OC_PUSH_WORD_RETVAL);
		ci_code_generic_opcode(tx, OC_CALL_B, (stackword_t)exit_index);
		ci_code_byte(tx, 1);
	}

	ci_code_opcode(tx, OC_TRAP);

	tx->tx_flags |= save_check_sp;

	return entry_point;
}

static taddr_t
assign_local_addrs(blocks, base_addr, p_data_addr, p_bss_addr)
block_t *blocks;
taddr_t base_addr;
taddr_t *p_data_addr, *p_bss_addr;
{
	block_t *bl;
	taddr_t max_addr;

	max_addr = base_addr;
	for (bl = blocks; bl != NULL; bl = bl->bl_next) {
		taddr_t addr;

		addr = base_addr;
		set_var_addrs(bl->bl_vars, &addr, p_data_addr, p_bss_addr, FALSE);
		addr = assign_local_addrs(bl->bl_blocks, addr, p_data_addr,
									p_bss_addr);
		if (addr > max_addr)
			max_addr = addr;
	}
	return max_addr;
}

/*  Set the addresses of the parameters of function f.
 *  These are the variables hanging off the outermost block.
 *  As the stack grows downwards, the addresses are positive offsets
 *  from the frame pointer, starting at two stack words offset
 *  (these slots are used for the saved fp and pc).
 *
 *  See the diagram below for a picture of the stack layout.
 */
static void
set_parameter_addrs(f)
func_t *f;
{
	taddr_t stack_addr, data_addr, bss_addr;
	typecode_t typecode;

	stack_addr = 2 * sizeof(stackword_t);
	typecode = f->fu_type->ty_base->ty_code;
	if (typecode == TY_UNION || typecode == TY_STRUCT)
		stack_addr += sizeof(stackword_t);
	
	data_addr = bss_addr = 0;
	set_var_addrs(f->fu__blocks->bl_vars, &stack_addr, &data_addr, &bss_addr,
									    TRUE);
	if (data_addr != 0 || bss_addr != 0)
		ci_panic("params botch in spa");
}

static void
set_local_addrs(tx, f)
text_t *tx;
func_t *f;
{
	taddr_t stack_addr;

	/*  Allocate space for local variables.
	 *
	 *  The outermost block is for parameters, which we don't have
	 *  to allocate space for.
	 */
	stack_addr = assign_local_addrs(f->fu__blocks->bl_blocks, 0,
					&tx->tx_data_addr, &tx->tx_bss_addr);

	if (stack_addr % sizeof(stackword_t) != 0)
		stack_addr += sizeof(stackword_t) - stack_addr % sizeof(stackword_t);
	tx->tx_sp = stack_addr;

	/*  We offset the local addresses so they point above the
	 *  frame pointer.  As the stack grows downwards, these are
	 *  negative offsets starting at -4(fp).  We shift the
	 *  addresses down by the number of slots used by the locals,
	 *  so the last local ends up at -4(fp).
	 *
	 *  Thus:
	 *			  .	     .
	 *			  .	     .
	 *			  |----------|
	 *		   -8(fp) | local n-1|
	 *			  |----------|
	 *		   -4(fp) | local n  |
	 *			  |----------|
	 *	fp ----->   0(fp) | saved fp |
	 *			  |----------|
	 *		    4(fp) | saved pc |
	 *			  |----------|
	 *		    8(fp) |  arg 1   |
	 *			  |----------|
	 *		   12(fp) |  arg 2   |
	 *			  |----------|
	 *			  .	     .
	 *			  .	     .
	 */
	adjust_local_addrs(f->fu__blocks->bl_blocks, -(long)stack_addr);
}

/*  Add delta to the addresses of all local variables in block block,
 *  and recursively do all the sub-blocks.
 */
static void
adjust_local_addrs(par, delta)
block_t *par;
long delta;
{
	var_t *v;
	block_t *bl;
	
	for (bl = par->bl_blocks; bl != NULL; bl = bl->bl_next)
		adjust_local_addrs(bl, delta);
	for (v = par->bl_vars; v != NULL; v = v->va_next)
		if (IS_LOCAL_CLASS(v->va_class))
			v->va_addr += delta;
}

static void
compile_func(tx, f)
text_t *tx;
func_t *f;
{
	taddr_t savepc, checkpc, savesp;
	opcode_t opcode;

	/*  All functions should have a single outer block which holds
	 *  the parameters, with one child which corresponds to the
	 *  compound statement for the function body.  Check this.
	 */
	if (f->fu__blocks == NULL ||
	    f->fu__blocks->bl_next != NULL ||
	    f->fu__blocks->bl_blocks == NULL ||
	    f->fu__blocks->bl_blocks->bl_next != NULL)
		ci_panic("blocks botch in cf");

	set_parameter_addrs(f);
	set_local_addrs(tx, f);

	tx->tx_max_sp = tx->tx_sp;
	opcode = (tx->tx_flags & CI_CP_FUNCTRACE) ? OC_TRACELINK_B : OC_LINK_B;
	ci_code_generic_opcode(tx, opcode, tx->tx_sp);

	savesp = tx->tx_sp;
	checkpc = tx->tx_pc++;

	tx->tx_curfil = f->fu_fil;
	compile_statement(tx, (statement_t *)f->fu_statement_id);
	tx->tx_curfil = NULL;

	savepc = tx->tx_pc;
	tx->tx_pc = checkpc;
	ci_code_byte(tx, (long)(tx->tx_max_sp - savesp));
	tx->tx_pc = savepc;

	f->fu__lnos = tx->tx_lnos;
	tx->tx_lnos = tx->tx_last_lno = NULL;

	while (tx->tx_label_lists[(int)LT_GOTO] != NULL)
		pop_label(tx, LT_GOTO);
}

static bool
need_final_return(body_st)
statement_t *body_st;
{
	statement_t *last, *st;

	if (body_st->st_type != STT_COMPOUND)
		ci_panic("st_type botch in nfr");
	
	last = NULL;
	for (st = body_st->st_compound->co_statements; st != NULL; st = st->st_next)
		last = st;
	
	return last == NULL || last->st_type != STT_RETURN;
}

static void
compile_statement(tx, st)
text_t *tx;
statement_t *st;
{
	if (st->st_lexinfo != NULL)
		add_lno_entry(tx, tx->tx_pc, st->st_lexinfo);

	switch (st->st_type) {
	case STT_LABELED:
		compile_labeled_statement(tx, st->st_labeled);
		break;
	case STT_CASE_LABELED:
		compile_case_labeled_statement(tx, st->st_case);
		break;
	case STT_EXPR:
		compile_expression_statement(tx, st->st_expr);
		break;
	case STT_COMPOUND:
		compile_compound_statement(tx, st->st_compound);
		break;
	case STT_IF:
		compile_if_statement(tx, st->st_if);
		break;
	case STT_SWITCH:
		compile_switch_statement(tx, st->st_switch);
		break;
	case STT_WHILE:
		compile_while_or_do_statement(tx, st->st_while, TRUE);
		break;
	case STT_DO:
		compile_while_or_do_statement(tx, st->st_while, FALSE);
		break;
	case STT_FOR:
		compile_for_statement(tx, st->st_for);
		break;
	case STT_GOTO:
		compile_goto_statement(tx, st->st_goto_label);
		break;
	case STT_CONTINUE:
		compile_continue_or_break_statement(tx, LT_CONTINUE);
		break;
	case STT_BREAK:
		compile_continue_or_break_statement(tx, LT_BREAK);
		break;
	case STT_RETURN:
		compile_return_statement(tx, st->st_expr);
		break;
	default:
		ci_panic("bad stm type in cs");
	}

}

static void
compile_case_labeled_statement(tx, cs)
text_t *tx;
case_labeled_stm_t *cs;
{
	if (cs->cs_labeldesc == NO_LABELDESC)
		ci_panic("labeldesc botch in ccls");
	set_label_addr(tx, cs->cs_labeldesc, tx->tx_pc);
	compile_statement(tx, cs->cs_stm);
}

static void
add_lno_entry(tx, addr, lx)
text_t *tx;
taddr_t addr;
lexinfo_t *lx;
{
	lno_t *lno;

	lno = (lno_t *)alloc(tx->tx_alloc_id, sizeof(lno_t));
	lno->ln_addr = addr;
	lno->ln_num = lx->lx_lnum;
	lno->ln_next = NULL;

	if (tx->tx_lnos == NULL)
		tx->tx_lnos = lno;
	else
		tx->tx_last_lno->ln_next = lno;
	tx->tx_last_lno = lno;
}

static void
compile_expression_statement(tx, expr)
text_t *tx;
expr_t *expr;
{
	if (expr != NULL)
		ci_compile_expression(tx, expr, EC_EFFECT);
}

static void
compile_compound_statement(tx, co)
text_t *tx;
compound_stm_t *co;
{
	statement_t *st;
	initlist_t *il, *next;

	for (il = co->co_block->bl_initlist; il != NULL; il = next) {
		next = il->il_next;
		if (IS_LOCAL_CLASS(il->il_var->va_class)) {
			initialiser_t *in;

			in = (initialiser_t *)il->il_initialiser_id;
			if (in->in_inittype != IT_EXPR) {
				diagf(ET_ERROR, il->il_var->va_lexinfo,
	      "Initialisation of local aggregate %s not yet implemented",
							il->il_var->va_name);
				continue;
			}
			ci_compile_var_reference(tx, il->il_var,
						 il->il_var->va_lexinfo, EC_ADDRESS);
			ci_compile_expression(tx, in->in_expr, EC_VALUE);
			ci_code_assign(tx, il->il_var->va_type, FALSE, AT_CI_ADDR);
		}
		else {
			il->il_next = tx->tx_initlist;
			tx->tx_initlist = il;
		}
	}
	for (st = co->co_statements; st != NULL; st = st->st_next)
		compile_statement(tx, st);
}

static void
compile_if_statement(tx, is)
text_t *tx;
if_stm_t *is;
{
	labeldesc_t elselabel;

	ci_compile_expression(tx, is->is_expr, EC_VALUE);
	elselabel = ci_code_jump_to_label(tx, OC_JUMP_IF_ZERO, NO_LABELDESC);
	compile_statement(tx, is->is_ifpart);

	if (is->is_elsepart == NULL)
		ci_resolve_and_free_label(tx, elselabel);
	else {
		labeldesc_t nextlabel;

		nextlabel = ci_code_jump_to_label(tx, OC_JUMP, NO_LABELDESC);
		ci_resolve_and_free_label(tx, elselabel);
		compile_statement(tx, is->is_elsepart);
		ci_resolve_and_free_label(tx, nextlabel);
	}
}

/*  If greater than MIN_JUMP_DENSITY percent of the possible case label
 *  values in a switch statement are used, we use a jump table, otherwise
 *  we use a chain of values.
 */
#define MIN_JUMP_DENSITY	80

static void
compile_switch_statement(tx, ss)
text_t *tx;
switch_stm_t *ss;
{
	case_labeled_stm_t **p_cs, **cslim;
	int ncase, minval, maxval, dist;
	labeldesc_t default_labeldesc;

	ci_compile_expression(tx, ss->ss_expr, EC_VALUE);

	ncase = ss->ss_ncase;
	cslim = ss->ss_cstab + ncase;

	/*  Make labels for all the switch statements
	 */
	for (p_cs = ss->ss_cstab; p_cs < cslim; ++p_cs)
		(*p_cs)->cs_labeldesc = make_label(tx);
	
	default_labeldesc = make_label(tx);
	if (ss->ss_default_cs != NULL)
		ss->ss_default_cs->cs_labeldesc = default_labeldesc;
	
	minval = (*ss->ss_cstab)->cs_val;
	maxval = cslim[-1]->cs_val;
	dist = maxval - minval;
	if (dist < 0)
		ci_panic("val botch in css");

	/*  Unsophisticated code generation here - if greater than
	 *  MIN_JUMP_DENSITY percent of the possible label values are
	 *  used, then use a jump table, otherwise use a chain of values.
	 */
	if (dist == 0 || (ncase * 100) / dist > MIN_JUMP_DENSITY) {
		int val;

		ci_code_opcode(tx, OC_SWITCH_ON_TABLE);
		ci_code_word(tx, dist + 1);
		ci_code_long(tx, minval);
		p_cs = ss->ss_cstab;
		for (val = 0; val <= dist; ++val) {
			labeldesc_t labeldesc;

			if (val + minval == (*p_cs)->cs_val) {
				labeldesc = (*p_cs)->cs_labeldesc;
				++p_cs;
			}
			else
				labeldesc = default_labeldesc;

			add_label_reference(tx, labeldesc, tx->tx_pc);
			tx->tx_pc += 2;
		}
		if (p_cs != cslim)
			ci_panic("case botch in css");
	}
	else {
		void (*codefunc)PROTO((text_t *ftx, long val));
		opcode_t opcode;

		if (dist <= MAX_BYTE) {
			opcode = OC_SWITCH_ON_CHAIN_B;
			codefunc = ci_code_byte;
		}
		else if (dist <= MAX_WORD) {
			opcode = OC_SWITCH_ON_CHAIN_W;
			codefunc = ci_code_word;
		}
		else {
			opcode = OC_SWITCH_ON_CHAIN_L;
			codefunc = ci_code_long;
		}
		ci_code_opcode(tx, opcode);
		ci_code_word(tx, ncase);
		ci_code_long(tx, minval);
		for (p_cs = ss->ss_cstab; p_cs < cslim; ++p_cs) {
			(*codefunc)(tx, (*p_cs)->cs_val - minval);
			add_label_reference(tx, (*p_cs)->cs_labeldesc, tx->tx_pc);
			tx->tx_pc += 2;
		}
	}
	add_label_reference(tx, default_labeldesc, tx->tx_pc);
	tx->tx_pc += 2;

	push_label(tx, LT_BREAK);
	compile_statement(tx, ss->ss_stm);
	pop_label(tx, LT_BREAK);
	ci_resolve_and_free_label(tx, default_labeldesc);

	for (p_cs = ss->ss_cstab; p_cs < cslim; ++p_cs)
		ci_resolve_and_free_label(tx, (*p_cs)->cs_labeldesc);
}

static void
compile_while_or_do_statement(tx, ws, is_while_statement)
text_t *tx;
while_stm_t *ws;
bool is_while_statement;
{
	labeldesc_t loop_label;

	push_label(tx, LT_BREAK);
	push_label(tx, LT_CONTINUE);

	if (is_while_statement)
		compile_continue_or_break_statement(tx, LT_CONTINUE);

	loop_label = make_label(tx);
	set_label_addr(tx, loop_label, tx->tx_pc);
	compile_statement(tx, ws->ws_stm);

	pop_label(tx, LT_CONTINUE);
	ci_compile_expression(tx, ws->ws_expr, EC_VALUE);
	(void) ci_code_jump_to_label(tx, OC_JUMP_IF_NON_ZERO, loop_label);

	pop_label(tx, LT_BREAK);
	ci_resolve_and_free_label(tx, loop_label);
}

static void
compile_for_statement(tx, fs)
text_t *tx;
for_stm_t *fs;
{
	labeldesc_t loop_label, test_label;

	push_label(tx, LT_BREAK);
	push_label(tx, LT_CONTINUE);

	compile_expression_statement(tx, fs->fs_init);
	test_label = ci_code_jump_to_label(tx, OC_JUMP, NO_LABELDESC);

	loop_label = make_label(tx);
	set_label_addr(tx, loop_label, tx->tx_pc);
	compile_statement(tx, fs->fs_stm);

	pop_label(tx, LT_CONTINUE);
	compile_expression_statement(tx, fs->fs_reinit);
	
	ci_resolve_and_free_label(tx, test_label);
	if (fs->fs_test != NULL) {
		ci_compile_expression(tx, fs->fs_test, EC_VALUE);
		(void) ci_code_jump_to_label(tx, OC_JUMP_IF_NON_ZERO, loop_label);
	}
	else
		(void) ci_code_jump_to_label(tx, OC_JUMP, loop_label);
	
	pop_label(tx, LT_BREAK);

	ci_resolve_and_free_label(tx, loop_label);
}

static void
compile_labeled_statement(tx, ls)
text_t *tx;
labeled_stm_t *ls;
{
	goto_label_t *gl;

	gl = ls->ls_goto_label;
	if (gl->gl_labeldesc == NO_LABELDESC) {
		push_label(tx, LT_GOTO);
		gl->gl_labeldesc = tx->tx_label_lists[(int)LT_GOTO]->ll_labeldesc;
	}
	set_label_addr(tx, gl->gl_labeldesc, tx->tx_pc);
	compile_statement(tx, ls->ls_stm);
}

static void
compile_goto_statement(tx, gl)
text_t *tx;
goto_label_t *gl;
{
	if (gl->gl_labeldesc == NO_LABELDESC) {
		push_label(tx, LT_GOTO);
		gl->gl_labeldesc = tx->tx_label_lists[(int)LT_GOTO]->ll_labeldesc;
	}
	(void) ci_code_jump_to_label(tx, OC_JUMP, gl->gl_labeldesc);
}

static void
compile_continue_or_break_statement(tx, label_type)
text_t *tx;
label_type_t label_type;
{
	label_list_t *ll;

	ll = tx->tx_label_lists[(int)label_type];
	if (ll == NULL)
		ci_panic("labellist botch in ccbs");
	(void) ci_code_jump_to_label(tx, OC_JUMP, ll->ll_labeldesc);
}

static void
compile_return_statement(tx, expr)
text_t *tx;
expr_t *expr;
{
	opcode_t opcode;
	typecode_t typecode;
	bool functrace;

	functrace = (tx->tx_flags & CI_CP_FUNCTRACE) != 0;

	if (expr == NULL) {
		opcode = functrace ? OC_TRACERET : OC_RET;
		ci_code_opcode(tx, opcode);
		return;
	}

	typecode = expr->ex_type->ty_code;

	if (typecode == TY_STRUCT || typecode == TY_UNION) {
		if (expr->ex_exprtype == ET_FUNC_CALL) {
			ci_code_opcode(tx, OC_PUSH_STRUCTRET_ADDR);
			ci_compile_expression(tx, expr, EC_ADDRESS);
			opcode = functrace ? OC_TRACERET : OC_RET;
			ci_code_opcode(tx, opcode);
		}
		else {
			ci_compile_expression(tx, expr, EC_ADDRESS);
			opcode = functrace ? OC_TRACERET_STRUCT : OC_RET_STRUCT;
			ci_code_opcode(tx, opcode);
			ci_code_long(tx, ci_typesize(expr->ex_lexinfo,
								expr->ex_type));
		}
	}
	else {
		ci_compile_expression(tx, expr, EC_VALUE);

		switch (expr->ex_type->ty_code) {
		case TY_FLOAT:
			opcode = functrace ? OC_TRACERET_FLOAT : OC_RET_FLOAT;
			break;
		case TY_DOUBLE:
			opcode = functrace ? OC_TRACERET_DOUBLE : OC_RET_DOUBLE;
			break;
		default:
			opcode = functrace ? OC_TRACERET_WORD : OC_RET_WORD;
			break;
		}
		ci_code_opcode(tx, opcode);
	}

}
