/* ci_compile.h - header file for the C interpreter compilation routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_compile.h	1.7 4/7/91 (UKC) */

#define IS_LOCAL_CLASS(class)	((class) == CL_AUTO || (class) == CL_ARG)

typedef enum { RLT_RELOC_DATA, RLT_RELOC_BSS, RLT_COPY_TO_DATA } reloc_type_t;

typedef struct relocst {
	reloc_type_t rl_reloc_type;
	taddr_t rl_location;
	taddr_t rl_addr;
	long rl_count;
	struct relocst *rl_next;
} reloc_t;

typedef struct labelrefst {
	taddr_t lr_location;
	struct labelrefst *lr_next;
} labelref_t;

typedef struct labelst {
	taddr_t lb_addr;
	labelref_t *lb_refs;
} branch_label_t;

typedef struct label_listst {
	labeldesc_t ll_labeldesc;
	struct label_listst *ll_next;
} label_list_t;

typedef enum { LT_GOTO, LT_BREAK, LT_CONTINUE, LT_NLABEL_TYPES } label_type_t;

typedef struct libfuncst {
	const char *lf_name;
	taddr_t lf_addr;
	int lf_index;
	struct libfuncst *lf_next;
} libfunc_t;

typedef struct textst {
	alloc_id_t tx_alloc_id;
	textword_t *tx_text;
	taddr_t tx_pclim;
	taddr_t tx_pc;
	taddr_t tx_sp;
	taddr_t tx_max_sp;
	taddr_t tx_bss_addr;
	taddr_t tx_data_addr;
	branch_label_t **tx_labels;
	lno_t *tx_lnos;
	lno_t *tx_last_lno;
	fil_t *tx_curfil;		/* File we are currently compiling */
	initlist_t *tx_initlist;
	reloc_t *tx_relocs;
	int tx_labellim;
	label_list_t *tx_label_lists[(int)LT_NLABEL_TYPES];
	func_t *tx_funclist;
	var_t *tx_varlist;
	libfunc_t *tx_direct_libfuncs;
	libfunc_t *tx_indirect_libfuncs;
	int tx_num_funcs;
	int tx_num_direct_libfuncs;
	int tx_num_indirect_libfuncs;
	ci_report_error_func_t tx_report_error_func;
	ci_getaddrproc_t tx_getaddrproc;
	ci_checkarg_proc_t tx_checkarg_proc;
	ci_regno_to_addr_proc_t tx_regno_to_addr_proc;
	bool tx_want_reg_relocs;
	reg_reloc_t *tx_reg_relocs;
	unsigned long tx_flags;
	bool tx_had_error;
	bool tx_have_proc_vars;
} text_t;

void ci_code_byte PROTO((text_t *tx, long byte));
void ci_code_word PROTO((text_t *tx, long word));
void ci_code_long PROTO((text_t *tx, long l));
void ci_code_opcode PROTO((text_t *tx, opcode_t opcode));
void ci_code_generic_opcode PROTO((text_t *tx, opcode_t byte_opcode,
				   stackword_t arg));
void ci_code_constpush PROTO((text_t *tx, long val));
labeldesc_t ci_code_jump_to_label PROTO((text_t *tx, opcode_t opcode,
							labeldesc_t labeldesc));
void ci_resolve_and_free_label PROTO((text_t *tx, labeldesc_t labeldesc));
void ci_add_relocation PROTO((text_t *tx, reloc_type_t reloc_type,
				taddr_t location, taddr_t addr, long count));

int ci_funcname_to_index PROTO((text_t *tx, lexinfo_t *lx, const char *name,
							bool error_if_not_found));

taddr_t ci_libvarname_to_addr PROTO((text_t *tx, lexinfo_t *lx, const char *name));

func_t *ci_addr_to_func PROTO((func_t *funclist, taddr_t addr));
