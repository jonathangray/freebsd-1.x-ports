/* ci.h - public header file for the ci_* package */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci.h	1.14 12/9/92 (UKC) */

#define CI_H_INCLUDED

typedef struct { int pi_dummy; } *parse_id_t;
typedef struct { int ci_dummy; } *code_id_t;

typedef enum { MA_IGNORE, MA_WARNING_ONLY, MA_DEFAULT } ci_message_action_t;

typedef bool (*ci_report_error_func_t)PROTO((const char *filename,
					     int lnum, int cnum,
					     const char *mesg));

const char *ci_translate_escape PROTO((const char *s, int *p_res));

#ifdef SYMTAB_H_INCLUDED
type_t *ci_code_to_type PROTO((typecode_t code));
enum_member_t *ci_make_enum_member PROTO((alloc_id_t alloc_id,
					  const char *name, long val));
aggr_or_enum_def_t *ci_make_aggr_or_enum_def PROTO((alloc_id_t alloc_id,
						    const char *tag,
						    typecode_t typecode,
						    type_t *type));
func_t *ci_make_func PROTO((alloc_id_t alloc_id, const char *name,
				taddr_t addr, symtab_id_t symtab_id,
				fil_t *fil, func_t *next, funclist_t *fl));
var_t *ci_make_var PROTO((alloc_id_t alloc_id, const char *name, class_t class,
						type_t *type, taddr_t addr));
var_t *ci_push_vars PROTO((var_t *v, var_t *list));
type_t *ci_make_undef_type PROTO((alloc_id_t alloc_id, const char *tag,
					typecode_t typecode, type_t *type));
type_t *ci_make_bitfield_type PROTO((alloc_id_t alloc_id, typecode_t typecode,
						int bit_offset, int bit_width));
type_t *ci_make_type PROTO((alloc_id_t alloc_id, typecode_t code));
void ci_init_type PROTO((type_t *type, typecode_t typecode));
block_t *ci_make_block PROTO((alloc_id_t alloc_id, block_t *parent));
long ci_typesize PROTO((lexinfo_t *lx, type_t *type));
const char *ci_basetype_name PROTO((type_t *type));
char *ci_type_to_english PROTO((type_t *type, bool resolve_typedefs));
char *ci_type_to_decl PROTO((type_t *type, bool resolve_typedefs));
aggr_or_enum_def_t *ci_apply_to_aelist PROTO((
	       aggr_or_enum_def_t *aelist,
	       aggr_or_enum_def_t *(*func)(aggr_or_enum_def_t *ae, const char *farg),
	       const char *arg));
#endif

void ci_add_message_action PROTO((const char *pat, ci_message_action_t action));

#ifdef SYMTAB_H_INCLUDED
typedef int (*ci_resolve_name_func_t)PROTO((const char *name, var_t **p_v));

parse_id_t ci_parse_file PROTO((parse_id_t parse_id, const char *filename,
				block_t *block, unsigned long flags,
				ci_report_error_func_t report_error_func,
				ci_resolve_name_func_t resolve_name_func,
				const char *(*getline_func)(char *arg),
				char *getline_arg));

/*  We complain about a return statement in the special function $start
 *  with this message.  We put the message here so that the error  output
 *  function can special case it.  We do all this for the benifit of ups,
 *  where we want to give an error message if the user puts a return
 *  statement in breakpoint code (as it won't do what they expect).
 */
extern const char ci_Illegal_return_from_start_message[];

/* Flags for ci_compile_program.
 */
#define CI_MAKE_EXTERNS_TOPLEVEL 0x0001 /* Treat local extern decls like pcc */
#define CI_DONT_PANIC	0x0004	/* Error message rather than panic on botch */

typedef enum {
	CI_DATA,
	CI_DIRECT_LIBFUNC,
	CI_INDIRECT_LIBFUNC,
	CI_UNDEFINED
} ci_nametype_t;

typedef ci_nametype_t (*ci_getaddrproc_t)PROTO((const char *name, taddr_t *p_addr));

typedef bool (*ci_checkarg_proc_t)PROTO((type_t *type, int nargs, int argn,
								     long *p_val));
typedef unsigned long (*ci_regno_to_addr_proc_t)PROTO((int regno));
typedef unsigned long (*ci_get_regaddr_proc_t)PROTO((char *arg, int regno));

code_id_t ci_compile_program PROTO((parse_id_t parse_id,
				    ci_report_error_func_t report_error,
				    ci_getaddrproc_t getaddrproc,
				    ci_checkarg_proc_t checkarg_proc,
				    ci_regno_to_addr_proc_t regno_to_addr_proc,
				    ci_get_regaddr_proc_t get_regaddr_proc,
				    char *get_regaddr_proc_arg,
				    unsigned long flags,
				    int argc, const char **argv));

#define CI_CP_CHECKSP	 0x0001	/* Generate instructions to runtime check the sp */
#define CI_CP_FUNCTRACE	 0x0002	/* Emit code to print a function call trace */
#define CI_CP_DONT_PANIC 0x0004	/* Error message rather than panic on botch */
#define CI_CP_CHECK_DIV	 0x0008	/* Generate divide by zero checking div instrs */

#endif

void ci_free_code_id PROTO((code_id_t code_id));
void ci_free_parse_id PROTO((parse_id_t parse_id));

typedef unsigned long ci_opcode_t;
ci_opcode_t ci_install_trap_instruction PROTO((code_id_t code_id, long location));
void ci_uninstall_trap_instruction PROTO((code_id_t code_id, long location,
								ci_opcode_t opcode));

void ci_disassemble PROTO((parse_id_t parse_id, code_id_t code_id));

typedef enum {
	CI_ER_CONTINUE,		/* for indirect functions */
	CI_ER_TRAP,
	CI_ER_READDATA_FAILED,
	CI_ER_WRITEDATA_FAILED,
	CI_ER_INDIRECT_CALL_FAILED,
	CI_ER_STACK_OVERFLOW,
	CI_ER_DIVISION_BY_ZERO,
	CI_ER_ARITHMETIC_EXCEPTION,
	CI_ER_BAD_MA_COUNT,
	CI_ER_USER1,
	CI_ER_USER2,
	CI_ER_USER3
} ci_exec_result_t;

const char *ci_exec_result_to_string PROTO((ci_exec_result_t res));

typedef int (*ci_readproc_t)PROTO((unsigned long addr, char *buf, int nbytes));
typedef int (*ci_writeproc_t)PROTO((unsigned long addr, const char *buf,
									int nbytes));
typedef ci_exec_result_t (*ci_indirect_call_proc_t)PROTO((code_id_t code_id,
					     unsigned long addr,
					     unsigned long *args,
					     int nargs,
					     unsigned long *p_res));

void ci_initialise_code PROTO((code_id_t code_id, bool reset_data));

ci_exec_result_t ci_execute_code PROTO((code_id_t code_id,
			    unsigned long procfp, unsigned long procap,
			    ci_readproc_t readproc, ci_writeproc_t writeproc,
			    ci_indirect_call_proc_t indirect_call_proc));

bool ci_is_ci_addr PROTO((code_id_t code, unsigned long addr));
