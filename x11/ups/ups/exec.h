/* exec.h - header file for exec.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)exec.h	1.9 26/7/92 (UKC) */

void write_target_core PROTO((void));
int attach_to_process PROTO((const char *name, int pid));
void kill_or_detach_from_target PROTO((void));
void do_menu_target_command PROTO((int rv));

/*  Address of builtin ups functions.  Can be any addresses that can't be
 *  mistaken for target function address.
 */
#define STOP_ADDR 		1
#define PRINTF_ADDR		2
#define SET_EXPR_RESULT_ADDR	3

extern const char Stop_funcname[];
extern const char Set_expr_result_funcname[];

#ifdef SYMTAB_H_INCLUDED
#ifdef CI_H_INCLUDED

/*  Return values for special functions.
 */
#define STOP	CI_ER_USER1

ci_exec_result_t call_target_function PROTO((code_id_t code_id, taddr_t addr,
				taddr_t *args, int nargs, taddr_t *p_res));
#endif
void exec_to_lnum PROTO((func_t *f, int lnum));

#endif

#ifdef PROC_H_INCLUDED
proc_t start_target PROTO((stopres_t *p_whystopped));
int get_restart_sig PROTO((proc_t proc));
void refresh_target_display PROTO((proc_t proc, stopres_t whystopped));
#endif
