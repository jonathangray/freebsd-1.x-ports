/* ci_compile_expr.h - header file for ci_compile_expr.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_compile_expr.h	1.3 4/7/91 (UKC) */

typedef enum { EC_ADDRESS, EC_EFFECT, EC_VALUE } expr_context_t;

typedef enum { AT_CI_ADDR, AT_PROC_ADDR } addrtype_t;

addrtype_t ci_compile_expression PROTO((text_t *tx, expr_t *expr,
						expr_context_t expr_context));
void ci_code_assign PROTO((text_t *tx, type_t *type,
					bool want_value, addrtype_t addrtype));

addrtype_t ci_compile_var_reference PROTO((text_t *tx, var_t *v,
					lexinfo_t *lx, expr_context_t context));

void ci_do_static_initialisations PROTO((text_t *tx, initlist_t *initlist,
					 taddr_t data_addr, taddr_t bss_addr,
					 char *data_copy));
