/* ci_func.h - header file for ci_func.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_func.h	1.2 4/7/91 (UKC) */

goto_label_t *ci_name_to_label PROTO((identifier_t *id, bool is_definition));
func_t *ci_start_func PROTO((declaration_t *declaration));
void ci_check_func_decls PROTO((func_t *f));
expr_list_t *ci_check_func_call PROTO((expr_t *func_expr,
				       const char *funcname,
				       expr_list_t *arglist,
				       bool is_implicit_declaration));
void ci_end_func PROTO((statement_t *statement));
void ci_check_return_expr PROTO((expr_t *expr));
