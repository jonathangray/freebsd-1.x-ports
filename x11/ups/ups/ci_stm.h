/* ci_stm.h - header file for ci_stm.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_stm.h	1.2 4/7/91 (UKC) */

statement_t *ci_make_labeled_statement PROTO((identifier_t *id,
						     statement_t *statement));

statement_t *ci_make_goto_statement PROTO((identifier_t *id));

statement_t *ci_make_case_labeled_statement PROTO((bool in_switch,
						   expr_t *label_expr,
						   statement_t *statement));

statement_t *ci_make_expression_statement PROTO((expr_t *expr));

statement_t *ci_make_if_statement PROTO((expr_t *expr,
					 statement_t *ifpart,
					 statement_t *elsepart));

statement_t *ci_make_switch_statement PROTO((expr_t *expr, statement_t *statement));

statement_t *ci_make_while_statement PROTO((statement_type_t statement_type,
						   expr_t *expr,
						   statement_t *statement));

statement_t *ci_make_for_statement PROTO((expr_t *init,
					  expr_t *test,
					  expr_t *reinit,
					  statement_t *statement,
					  lexinfo_t *lx));

statement_t *ci_make_continue_statement PROTO((bool in_loop, lexinfo_t *lx));

statement_t *ci_make_break_statement PROTO((bool in_loop_or_switch, lexinfo_t *lx));

statement_t *ci_make_return_statement PROTO((expr_t *expr, lexinfo_t *lx));
