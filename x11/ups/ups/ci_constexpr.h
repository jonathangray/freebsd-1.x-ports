/* ci_constexpr.h - header file for ci_constexpr.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_constexpr.h	1.3 4/7/91 (UKC) */

bool ci_evaluate_constant_expression PROTO((expr_t *expr, const char *what,
				   bool must_be_integral, constval_t *cv));
