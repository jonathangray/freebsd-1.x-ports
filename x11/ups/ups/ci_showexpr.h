/* ci_showexpr.h - header file for ci_showexpr.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_showexpr.h	1.2 4/7/91 (UKC) */

char *ci_expr_to_english PROTO((expr_t *expr));

void ci_show_expr_and_type PROTO((expr_t *expr, type_t *type));
