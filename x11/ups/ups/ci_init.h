/* ci_init.h - header file for ci_init.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_init.h	1.2 4/7/91 (UKC) */

initlist_t *ci_make_var_initialiser PROTO((var_t *v, initexpr_t *in));
