/* printf.h - header file for printf.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)printf.h	1.3 4/7/91 (UKC) */

ci_exec_result_t ups_printf PROTO((proc_t proc, code_id_t code_id, taddr_t *args, int nargs));
