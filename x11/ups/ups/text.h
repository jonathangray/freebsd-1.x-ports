/* text.h - public header file for text.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)text.h	1.5 5/3/91 (UKC) */

lno_t *addr_to_lno PROTO((func_t *f, taddr_t text_addr));
int rbrac_addr_to_lnum PROTO((func_t *f, taddr_t text_addr));
int addr_to_lnum PROTO((func_t *f, taddr_t text_addr));
taddr_t lnum_to_addr PROTO((func_t *f, int lnum));
taddr_t min_bpt_addr PROTO((func_t *f));
int map_lnum_to_addr PROTO((func_t *f, int lnum, taddr_t *p_addr));
preamble_id_t get_startup_code PROTO((func_t *f));
