/* core.h - header file for core.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)core.h	1.5 4/7/91 (UKC) */

int core_getreg PROTO((int regno, taddr_t *p_val));
int open_corefile PROTO((const char *corename, const char *textname,
				 bool user_gave_core, taddr_t data_addr));
int core_get_lastsig PROTO((void));
taddr_t get_max_stack_addr PROTO((void));
int core_dread PROTO((taddr_t addr, char *buf, int nbytes));
