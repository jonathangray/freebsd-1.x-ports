/* regex.h - prototypes for Ozan Yigit's regex functions in regex.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)regex.h	1.2 7/4/91 (UKC) */

const char *re_comp PROTO((const char *pat));
int re_exec PROTO((const char *lp));
int e_re_exec PROTO((const char *lp, int offset, int *p_start, int *p_end));

void re_fail PROTO((const char *msg, int op));
