/* o_lptrs.h - header file for o_lptrs.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)o_lptrs.h	1.2 4/7/91 (UKC) */

int get_info_depth PROTO((void));
void do_lptrs PROTO((int no_skip));
void set_update_lptrs PROTO((struct objst *obj, int recurse));
int get_indent PROTO((struct objst *obj));
struct lptrst *new_lptr PROTO((void));
void free_lptr_list PROTO((struct lptrst *lptr));
