/* wn_misc.h - header file for wn_misc.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn_misc.h	1.6 25/4/92 (UKC) */

void wn__do_xflush PROTO((void));
char *wn__e_malloc PROTO((size_t nbytes));
char *wn__e_realloc PROTO((char *ptr, size_t nbytes));
void wn__panic PROTO((const char *mesg));
#ifdef X11
void wn__send_selection PROTO((XEvent *event));
#endif
#ifdef SUNVIEW
int wn__setup_sunview_wakeup_pipe PROTO((void));
extern int wn__Lost_selection;
#endif
