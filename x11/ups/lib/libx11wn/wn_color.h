/* wn_color.h - header file for wn_color.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn_color.h	1.3 4/7/91 (UKC) */

void _wn_set_fgbg PROTO((int is_fg, char *sred, char *sgreen, char *sblue));
void _wn_init_sunview_colors PROTO((swin_t *w, int is_mainwin));

extern int wn__Use_mono;
