/* wn_icon.h - header file for wn_icon.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn_icon.h	1.2 4/7/91 (UKC) */

int _wn_create_icon_win PROTO((swin_t *w, swin_t *main_w, const char *name));
void _wn_wzoom PROTO((Window oldwin, Window newwin));
void _wn_enable_zoom PROTO((void));
void _wn_draw_icon_window PROTO((int wn));
