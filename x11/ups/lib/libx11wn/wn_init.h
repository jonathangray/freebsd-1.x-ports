/* wn_init.h - header file for wn_init.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn_init.h	1.2 4/7/91 (UKC) */

void _wn_add_to_unmunge_list PROTO((const char **args, int nargs));
const char *_wn_reason PROTO((void));
int _wn_init PROTO((void));
int _wn_make_window PROTO((swin_t *w, const char *name, int is_mainwin));
void _wn_map_X_window PROTO((swin_t *w, swin_t *iw));

#ifdef SUNVIEW
void _wn_make_retained_pixrect PROTO((struct pixwin *pw, int width, int height));
#endif

#ifdef X11
void wn_get_X11_info PROTO((Display **p_display, Window *p_win, GC *p_gc, Colormap *p_colormap));
#endif

extern const char *_wn_Sysfont_file;

