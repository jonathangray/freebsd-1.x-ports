/* wn_event.h - header file for wn_event.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn_event.h	1.4 4/7/91 (UKC) */

void _wn_catch_sigwsts PROTO((void));
void _wn_change_wn_fdmask PROTO((int fd));
void _wn_catch_sigwinch PROTO((void));
void _wn_enable_self_deiconise PROTO((void));
void _wn_set_inmode PROTO((swin_t *w, int mode));
void _wn_set_sunview_wakeup_fd PROTO((int wakeup_fd));

#ifdef X11
/*  Last window that we got an event on.  Used to speed up lookups
 *  in get_event.  We have to have it here so wn_close_window can
 *  set it to NULL, to avoid referencing destroyed windows.
 */
extern Window wn__Last_event_window;
#endif
