
#ifndef LYCURSES_H
#define LYCURSES_H

#ifdef VMS
#define FANCY_CURSES
#endif

#ifdef ULTRIX
#include <cursesX.h>  /* ultrix */
#else
#ifdef SUN
#undef TRUE  /* to prevent parse error :( */
#undef FALSE
#include "curses.h"   /* SUN */
#else
#include <curses.h>   /* everything else */
#endif /* SUN */
#endif /* ULTRIX */

extern int LYlines;  /* replaces LINES */
extern int LYcols;   /* replaces COLS */

#ifndef HTUTILS_H
#include "HTUtils.h"
#endif

extern void start_curses PARAMS(());
extern void stop_curses PARAMS(());
extern BOOLEAN setup PARAMS((char *terminal));

#ifdef VMS
extern void VMSexit();
extern int ttopen();
extern int ttclose();
extern int ttgetc();
extern void *vsignal PARAMS((int sig, void (*func)()));
#endif /* VMS */

/* define curses functions */
#ifdef FANCY_CURSES

#ifdef VMS
#define start_bold()      setattr(_BOLD)
#define start_underline() setattr(_UNDERLINE)
#define start_reverse()   setattr(_REVERSE)
#define stop_bold()       clrattr(_BOLD)
#define stop_underline()  clrattr(_UNDERLINE)
#define stop_reverse()    clrattr(_REVERSE)
#else
#define start_bold()      attrset(A_BOLD)
#define start_underline() attrset(A_UNDERLINE)
#define start_reverse()   attrset(A_REVERSE)
#define stop_bold()       attroff(A_BOLD)
#define stop_underline()  attroff(A_UNDERLINE)
#define stop_reverse()    attroff(A_REVERSE)
#endif /* VMS */

#else /* not FANCY_CURSES */
#define start_bold()      standout()  
#define start_underline() 1  /* nothing */
#define start_reverse()   standout()
#define stop_bold()       standend()  
#define stop_underline()  1  /* nothing */
#define stop_reverse()    standend()
#endif /* FANCY_CURSES */

#endif /* LYCURSES_H */
