
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
#ifdef NCURSES
#include <ncurses.h>
#else
#include <curses.h>   /* everything else */
#endif /* NCURSES */
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

#ifdef UNDERLINE_LINKS
#define start_bold()      setattr(_UNDERLINE)
#define stop_bold()       clrattr(_UNDERLINE)
#define start_underline() setattr(_BOLD)
#define stop_underline()  clrattr(_BOLD)
#else /* not UNDERLINE_LINKS */
#define start_bold()      setattr(_BOLD)
#define stop_bold()       clrattr(_BOLD)
#define start_underline() setattr(_UNDERLINE)
#define stop_underline()  clrattr(_UNDERLINE)
#endif /* UNDERLINE_LINKS */
#define start_reverse()   setattr(_REVERSE)
#define wstart_reverse(a)   wsetattr(a,_REVERSE)
#define wstop_underline(a)  wclrattr(a,_UNDERLINE)
#define stop_reverse()    clrattr(_REVERSE)
#define wstop_reverse(a)    wclrattr(a,_REVERSE)

#else /* NOT VMS */

#ifdef UNDERLINE_LINKS
#define start_bold()      attrset(A_UNDERLINE)
#define stop_bold()       attroff(A_UNDERLINE)
#define start_underline() attrset(A_BOLD)
#define stop_underline()  attroff(A_BOLD)
#else /* not UNDERLINE_LINKS */
#define start_bold()      attrset(A_BOLD)
#define stop_bold()       attroff(A_BOLD)
#define start_underline() attrset(A_UNDERLINE)
#define stop_underline()  attroff(A_UNDERLINE)
#endif /* UNDERLINE_LINKS */
#define start_reverse()   attrset(A_REVERSE)
#define wstart_reverse(a)   wattrset(a,A_REVERSE)
#define stop_reverse()    attroff(A_REVERSE)
#define wstop_reverse(a)    wattroff(a,A_REVERSE)
#endif /* VMS */

#else /* not FANCY_CURSES */
#define start_bold()      standout()  
#define start_underline() 1  /* nothing */
#define start_reverse()   standout()
#define wstart_reverse(a)   wstandout(a)
#define stop_bold()       standend()  
#define stop_underline()  1  /* nothing */
#define stop_reverse()    standend()
#define wstop_reverse(a)    wstandend(a)
#endif /* FANCY_CURSES */

#endif /* LYCURSES_H */
