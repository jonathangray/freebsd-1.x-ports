/********************************************************************
 * lindner
 * 3.3
 * 1993/07/30 17:31:39
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/CURcurses.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: CURcurses.h
 * Header file access methods for CURcurses.c
 *********************************************************************
 * Revision History:
 * CURcurses.h,v
 * Revision 3.3  1993/07/30  17:31:39  lindner
 * Mods to support AskP:
 *
 * Revision 3.2  1993/04/15  21:25:20  lindner
 * Fixes for CURbox() definitions
 *
 * Revision 3.1.1.1  1993/02/11  18:02:56  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.2  1992/12/31  05:57:38  lindner
 * Mods for VMS
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#include <ctype.h>

#if defined(ultrix)
#  include <cursesX.h>
#else
#  include <curses.h>
#endif

#include "boolean.h"
#include "STRstring.h"

#ifdef KEY_RIGHT
#define SYSVCURSES
#endif

struct CursesStruct {
     WINDOW *Screen;
     String *Termtype;

     /** Termcap/terminfo stuff **/
     String *Clearscreen;
     String *AudibleBell;
     String *Highlighton;
     String *Highlightoff;
     
     boolean inCurses;
     
     int COLS;
     int ROWS;
     
     void (*sigtstp)();
     void (*sigwinch)();
};

typedef struct CursesStruct CursesObj;
typedef struct CursesStruct *CursesObjp;

#ifndef SYSVCURSES
#define KEY_DOWN        0402           /* The four arrow keys ...*/
#define KEY_UP          0403
#define KEY_LEFT        0404
#define KEY_RIGHT       0405           
#define KEY_NPAGE       0522           /* Next page */
#define KEY_PPAGE       0523           /* Previous page */
#define KEY_ENTER       0527
#define KEY_BACKSPACE   0407
#endif

/*
 * Definitions for character graphics
 */

#if defined(SYSVCURSES) && !defined(ultrix) && !defined(hpux) && !defined(_AUX_SOURCE)
# define BOX_UL          ('l'|A_ALTCHARSET)
# define BOX_UR          'k'|A_ALTCHARSET
# define BOX_LL          'm'|A_ALTCHARSET
# define BOX_LR          'j'|A_ALTCHARSET
# define BOX_VLINE       'x'|A_ALTCHARSET
# define BOX_HLINE       'q'|A_ALTCHARSET
#else
# define BOX_UL          '+'
# define BOX_UR          '+'
# define BOX_LL          '+'
# define BOX_LR          '+'
# define BOX_VLINE       '|'
# define BOX_HLINE       '-'
#endif

#if defined(ISC)
/*
 * These work for ISC sysv3.2, they might work for other IBM style things too.
 */
# define BOX_UL          ('Z'|A_ALTCHARSET)
# define BOX_UR          '?'|A_ALTCHARSET
# define BOX_LL          '@'|A_ALTCHARSET
# define BOX_LR          'Y'|A_ALTCHARSET
# define BOX_VLINE       '3'|A_ALTCHARSET
# define BOX_HLINE       'D'|A_ALTCHARSET
#endif

#if defined(VMS) || defined(__convex__) || defined(sequent)
#if !defined(cbreak)
#define cbreak crmode
#endif
#endif

/*
 * Request types....  More to be added later..
 */

#define CUR_LABEL  1
#define CUR_PROMPT 2
#define CUR_PASSWD 3


/*
 * access functions
 */

#define CURgetScreen(a)    ((a)->Screen)
#define CURgetTerm(a)      (STRget((a)->Termtype))
#define CURgetCLS(a)       (STRget((a)->Clearscreen))
#define CURgetBell(a)      (STRget((a)->AudibleBell))
#define CURgetHighon(a)    (STRget((a)->Highlighton))
#define CURgetHighoff(a)   (STRget((a)->Highlightoff))

#ifdef VMS
#define CURsetScreen(a,b)  ((b),(a)->Screen=stdscr)
#else
#define CURsetScreen(a,b)  ((a)->Screen=(b))
#endif
#define CURsetTerm(a,b)    (STRset((a)->Termtype, (b)))
#define CURsetCLS(a,b)     (STRset((a)->Clearscreen,(b)))
#define CURsetBell(a,b)    (STRset((a)->AudibleBell,(b)))
#define CURsetHighon(a,b)  (STRset((a)->Highlighton,(b)))
#define CURsetHighoff(a,b) (STRset((a)->Highlightoff,(b)))
#define CURsetSIGTSTP(a,b) ((a)->sigtstp=(b))
#define CURsetSIGWINCH(a,b) ((a)->sigwinch=(b))


CursesObj *CURnew();
void      CURinit();
void      CURcenterline();
void      CURenter();
void      CURexit();
int       CURgetstr();
int       CURgetch();
void      CURresize();
int       CURoutchar();
int       CURGetOneOption();
void      CURBeep();
void      CURwenter();
