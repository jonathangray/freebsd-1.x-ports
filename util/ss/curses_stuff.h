/**********************************************************************
* %W% %G%
*
* ss    :       A SpreadSheet Program
*
* Art's Spreadsheet program.          Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Header file for Curses-Related stuff
***********************************************************************
* 
**********************************************************************/

#ifndef curses_stuff_h
#  define curses_stuff_h

/*
 * For Dec/Ultrix you have to include cursesX.h in order to get the
 * Unix System-V curses stuff.
 */
#if defined(ULTRIX)
#  include <cursesX.h>
#else	/* not Ultrix */
#  include <curses.h>
#endif

#ifndef A_STANDOUT      /* Should be defined in curses.h */
#   define A_STANDOUT _STANDOUT
#endif

/*
 * Take care of some system specific curses definitions
 */
#ifndef A_CHARTEXT      /* Should be defined in curses.h */
# ifdef INTERNATIONAL
#   define A_CHARTEXT 0xff
# else
#   define A_CHARTEXT 0x7f
# endif
#endif

#endif /* ifndef curses_stuff_h */
/*----------------------------------------------------------------------
 *	End
 */
