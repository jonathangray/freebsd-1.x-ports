/*
 * -> writing "help" has begun, but only just, and so it is
 *    currently not installed.  I'm not sure whether it is 
 *    worth the work involved to install it.
 */

/**********************************************************************
* %M%
* ss 	:	A SpreadSheet Program
*
* Art's Spreadsheet program.          Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Help Module
***********************************************************************
* Functions for displaying help
**********************************************************************/
#ifndef lint
  static char Sccsid[] = "%W% %G%";
#endif

/*
 * Include files
 */
#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include "curses_stuff.h"
#include <ctype.h>

#include "ss.h"
#include "keys.h"

/*	External Global variables
 *----------------------------------------------------------------------
 */
    extern int showneed;	/* From main.c */
    extern int showrange;	/* ditto */
    extern int running;		/* ditto */
    extern int anychanged; 	/* ditto */
    extern int showexpr;	/* ditto */
    extern int ClearScreen;	/* ditto */

    extern int FullUpdate;	/* ditto */

    extern int lastmx, lastmy;	/* From screen.c */
    extern int lastcol, lcols;	/* ditto */

/*	Internal Global variables
 *----------------------------------------------------------------------
 */

#define HelpSize 21
static char *Help[] =  {
 "- Top-Level Commands: --------------------------------------------------",
 "  / : Main Menu |  0-9 @ = - + . : Number or Function Entry",
 "  ? : Help      |  \" < >         : Centred, Right, Left Justified String",
 "      Any other input results in entering a Left Justifed string",
 "",
 "- Cursor Movement: -----------------------------------------------------",
 "   <Left>, ^B : Left one Cell  | <Home>, <Esc> < : Go to A0",
 "  <Right>, ^F : Right one Cell | <End>, <Esc> > : End of Col",
 "   <Down>, ^N : Down one Cell  | <PgUp>, <Esc> v : Page Up",
 "     <Up>, ^P : Up one Cell    | <PgDn>, ^V,     : Page Down",
 "           ^T : Top of Col     | <Sh><Left>, ^X <    : Page Left",
 "           ^A : Start of Row   | <Sh><Right>, ^X >    : Page Right",
 "           ^E : End of Row     | <Esc> b : Back, to previous valid Cell",
 "                               | <Esc> f : Forward, to next valid Cell",
 "- Operations: ----------------------------------------------------------",
 "  ^X-v           : Edit Cell Value  | ^L    : Redraw screen                ",
 "  ^X-l           : Edit Cell Label  | ^R    : Redraw; Highlight Values     ",
 "  ^G             : Goto a Cell      | ^K    : Redraw; Highlight Expressions",
 "  ^W             : Mark Cell        | <Tab> : Toggle Range display. ",
 "  ^Y             : Yank marked Cell | ^Z    : Stop program",
 " <Del>, <BS>, ^D : Erase Cell",
  };
/*
 * Keep an alternate arrangement also:
 *
- Top-Level Commands: --------------------------------------------------
              /       : Main Menu",
              ?       : Help",
        0-9 @ = - + . : Number or Function Entry",
           \" < >     : Centred, Right or Left Justified String",

    Any other input results in entering a Left Justifed string",

- Cursor Movement: -----------------------------------------------------
      <Left>, ^B      : Left one Cell
     <Right>, ^F      : Right one Cell
      <Down>, ^N      : Down one Cell
        <Up>, ^P      : Up one Cell
      <Home>, <Esc> < : Go to A0
       <End>, <Esc> > : End of Col
      <PgUp>, <Esc> v : Page Up
      <PgDn>, ^V,     : Page Down
  <Sh><Left>, ^X <    : Page Left
 <Sh><Right>, ^X >    : Page Right
              ^T      : Top of Col
              ^A      : Start of Row
              ^E      : End of Row 
              <Esc> b : Back, to previous valid Cell
              <Esc> f : Forward, to next valid Cell
             <Return> : Move right or down (depends on -C,-R program flag)

- Operations: ----------------------------------------------------------
              ^G      : Goto a Cell
              ^W      : Mark Cell
              ^Y      : Yank previously marked Cell
              ^X-v    : Edit Cell Value
              ^X-l    : Edit Cell Label
      <Del>, <BS>, ^D : Erase the current Cell (Value and Label)
              ^L      : Redraw screen                 | 
              ^R      : Redraw; Highlight Values      |
              ^K      : Redraw; Highlight Expressions |
              <Tab>   : Toggle Range display. 
              ^Z      : Stop program
------------------------------------------------------------------------
**/

/*	Internal Function Prototypes
 ***********************************************************************
 */


/*	Externally Accessible Functions
 ***********************************************************************
 */

void help(context)
/*----------------------------------------------------------------------
** 
*/
  int context;		/* Help Context */
{
    int row = 3;
    int x;

    (void) move(0,0);
    (void) clrtobot();

    switch (context) {
      case HELP:	/* top level help */
	for (x=0; x<HelpSize; x++)
	  mvaddstr(row++, 0, Help[x]);
	break;

      default:		/* unknown help context */
	break;		/* shouldn't happen */
    }

    Prompt("** Strike any key to continue");
    (void) nmgetch();

    FullUpdate++;
    Refresh();	

} /* help() */


/**********************************************************************
*       End
**********************************************************************/
