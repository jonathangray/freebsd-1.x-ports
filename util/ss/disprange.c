/**********************************************************************
* %M%
* ss 	:	A SpreadSheet Program
*
* Art's Spreadsheet program.          Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Range Display/Manip functions.
***********************************************************************
* Functions for displaying ranges' on the spreadsheet.
* Also, utility functions for accessing/manipulating those ranges.
***********************************************************************
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
#include "disprange.h"

/*	Function Prototypes
 *----------------------------------------------------------------------
 */

/*	External Global variables
 *----------------------------------------------------------------------
 */
    extern int showneed;	/* From main.c */
    extern int  showrange;	/* ditto */
    extern int running;		/* ditto */
    extern int anychanged; 	/* ditto */
    extern int showexpr;	/* ditto */
    extern int ClearScreen;	/* ditto */

    extern int lastmx, lastmy;	/* From screen.c */
    extern int lastcol, lcols;	/* ditto */

/*********************************************************************/

void RangeToggle()
/*----------------------------------------------------------------------
** Toggle Range Display mode.
** -> If NOT in Range Display Mode, then START Range Display Mode.
** -> If IN Range Display Mode, then STOP Range Display Mode.
*/
{
  if (showrange == TRUE) {	/* currently IN Range mode */
    showrange = FALSE;
    
  } else {			/* currently NOT in Range mode */
    showrange = TRUE;
    showsr = currow;		/* Starting Row of Range */
    showsc = curcol;		/* Starting Col of Range */
  }
  
} /* RangeToggle() */

/* TEMP */
void startshow()
{
  showrange = 1;
  showsr = currow;
  showsc = curcol;
}
/* END TEMP */


/* insert the range we defined by moving around the screen, see startshow() */
void showdr()
{
    int     minsr, minsc, maxsr, maxsc;

    minsr = showsr < currow ? showsr : currow;
    minsc = showsc < curcol ? showsc : curcol;
    maxsr = showsr > currow ? showsr : currow;
    maxsc = showsc > curcol ? showsc : curcol;
    (void) sprintf (line+linelim,"%s", r_name(minsr, minsc, maxsr, maxsc));
}

char * RangeGet()
/*----------------------------------------------------------------------
** Get & Return the currently highlighted range.  Return "" if
** no range is currently highlighted.  TURNS OFF RANGE HIGHLIGHTING.
*/
{
    static char CurrentRange[8];	/* no need for more space */
    int     minsr, minsc, maxsr, maxsc;

    
    if (showrange == TRUE) {	/* Range IS being displayed */
        minsr = showsr < currow ? showsr : currow;
        minsc = showsc < curcol ? showsc : curcol;
        maxsr = showsr > currow ? showsr : currow;
        maxsc = showsc > curcol ? showsc : curcol;
	Sprintf (CurrentRange,"%s", r_name(minsr, minsc, maxsr, maxsc));
	RangeToggle();
	
    } else
	CurrentRange[0] = '\0';	/* Null Range */

    return CurrentRange;
    
} /* RangeGet() */

void RangeGetNum(minr,minc,maxr,maxc)
/*----------------------------------------------------------------------
** Get & Return the currently highlighted range -- In numeric form.
** Return the current cell location (as a range) if no range is
** currently highlighted.  TURNS OFF RANGE HIGHLIGHTING.
*/
   int *minr, *minc,	/* starting row/column of range selected */
       *maxr, *maxc;	/* ending   row/column of range selected */
{
    if (showrange == TRUE) {	/* Range IS being displayed */
	*minr = showsr < currow ? showsr : currow;
	*minc = showsc < curcol ? showsc : curcol;
	*maxr = showsr > currow ? showsr : currow;
	*maxc = showsc > curcol ? showsc : curcol;
	RangeToggle();
	
    } else {			/* No Range */
	*minr = currow;	*minc = curcol;
	*maxr = currow;	*maxc = curcol;
    }
} /* RangeGetNum() */


char *RangeForceInput()
/*----------------------------------------------------------------------
** Force the user to input a range (using cursor keys).  Only cursor
** motion keys are considered to be legal input.  Terminate with a
** <Tab> or <CR>
*/
{
    register int   c;		/* input */
    int	anychanged = FALSE;

    /* NOTE: Range display MUST be off before calling
     * this function.
     */

    Prompt("Enter Range.  <CR>/<Tab> to accept.");
    
    for(;;) {			/* Loop Forever... */
	update(anychanged); 	/* Update Screen */

#ifndef SYSV3			/* HP/Ux 3.1 this may not be wanted */
/**	(void) refresh();  **/	/* Unix SystemV R3 does a refresh in getch */ 
#endif
        c = nmgetch();		/* get next character of input */

	/*
	 * 1) If the input is a cursor-movement command, then
	 * deal with it.
	 */
	if (ProcessCursorCommands(c) != TRUE) {

	    /*
	     * 2) ELSE if it is a <Tab> then start range-show
	     *    mode ONLY if we are not already in range-show
	     *    mode.
	     */
	    if ( (c == kTAB) && (! showrange) )
		RangeToggle();

	    /*
	     * 3) ELSE a <CR> or a <Tab> (in range-show mode)
	     *    signals that the user is done entering a range.
	     */
	    else if ( (c == ctl('m')) || (c == kTAB) )
		return RangeGet();

/**	    else if (c == ' '){ 
***		Message("** aborted"); 
***		return NULL;
***	    }
**/		
	    /*
	     * 4) ELSE, bogus input, beep!
	     */
	    else
		beep();
	}
    }  /* for */

} /* RangeForceInput() */

/**********************************************************************
*       End
**********************************************************************/
