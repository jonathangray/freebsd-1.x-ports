
/**********************************************************************
* %M%
* Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Name Sub-Menu Operations
***********************************************************************
**********************************************************************/
#ifndef lint
  static char Sccsid[] = "%W% %G%";
#endif

/*
 * Include files
 */
#include <stdio.h>
#include <string.h>
#include "curses_stuff.h"

#include "ss.h"
#include "getinput.h"
#include "disprange.h"
#include "menu_name.h"

extern  char    *getenv();

/*	Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 


/*	External Global variables
 *----------------------------------------------------------------------
 */



/*	Externally Accessible Functions
 ***********************************************************************
 */

void NameDefine()
/*----------------------------------------------------------------------
** Define a Name for a range of cells.  If no range is currently defined,
** then define a name for the current cell.
*/
{
    range = RangeGet();         /* Get range, if any, "" otherwise. */

    if (range[0] == NULL)       /* Null range */
        Sprintf(message, "** Define a Name for the current cell" );
    else
        Sprintf(message, "** Define a Name for the cell range %s", range);

    Message(message);
    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;

    Sprintf(line,"define [string range] \"%s\" %s", buff, range);
    modflg++;

    PROCESS_line;

} /* NameDefine() */

void NameErase()
/*----------------------------------------------------------------------
** Erase the name assigned to a range of cells.  If no range is
** currently defined, then prompt for a cell name to erase.
*/
{
    range = RangeGet();         /* Get range, if any, "" otherwise. */

    if (range[0] == NULL) {       /* Null range */
        Sprintf(message, "** Enter a Cell/Range Name to be erased" );
	Message(message);
	buff = gi_line();
	ABORT_AND_RETURN_IF_BUFF_NULL;
	
	Sprintf(line,"undefine [range] %s", buff);
	PROCESS_line;
	modflg++;

    } else {
        Sprintf(message, "** Erase the Name for the cell range %s ?", range);
	if (yn_ask(message) == 1) {
	    Sprintf(line,"undefine [range] %s", range);
	    linelim = 0;	
	    (void) yyparse ();
	    linelim = -1;
	    modflg++;
	}
    }
} /* NameErase() */

void NameShow()
/*----------------------------------------------------------------------
** List all Names that have been assigned to cells, or ranges
** of cells.  Pipe the output to 'sort' and then to a pager.  
** Therefore the list of names is sorted, and it will not run off the
** top of your display.
*/
{
#   define MAXCMD 160                   /* for ! command below */
#   ifndef DFLT_PAGER
#       define  DFLT_PAGER "more"       /* Default pager */
#   endif /* DFLT_PAGER */

    if(are_ranges()) {
        FILE *f;
        int pid;
        char px[MAXCMD] ;
        char *pager;

        (void) strcpy(px, "| sort | ");
        if(!(pager = getenv("PAGER")))
            pager = DFLT_PAGER;
        (void) strcat(px,pager);
        f = openout(px, &pid);
        if (!f) {
            Message("** Can't open pipe to sort");
            return;
        }
        list_range(f);
        closeout(f, pid);

    } else Message("** No Names have been defined");
    
} /* NameShow() */

/**********************************************************************
*       End
**********************************************************************/
