/**********************************************************************
* %M%
* Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Edit Menu Operations
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
#include "menu_edit.h"

/*	Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 

/*	External Global variables
 *----------------------------------------------------------------------
 */


/*	Externally Accessible Functions
 ***********************************************************************
 */

/*
** NOTE: How about making some sort of internal Cell Buffer.  Then
** write Cut/Copy/Paste Functions to replace the current Copy/Erase
** functions.  FUTURE WORK, for now live with the kludge.
*/

void EditCopy()
/*----------------------------------------------------------------------
** Copy a range of cells.  Copy the current cell, if no range is
** currently defined.  Prompt the user to Enter a Destination Range.
*/
{
    static char saverange[10];		/* To save the first range */
    /* need to make special arrangements, because we are handling
     * more than one range.
     */

    range = RangeGet();		/* Get range, if any, "" otherwise. */

    
    /*
     * Prompt the user to enter a destination range
     */
    if (range[0] == NULL) {	/* Null range */
	static char *curcell;		/* to hold current cell */
	curcell = v_name(currow, curcol); /* Get current cell */
	Sprintf(saverange, "%s:%s", curcell, curcell);
	     /* record the current cell as the starting range */
	

	Sprintf(message, "** Copy the Current Cell to... (Enter a range)" );

    } else {
	Sprintf(message, "** Copy the Cell Range %s to... (Enter a range)",
		range);
	strcpy(saverange, range); /* Save the starting range */
    }
    
    Message(message);

/*
 * BUG: No way out of this operation!  MUST enter a range
 * and the copy WILL take place.
 */
      
    range = RangeForceInput();	/* Get Destination range */
    if (range[0] == NULL) 	/* Null range */
	range = r_name(currow, curcol, currow, curcol);

    ClearMessage; 
    Sprintf(line,"copy [dest_range src_range] %s %s", range, saverange);
    PROCESS_line;

} /* EditCopy() */

void EditErase()
/*----------------------------------------------------------------------
** Erase a range of cells.  Erase the current cell, if no range is
** currently defined.  DANGEROUS OPERATION: Prompt for confirmation.
*/
{
    range = RangeGet();         /* Get range, if any, "" otherwise. */

    if (range[0] == NULL)       /* Null range */
        Sprintf(message, "** Erase the Current Cell?" );
    else
        Sprintf(message, "** Erase the Cell Range %s ?", range);

    if (yn_ask(message) == 1) {
	Sprintf(line,"erase [range] %s", range);
	PROCESS_line;
    }

} /* EditErase(); */

void EditLock()
/*----------------------------------------------------------------------
** Lock a range of cells.  Lock the current cell, if no range is
** currently defined.
*/
{
    range = RangeGet();		/* Get range, if any, "" otherwise. */

    if (range[0] == NULL)	/* Null range */
	Sprintf(message, "** Lock the Current Cell?" );
    else
	Sprintf(message, "** Lock the Cell Range %s ?", range);

    if (yn_ask(message) == 1) {
	Sprintf(line,"lock [range] %s", range);
	Message("** Locked");
	PROCESS_line;
	
    }
} /* EditLock(); */

void EditUnLock()
/*----------------------------------------------------------------------
** UnLock a range of cells.  UnLock the current cell, if no range is
** currently defined.
*/
{
    range = RangeGet();		/* Get range, if any, "" otherwise. */

    Sprintf(line,"unlock [range] %s", range);
    PROCESS_line;

} /* EditUnLock(); */

void EditFormat()
/*----------------------------------------------------------------------
** Format a range of cells.  Format the current cell, if no range is
** currently defined.
*/
{
    range = RangeGet();		/* Get range, if any, "" otherwise. */

    /*
     * No Range is Defined, Format the current Cell
     */
    if (range[0] == NULL) {
        register struct ent *p = *ATBL(tbl, currow, curcol);
	static char *curcell;		/* for displaying current cell */

	curcell = v_name(currow, curcol); /* Get current cell */
	
        /*
         * If the cell already has a Format, Display it for editing,
         * otherwise get into insert mode and read in a new format.
         */
        if (p && p->format) {	/* There IS an existing format  */
	    Sprintf(message,
		    "** Enter format for cell %s (Edit existing format)",
		    curcell);
	    Message(message);
	    buff = gi_editline( p->format );

        } else {		/* There IS NOT an existing format */
	    Sprintf(message, "** Enter format for cell %s", curcell);
	    Message(message);
	    buff = gi_line();
	}
	Sprintf(line, "fmt [format] %s \"%s\"", curcell, buff);

    /*
     * Else, there is a range defined, format the range.
     */
    } else {
	Sprintf(message, "** Enter format for the cell range %s", range);
	Message(message);
	buff = gi_line();
	Sprintf(line, "fmt [range \"format\"] %s \"%s\" ", range, buff);
    }
    ABORT_AND_RETURN_IF_BUFF_NULL;
    ClearMessage;	
    PROCESS_line;

} /* EditFormat(); */

void EditFill()
/*----------------------------------------------------------------------
** Fill a range of cells.  DANGEROUS OPERATION: Prompt for confirmation.
*/
{
    range = RangeGet();		/* Get range, if any, "" otherwise. */

    if (range[0] == NULL) {	/* Null range */
	Message("** No Range Defined, Fill aborted");
	return;
    }
    
    Message("** Fill %s.  Enter Starting Value & Increment (eg: \"5 2\")",
	    range);
    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;
    
    ClearMessage;

    Sprintf(message, "** Fill the range %s ? (starting val., incr. = %s)",
	    range, buff);
    if (yn_ask(message) == 1) {
	Sprintf(line,"fill [range start inc] %s %s", range, buff);
	PROCESS_line;
    }
} /* EditFill(); */

void EditValueize()
/*----------------------------------------------------------------------
** "Valueize" a range of cells.  Valueize means to convert all formulas
** to just plain numbers.  Valueize the current cell, if no range is
** currently defined.  DANGEROUS OPERATION: Prompt for confirmation.
*/
{
    range = RangeGet();		/* Get range, if any, "" otherwise. */

    if (range[0] == NULL)	/* Null range */
	Sprintf(message, "** Valueize the Current Cell?" );
    else
	Sprintf(message, "** Valueize the Cell Range %s ?", range);

    if (yn_ask(message) == 1) {
	Sprintf(line,"value [range] %s", range);
	PROCESS_line;
    }

} /* EditValueize() */


/**********************************************************************
*       End
**********************************************************************/
