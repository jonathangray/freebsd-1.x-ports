/**********************************************************************
* %M%
* Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Row & Column Menu Operations
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
#include "menu_rowcol.h"

/*	Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 
static int minr;		/* To hold a range: min-row */
static int minc;		/* ... min-column */
static int maxr;		/* ... max-row */
static int maxc;		/* ... max-column */

/*	External Global variables
 *----------------------------------------------------------------------
 */



/*	Externally Accessible Functions
 ***********************************************************************
 */

void RCInsert(IsRow)
/*----------------------------------------------------------------------
** Insert some Rows or Columns.  If a range is defined, insert the
** number of rows (columns) spanned by the range.  If no range is
** defined, insert just one row (column). 
**
** Rows (columns) will be inserted *BEFORE* the current one.
** In the case of a range, the 'current' row (column) will be the 
** topmost row (leftmost column).
*/
    int IsRow;			/* TRUE = a Row, FALSE = Column */
{
    RangeGetNum(&minr, &minc, &maxr, &maxc);         

    if (IsRow) {		/* Insert row(s) */
	currow = minr;
	insertrow( (maxr - minr +1) );	     /* # rows to insert */
	
    } else {			/* Insert Column(s) */
	curcol = minc;
	opencol(curcol, (maxc - minc +1) );  /* # cols to insert */
    }
} /* RCInsert() */

void RCDelete(IsRow)
/*----------------------------------------------------------------------
** Delete some Rows or Columns.  If a range is defined, the
** number of rows (columns) spanned by the range.  If no range is
** defined, delete just the row (column) where the cell cursor is. 
*/
    int IsRow;			/* TRUE = a Row, FALSE = Column */
{
    RangeGetNum(&minr, &minc, &maxr, &maxc);         

    /*
     * Set up message to prompt user
     */
    if (IsRow) {		/* Deleting a Row */
	if (minr == maxr)	/* ...No Range */
	    Sprintf(message, "** Delete the Current Row ?");
	else
	    Sprintf(message, "** Delete the Rows %d:%d ?", minr, maxr);

    } else {			/* Deleting a Column */
	if (minc == maxc)	/* ...No Range */
	    Sprintf(message, "** Delete the Current Column ?");
	else {
	    /* coltoa uses static storage, which we have to get around.  */
	    Sprintf(message, "** Delete the Columns %s:", coltoa(minc) );
	    Sprintf(message + strlen(message), "%s ?", coltoa(maxc) );
	}
    }

    /*
     * Ask user...
     */
    if (yn_ask(message) == 1) {
	if (IsRow)		/* Delete Row(s) */
	    deleterow(minr,maxr);
	else			/* Delete Column(s) */
	    closecol(minc, maxc);
    }
} /* RCDelete() */


void RCYank(IsRow)
/*----------------------------------------------------------------------
** Yank back previously deleted set of cells, making room for them by
** inserting enough rows, or columns.
*/
    int IsRow;			/* TRUE = a Row, FALSE = Column */
{

  if (IsRow)			/* Yank Rows */
      pullcells('r');
  else				/* Yank Columns */
      pullcells('c');

} /* RCYank() */


void RCMerge()
/*----------------------------------------------------------------------
** Yank back previously deleted set of cells.
** DO NOT make room for them.  Overwrite the contents of
** any cells at the current cell-cursor position.
**
** NOTE: THIS IS IRRESPECTIVE OF "Row" or "Column".
** It really belongs on the "Edit" Menu, but leave here for now,
** to be by the "Yank" Command.
*/
{
    if (yn_ask("OVERWRITE current cells with Yanked cells?")) 
	pullcells('m');
    
} /* RCMerge() */


void RCCopy(IsRow)
/*----------------------------------------------------------------------
** Make a copy (duplicate) of the current row (column), and insert
** it into the spreadsheet to the right (below) of the current
** row (column).
*/
    int IsRow;			/* TRUE = a Row, FALSE = Column */
{

    if (IsRow)			/* Hide Row */
	duprow();
    else			/* Hide Column */
      dupcol();

} /* RCCopy() */

void RCHide(IsRow)
/*----------------------------------------------------------------------
** Hide some rows (columns).  If a range is defined, hide the
** number of rows (columns) spanned by the range.  If no range is
** defined, hide just the current row (column). 
*/
    int IsRow;			/* TRUE = a Row, FALSE = Column */
{
    RangeGetNum(&minr, &minc, &maxr, &maxc);
    
    if (IsRow)			/* Show Hidden Row(s) */
	hiderow(minr,maxr);
    else			/* Show Hidden Column(s) */
	hidecol(minc,maxc);

} /* RCHide() */

void RCShow(IsRow)
/*----------------------------------------------------------------------
** Show Hidden Row's (Columns)
** Shows the *first* hidden row(s) (columns) in the spreadsheet,
** starting at the left (top).
*/
    int IsRow;			/* TRUE = a Row, FALSE = Column */
{
    register int i,j;

    
    if (IsRow) {			/* Show Hidden Row(s) */
	/**      rowshow_op(); **/

	for (i=0; i<maxrows; i++) 	/* STOLEN from "rowshow_op()" */
	    if (row_hidden[i])		/* in "cmds.c" ... */
		break;
	for(j=i; j<maxrows; j++)
	    if (!row_hidden[j]) 
		break;
	j--;

	if (i>=maxrows) {
	    error ("No hidden rows to show");
	    return;

	} else 
	    Sprintf(line,"show %d:%d", i, j);
	
    } else {			/* Show Hidden Column(s) */
	/**      colshow_op(); **/

	for (i=0; i<maxcols; i++) 	/* STOLEN from "colshow_op()" */
	    if (col_hidden[i])		/* in "cmds.c" */
		break;
	for(j=i; j<maxcols; j++)
	    if (!col_hidden[j])
		break;
	j--;

	if (i>=maxcols) {
	    error ("No hidden columns to show");
	    return;
	} else {
	    Sprintf(line,"show %s:", coltoa(i));
	    Sprintf(line + strlen(line),"%s",coltoa(j));
	}
    }
    PROCESS_line;

} /* RCShow() */

void RCValueize(IsRow)
/*----------------------------------------------------------------------
** Valueize some rows (columns).  If a range is defined, Valueize the
** number of rows (columns) spanned by the range.  If no range is
** defined, valueize just the current row (column). 
*/
    int IsRow;			/* TRUE = a Row, FALSE = Column */
{
    RangeGetNum(&minr, &minc, &maxr, &maxc);

    /*
     * Set up message to ask user...
     */
    if (IsRow) {		/* Valueizing a Row */
	if (minr == maxr)	/* ...No Range */
	    Sprintf(message, "** Valueize the Current Row ?");
	else
	    Sprintf(message, "** Valueize the Rows %d:%d ?", minr, maxr);

    } else {			/* Valueizing a Column */
	if (minc == maxc)	/* ...No Range */
	    Sprintf(message, "** Valueize the Current Column ?");
	else
	    Sprintf(message, "** Valueize the Columns %s:%s ?",
		    coltoa(minc), coltoa(maxc) );
    }

    /*
     * Ask user...
     */
    if (yn_ask(message) == 1) {
	if (IsRow)		/* Valueize Row */
	    valueize_area(minr, 0, maxr , maxcol);
	else {			/* Valueize Column */
	    valueize_area(0, minc, maxrow, maxc );
/**	    pullcells('c'); **/  /* I dunno *WHY* this is here?  Art M. */
	}
	modflg = 1;
    }
} /* RCValueize() */

	
void ColFormat()
/*----------------------------------------------------------------------
** Format the current column.  (This is primarily usefull for adjusting
** the column width.)  There seems to be some overlap here between
** this funciton and the format function in the edit menu.
**
** SHOULD BE FIXED UP
*/
{
    static char temp_str[MAXSTR];

    Sprintf(temp_str, "%d %d %d", fwidth[curcol], precision[curcol],
	    realfmt[curcol]);
    Sprintf(message,
	    "** Enter format for column %s (width, precision, realfmt)",
	    coltoa(curcol) );

    Message(message);
    buff = gi_editline(temp_str);
    ABORT_AND_RETURN_IF_BUFF_NULL;

    Sprintf( line, "format [for column] %s %s", coltoa(curcol), buff);
    PROCESS_line;
    
} /* ColFormat() */

/**********************************************************************
*       End
**********************************************************************/

