/**********************************************************************
* %M%
* Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Cell Menu Operations
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
#include "menu_cell.h" 

/*	Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 

/*	External Global variables
 *----------------------------------------------------------------------
 */


/*	Externally Accessible Functions
 ***********************************************************************
 */


void CellGoto()
/*----------------------------------------------------------------------
** Goto a Cell (or the location of a ``name'').
*/
{
    Message("** Goto: Enter Cell (or cell name) to go to");
    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;
    Sprintf (line, "goto [v] %s", buff);
    ClearMessage;
    PROCESS_line;
} /* CellGoto() */

void CellMark()
/*----------------------------------------------------------------------
** Mark a cell.  The cell is then available to the CellCopy command.
*/
{
    Message("** Cell marked for later copying.");
    savedrow = currow;
    savedcol = curcol;
} /* CellMark() */

void CellCopy()
/*----------------------------------------------------------------------
** Copy a cell that was previous Marked [ CellMark() ] into the
** current cell.
*/
{
     
    register struct ent *p = *ATBL(tbl, savedrow, savedcol);
    register struct ent *n;
    
    if (!p)			/* Make sure the marked cell exists */
	return;

    Message("** Previously marked cell copied.");
    n = lookat (currow, curcol);
    (void) clearent(n);
    copyent( n, p, currow - savedrow, curcol - savedcol);
    FullUpdate++;
    modflg++;
	
} /* CellCopy() */

void CellEditLabel()
/*----------------------------------------------------------------------
** Edit the String portion of a cell's contents.
**
** NOTE: based on edits() in interp.c
*/
{
    register struct ent *p;	/* To point to the current cell */
     
    if (locked_cell(currow, curcol)) { 
	Message("** Current cell is LOCKED, Edit aborted");
	beep();
	return;
    }

    Message("** Edit Cell Label");
    p = lookat (currow, curcol);
    if (p->label)		/* A Label exists */
	buff = gi_editline( p->label);
    else			/* No Label exists, input one! */
	buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;

    if( p->flags&is_label )
        Sprintf(line, "label %s = \"%s\"", v_name(currow, curcol),
		buff);
    else
	Sprintf(line, "%sstring %s = \"%s\"",
		((p->flags&is_leftflush) ? "left" : "right"),
		v_name(currow, curcol),
		buff);

    ClearMessage;

    linelim = 0;	
    (void) yyparse ();
    linelim = -1;	

} /* CellEditLabel() */


void CellEditValue()
/*----------------------------------------------------------------------
** Edit the Value (number/function etc) portion of a cell's contents.
*/
{
    register struct ent *p;
   
    if (locked_cell(currow, curcol)) { 
	Message("** Current cell is LOCKED, Edit aborted");
	beep();
	return;
    }

    Message("** Edit Cell Value");
    p = lookat (currow, curcol);

    /*
     * Ugh, this is a Horrible Hack -> to take the easy way
     * out and avoid figuring out some obscure code.
     * Fix it some time!
     *
     * NOTE: based on editv() in interp.c
     */
    line[0] = '\0';
    linelim = 0;
    if (p->flags & is_strexpr || p->expr == 0) {
        Sprintf (line+linelim, "%.15g", p->v);
        linelim += strlen (line+linelim);
    } else {
        editexp(currow,curcol);
    }
    /** END "Horrible Hack", continue with regular hack :-) **/

    buff = gi_editline( line );
    linelim = -1;
    ABORT_AND_RETURN_IF_BUFF_NULL;
    
    Sprintf(line, "let %s = %s", v_name(currow, curcol), buff);
    ClearMessage;
    PROCESS_line;

} /* CellEditValue() */


/*	Internal Functions
 ***********************************************************************
 */

/*
** NOTE: the ability to erase a single cell is not considered
** necessary, since you can achieve the same thing through the
** Edit menu.  the function is preserved here for informative
** purposes.
**
** HMMMM: May want to bind this to like the backspace key, to zap
** the contents of the cell under the ptr.  Bear it in mind.
**/

/*----------------------------------------------------------------------
** Erase the contents of the current cell.
*/

/* void CellErase()
** {
**     register struct ent **pp;
**
**     flush_saved();
**     pp = ATBL(tbl, currow, curcol);
**     if ((*pp) && !locked_cell(currow, curcol)) {
**        if (*pp) {
**	   free_ent(*pp);
**	   *pp = (struct ent *)0;
**        }
**     }
**     sync_refs();
**     modflg++;
**     FullUpdate++;
** 
** }
*/

/**********************************************************************
*       End
**********************************************************************/
