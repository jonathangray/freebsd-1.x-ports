/**********************************************************************
* %M%
* ss 	:	A SpreadSheet Program
*
* Art's Spreadsheet program.          Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Process Control Character Commands (NOT menu options)
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

#include "disprange.h"
#include "ctrl.h"
#include "menu_cell.h"
#include "menu_misc.h"

/*	Local Macro Definitions
 *----------------------------------------------------------------------
 */

/* 
 * Move the cursor to the HOME position (Cell A0) in the Spreadsheet. 
 */
#define doHOME	{ currow = 0; curcol = 0; FullUpdate++; Message("<Home>"); }

/*
 * Jump to the Last Valid Row in the current column.
 */
#define doEND 	{	 					\
	register struct ent *p;					\
	currow = maxrows - 1;					\
	while (!VALID_CELL(p, currow, curcol) && currow > 0)	\
	    currow--;						\
	Message("<End>");					\
	}

#define doSCROLL_LEFT 	backcol((curcol-stcol+1)+1)
#define doSCROLL_RIGHT	forwcol(lcols -(curcol-stcol)+1)

/*	Function Prototypes  (Local Functions)
 *----------------------------------------------------------------------
 */

    void ProcessControls();
    int ProcessCursors();

    void doForwardCell();
    void doBackwardCell();

/*	External Global variables
 *----------------------------------------------------------------------
 */
    extern int running;		/* from main.c */
    extern int ClearScreen;	/* ditto */
    extern int showneed;	/* ditto */
    extern int showexpr;	/* ditto */

    extern int lcols;		/* From screen.c */


/*	Local Global variables
 *----------------------------------------------------------------------
 */

static int meta_flag = FALSE;	/* Flag: indicates <ESC> prefix */
static int ctrlx_flag = FALSE;	/* Flag: indicates ^X prefix */

/*      Externally Accessible Functions
 ***********************************************************************
 */

/************************
*************************
** NOTE: This whole setup is a bit (!) of a kludge to allow
** functins to *just* process cursor commands, and no other forms of
** input.  Due to the fact that both cell-cursor
** motion commands, and regular control-key commands make use of
** the <Esc>- and ^X- prefix's, it becomes difficult to separate the
** the two operations.
**
** Currently, only the function "RangeForceInput()" in "disprange.c"
** calls the "ProcessCursorCommands()" function.  That function is
** removable, if/when we move to a CUT/COPY/PASTE ``clipboard'' style
** of editing the spreadsheet. 
**
** When no functions call "ProcessCursorCommands()" any longer,
** then we can safely take all the functions in this file and
** merge them into one, clean, comprehensible, function.
*************************
************************/

void ProcessControlCommands(c)
/*----------------------------------------------------------------------
** Process all Control-Key (& Related) Commands.
**
** This function serves as a "front end" to the actual
** Control-key / Function-key command processor function
** ``ProcessControls()''.
** The reason for this front-end is to handle two-key control-key
** combinations --> the <Esc>- and ^X- prefixed commands.
*/
    register int   c;	/* Control character command to process */
{

    /*
     * Procedure: Go process/parse the control key command ``c''.
     * If one of the flag's indicating an <Esc>- or ^X- prefix is then
     * set to be true, then grab the next character and process it
     * also.
     */

    ProcessControls(c);	 
    if ( (meta_flag == TRUE) || (ctrlx_flag == TRUE) ) {
	if (meta_flag == TRUE)
	    Message("** Meta -");
	else
	    Message("** Ctrl X - ");
	
	c = nmgetch();
	ProcessControls(c);
	meta_flag = ctrlx_flag = FALSE;
    }

} /* ProcessControlCommands() */


int ProcessCursorCommands(c)
/*----------------------------------------------------------------------
** Process all Cell-cursor motion commands... ONLY
**
** This function serves as a "front end" to the actual
** Cursor-motion  command processor function ``ProcessCursors()''.
**
** This function is derived from "ProcessControlCommands()".
**
** RETURNS: TRUE if `c' was in fact a cursor motion,
**          FALSE otherwise.
*/
    register int   c;	/* Control character command to process */
{
    int return_val;		/* To be returned... */
    
    return_val = ProcessCursors(c);
    
    if ( (meta_flag == TRUE) || (ctrlx_flag == TRUE) ) {
	if (meta_flag == TRUE)
	    Message("** Meta -");
	else
	    Message("** Ctrl X - ");
	
	c = nmgetch();
	return_val = ProcessCursors(c);
	meta_flag = ctrlx_flag = FALSE;

	if (return_val != TRUE)	/* INVALID Meta- or ^X- command! */
	    beep();
    }
    return return_val;
    
} /* ProcessCursorCommands() */

/*      Internal Functions
 ***********************************************************************
 */

void ProcessControls(c)
/*----------------------------------------------------------------------
** NOTE: see 'keys.h' for definitions of control characters
*/
    register int   c;	/* Control character command to process */
{
/* (1)
 * First go see if it is a Cell-Cursor Movement command.  If so,
 * process it in that function.
 */
    if (ProcessCursors(c) != TRUE) {

/* (2)
 * Else, check ``meta_flag'' to see if we are processing the second
 * character of a Meta (<Esc>-) prefixed command sequence.
 */
	if (meta_flag == TRUE)
	    switch (c) {
/**
 ** Currently, only cursor-movement keys use Meta- prefixes
 **/
	      default:
		Message("** Invalid Meta- command");
		beep();		/* error bell */
		break;
	    }
/* (3)
 * Else, check ``ctrlx_flag'' to see if we are processing the second
 * character of a Control-X prefixed command sequence.
 */
  	else if (ctrlx_flag == TRUE)
	    switch (c) {
	/*
	 * Menu Shortcuts: Edit Menu.
	 */
	      case kEDVAL:	CellEditValue();	break;
	      case kEDLABL:	CellEditLabel();	break;
		
	      default:
		Message("** Invalid Meta- command");
		beep();		/* error bell */
		break;
	    } 

	else
/* (4)
 * Else, it must just be a single Control-Key/Function-Key command.
 */
	    switch (c) {

	/*
	 * General Commands
	 */
	      case kMETA:	meta_flag = TRUE;	break;
	      case kCTRLX:	ctrlx_flag = TRUE;	break;
		
	      case kTAB: 	RangeToggle();		break;

	      case ctl('m'):		/* ^M = <Return> */
		  switch(craction) {
		    case CRROWS:
		      if ((rowlimit >= 0) && (currow >= rowlimit)) {
			  forwcol(1);
			  currow = 0;
		      } else {
			  forwrow(1);
		      }
		      break;
		    case CRCOLS:
		      if ((collimit >= 0) && (curcol >= collimit)) {
			  forwrow(1);
			  curcol = 0;
		      } else {
			  forwcol(1);
		      }
		      break;
		    default:
		      break;
		  }
		break;

	      case kBREAK: 	running = 0;    break; /* Quit Program */

#ifdef SIGTSTP
	      case kSTOP:	/* Stop process */
		(void) deraw();
		(void) kill(0, SIGTSTP); /* Nail process group */

				/* the pc stops here */

		(void) goraw();
		break;
#endif

      /*
       * Menu Shortcuts: Edit Menu.
       */
	      case DEL: case kBS:	/* <DEL> & <BackSpc>, Erase Cur Cell */
	      case kDEL:
		/* NOTE: For clean code, may prefer to substitute a call
		 * to EditErase().  With no range defined, it will erase
		 * the current cell.  However it will also prompt for
		 * confirmation.  Perhaps If/When a proper cut/paste/undo
		 * is written in, that be an appropriate fix.
		 */
		Sprintf(line,"erase [range]");
		PROCESS_line;
		break;
	  		
      /*
       * Menu Shortcuts: Misc Menu.
       */
	      case kREDRAW: MiscRedraw(FALSE); break;
	      case kVAL:    MiscRedraw(TRUE);  break; /* & hilite Values */
	      case kEXP:    MiscRedraw_Expr(); break; /* & hilite Expr'ns */

      /*
       * Menu Shortcuts: CellMenu.
       */
	      case kGOTO:	CellGoto();	break; /* Goto a Cell */
	      case kMARK:	CellMark();	break; /* Mark a Cell */
	      case kCOPY:	CellCopy();	break; /* Copy a Marked Cell */
	
      /*
       * Unknown
       */
	      default:		/* Unknown */
		Message ("No such command (^%c)", c + 0100);
		break;
	    } 
    } /* end "if (ProcessCursors(c) != TRUE)" */

} /* ProcessControls() */

int ProcessCursors(c)
/*----------------------------------------------------------------------
** Process all Cell-Cursor Movement Commands.
** - When inputting a range of cells, (ie: for copying a range of cells)
**   It is desirable to move the cursor around in the spread sheet.
** - Therefore, seperate all the Cell-Cursor movement commands into
**   a seperate function, here, so that they can be referenced from
**   more than one place in the program.
** RETURNS: TRUE:  If a cursor command was processed.
**	    FALSE: Otherwise.
** NOTE: see 'keys.h' for definitions of control characters
**
** Is this a hack?  We'll see.
*/
    register int   c;	/* Control character command to process */
{
    static int arg = 1;	/* numeric argument, sc holdover */


/* 
 * (1) Check ``meta_flag'' to see if we are processing the second
 * character of a Meta (<Esc>-) prefixed command sequence.
 */
    if (meta_flag == TRUE)
	switch (c) {

	  case kHOME: 	doHOME; 			break;
	  case kEND: 	doEND; 				break;
	  case kPGUP: 	backrow((currow-strow+1)+3);
	    break;		/* (Half-) Page Up */
	  case kBACK: 	doBackwardCell();		break;
	  case kFORW: 	doForwardCell(); 		break;

	  default:
	    return FALSE;
	    break;
	}
/*
 * (2) Else, check ``ctrlx_flag'' to see if we are processing the second
 * character of a Control-X prefixed command sequence.
 */
    else if (ctrlx_flag == TRUE)
	switch (c) {
	  case kPGLEFT:    doSCROLL_LEFT;  break; /* Scroll Half-page left */
	  case kPGRIGHT:   doSCROLL_RIGHT; break; /* Scroll Half-page right */

	  default:
	    return FALSE;
	    break;
	} 

    else
/* 
 * (3) Else, it must just be a single Control-Key/Function-Key command.
 */
	switch (c) {

    /*
     * CURSOR KEYS:  Move the Cell Cursor 1 Cell in different directions
     */
	  case kLEFT:	backcol(arg);	break;
	  case kRIGHT: forwcol(arg);
#ifdef RIGHT_CBUG
	    wasforw++;
#endif
	    break;
	  case kDOWN:	forwrow(arg);	break;
	  case kUP:	backrow(arg);	break;

    /*
     * CURSOR MOVEMENT:	Moves the Cell Cursor...
     */
	  case kJUMP:		/* to the end of a range */
	    Message("(** Jump to the End of a range)");
	    Prompt("Choose Direction to jump in, or <CR> to abort :");

	    switch (nmgetch()) {
	      case kUP: 	doend(-1, 0); break;
	      case kDOWN: 	doend( 1, 0); break;
	      case kLEFT:  	doend( 0,-1); break;
	      case kRIGHT: 	doend( 0, 1); break;

	      case kABORT:		/* Abort current command */
	      case ' ':
		ClearMessage;
		/** Refresh(); Jan 25/93 **/
		break;
	      default:
		Message("** Invalid Jump command");
		beep();		/* error bell */
		break;
	    }
	    break;

	  case kTOP:    currow = 0; break; /* to Top row in cur. col. */

	  case kSTART:  curcol = 0; break; /* to col. 0 in cur. row */
	  case kFINISH:		/* Jump to End of current row */
	    {
		register struct ent *p;

		curcol = maxcols - 1;
		while (!VALID_CELL(p, currow, curcol) && curcol > 0)
		    curcol--;
		break;
	    }

	  case kPGDN:	/* (Half-) Page Down */
	    forwrow(LINES-RESROW-(currow-strow)+1);
	    break;

	  default:		/* not a cursor movement command */
	    return(FALSE);
	} /* switch */

    return(TRUE);

} /* ProcessCursors() */

void doForwardCell()
/*----------------------------------------------------------------------
** - Forward to next valid cell
*/
{
    register struct ent *p;

    do {
        if (curcol < maxcols - 1)
            curcol++;
	else {
	    if (currow < maxrows - 1) {
	        while(++currow < maxrows - 1 && row_hidden[currow])
		    /* NULL BODY */;
	        curcol = 0;
            } else {
		    Message("At end of table");
		    break;
            }
        }
    } while(col_hidden[curcol] || !VALID_CELL(p, currow, curcol));

} /* doForwardCell() */

void doBackwardCell()
/*----------------------------------------------------------------------
** - Backward to next valid cell
*/
{
    register struct ent *p;

    do {
        if (curcol) 
		curcol--;
        else {
            if (currow) {
	        while(--currow && row_hidden[currow])
                    /* NULL */;
	        curcol = maxcols - 1;
	    } else {
	        Message ("At start of table");
	        return;
            }
	}
    } while(col_hidden[curcol] || !VALID_CELL(p, currow, curcol));

} /* doBackwardCell() */

/**********************************************************************
*       End
**********************************************************************/
