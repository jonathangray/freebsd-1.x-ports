/**********************************************************************
* %M%
* Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* File Menu Operations
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
#include "menu_file.h"

/*	Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 

/*	External Global variables
 *----------------------------------------------------------------------
 */
extern int running;		/* from main() */


/*	Externally Accessible Functions
 ***********************************************************************
 */

void FileLoad()
/*----------------------------------------------------------------------
** Load a spreadsheet into memory.
** -> NOTE: this overwrites the current spreadsheet in memory.
*/
{
    if (*curfile)
	Sprintf(message, "** Enter name of file to load (Default: \"%s\")",
		curfile);
    else
	Sprintf(message, "** Enter name of file to load");

    Message(message);
    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;
    
    Sprintf (line, "get [\"source\"] \"%s\"", buff);
    ClearMessage;
    PROCESS_line;
    
} /* fileLoad() */

void FileMerge()
/*----------------------------------------------------------------------
*/
{
     Sprintf(message, "** Enter name of file to merge in");
     Message(message);
     buff = gi_line();
     ABORT_AND_RETURN_IF_BUFF_NULL;

     Sprintf (line, "merge [\"source\"] \"%s\"", buff);
     ClearMessage;
     PROCESS_line;
     
} /* FileMerge() */

int FileSave()
/*----------------------------------------------------------------------
** Save the complete spreadsheet to the current file
*/
{
    if (modflg && curfile[0]) {
	sprintf (line, "put [\"dest\" range] \"%s\" ", curfile);
	PROCESS_line;

/**     if (writefile(curfile, 0, 0, maxrow, maxcol) < 0) {
	ClearPrompt;
	linelim = -1;	  
	return (1);
	}
**/	
    } else if (modflg) { /* No filename yet established */
      FileSaveAs();

    } else			/* the file is not modified */
	Message("** No changes needed to be saved");
    return(0);
} /* FileSave() */


void FileSaveAs()
/*----------------------------------------------------------------------
** Prompt for a filename & save to that file.  The user can also
** optionally enter a range of  cells to be saved (default is all
** cells.)
*/
{
#ifdef __STDC__				/* ANSI C can handle this: */
    char def_file[MAXSTR] = "";	/* default filename */
#else					/* Non-ANSI C can't */
    char def_file[MAXSTR];
    def_file[0] = '\0';
#endif

    range = RangeGet();		/* Get range, if any, "" otherwise. */
    
    if (*curfile)
	Sprintf(def_file, "(Default: \"%s\")", curfile);

    Sprintf(message, "** Enter filename in which to save %s %s",
	    range, def_file);

    Message(message);
    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;

    Sprintf (line, "put [\"dest\" range] \"%s\" %s", buff, range);
    PROCESS_line;

} /* FileSaveAs() */


void FileWriteTxt()
/*----------------------------------------------------------------------
*/
{
    range = RangeGet();		/* Get range, if any, "" otherwise. */

    Sprintf(message,
	    "** Write File, Text only: Enter filename in which to save %s",
	    range);

    Message(message);
    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;
    
    Sprintf (line, "write [\"dest\" range] \"%s\" %s", buff, range);
    PROCESS_line;

} /* FileWriteTxt() */


void FileTblSave(tbl_mode)
/*----------------------------------------------------------------------
** Save the spreadsheet in ``tbl'' mode.  Which tbl mode (tbl, tex, latex
** slatex, framemaker) depends on the setting of the global variable
** ``tbl_style'' which is set in the /Misc/Settings menu.
** tbl_mode is a string representation of tbl_style.
*/
    char * tbl_mode;	
{
    range = RangeGet();		/* Get range, if any, "" otherwise. */

    Sprintf(message, "** %s : Enter filename in which to save %s",
	    tbl_mode, range);

    Message(message);
    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;

    Sprintf (line, "tbl [\"dest\" range] \"%s\" %s", buff, range);
    PROCESS_line;

} /* FileTblSave() */

void FileQuit()
/*----------------------------------------------------------------------
** Quit the program
*/
{
     Message("** Quit");
     running = 0;
}


/**********************************************************************
*       End
**********************************************************************/
