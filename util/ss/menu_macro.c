/**********************************************************************
* %M%
* Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Macro Menu Operations
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
#include "menu_macro.h"

/*	Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 


/*	External Global variables
 *----------------------------------------------------------------------
 */


/*	Externally Accessible Functions
 ***********************************************************************
 */

void MacroRun()
/*----------------------------------------------------------------------
** Load & Run a file of Macros
*/
{
    Message("** Run Macros from file...");
    
    if (mdir) {
	char temp[MAXSTR];

	Sprintf(temp,"%s/", mdir);
	buff = gi_editline( temp );
    } else
	buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;

    Sprintf (line,"merge [\"macro_file\"] \"%s\" ", buff);
    ClearMessage;
    PROCESS_line;
    
} /* MacroRun() */

void MacroDefine()
/*----------------------------------------------------------------------
** Define a Sub-directory name, where macro files are to be found
*/
{
    Message("** Define path (Sub-directory) for `Run Macro'");
    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;

    Sprintf (line, "mdir [\"macro_directory\"] \"%s\"", buff);
    ClearMessage;
    PROCESS_line;

} /* MacroDefine() */

/**********************************************************************
*       End
**********************************************************************/

