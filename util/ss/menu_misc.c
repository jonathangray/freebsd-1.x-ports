
/**********************************************************************
* %M%
* Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Misc Menu Operations
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
#include <signal.h>
#include "curses_stuff.h"
#include <stdlib.h>

#include "ss.h"
#include "getinput.h"
#include "disprange.h"
#include "menu_misc.h"

/*	Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 


/*	External Global variables
 *----------------------------------------------------------------------
 */
    extern int showneed;        /* From main.c */
    extern int anychanged;      /* ditto */
    extern int showexpr;        /* ditto */
    extern int ClearScreen;     /* ditto */



/*	Externally Accessible Functions
 ***********************************************************************
 */

void MiscShell()
/*----------------------------------------------------------------------
** - Access to the shell
**  	"! command"  executes command
**  	"!"	forks a shell
**  	"!!" repeats last command
*/
{
#if VMS || MSDOS
    Message("Not implemented on VMS or MS-DOS");
#else /* NOT VMS or MSDOS */

    char *shl;
    int pid, temp;
    char cmd[MAXSTR];
    static char lastcmd[MAXSTR];

    if (!(shl = getenv("SHELL")))
	shl = "/bin/sh";

    deraw();
    (void) fputs("! ", stdout);
    (void) fflush(stdout);
    (void) fgets(cmd, MAXSTR, stdin);
    cmd[strlen(cmd) - 1] = '\0';		/* clobber \n */
    if(strcmp(cmd,"!") == 0)			/* repeat? */
	    (void) strcpy(cmd, lastcmd);
    else
	    (void) strcpy(lastcmd, cmd);

    if (modflg) {
	(void) puts("[No write since last change]");
	(void) fflush (stdout);
    }

    if (!(pid = fork())) {
	(void) signal (SIGINT, SIG_DFL);  /* reset */
	if(strlen(cmd))
		(void)execl(shl,shl,"-c",cmd,(char *)0);
	else
		(void) execl(shl, shl, (char *)0);
	exit(-127);
    }

    while (pid != wait(&temp));

    (void) printf("Press RETURN to continue ");
    fflush(stdout);
    (void)nmgetch();
    goraw();
#endif /* VMS */
} /* MiscShell() */

void MiscSettings()
/*----------------------------------------------------------------------
** Change various spreadsheet settings.
**
** THIS IS UGLY.  HOLDOVER FROM 'sc'  FIX PLEASE!
*/
{

    Message("Set:byrows,bycols,iterations=n,tblstyle=(0|tbl|latex|slatex|tex|frame),<MORE>");

    buff = gi_line();
    ABORT_AND_RETURN_IF_BUFF_NULL;
    Sprintf (line, "set %s", buff);    
    ClearMessage;
    modflg++;		/* So Settings get saved in Spreadsheet file */
    PROCESS_line
    
} /* MiscSettings() */

void MiscRedraw(show_values)
/*----------------------------------------------------------------------
** Redraw the screen, Highlight Cells containing Values if
** ``show_values'' is true.
*/
    int show_values;
{

    if (show_values == TRUE) { 
	Message("** All Highlighted cells contain Values.");
	showneed = 1;
    }
    FullUpdate++;
    ClearScreen++;
    (void) clearok(stdscr,1);

    /* Centering the display with cursor for middle */
    if(currow > (LINES-RESROW)/2)
	strow = currow - ((LINES-RESROW)/2);

} /* MiscRedraw() */

void MiscRedraw_Expr()
/*----------------------------------------------------------------------
** Redraw the screen, Highlight Cells containing Expressions 
*/
{
    Message("** All Highlighted cells contain Expressions.");

    FullUpdate++;
    showexpr = 1;
    (void) clearok(stdscr,1);
}

void MiscRecalc()
/*----------------------------------------------------------------------
** Recalculate the Spreadsheet.
*/
{
    ClearMessage;
    EvalAll ();
    changed = 0;
    anychanged = TRUE;
    Message("** Spreadsheet Recalculated.");
}

/**********************************************************************
*       End
**********************************************************************/
