/**********************************************************************
* %M%
* ss 	:	A SpreadSheet Program
*
* Art's Spreadsheet program.          Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Main Driver
***********************************************************************
* This program is based (Heavily!) on the well know Public Domain
* spreadsheet program 'sc' Revision 6.19
* May 12/92 -> hacked a bit to bring it up to Rev 6/21 of `sc'
***********************************************************************
* SS Notes & Author List:
*	Feb 1992: sc 6.19 hacked into a new incarnation: 'ss'
*	- currently maintained by Art Mulder ( art@cs.ualberta.ca )
*
* SC Notes & Author List:
*  	original by James Gosling, September 1982
*  	modifications by Mark Weiser and Bruce Israel, 
*  		University of Maryland
*  
*  	More mods Robert Bond, 12/86
*  	More mods by Alan Silverstein, 3-4/88, see list of changes.
*  	Currently supported by sequent!sawmill!buhrt (Jeff Buhrt)
*  	$Revision: 1.1 $
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

#ifdef BSD42
# include <strings.h>
#else
# ifndef SYSIII
#   include <string.h>
# endif
#endif

#include "ss.h"
#include "menu.h"
#include "disprange.h"
#include "getinput.h"


/*      Global Variables
 *----------------------------------------------------------------------
 */

/*
 * User I/O variables
 */
char message[MAXSTR];		/* To hold Messages to the user */
char *buff;			/* Pointer to User Input buffer - see
				   the gi_..() functions  */
char *range;			/* Ptr to a range string - see
				   functions that deal with ranges */

/*
 * General Spreadsheet Variables
 */
struct ent ***tbl;		/* data table ref. in vmtbl.c and ATBL() */

  int strow = 0, stcol = 0;
  int currow = 0, curcol = 0;	/* Current location of cell cursor */
  int savedrow, savedcol;
  int maxrow, maxcol;
  int maxrows, maxcols;		/* # cells currently allocated */

char curfile[PATHLEN];		/* Filename of spreadsheet */

/** UNEXPLORED STUFF FOLLOWS **/
/*********************************************************************/
extern	void	startdisp(), stopdisp();

#ifdef SYSV3
  void exit();
#endif

#ifndef SAVENAME
# define	SAVENAME "SC.SAVE" /* file name to use for emergency saves */
#endif /* SAVENAME */


/*
 * Globals defined in sc.h
 */

  int FullUpdate = 0;
  int ClearScreen = 0;	/* don't try to be smart (?? Art M.)*/
  int *fwidth;
  int *precision;
  int *realfmt;
  char *col_hidden;
  char *row_hidden;
  char line[FBUFLEN];		/* Most commands to manipulate the
				   spreadsheet are stored in this
				   variable, and then parsed by a main
				   routine. */
  int changed;
  struct ent *to_fix;
  int modflg;			/* Flag: spreadsheet modified */
  char *mdir;
  int showsc, showsr;	/* Starting cell for highlighted range */
#ifdef RIGHT_CBUG
  int	wasforw	= FALSE;
#endif

/*
 * Function Prototypes
 */
  void	update();
  void	repaint();


  char    revmsg[80];

  int  linelim = -1;

  int  showtop   = 1;	/* Causes current cell value display in top line  */
  int  showcell  = 1;	/* Causes current cell to be highlighted	  */
  int  showrange = 0;	/* Causes ranges to be highlighted		  */
  int  showneed  = 0;	/* Causes cells needing values to be highlighted  */
  int  showexpr  = 0;	/* Causes cell exprs to be displayed, highlighted */

  int  autocalc = 1 ;	/* 1 to calculate after each update */
  int  autolabel = 1;   /* If room, causes label to be created after a define*/
  int  calc_order = BYROWS;
  int  tbl_style = 0;	/* headers for T command output */
  int  rndinfinity = 0;
  int  craction = 0;	/* 1 for down, 2 for right */
  int  rowlimit = -1;
  int  collimit = -1;
#ifdef	SIGWINCH
  int  hitwinch = 0;	/* got a SIGWINCH? */
#endif

  extern int lastmx, lastmy;	/* Screen address of the cursor */
  extern int lastcol, lcols;	/* Spreadsheet Column the cursor was in last */

/* a linked list of free [struct ent]'s, uses .next as the pointer */
  struct ent *freeents = NULL;

  extern	int	seenerr;
  extern	char	*rev;

#ifdef VMS
  int VMS_read_raw = 0;
#endif

char    *progname;
/*********************************************************************/
/** UNEXPLORED STUFF ENDS **/

/*
 * Global Variables 
 */
    int running;	/* Flag for signal a 'quit' request from user */
    int anychanged = FALSE; 

/*      Internal Macros & Data Structures
 *----------------------------------------------------------------------
 */
 

int main (argc, argv)
/*--------------------------------------------------------------------*/
  int 	argc;
  char  **argv;
{
    int     inloop = 1;
    register int   c; 		/* Main input character */
    char    *revi;
/** int	    anychanged = FALSE; **/

/*
 * PROCESS COMMAND LINE OPTIONS
 * 	Keep command line options around until the file is read so the
 * 	command line overrides file options.
 */
    int Mopt = 0;
    int Nopt = 0;
    int Copt = 0; 
    int Ropt = 0;

    int tempx, tempy; 	/* Temp versions of curx, cury */

#if defined(MSDOS)
    if ((revi = strrchr(argv[0], '\\')) != NULL)
#else
# ifdef VMS
    if ((revi = strrchr(argv[0], ']')) != NULL)
# else
    if ((revi = strrchr(argv[0], '/')) != NULL)
# endif
#endif
	progname = revi+1;
    else
	progname = argv[0];

    while (argc > 1 && argv[1][0] == '-') {
	argv++;
	argc--;
    	switch (argv[0][1]) {
#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
	    case 'x':
		    Crypt = 1;
		    break;
#endif
	    case 'm':
		    Mopt = 1;
		    break;
	    case 'c':
		    Copt = 1;
		    break;
	    case 'r':
		    Ropt = 1;
		    break;
	    case 'C':
		    craction = CRCOLS;
		    break;
	    case 'R':
		    craction = CRROWS;
		    break;
	    default:
		    Fprintf(stderr,"%s: unrecognized option: \"%c\"\n",
			progname,argv[0][1]);
		    exit(1);
	}
    }

    *curfile ='\0';

    startdisp();
    signals();

	/* setup the spreadsheet arrays, initscr() will get the screen size */
    if (!growtbl(GROWNEW, 0, 0))
    {	stopdisp();
	exit(1);
    }

/*
 * Build revision message for later use:
 */
    Strcpy (revmsg, progname);
    for (revi = rev; (*revi++) != ':'; );	/* copy after colon */
    Strcat (revmsg, revi);
    revmsg [strlen (revmsg) - 2] = 0;		/* erase last character */

/*  Strcat (revmsg, ":  Type '?' for help, '/' for main menu");  */
    Strcat (revmsg, ":  Type '/' for the main menu."); 
    
/*
 * READ IN SPREADSHEET FILE
 */
    if (argc > 1) {
	Strcpy(curfile,argv[1]);
	readfile (argv[1], 0);
    }

/*
 * APPLY COMMAND LINE OPTIONS
 * 	Command line options override settings stored in spreadsheet
 *	file.  Therefore they are applied after the file is read.
 */
    if (Mopt)
	autocalc = 0;
    if (Copt)
	calc_order = BYCOLS;
    if (Ropt)
	calc_order = BYROWS;

    modflg = 0;
#ifdef VENIX
    setbuf (stdin, NULL);
#endif
    FullUpdate++;

/************
* MAIN LOOP
************/
    while (inloop) { running = 1;
    while (running) {

/* 
 * RECALCULATE THE SPREADSHEET.
 */
/**	if (edistate < 0 && linelim < 0 && autocalc && (changed || FullUpdate))
**/
	if (autocalc && (changed || FullUpdate)) {
	    EvalAll ();
	    if (changed)		/* if EvalAll changed or was before */
		anychanged = TRUE;
	    changed = 0;

	} else if (changed)		/* any cells change? */
	     anychanged = TRUE;

#ifdef	SIGWINCH
	/* got a SIGWINCH? */
	if (hitwinch) {
	    hitwinch = 0;
	    stopdisp();
	    startdisp();
	    FullUpdate++;
	}
#endif
	update(anychanged);
	anychanged = FALSE;

/*
 * GET MAIN ROOT-LEVEL COMMAND/INPUT
 */
	c = nmgetch();			/* Get next input character */
	getyx(stdscr, tempy, tempx);
	ClearMessage;
	Move(tempy, tempx);
/*	(void) fflush (stdout);*/
	seenerr = 0;
	showneed = 0;	/* reset after each update */
	showexpr = 0;

     /* if ((c < ' ') || ( c == DEL )) { how about international here ? PB */
#if pyr
	if ( iscntrl(c) || (c >= 011 && c <= 015) ) {
#else
	if ( ( isascii(c) && iscntrl(c) ) || (c == 020 ) ) {
#endif
	  /* iscntrl broken in OSx4.1 */
	    /*
	     * There seems to be some question about what to do w/ the
	     * iscntrl, some BSD systems are reportedly broken as well.
	     */
            ProcessControlCommands(c);	/* 1) A Control character */

	} else { 	/* NOT a control character, just plain ascii.  */

/*
 * Local Macro: Cannot change locked cells
 */
#define ABORT_IF_LOCKED_CELL 	{if (locked_cell(currow, curcol)) \
				     { beep(); break; } }
		
	    switch (c) {
	      case '/':		/* 2) Enter Command/Menu Mode */
		MainMenu();	
		break;

	      case '?':		/* 3) Invoke Help */
/*   		help(HELP); */
		break;


	      /*
	       * 4) Numeric or Function Entry into cell.
	       *    The case of the '=' differs from the rest in that
	       *    we do not want the '=' put into the user's input
	       *    buffer.
	       */
	      case '=':		/* retained for 'sc' compatibility */
	      case '0': case '1': case '2': case '3': case '4':
	      case '5': case '6': case '7': case '8': case '9':
	      case '-': case '.': case '+':
	      case '@':		/* @ = begin a function */
		ABORT_IF_LOCKED_CELL;
		Message("%s (Value)", v_name(currow, curcol) );

		if ( c != '=' )
		    nmungetch( c ); /* push c back onto input stream */
		
		buff = gi_line_cursor();
		ABORT_AND_BREAK_IF_BUFF_NULL;
		
		Sprintf(line,"let %s = %s", v_name(currow, curcol), buff);
		ClearMessage;
		PROCESS_line;
		break;

	      /*
	       * 5) Label/String Entry into cell.
	       */
	      case '"':		/* 5.a) Centered String */
		ABORT_IF_LOCKED_CELL;
		Message("%s (Label)", v_name(currow, curcol) );
		buff = gi_line_cursor();
		ABORT_AND_BREAK_IF_BUFF_NULL;
		Sprintf (line, "label %s = \"%s\"",
			 v_name(currow, curcol), buff);
		PROCESS_line;
		break;
	      case '>':		/* 5.b) Right Justified String */
		ABORT_IF_LOCKED_CELL;
		Message("%s (Flush-Right Label)", v_name(currow, curcol) );
		buff = gi_line_cursor();
		ABORT_AND_BREAK_IF_BUFF_NULL;
		Sprintf (line, "rightstring %s = \"%s\"",
			 v_name(currow, curcol), buff);
		PROCESS_line;
		break;

	      /* 
	       * The default is to assume that they are starting a
	       * left-justified string.  Make sure that the character
	       * they just entered gets included in the string value.
	       */
	      case '<':		/* 5.c Left Justified String */
	      default:
		ABORT_IF_LOCKED_CELL;
		Message("%s (Flush-Left Label)", v_name(currow, curcol) );

		if ( c != '<' )
		    nmungetch( c ); /* push c back onto input stream */
		
		buff = gi_line_cursor();
		ABORT_AND_BREAK_IF_BUFF_NULL;
		
		Sprintf (line, "leftstring %s = \"%s\"",
			 v_name(currow, curcol), buff);
		PROCESS_line;
		break;

	    }	/* End of Command Switch */
	} /* End of Else */
/*
 * NO MORE Command options.
 */
    }		/* while (running) */
    inloop = modcheck(" before exiting");
    }		/*  while (inloop) */
/******************
* END OF MAIN LOOP
******************/

    stopdisp();
#ifdef VMS	/* Until VMS "fixes" exit we should say 1 here */
    exit(1);
#else
    exit(0);
#endif
    /*NOTREACHED*/

} /* end Main() */

/*********************************************************************/

/* set the calculation order */
void setorder(i)
    int i;
{
    if((i == BYROWS)||(i == BYCOLS))
	calc_order = i;
}

void setauto(i)
    int i;
{
    autocalc = i;
}

void signals()
{
    VOID_OR_INT doquit();
    VOID_OR_INT time_out();
    VOID_OR_INT dump_me();
#ifdef	SIGWINCH
    VOID_OR_INT winchg();
#endif

    Signal(SIGINT, SIG_IGN);
#if !defined(MSDOS)
    Signal(SIGQUIT, dump_me);
    Signal(SIGPIPE, doquit);
    Signal(SIGALRM, time_out);
    Signal(SIGBUS, doquit);
#endif
    Signal(SIGTERM, doquit);
    Signal(SIGFPE, doquit);
#ifdef	SIGWINCH
    Signal(SIGWINCH, winchg);
#endif
}

#ifdef	SIGWINCH
  VOID_OR_INT winchg()
  {	hitwinch++;
	  Signal(SIGWINCH, winchg);
  }
#endif

VOID_OR_INT doquit()
{
    diesave();
    stopdisp();
    exit(1);
}

VOID_OR_INT dump_me()
{
    diesave();
    deraw();
    abort();
}

/* try to save the current spreadsheet if we can */
void diesave()
{   char	path[PATHLEN];

    if (modcheck(" before Spreadsheet dies") == 1)
    {	Sprintf(path, "~/%s", SAVENAME);
	if (writefile(path, 0, 0, maxrow, maxcol) < 0)
	{
	    Sprintf(path, "/tmp/%s", SAVENAME);
	    if (writefile(path, 0, 0, maxrow, maxcol) < 0)
		Message("Couldn't save current spreadsheet, Sorry");
	}
    }
}

/*
 * modcheck()
 *	Check if Spreadsheet has been modified and ask to save
 */
int modcheck(endstr)
  char *endstr;		/* message to user */
{
    if (modflg && curfile[0]) {
	int	yn_ans;
	char	lin[100];

	Sprintf (lin,"File \"%s\" is modified, save%s? ",
		curfile,endstr);
	if ((yn_ans = yn_ask(lin)) < 0)
	    return(1);
	else if (yn_ans == 1) {
	    if (writefile(curfile, 0, 0, maxrow, maxcol) < 0)
 		return (1);
	}
    } else if (modflg) {
	int	yn_ans;

	if ((yn_ans = yn_ask("Do you want a chance to save the data? ")) < 0)
	    return(1);
	else
	    return(yn_ans);
    }
    return(0);
} /* modcheck() */

/* Returns 1 if cell is locked, 0 otherwise */
int locked_cell (r, c)
    int r, c;
{
    struct ent *p = *ATBL(tbl, r, c);
    if (p && (p->flags & is_locked)) {
	Message("Cell %s%d is locked", coltoa(c), r) ;
	return(1);
    }
    return(0);
}

/* Check if area contains locked cells */
int any_locked_cells(r1, c1, r2, c2)
    int r1, c1, r2, c2 ;
{
    int r, c;
    struct ent *p ;

    for (r=r1; r<=r2; r++)
	for (c=c1; c<=c2; c++) {
	    p = *ATBL(tbl, r, c);
	    if (p && (p->flags&is_locked))
		return(1);
	}
    return(0);
}

/* 
 * return a pointer to a cell's [struct ent *], creating if needed 
 */
struct ent * lookat(row,col)
    int	row, col;
{
    register struct ent **pp;

    checkbounds(&row, &col);
    pp = ATBL(tbl, row, col);
    if (*pp == (struct ent *)0) {
        if (freeents != NULL)
	{	*pp = freeents;
		freeents = freeents->next;
	}
	else
		*pp = (struct ent *) Malloc((unsigned)sizeof(struct ent));
	if (row>maxrow) maxrow = row;
	if (col>maxcol) maxcol = col;
	(*pp)->label = (char *)0;
	(*pp)->row = row;
	(*pp)->col = col;
	(*pp)->flags = 0;
	(*pp)->expr = (struct enode *)0;
	(*pp)->v = (double) 0.0;
	(*pp)->format = (char *)0;
	(*pp)->cellerror = CELLOK;
	(*pp)->next = NULL;
    }
    return *pp;
}

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */
void free_ent(p)
    register struct ent *p;
{
    p->next = to_fix;
    to_fix = p;
    p->flags |= is_deleted;
    p->flags &= ~is_locked;
}

/* free deleted cells */
void flush_saved()
{
    register struct ent *p;
    register struct ent *q;

    if (!(p = to_fix))
	return;
    while (p) {
	clearent(p);
	q = p->next;
	p->next = freeents;	/* put this ent on the front of freeents */
	freeents = p;
	p = q;
    }
    to_fix = NULL;
}
/**********************************************************************
*	END
**********************************************************************/
