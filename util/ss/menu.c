/**********************************************************************
* %M%
* ss 	:	A SpreadSheet Program
*
* Art's Spreadsheet program.          Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* Menu Driver
***********************************************************************
* Functions for displaying the command menus and processing the 
* related input.
* NOTE: this is not the ONLY interface to the user.
* These functions are used exclusively by ss.c
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

/*
** #ifdef BSD42
** # include <strings.h>
** #else
** # ifndef SYSIII
** #   include <string.h>
** # endif
** #endif
*/

#include "ss.h"
#include "keys.h"
#include "menu.h"
#include "menu_file.h"
#include "menu_edit.h"
#include "menu_cell.h"
#include "menu_rowcol.h"
#include "menu_misc.h"
#include "menu_macro.h"

#include "menu_name.h"


/*	Function Prototypes (lib func's)
 *----------------------------------------------------------------------
 */
    extern	char	*getenv();

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

/*	Internal Function Prototypes
 ***********************************************************************
 */

static int Menu();
static void FileMenu();
static void EditMenu();     
static void CellMenu();
static void RowColumnMenu();
static void NameMenu();
static void MiscMenu();
static void MacroMenu();

/*	Externally Accessible Functions
 ***********************************************************************
 */

void MainMenu()
/*----------------------------------------------------------------------
** Top command level/menu.  Invoked by the "/" command
*/
{
#   define MainSize 8
    static char MainKeys[] =  {'F',   'E',    'C',   'R', 'O', 'M', 'A', 'Q'};
    static char *MainDesc[] = {"File","Edit", "Cell","Row", "Column","Misc",
		"Macro", "Quit"};

    switch (Menu(MainSize, MainKeys, MainDesc, MAINHELP)) {

    case 'F':	FileMenu();		break;
    case 'E':	EditMenu();		break;
    case 'C':	CellMenu();		break;
      
    case 'R':	RowColumnMenu(TRUE);	break; /* TRUE = Row */
    case 'O':	RowColumnMenu(FALSE);	break;
      
    case 'M':	MiscMenu();		break;
    case 'A':	MacroMenu();		break;
    case 'Q':	FileQuit();		break;
    }
} /* MainMenu() */


/*	Internal Functions
 ***********************************************************************
 */

/**
 ** ADD message to Menu() so we can say
 ** "Insert Row or Column", optionally		???????????
 **/

static int Menu(size, keys, keydesc, helpcontext)
/*----------------------------------------------------------------------
** The Main Menu display function.  Displays a menu and does rough
** preprocessign of the input.  Ensures that only valid input is received,
** also deals with aborts and help requests.
**
** NOTE: The Keys should all be in uppercase.  (if they are letters).
*/
    int 	size;		/* # of elements in keys & keydesc arrays */
    char	keys[];		/* Menu keys */
    char	*keydesc[];	/* Corresponding descriptions of keys */
    int		helpcontext;	/* A flag to pass along to help() */
/*
 * Size must be a correct number.  Anything else has an undefined effect.
 * No checking is made to ensure that all the keys & descriptions fit
 * across the screen.
 */
{
    int x;
    int  ValidInput;		/* Loop Control */
    int c;			/* input */

    FullUpdate++;		/* to clear the message line */

/*
 * The Command line is the top line of the display.	starts at(0,0)
 * The message/error line is the second line.		starts at(1,0)
 */

/*
 * 1) Display the Menu on the Command/error line 
 *
 * Given a size of 3, and a set of keys and descriptions like this:
 * 	key[0] = 'f',        key[1] = 'e',        key[2] = 'o',
 * 	keydesc[0] = "File", keydesc[1] = "Edit", keydesc[2] = "Options".
 * A menu looking like this would be displayed:
 *	f:File e:Edit o:Options
 * Except the keys would be highlighted.
 */
    (void)move(0,0); 		/* Get into position */
    (void)clrtoeol(); 		/* Clear line */
    for (x=0; x < size; x++) {
	addch(keys[x] | A_STANDOUT);	/* Display key, highlighted */
	addch(':');			/* Separator */
	addstr(keydesc[x]);		/* Key Description */
	addch(' ');
    }

/* 2) Display a Prompt on the Message Line */
    Message("Enter Command, or <Space> to Abort: ");

/*
 * 3) Get and Process input
 *
 * Valid Input:
 * a)	A '?' will invoke help.  Pass along 'helpcontext' so that help()
 *	knows from where it was invoked, so context-sensitive help can
 *	be provided.
 * b)	<Space> or kABORT will Abort the menu.
 * c)	Any of the keys (Case is unimportant) will be accepted as valid
 *	and be returned to the calling routine for processing there.
 * d)	Anything else will result in an error (beep!)
 */
    for (;;) {			/* Forever... */
	c = nmgetch();
	switch (c) {

	case '?':		/* Help */
	    /** Context sensitive help **/		/** WRITE THIS **/
	    /* refresh needed? */
	    break;
	case kABORT:		/* Abort current command */
	case ' ':
	    ClearMessage;
/**	    (void) refresh(); **/
	    return(' ');
	    break;
	default:
	    c = (char) toupper(c);
	    for (x=0; x < size; x++) {	/* Check if input is valid key */
		if ( c == keys[x]) {
		    ClearMessage;
		    return(c);
		}
	    }
	    beep();		/* error bell */
	    break;
	}
    } 
    /* NOTREACHED */
} /* Menu() */

static void FileMenu()
/*----------------------------------------------------------------------
** File command level/menu.  Invoked from the Main menu
*/
{
#   define FileSize 	8
    static char FileKeys[]  = {'N',  'L',   'M',    'S',   'A', 'W','T','Q'};
    char tbl_mode[MAXSTR];
/* 
 * Set up menu to show the current mode of ``Tbl Save''
 *	- end up with something like ``Tbl Save(TeX)''
 */
    strcpy(tbl_mode, "Tbl Save(");
    switch (tbl_style) {
    case TBL:		strcat(tbl_mode, "tbl)");	break;
    case TEX:		strcat(tbl_mode, "TeX)");	break;
    case LATEX:		strcat(tbl_mode, "LaTeX)");	break;
    case SLATEX: 	strcat(tbl_mode, "SLaTeX)");	break;
    case FRAME:		strcat(tbl_mode, "FrameMaker)"); break;
    default:		strcat(tbl_mode, "?)");		break;
    }

    {
#ifdef __STDC__                               /* Ansi C can handle this: */
    char *FileDesc[] = {"New", "Load", "Merge", "Save", "Save As",
	"Write txt", tbl_mode, "Quit"};
#else                                         /* Non-Ansi Can't. */
    static char *FileDesc[] = {"New", "Load", "Merge", "Save", "Save As",
    	"Write txt", "", "Quit"};
    FileDesc[6] = tbl_mode;
#endif

	switch (Menu(FileSize, FileKeys, FileDesc, FILEHELP)) {

	case 'N':			/* New */
	    Message("** New -> Unimplemented");
	    break;
	case 'L': 	FileLoad(); 		break;
	case 'M':	FileMerge(); 		break;
	case 'S':	FileSave(); 		break;
	case 'A':	FileSaveAs(); 		break;
	case 'W':	FileWriteTxt(); 	break;
	case 'T':	FileTblSave(tbl_mode); 	break;
	case 'Q':	FileQuit();		break;

	} /* switch */
    } /* filemenu declaration */

} /* FileMenu() */

static void EditMenu()
/*----------------------------------------------------------------------
** Edit command level/menu.  Invoked from the Main menu
** - commands that operate either on the entire worksheet,
**   or on a range of cells.  (see the range commands of sc)
*/
{
#   define WorkSize 	8
    static char WorkKeys[]  ={'C','E','N','L','U','F','I','V'};
    static char *WorkDesc[] ={"Copy", "Erase", "Name", "Lock", "Unlck",
			      "Format", "Fill", "Valueize"};

    switch (Menu(WorkSize, WorkKeys, WorkDesc, WORKHELP)) {

    case 'C':	EditCopy();	break;
    case 'E': 	EditErase();	break;
    case 'N':	NameMenu();	break;
    case 'L':	EditLock();	break;
    case 'U': 	EditUnLock();	break;
    case 'F':	EditFormat();	break;
    case 'I':	EditFill();	break;
    case 'V':	EditValueize();	break;
    }

} /* EditMenu() */


static void CellMenu()
/*----------------------------------------------------------------------
** Cell command level/menu.  Invoked from the Main menu
*/
{
#   define CellSize 	5
    static char CellKeys[]  ={'G',    'M',    'C', 'L', 'V'};
    static char *CellDesc[] ={"Goto", "Mark", "Copy marked cell",
				  "edit Label", "edit Value"};

    switch (Menu(CellSize, CellKeys, CellDesc, CELLHELP)) {
/**   case 'E':	CellErase();		break; **/
      case 'G':	CellGoto();		break;	
      case 'M':	CellMark();		break;
      case 'C':	CellCopy();		break;
      case 'L':	CellEditLabel();	break;
      case 'V':	CellEditValue();	break;
    }
} /* CellMenu() */


static void RowColumnMenu(IsRow)
/*----------------------------------------------------------------------
** Row & Column command level/menu.  Invoked from the Main menu
*/
    int IsRow;
{
    /*
     * The Row & the Column Menu are *ALMOST* identical.
     * "Format: is a Column option only.
     */
#   define RowSize 	8
    static char RowKeys[]  ={'I','D','Y','M','C','H','S','V'};
    static char *RowDesc[] ={"Insert", "Delete", "Yank", "Merge", "Copy",
			     "Hide", "Show", "Valueize"};
#   define ColSize 	9
    static char ColKeys[]  ={'I','D','Y','M','C','H','S','V','F'};
    static char *ColDesc[] ={"Insert", "Delete", "Yank", "Merge", "Copy",
			     "Hide", "Show", "Valueize","Fmt"};

    int menuchoice;

    if (IsRow == TRUE)		/* Processing a Row */
	menuchoice = Menu(RowSize, RowKeys, RowDesc, ROWHELP);
    else			/* Processing a Column */
	menuchoice = Menu(ColSize, ColKeys, ColDesc, ROWHELP);
    
    switch (menuchoice) {

      case 'I':	RCInsert(IsRow);	break; /* Insert */
      case 'D': RCDelete(IsRow);	break; /* Delete */
      case 'Y': RCYank(IsRow);		break; /* Yank */
      case 'M': RCMerge();		break; /* Merge */
      case 'C': RCCopy(IsRow);		break; /* Copy */
      case 'H': RCHide(IsRow);		break; /* Hide */
      case 'S': RCShow(IsRow);		break; /* Show */
      case 'V': RCValueize(IsRow);	break; /* Valueize */
      case 'F': ColFormat();		break; /* Format Column */
    }

} /* RowColumnMenu() */


static void NameMenu()
/*----------------------------------------------------------------------
** Name command level/sub-menu.  Invoked from the Top-Level Edit menu.
*/
{
#   define NSize 	3
    static char NKeys[]  ={'D','E','S'};
    static char *NDesc[] ={"Define Name", "Erase Name", "Show Names"};

    switch (Menu(NSize, NKeys, NDesc, WNHELP)) {

      case 'D':	NameDefine();	break; /* Define a name */
      case 'E':	NameErase();	break; /* Erase a name */
      case 'S':	NameShow();	break; /* Show all names */
    }

} /* NameMenu() */

static void MiscMenu()
/*----------------------------------------------------------------------
** Misc commands menu.  Invoked from the Main menu
*/
{
#   define MiscSize 	6
    static char MiscKeys[]  ={'!','O','S','V','E','R'};
    static char *MiscDesc[] ={ "Shell Cmd", "Options", "Settings",
			       "show Values", "show Expr.", "Recalc"};

    switch (Menu(MiscSize, MiscKeys, MiscDesc, MISCHELP)) {

      case '!': MiscShell(); break; 	/* Execute a Shell command */
      case 'O': OptionsMenu();	break; 	/* WorkSheet - Toggle Options */
      case 'S': MiscSettings(); break; 	/* WorkSheet - Settings */
      case 'V': MiscRedraw(TRUE); break; /* Hilite Cells containing Values */
      case 'E': MiscRedraw_Expr(); break; /* Hilite Cells containing Exp'ns */
      case 'R': MiscRecalc(); break;	/* Recalc Spreadsheet */
    }

} /* MiscMenu() */


static void MacroMenu()
/*----------------------------------------------------------------------
** Macro command level/menu.  Invoked from the Main menu
*/
{
#   define MacroSize 	2
    static char MacroKeys[]  = {'R',  'D',    };
    static char *MacroDesc[] = {"Run","Define"};

    switch (Menu(MacroSize, MacroKeys, MacroDesc, MACROHELP)) {
      case 'R':	MacroRun();	break; /* Run macros */
      case 'D':	MacroDefine();	break; /* Define path */
    }

} /* MacroMenu() */

/*** SO FAR UPDATED ***/

void OptionsMenu()
/*----------------------------------------------------------------------
** Options command level/menu.  Invoked from the WorkSheet menu
** - Toggle options governing the spreadsheet
*/
{
/* Options Menu */
#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
#   define OptSize 	9
    static char OptKeys[]  ={'X','A','C','E','L','R','T','Z','$'};
    static char *OptDesc[] ={"Encrypt", "Auto", "Cell", "Ext fn's", "Label",
			     "Return", "Top", "Limits", "Pre-Scale"};
#else	/* No Encryption */
#   define OptSize 	8
    static char OptKeys[]  ={'A','C','E','L','R','T','Z','$'};
    static char *OptDesc[] ={"Auto", "Cell", "Ext fn's", "Label", 
			     "Return", "Top", "Limits", "Pre-Scale"};
#endif

    switch (Menu(OptSize, OptKeys, OptDesc, OPTHELP)) {
    case 'A': 	/* WorkSheet - Options - Auto Recalc */
	autocalc ^= 1;
	Message("** Automatic recalculation %sabled.", autocalc ? "en":"dis");
	break;
    case 'C':	/* WorkSheet - Options - Cell Highlight */
	showcell = (! showcell);
	repaint(lastmx, lastmy, fwidth[lastcol]);
	Message ("** Cell highlighting %sabled.", showcell ? "en" : "dis");
	break;
    case 'E':	/* WorkSheet - Options - Ext Funcs */
	extfunc = (! extfunc);
	Message ("** External functions %sabled.", extfunc? "en" : "dis");
	break;
    case 'T':	/* WorkSheet - Options - Top Line Display */
	showtop = (! showtop);
	Message ("** Top line display of cell contents %sabled.",
	    showtop ? "en" : "dis");
	break;
    case 'L':	/* WorkSheet - Options - Auto Labeling */
	autolabel = (! autolabel);
	Message ("** Autolabel %sabled.", autolabel? "en" : "dis");
	break;
    case 'R':	/* WorkSheet - Options - Return Action */
	++craction;
	if(craction >= 3)
	    craction = 0;
	switch(craction) {
	default:
	    craction = 0; 			/* fall through */
	case 0:
	    Message("** After a <CR> the Cell Cursor stays put.");
	    break;
	case CRROWS:
	    Message("** After a <CR> the Cell Cursor moves down one row.");
	    break;
	case CRCOLS:
	    Message("** After a <CR> the Cell Cursor moves right one column.");
	    break;
	}
	break;
#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)
    case 'X': 	/* WorkSheet - Options - Encrypt */
	Crypt = (! Crypt);
	Message ("** Encryption %sabled.", Crypt? "en" : "dis");
	break;
#endif
    case 'Z': 	/* WorkSheet - Options - Row/Col Limits */
	rowlimit = currow;
	collimit = curcol;
	Message("** Row and column limits set");
	break;
    case '$': 	 	/* WorkSheet - Options - Prescale $ */
	if (prescale == 1.0) {
	    Message ("** Prescale enabled.  (Numbers are multiplied by .01)");
	    prescale = 0.01;
	} else {
	    prescale = 1.0;
	    Message ("** Prescale disabled.");
	}
	break;
    }

    modflg++;		/* So options get saved in Spreadsheet file */
} /* OptionsMenu() */


/**********************************************************************
*       End
**********************************************************************/
