/********************************************************************
 * lindner
 * 3.11
 * 1993/08/23 02:32:48
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/manager.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: manager.c
 * Procedures to manage the screen.
 *********************************************************************
 * Revision History:
 * manager.c,v
 * Revision 3.11  1993/08/23  02:32:48  lindner
 * Fix for arrow updating
 *
 * Revision 3.10  1993/08/19  20:22:54  lindner
 * Mitra's Debug patch
 *
 * Revision 3.9  1993/08/16  18:11:29  lindner
 * fix for VMS Alpha systems
 *
 * Revision 3.8  1993/07/30  14:19:34  lindner
 * Mitra autoexit patch
 *
 * Revision 3.7  1993/07/27  05:28:56  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.6  1993/07/27  02:02:45  lindner
 * More comments
 *
 * Revision 3.5  1993/07/20  23:12:32  lindner
 * Mods to use patchlevel.h
 *
 * Revision 3.4  1993/04/23  20:14:13  lindner
 * Fix for munged characters in Draw_Status
 *
 * Revision 3.3  1993/04/15  21:17:00  lindner
 * none
 *
 * Revision 3.2  1993/03/26  19:43:46  lindner
 * Fix for repainting when selecting an item bu number
 *
 * Revision 3.1.1.1  1993/02/11  18:02:58  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:36:56  lindner
 * Now shows ask items, provision for auto size display in the future
 *
 * Revision 1.4  1993/01/31  00:08:12  lindner
 * New fcn DisplayTitle()
 *
 * Revision 1.3  1993/01/06  21:01:40  lindner
 * Improved behaviour when executing an item by typing it's number
 * The arrow and screen are updated to reflect the item being
 * retrieved.
 *
 * Revision 1.2  1993/01/05  22:31:57  lindner
 * Fixed display problems with directory title searching.
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#include "gopher.h"
#include "patchlevel.h"
#include "Debug.h"

#define MENULINE(x)   (x)+3

/* If any gophers to display (screen can be blank), count the number
   of pages.  If there is a remainder greater than zero, add one page */
#define PAGECALC(x,y) (y) ? (x/y) + ((x%y)>0) : 1



/*
** Draw the title on the top
*/

void
Draw_Banner()
{
     char line[80];

     standout();
     sprintf(line, "Internet Gopher Information Client %s.%s pl%d", GOPHER_MAJOR_VERSION, GOPHER_MINOR_VERSION, PATCHLEVEL);
     CURcenterline(CursesScreen,line, 0);
     standend();
}     


/*
** Draw the status line
*/

void
Draw_Status(textline)
  char *textline;
{
     move(LINES-1, 0);
     clrtoeol();
     mvaddstr(LINES-1, 0, "Press ");
     standout();
     addstr("?");
     standend();

     addstr(" for Help, ");

     standout();
     addstr("q");
     standend();
     addstr(" to Quit");
     
#ifndef AUTOEXITONU
     addstr(", ");
     standout();
     addstr("u");
     standend();
     addstr(" to go up a menu");
#endif
     clrtoeol();

     mvaddch(LINES-1,  COLS-strlen(textline)-4, ' ');
     addstr(textline);
}

void
DisplayTitle(gs, max, dogplus) 
  GopherObj *gs;
  int max;
  boolean dogplus;
{
     char type;
     char *title;
     char *c, *d;
     char *size;
     int  m,n;
     char temp[1024];

     type = GSgetType(gs);
     d = GSgetTitle(gs);

     if (GSisGplus(gs) && GSgplusInited(gs)) {

	  size = VIgetSize(GSgetView(gs, 0));
     }

     switch(type)
     {
     case A_DIRECTORY:
     case A_FILE:
	  max--;
	  break;
     case A_SOUND:
	  max -= 3;
	  break;
     case A_INDEX:
	  max -=4;
	  break;
     case A_CSO:
     case A_TN3270:
     case A_TELNET:
     case A_UNIXBIN:
     case A_MACHEX:
	  max -=6;
	  break;
     case A_PCBIN:
	  max -=9;
	  break;
     case A_GIF:
     case A_IMAGE:
	  max -= 10;
	  break;
     }

     
     if((m = strlen(d)) <= max)
	  printw(" %s", d);
     else {
	  /*** Cut out the middle bits **/
	  if ((c = strchr(d, '/'))!=NULL && (max > (c-d))) {
	       n = c - d;
	       strncpy(temp, d, n);
	       strcpy(temp + n, "..");
	       strcat(temp, d + (m + n - max));
	       printw(" %s", temp);
	  } else {
	       /** Trunc it.. **/
	       strcpy(temp, d);
	       temp[max] ='\0';
	       printw(" %s..", temp);
	  }
     }
     
     switch(type)
     {
     case A_DIRECTORY:
	  addch('/');
	  break;
     case A_CSO:
	  addstr(" <CSO>");
	  break;
     case A_TN3270:
	  addstr(" <3270>");
	  break;
     case A_TELNET:
	  addstr(" <TEL>");
	  break;
     case A_INDEX:
	  addstr(" <?>");
	  break;
     case A_SOUND:
	  addstr(" <)");  /** It's supposed to look like a speaker! **/
	  break;
     case A_FILE:
	  addch('.');
	  break;
     case A_PCBIN:
	  addstr(" <PC Bin>");
	  break;
     case A_UNIXBIN:
	  addstr(" <Bin>");
	  break;
     case A_IMAGE:
     case A_GIF:
	  addstr(" <Picture>");
	  break;
     case A_MACHEX:
	  addstr(" <HQX>");
	  break;
	  
     }

     if (GSisAsk(gs))
	  addstr(" <??>");
     
}


/*
** Man is this ugly.
*/

void
Display_Dir_Page(gopherdir, iNewLine, nNewPage, nMaxPages, iPageLen, iLastPageLen)
  GopherDirObj *gopherdir;
  int iNewLine;
  int nNewPage, nMaxPages, iPageLen, iLastPageLen;
{
     int i, iLoop, iOffset;
     boolean dogplus = FALSE;

     /*** Clear the screen and redraw the top line **/
     clear();
     Draw_Banner();

     /** Draw the menu **/
     iLoop = (nNewPage == nMaxPages) && iLastPageLen ? iLastPageLen : iPageLen;

     /** Look at the first item in the directory to decide whether to
         use a gopher+ display of the menu ***/


     for (i= 0, iOffset = (nNewPage-1) * iPageLen; i <iLoop; i++, iOffset++) {
	  move(MENULINE(i+1), 6);
	  printw("%d.", iOffset +1);
	  if (iOffset + 1 < 10)
	       addch(' ');

	  dogplus = GSisGplus(GDgetEntry(gopherdir, iOffset));
	  DisplayTitle(GDgetEntry(gopherdir, iOffset), COLS-13, dogplus);
     }
}


/* scline - Screen line relocator.
 *          Returns the line resulting from choice */
int 
scline( iOldGopher, iNewGopher, gophersdir)
  int iOldGopher;     /* Which gopher previously displayed */
  int iNewGopher;     /* New gopher to be displayed */
  GopherDirObj  *gophersdir;
{
     int iPageLen, iLastPageLen;        /* Length of normal, final pages */
     int nMaxPages, nNewPage, nOldPage; /* Natural numbers */
     int iOldLine, iNewLine;            /* Screen locations */
     char sPagenum[40];
     int iMaxGophers;
     
     iMaxGophers = GDgetNumitems(gophersdir);

     if (iNewGopher==0)
	  iNewGopher = GDgetNumitems(gophersdir);

     if ((iNewGopher > iMaxGophers))
	  iNewGopher = 1;
     
     iPageLen = LINES-6;    /* Number of menu lines possible per page */
     
     nMaxPages = PAGECALC(iMaxGophers, iPageLen);    /* Total number of pages */
     nOldPage =  PAGECALC(iOldGopher, iPageLen); 
     nNewPage =  PAGECALC(iNewGopher, iPageLen);
     
     if ((nNewPage < 1) || (nNewPage > nMaxPages))   /* It won't work , make*/
	  return(iOldGopher);                          /* no changes */
     
    iLastPageLen = iMaxGophers % iPageLen;

    /* Lines on last page */

     iOldLine = iOldGopher - ((nOldPage-1)*iPageLen);/* Old Screen location */
     iNewLine = iNewGopher - ((nNewPage-1)*iPageLen);/* New Screen location */
     
     if ((iNewLine < 0) || (iNewLine > iPageLen))
	  return(iOldGopher);
     
     if (nOldPage != nNewPage)    {
	Display_Dir_Page(gophersdir,
                        iNewLine, nNewPage, nMaxPages, iPageLen, iLastPageLen);
	CURcenterline(CursesScreen,GDgetTitle(gophersdir), 2);       /*** Draw the title ***/
   }
	

     sprintf(sPagenum, "  Page: %d/%d", nNewPage, nMaxPages);
     Draw_Status(sPagenum);
     mvaddstr(MENULINE(iOldLine), 1, "   ");
     mvaddstr(MENULINE(iNewLine), 1, "-->");
     refresh();

     return(iNewGopher);
}

/*
** This routine draws a numbered menu
** from a gopherdirobj
** 
** It returns the number that the user selected, or it returns
** zero if the user decided to cancel.
**
**       RETURN Code isnt used currently anywhere!
*/

int 
GetMenu(gd, typedchar, redisplay)
  GopherDirObj *gd;   /** where the items are **/
  int          *typedchar;
  boolean      redisplay;
{
     int ch;                /* Input character */
     int iItem;             /* Display line */
     static int iNewItem=1;
     char sLinenum[5];      /* Used when going to a specific line */
     int numitems;
     /** variables for searching **/
     char search1[100];
     char *search2;
     int sfound;
     int i;

     search1[0] = '\0'; /* search string will be remembered so init now */

     numitems = GDgetNumitems(gd);
     iItem = -1;
     iNewItem = GDgetCurrentItem(gd);

     if (redisplay == TRUE) {

	  /*** Draw the title ***/
	  CURcenterline(CursesScreen,GDgetTitle(gd), 2);

	  /* Move to the last line that we were sitting on */
	  iItem = scline(iItem, iNewItem, gd);
     } else
	  iItem = GDgetCurrentItem(gd);


     while (1) {
#ifdef VMS
	  if (HadVMSInt) {
	       HadVMSInt = FALSE;
	       ch = 'q';
	  }
	  else
	       ch = CURgetch(CursesScreen);
#else
	  ch = CURgetch(CursesScreen);
#endif

 	  /* Note letters not in here are picked up by default and passed back
 		to caller for processing */
	  switch(ch)
	  {
	  case 'j':
	  case '\016':
	  case KEY_DOWN:

	       iNewItem = iItem + 1; /* Advance down the page */
	       break;
	       
	  case 'k':
	  case '\020':   /*** For those emacs dudes **/
	  case KEY_UP:

	       iNewItem = iItem - 1; /* Back up */
 	       break;


	  case '+':  /** Like in elm **/
	  case '>':  /** Like in nn  **/
	  case ' ':  /** Like in the pager ***/
	  case KEY_NPAGE:
	       /*** Go down a page ***/
	       iNewItem = iItem + (LINES -6);
		    if (iNewItem > numitems)
			 iNewItem = numitems;
	       break;

	       
	  case '-':
	  case '<':
	  case KEY_PPAGE:
	  case 'b':      /*** Like in the pager ***/
	       /*** Go up a page ***/

	       iNewItem = iItem - (LINES - 6);
	       if ( iNewItem < 0 )
		    iNewItem = 1;
               break;

	  case '1': case '2': case '3': case '4': case '5':
	  case '6': case '7': case '8': case '9': case '0':
	       
	       sLinenum[0] = (char) ch;
	       sLinenum[1] = '\0';
	       
	       if (CUROldGetOneOption(CursesScreen, "View item number: ", sLinenum) <0) {
		    scline(iItem, iNewItem, gd);
		    break;
	       }
	       
	       if (atoi(sLinenum) <= numitems)
		    iNewItem = atoi(sLinenum); /* Jump */
	       else {
		    Debug("GetMenu beepo digit\r\n",NULL)
		    CURBeep(CursesScreen);
		    break;
	       }

	       iItem = scline(iItem, iNewItem, gd);

	       if (iNewItem > 0 && iNewItem <= GDgetNumitems(gd)) {
		    *typedchar = '\n';
		    GDsetCurrentItem(gd, iNewItem);
		    return(iItem);
	       }


	       break;
	       
	  case '/': 
	  case 'n':
	       sfound = 0;

	       if (search1[0] == '\0' && ch == 'n') {
		    CursesErrorMsg("Use '/' to define a search first...");
		    iItem = scline(-1,iItem, gd);
		    break;
	       }

	       if (search1[0] == '\0' || ch == '/')
		    if (CURGetOneOption(CursesScreen, "Search directory titles for", search1)<0) {
			 iItem = scline(-1,iItem, gd);
			 break;
		    }
	       if (strlen(search1) == 0) {
		    iItem = scline(-1, iItem, gd);
		    break;
	       }
	       
	       /*
		* Start searching from next item
		*/

	       for (i=iItem; i < numitems && sfound==0; i++) {
		    search2 = GSgetTitle(GDgetEntry(gd, i));
		    if (strcasestr(search2, search1) != NULL ) {
			 iNewItem = i+1;
			 sfound = 1;
		    }
	       }
	       /* if it wasn't found after the current line start
		  from the beginning again
	       */

               for ( i= 0 ; i < iItem && sfound==0;i++)  {
                       search2 = GSgetTitle(GDgetEntry(gd,i));
                       if (strcasestr(search2,search1) != NULL ) {
                               iNewItem = i+1;
                               sfound = 1;
                       }
               }
	       if (sfound == 0) {
		    search1[0] = '\0';
		    Debug("GetMenu beep search\r\n",NULL)
		    CURBeep(CursesScreen);
		    CursesErrorMsg("Search failed...");
	       }

	       if (ch != 'n')
		    iItem = scline(-1, iNewItem, gd);
               break;

	  
	  case '\0':
	       break;

	  default:
	       if (ch == KEY_LEFT  || ch == 'h' || ch == '\002')
		    ch = 'u';
	       if (ch == KEY_RIGHT || ch == 'l' || ch == '\006') 
		    ch = '\n';
	       *typedchar = ch;
	       GDsetCurrentItem(gd, iItem);
	       return(iItem);
	  }

	  iItem = scline(iItem, iNewItem, gd);
     
	  refresh();
     
     }

}
