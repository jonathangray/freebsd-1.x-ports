/********************************************************************
 * lindner
 * 3.14
 * 1993/08/19 20:22:45
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/CURcurses.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: CURcurses.c
 * Abstraction of all Curses Functions
 *********************************************************************
 * Revision History:
 * CURcurses.c,v
 * Revision 3.14  1993/08/19  20:22:45  lindner
 * Mitra's Debug patch
 *
 * Revision 3.13  1993/08/16  18:17:01  lindner
 * Fixes from F.Macrides:
 *
 * Added temporary code to work around DECC/AXP's problems with screen
 * clearing and cursor homing (we'll get rid of that code if the problem
 * goes away in the next version of DECC/AXP).  It's bolding via
 * standout() or wstandout(win) still doesn't work, but that's not a
 * serious functional problem for gopher users on Alphas.
 *
 * Added exit block to ensure that the terminal characteristics are
 * retored and cleanups are done on VMS.
 *
 * Added code for getting terminal characteristics
 * from the terminal table on VMS.
 *
 * Replaced/modified Cruft for VMS with routines which handle both
 * Control C and Control Y, enable use of ReallyQuit(), and restore all
 * original terminal characteristics for spawns and intentional or
 * unintentional exits.  Did it in a way that should stay transparent to
 * the otherwise "for-Unix" code, and should hold up with future mods or
 * enhancements of that code.  Standout() doesn't work at all and
 * endwin() and delwin() are unreliable on Alphas (due to bugs in the
 * Alpha's Curses library).  Andrew Heyler is looking into workarounds,
 * should DEC not fix the bugs soon.  Code compiles with DECC on Alphas
 * without warnings or error messages, but still get lots of
 * "informational" messages due to incomplete prototyping (no problems or
 * compiler messages with VAXC).
 *
 * Revision 3.12  1993/08/12  06:32:06  lindner
 * Add needed variable
 *
 * Revision 3.11  1993/08/09  20:44:48  lindner
 * Fix for really long strings
 *
 * Revision 3.10  1993/08/09  20:29:50  lindner
 * Get rid of the beep during a ^G in CURwgetstr().
 *
 * Make CURChoice() delete its window when it exits.
 *
 * Revision 3.9  1993/07/30  17:31:34  lindner
 * Mods to support AskP:
 *
 * Revision 3.8  1993/07/27  02:02:47  lindner
 * More comments
 *
 * Revision 3.7  1993/07/23  04:33:48  lindner
 * Mods to curchoice for default
 *
 * Revision 3.6  1993/07/20  23:10:17  lindner
 * none
 *
 * Revision 3.5  1993/04/15  21:23:36  lindner
 * Removed extraneous wattron/wattroff
 *
 * Revision 3.4  1993/03/26  19:42:32  lindner
 * Fix for skip by two problem in CURrequest
 *
 * Revision 3.3  1993/03/18  23:15:24  lindner
 * Mods to support titles inside of a CURrequest.
 *
 * Revision 3.2  1993/02/16  23:26:02  lindner
 * Fixes for SIG_ERR (for Crays)
 *
 * Revision 3.1.1.1  1993/02/11  18:02:56  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.8  1993/01/11  20:25:31  lindner
 * Fixed weird wprintw error on EP/IX.
 *
 * Revision 1.8  1993/01/11  20:25:31  lindner
 * Fixed weird wprintw error on EP/IX.
 *
 * Revision 1.7  1993/01/09  02:16:21  lindner
 * Changed (void*)-1 constructs to SIG_ERR
 *
 * Revision 1.6  1993/01/09  01:28:11  lindner
 * Replaced hosed Log messages (Ooops!)
 *
 * Revision 1.5  1993/01/09  01:24:42  lindner
 * Added CURchoice(), let's you choose one option from [2-9] items.
 *
 * Revision 1.4  1993/01/09  00:49:16  lindner
 * More mods for VMS from jqj.  Looks like better ctrl-y and ctrl-c
 * processing.
 *
 * Revision 1.3  1993/01/06  17:05:46  lindner
 * Added nl() to CURexit() for EP/IX machines.
 *
 * Revision 1.2  1992/12/31  05:55:44  lindner
 * Mods for VMS
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#include "CURcurses.h"
#include "Malloc.h"

#include <signal.h>
#ifndef SIG_ERR
#define SIG_ERR ((void *) -1)
#endif
#include "Stdlib.h"
#include "compatible.h"
#include "Debug.h"

#ifdef VMS
static int w_getch();
#undef wgetch
#define wgetch w_getch
void setterm_pas();
void resetterm();
void VMSCURinit(/* CursesObj* */);
void VMSinit();
void VMSexit();
int spawn_DCLprocess();
int DCLspawn_exception();
#endif /* VMS */
 

/*
 * Initialize data space for various screen information
 */

CursesObj *
CURnew()
{
     CursesObj *cur;

     cur = (CursesObj *) malloc(sizeof(CursesObj));

     cur->Screen       = NULL;
     cur->Termtype     = STRnew();
     cur->Clearscreen  = STRnew();
     cur->AudibleBell  = STRnew();
     cur->Highlighton  = STRnew();
     cur->Highlightoff = STRnew();

     cur->inCurses     = FALSE;
     cur->sigtstp      = SIG_ERR;
     cur->sigwinch     = SIG_ERR;

     CURinit(cur);
     
     return(cur);

}


/*
 * Initialize various strings and such. 
 */

void
CURinit(cur)
  CursesObj *cur;
{
#ifdef VMS
     VMSCURinit(cur);
#else
     int err;
     static char terminal[1024];
     static char capabilities[1024];   /* String for cursor motion */
     static char *ptr = capabilities;  /* for buffering         */
     char *cp;


     /*** Set the terminal type ***/
     if (getenv("TERM") != NULL)
	  CURsetTerm(cur, getenv("TERM"));
     else 
	  CURsetTerm(cur, "unknown");

     err = tgetent(terminal, CURgetTerm(cur));
     
     if (err !=1)
	  CURsetTerm(cur, "unknown");

     /*** Get the clearscreen code ***/
     if ((cp = (char *)tgetstr("cl", &ptr)) != NULL)
	  CURsetCLS(cur, cp);
     else
	  CURsetCLS(cur, "");

     /*** Set the bell ***/
     if ((cp = (char *) tgetstr("bl", &ptr)) != NULL)
	  CURsetBell(cur, cp);
     else
	  CURsetBell(cur, "\007");

     /*** Set the highlight codes ***/
     if ((cp = (char *) tgetstr("so", &ptr)) != NULL) {
	  CURsetHighon(cur, cp);
	  if ((cp = (char *) tgetstr("se", &ptr)) != NULL)
	       CURsetHighoff(cur, cp);
     } else {
	  CURsetHighon(cur, "");
	  CURsetHighoff(cur, "");
     }
     CURsetScreen(cur,initscr());
#endif
     cur->inCurses = FALSE;
     
}


/* 
 * Given a properly "CURnew" cursesobj, initialize the screen..
 */

void
CURenter(cur)
  CursesObj *cur;
{
     /* for safety */
     if (cur->inCurses == TRUE)
	  return;

#ifdef VMS
     (void)setterm_pas();
     CURsetScreen(cur,initscr());
#else
     tputs(CURgetCLS(cur),1,CURoutchar);
     fflush(stdout); 
#endif

     cur->inCurses = TRUE;

     CURwenter(cur,stdscr);

#ifdef SIGWINCH
     if (cur->sigwinch != SIG_ERR)
	  signal(SIGWINCH, cur->sigwinch);
#endif
#ifndef VMS
     if (cur->sigtstp != SIG_ERR)
	  signal(SIGTSTP, cur->sigtstp);
#endif
}

/*
 * Set up processing for the window (especially for system V curses!
 */

void
CURwenter(cur, win)
  CursesObj *cur;
  WINDOW *win;
{
     cbreak();
     noecho();
     nonl();
#ifdef SYSVCURSES
     intrflush(win, FALSE);
     nodelay(win, FALSE);
#ifndef ultrix			/** Causes wgetch to dump core in ultrix **/
     keypad(win, TRUE);
#endif
#endif
}     
/*
 * Exit curses system.
 */

void
CURexit(cur)
  CursesObj *cur;
{
     
     if (!cur->inCurses)
	return;
     cur->inCurses = FALSE;
     echo();
     nl();
     endwin();

#ifdef SYSVCURSES
     keypad(stdscr, FALSE);
#endif

#ifdef VMS
     (void)resetterm();
#else
     tputs(CURgetCLS(cur),1,CURoutchar);
     fflush(stdout);
 
     cur->sigtstp = signal(SIGTSTP, SIG_DFL);
#endif

#ifdef SIGWINCH
     cur->sigwinch = signal(SIGWINCH, SIG_DFL);
#endif

}



/*
 * send a character to stdout, not really curses, but we do use it...
 */

int
CURoutchar(c)
  char c;
{
     /** output the given character.  From tputs... **/
     /** Note: this CANNOT be a macro!              **/
     
     putc(c, stdout);
     return(c);
}


/*
 * Centerline, uses curses routines to center a line.
 */
void CURcenterline(cur, theline, yval)
  CursesObj *cur;
  char      *theline;
  int       yval;
{
     mvaddstr(yval, (COLS - strlen(theline))/2, theline);
}


/*
 * CURwgetstr is a replacement of getstr that allows editing of the string
 *
 * if the user types control codes we don't recognize, it's returned instead
 *
 * We assume that the incoming string is shorter than the max..
 *
 */


int
CURwgetstr(cur, win, inputline, maxlength, hidden)
  CursesObj *cur;
  WINDOW    *win;
  char      *inputline;
  int       maxlength;
  boolean   hidden;
{
     int pointer = 0;
     int curpointer = 0;
     int ch;
     int y,x;

     CURwenter(cur, win);
     cbreak();
     noecho();

     wstandout(win);

     /*** Check to see if there's something in the inputline already ***/
     
     while (inputline[pointer] != '\0') {
	  if (hidden)
	       waddch(win, '*');
	  else
	       waddch(win, inputline[pointer]);
	  pointer ++;
	  curpointer ++;
     }

     wrefresh(win);

     for (;;) {
	  ch = CURwgetch(cur,win);

	  switch (ch) {

	  case '\n':
	       inputline[pointer] = '\0';
	       return(ch);
	       break;

	  /**  Backspace and delete **/

	  case '\b':
	       if (curpointer > 0) {
		    char *cp;

		    getyx(win, y,x);
		    wmove(win, y, x-1);
		    
		    /** Update the string **/

		    for (cp = inputline +curpointer-1; *cp != '\0'; cp++) {
			 *cp = *(cp+1);
			 if (*cp != '\0'){
			      if (hidden)
				   waddch(win, '*');
			      else
				   waddch(win, *cp);
			 }
		    }
		    *cp = '\0';
		    waddch(win, ' ');
		    waddch(win, ' ');

		    pointer--;
		    curpointer--;

		    wmove(win, y, x-1);
	       
		    wrefresh(win);
	       } else
		    CURBeep(cur);
	       break;

	  case '\007':  /*** ^G cancel... ***/
	       wstandend(win);
	       return(-1);
	       break;
	       
	  /*** Kill character (ctrl-u) ***/
	  case '\025':
	       while (pointer!=0) {
		    waddch(win,'\010');
		    waddch(win, ' ');
		    waddch(win, '\010');
	       
		    inputline[--pointer] = '\0';
		    curpointer = 0;
	       }
	       wrefresh(win);
	       break;

	  case KEY_LEFT:
	       if (curpointer > 0) {

		    curpointer--;
		    getyx(win, y, x);
		    wmove(win, y, x-1);
		    wrefresh(win);
	       }
	       break;

	  case KEY_RIGHT:
	       if (curpointer<pointer) {
		    int y,x;

		    curpointer++;
		    getyx(win, y, x);
		    wmove(win, y, x+1);
		    wrefresh(win);
	       }
	       break;

	  default:
	       if (isprint(ch) && curpointer == maxlength) {
		    Debug("CURwgetstr Beep %d\r\n",cur)
		    CURBeep(cur);
	       }
	       else if (isprint(ch)) {

		    inputline[curpointer++] = ch;

		    if (curpointer > pointer) {
			 pointer = curpointer;
			 inputline[curpointer+1] = '\0';
		    }
		    if (hidden)
			 waddch(win, '*');
		    else
			 waddch(win, ch);
		    wrefresh(win);
	       }
	       else {
		    wstandend(win);
		    return(ch);
	       }
	  } /* switch */
     } /* for */
}


/*
 * This stuff is stolen and modified from hytelnet  Thanks Earl!
 */

int
CURwgetch(cur, window)
  CursesObj *cur;
  WINDOW *window;
{
     int a, b, c;
     
     c = wgetch(window);
     
     if (c == 27) {      /* handle escape sequence */
	  b = wgetch(window);
	  if (b == '[' || b == 'O')
	       a = wgetch(window);
	  else
	       a = b;
	  
	  switch (a) {
	  case 'A': c = KEY_UP; break;
	  case 'B': c = KEY_DOWN; break;
	  case 'C': c = KEY_RIGHT; break;
	  case 'D': c = KEY_LEFT; break;
	  case '5':                       /* vt 300 prev. screen */
	       if (b == '[' && wgetch(window) == '~')
		    c = KEY_PPAGE;
	       break;
	  case '6':                       /* vt 300 next screen */
	       if (b == '[' && wgetch(window) == '~')
		    c = KEY_NPAGE;
	       break;
	  }
     }
     
     /* The many forms of the return key... */
     if ((c == KEY_ENTER)|| (c=='\r')) 
	  c = '\n'; /** SYSV curses Gack! **/
     
     /* The many forms of backspace */
     if (c == '\010' || c == '\177' || c == KEY_BACKSPACE)
	  return('\b');

     return(c);
}

int
CURgetch(cur)
  CursesObj *cur;
{
   return(CURwgetch(cur, stdscr));
}  

/*
 * Resets the screen when a size change has happened
 */

void
CURresize(cur)
  CursesObj *cur;
{
     if (cur->inCurses) {
	  CURexit(cur);
#ifndef VMS
	  CURsetScreen(cur, initscr());
#endif
	  CURenter(cur);
     }
}

/*
 * Get one option displays a message, and gets a response
 *
 * If the Response has something in it, it is displayed and editable
 * 
 * If the user wants to abort, GetOneOption returns a -1, otherwise it
 * returns a 0
 */

int
CURGetOneOption(cur, OptionName, Response)
  CursesObj *cur;
  char *OptionName, *Response;
{
     int i;
     char *message[2];
     char *response[2];

     message[0] = OptionName;
     message[1] = NULL;
     
     response[0] = Response;
     response[1] = NULL;

     i = CURRequest(cur, NULL, message, response);
     
     refresh();
     return(i);
}

/*
 * This is the old version of GetOneOption, for those times when the
 * garsh darn terminal is just too gadblam slow :-)
 */
int
CUROldGetOneOption(cur, OptionName, Response)
  CursesObj *cur;
  char *OptionName, *Response;
{
     int i;
     
     mvaddstr(LINES-1, 0, OptionName);
     standout();
     addstr("    ");
     standend();
     clrtoeol();
     move(LINES-1, strlen(OptionName));
     
     refresh();
     echo();
     i = CURwgetstr(cur, stdscr, Response, 4, FALSE);
     noecho();
     
     return(i);
}



/*
 * Fills in the Response with either a lowercase 'y' or 'n'
 */

void
CURgetYesorNo(cur, OptionName, Response)
  CursesObj *cur;
  char *OptionName, *Response;
{
     int c;
     int posx, posy;

     mvaddstr(LINES-1, 0, OptionName);
     clrtoeol();
     noecho();
     getyx(cur->Screen, posy, posx);
     addch(' ');

     if (*Response == 'y')
	  mvaddstr(posy, posx+1, "y");
     else {
	  *Response = 'n';
	  mvaddstr(posy, posx+1, "n ");
     }
     move(posy, posx+1);

     refresh();

     while (1) {
	  c = CURgetch(cur);

	  if (c == 'y') {
	       mvaddstr(posy, posx+1, "Yes");
	       move(posy, posx+1);
	       refresh();
	       *Response = 'y';
	       *(Response +1) = '\0';
	       return;
	  }
	  else if (c == 'n') {
	       mvaddstr(posy, posx+1, "No ");
	       move(posy, posx+1);
	       refresh();
	       *Response = 'n';
	       *(Response +1) = '\0';
	       return;
	  }
	  
	  else if ((c == '\n')||(c=='\r')) {
	       return;
	  }
#ifdef VMS
	  else if ( c == '\032' ) {	/* control-Z */
		return;
	  }
#endif
	  else {
	       Debug("CURgetYesorNo beep\r\n",NULL)
	       CURBeep(cur);
	  }
     }
}
	  
void 
CURBeep(cur)
  CursesObj *cur;
{
	Debug("CURBeep\r\n",NULL)
#ifdef SYSVCURSES
     beep();
#else
     CURcenterline(cur,CURgetBell(cur),1);
     /* tputs(CURgetBell(cur),1,CURoutchar); */
     /* fflush(stdout); */
#endif
}


void
CURbox(cur, win, height,width)
  CursesObj *cur;
  WINDOW *win;
  int width, height;
{
     int i;

     wmove(win,0,0);

     waddch(win, BOX_UL);
     for (i=0; i<width-2; i++)
	  waddch(win, BOX_HLINE);
     waddch(win, BOX_UR);
     for (i=1; i<height-1; i++) {
	  wmove(win, i,0);
	  waddch(win, BOX_VLINE);
	  wmove(win, i,width-1);
	  waddch(win, BOX_VLINE);
     }

     wmove(win, height-1,0);
     waddch(win, BOX_LL);
     for (i=0; i<width-2; i++)
	  waddch(win, BOX_HLINE);
     waddch(win, BOX_LR);

}


void
CURbutton(cur, win, Label, bright)
  CursesObj *cur;
  WINDOW *win;
  char *Label;
  boolean bright;
{
#ifdef SYSVCURSES
     wattron(win, A_BOLD);
#endif

     if (bright)
	  wstandout(win);

     waddstr(win, "[");
     waddstr(win, Label);
     waddstr(win, "]");

     if (bright)
	  wstandend(win);
#ifdef SYSVCURSES
     wattroff(win, A_BOLD);
#endif
}     



int
CURDialog(cur, Wintitle, Message)
  CursesObj *cur;
  char **Message;
  char *Wintitle;
{
     WINDOW *tempwin;
     int i,messlength=25, winwidth;
     int messheight=0;

     while (Message[messheight] != NULL) {
	  if (strlen(Message[messheight]) > messlength)
	      messlength = strlen(Message[messheight]);
	  messheight++;
     }

     if (messlength > COLS)
	  messlength = COLS -4;
     if (strlen(Wintitle) > messlength)
	  winwidth = strlen(Wintitle) + 2;
     winwidth = messlength + 6;

     if (winwidth < 30)
	  winwidth = 30;
     
     tempwin = newwin(6+messheight, winwidth, (LINES-(6+messheight))/2, (COLS-winwidth) /2);
     CURwenter(cur,tempwin);
     CURbox(cur, tempwin, 6+messheight, winwidth);

     /** Add the message **/
     for (i=0; i<messheight; i++) {
	  int len = strlen(Message[i]), j;

          wmove(tempwin, 2+i,(winwidth - messlength)/2);

	  if (len > messlength) {
	       for (j=0; j < messlength; j++)
		    waddch(tempwin, Message[i][j]);
	  }
	  else
	       waddstr(tempwin, Message[i]);

     }



     /** Add the window title **/
     if (Wintitle != NULL) {
	  wmove(tempwin, 0,(winwidth - strlen(Wintitle))/2);
	  wstandout(tempwin);
	  waddstr(tempwin, Wintitle);
	  wstandend(tempwin);
     }

     /** Add the keyboard labels **/
     wmove(tempwin, 3+messheight, winwidth - 29);
     CURbutton(cur, tempwin, "Cancel - ^G", FALSE);
     waddch(tempwin, ' ');
     CURbutton(cur, tempwin, "OK - Enter", FALSE);

     wrefresh(tempwin);

     switch(CURwgetch(cur, tempwin)) {
     case -1:
     case '\007':
	  delwin(tempwin);
	  return(-1);
     default:
	  delwin(tempwin);
	  return(0);
     }
}


int
CURRequest(cur,Wintitle,Prompts,Stowages)
  CursesObj *cur;
  char *Wintitle;
  char **Prompts;
  char **Stowages;
{
     int things[50];  /** Ack static! **/
     int i;

     for (i=0; Prompts[i]!=NULL; i++) {
	  if (Stowages[i] == NULL)
	       things[i] = CUR_LABEL;
	  else
	       things[i] = CUR_PROMPT;
     }
     things[i] = 0;
     
     return(CURRequest_things(cur, Wintitle, Prompts, Stowages, things));
}

/*
 * things defines what the requested item is, (text, pass, etc..)
 */

int
CURRequest_things(cur,Wintitle,Prompts,Stowages, things)
  CursesObj *cur;
  char *Wintitle;
  char **Prompts;
  char **Stowages;
  int  *things;
{
     WINDOW *tempwin;
     int i,j;
     int numprompts=0;
     int maxpromptwidth =0;
     int currentfield = 0;

     /** Find the number of prompts... and the max width***/
     while (Prompts[numprompts++] != NULL) {
	  /*** Skip non editable prompts ***/
	  if (things[numprompts-1] == CUR_PROMPT ||
	      things[numprompts-1] == CUR_PASSWD) {
	       if (strlen(Prompts[numprompts-1]) > maxpromptwidth)
		    maxpromptwidth = strlen(Prompts[numprompts-1]);
	  } else {
	       if (currentfield == numprompts-1)
		    currentfield++;
	  }
     }

     numprompts --;

     if (numprompts == 0) {
	  return(-1);
     }
     
     tempwin = newwin(6 + numprompts, COLS-2, (LINES-(6+numprompts))/2,1);
     CURwenter(cur,tempwin);
     CURbox(cur,tempwin, 6+numprompts, COLS-2);
     
     /*** Add the window title ***/
     if (Wintitle != NULL) {
	  wmove(tempwin, 0,(COLS -2  - strlen(Wintitle))/2);
	  wstandout(tempwin);
	  waddstr(tempwin, Wintitle);
	  wstandend(tempwin);
     }
     
     /** Add the prompts and typing area **/
     for (i=0; i <numprompts; i++) {
	  wmove(tempwin, 2+i, 2);
	  waddstr(tempwin, Prompts[i]);
	  
	  if (things[i] != CUR_LABEL) {
	       
	       /** Add the black space for the stowage, and the stowage, if it
		 exists **/
	       wmove(tempwin, 2+i, maxpromptwidth +4);
	       wstandout(tempwin);

	       if (things[i] == CUR_PASSWD) {
		    int numchars = strlen(Stowages[i]);

		    for (j=0; j<numchars; j++)
			 waddch(tempwin, '*');
	       } else
		    waddstr(tempwin, Stowages[i]);

	       for (j=strlen(Stowages[i])+maxpromptwidth+4; j< COLS-6; j++) {
		    waddch(tempwin, ' ');
	       }
	       wstandend(tempwin);
	  }
     }

     /** Add the labels **/
     if (numprompts > 1) {
	  wmove(tempwin, 3+numprompts, 3);
	  CURbutton(cur, tempwin, "Switch Fields - TAB", FALSE);
     }

     wmove(tempwin, 3+numprompts, COLS/2);
     CURbutton(cur, tempwin, "Cancel ^G", FALSE);
     waddch(tempwin, ' ');
     CURbutton(cur, tempwin, "Accept - Enter", FALSE);


     while (1) {
	  boolean hidden;

	  if (things[currentfield] == CUR_PASSWD)
	       hidden = TRUE;
	  else
	       hidden = FALSE;

	  wmove(tempwin, 2+currentfield, maxpromptwidth +4);
	  wrefresh(tempwin);
	  switch (CURwgetstr(cur,tempwin,Stowages[currentfield],80, hidden)) {

	  case '\t':
	  case KEY_DOWN:
	       /*** Move to another field ***/
	       do {
		    currentfield = (currentfield +1) % numprompts;
	       } while (Stowages[currentfield] == NULL);
	       break;

	  case KEY_UP:
	       do {
		    currentfield--;
		    if (currentfield <0)
			 currentfield = numprompts-1;
	       } while (Stowages[currentfield] == NULL);
	       
	       break;
	       
	  case '\007':
	  case -1:
	       /*** Cancel ***/
	       delwin(tempwin);
	       return(-1);
	       
	  case '\n':
	       delwin(tempwin);
	       return(0);
	  }
	  
     }

}


/* 
 * CURChoice takes a bunch of titles, throws them on the screen,
 * and asks the user to choose one.
 *
 * Returns the number chosen, or -1 if the user cancels.
 */

int
CURChoice(cur, Wintitle, choices, prompt, default_choice)
  CursesObj *cur;
  char *Wintitle;
  char **choices;
  char *prompt;
  int default_choice;
{
     int numchoices=0, i, maxchoicewidth=0;
     WINDOW *tempwin;
     
     while (choices[numchoices++] != NULL)
	  if ((i=strlen(choices[numchoices-1])) > maxchoicewidth)
	       maxchoicewidth = i;
     
     numchoices--;

     if (numchoices == 0)
	  return(-1);

     if ((i=strlen(prompt)) > maxchoicewidth)
	  maxchoicewidth = i;

     if ((i=strlen(Wintitle)) > maxchoicewidth)
	  maxchoicewidth = i;


     tempwin = newwin(8+numchoices, maxchoicewidth + 10, 
		      (LINES-(6+numchoices))/2, (COLS-(maxchoicewidth+10))/2);

     CURbox(cur, tempwin, 8+numchoices, 10 + maxchoicewidth);

     /*** Add the window title ***/
     if (Wintitle != NULL) {
	  wmove(tempwin, 0,(maxchoicewidth+10 -2  - strlen(Wintitle))/2);
	  wstandout(tempwin);
	  waddstr(tempwin, Wintitle);
	  wstandend(tempwin);
     }

     /*** Add the choices to the screen ***/


     for (i=0; i<numchoices; i++) {
	  wmove(tempwin, 2+i,3);
	  wprintw(tempwin, "%d", i+1);
	  wprintw(tempwin, ". %s", choices[i]);
	  ;
	  
     }

     /** Add the keystroke methods **/
     wmove(tempwin, 5+numchoices, 3);
     wprintw(tempwin, "[Cancel ^G]  [Choose 1-%d]", numchoices);


     /** Add the prompt **/

     wmove(tempwin, 3+numchoices, 3);
     if (default_choice <0)
	  wprintw(tempwin, "%s: ", prompt);
     else
	  wprintw(tempwin, "%s [%d]: ", prompt, default_choice+1);


     wrefresh(tempwin);

     while (1) {
	  i = CURwgetch(cur, tempwin);
	  wrefresh(tempwin);
	  switch (i) {
	  case '\007':
	       delwin(tempwin);
	       return(-1);
	  case '\n':   
	       delwin(tempwin);
	       return(default_choice);
	  }

	  if (i < '1' || i > ('0'+numchoices))
	       CURBeep(cur);
	  else {
	       delwin(tempwin);
	       return(i-'1');
	  }
     }	  

}

/********************** Cruft for VMS follows ****************************/

#ifdef VMS
#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#include <ttdef.h>
#include <tt2def.h>
#include <libclidef.h>
#include <smg$routines.h>
#include <smgdef.h>
#include <smgtrmptr.h>
#ifdef signal
#undef signal
#endif
#include <signal.h>
#ifdef system
#undef system
#endif
#include <processes.h>

#define EFN	0				/* Event flag		*/
static	int	mask = LIB$M_CLI_CTRLY|LIB$M_CLI_CTRLT; /* ^Y and ^T	*/
static	int 	old_msk;			/* Saved control mask	*/
static	short	trap_flag = FALSE;		/* TRUE if AST is set	*/
static $DESCRIPTOR (term_name, "SYS$INPUT:");	/* For channel assigns	*/
short term_chan;				/* The channel		*/
static short first = TRUE;		/* Terminal initialization flag	*/
struct char_buffer_type {		/* Terminal characteristics	*/ 
			  char 		 class;
			  char		 type;
			  unsigned short size;
			  unsigned long  tchars;
			  unsigned long  tchars2;
} oldbuf;
static int in_pos, in_len;			/* For escape sequence	*/
static unsigned char buffer[20];		/*  handling in w_getch	*/
boolean DidCleanup = FALSE;			/* Exit handler flag	*/


/*
 * Define local replacement for wgetch that returns the characters without
 * having to set the terminal /pasthru, which screws up control-Y processing.
 */
static int w_getch(win)
     int win;
{
     int status;
     unsigned short iosb[4];
     
     if (in_pos < in_len)
          return(buffer[in_pos++]);

     status = sys$qiow (0, term_chan, IO$_READVBLK|IO$M_NOECHO|IO$M_NOFILTR,
     			&iosb, 0, 0, &buffer, 1, 0, 0, 0, 0);
     if ((status&1) == 1)
          status = iosb[0];
     if (status == SS$_PARTESCAPE) {
	  /* escape sequence in progress, fake a successful read */
	  status = 1;
     }
     if ((status&1) != 1)
          exit(status);
     in_pos = 1;
     in_len = iosb[1] + iosb[3];
     return (buffer[0]);
}

void
setterm_pas()
{
     int status;
     short iosb[4];

     if(first)
          VMSinit();
     else
          status = lib$disable_ctrl(&mask);

     in_pos = 0; in_len = 0;
}

void
resetterm()
{
     int status;
     
     status = sys$qiow(0,term_chan,IO$_SETMODE,0,0,0,
		       &oldbuf,12,0,0,0,0);
     status = lib$enable_ctrl(&old_msk);
}


/* VMS doesn't have termcap.  Unfortunately, the code in this */
/* module uses termcap just a little bit (it really shouldn't) */
/* rather than doing everything through curses */
 
/* The following simulates tputs, but does not support padding */
int
tputs(cp, affcnt, outc)
     register char *cp;
     int affcnt;
     int (*outc)();
{
     while (*cp)
          outc(*(cp++));
     return(0);
}

#if defined (VMS) && defined (__ALPHA)
/*
 *  Temporary workaround for problems with
 *  Curses library functions on Alphas.
 */
static char VMShome[20];
static char VMSclear[20];

void
VMSClearandHome(cur)
  CursesObj *cur;
{
     printf("%s%s", VMSclear, VMShome);
     fflush(stdout);
}
#endif /* VMS && __ALPHA */

/*
 *  VMSsignal -- F.Macrides 31-Jul-1993 (modification of my LYNX routine)
 *	Sets up AST for both Ctrl-C and Ctrl-Y, with system response to Ctrl-T
 *	 disabled.  If called with a sig other than SIGINT, it will use the C
 *	 library's signal(sig, func).
 *	VMSsignal(SIGINT, SIG_DFL) is treated as a call to resetterm().
 *      Call VMSsignal(SIGINT, SIG_IGN) before system() calls to enable Ctrl-C
 *	 and Ctrl-Y in the subprocess, and then call VMSsignal(SIG_INT, func)
 *	 on return from the subprocess.
 *	For func's which do not invoke an exit, the func should reassert itself.
 *	The VMS C signal() calls do not fully emulate the Unix calls, and
 *	 VMSsignal() is just a "helper", also not a full emulation.
 */

void
(*VMSsignal (sig, func)) (int)
int sig;
void (*func)();
{
     int status;
     short iosb[4];
     static int SIG_IGN_flag;

     /* pass all signals other than SIGINT to signal() */
     if (sig != SIGINT) {
          return(signal(sig, func));
     }

     /* if func is SIG_DFL, treat it as resetterm() */
     if (func == SIG_DFL) {
          resetterm();
	  return(SIG_DFL);
     }

     /* Clear any previous AST */
     if (trap_flag) {
          status = sys$dassgn (term_chan);
	  status = lib$enable_ctrl(&old_msk);
	  trap_flag = FALSE;
     }

     /* if func is SIG_IGN, leave the TT channel closed and the  */
     /* system response to interrupts enabled for system() calls */
     if (func == SIG_IGN)
          return(SIG_IGN);

     /* if we get to here, we have a SIGINT func, so set the AST */
     if(first)
          VMSinit();
     else {
	 status = sys$assign (&term_name, &term_chan, 0, 0);
         status = lib$disable_ctrl(&mask);
     }
     status = sys$qiow (EFN, term_chan,
			IO$_SETMODE|IO$M_CTRLCAST|IO$M_CTRLYAST,
			&iosb, 0, 0,
			func, SIGINT, 0, 0, 0, 0);
     trap_flag = TRUE;
     return(func);
}


/*
 *  VMSCURinit, VMSinit, VMSexit -- F.Macrides 12-Aug-1993
 *	Save termial characteristics at the time when gopher.c's Initialize()
 *	 is called.  Make sure they're retored and that cleanup is done if we
 *	 exit via exit(-1)'s or ACCVIO's.
 */

void
VMSCURinit(cur)
  CursesObj *cur;
{
     int status, i;
     short Type = 0;
     long buf_siz = 20, len;
     unsigned long Addr;
     char ch, Name[20], cl[20], bl[20], so[20], se[20];

     /** Keep DECC from complaining **/
     struct dsc$descriptor_s Name_desc;
     Name_desc.dsc$w_length  = 20;
     Name_desc.dsc$b_class   = DSC$K_CLASS_S;
     Name_desc.dsc$b_dtype   = DSC$K_DTYPE_T;
     Name_desc.dsc$a_pointer = Name;
     
     /*** Initialize the terminal, if we haven't already ***/
     if(first)
          VMSinit();

     /*** Can't handle "unknown" terminal type ***/
     if (oldbuf.type == 0) {
	  CURsetTerm(cur, "unknown");
	  return;
     }

     /*** Get the terminal table ready ***/
     Type = (short) oldbuf.type;
     status = smg$init_term_table_by_type (&Type, &Addr, &Name_desc);
     if (!(status&1))
     {
	  CURsetTerm(cur, "unknown");
	  return;
     }
     
     /*** Set the terminal name ***/
     i = 0;
     while ((Name[i++] != ' ') && i <= 20) ;
     Name[--i] = '\0';
     if (strlen(Name))
          CURsetTerm(cur, Name);
     else
          CURsetTerm(cur, "VMS");

     /*** Get the clearscreen code ***/
     status = smg$get_term_data(&Addr, &SMG$K_ERASE_WHOLE_DISPLAY,
                                &buf_siz, &len, cl);
     if (status&1) {
          cl[len] = '\0';
	  CURsetCLS(cur, cl);
     }
     else
     	  /*** Assume 7-bit ***/
          CURsetCLS(cur, "\033[2J");
     
     /*** Set the bell ***/
     CURsetBell(cur, "\007");

     /*** Set the highlight codes ***/
     status = smg$get_term_data (&Addr, &SMG$K_BEGIN_BOLD,
     				 &buf_siz, &len, so);
     if (status&1) {
          so[len] = '\0';
	  CURsetHighon(cur, so);
     }
     else
	  CURsetHighon(cur, "\033[1m");

     status = smg$get_term_data (&Addr, &SMG$K_BEGIN_NORMAL_RENDITION,
     				 &buf_siz, &len, se);
     if (status&1) {
          se[len] = '\0';
	  CURsetHighoff(cur, se);
     }
     else
	  CURsetHighoff(cur, "\033[m");

#if defined (VMS) && defined (__ALPHA)
    /*
     *  Temporary workaround for problems with
     *  Curses library functions on Alphas.
     */
     strcpy(VMSclear, cl);
     status = smg$get_term_data (&Addr, &SMG$K_HOME,
     				 &buf_siz, &len, VMShome);
     if (status&1)
          VMShome[len] = '\0';
     else
	  strcpy(VMShome, "\033[;H");
#endif
}

void
VMSinit()
{
     int status;
     static unsigned long condition;
     static struct _exit_block {
          unsigned long forward;
          unsigned long address;
          unsigned long zero;
          unsigned long condition;
     } exit_handler_block;

     if(first) {
          /* get a channel and save terminal parms if we haven't already */
	  status = sys$assign (&term_name, &term_chan, 0, 0 );
          status = sys$qiow(0,term_chan,IO$_SENSEMODE,0,0,0,
	  		    &oldbuf,12,0,0,0,0);
          first = FALSE;
          status = lib$disable_ctrl(&mask, &old_msk);

          exit_handler_block.forward   = 0;
          exit_handler_block.address   = (unsigned long) &VMSexit;
          exit_handler_block.zero      = 0;
          exit_handler_block.condition = (unsigned long) &condition;

	  /* declare the exit handler block */
          status = sys$dclexh(&exit_handler_block);
          if (status != SS$_NORMAL) {
               printf("exit_status: %d", status);
          }
     }
}

void
VMSexit()
{
    /*
     * If we didn't get here via CleanupandExit(), it was via an
     *  exit(-1) or ACCVIO, so make *sure* we reset the terminal,
     *  then attempt a cleanup.
     */
     if (!DidCleanup) {
          resetterm();
          CleanupandExit(-1);
     }
}

/*
 *  DCLsystem, spawn_DCLprocess, DCLspawn_exception -- F.Macrides 31-Jul-1993
 *	Exception-handler routines for enabling interrupts and Control-T
 *      during spawns when setjmp() has been established, and the parent
 *	passes a DCL CLI.
 */

int
DCLsystem(command)
char *command;
{
     int status;
     extern void controlc();
     
     VMSsignal(SIGINT, SIG_IGN);
     status = spawn_DCLprocess(command);
     VMSsignal(SIGINT, controlc);
     return(status&1);
}

int
spawn_DCLprocess(command)
char *command;
{
     int status;
     /** Keep DECC from complaining **/
     struct dsc$descriptor_s  command_desc;
     command_desc.dsc$w_length  = strlen(command);
     command_desc.dsc$b_class   = DSC$K_CLASS_S;
     command_desc.dsc$b_dtype   = DSC$K_DTYPE_T;
     command_desc.dsc$a_pointer = command;

     VAXC$ESTABLISH(DCLspawn_exception);

     if (command == "")
          status = lib$spawn(0);
     else
          status = lib$spawn(&command_desc);

     return(status);
}

int
DCLspawn_exception(sigarr, mecharr)
int *sigarr, *mecharr;
{
     int status;
     
     status = lib$sig_to_ret(sigarr, mecharr);
     return(SS$_UNWIND);
}
#endif /* VMS */
