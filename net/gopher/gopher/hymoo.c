/********************************************************************
 * lindner
 * 3.5
 * 1993/08/19 20:22:51
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/hymoo.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: hymoo.c
 * More stuff to do html.
 *********************************************************************
 * Revision History:
 * hymoo.c,v
 * Revision 3.5  1993/08/19  20:22:51  lindner
 * Mitra's Debug patch
 *
 * Revision 3.4  1993/08/16  18:49:17  lindner
 * Alpha and DECC mods
 *
 * Revision 3.3  1993/07/29  17:21:22  lindner
 * eliminate non-used variables
 *
 * Revision 3.2  1993/07/27  05:28:54  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.1.1.1  1993/02/11  18:03:00  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:36:38  lindner
 * Fixes for new rc stuff
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include "String.h"
#include "CURcurses.h"
#include "HTML.h"
#include "gopherrc.h"
#include "globals.h"
#include "Debug.h"

#define HELP "Commands: Use arrow keys to move, '?' for help, 'q' to quit"
#define MOREHELP \
  "-- press space for more, use arrow keys to move, '?' for help, 'q' to quit"
#define MORE "-- press space for more --"

#define STREQ(a,b) (strcmp(a,b) == 0)
#define STRNEQ(a,b,c) (strncmp(a,b,c) == 0)
#define printable(c) (((c)>31 && (c)<=127) || (c)==9 || (c)==10)


boolean more = FALSE;
char *interaddr(), *nextword(), *pop(), *mystrncpy();

#ifdef VMS
#   define strncasecmp strncmp   /* vms doesn't have strncasecmp */
#endif

cleanup()
{
    int lastx=1, lasty=1;

    move(LINES-1, 0);
    mvcur(lasty, lastx, LINES-1, 0);
    clrtoeol();
    refresh();

    CURexit(CursesScreen);
}

/*
 * Returns the page number of a given line number
 */

int
Pagenum(linenum)
  int linenum;
{
     return(linenum/LINES);
}

/*
 * here's where we do all the work
 */

void
HTML_pager(filename, html)
  char *filename;
  HTMLObj *html;
{
     int     ch;
     FILE    *fp;
     int     OldPage=1, CurrentPage=1;
     int     Currentlink = 0;

     CURenter(CursesScreen);

     if ((fp=fopen(filename, "r")) != NULL) {
	  /*** Show the first page ***/
	  showpage(fp, 1, 0, html);
	  if (HTMLGetNumLinks(html) != 0)
	       highlight(TRUE, Currentlink, CurrentPage, html);
     } else {
	  return;
     }

     while (TRUE) {
	  if (CurrentPage != OldPage) {
	       showpage(fp, CurrentPage, OldPage, html);
	       OldPage = CurrentPage;
	  }

	  ch = CURgetch(CursesScreen);
	  switch (ch) {

	  case '\n':		/* Traverse the link */
	       do_html(HTMLGetLink(html, Currentlink));
	       break;

	  case 'u':		/** Exit from this page **/
	  case 'h':
	  case KEY_LEFT:
	       CURexit(CursesScreen);
	       return;
	       break;
	       
	  case '\t':		/* Next Link */
	       if (Currentlink == HTMLGetNumLinks(html) || HTMLGetNumLinks(html)==0) {
			Debug("CURBeep HTML_Pager\r\n",NULL)
		    CURBeep(CursesScreen);
		    break;
	       }

	       OldPage = CurrentPage;

	       /*** Move down, if necessary ***/

	       while (Pagenum(HTMLGetLinenum(html, Currentlink+1)) != CurrentPage) {
		    CurrentPage++;
	       }
		    

	       highlight(FALSE, Currentlink++, CurrentPage, html);
	       highlight(TRUE,  Currentlink, CurrentPage, html);

	       break;

	  case ' ':		/* Next screenful */
	  case KEY_NPAGE:

	       OldPage = CurrentPage;
	       CurrentPage++;
	       break;

	  case KEY_PPAGE:
	  case 'b':
	       OldPage = CurrentPage;
	       CurrentPage--;
	       break;
	  }
     }
}

HTMLMoo_pager(filename, html)
  char *filename;
  HTMLObj *html;
{
    int  c, arrowup=FALSE, show_help=FALSE;
    int  cur = 0, savcur = 0;
    int  oldpage = 0, newpage = 1;
    FILE *fp = NULL;



    while (TRUE) {
	if (oldpage != newpage) {
	    if (showpage(fp, newpage, oldpage, html) > 0) {
		if (arrowup) {
		    cur = HTMLGetLinkMax(html) - 1;
		    arrowup = FALSE;
		} else
		    cur = savcur;
		savcur = 0;
		oldpage = newpage;
	    } else {
		newpage = oldpage;
	    }
	}

	if (!show_help) {
	    if (more)
		statusline(MORE);
	    else
		statusline((char *) NULL);
	}


	c=CURgetch(CursesScreen);

	highlight(TRUE, cur, newpage, html);	/* highlight current link */

	if (show_help) {
	    if (more)
		statusline(MORE);
	    else
		statusline((char *) NULL);
	    show_help = FALSE;
	}

	switch(c) {
	case 'q':	/* quit */
	case 'Q':
	case 4:
            cleanup();
	    return;
	case ' ':	/* next page */
	case '+':
	    newpage++;
	    break;
	case 'b':	/* prev page */
	case '-':
	    newpage--;
	    break;
	case KEY_UP:
	case 'k':
	    if (cur>0) {		/* previous link */
		highlight(FALSE, cur, newpage, html);
		cur--;
	    } else if (oldpage > 1) {	/* previous page */
		newpage--;
		arrowup = TRUE;
	    }
	    break;
	case KEY_DOWN:
	case 'j':
	    if (HTMLGetLinenum(html, cur+1) < (newpage+1)*LINES)  {
		highlight(FALSE, cur, newpage, html);     /** Next link **/
		cur++;
	    } else {			/* next page */
		newpage++;
	    }
	    break;
	case KEY_LEFT:			/* back up a level */
	case 'h':			/* i.e. quit */
            cleanup();
            return;
	    break;
	case KEY_RIGHT:			/* follow a link */
	case 'l':
	case '\n':
	case '\r':
	    if (HTMLGetLinkMax(html) > 0) {
		 /** Traverse link here **/

		    statusline((char *) NULL);
		}
	    break;
	default:
	    if (more)
		statusline(MOREHELP);
	    else
		statusline(HELP);
	    show_help = TRUE;
#ifdef DEBUGGING
	    printw("%d", c);
#endif
	    break;
	}
    }
}


/*
 * display one screen of text
 *
 * Read & display one screenfull of text.
 *
 * Pre-reads one line from the next page, to ensure that the "- more -"
 * message is only displayed when there really is more.
 */
int
showpage(fp, page, oldpage, html)
  FILE *fp;
  int page, oldpage;
  HTMLObj *html;
{
     int col, lineno;
     static char line[512];
     char *cp;
     
     if (page < 1)
	  page = 1;
     
     if (page == oldpage) {		/* nothing to do */
	  return(0);
     } else if (page < oldpage) {	/* have to back up */
	  rewind(fp);
	  oldpage = 0;
     }
     
     if (page == 1 || page != oldpage+1) {
	  more = FALSE;
	  lineno = oldpage*(LINES-1);
	  while (lineno<(page-1)*(LINES-1) && fgets(line, COLS, fp) != NULL) {
	       lineno++;
	  }
     }
     
     lineno = 0;
     clear();
#ifdef VMS
     refresh();
#endif

     while (lineno<(LINES-1) && (more || fgets(line, 512, fp) != NULL)) {
	  more = FALSE;

	  ZapCRLF(line);
	  
	  for (cp = line; *cp != '\0'; cp++) {
	       if ((*cp == '_') && (*(cp +1) == '\b')) {
		    cp +=2;
#ifdef A_BOLD
		    addch(A_BOLD | *cp);
#else
		    addch(*cp);
#endif
	  } else
	       addch(*cp);
	  }
	  addch('\n');
	  lineno++;
     }
     
     if (lineno==(LINES-1) && fgets(line,512, fp) != NULL)
	  more = TRUE;
     standout();
     addstr("<Space> for next page");
     standend();

     refresh();
     return(lineno);
}


/*
 * Return pointer to the second word in a string
 */
char *
nextword(cp)
char *cp;
{
    while (*cp != ' ' && *cp != '\0') cp++;	/* skip non-spaces */
    while (*cp == ' ') cp++;			/* skip spaces */
    return(cp);
}


/*
 * my strncpy() terminates strings with a null byte.
 * Writes a null byte into the n+1 byte of dst.
 */
char *
mystrncpy(dst, src, n)
char *dst, *src;
int n;
{
    char *val;

    val = strncpy(dst, src, n);
    *(dst+n) = '\0';
    return val;
}




/*
 * highlight (or unhighlight) a given link
 */
highlight(flag, cur, curpage, html)
  boolean flag;
  int cur;
  int curpage;
  HTMLObj *html;
{
     /*** Assume that the link is on this page **/
     
     if (HTMLGetLinkMax(html) > 0) {
	  move(HTMLGetLinenum(html, cur) - (curpage-1) * LINES,
	       HTMLGetLinepos(html, cur));
	if (flag) standout();
	addstr(GSgetTitle(HTMLGetLink(html, cur)));
	if (flag) standend();
	refresh();
    }
}


/*
 * display (or hide) the status line
 */
statusline(text)
char *text;
{
    move(LINES-1,0);
    clrtoeol();
    if (text != NULL) {
	standout();
	addstr(text);
	standend();
    }
    refresh();
}


