/*	AT386:	 hacked tcap.c for the 386 console, when you don't
 *		have libtermcap.   grrr.
 *
 * $Log: at386.c,v $
 * Revision 1.1  1994/02/01 03:29:11  jkh
 * Initial revision
 *
 * Revision 1.9  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.8  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.7  1992/04/10  18:47:25  pgf
 * change abs to absol to get rid of name conflicts
 *
 * Revision 1.6  1991/09/10  01:19:35  pgf
 * re-tabbed, and moved ESC and BEL to estruct.h
 *
 * Revision 1.5  1991/08/07  12:34:39  pgf
 * added RCS log messages
 *
 * revision 1.4
 * date: 1991/06/19 01:32:21;
 * change name of howmany 'cuz of HP/UX conflict
 * sheesh
 * 
 * revision 1.3
 * date: 1991/05/31 10:27:03;
 * moved PRETTIER_SCROLL #define to estruct.h
 * 
 * revision 1.2
 * date: 1990/10/01 10:37:32;
 * un-#ifdef spal()
 * 
 * revision 1.1
 * date: 1990/09/21 10:24:39;
 * initial vile RCS revision
 */

#define termdef 1			/* don't define "term" external */

#include	"estruct.h"
#include	"edef.h"

#if AT386

#define NROW	25			/* Screen size. 		*/
#define NCOL	80			/* Edit if you want to. 	*/
#define MARGIN	8
#define SCRSIZ	64
#define NPAUSE	10			/* # times thru update to pause */

extern int	ttopen();
extern int	ttgetc();
extern int	ttputc();
extern int	tgetnum();
extern int	ttflush();
extern int	ttclose();
extern int	at386kopen();
extern int	at386kclose();
extern int	at386move();
extern int	at386eeol();
extern int	at386eeop();
extern int	at386beep();
extern int	at386rev();
extern int	at386cres();
extern int	at386open();
extern int	tput();
extern char	*tgoto();
#if	COLOR
extern	int	at386fcol();
extern	int	at386bcol();
#endif
#if	SCROLLCODE
extern	int	at386scroll_delins();
#endif

#define TCAPSLEN 315
char at386buf[TCAPSLEN];
char *UP, PC, *CM, *CE, *CL, *SO, *SE;

#if	SCROLLCODE
char *DL, *AL, *SF, *SR;
#endif

TERM term = {
	NROW-1,
	NROW-1,
	NCOL,
	NCOL,
	MARGIN,
	SCRSIZ,
	NPAUSE,
	at386open,
	ttclose,
	at386kopen,
	at386kclose,
	ttgetc,
	ttputc,
	ttflush,
	at386move,
	at386eeol,
	at386eeop,
	at386beep,
	at386rev,
	at386cres
#if	COLOR
	, at386fcol,
	at386bcol
#endif
#if	SCROLLCODE
	, NULL		/* set dynamically at open time */
#endif
};

at386open()
{
	char *t, *p, *tgetstr();
	char tcbuf[1024];
	char *tv_stype;
	char err_str[72];

	CL = "\033[2J\033[H";
	CE = "\033[K";
	UP = "\033[A";
	SE = "\033[m";
	SO = "\033[7m";
	revexist = TRUE;


#if SCROLLCODE
	DL = "\033[1M";
	AL = "\033[1L";
	term.t_scroll = at386scroll_delins;
#endif
	ttopen();
}

at386kopen()
{
	strcpy(sres, "NORMAL");
}

at386kclose()
{
}

csi()
{
	ttputc(ESC);
	ttputc('[');
}

ansiparm(n)
register int	n;
{
	register int q,r;

	q = n/10;
	if (q != 0) {
		r = q/10;
		if (r != 0) {
			ttputc((r%10)+'0');
		}
		ttputc((q%10) + '0');
	}
	ttputc((n%10) + '0');
}

at386move(row, col)
register int row, col;
{
	csi();
	if (row) ansiparm(row+1);
	ttputc(';');
	if (col) ansiparm(col+1);
	ttputc('H');
}

at386eeol()
{
	fputs(CE,stdout);
}

at386eeop()
{
	fputs(CL,stdout);
}

at386rev(state) 	/* change reverse video status */
int state;		/* FALSE = normal video, TRUE = reverse video */
{
	static int revstate = -1;
	if (state == revstate)
		return;
	revstate = state;
	if (state) {
		if (SO != NULL)
			fputs(SO,stdout);
	} else {
		if (SE != NULL)
			fputs(SE,stdout);
	}
}

at386cres()	/* change screen resolution */
{
	return(TRUE);
}

#if SCROLLCODE

/* 
PRETTIER_SCROLL is prettier but slower -- it scrolls 
		a line at a time instead of all at once.
*/

/* move howmany lines starting at from to to */
at386scroll_delins(from,to,n)
{
	int i;
	if (to == from) return;
#if PRETTIER_SCROLL
	if (absol(from-to) > 1) {
		at386scroll_delins(from, (from<to) ? to-1:to+1, n);
		if (from < to)
			from = to-1;
		else
			from = to+1;    
	}
#endif
	if (to < from) {
		at386move(to,0);
		for (i = from - to; i > 0; i--)
			fputs(DL,stdout);
		at386move(to+n,0);
		for (i = from - to; i > 0; i--)
			fputs(AL,stdout);
	} else {
		at386move(from+n,0);
		for (i = to - from; i > 0; i--)
			fputs(DL,stdout);
		at386move(from,0);
		for (i = to - from; i > 0; i--)
			fputs(AL,stdout);
	}
}

#endif

spal(dummy)	/* change palette string */
{
	/*	Does nothing here	*/
}


#if	COLOR
at386fcol()	/* no colors here, ignore this */
{
}

at386bcol()	/* no colors here, ignore this */
{
}
#endif

at386beep()
{
	ttputc(BEL);
}


#if	FLABEL
int
fnclabel(f, n)		/* label a function key */
int f,n;	/* default flag, numeric argument [unused] */
{
	/* on machines with no function keys...don't bother */
	return(TRUE);
}
#endif
#else

hello()
{
}

#endif
