/*
 * The routines in this file provide support for ANSI style terminals
 * over a serial line. The serial I/O services are provided by routines in
 * "termio.c". It compiles into nothing if not an ANSI device.
 *
 * $Log: ansi.c,v $
 * Revision 1.1  1994/02/01 03:29:10  jkh
 * Initial revision
 *
 * Revision 1.12  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.11  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.10  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.9  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.8  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.7  1992/04/10  18:47:25  pgf
 * change abs to absol to get rid of name conflicts
 *
 * Revision 1.6  1991/11/16  18:28:25  pgf
 * removed an old ifdef
 *
 * Revision 1.5  1991/09/10  01:19:35  pgf
 * re-tabbed, and moved ESC and BEL to estruct.h
 *
 * Revision 1.4  1991/08/07  12:34:39  pgf
 * added RCS log messages
 *
 * revision 1.3
 * date: 1991/06/19 01:32:21;
 * change name of howmany 'cuz of HP/UX conflict
 * sheesh
 *
 * revision 1.2
 * date: 1991/05/31 10:26:19;
 * moved PRETTIER_SCROLL #define to estruct.h
 *
 * revision 1.1
 * date: 1990/09/21 10:24:38;
 * initial vile RCS revision
 */

#define termdef 1			/* don't define "term" external */

#include	"estruct.h"
#include	"edef.h"

#if	ANSI

#if	AMIGA
#define NROW	23			/* Screen size.			*/
#define NCOL	77			/* Edit if you want to.		*/
#endif

#if	MSDOS
#define NROW    25
#define NCOL    80
#undef SCROLLCODE
#define SCROLLCODE 0			/* ANSI.SYS doesn't do scrolling */
#endif

#ifndef NROW
#define NROW	24			/* Screen size.			*/
#define NCOL	80			/* Edit if you want to.		*/
#endif

#define NPAUSE	100			/* # times thru update to pause */
#define MARGIN	8			/* size of minimim margin and	*/
#define SCRSIZ	64			/* scroll size for extended lines */

extern	void	ansimove   P((int,int));
extern	void	ansieeol   P((void));
extern	void	ansieeop   P((void));
extern	void	ansibeep   P((void));
extern	void	ansiopen   P((void));
extern	void	ansirev    P((int));
extern	void	ansiclose  P((void));
extern	void	ansikopen  P((void));
extern	void	ansikclose P((void));
extern	int	ansicres   P((char *));
#if SCROLLCODE
extern	void	ansiscroll P((int,int,int));
#endif

#if	COLOR
extern	void	ansifcol P((int));
extern	void	ansibcol P((int));

int	cfcolor = -1;		/* current forground color */
int	cbcolor = -1;		/* current background color */

#if	AMIGA
/* apparently the AMIGA does not follow the ANSI standards as regards to
 * colors ...maybe because of the default palette settings?
 */
int coltran[8] = {2, 3, 5, 7, 0, 4, 6, 1};	/* color translation table */
#endif
#endif

/*
 * Standard terminal interface dispatch table. Most of the fields point into
 * "termio" code.
 */
TERM	term	= {
	NROW-1,
	NROW-1,
	NCOL,
	NCOL,
	MARGIN,
	SCRSIZ,
	NPAUSE,
	ansiopen,
	ansiclose,
	ansikopen,
	ansikclose,
	ttgetc,
	ttputc,
	ttflush,
	ansimove,
	ansieeol,
	ansieeop,
	ansibeep,
	ansirev,
	ansicres
#if	COLOR
	, ansifcol,
	ansibcol
#endif
#if SCROLLCODE
#define SCROLL_REG 1
	, ansiscroll
#endif
};

static	void	ansiparm P((int));
#if SCROLL_REG
static	void	ansiscrollregion P((int,int));
#endif

static void
csi P((void))
{
	ttputc(ESC);
	ttputc('[');
}

#if	COLOR
void
ansifcol(color)		/* set the current output color */
	int	color;	/* color to set */
{
	if (color == cfcolor)
		return;
	csi();
#if	AMIGA
	ansiparm(coltran[color]+30);
#else
	ansiparm(color+30);
#endif
	ttputc('m');
	cfcolor = color;
}

void
ansibcol(color)		/* set the current background color */
	int	color;	/* color to set */
{
	if (color == cbcolor)
		return;
	csi();
#if	AMIGA
	ansiparm(coltran[color]+40);
#else
	ansiparm(color+40);
#endif
	ttputc('m');
	cbcolor = color;
}
#endif

void
ansimove(row, col)
int	row;
int	col;
{
	csi();
	if (row) ansiparm(row+1);
	ttputc(';');
	if (col) ansiparm(col+1);
	ttputc('H');
}

void
ansieeol()
{
	csi();
	ttputc('K');
}

void
ansieeop()
{
#if	COLOR
	ansifcol(gfcolor);
	ansibcol(gbcolor);
#endif
	csi();
	ttputc('J');
}

void
ansirev(state)		/* change reverse video state */
int state;	/* TRUE = reverse, FALSE = normal */
{
#if	COLOR
	int ftmp, btmp;	/* temporaries for colors */
#else
	static int revstate = -1;
	if (state == revstate)
		return;
	revstate = state;
#endif

	csi();
#if COLOR && MSDOS
	ttputc('1');	/* bold-on */
#else
	if (state) ttputc('7');	/* reverse-video on */
#endif
	ttputc('m');

#if	COLOR
#if	MSDOS
	/*
	 * Setting reverse-video with ANSI.SYS seems to reset the colors to
	 * monochrome.  Using the colors directly to simulate reverse video
	 * works better. Bold-face makes the foreground colors "look" right.
	 */
	ftmp = cfcolor;
	btmp = cbcolor;
	cfcolor = -1;
	cbcolor = -1;
	ansifcol(state ? btmp : ftmp);
	ansibcol(state ? ftmp : btmp);
#else	/* normal ANSI-reverse */
	if (state == FALSE) {
		ftmp = cfcolor;
		btmp = cbcolor;
		cfcolor = -1;
		cbcolor = -1;
		ansifcol(ftmp);
		ansibcol(btmp);
	}
#endif	/* MSDOS vs ANSI-reverse */
#endif	/* COLOR */
}

int
ansicres(flag)	/* change screen resolution */
char	*flag;
{
	return(TRUE);
}

void
spal(dummy)		/* change palette settings */
char	*dummy;
{
	/* none for now */
}

void
ansibeep()
{
	ttputc(BEL);
	ttflush();
}

#if SCROLLCODE

/* if your ansi terminal can scroll regions, like the vt100, then define
	SCROLL_REG.  If not, you can use delete/insert line code, which
	is prettier but slower if you do it a line at a time instead of
	all at once.
*/

/* move howmany lines starting at from to to */
void
ansiscroll(from,to,n)
int	from;
int	to;
int	n;
{
	int i;
	if (to == from) return;
#if SCROLL_REG
	if (to < from) {
		ansiscrollregion(to, from + n - 1);
		ansimove(from + n - 1,0);
		for (i = from - to; i > 0; i--)
			ttputc('\n');
	} else { /* from < to */
		ansiscrollregion(from, to + n - 1);
		ansimove(from,0);
		for (i = to - from; i > 0; i--) {
			ttputc(ESC);
			ttputc('M');
		}
	}
	ansiscrollregion(0, term.t_mrow);

#else /* use insert and delete line */
#if PRETTIER_SCROLL
	if (absol(from-to) > 1) {
		ansiscroll(from, (from<to) ? to-1:to+1, n);
		if (from < to)
			from = to-1;
		else
			from = to+1;
	}
#endif
	if (to < from) {
		ansimove(to,0);
		csi();
		if (from - to > 1) ansiparm(from - to);
		ttputc('M'); /* delete */
		ansimove(to+n,0);
		csi();
		if (from - to > 1) ansiparm(from - to);
		ttputc('L'); /* insert */
	} else {
		ansimove(from+n,0);
		csi();
		if (to - from > 1) ansiparm(to - from);
		ttputc('M'); /* delete */
		ansimove(from,0);
		csi();
		if (to - from > 1) ansiparm(to - from);
		ttputc('L'); /* insert */
	}
#endif
}

#if SCROLL_REG
static void
ansiscrollregion(top,bot)
int	top;
int	bot;
{
	csi();
	if (top) ansiparm(top + 1);
	ttputc(';');
	if (bot != term.t_nrow) ansiparm(bot + 1);
	ttputc('r');
}
#endif

#endif

void
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

void
ansiopen()
{
	strcpy(sres, "NORMAL");
	revexist = TRUE;
	ttopen();
}

void
ansiclose()
{
#if	COLOR
	ansifcol(7);
	ansibcol(0);
#endif
	/*ttclose();*/
}

void
ansikopen()	/* open the keyboard (a noop here) */
{
}

void
ansikclose()	/* close the keyboard (a noop here) */
{
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

#endif	/* ANSI */
