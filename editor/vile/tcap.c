/*	tcap:	Unix V5, V7 and BS4.2 Termcap video driver
 *		for MicroEMACS
 *
 * $Log: tcap.c,v $
 * Revision 1.1  1994/02/01 03:29:39  jkh
 * Initial revision
 *
 * Revision 1.29  1993/09/16  11:17:43  pgf
 * added missing args to showcpos
 *
 * Revision 1.28  1993/09/16  10:58:20  pgf
 * xterm mouse motions now cause a call to showcpos()
 *
 * Revision 1.27  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.26  1993/08/21  18:21:08  pgf
 * protect against window sizes less than 2, and fix botch that always
 * set i_am_xterm to TRUE
 *
 * Revision 1.25  1993/08/18  16:50:10  pgf
 * bumped mrow from 100 to 200 -- some people have very good eyesight
 *
 * Revision 1.24  1993/08/17  18:20:16  pgf
 * only require that TERM _end_ in "xterm" to turn on xterm hacks
 *
 * Revision 1.23  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.22  1993/07/08  15:03:52  pgf
 * reduce max rows from 200 to 100.  it would take an _awfully_ big screen
 * to have 200 readable lines of text.
 *
 * Revision 1.21  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.20  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.19  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.18  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.17  1992/12/23  09:27:14  foxharp
 * hack in missing CS or SR for xterms
 *
 * Revision 1.16  1992/12/04  09:18:31  foxharp
 * allow the open routine to be called again, to emit TI, KS
 *
 * Revision 1.15  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.14  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.13  1992/04/10  18:47:25  pgf
 * change abs to absol to get rid of name conflicts
 *
 * Revision 1.12  1992/03/24  08:46:02  pgf
 * fixed support for AL,DL -- I hope it's really safe to use tgoto as
 * a generic parm capability expander.  I
 *
 * Revision 1.11  1992/03/24  07:46:34  pgf
 * added support for DL and AL capabilities, which are multi-line insert
 * and delete -- much better in an xterm
 *
 * Revision 1.10  1992/01/22  20:27:47  pgf
 * added TI, TE, KS, KE support, per user suggestion (sorry, forgot who)
 *
 * Revision 1.9  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.8  1991/10/23  12:05:37  pgf
 * NULL initializations should have been 0 instead
 *
 * Revision 1.7  1991/09/10  01:19:35  pgf
 * re-tabbed, and moved ESC and BEL to estruct.h
 *
 * Revision 1.6  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.5
 * date: 1991/08/06 15:26:22;
 * sprintf changes
 * 
 * revision 1.4
 * date: 1991/06/19 01:32:21;
 * change name of howmany 'cuz of HP/UX conflict
 * sheesh
 * 
 * revision 1.3
 * date: 1991/05/31 11:25:14;
 * moved PRETTIER_SCROLL to esturct.h
 * 
 * revision 1.2
 * date: 1990/10/01 10:37:44;
 * un-#ifdef spal()
 * 
 * revision 1.1
 * date: 1990/09/21 10:26:09;
 * initial vile RCS revision
 */

#define termdef 1			/* don't define "term" external */

#include	"estruct.h"
#include	"edef.h"

#if TERMCAP

#define MARGIN	8
#define SCRSIZ	64
#define NPAUSE	10			/* # times thru update to pause */


#define TCAPSLEN 315
char tcapbuf[TCAPSLEN];
char *UP, PC, *CM, *CE, *CL, *SO, *SE;
char *TI, *TE, *KS, *KE;

#if	SCROLLCODE
char *CS, *dl, *al, *DL, *AL, *SF, *SR;
#endif

#if OPT_FLASH
char *vb;	/* visible-bell */
#endif

extern char *tgoto P((char *, int, int));
extern int tgetent P((char *, char *));
extern int tgetnum P((char * ));
extern int tputs P((char *, int, void(*_f)(int) ));

TERM term = {
	0,	/* these four values are set dynamically at open time */
	0,
	0,
	0,
	MARGIN,
	SCRSIZ,
	NPAUSE,
	tcapopen,
	tcapclose,
	tcapkopen,
	tcapkclose,
	ttgetc,
	ttputc,
	ttflush,
	tcapmove,
	tcapeeol,
	tcapeeop,
	tcapbeep,
	tcaprev,
	tcapcres
#if	COLOR
	, tcapfcol,
	tcapbcol
#endif
#if	SCROLLCODE
	, NULL		/* set dynamically at open time */
#endif
};

#define	XtermPos()	TTgetc() - 040

#if OPT_XTERM >= 3
# define XTERM_ENABLE_TRACKING   "\033[?1001h"	/* mouse hilite tracking */
# define XTERM_DISABLE_TRACKING  "\033[?1001l"
#else
# if OPT_XTERM >= 2
#  define XTERM_ENABLE_TRACKING   "\033[?1000h"	/* normal tracking mode */
#  define XTERM_DISABLE_TRACKING  "\033[?1000l"
# else
#  define XTERM_ENABLE_TRACKING   "\033[?9h"	/* X10 compatibility mode */
#  define XTERM_DISABLE_TRACKING  "\033[?9l"
# endif
#endif

static	int	i_am_xterm;

void
tcapopen()
{
	char *t, *p, *tgetstr P((char *, char **));
	char tcbuf[1024];
	char *tv_stype;
	char err_str[72];
	static int already_open = 0;

	if (already_open) 
	{
		if (TI)
			putnpad(TI, strlen(TI));
		if (KS)
			putpad(KS);
		return;
	}

	if ((tv_stype = getenv("TERM")) == NULL)
	{
		puts("Environment variable TERM not defined!");
		ExitProgram(BAD(1));
	}

	if ((tgetent(tcbuf, tv_stype)) != 1)
	{
		(void)lsprintf(err_str, "Unknown terminal type %s!", tv_stype);
		puts(err_str);
		ExitProgram(BAD(1));
	}

	/* Get screen size from system, or else from termcap.  */
	getscreensize(&term.t_ncol, &term.t_nrow);
 
	if ((term.t_nrow <= 1) && (term.t_nrow=(short)tgetnum("li")) == -1) {
		puts("Termcap entry incomplete (lines)");
		ExitProgram(BAD(1));
	}
	term.t_nrow -= 1;


	if ((term.t_ncol <= 1) &&(term.t_ncol=(short)tgetnum("co")) == -1){
		puts("Termcap entry incomplete (columns)");
		ExitProgram(BAD(1));
	}

	{ 	/* are we probably an xterm? 
			(anything starting or ending with...) */
		int l = strlen(tv_stype);
		i_am_xterm = (strncmp(tv_stype, "xterm", 5) == 0 || 
			(l >= 5 && strcmp(tv_stype + l - 5, "xterm") == 0));
	}

#ifdef SIGWINCH
	term.t_mcol = 200;
	term.t_mrow = 200;
#else
	term.t_mrow =  term.t_nrow;
	term.t_mcol =  term.t_ncol;
#endif
	p = tcapbuf;
	t = tgetstr("pc", &p);
	if(t)
		PC = *t;

	CL = tgetstr("cl", &p);
	CM = tgetstr("cm", &p);
	CE = tgetstr("ce", &p);
	UP = tgetstr("up", &p);
	SE = tgetstr("se", &p);
	SO = tgetstr("so", &p);
	TI = tgetstr("ti", &p);
	TE = tgetstr("te", &p);
	KS = tgetstr("ks", &p);
	KE = tgetstr("ke", &p);
	if (SO != NULL)
		revexist = TRUE;

	if(CL == NULL || CM == NULL || UP == NULL)
	{
		puts("Incomplete termcap entry\n");
		ExitProgram(BAD(1));
	}

	if (CE == NULL) 	/* will we be able to use clear to EOL? */
		eolexist = FALSE;
#if SCROLLCODE
	CS = tgetstr("cs", &p);
	SF = tgetstr("sf", &p);
	SR = tgetstr("sr", &p);
	dl = tgetstr("dl", &p);
	al = tgetstr("al", &p);
	DL = tgetstr("DL", &p);
	AL = tgetstr("AL", &p);
        
	if (!CS || !SR) { /* some xterm's termcap entry is missing entries */
		if (i_am_xterm) {
			if (!CS) CS = "\033[%i%d;%dr";
			if (!SR) SR = "\033[M";
		}
	}

	if (CS && SR) {
		if (SF == NULL) /* assume '\n' scrolls forward */
			SF = "\n";
		term.t_scroll = tcapscroll_reg;
	} else if ((DL && AL) || (dl && al)) {
		term.t_scroll = tcapscroll_delins;
	} else {
		term.t_scroll = NULL;
	}
#endif
#if OPT_FLASH
	vb = tgetstr("vb", &p);
#endif
	        
	if (p >= &tcapbuf[TCAPSLEN])
	{
		puts("Terminal description too big!\n");
		ExitProgram(BAD(1));
	}
	ttopen();
	if (TI)
		putnpad(TI, strlen(TI));
	if (KS)
		putpad(KS);
	already_open = TRUE;
}

void
tcapclose()
{
	if (TE)
		putnpad(TE, strlen(TE));
	if (KE)
		putpad(KE);
}

void
tcapkopen()
{
#if OPT_XTERM
	if (i_am_xterm && global_g_val(GMDXTERM_MOUSE))
		putpad(XTERM_ENABLE_TRACKING);
#endif
	(void)strcpy(sres, "NORMAL");
}

void
tcapkclose()
{
#if OPT_XTERM
	if (i_am_xterm && global_g_val(GMDXTERM_MOUSE))
		putpad(XTERM_DISABLE_TRACKING);
#endif
}

void
tcapmove(row, col)
register int row, col;
{
	putpad(tgoto(CM, col, row));
}

void
tcapeeol()
{
	putpad(CE);
}

void
tcapeeop()
{
	putpad(CL);
}

void
tcaprev(state)		/* change reverse video status */
int state;		/* FALSE = normal video, TRUE = reverse video */
{
	static int revstate = -1;
	if (state == revstate)
		return;
	revstate = state;
	if (state) {
		if (SO != NULL)
			putpad(SO);
	} else {
		if (SE != NULL)
			putpad(SE);
	}
}

/*ARGSUSED*/
int
tcapcres(res)	/* change screen resolution */
char *	res;
{
	return(TRUE);
}

#if SCROLLCODE

/* move howmany lines starting at from to to */
void
tcapscroll_reg(from,to,n)
int from, to, n;
{
	int i;
	if (to == from) return;
	if (to < from) {
		tcapscrollregion(to, from + n - 1);
		tcapmove(from + n - 1,0);
		for (i = from - to; i > 0; i--)
			putpad(SF);
	} else { /* from < to */
		tcapscrollregion(from, to + n - 1);
		tcapmove(from,0);
		for (i = to - from; i > 0; i--)
			putpad(SR);
	}
	tcapscrollregion(0, term.t_nrow);
}

/* 
PRETTIER_SCROLL is prettier but slower -- it scrolls 
		a line at a time instead of all at once.
*/

/* move howmany lines starting at from to to */
void
tcapscroll_delins(from,to,n)
int from, to, n;
{
	int i;
	if (to == from) return;
	if (DL && AL) {
		if (to < from) {
			tcapmove(to,0);
			putpad(tgoto(DL,0,from-to));
			tcapmove(to+n,0);
			putpad(tgoto(AL,0,from-to));
		} else {
			tcapmove(from+n,0);
			putpad(tgoto(DL,0,to-from));
			tcapmove(from,0);
			putpad(tgoto(AL,0,to-from));
		}
	} else { /* must be dl and al */
#if PRETTIER_SCROLL
		if (absol(from-to) > 1) {
			tcapscroll_delins(from, (from<to) ? to-1:to+1, n);
			if (from < to)
				from = to-1;
			else
				from = to+1;    
		}
#endif
		if (to < from) {
			tcapmove(to,0);
			for (i = from - to; i > 0; i--)
				putpad(dl);
			tcapmove(to+n,0);
			for (i = from - to; i > 0; i--)
				putpad(al);
		} else {
			tcapmove(from+n,0);
			for (i = to - from; i > 0; i--)
				putpad(dl);
			tcapmove(from,0);
			for (i = to - from; i > 0; i--)
				putpad(al);
		}
	}
}

/* cs is set up just like cm, so we use tgoto... */
void
tcapscrollregion(top,bot)
int top,bot;
{
	putpad(tgoto(CS, bot, top));
}

#endif

/* ARGSUSED */
void
spal(dummy)	/* change palette string */
char *dummy;
{
	/*	Does nothing here	*/
}

#if	COLOR
void
tcapfcol()	/* no colors here, ignore this */
{
}

void
tcapbcol()	/* no colors here, ignore this */
{
}
#endif

void
tcapbeep()
{
#if OPT_FLASH
	if (global_g_val(GMDFLASH)
	 && vb != NULL) {
		putpad(vb);
	} else
#endif
	ttputc(BEL);
}

void
putpad(str)
char	*str;
{
	tputs(str, 1, ttputc);
}

void
putnpad(str, n)
char	*str;
int n;
{
	tputs(str, n, ttputc);
}


#if	FLABEL
/* ARGSUSED */
int
fnclabel(f, n)		/* label a function key */
int f,n;	/* default flag, numeric argument [unused] */
{
	/* on machines with no function keys...don't bother */
	return(TRUE);
}
#endif

#if OPT_XTERM
/* Finish decoding a mouse-click in an xterm, after the ESC and '[' chars.
 *
 * There are 3 mutually-exclusive xterm mouse-modes (selected here by values of
 * OPT_XTERM):
 *	(1) X10-compatibility (not used here)
 *		Button-press events are received.
 *	(2) normal-tracking
 *		Button-press and button-release events are received.
 *		Button-events have modifiers (e.g., shift, control, meta).
 *	(3) hilite-tracking
 *		Button-press and text-location events are received.
 *		Button-events have modifiers (e.g., shift, control, meta).
 *		Dragging with the mouse produces highlighting.
 *		The text-locations are checked by xterm to ensure validity.
 *
 * NOTE:
 *	The hilite-tracking code is here for testing and (later) use.  Because
 *	we cannot guarantee that we always are decoding escape-sequences when
 *	reading from the terminal, there is the potential for the xterm hanging
 *	when a mouse-dragging operation is begun: it waits for us to specify
 *	the screen locations that limit the highlighting.
 *
 * 	While highlighting, the xterm accepts other characters, but the display
 *	does not appear to be refreshed until highlighting is ended. So (even
 *	if we always capture the beginning of highlighting) we cannot simply
 *	loop here waiting for the end of highlighting.
 *
 *	1993/aug/6 dickey@software.org
 */
int
xterm_button(c)
int	c;
{
	WINDOW	*wp;
	int	event;
	int	button;
	int	x;
	int	y;
	int	status;
#if OPT_XTERM >= 3
	int	save_row = ttrow;
	int	save_col = ttcol;
	int	firstrow, lastrow;
	int	startx, endx, mousex;
	int	starty, endy, mousey;
	MARK	save_dot;
	char	temp[NSTRING];
	static	char	*fmt = "\033[%d;%d;%d;%d;%dT";
#endif	/* OPT_XTERM >= 3 */

	if ((status = (i_am_xterm && global_g_val(GMDXTERM_MOUSE))) != 0) {
		beginDisplay;
		switch(c) {
		case 'M':	/* button-event */
			event	= TTgetc();
			x	= XtermPos();
			y	= XtermPos();

			button	= (event & 3) + 1;
			wp = row2window(y-1);
#if OPT_XTERM >= 3
			/* Tell the xterm how to highlight the selection.
			 * It won't do anything else until we do this.
			 */
			if (wp != 0) {
				firstrow = wp->w_toprow + 1;
				lastrow  = wp->w_toprow + wp->w_ntrows + 1;
			} else {		/* from message-line */
				firstrow = term.t_nrow + 1;
				lastrow  = term.t_nrow + 2;
			}
			if (y >= lastrow)	/* don't select modeline */
				y = lastrow - 1;
			(void)lsprintf(temp, fmt, 1, x, y, firstrow, lastrow);
			putpad(temp);
			TTflush();
#endif	/* OPT_XTERM >= 3 */
			/* Set the dot-location if button 1 was pressed in a
			 * window.
			 */
			if (wp != 0
			 && button == 1
			 && ttrow != term.t_nrow
			 && setcursor(y-1, x-1)) {
				showcpos(FALSE,1);
				(void)update(TRUE);
			} else if (button <= 3) {
#if OPT_XTERM >= 3
				/* abort the selection */
				(void)lsprintf(temp, fmt, 0, x, y, firstrow, lastrow);
				putpad(temp);
				TTflush();
#endif	/* OPT_XTERM >= 3 */
				status = ABORT;
			}
			break;
#if OPT_XTERM >= 3
		case 't':	/* reports valid text-location */
			x = XtermPos();
			y = XtermPos();

			setwmark(y-1, x-1);
			yankregion();

			movecursor(save_row, save_col);
			mlerase();
			(void)update(TRUE);
			break;
		case 'T':	/* reports invalid text-location */
			/*
			 * The starting-location returned is not the location
			 * at which the mouse was pressed.  Instead, it is the
			 * top-most location of the selection.  In turn, the
			 * ending-location is the bottom-most location of the
			 * selection.  The mouse-up location is not necessarily
			 * a pointer to valid text.
			 *
			 * This case handles multi-clicking events as well as
			 * selections whose start or end location was not
			 * pointing to text.
			 */
			save_dot = DOT;
			startx = XtermPos();	/* starting-location */
			starty = XtermPos();
			endx   = XtermPos();	/* ending-location */
			endy   = XtermPos();
			mousex = XtermPos();	/* location at mouse-up */
			mousey = XtermPos();

			setcursor(starty - 1, startx - 1);
			setwmark (endy   - 1, endx   - 1);
			if (MK.o != 0 && !is_at_end_of_line(MK))
				MK.o += 1;
			yankregion();

			DOT = save_dot;
			movecursor(save_row, save_col);
			mlerase();
			(void)update(TRUE);
			break;
#endif /* OPT_XTERM >= 3 */
		default:
			status = FALSE;
		}
		endofDisplay;
	}
	return status;
}
#endif	/* OPT_XTERM */

#endif	/* TERMCAP */
