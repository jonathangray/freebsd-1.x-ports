/*
 * The functions in this file handle redisplay. There are two halves, the
 * ones that update the virtual display screen, and the ones that make the
 * physical display screen the same as the virtual display screen. These
 * functions use hints that are left in the windows by the commands.
 *
 *
 * $Log: display.c,v $
 * Revision 1.1  1994/02/01 03:29:15  jkh
 * Initial revision
 *
 * Revision 1.102  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.101  1993/09/06  16:24:28  pgf
 * used-before-set warning cleanup
 *
 * Revision 1.100  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.99  1993/08/21  18:20:06  pgf
 * protect against window sizes less than 1
 *
 * Revision 1.98  1993/08/20  21:22:12  pgf
 * added AIX to the machines that need sys/ioctl.h, to pick up TIOCGWINSIZE
 *
 * Revision 1.97  1993/08/16  14:06:06  pgf
 * took out reference to variable 'req' in updateline() when REVSTA not
 * defined.
 *
 * Revision 1.96  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.95  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.94  1993/07/27  18:28:06  pgf
 * one more change from tom -- missed inequality in reframe()
 *
 * Revision 1.93  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.92  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.91  1993/07/07  12:59:30  pgf
 * don't put cursor at nu_width if the buffer is empty, since there won't be
 * a line number in that case.
 *
 * Revision 1.90  1993/07/06  16:39:04  pgf
 * integrated Tuan DANG's changes for the djgpp compiler under DOS
 *
 * Revision 1.89  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.88  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.87  1993/06/22  10:26:53  pgf
 * dbgwrite now loops for a ^G to be typed -- safer when typing fast than
 * a simple getchar()
 *
 * Revision 1.86  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.85  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.84  1993/05/24  15:25:41  pgf
 * tom's 3.47 changes, part b
 *
 * Revision 1.83  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.82  1993/05/05  12:25:41  pgf
 * osf1 and ultrix keep ioctl.h in sys
 *
 * Revision 1.81  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.80  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.79  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.78  1993/04/20  12:03:35  pgf
 * AIX needs sys/ioctl.h (actually it doesn't, since it's POSIX, but i'm
 * adding it here for completeness)
 *
 * Revision 1.77  1993/04/20  12:00:02  pgf
 * scroll sideways by half screens
 *
 * Revision 1.76  1993/04/09  16:50:17  pgf
 * fix off-by-one when doing auto-horizontal-scroll leftward, and
 * allow the cursor to sit on the last column if it's the last char on the
 * line
 *
 * Revision 1.75  1993/04/09  13:36:01  pgf
 * include ioctl.h if LINUX
 *
 * Revision 1.74  1993/04/08  11:16:21  pgf
 * changed offset for horiz scrolling from 1/3 to 1/4
 *
 * Revision 1.73  1993/04/08  11:09:27  pgf
 * implemented horizscroll mode
 *
 * Revision 1.72  1993/04/02  10:58:37  pgf
 * first of probably many ioctl.h fixups :-(
 *
 * Revision 1.71  1993/04/01  13:06:31  pgf
 * turbo C support (mostly prototypes for static)
 *
 * Revision 1.70  1993/03/29  11:18:59  pgf
 * now look at environment variables LINES and COLUMNS in getscreensize()
 * if the tty driver doesn't know the size
 *
 * Revision 1.69  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.68  1993/03/18  17:42:20  pgf
 * see 3.38 section of CHANGES
 *
 * Revision 1.67  1993/03/17  11:35:04  pgf
 * cleaned up confusion in alt-tabstop mode
 *
 * Revision 1.66  1993/03/17  10:07:21  pgf
 * extra includes/defs for correct operation on OSF (shouldn't termios.h
 * provide TIOCGWINSIZE, and shouldn't FION_READ work?)
 *
 * Revision 1.65  1993/03/17  09:57:59  pgf
 * fix for calling shorten_path() correctly
 *
 * Revision 1.64  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.63  1993/03/05  18:46:39  pgf
 * fix for tab cursor positioning in insert mode, and mode to control
 * positioning style
 *
 * Revision 1.62  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.61  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.60  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.59  1993/01/23  13:38:23  foxharp
 * tom's changes -- reverse video works better, fix for list mode,
 * dfoutfn now re-entrant, dfputli now does hex
 *
 * Revision 1.58  1993/01/16  10:26:13  foxharp
 * for_each_window and other new macros, and fixes for column 80 indicator
 *
 * Revision 1.57  1993/01/12  08:48:43  foxharp
 * tom dickey's changes to support "set number", i.e. line numbering
 *
 * Revision 1.56  1992/12/23  09:17:17  foxharp
 * ifdef of unused code
 *
 * Revision 1.55  1992/12/14  08:32:20  foxharp
 * fix for the sideways-shifted-but-deextended bug.  thanks to Tom Dickey.
 * also lint cleanup.
 *
 * Revision 1.54  1992/12/04  09:12:25  foxharp
 * deleted unused assigns
 *
 * Revision 1.53  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.52  1992/08/05  21:55:16  foxharp
 * handle files with DOS drive designators properly on mode line
 *
 * Revision 1.51  1992/07/24  07:49:38  foxharp
 * shorten_name changes
 *
 * Revision 1.50  1992/07/22  19:25:57  foxharp
 * handle non-printables correctly -- how did I get involved in this, anyway!
 *
 * Revision 1.49  1992/07/22  09:18:16  foxharp
 * got it.  sheesh.
 *
 * Revision 1.48  1992/07/22  00:50:43  foxharp
 * interim -- still dumping core in vtset
 *
 * Revision 1.47  1992/07/21  09:08:30  foxharp
 * now pass lp to vtset()
 *
 * Revision 1.46  1992/07/21  08:57:53  foxharp
 * pushed list mode choice into vtset()
 *
 * Revision 1.45  1992/07/20  22:44:44  foxharp
 * performance improvements -- fewer vtputc's
 *
 * Revision 1.44  1992/07/18  13:13:56  foxharp
 * put all path-shortening in one place (shorten_path()), and took out
 * some old code now unnecessary
 *
 * Revision 1.43  1992/07/13  20:03:54  foxharp
 * the "terse" variable is now a boolean mode
 *
 * Revision 1.42  1992/07/13  19:37:00  foxharp
 * trim leading `pwd` from filenames in modeline, now that filenames are
 * usually absolute
 *
 * Revision 1.41  1992/07/01  16:59:46  foxharp
 * scwrite() arg changes, and some fore/background color cleanup
 *
 * Revision 1.40  1992/06/26  22:21:05  foxharp
 * small almost cosmetic changes to mlsav'ing
 *
 * Revision 1.39  1992/05/29  08:36:06  foxharp
 * moved the SVR3_PTEM ifdef so it works with POSIX too
 *
 * Revision 1.38  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.37  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.36  1992/04/27  19:50:43  pgf
 * fixed ifdefs on termio inclusion
 *
 * Revision 1.35  1992/04/14  08:50:01  pgf
 * fix SIGWINCH handling for X11 case
 *
 * Revision 1.34  1992/03/25  19:13:17  pgf
 * BSD portability changes
 *
 * Revision 1.33  1992/03/19  23:31:54  pgf
 * mlputc now converts tabs to single spaces, since we don't really
 * know how wide they should be anyway
 *
 * Revision 1.32  1992/03/19  23:13:04  pgf
 * SIGT for signals
 *
 * Revision 1.31  1992/03/07  10:21:29  pgf
 * suppress prompts (mlprompt()) if execing buff or file
 *
 * Revision 1.30  1992/03/05  09:17:21  pgf
 * added support for new "terse" variable, to control unnecessary messages
 *
 * Revision 1.29  1992/03/01  18:38:40  pgf
 * compilation error #if COLOR
 *
 * Revision 1.28  1992/02/17  08:57:36  pgf
 * added "showmode" support
 *
 * Revision 1.27  1992/01/22  20:26:00  pgf
 * fixed ifdef conflict: SVR3_PTEM
 *
 * Revision 1.26  1992/01/05  00:05:24  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.25  1991/11/13  20:09:27  pgf
 * X11 changes, from dave lemke
 *
 * Revision 1.24  1991/11/08  13:14:40  pgf
 * more lint
 *
 * Revision 1.23  1991/11/07  03:58:31  pgf
 * lint cleanup
 *
 * Revision 1.22  1991/11/03  17:35:06  pgf
 * don't access unset vt column 80 if screen isn't that wide
 *
 * Revision 1.21  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.20  1991/10/28  14:22:46  pgf
 * no more TABVAL macro, curtabstopval renamed curtabval
 *
 * Revision 1.19  1991/10/26  00:14:56  pgf
 * put termio.h back in, and switch to SVR3 around ptem.h
 *
 * Revision 1.18  1991/10/23  12:05:37  pgf
 * we don't need termio.h
 *
 * Revision 1.17  1991/10/08  01:28:43  pgf
 * dbgwrite now uses raw'est i/o
 *
 * Revision 1.16  1991/09/26  13:16:19  pgf
 * LIST mode and w_sideways are now both window values
 *
 * Revision 1.15  1991/09/11  02:30:22  pgf
 * use get_recorded_char, now that we have it
 *
 * Revision 1.14  1991/08/16  11:10:46  pgf
 * added insert mode indicator to modeline: I for insert, O for overwrite, and
 * R for replace-char
 *
 * Revision 1.13  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.12
 * date: 1991/08/06 15:12:29;
 * global/local values, and printf/list changes
 * 
 * revision 1.11
 * date: 1991/06/28 10:53:18;
 * make update quit early if !curbp
 * 
 * revision 1.10
 * date: 1991/06/25 19:52:13;
 * massive data structure restructure
 * 
 * revision 1.9
 * date: 1991/06/16 17:34:57;
 * switched to modulo tab calculations
 * 
 * revision 1.8
 * date: 1991/05/31 10:35:55;
 * added new "dbgwrite()" routine, for debugging
 * 
 * revision 1.7
 * date: 1991/04/22 09:04:14;
 * more ODT support
 * 
 * revision 1.6
 * date: 1991/03/19 12:15:08;
 * fix flag checking
 * 
 * revision 1.5
 * date: 1991/02/21 10:04:01;
 * horizontal scrolling is most of the way there.  only thing
 * left to do is to extend lines where the cursor is beyond the
 * left edge of a scrolled screen
 * 
 * revision 1.4
 * date: 1990/10/05 14:19:31;
 * fixed typo in ODT ifdef.
 * propagated mode line changes to all windows holding a given buffer
 * 
 * revision 1.3
 * date: 1990/10/01 11:04:42;
 * atatus return in newscreensize
 * 
 * revision 1.2
 * date: 1990/09/25 11:38:06;
 * took out old ifdef BEFORE code
 * 
 * revision 1.1
 * date: 1990/09/21 10:24:58;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include        "edef.h"

#if UNIX
# if POSIX
#  include <termios.h>
# else
#  if USG
#   include <termio.h>
#  else
#   if BERK
#    if APOLLO || AIX || OSF1 || ULTRIX || __NetBSD__ || __386BSD__
#     include <sys/ioctl.h>
#   else
#     include <ioctl.h>
#    endif
#   endif
#  endif
# endif
# if SVR3_PTEM
#  include <sys/types.h>
#  include <sys/stream.h>
#  include <sys/ptem.h>
# endif
#endif

#if OSF1 || AIX || LINUX
# include <sys/ioctl.h>
#endif

#define	NU_WIDTH 8

#define	MRK_EMPTY        '~'
#define	MRK_EXTEND_LEFT  '<'
#define	MRK_EXTEND_RIGHT '>'

VIDEO   **vscreen;                      /* Virtual screen. */
#if	MEMMAP
#define PSCREEN vscreen
#else
#define PSCREEN pscreen
VIDEO   **pscreen;                      /* Physical screen. */
#endif

#if IBMPC
#define PScreen(n) scread((VIDEO *)0,n)
#else
#define	PScreen(n) pscreen[n]
#endif

#if SCROLLCODE && (IBMPC || !MEMMAP)
#define CAN_SCROLL 1
#else
#define CAN_SCROLL 0
#endif

static	int	displayed;

/* for window size changes */
int chg_width, chg_height;

/******************************************************************************/

typedef	void	(*OutFunc) P(( int ));

static	OutFunc	dfoutfn;

static	char *	right_num P(( char *, int, long ));
static	int	dfputs  P(( OutFunc, char * ));
static	int	dfputsn P(( OutFunc, char *, int ));
static	int	dfputi  P(( OutFunc, int,  int ));
static	int	dfputli P(( OutFunc, long, int ));
static	int	dfputf  P(( OutFunc, int ));
static	void	dofmt P(( char *, va_list * ));
static	void	erase_remaining_msg P(( int ));
static	void	PutMode P(( char * ));

static	void	l_to_vline P(( WINDOW *, LINEPTR, int ));
static	int	updpos P(( int *, int * ));
static	void	upddex P(( void ));
static	void	updgar P(( void ));
static	void	updone P(( WINDOW * ));
static	void	updall P(( WINDOW * ));
static	void	updupd P(( int ));
static	int	updext_past P(( int, int ));
static	int	updext_before P(( int ));
static	void	updateline P(( int, int, int ));
static	int	endofline P(( char *, int ));
static	void	modeline P(( WINDOW * ));
#if	OPT_UPBUFF
static	void	recompute_buffer P(( BUFFER * ));
#endif
static	int	texttest P(( int, int ));
#if CAN_SCROLL
static	int	scrolls P(( int ));
#endif

/*--------------------------------------------------------------------------*/

/*
 * Format a number, right-justified, returning a pointer to the formatted
 * buffer.
 */
static char *
right_num (buffer, len, value)
	char	*buffer;
	int	len;
	long	value;
{
	char	temp[NSTRING];
	register char	*p = lsprintf(temp, "%D", value);
	register char	*q = buffer + len;

	*q = EOS;
	while (q != buffer)
		*(--q) = (p != temp) ? *(--p) : ' ';
	return buffer;
}

/*
 * Do format a string.
 */
static
int	dfputs(outfunc, s)
	OutFunc outfunc;
	char *s;
{
	register int c;
	register int l = 0;

	while ((c = *s++) != 0) {
	        (*outfunc)(c);
		l++;
	}
	return l;
}

/* as above, but takes a count for s's length */
static
int	dfputsn(outfunc,s,n)
	OutFunc outfunc;
	char *s;
	int n;
{
	register int c;
	register int l = 0;
	while ((c = *s++) != 0 && n-- != 0) {
		(*outfunc)(c);
		l++;
	}
	return l;
}

/*
 * Do format an integer, in the specified radix.
 */
static
int	dfputi(outfunc,i, r)
	OutFunc outfunc;
	int i,r;
{
	register int q;
	static char hexdigits[] = "0123456789ABCDEF";

	if (i < 0) {
		(*outfunc)('-');
		return dfputi(outfunc, -i, r) + 1;
	}

	q = (i >= r) ? dfputi(outfunc, i/r, r) : 0;

	(*outfunc)(hexdigits[i%r]);
	return q + 1; /* number of digits printed */
}

/*
 * do the same except as a long integer.
 */
static
int	dfputli(outfunc,l, r)
	OutFunc outfunc;
	long l;
	int r;
{
	register int q;

	if (l < 0) {
		(*outfunc)('-');
		return dfputli(outfunc, -l, r) + 1;
	}

	q = (l >= r) ? dfputli(outfunc, l/r, r) : 0;

	return q + dfputi(outfunc, (int)(l%r), r);
}

/*
 *	Do format a scaled integer with two decimal places
 */
static
int	dfputf(outfunc,s)
	OutFunc outfunc;
	int s;	/* scaled integer to output */
{
	register int i;	/* integer portion of number */
	register int f;	/* fractional portion of number */

	/* break it up */
	i = s / 100;
	f = s % 100;

	/* send out the integer portion */
	i = dfputi(outfunc, i, 10);
	(*outfunc)('.');
	(*outfunc)((f / 10) + '0');
	(*outfunc)((f % 10) + '0');
	return i + 3;
}

/*
 * Generic string formatter.  Takes printf-like args, and calls
 * the global function (*dfoutfn)(c) for each c
 */
static
void
#if	ANSI_VARARGS
dofmt( char *fmt, va_list *app)
#else
dofmt(app)
va_list *app;
#endif
{
#if	!ANSI_VARARGS
	register char *fmt = va_arg(*app, char *);
#endif
	register int c;		/* current char in format string */
	register int wid;
	register int n;
	register int nchars = 0;
	int islong;
	OutFunc outfunc = dfoutfn;  /* local copy, for recursion */

	while ((c = *fmt++) != 0 ) {
		if (c != '%') {
			(*outfunc)(c);
			nchars++;
			continue;
		}
		c = *fmt++;
		wid = 0;
		islong = FALSE;
		if (c == '*') {
			wid = va_arg(*app,int);
			c = *fmt++;
		} else while (isdigit(c)) {
			wid = (wid * 10) + c - '0';
			c = *fmt++;
		}
		if (c == 'l') {
			islong = TRUE;
			c = *fmt++;
		}
		switch (c) {
			case EOS:
				n = 0;
				break;
			case 'c':
				(*outfunc)(va_arg(*app,int));
				n = 1;
				break;

			case 'd':
				if (!islong) {
					n = dfputi(outfunc, va_arg(*app,int), 10);
					break;
				}
				/* fall through */
			case 'D':
				n = dfputli(outfunc, va_arg(*app,long), 10);
				break;

			case 'o':
				n = dfputi(outfunc, va_arg(*app,int), 8);
				break;

			case 'x':
				if (!islong) {
					n = dfputi(outfunc, va_arg(*app,int), 16);
					break;
				}
				/* fall through */
			case 'X':
				n = dfputli(outfunc, va_arg(*app,long), 16);
				break;

			case 's':
				n = dfputs(outfunc, va_arg(*app,char *));
				break;

			case 'S': /* use wid as max width */
				n = dfputsn(outfunc, va_arg(*app,char *),wid);
				break;

			case 'f':
				n = dfputf(outfunc, va_arg(*app,int));
				break;

			case 'P': /* output padding -- pads total output to
					"wid" chars, using c as the pad char */
				wid -= nchars;
				/* fall through */

			case 'p': /* field padding -- puts out "wid"
					copies of c */
				n = 0;
				c = va_arg(*app,int);
				while (n < wid) {
					(*outfunc)(c);
					n++;
				}
				break;


			default:
				(*outfunc)(c);
				n = 1;
		}
		wid -= n;
		nchars += n;
		while (wid-- > 0) {
			(*outfunc)(' ');
			nchars++;
		}
	}

}

/******************************************************************************/

/*
 * Line-number mode
 */
int
nu_width(wp)
WINDOW *wp;
{
	return w_val(wp,WMDNUMBER) ? NU_WIDTH : 0;
}

int
col_limit(wp)
WINDOW *wp;
{
#ifdef WMDLINEWRAP
	if (w_val(wp,WMDLINEWRAP))
		return curcol + 1;	/* effectively unlimited */
#endif
	return term.t_ncol - 1 - nu_width(wp);
}

/*
 * Initialize the data structures used by the display code. The edge vectors
 * used to access the screens are set up. The operating system's terminal I/O
 * channel is set up. All the other things get initialized at compile time.
 * The original window has "WFCHG" set, so that it will get completely
 * redrawn on the first call to "update".
 */
void
vtinit()
{
    register int i;
    register VIDEO *vp;

    TTopen();		/* open the screen */
    TTkopen();		/* open the keyboard */
    TTrev(FALSE);
    vscreen = typeallocn(VIDEO *,term.t_mrow);

    if (vscreen == NULL)
	ExitProgram(BAD(1));

#if	! MEMMAP
    pscreen = typeallocn(VIDEO *,term.t_mrow);

    if (pscreen == NULL)
	ExitProgram(BAD(1));
#endif

    for (i = 0; i < term.t_mrow; ++i) {
    	/* struct VIDEO already has 4 of the bytes */
        vp = typeallocplus(VIDEO, term.t_mcol - 4);

        if (vp == NULL)
	    ExitProgram(BAD(1));


	/* unnecessary */
	/* (void)memset(vp, ' ', sizeof(struct VIDEO) + term.t_mcol - 4); */
	vp->v_flag = 0;
#if	COLOR
	vp->v_rfcolor = gfcolor;
	vp->v_rbcolor = gbcolor;
#endif
        vscreen[i] = vp;
#if	! MEMMAP
    	/* struct VIDEO already has 4 of the bytes */
        vp = typeallocplus(VIDEO, term.t_mcol - 4);

        if (vp == NULL)
	    ExitProgram(BAD(1));

	/* unnecessary */
	/* (void)memset(vp, 0, sizeof(struct VIDEO) + term.t_mcol - 4); */

	vp->v_flag = 0;
        pscreen[i] = vp;
#endif
        }
	if_OPT_WORKING(imworking(0))
}

/*
 * Clean up the virtual terminal system, in anticipation for a return to the
 * operating system. Move down to the last line and advance, to make room
 * for the system prompt. Shut down the channel to the
 * terminal.
 */
void
vttidy(f)
int f;
{
	ttclean(f);	/* does it all now */
}

/*
 * Set the virtual cursor to the specified row and column on the virtual
 * screen. There is no checking for nonsense values.
 */
void
vtmove(row, col)
int row,col;
{
    vtrow = row;
    vtcol = col;
}

/* Write a character to the virtual screen. The virtual row and
   column are updated. If we are not yet on left edge, don't print
   it yet. If the line is too long put a ">" in the last column.
   This routine only puts printing characters into the virtual
   terminal buffers. Only column overflow is checked.
*/

void
vtputc(c)
int c;
{
	register VIDEO *vp;	/* ptr to line being updated */

	vp = vscreen[vtrow];

	if (isprint(c) && vtcol >= 0 && vtcol < term.t_ncol) {
		vp->v_text[vtcol] = c;
		++vtcol;
		return;
	}
	if (c == '\t') {
		do {
			vtputc(' ');
		} while (((vtcol + taboff)%curtabval) != 0);
	} else if (c == '\n') {
		return;
	} else if (vtcol >= term.t_ncol) {
		++vtcol;
		vp->v_text[term.t_ncol - 1] = MRK_EXTEND_RIGHT;
	} else if (isprint(c)) {
		++vtcol;
	} else {
		vtputc('^');
		vtputc(toalpha(c));
	}
}

/* as above, but tabs and newlines are made visible */
void
vtlistc(c)
int c;
{
	register VIDEO *vp;	/* ptr to line being updated */

	vp = vscreen[vtrow];

	if (vtcol >= term.t_ncol) {
		++vtcol;
		vp->v_text[term.t_ncol - 1] = MRK_EXTEND_RIGHT;
	} else if (isprint(c)) {
		if (vtcol >= 0)
			vp->v_text[vtcol] = c;
		++vtcol;
	} else {
		vtputc('^');
		vtputc(toalpha(c));
	}
}

int
vtgetc(col)
int col;
{
	return vscreen[vtrow]->v_text[col];
}

void
vtputsn(s,n)
char *s;
int n;
{
	int c;
	while (n-- && (c = *s++) != EOS)
		vtputc(c);
}


void
vtset(lp,wp)
LINEPTR lp;
WINDOW *wp;
{
	register char *from;
	register int n = lLength(lp);
	int	skip = -vtcol,
		list = w_val(wp,WMDLIST);

	if (w_val(wp,WMDNUMBER)) {
		register int j, k, jk;
		L_NUM	line = line_no(wp->w_bufp, lp);
		int	fill = ' ';
		char	temp[NU_WIDTH+2];

		vtcol = 0;	/* make sure we always see line numbers */
		vtputsn(right_num(temp, NU_WIDTH-2, (long)line), NU_WIDTH-2);
		vtputsn("  ", 2);
		taboff = skip - vtcol;

		/* account for leading fill; this repeats logic in vtputc so
		 * I don't have to introduce a global variable... */
		from = l_ref(lp)->l_text;
		for (j = k = jk = 0; (j < n) && (k < skip); j++) {
			register int	c = from[j];
			if (list && !isprint(c)) {
				k += 2;
				fill = toalpha(c);
			} else {
				if (c == '\t')
					k += (curtabval - (k % curtabval));
				else if (isprint(c))
					k++;
				fill = ' ';
			}
			jk = j+1;
		}
		while (k-- > skip)
			vtputc(fill);
		if ((skip = jk) < 0)
			skip = 0;
		n -= skip;
	} else
		skip = 0;

#if OPT_B_LIMITS
	taboff -= w_left_margin(wp);
#endif
	from = l_ref(lp)->l_text + skip;
	while ((vtcol <= term.t_ncol) && (n > 0)) {
		if (list)
			vtlistc(*from++);
		else
			vtputc(*from++);
#ifdef WMDLINEWRAP
		if (w_val(wp,WMDLINEWRAP)
		 && (n > 0)
		 && (vtcol == term.t_ncol)) {
			if (vtrow < term.t_nrow) {
				if (vtrow+1 >= wp->w_toprow + wp->w_ntrows)
					break;
				vtcol = 0;
				vtrow++;
				vscreen[vtrow]->v_flag |= VFCHG;
			}
		}
#endif
		n--;
	}
	if (list && (n >= 0))
		vtlistc('\n');
}

/* VARARGS1 */
void
#if	ANSI_VARARGS
vtprintf( char *fmt, ...)
#else
vtprintf(va_alist)
va_dcl
#endif
{

	va_list ap;
#if	ANSI_VARARGS
	va_start(ap,fmt);
#else
	va_start(ap);
#endif

	dfoutfn = vtputc;

#if	ANSI_VARARGS
	dofmt(fmt,&ap);
#else
	dofmt(&ap);
#endif
	va_end(ap);

}

/*
 * Erase from the end of the software cursor to the end of the line on which
 * the software cursor is located.
 */
void
vteeol()
{
	if (vtcol < term.t_ncol) {
		(void)memset(&vscreen[vtrow]->v_text[vtcol],
			' ', term.t_ncol-vtcol);
		vtcol = term.t_ncol;
	}
}

/* upscreen:	user routine to force a screen update
		always finishes complete update		*/
#if !SMALLER
/* ARGSUSED */
int
upscreen(f, n)
int f,n;
{
	return update(TRUE);
}
#endif

static	int	scrflags;

/*
 * Make sure that the display is right. This is a three part process. First,
 * scan through all of the windows looking for dirty ones. Check the framing,
 * and refresh the screen. Second, make sure that "currow" and "curcol" are
 * correct for the current window. Third, make the virtual and physical
 * screens the same.
 */
int
update(force)
int force;	/* force update past type ahead? */
{
	register WINDOW *wp;
	int screenrow, screencol;

	if (!curbp) /* not initialized */
		return FALSE;
#if	TYPEAH
	if (force == FALSE && typahead())
		return SORTOFTRUE;
#endif
#if	VISMAC == 0
	if (force == FALSE && kbd_replaying(TRUE) && (get_recorded_char(FALSE) != -1))
		return SORTOFTRUE;
#endif

	beginDisplay;

	/* first, propagate mode line changes to all instances of
		a buffer displayed in more than one window */
	for_each_window(wp) {
		if (wp->w_flag & WFMODE) {
			if (wp->w_bufp->b_nwnd > 1) {
				/* make sure all previous windows have this */
				register WINDOW *owp;
				for_each_window(owp)
					if (owp->w_bufp == wp->w_bufp)
						owp->w_flag |= WFMODE;
			}
		}
	}

	/* look for scratch-buffers that should be recomputed.  */
#if	OPT_UPBUFF
	for_each_window(wp)
		if (b_is_obsolete(wp->w_bufp))
			recompute_buffer(wp->w_bufp);
#endif

	/* look for windows that need the ruler updated */
#ifdef WMDRULER
	for_each_window(wp) {
		if (w_val(wp,WMDRULER)) {
			WINDOW	*save = curwp;
			int	line, col;

			curwp = wp;
			col   = getccol(FALSE) + 1;
			curwp = save;
			line  = line_no(wp->w_bufp, wp->w_dot.l);

			if (line != wp->w_ruler_line
			 || col  != wp->w_ruler_col) {
				wp->w_ruler_line = line;
				wp->w_ruler_col  = col;
				wp->w_flag |= WFMODE;
			}
		} else if (wp->w_flag & WFSTAT) {
			wp->w_flag |= WFMODE;
		}
		wp->w_flag &= ~WFSTAT;
	}
#endif

 restartupdate:

	/* update any windows that need refreshing */
	for_each_window(wp) {
		if (wp->w_flag) {
			curtabval = tabstop_val(wp->w_bufp);
			/* if the window has changed, service it */
			reframe(wp);	/* check the framing */
			if (wp->w_flag & (WFKILLS|WFINS)) {
				scrflags |= (wp->w_flag & (WFINS|WFKILLS));
				wp->w_flag &= ~(WFKILLS|WFINS);
			}
			if ((wp->w_flag & ~(WFMODE)) == WFEDIT)
				updone(wp);	/* update EDITed line */
			else if (wp->w_flag & ~(WFMOVE))
				updall(wp);	/* update all lines */
			if (scrflags || (wp->w_flag & (WFMODE|WFCOLR)))
				modeline(wp);	/* update modeline */
			wp->w_flag = 0;
			wp->w_force = 0;
		}
	}
	curtabval = tabstop_val(curbp);

	/* recalc the current hardware cursor location */
	if (updpos(&screenrow, &screencol))
		/* if true, full horizontal scroll happened */
		goto restartupdate;

	/* check for lines to de-extend */
	upddex();

	/* if screen is garbage, re-plot it */
	if (sgarbf)
		updgar();

	/* update the virtual screen to the physical screen */
	updupd(force);

	/* update the cursor and flush the buffers */
	movecursor(screenrow, screencol);

	TTflush();
	endofDisplay;
	displayed  = TRUE;

	while (chg_width || chg_height)
		newscreensize(chg_height,chg_width);
	return(TRUE);
}

/*	reframe:	check to see if the cursor is on in the window
			and re-frame it if needed or wanted		*/
void
reframe(wp)
WINDOW *wp;
{
	fast_ptr LINEPTR dlp;
	fast_ptr LINEPTR lp;
	register int i = 0;
	register int rows;
	int	founddot = FALSE;	/* set to true iff we find dot */

	/* if not a requested reframe, check for a needed one */
	if ((wp->w_flag & WFFORCE) == 0) {
		/* initial update in main.c may not set these first... */
		if (l_ref(wp->w_dot.l) == (LINE *)0) {
			wp->w_dot.l = lFORW(win_head(wp));
			wp->w_dot.o = 0;
		}
		if (l_ref(wp->w_line.l) == (LINE *)0) {
			wp->w_line.l = wp->w_dot.l;
			wp->w_line.o = 0;
		}
#if CAN_SCROLL
		/* loop from one line above the window to one line after */
		lp = lBACK(wp->w_line.l);
		i  = -line_height(wp,lp);
#else
		/* loop through the window */
		lp = wp->w_line.l;
		i  = 0;
#endif
		for (;;) {
			/* if the line is in the window, no reframe */
			if (same_ptr(lp, wp->w_dot.l)) {
				founddot = TRUE;
#if CAN_SCROLL
				/* if not _quite_ in, we'll reframe gently */
				if ( i < 0 || i >= wp->w_ntrows) {
					/* if the terminal can't help, then
						we're simply outside */
					if (term.t_scroll == NULL)
						i = wp->w_force;
					break;
				}
#endif
#ifdef WMDLINEWRAP
				if (w_val(wp,WMDLINEWRAP)
				 && i > 0
				 && i + line_height(wp,lp) > wp->w_ntrows) {
					i = wp->w_ntrows;
					break;
				}
#endif
				return;
			}

			/* if we are at the end of the file, reframe */
			if (i >= 0 && same_ptr(lp, win_head(wp)))
				break;

			/* on to the next line */
			if (i >= wp->w_ntrows) {
				i = 0;	/* dot-not-found */
				break;
			}
			i += line_height(wp,lp);
			lp = lFORW(lp);
		}
	}

#if CAN_SCROLL
	if (i < 0) {	/* we're just above the window */
		i = 1;	/* put dot at first line */
		scrflags |= WFINS;
	} else if (founddot && (i >= wp->w_ntrows)) {
		/* we're just below the window */
		i = -1;	/* put dot at last line */
		scrflags |= WFKILLS;
	} else /* put dot where requested */
#endif
		i = wp->w_force;  /* (is 0, unless reposition() was called) */

	wp->w_flag |= WFMODE;

	/* w_force specifies which line of the window dot should end up on */
	/* 	positive --> lines from the top				*/
	/* 	negative --> lines from the bottom			*/
	/* 	zero --> middle of window				*/

	lp = wp->w_dot.l;

#ifdef WMDLINEWRAP
	/*
	 * Center dot in middle of screen with line-wrapping
	 */
	if (i == 0 && w_val(wp,WMDLINEWRAP)) {
		rows = (wp->w_ntrows - line_height(wp,lp) + 2) / 2;
		while (rows > 0) {
			dlp = lBACK(lp);
			if (same_ptr(dlp, win_head(wp)))
				break;
			if ((rows -= line_height(wp, dlp)) < 0)
				break;
			lp = dlp;
		}
	} else
#endif
	{
		rows = (i != 0)
			? wp->w_ntrows
			: (wp->w_ntrows+1) / 2;
		while (rows > 0) {
			if ((i > 0)
			 && (--i <= 0))
				break;
			dlp = lBACK(lp);
			if (same_ptr(dlp, win_head(wp)))
				break;
			if ((rows -= line_height(wp, lp)) < 0)
				break;
			lp = dlp;
		}
		if (rows < line_height(wp, lp))
			while (i++ < 0) {
				dlp = lFORW(lp);
				if (!same_ptr(dlp, win_head(wp)))
					lp = dlp;
			}
	}

	/* and reset the current line-at-top-of-window */
	if (!same_ptr(lp, win_head(wp))) { /* mouse-click could be past end */
		wp->w_line.l = lp;
		wp->w_flag |= WFHARD;
		wp->w_flag &= ~WFFORCE;
	}
}

/*	updone:	update the current line	to the virtual screen		*/

static void
updone(wp)
WINDOW *wp;	/* window to update current line in */
{
	fast_ptr LINEPTR lp;	/* line to update */
	register int sline;	/* physical screen line to update */

	/* search down the line we want */
	lp = wp->w_line.l;
	sline = wp->w_toprow;
	while (!same_ptr(lp, wp->w_dot.l)) {
		sline += line_height(wp,lp);
		lp = lFORW(lp);
	}

	l_to_vline(wp,lp,sline);
	vteeol();
}

/*	updall:	update all the lines in a window on the virtual screen */

static void
updall(wp)
WINDOW *wp;	/* window to update lines in */
{
	fast_ptr LINEPTR lp;	/* line to update */
	register int sline;	/* physical screen line to update */

	/* search down the lines, updating them */
	lp = wp->w_line.l;
	sline = wp->w_toprow;
	while (sline < wp->w_toprow + wp->w_ntrows) {
		l_to_vline(wp,lp,sline);
		vteeol();
		sline += line_height(wp,lp);
		if (!same_ptr(lp, wp->w_bufp->b_line.l))
			lp = lFORW(lp);
	}
}

/* line to virtual screen line */
static void
l_to_vline(wp,lp,sline)
WINDOW *wp;	/* window to update lines in */
LINEPTR lp;
int sline;
{
	C_NUM	left;

	/*
	 * Mark the screen lines changed, resetting the requests for reverse
	 * video.  Set the global 'taboff' to the amount of horizontal
	 * scrolling.
	 */
#ifdef WMDLINEWRAP
	if (w_val(wp,WMDLINEWRAP)) {
		register int	n = sline + line_height(wp, lp);
		while (n > sline)
			if (--n < wp->w_toprow + wp->w_ntrows) {
				vscreen[n]->v_flag |= VFCHG;
				vscreen[n]->v_flag &= ~VFREQ;
			}
		taboff = 0;
	} else
#endif
	{
		vscreen[sline]->v_flag |= VFCHG;
		vscreen[sline]->v_flag &= ~VFREQ;
		if (w_val(wp,WVAL_SIDEWAYS))
			taboff = w_val(wp,WVAL_SIDEWAYS);
	}
	left = taboff;

	if (!same_ptr(lp, wp->w_bufp->b_line.l)) {
		vtmove(sline, -left);
		vtset(lp, wp);
		if (left) {
			register int	zero = nu_width(wp);
			vscreen[sline]->v_text[zero] = MRK_EXTEND_LEFT;
			if (vtcol <= zero) vtcol = zero+1;
		}
	} else {
		vtmove(sline, 0);
		vtputc(MRK_EMPTY);
	}
	taboff = 0;
#if	COLOR
	vscreen[sline]->v_rfcolor = w_val(wp,WVAL_FCOLOR);
	vscreen[sline]->v_rbcolor = w_val(wp,WVAL_BCOLOR);
#endif
}

/*	updpos:	update the position of the hardware cursor and handle extended
		lines. This is the only update for simple moves.
		returns the screen column for the cursor, and
		a boolean indicating if full sideways scroll was necessary */
static int
updpos(screenrowp, screencolp)
int *screenrowp;
int *screencolp;
{
	fast_ptr LINEPTR lp;
	register int c;
	register int i;
	register int col, excess;
	register int collimit;
	int moved = FALSE;
	int nuadj = is_empty_buf(curwp->w_bufp) ? 0 : nu_width(curwp);

	/* find the current row */
	lp = curwp->w_line.l;
	currow = curwp->w_toprow;
	while (!same_ptr(lp, DOT.l)) {
		currow += line_height(curwp,lp);
		lp = lFORW(lp);
		if (same_ptr(lp, curwp->w_line.l)
		 || currow > (curwp->w_toprow + curwp->w_ntrows)) {
			mlforce("BUG:  lost dot updpos().  setting at top");
			lp = curwp->w_line.l = DOT.l = lFORW(curbp->b_line.l);
			currow = curwp->w_toprow;
		}
	}

	/* find the current column */
	col = 0;
	i = w_left_margin(curwp);
	while (i < DOT.o || (!global_g_val(GMDALTTABPOS) && !insertmode &&
				i <= DOT.o && i < lLength(lp))) {
		c = lGetc(lp, i++);
		if (c == '\t' && !w_val(curwp,WMDLIST)) {
			do {
				col++;
			} while ((col%curtabval) != 0);
		} else {
			if (!isprint(c))
				++col;
			++col;
		}

	}
	col += w_left_margin(curwp);
	if (!global_g_val(GMDALTTABPOS) && !insertmode &&
			col != 0 && DOT.o < lLength(lp))
		col--;

#ifdef WMDLINEWRAP
	if (w_val(curwp,WMDLINEWRAP)) {
		curcol = col;
		collimit = term.t_ncol - nuadj;
		*screenrowp = currow;
		if (col >= collimit) {
			col -= collimit;
			*screenrowp += 1;
			if (col >= term.t_ncol)
				*screenrowp += (col / term.t_ncol);
			*screencolp = col % term.t_ncol;
		} else {
			*screencolp = col + nuadj;
		}
		/* kludge to keep the cursor within the window */
		i = curwp->w_toprow + curwp->w_ntrows - 1;
		if (*screenrowp > i) {
			*screenrowp = i;
			*screencolp = term.t_ncol - 1;
		}
		return FALSE;
	} else
#endif
	 *screenrowp = currow;

	/* ...adjust to offset from shift-margin */
	curcol = col - w_val(curwp,WVAL_SIDEWAYS);

	/* if extended, flag so and update the virtual line image */
	collimit = col_limit(curwp);
	excess = curcol - collimit;
	if ((excess > 0) || (excess == 0 &&
			(DOT.o < lLength(DOT.l) - 1 ))) {
		if (w_val(curwp,WMDHORSCROLL)) {
			(void)mvrightwind(TRUE, excess + collimit/2 );
			moved = TRUE;
		} else {
			*screencolp = updext_past(col, excess);
		}
	} else if (w_val(curwp,WVAL_SIDEWAYS) && (curcol < 1)) {
		if (w_val(curwp,WMDHORSCROLL)) {
			(void)mvleftwind(TRUE, -curcol + collimit/2 + 1);
			moved = TRUE;
		} else {
			*screencolp = updext_before(col);
		}
	} else {
		if (vscreen[currow]->v_flag & VFEXT) {
			l_to_vline(curwp,lp,currow);
			vteeol();
			/* this line no longer is extended */
			vscreen[currow]->v_flag &= ~VFEXT;
		}
		*screencolp = curcol;
	}
	if (!moved)
		*screencolp += nuadj;
	return moved;
}

/*	upddex:	de-extend any line that deserves it		*/

static void
upddex()
{
	register WINDOW *wp;
	fast_ptr LINEPTR lp;
	register int i;

	for_each_window(wp) {
		lp = wp->w_line.l;
		i = wp->w_toprow;

		curtabval = tabstop_val(wp->w_bufp);

		while (i < wp->w_toprow + wp->w_ntrows) {
			if (vscreen[i]->v_flag & VFEXT) {
				if ((wp != curwp)
				 || (!same_ptr(lp, wp->w_dot.l))
				 || ((i != currow)
				  && (curcol < col_limit(wp)))) {
					l_to_vline(wp,lp,i);
					vteeol();
					/* this line no longer is extended */
					vscreen[i]->v_flag &= ~VFEXT;
				}
			}
			i += line_height(wp,lp);
			lp = lFORW(lp);
		}
	}
	curtabval = tabstop_val(curbp);
}

/*	updgar:	if the screen is garbage, clear the physical screen and
		the virtual screen and force a full update		*/

extern char mlsave[];

static void
updgar()
{
#if !MEMMAP
	register char *txt;
	register int j;
#endif
	register int i;

	for (i = 0; i < term.t_nrow; ++i) {
		vscreen[i]->v_flag |= VFCHG;
#if	REVSTA
		vscreen[i]->v_flag &= ~VFREV;
#endif
#if	COLOR
		vscreen[i]->v_fcolor = gfcolor;
		vscreen[i]->v_bcolor = gbcolor;
#endif
#if	! MEMMAP
		txt = pscreen[i]->v_text;
		for (j = 0; j < term.t_ncol; ++j)
			txt[j] = ' ';
#endif
	}

	movecursor(0, 0);		 /* Erase the screen. */
	TTeeop();
	sgarbf = FALSE;			 /* Erase-page clears */
	mpresf = 0;			 /* the message area. */
	if (mlsave[0]) {
		mlforce(mlsave);
	}
#if	COLOR
	else
		mlerase();		/* needs to be cleared if colored */
#endif
}

/*	updupd:	update the physical screen from the virtual screen	*/

static void
updupd(force)
int force;	/* forced update flag */
{
	register int i;

#if CAN_SCROLL
	if (scrflags & WFKILLS)
		scrolls(FALSE);
	if (scrflags & WFINS)
		scrolls(TRUE);
	scrflags = 0;
#endif

	for (i = 0; i < term.t_nrow; ++i) {
		/* for each line that needs to be updated*/
		if ((vscreen[i]->v_flag & (VFCHG|VFCOL)) != 0) {
#if	TYPEAH
			if (force == FALSE && typahead())
				return;
#endif
			updateline(i, 0, term.t_ncol);
		}
	}
}

/*
 * Translate offset (into a line's text) into the display-column, taking into
 * account the tabstop, sideways, number- and list-modes.
 */
int
offs2col(wp, lp, offset)
WINDOW	*wp;
LINEPTR	lp;
C_NUM	offset;
{
	SIZE_T	length = lLength(lp);
	int	column = 0;
	int	tabs = tabstop_val(wp->w_bufp);
	int	list = w_val(wp,WMDLIST);
	int	left =
#ifdef WMDLINEWRAP	/* overrides left/right scrolling */
			w_val(wp,WMDLINEWRAP) ? 0 :
#endif
			w_val(wp,WVAL_SIDEWAYS);

	register C_NUM	n, c;

	if (same_ptr(lp, win_head(wp))) {
		column = 0;
	} else {
		for (n = w_left_margin(wp); (n < offset) && (n <= length); n++) {
			c = (n == length) ? '\n' : l_ref(lp)->l_text[n];
			if (isprint(c)) {
				column++;
			} else if (list || (c != '\t')) {
				column += 2;
			} else if (c == '\t') {
				column = ((column / tabs) + 1) * tabs;
			}
		}
		column = column - left + nu_width(wp) + w_left_margin(wp);
	}
	return column;
}

/*
 * Translate a display-column (assuming an infinitely-wide display) into the
 * line's offset, taking into account the tabstop, sideways, number and list
 * modes.
 */
#if OPT_MOUSE
int
col2offs(wp, lp, col)
WINDOW	*wp;
LINEPTR	lp;
C_NUM	col;
{
	int	tabs = tabstop_val(wp->w_bufp);
	int	list = w_val(wp,WMDLIST);
	int	left =
#ifdef WMDLINEWRAP	/* overrides left/right scrolling */
			w_val(wp,WMDLINEWRAP) ? 0 :
#endif
			w_val(wp,WVAL_SIDEWAYS);
	int	goal = col + left - nu_width(wp) - w_left_margin(wp);

	register C_NUM	n;
	register C_NUM	offset;
	register C_NUM	len	= llength(l_ref(lp));
	register char	*text	= l_ref(lp)->l_text;

	if (same_ptr(lp, win_head(wp))) {
		offset = 0;
	} else {
		for (offset = w_left_margin(wp), n = 0;
			(offset < len) && (n < goal);
				offset++) {
			register int c = text[offset];
			if (isprint(c)) {
				n++;
			} else if (list || (c != '\t')) {
				n += 2;
			} else if (c == '\t') {
				n = ((n / tabs) + 1) * tabs;
			}
			if (n > goal)
				break;
		}
	}
	return offset;
}
#endif

/*
 * Compute the number of rows required for displaying a line.
 */
#ifdef WMDLINEWRAP
int
line_height(wp, lp)
WINDOW	*wp;
LINEPTR	lp;
{
	if (w_val(wp,WMDLINEWRAP)) {
		int	len = lLength(lp);
		if (len > 0) {
			return (offs2col(wp,lp,len) + term.t_ncol - 1)
				/ term.t_ncol;
		}
	}
	return 1;
}
#endif

/*
 * Given a row on the screen, determines which window it belongs to.  Returns
 * null only for the message line.
 */
#if defined(WMDLINEWRAP) || OPT_MOUSE
WINDOW *
row2window (row)
int	row;
{
	register WINDOW *wp;

	for_each_window(wp)
		if (row >= wp->w_toprow && row <= wp->w_ntrows + wp->w_toprow)
			return wp;
	return 0;
}
#endif

/*
 * Highlight the requested portion of the screen.  We're mucking with the video
 * attributes on the line here, so this is NOT good code - it would be better
 * if there was an individual colour attribute per character, rather than per
 * row, but I didn't write the original code.  Anyway, hilite is called only
 * once so far, so it's not that big a deal.
 */
void
hilite(row, colfrom, colto, on)
int	row;		/* row to start highlighting */
int	colfrom;	/* column to start highlighting */
int	colto;		/* column to end highlighting */
int	on;		/* start highlighting */
{
	register VIDEO *vp1 = vscreen[row];
#ifdef WMDLINEWRAP
	WINDOW	*wp = row2window(row);
	if (w_val(wp,WMDLINEWRAP)) {
		if (colfrom < 0)
			colfrom = 0;
		if (colfrom > term.t_ncol) {
			do {
				row++;
				colfrom -= term.t_ncol;
				colto   -= term.t_ncol;
				hilite(row, colfrom, colto, on);
			} while (colto > term.t_ncol);
			return;
		}
	}
#endif
	if (row < term.t_nrow && (colfrom >= 0 || colto <= term.t_ncol)) {
		if (on) {
			vp1->v_flag |= VFREQ;
		} else {
			vp1->v_flag &= ~VFREQ;
		}
		if (colfrom < 0)
			colfrom = 0;
		if (colto > term.t_ncol)
			colto = term.t_ncol;
		updateline(row, colfrom, colto);
	}
}

#if CAN_SCROLL
/* optimize out scrolls (line breaks, and newlines) */
/* arg. chooses between looking for inserts or deletes */
static int
scrolls(inserts)	/* returns true if it does something */
int inserts;
{
	struct	VIDEO *vpv ;	/* virtual screen image */
	struct	VIDEO *vpp ;	/* physical screen image */
	int	i, j, k ;
	int	rows, cols ;
	int	first, match, count, ptarget = 0, vtarget = 0, end ;
	int	longmatch, longcount;
	int	from, to;

	if (!term.t_scroll) /* no way to scroll */
		return FALSE;

	rows = term.t_nrow ;
	cols = term.t_ncol ;

	first = -1 ;
	for (i = 0; i < rows; i++) {	/* find first wrong line */
		if (!texttest(i,i)) {
			first = i;
			break;
		}
	}

	if (first < 0)
		return FALSE;		/* no text changes */

	vpv = vscreen[first] ;
	vpp = PScreen(first) ;

	if (inserts) {
		/* determine types of potential scrolls */
		end = endofline(vpv->v_text,cols) ;
		if ( end == 0 )
			ptarget = first ;		/* newlines */
		else if ( strncmp(vpp->v_text, vpv->v_text, end) == 0 )
			ptarget = first + 1 ;	/* broken line newlines */
		else
			ptarget = first ;
		from = ptarget;
	} else {
		from = vtarget = first + 1 ;
	}

	/* find the matching shifted area */
	longmatch = -1;
	longcount = 0;
	for (i = from+1; i < rows; i++) {
		if (inserts ? texttest(i,from) : texttest(from,i) ) {
			match = i ;
			count = 1 ;
			for (j=match+1, k=from+1; j<rows && k<rows; j++, k++) {
				if (inserts ? texttest(j,k) : texttest(k,j))
					count++ ;
				else
					break ;
			}
			if (longcount < count) {
				longcount = count;
				longmatch = match;
			}
		}
	}
	match = longmatch;
	count = longcount;

	if (!inserts) {
		/* full kill case? */
		if (match > 0 && texttest(first, match-1)) {
			vtarget-- ;
			match-- ;
			count++ ;
		}
	}

	/* do the scroll */
	if (match>0 && count>2) {		 /* got a scroll */
		/* move the count lines starting at ptarget to match */
		/* mlwrite("scrolls: move the %d lines starting at %d to %d",
						count,ptarget,match);
		*/
		if (inserts) {
			from = ptarget;
			to = match;
		} else {
			from = match;
			to = vtarget;
		}
		scrscroll(from, to, count) ;
		for (i = 0; i < count; i++) {
			vpp = PScreen(to+i) ;
			vpv = vscreen[to+i];
			(void)strncpy(vpp->v_text, vpv->v_text, cols) ;
		}
		if (inserts) {
			from = ptarget;
			to = match;
		} else {
			from = vtarget+count;
			to = match+count;
		}
		for (i = from; i < to; i++) {
			char *txt;
			txt = PScreen(i)->v_text;
			for (j = 0; j < term.t_ncol; ++j)
				txt[j] = ' ';
			vscreen[i]->v_flag |= VFCHG;
		}
		return(TRUE) ;
	}
	return(FALSE) ;
}

/* move the "count" lines starting at "from" to "to" */
void
scrscroll(from, to, count)
int from, to, count;
{
	beginDisplay;
	ttrow = ttcol = -1;
	TTscroll(from,to,count);
	endofDisplay;
}

static int
texttest(vrow,prow)		/* return TRUE on text match */
int	vrow, prow ;		/* virtual, physical rows */
{
	struct	VIDEO *vpv = vscreen[vrow] ;	/* virtual screen image */
	struct	VIDEO *vpp = PScreen(prow)  ;	/* physical screen image */

	return (!memcmp(vpv->v_text, vpp->v_text, term.t_ncol)) ;
}

/* return the index of the first blank of trailing whitespace */
static int
endofline(s,n)
char 	*s;
int	n;
{
	int	i;
	for (i = n - 1; i >= 0; i--)
		if (s[i] != ' ') return(i+1) ;
	return(0) ;
}

#endif /* SCROLLCODE */


/*	updext_past: update the extended line which the cursor is currently
		on at a column greater than the terminal width. The line
		will be scrolled right or left to let the user see where
		the cursor is		*/
static int
updext_past(col, excess)
int	col;
int	excess;
{
	register int rcursor;
	register int zero = nu_width(curwp);

	/* calculate what column the real cursor will end up in */
	rcursor = ((excess - 1) % term.t_scrsiz) + term.t_margin;
	taboff = col - rcursor;

	/* scan through the line outputing characters to the virtual screen */
	/* once we reach the left edge					*/

	/* start scanning offscreen */
	vtmove(currow, -taboff);
	vtset(DOT.l, curwp);

	/* truncate the virtual line, restore tab offset */
	vteeol();
	taboff = 0;

	/* and put a marker in column 1 */
	vscreen[currow]->v_text[zero] = MRK_EXTEND_LEFT;
	vscreen[currow]->v_flag |= (VFEXT | VFCHG);
	return rcursor;
}

/*	updext_before: update the extended line which the cursor is currently
		on at a column less than the terminal width. The line
		will be scrolled right or left to let the user see where
		the cursor is		*/
static int
updext_before(col)
int	col;
{
	register int rcursor;

	curcol = col;

	/* calculate what column the real cursor will end up in */
	rcursor = (col % (term.t_ncol - term.t_margin));
	taboff = col - rcursor;

	/* scan through the line outputing characters to the virtual screen */
	/* once we reach the left edge					*/
	vtmove(currow, -taboff);	/* start scanning offscreen */
	vtset(DOT.l, curwp);

	/* truncate the virtual line, restore tab offset */
	vteeol();
	taboff = 0;

	if (col != rcursor) { /* ... put a marker in column 1 */
		vscreen[currow]->v_text[nu_width(curwp)] = MRK_EXTEND_LEFT;
		vscreen[currow]->v_flag |= VFEXT;
	}
	vscreen[currow]->v_flag |= (VFEXT|VFCHG);
	return rcursor;
}



/*
 * Update a single line. This does not know how to use insert or delete
 * character sequences; we are using VT52 functionality. Update the physical
 * row and column variables. It does try an exploit erase to end of line.
 */
#if	MEMMAP
/*	UPDATELINE specific code for the IBM-PC and other compatibles */

static void
updateline(row, colfrom, colto)

int	row;		/* row of screen to update */
int	colfrom;	/* first column on screen */
int	colto;		/* last column on screen */

{
	register struct VIDEO *vp1 = vscreen[row];	/* virtual screen image */
	register int	req = (vp1->v_flag & VFREQ) == VFREQ;
#if	COLOR
	vp1->v_fcolor = vp1->v_rfcolor;
	vp1->v_bcolor = vp1->v_rbcolor;
	scwrite(row, colfrom, colto - colfrom,
		vp1->v_text,
		req ? vp1->v_rbcolor : vp1->v_rfcolor,
		req ? vp1->v_rfcolor : vp1->v_rbcolor);
#else
	scwrite(row, colfrom, colto - colfrom,
		vp1->v_text,
		req ? C_BLACK : C_WHITE,
		req ? C_WHITE : C_BLACK);
#endif
	vp1->v_flag &= ~(VFCHG | VFCOL); /* flag this line as updated */
	if (req)
		vp1->v_flag |= VFREV;
	else
		vp1->v_flag &= ~VFREV;
}

#else	/* !MEMMAP */

static void
updateline(row, colfrom, colto)

int	row;		/* row of screen to update */
int	colfrom;	/* first column on screen */
int	colto;		/* first column on screen */

{
    struct VIDEO *vp1 = vscreen[row];	/* virtual screen image */
    struct VIDEO *vp2 = PSCREEN[row];	/* physical screen image */
/*	UPDATELINE code for all other versions		*/

	register char *cp1;
	register char *cp2;
	register char *cp3;
	register char *cp4;
	register char *cp5;
	register int nbflag;	/* non-blanks to the right flag? */
	int rev;		/* reverse video flag */
	int req;		/* reverse video request flag */


	/* set up pointers to virtual and physical lines */
	cp1 = &vp1->v_text[colfrom];
	cp2 = &vp2->v_text[colfrom];

#if	COLOR
	TTforg(vp1->v_rfcolor);
	TTbacg(vp1->v_rbcolor);
#endif

#if	REVSTA | COLOR
	/* if we need to change the reverse video status of the
	   current line, we need to re-write the entire line     */
	rev = (vp1->v_flag & VFREV) == VFREV;
	req = (vp1->v_flag & VFREQ) == VFREQ;
	if ((rev != req)
#if	COLOR
	    || (vp1->v_fcolor != vp1->v_rfcolor)
	    || (vp1->v_bcolor != vp1->v_rbcolor)
#endif
#if	HP150
	/* the HP150 has some reverse video problems */
	    || req || rev
#endif
			) {
		movecursor(row, colfrom);	/* Go to start of line. */
		/* set rev video if needed */
		if (req)
			TTrev(req);

		/* scan through the line and dump it to the screen and
		   the virtual screen array				*/
		cp3 = &vp1->v_text[colto];
#if X11
		x_putline(row, cp1, cp3 - cp1);
#endif
		while (cp1 < cp3) {
#if !X11
			TTputc(*cp1);
#endif
			++ttcol;
			*cp2++ = *cp1++;
		}
		/* turn rev video off */
		if (req)
			TTrev(FALSE);

		/* update the needed flags */
		vp1->v_flag &= ~(VFCHG|VFCOL);
		if (req)
			vp1->v_flag |= VFREV;
		else
			vp1->v_flag &= ~VFREV;
#if	COLOR
		vp1->v_fcolor = vp1->v_rfcolor;
		vp1->v_bcolor = vp1->v_rbcolor;
#endif
		return;
	}
#else
	rev = FALSE;
#endif

	/* advance past any common chars at the left */
	if (!rev)
		while (cp1 != &vp1->v_text[colto] && *cp1 == *cp2) {
			++cp1;
			++cp2;
		}

/* This can still happen, even though we only call this routine on changed
 * lines. A hard update is always done when a line splits, a massive
 * change is done, or a buffer is displayed twice. This optimizes out most
 * of the excess updating. A lot of computes are used, but these tend to
 * be hard operations that do a lot of update, so I don't really care.
 */
	/* if both lines are the same, no update needs to be done */
	if (cp1 == &vp1->v_text[colto]) {
		vp1->v_flag &= ~VFCHG;	/* flag this line unchanged */
		return;
	}

	/* find out if there is a match on the right */
	nbflag = FALSE;
	cp3 = &vp1->v_text[colto];
	cp4 = &vp2->v_text[colto];

	if (!rev)
		while (cp3[-1] == cp4[-1]) {
			--cp3;
			--cp4;
			if (cp3[0] != ' ')		/* Note if any nonblank */
				nbflag = TRUE;		/* in right match. */
		}

	cp5 = cp3;

	/* Erase to EOL ? */
	if (nbflag == FALSE && eolexist == TRUE 
#if	REVSTA
		&& (req != TRUE)
#endif
			) {
		while (cp5!=cp1 && cp5[-1]==' ')
			--cp5;

		if (cp3-cp5 <= 3)		/* Use only if erase is */
			cp5 = cp3;		/* fewer characters. */
	}

	movecursor(row, cp1 - &vp1->v_text[colfrom]);	/* Go to start of line. */
#if	REVSTA
	TTrev(rev);
#endif

#if X11
	x_putline(row, cp1, cp5 - cp1 + 1);
#endif
	while (cp1 != cp5) {		/* Ordinary. */
#if !X11
		TTputc(*cp1);
#endif
		++ttcol;
		*cp2++ = *cp1++;
	}

	if (cp5 != cp3) {		/* Erase. */
		TTeeol();
		while (cp1 != cp3)
			*cp2++ = *cp1++;
	}
#if	REVSTA
	TTrev(FALSE);
#endif
	vp1->v_flag &= ~(VFCHG|VFCOL);	/* flag this line as updated */
	return;
}
#endif	/* MEMMAP(updateline) */

static	char *	PutMode_gap;

static void
PutMode (name)
char	*name;
{
	vtputsn(PutMode_gap, 10);
	vtputsn(name, 20);
	PutMode_gap = " ";
}

/*
 * Redisplay the mode line for the window pointed to by the "wp". This is the
 * only routine that has any idea of how the modeline is formatted. You can
 * change the modeline format by hacking at this routine. Called by "update"
 * any time there is a dirty window.
 */
static void
modeline(wp)
WINDOW *wp;
{
	register int n;
	register BUFFER *bp;
	register lchar;		/* character to draw line in buffer with */
	int	left, col;
	char	temp[NFILEN];

	n = wp->w_toprow+wp->w_ntrows;      	/* Location. */
	vscreen[n]->v_flag |= VFCHG | VFREQ | VFCOL;/* Redraw next time. */
#if	COLOR
	vscreen[n]->v_rfcolor = w_val(wp,WVAL_FCOLOR);
	vscreen[n]->v_rbcolor = w_val(wp,WVAL_BCOLOR);
#endif
	vtmove(n, 0);                       	/* Seek to right line. */
	if (wp == curwp) {			/* mark the current buffer */
		lchar = '=';
	} else {
#if	REVSTA
		if (revexist)
			lchar = ' ';
		else
#endif
			lchar = '-';
	}
	bp = wp->w_bufp;

	vtputc(lchar);
	if (b_val(bp, MDSHOWMODE)) {
		register int ic;
		ic = lchar;
		if (wp == curwp) {
			if (insertmode == INSERT)
				ic = 'I';
			else if (insertmode == REPLACECHAR)
				ic = 'R';
			else if (insertmode == OVERWRITE)
				ic = 'O';
		}
		vtputc(ic);
	}
	vtprintf("%c %s",lchar, get_bname(bp));

	/* show the major-modes of the buffer */
	PutMode_gap = " [";
	if (b_val(bp,MDCMOD))
		PutMode("cmode");
#if CRYPT
	if (b_val(bp,MDCRYPT))
		PutMode("crypt");
#endif
	if (b_val(bp,MDDOS))
		PutMode("dos-style");
	if (b_val(bp,MDVIEW))
		PutMode("view-only");
	if (!PutMode_gap[1])
		vtputc(']');

	if (b_is_changed(bp))
		vtputsn(" [modified]", 20);
	if (bp->b_fname != 0 && bp->b_fname[0] != EOS) {
		char *p;
		p = shorten_path(strcpy(temp,bp->b_fname), FALSE);
		if (p != 0 && !eql_bname(bp, p)) {
			/* line-up the internal-names */
#if !SMALLER
			if (is_internalname(p)) {
				int	fix,
					len = strlen(p),
					gap = term.t_ncol - (11 + len + vtcol);
				vtputc(' ');
				while (gap-- > 0)
					vtputc(lchar);
				fix = vtcol;
				vtputsn(p, len);
				if (lchar != ' ') {
					register int m;
					char	*s = vscreen[vtrow]->v_text;

					for (m = fix; m < vtcol && isspace(s[m]); m++)
						s[m] = lchar;
					s[--m] = ' ';
				}
			} else
#endif /* !SMALLER */
				vtprintf(" is %s", p);
		}
	}
	vtputc(' ');

	/* Pad to full width, then go back and overwrite right-end info */
	n = term.t_ncol;
	while (vtcol < n)
		vtputc(lchar);

#ifdef WMDRULER
	if (w_val(wp,WMDRULER)) {
		(void)lsprintf(temp, "(%d,%d)",
			wp->w_ruler_line, wp->w_ruler_col);
		vtcol = n - strlen(temp);
		vtputsn(temp, sizeof(temp));
	} else
#endif
	{ /* determine if top line, bottom line, or both are visible */
		LINE *lp = l_ref(wp->w_line.l);
		int rows = wp->w_ntrows;
		char *msg = NULL;

		vtcol = n - 7;  /* strlen(" top ") plus a couple */
		while (rows--) {
			lp = lforw(lp);
			if (lp == l_ref(wp->w_bufp->b_line.l)) {
				msg = " bot ";
				break;
			}
		}
		if (lBack(wp->w_line.l) == l_ref(wp->w_bufp->b_line.l)) {
			if (msg) {
				if (same_ptr(wp->w_line.l, wp->w_bufp->b_line.l))
					msg = " emp ";
				else
					msg = " all ";
			} else {
				msg = " top ";
			}
		}
		if (!msg)
			msg = " mid ";
		vtputsn(msg,20);
		vtputc(lchar);
		vtputc(lchar);

	}
#ifdef show_tabstop_on_modeline
	{ /* put in the tabstop value */
		int t = tabstop_val(wp->w_bufp);
		vtcol = n - 11;
		while (t) {
			vtputc((t%10)+'0');
			t /= 10;
			vtcol -= 2;
		}
	}
#endif

	/* mark column 80 */
	left = -nu_width(wp);
#ifdef WMDLINEWRAP
	if (!w_val(wp,WMDLINEWRAP))
#endif
	 left += w_val(wp,WVAL_SIDEWAYS);
	n += left;
	col = 80 - left;

	if ((n > 80) && (col >= 0) && (vtgetc(col) == lchar)) {
		vtcol = col;
		vtputc('|');
	}
}

void
upmode()	/* update all the mode lines */
{
	register WINDOW *wp;

	for_each_window(wp)
		wp->w_flag |= WFMODE;
}

/*
 * Recompute the given buffer. Save/restore its modes and position information
 * so that a redisplay will show as little change as possible.
 */
#if	OPT_UPBUFF
typedef	struct	{
	WINDOW	*wp;
	struct VAL w_vals[MAX_W_VALUES];
	int	top;
	int	line;
	int	col;
	} SAVEWIN;

static	SAVEWIN	*recomp_tbl;
static	ALLOC_T	recomp_len;

static void
recompute_buffer(bp)
BUFFER	*bp;
{
	register WINDOW *wp;
	register SAVEWIN *tbl;

	struct VAL b_vals[MAX_B_VALUES];
	int	num = 0;
	BUFFER *savebp = curbp;
	WINDOW *savewp = curwp;
	int	mygoal = curgoal;

	if (!b_val(bp,MDUPBUFF)) {
		b_clr_flags(bp,BFUPBUFF);
		return;
	}
	if (recomp_len < bp->b_nwnd) {
		recomp_len = bp->b_nwnd + 1;
		recomp_tbl = (recomp_tbl != 0)
			? typereallocn(SAVEWIN,recomp_tbl,recomp_len)
			: typeallocn(SAVEWIN,recomp_len);
		if (recomp_tbl == 0) {
			recomp_len = 0;
			return;
		}
	}
	tbl = recomp_tbl;

	/* remember where we are, to reposition */
	/* ...in case line is deleted from buffer-list */
	if (curbp != bp) {
		curbp = bp;
		curwp = bp2any_wp(bp);
	}
	for_each_window(wp) {
		if (wp->w_bufp == bp) {
			curwp = wp;	/* to make 'getccol()' work */
			curbp = curwp->w_bufp;
			tbl[num].wp   = wp;
			tbl[num].top  = line_no(bp, wp->w_line.l);
			tbl[num].line = line_no(bp, wp->w_dot.l);
			tbl[num].col  = getccol(FALSE);
			save_vals(NUM_W_VALUES, global_w_values.wv,
				tbl[num].w_vals, wp->w_values.wv);
			if (++num >= recomp_len)
				break;
		}
	}
	curwp = savewp;
	curbp = savebp;

	save_vals(NUM_B_VALUES, global_b_values.bv, b_vals, bp->b_values.bv);
	(bp->b_upbuff)(bp);
	copy_mvals(NUM_B_VALUES, bp->b_values.bv, b_vals);

	/* reposition and restore */
	while (num-- > 0) {
		curwp = wp = tbl[num].wp;
		curbp = curwp->w_bufp;
		(void)gotoline(TRUE, tbl[num].top);
		wp->w_line.l = wp->w_dot.l;
		wp->w_line.o = 0;
		if (tbl[num].line != tbl[num].top)
			(void)gotoline(TRUE, tbl[num].line);
		(void)gocol(tbl[num].col);
        	wp->w_flag |= WFMOVE;
		copy_mvals(NUM_W_VALUES, wp->w_values.wv, tbl[num].w_vals);
	}
	curwp = savewp;
	curbp = savebp;
	curgoal = mygoal;
	b_clr_flags(bp,BFUPBUFF);
}
#endif

/*
 * Send a command to the terminal to move the hardware cursor to row "row"
 * and column "col". The row and column arguments are origin 0. Optimize out
 * random calls. Update "ttrow" and "ttcol".
 */
void
movecursor(row, col)
int row,col;
{
	beginDisplay;
	if (row!=ttrow || col!=ttcol)
        {
	        ttrow = row;
	        ttcol = col;
	        TTmove(row, col);
        }
	endofDisplay;
}

/* Erase the message-line from the current position */
static void
erase_remaining_msg (column)
int	column;
{
	beginDisplay;
#if !RAMSIZE
	if (eolexist == TRUE)
		TTeeol();
	else
#endif
	{
		register int i;
#if RAMSIZE
		int	limit = global_g_val(GMDRAMSIZE)
				? LastMsgCol
				: term.t_ncol - 1;
#else
		int	limit = term.t_ncol - 1;
#endif
		for (i = ttcol; i < limit; i++)
			TTputc(' ');
		ttrow = term.t_nrow-1;	/* force the move! */
		movecursor(term.t_nrow, column);
	}
	TTflush();
	endofDisplay;
}


/*
 * Erase the message line. This is a special routine because the message line
 * is not considered to be part of the virtual screen. It always works
 * immediately; the terminal buffer is flushed via a call to the flusher.
 */
void
mlerase()
{
	beginDisplay;
	if (mpresf != 0) {
		movecursor(term.t_nrow, 0);
		if (discmd != FALSE) {
#if	COLOR
			TTforg(gfcolor);
			TTbacg(gbcolor);
#endif
			erase_remaining_msg(0);
			mpresf = 0;
		}
	}
	endofDisplay;
}

char *mlsavep;
char mlsave[NSTRING];

void
mlsavec(c)
int c;
{
	if (mlsavep - mlsave < NSTRING-1) {
		*mlsavep++ = c;
		*mlsavep = EOS;
	}

}

/*
 * Write a message into the message line only if appropriate.
 */
/* VARARGS1 */
void
#if	ANSI_VARARGS
mlwrite( char *fmt, ...)
#else
mlwrite(va_alist)
va_dcl
#endif
{
	va_list ap;
	/* if we are not currently echoing on the command line, abort this */
	if (global_b_val(MDTERSE) || kbd_replaying(FALSE) || discmd == FALSE) {
		movecursor(term.t_nrow, 0);
		return;
	}
#if	ANSI_VARARGS
	va_start(ap,fmt);
	mlmsg(fmt,&ap);
#else
	va_start(ap);
	mlmsg(&ap);
#endif
	va_end(ap);
}

/*	Put a string out to the message line regardless of the
	current $discmd setting. This is needed when $debug is TRUE
	and for the write-message and clear-message-line commands
	Also used for most errors, to be sure they're seen.
*/
/* VARARGS1 */
void
#if	ANSI_VARARGS
mlforce(char *fmt, ...)
#else
mlforce(va_alist)
va_dcl
#endif
{
	va_list ap;
#if	ANSI_VARARGS
	va_start(ap,fmt);
	mlmsg(fmt,&ap);
#else
	va_start(ap);
	mlmsg(&ap);
#endif
	va_end(ap);
}

/* VARARGS1 */
void
#if	ANSI_VARARGS
mlprompt( char *fmt, ...)
#else
mlprompt(va_alist)
va_dcl
#endif
{
	va_list ap;
	int osgarbf = sgarbf;
	if (discmd == FALSE) {
		movecursor(term.t_nrow, 0);
		return;
	}
	sgarbf = FALSE;
#if	ANSI_VARARGS
	va_start(ap,fmt);
	mlmsg(fmt,&ap);
#else
	va_start(ap);
	mlmsg(&ap);
#endif
	va_end(ap);
	sgarbf = osgarbf;
}

/* VARARGS */
void
#if	ANSI_VARARGS
dbgwrite( char *fmt, ...)
#else
dbgwrite(va_alist)
va_dcl
#endif
{
	va_list ap;	/* ptr to current data field */
#if	ANSI_VARARGS
	va_start(ap,fmt);
	mlmsg(fmt,&ap);
#else
	va_start(ap);
	mlmsg(&ap);
#endif
	va_end(ap);
	beginDisplay;
	while (TTgetc() != '\007')
		;
	endofDisplay;
}

/*
 * Do the real message-line work.  Keep track of the physical cursor
 * position. A small class of printf like format items is handled.
 * Set the "message line" flag TRUE.
 */
void
#if	ANSI_VARARGS
mlmsg( char *fmt, va_list *app)
#else
mlmsg(app)
va_list *app;	/* ptr to current data field */
#endif
{

	if (sgarbf) {
		/* then we'll lose the message on the next update(), so save it now */
		mlsavep = mlsave;
		dfoutfn = mlsavec;
#if	ANSI_VARARGS
		dofmt(fmt,app);
#else
		dofmt(app);
#endif
		return;
	}

#if	COLOR
	/* set up the proper colors for the command line */
	TTforg(gfcolor);
	TTbacg(gbcolor);
#endif

	beginDisplay;
	movecursor(term.t_nrow, 0);

	kbd_expand = -1;
	dfoutfn = kbd_putc;
#if	ANSI_VARARGS
	dofmt(fmt,app);
#else
	dofmt(app);
#endif
	kbd_expand = 0;

	/* if we can, erase to the end of screen */
	erase_remaining_msg(ttcol);
	mpresf = ttcol;
	mlsave[0] = EOS;
	endofDisplay;
}

/*
 * Do the equivalent of 'perror()' on the message line
 */
void
mlerror(s)
char	*s;
{
#if UNIX || VMS || NEWDOSCC
	if (errno > 0 && errno < sys_nerr)
		mlforce("[%s: %s]", s, sys_errlist[errno]);
#endif
}

/*
 * Local sprintf -- similar to standard libc, but
 *  returns pointer to null character at end of buffer, so it can
 *  be called repeatedly, as in:
 *	cp = lsprintf(cp, fmt, args...);
 *
 */

char *lsp;

void
lspputc(c)
int c;
{
	*lsp++ = c;
}

/* VARARGS1 */
char *
#if	ANSI_VARARGS
lsprintf( char *buf, char *fmt, ...)
#else
lsprintf(va_alist)
va_dcl
#endif
{

	va_list ap;
#if	ANSI_VARARGS
	va_start(ap,fmt);
#else
	char *buf;
	va_start(ap);
	buf = va_arg(ap, char *);
#endif

	lsp = buf;
	dfoutfn = lspputc;

#if	ANSI_VARARGS
	dofmt(fmt,&ap);
#else
	dofmt(&ap);
#endif
	va_end(ap);

	*lsp = EOS;
	return lsp;
}

#ifdef	UNUSED
static char *lsbuf;

void
lssetbuf(buf)
char *buf;
{
	lsbuf = buf;
}

/* VARARGS1 */
char *
#if	ANSI_VARARGS
_lsprintf( char *fmt, ...)
#else
_lsprintf(va_alist)
va_dcl
#endif
{

	va_list ap;
#if	ANSI_VARARGS
	va_start(ap,fmt);
#else
	va_start(ap);
#endif

	lsp = lsbuf;
	dfoutfn = lspputc;

#if	ANSI_VARARGS
	dofmt(fmt,&ap);
#else
	dofmt(&ap);
#endif
	va_end(ap);

	*lsp = EOS;
	return lsp;
}
#endif	/* UNUSED */

/*
 * Buffer printf -- like regular printf, but puts characters
 *	into the BUFFER.
 */
void
bputc(c)
int c;
{
	if (c == '\n')
		(void)lnewline();
	else
		(void)linsert(1,c);
}

/* printf into curbp, at DOT */
/* VARARGS */
void
#if	ANSI_VARARGS
bprintf( char *fmt, ...)
#else
bprintf(va_alist)
va_dcl
#endif
{
	va_list ap;

	dfoutfn = bputc;

#if	ANSI_VARARGS
	va_start(ap,fmt);
	dofmt(fmt,&ap);
#else
	va_start(ap);
	dofmt(&ap);
#endif
	va_end(ap);

}

/* Get terminal size from system, first trying the driver, and then
 * the environment.  Store number of lines into *heightp and width
 * into *widthp.  If zero or a negative number is stored, the value
 * is not valid.  This may be fixed (in the tcap.c case) by the TERM
 * variable.
 */
#if defined( SIGWINCH) && ! X11
void
getscreensize (widthp, heightp)
int *widthp, *heightp;
{
	char *e;
#ifdef TIOCGWINSZ
	struct winsize size;
#endif
	*widthp = 0;
	*heightp = 0;
#ifdef TIOCGWINSZ
	if (ioctl (0, TIOCGWINSZ, (caddr_t)&size) == 0) {
		if ((int)(size.ws_row) > 0)
			*heightp = size.ws_row;
		if ((int)(size.ws_col) > 0)
			*widthp = size.ws_col;
	}
	if (*widthp <= 0) {
		e = getenv("COLUMNS");
		if (e)
			*widthp = atoi(e);
	}
	if (*heightp <= 0) {
		e = getenv("LINES");
		if (e)
			*heightp = atoi(e);
	}
#else
	e = getenv("COLUMNS");
	if (e)
		*widthp = atoi(e);
	e = getenv("LINES");
	if (e)
		*heightp = atoi(e);
#endif
}

/* ARGSUSED */
SIGT
sizesignal (ACTUAL_SIG_ARGS)
ACTUAL_SIG_DECL
{
	int w, h;
	int old_errno = errno;

	getscreensize (&w, &h);

	if ((h > 1 && h-1 != term.t_nrow) || (w > 1 && w != term.t_ncol))
		newscreensize(h, w);

	(void)signal(SIGWINCH, sizesignal);
	errno = old_errno;
	SIGRET;
}
#endif

void
newscreensize (h, w)
int h, w;
{
	/* do the change later */
	if (displaying) {
		chg_width = w;
		chg_height = h;
		return;
	}
	chg_width = chg_height = 0;
	if ((h - 1) <= term.t_mrow)
		if (!newlength(TRUE,h))
			return;
	if (w < term.t_mcol)
		if (!newwidth(TRUE,w))
			return;

	(void)update(TRUE);
}

/*
 * Displays alternate
 *	"working..." and
 *	"...working"
 * at the end of the message line if it has been at least a second since
 * displaying anything or waiting for keyboard input.  The cur_working and
 * max_working values are used in 'slowreadf()' to show the progress of reading
 * large files.
 */
#if OPT_WORKING
/*ARGSUSED*/
SIGT
imworking (ACTUAL_SIG_ARGS)
ACTUAL_SIG_DECL
{
	static	char	*msg[] = {"working", "..."};
	static	int	flip;
	static	int	skip;

	if (displaying) {	/* look at the semaphore first! */
		;
	} else if (!global_b_val(MDTERSE) && !doing_kbd_read) {
		if (skip) {
			skip = FALSE;
		} else if (displayed) {
			int	save_row = ttrow;
			int	save_col = ttcol;
			int	show_col = LastMsgCol - 10;
			if (show_col < 0)
				show_col = 0;
			movecursor(term.t_nrow, show_col);
			if (cur_working != 0
			 && cur_working != old_working) {
				char	temp[20];
				int	len = cur_working > 999999L ? 10 : 6;

				old_working = cur_working;
				kbd_puts(right_num(temp, len, cur_working));
				if (len == 10)
					;
				else if (max_working != 0) {
					kbd_putc(' ');
					kbd_puts(right_num(temp, 2,
						(100 * cur_working) / max_working));
					kbd_putc('%');
				} else
					kbd_puts(" ...");
			} else {
				kbd_puts(msg[ flip]);
				kbd_puts(msg[!flip]);
			}
			movecursor(save_row, save_col);
			if (mpresf >= 0)
				mpresf = -(mpresf+1);
			TTflush();
#if X11
			x_working();
#endif
		}
	} else {
		if (mpresf < 0) {	/* erase leftover working-message */
			int	save_row = ttrow;
			int	save_col = ttcol;
			movecursor(term.t_nrow, -(mpresf+1));
			erase_remaining_msg(-(mpresf+1));
			movecursor(save_row, save_col);
			TTflush();
			mpresf = 0;
		}
		skip = TRUE;
	}
	(void)signal(SIGALRM,imworking);
	(void)alarm(1);
	flip = !flip;
}
#endif

/* For memory-leak testing (only!), releases all display storage. */
#if NO_LEAKS
void	vt_leaks()
{
	register int i;

	for (i = 0; i < term.t_mrow; ++i) {
		free ((char *)vscreen[i]);
#if	! MEMMAP
		free ((char *)pscreen[i]);
#endif
	}
	free ((char *)vscreen);
#if	! MEMMAP
	free ((char *)pscreen);
#endif
#if OPT_UPBUFF
	FreeIfNeeded(recomp_tbl);
#endif
}
#endif
