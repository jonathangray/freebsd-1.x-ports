/*
 * The functions in this file negotiate with the operating system for
 * characters, and write characters in a barely buffered fashion on the display.
 * All operating systems.
 *
 * $Log: termio.c,v $
 * Revision 1.1  1994/02/01 03:29:40  jkh
 * Initial revision
 *
 * Revision 1.82  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.81  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.80  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.79  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.78  1993/07/23  12:03:39  pgf
 * make ttclean a no-op under _all_ variants for X11 -- it was flushing
 * and closing in the USE_TERMIO case
 *
 * Revision 1.77  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.76  1993/07/08  11:51:39  pgf
 * use setvbuf if USG and POSIX_TERMIOS, since svr4 doesn't have setbuffer
 * unless you have the ucb extensions (Solaris, anyway)
 *
 * Revision 1.75  1993/07/06  16:39:04  pgf
 * integrated Tuan DANG's changes for the djgpp compiler under DOS
 *
 * Revision 1.74  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.73  1993/06/24  18:06:35  pgf
 * au/x keeps ioctl.h in sys
 *
 * Revision 1.72  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.71  1993/05/05  13:52:04  pgf
 * don't strip 8th bit in ttgetc()
 *
 * Revision 1.70  1993/05/05  12:25:16  pgf
 * osf1 and ultrix keep ioctl.h in sys
 *
 * Revision 1.69  1993/05/05  11:41:05  pgf
 * backspc is now handled separately from chartypes[backspc]
 *
 * Revision 1.68  1993/05/05  11:19:52  pgf
 * turned off typahead detection for X11 -- it gives me false-positives
 *
 * Revision 1.67  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.66  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.65  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.64  1993/04/20  12:00:37  pgf
 * AIX needs sys/ioctl.h
 *
 * Revision 1.63  1993/04/02  11:02:50  pgf
 * ioctl.h --> sys/ioctl.h for APOLLO
 *
 * Revision 1.62  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.61  1993/04/01  12:03:26  pgf
 * filio.h is in sys on sunos.
 *
 * Revision 1.60  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.59  1993/03/17  10:07:21  pgf
 * extra includes/defs for correct operation on OSF (shouldn't termios.h
 * provide TIOCGWINSIZE, and shouldn't FION_READ work?)
 *
 * Revision 1.58  1993/03/17  10:00:29  pgf
 * initial changes to make VMS work again
 *
 * Revision 1.57  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.56  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.55  1993/02/15  10:37:31  pgf
 * cleanup for gcc-2.3's -Wall warnings
 *
 * Revision 1.54  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.53  1993/01/16  10:42:47  foxharp
 * extra TTflush for apollo
 *
 * Revision 1.52  1992/12/23  09:27:52  foxharp
 * added missing fflush in ttclean
 *
 * Revision 1.51  1992/12/05  13:58:20  foxharp
 * added missing TTclose to ttclean, for Tom Dickey on his apollo -- I'm not
 * convinced this seems right, but I'll try it for now.
 *
 * Revision 1.50  1992/12/04  09:18:04  foxharp
 * added another flush
 *
 * Revision 1.49  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.48  1992/08/19  23:02:47  foxharp
 * ignore errors in ttunclean
 *
 * Revision 1.47  1992/08/04  20:14:58  foxharp
 * invert sense of ifdef -- it seems filio.h isn't as common as I thought
 *
 * Revision 1.46  1992/07/24  07:49:27  foxharp
 * aix has no filio.h
 *
 * Revision 1.45  1992/07/20  22:50:40  foxharp
 * use putchar/getchar instead of fputc/fgetc
 *
 * Revision 1.44  1992/07/17  19:19:47  foxharp
 * took out commented cruft
 *
 * Revision 1.43  1992/07/01  17:04:33  foxharp
 * always use && and || in #if statements, and
 * MSC now uses getch() to get characters, and
 * use TTputc() to put out cr-nl pairs instead of ttputc()
 *
 * Revision 1.42  1992/06/26  22:19:32  foxharp
 * improved (i hope) the FIONREAD ifdefs.  needs testing.
 *
 * Revision 1.41  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.40  1992/06/22  08:35:13  foxharp
 * AIX has the fcntl bug, but isn't BERK.  also added ifdef VSWTCH protection
 *
 * Revision 1.39  1992/06/14  11:33:54  foxharp
 * fcntl hack for AIX
 *
 * Revision 1.38  1992/06/11  09:44:10  foxharp
 * try to avoid the fcntl() bug with better ifdefing
 *
 * Revision 1.37  1992/05/27  19:16:22  foxharp
 * took out extra call to setbuffer()
 *
 * Revision 1.36  1992/05/25  21:34:29  foxharp
 * moved extern func declarations to header
 *
 * Revision 1.34  1992/05/20  18:55:42  foxharp
 * don't use fcntl on a/ux, either
 *
 * Revision 1.33  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.32  1992/05/13  09:13:41  pgf
 * make X11 do even less in ttclean/unclean routines
 *
 * Revision 1.31  1992/04/14  08:40:11  pgf
 * added osf hack, to use sgtty instead of posix, and fixed sun hack, to use
 * fionread instead of fcntl, to prevent output flush during typeahead check
 *
 * Revision 1.30  1992/04/10  18:50:22  pgf
 * always include termios.h, not sys/termios.h, and
 * don't call setbuffer on odt or isc
 *
 * Revision 1.29  1992/03/26  09:16:59  pgf
 * ifdef cleanup
 *
 * Revision 1.28  1992/03/25  19:13:17  pgf
 * BSD portability changes
 *
 * Revision 1.27  1992/03/20  08:59:28  pgf
 * kbd_is_polled was out of sync after ttclean/unclean
 *
 * Revision 1.26  1992/03/19  23:27:20  pgf
 * SIGT for signals, linux port
 *
 * Revision 1.25  1992/03/19  23:09:46  pgf
 * TIOCSETP is now TIOCSETN, in all cases
 *
 * Revision 1.24  1992/03/07  10:25:58  pgf
 * AIX support (where is termios.h supposed to be?)
 *
 * Revision 1.23  1992/01/22  20:28:28  pgf
 * call TTclose in ttunclean, to support tcapclose().  i hope this is safe
 * for other screen types
 *
 * Revision 1.22  1992/01/10  08:11:41  pgf
 * fixed signal handling, for sysV style handling.  also
 * fixed 2nd arg to tcsetattr()
 *
 * Revision 1.21  1992/01/02  22:42:40  pgf
 * oops -- dropped the termio restoral in ttclean
 *
 * Revision 1.20  1992/01/01  16:30:14  pgf
 * more ifdef'ing on V_SUSP (for pyramid?), and
 * now get tty chars under X11, though we still don't set new modes
 *
 * Revision 1.19  1991/12/20  08:12:00  pgf
 * check the error return from tcsetattr()
 *
 * Revision 1.18  1991/12/04  09:22:19  pgf
 * a little more massaging of ifdefs
 *
 * Revision 1.17  1991/11/27  10:13:10  pgf
 * code split/re-arranged, cleaner posix support
 *
 * Revision 1.16  1991/11/16  18:35:55  pgf
 * changed the ifdefing so termio or sgtty can be used independently of
 * other UNIX choices
 *
 * Revision 1.15  1991/11/14  08:58:17  pgf
 * switch to TCSETAW from TSSETAF in ttclean and unclean -- don't want to
 * flush input needlessly
 *
 * Revision 1.14  1991/11/13  20:09:27  pgf
 * X11 changes, from dave lemke
 *
 * Revision 1.13  1991/11/07  02:00:32  pgf
 * lint cleanup
 *
 * Revision 1.12  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.11  1991/10/15  12:18:39  pgf
 * fetch more termio chars at startup, like wkillc, suspc, etc.
 *
 * Revision 1.10  1991/08/12  15:05:43  pgf
 * rearranged read-ahead in one of the tgetc options
 *
 * Revision 1.9  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.8
 * date: 1991/07/19 17:17:49;
 * change backspace action to back_char_to_bol()
 * 
 * revision 1.7
 * date: 1991/06/28 10:54:00;
 * change a config ifdef
 * 
 * revision 1.6
 * date: 1991/04/22 09:06:57;
 * POSIX/ULTRIX changes
 * 
 * revision 1.5
 * date: 1991/02/14 15:50:41;
 * fixed size problem on olstate/nlstate
 * 
 * revision 1.4
 * date: 1990/12/06 18:54:48;
 * tried to get ttgetc to return -1 on interrupt -- doesn't work
 * 
 * revision 1.3
 * date: 1990/10/12 19:32:11;
 * do SETAF, not SETA in ttunclean
 * 
 * revision 1.2
 * date: 1990/10/03 16:01:04;
 * make backspace work for everyone
 * 
 * revision 1.1
 * date: 1990/09/21 10:26:10;
 * initial vile RCS revision
 */
#include	"estruct.h"
#include        "edef.h"

#if GO32
# include <pc.h>   /* for kbhit() */
#endif

#if UNIX

/* there are three copies of the tt...() routines here -- one each for
	POSIX termios, traditional termio, and sgtty.  If you have a
	choice, I recommend them in that order. */

/* ttopen() and ttclose() are responsible for putting the terminal in raw
	mode, setting up terminal signals, etc.
   ttclean() prepares the terminal for shell escapes, and ttunclean() gets
   	us back into vile's mode
*/

/* I suppose this config stuff should move to estruct.h */

#if POSIX
# define USE_POSIX_TERMIOS 1
# define USE_FCNTL 1
#else
# if USG
#  define USE_TERMIO 1
#  define USE_FCNTL 1
# else
#  if BERK || V7
#   define USE_SGTTY 1
#   define USE_FIONREAD 1
#  else
 huh?
#  endif
# endif
#endif

#if OSF1		/* don't know why termios doesn't work in osf */
# undef POSIX
# undef BERK
# define BERK 1
# undef USE_FIONREAD	/* because of osf screen refresh problems */
#endif

#if (BERK || AIX || AUX2) && !defined(FIONREAD)
/* try harder to get it */
# if SUNOS
#  include "sys/filio.h"
# else /* if you have trouble including ioctl.h, try "sys/ioctl.h" instead */
#  if APOLLO || AIX || OSF1 || ULTRIX || AUX2 || __NetBSD__ || __386BSD__
#   include <sys/ioctl.h>
#  else
#   include <ioctl.h>
#  endif
# endif
#endif

#if X11	/* don't use either one */
# undef USE_FCNTL
# undef USE_FIONREAD
#else
# if defined(FIONREAD)
  /* there seems to be a bug in someone's implementation of fcntl -- it
   * causes output to be flushed if you change to ndelay input while output
   * is pending.  for these systems, we use FIONREAD instead, if possible. 
   * In fact, if try and use FIONREAD in any case if present.  If you have
   * the problem with fcntl, you'll notice that the screen doesn't always
   * refresh correctly, as if the fcntl is interfering with the output drain
   */
#  undef USE_FCNTL
#  define USE_FIONREAD 1
# endif
#endif

#if USE_FCNTL
/* this is used to determine whether input is pending from the user */
#include	<fcntl.h>
int kbd_flags;			/* saved keyboard fcntl flags	*/
int kbd_is_polled;		/* in O_NDELAY mode?		*/
int kbd_char_present;		/* there is a char in kbd_char	*/
char kbd_char;			/* the char we've already read	*/
#endif

#define SMALL_STDOUT 1

#if defined(SMALL_STDOUT) && (defined (USE_FCNTL) || defined(USE_FIONREAD))
#define	TBUFSIZ	128 /* Provide a smaller terminal output buffer so that
	   the type-ahead detection works better (more often).
	   That is, we overlap screen writing with more keyboard polling */
#else
#define	TBUFSIZ	1024	/* reduces the number of writes */
#endif

extern CMDFUNC f_backchar;
extern CMDFUNC f_backchar_to_bol;


#if USE_POSIX_TERMIOS

#include <termios.h>

struct termios otermios, ntermios;

char tobuf[TBUFSIZ];		/* terminal output buffer */

void
ttopen()
{
	int s;
	s = tcgetattr(0, &otermios);
	if (s < 0) {
		perror("ttopen tcgetattr");
		ExitProgram(BAD(1));
	}
 /* the "|| USG" below is to support SVR4 -- let me know if
 	it conflicts on other systems */
#if ODT || ISC || USG
	setvbuf(stdout, tobuf, _IOLBF, TBUFSIZ);
#else
  	setbuffer(stdout, tobuf, TBUFSIZ);
#endif

	suspc =   otermios.c_cc[VSUSP];
	intrc =   otermios.c_cc[VINTR];
	killc =   otermios.c_cc[VKILL];
	startc =  otermios.c_cc[VSTART];
	stopc =   otermios.c_cc[VSTOP];
	backspc = otermios.c_cc[VERASE];
#ifdef VWERASE  /* Sun has it.  any others? */
	wkillc = otermios.c_cc[VWERASE];
#else
	wkillc =  tocntrl('W');
#endif

	/* this could probably be done more POSIX'ish? */
	(void)signal(SIGTSTP,SIG_DFL);		/* set signals so that we can */
	(void)signal(SIGCONT,rtfrmshell);	/* suspend & restart */
	(void)signal(SIGTTOU,SIG_IGN);		/* ignore output prevention */

#if USE_FCNTL
	kbd_flags = fcntl( 0, F_GETFL, 0 );
	kbd_is_polled = FALSE;
#endif

#if ! X11
	ntermios = otermios;

	/* setup new settings, preserve flow control, and allow BREAK */
	ntermios.c_iflag = BRKINT|(otermios.c_iflag & (IXON|IXANY|IXOFF));
	ntermios.c_oflag = 0;
	ntermios.c_lflag = ISIG;
	ntermios.c_cc[VMIN] = 1;
	ntermios.c_cc[VTIME] = 0;
#ifdef	VSWTCH
	ntermios.c_cc[VSWTCH] = -1;
#endif
	ntermios.c_cc[VSUSP] = -1;
	ntermios.c_cc[VSTART] = -1;
	ntermios.c_cc[VSTOP] = -1;
#endif

	ttmiscinit();

	ttunclean();


}

void
ttclose()
{
	ttclean(TRUE);
}

/*ARGSUSED*/
void
ttclean(f)
int f;
{
#if !X11
	if (f) {
		movecursor(term.t_nrow, ttcol); /* don't care about column */
		TTputc('\n');
		TTputc('\r');
	}
	(void)fflush(stdout);
#if ! LINUX
	tcdrain(1);
#endif
	tcsetattr(0, TCSADRAIN, &otermios);
	TTkclose();	/* xterm */
	TTclose();
	TTflush();
#if USE_FCNTL
	fcntl(0, F_SETFL, kbd_flags);
	kbd_is_polled = FALSE;
#endif
#endif
}

void
ttunclean()
{
#if ! X11
#if ! LINUX
	tcdrain(1);
#endif
	tcsetattr(0, TCSADRAIN, &ntermios);
#endif
	TTkopen();	/* xterm */
}


#endif /* USE_POSIX_TERMIOS */

#if USE_TERMIO

#include	<termio.h>

/* original terminal characteristics and characteristics to use inside */
struct	termio	otermio, ntermio;

#ifdef AVAILABLE  /* setbuffer() isn't on most termio systems */
char tobuf[TBUFSIZ];		/* terminal output buffer */
#endif

void
ttopen()
{

	ioctl(0, TCGETA, (char *)&otermio);	/* save old settings */
#ifdef AVAILABLE
	setbuffer(stdout, tobuf, TBUFSIZ);
#endif

	intrc =   otermio.c_cc[VINTR];
	killc =   otermio.c_cc[VKILL];
	startc =  tocntrl('Q');
	stopc =   tocntrl('S');
	backspc = otermio.c_cc[VERASE];
	wkillc =  tocntrl('W');

#if USE_FCNTL
	kbd_flags = fcntl( 0, F_GETFL, 0 );
	kbd_is_polled = FALSE;
#endif

#if SIGTSTP
/* be careful here -- VSUSP is sometimes out of the range of the c_cc array */
# ifdef VSUSP /* ODT (all POSIX?) uses this... */
	ntermio.c_cc[VSUSP] = -1;
	ntermio.c_cc[VSTART] = -1;
	ntermio.c_cc[VSTOP] = -1;
	suspc =   otermio.c_cc[VSUSP];
# else /* use V_SUSP */
#  ifdef V_SUSP
	ntermio.c_cc[V_SUSP] = -1;
	suspc = otermio.c_cc[V_SUSP];
#  else
	suspc = -1;
#  endif
#  ifdef V_DSUSP
	ntermio.c_cc[V_DSUSP] = -1;
#  endif
# endif

	(void)signal(SIGTSTP,SIG_DFL);		/* set signals so that we can */
	(void)signal(SIGCONT,rtfrmshell);	/* suspend & restart */
	(void)signal(SIGTTOU,SIG_IGN);		/* ignore output prevention */

#else /* no SIGTSTP */
	suspc =   tocntrl('Z');
#endif

#if ! X11
	ntermio = otermio;

	/* setup new settings, preserve flow control, and allow BREAK */
	ntermio.c_iflag = BRKINT|(otermio.c_iflag & IXON|IXANY|IXOFF);
	ntermio.c_oflag = 0;
	ntermio.c_lflag = ISIG;
	ntermio.c_cc[VMIN] = 1;
	ntermio.c_cc[VTIME] = 0;
#ifdef	VSWTCH
	ntermio.c_cc[VSWTCH] = -1;
#endif
#endif

	ttmiscinit();
	ttunclean();

}

void
ttclose()
{
	ttclean(TRUE);
}

void
ttclean(f)
int f;
{
#if ! X11
	if (f) {
		movecursor(term.t_nrow, ttcol); /* don't care about column */
		TTputc('\n');
		TTputc('\r');
	}
	(void)fflush(stdout);
	TTkclose();	/* xterm */
	TTflush();
	TTclose();
	ioctl(0, TCSETAF, (char *)&otermio);
#if USE_FCNTL
	fcntl(0, F_SETFL, kbd_flags);
	kbd_is_polled = FALSE;
#endif
#endif	/* X11 */
}

void
ttunclean()
{
#if ! X11
	ioctl(0, TCSETAW, (char *)&ntermio);
#endif
	TTkopen();	/* xterm */
}

#endif /* USE_TERMIO */

#if USE_SGTTY

#if USE_FIONREAD
char tobuf[TBUFSIZ];		/* terminal output buffer */
#endif

#undef	CTRL
#include        <sgtty.h>        /* for stty/gtty functions */
struct  sgttyb  ostate;          /* saved tty state */
struct  sgttyb  nstate;          /* values for editor mode */
struct  sgttyb  rnstate;          /* values for raw editor mode */
int olstate;		/* Saved local mode values */
int nlstate;		/* new local mode values */
struct ltchars	oltchars;	/* Saved terminal special character set */
struct ltchars	nltchars = { -1, -1, -1, -1, -1, -1 }; /* a lot of nothing */
struct tchars	otchars;	/* Saved terminal special character set */
struct tchars	ntchars; /*  = { -1, -1, -1, -1, -1, -1 }; */

void
ttopen()
{
	ioctl(0,TIOCGETP,(char *)&ostate); /* save old state */
	killc = ostate.sg_kill;
	backspc = ostate.sg_erase;

#if ! X11
	nstate = ostate;
        nstate.sg_flags |= CBREAK;
        nstate.sg_flags &= ~(ECHO|CRMOD);       /* no echo for now... */
	ioctl(0,TIOCSETN,(char *)&nstate); /* set new state */
#endif

	rnstate = nstate;
        rnstate.sg_flags &= ~CBREAK;
        rnstate.sg_flags |= RAW;

	ioctl(0, TIOCGETC, (char *)&otchars);	/* Save old characters */
	intrc =  otchars.t_intrc;
	startc = otchars.t_startc;
	stopc =  otchars.t_stopc;

#if ! X11
	ntchars = otchars;
	ntchars.t_brkc = -1;
	ntchars.t_eofc = -1;
	ioctl(0, TIOCSETC, (char *)&ntchars);	/* Place new character into K */
#endif

	ioctl(0, TIOCGLTC, (char *)&oltchars);	/* Save old characters */
	wkillc = oltchars.t_werasc;
	suspc = oltchars.t_suspc;
#if ! X11
	ioctl(0, TIOCSLTC, (char *)&nltchars);	/* Place new character into K */
#endif

#ifdef	TIOCLGET
	ioctl(0, TIOCLGET, (char *)&olstate);
#if ! X11
	nlstate = olstate;
	nlstate |= LLITOUT;
	ioctl(0, TIOCLSET, (char *)&nlstate);
#endif
#endif
#if USE_FIONREAD
	setbuffer(stdout, tobuf, TBUFSIZ);
#endif
#if ! X11

	(void)signal(SIGTSTP,SIG_DFL);		/* set signals so that we can */
	(void)signal(SIGCONT,rtfrmshell);	/* suspend & restart */
	(void)signal(SIGTTOU,SIG_IGN);		/* ignore output prevention */

#endif

	ttmiscinit();
}

void
ttclose()
{
	ttclean(TRUE);
}

void
ttclean(f)
int f;
{
#if ! X11
	if (f) {
		movecursor(term.t_nrow, ttcol); /* don't care about column */
		TTputc('\n');
		TTputc('\r');
	}
	TTflush();
	TTkclose();	/* xterm */
	TTclose();
	ioctl(0, TIOCSETN, (char *)&ostate);
	ioctl(0, TIOCSETC, (char *)&otchars);
	ioctl(0, TIOCSLTC, (char *)&oltchars);
#ifdef	TIOCLSET
	ioctl(0, TIOCLSET, (char *)&olstate);
#endif
#if APOLLO
	TTflush();
#endif
#endif
}

void
ttunclean()
{
#if ! X11
#if APOLLO
	int literal = LLITOUT;

	(void)fflush(stdout);
	ioctl(0, TIOCLSET, (caddr_t)&olstate);
	ioctl(0, TIOCSETP, (caddr_t)&nstate);	/* setting nlstate changes sb_flags */
	TTflush();
	ioctl(0, TIOCLBIS, (caddr_t)&literal);	/* set this before nltchars! */
	ioctl(0, TIOCSETC, (caddr_t)&ntchars);
	ioctl(0, TIOCSLTC, (caddr_t)&nltchars);

#else

	ioctl(0, TIOCSETN, (char *)&nstate);
	ioctl(0, TIOCSETC, (char *)&ntchars);
	ioctl(0, TIOCSLTC, (char *)&nltchars);
#ifdef	TIOCLSET
	ioctl(0, TIOCLSET, (char *)&nlstate);
#endif

#endif	/* APOLLO */
	TTkopen();	/* xterm */
#endif	/* !X11 */
}

#endif /* USE_SGTTY */

void
ttputc(c)
int c;
{
        (void)putchar(c);
}

void
ttflush()
{
        (void)fflush(stdout);
}

/*
 * Read a character from the terminal, performing no editing and doing no echo
 * at all.
 */
int
ttgetc()
{
#if	USE_FCNTL
	int n;
	if( kbd_char_present ) {
		kbd_char_present = FALSE;
	} else {
		if( kbd_is_polled && fcntl( 0, F_SETFL, kbd_flags ) < 0 )
			imdying(2);
		kbd_is_polled = FALSE;
		n = read(0, &kbd_char, 1);
		if (n <= 0) {
			if (n < 0 && errno == EINTR)
				return -1;
			imdying(2);
		}
	}
	return ( kbd_char );
#else /* USE_FCNTL */
#if APOLLO
	/*
	 * If we try to read a ^C in cooked mode it will echo anyway.  Also,
	 * the 'getchar()' won't be interruptable.  Setting raw-mode
	 * temporarily here still allows the program to be interrupted when we
	 * are not actually looking for a character.
	 */
	int	c;
	ioctl(0, TIOCSETN, (char *)&rnstate);
        c = getchar();
	ioctl(0, TIOCSETN, (char *)&nstate);
#else
	int c;
	c = getchar();
#endif
	if (c == EOF) {
		if (errno == EINTR)
			return -1;
		imdying(2);
	}
	return c;
#endif
}


/* typahead:	Check to see if any characters are already in the
		keyboard buffer
*/
int
typahead()
{
#if X11
#if BEFORE
this seems to think there is something ready more often than there really is
	return x_key_events_ready();
#else
	return x_is_pasting();
#endif
#else

	if (tungotc > 0)
		return TRUE;

# if	USE_FIONREAD
	{
	long x;
	return((ioctl(0,FIONREAD,(caddr_t)&x) < 0) ? 0 : (int)x);
	}
# else
#  if	USE_FCNTL
	if( !kbd_char_present )
	{
		if( !kbd_is_polled &&
				fcntl(0, F_SETFL, kbd_flags|O_NDELAY ) < 0 )
			return(FALSE);
		kbd_is_polled = TRUE;  /* I think */
		kbd_char_present = (1 == read( 0, &kbd_char, 1 ));
	}
	return ( kbd_char_present );
#  else
	return FALSE;
#  endif/* USE_FCNTL */
# endif/* USE_FIONREAD */
#endif	/* X11 */
}

/* this takes care of some stuff that's common across all ttopen's.  Some of
	it should arguably be somewhere else, but... */
void
ttmiscinit()
{
	/* make sure backspace is bound to backspace */
	asciitbl[backspc] = &f_backchar_to_bol;

	/* no buffering on input */
	setbuf(stdin, (char *)0);
}

#else /* not UNIX */

#if   MSDOS && (TURBO || MSC || WATCOM)
#include <conio.h>
#endif

#if GO32
#include <gppconio.h>
#endif

#if     AMIGA
#define NEW 1006L
#define AMG_MAXBUF      1024L
static long terminal;
static char     scrn_tmp[AMG_MAXBUF+1];
static long     scrn_tmp_p = 0;
#endif

#if ST520 && MEGAMAX
#include <osbind.h>
	int STscancode = 0;	
#endif

#if     VMS
#include        <stsdef.h>
#include        <ssdef.h>
#include        <descrip.h>
#include        <iodef.h>
#include        <ttdef.h>
#include	<tt2def.h>

#define NIBUF   128                     /* Input buffer size            */
#define NOBUF   1024			/* MM says big buffers win!	*/
#define EFN     0			/* Event flag			*/

char	obuf[NOBUF];			/* Output buffer		*/
int	nobuf;				/* # of bytes in above		*/
char	ibuf[NIBUF];			/* Input buffer			*/
int	nibuf;				/* # of bytes in above		*/
int	ibufi;				/* Read index			*/
int	oldmode[3];			/* Old TTY mode bits		*/
int	newmode[3];			/* New TTY mode bits		*/
short	iochan;				/* TTY I/O channel		*/
#endif

#if     CPM
#include        <bdos.h>
#endif

#if     MSDOS && ((!MSC && NEWDOSCC) || LATTICE || AZTEC)
union REGS rg;		/* cpu register for use of DOS calls (ibmpc.c) */
int nxtchar = -1;	/* character held from type ahead    */
#endif

extern CMDFUNC f_backchar;
extern CMDFUNC f_backchar_to_bol;

void
ttopen()
{
#if     AMIGA
	char oline[NSTRING];
#if	AZTEC
	extern	Enable_Abort;	/* Turn off ctrl-C interrupt */

	Enable_Abort = 0;	/* for the Manx compiler */
#endif
	strcpy(oline, "RAW:0/0/640/200/");
	strcat(oline, PROGNAME);
	strcat(oline, " ");
	strcat(oline, VERSION);
	strcat(oline, "/Amiga");
        terminal = Open(oline, NEW);
#endif
#if     VMS
        struct  dsc$descriptor  idsc;
        struct  dsc$descriptor  odsc;
        char    oname[40];
        int     iosb[2];
        int     status;

        odsc.dsc$a_pointer = "TT";
        odsc.dsc$w_length  = strlen(odsc.dsc$a_pointer);
        odsc.dsc$b_dtype        = DSC$K_DTYPE_T;
        odsc.dsc$b_class        = DSC$K_CLASS_S;
        idsc.dsc$b_dtype        = DSC$K_DTYPE_T;
        idsc.dsc$b_class        = DSC$K_CLASS_S;
        do {
                idsc.dsc$a_pointer = odsc.dsc$a_pointer;
                idsc.dsc$w_length  = odsc.dsc$w_length;
                odsc.dsc$a_pointer = &oname[0];
                odsc.dsc$w_length  = sizeof(oname);
                status = LIB$SYS_TRNLOG(&idsc, &odsc.dsc$w_length, &odsc);
                if (status!=SS$_NORMAL && status!=SS$_NOTRAN)
			ExitProgram(status);
                if (oname[0] == 0x1B) {
                        odsc.dsc$a_pointer += 4;
                        odsc.dsc$w_length  -= 4;
                }
        } while (status == SS$_NORMAL);
        status = SYS$ASSIGN(&odsc, &iochan, 0, 0);
        if (status != SS$_NORMAL)
		ExitProgram(status);
        status = SYS$QIOW(EFN, iochan, IO$_SENSEMODE, iosb, 0, 0,
                          oldmode, sizeof(oldmode), 0, 0, 0, 0);
        if (status!=SS$_NORMAL || (iosb[0]&0xFFFF)!=SS$_NORMAL)
		ExitProgram(status);
        newmode[0] = oldmode[0];
        newmode[1] = oldmode[1] | TT$M_NOECHO;
        newmode[1] &= ~(TT$M_TTSYNC|TT$M_HOSTSYNC);
        newmode[2] = oldmode[2] | TT2$M_PASTHRU;
        status = SYS$QIOW(EFN, iochan, IO$_SETMODE, iosb, 0, 0,
                          newmode, sizeof(newmode), 0, 0, 0, 0);
        if (status!=SS$_NORMAL || (iosb[0]&0xFFFF)!=SS$_NORMAL)
		ExitProgram(status);
        term.t_nrow = (newmode[1]>>24) - 1;
        term.t_ncol = newmode[0]>>16;

#endif
#if     CPM
#endif

#if     MSDOS && (HP150 == 0) && LATTICE
	/* kill the ctrl-break interrupt */
	rg.h.ah = 0x33;		/* control-break check dos call */
	rg.h.al = 1;		/* set the current state */
	rg.h.dl = 0;		/* set it OFF */
	intdos(&rg, &rg);	/* go for it! */
#endif

	/* make sure backspace is bound to backspace */
	asciitbl[backspc] = &f_backchar_to_bol;

}

void
ttclose()
{
#if     AMIGA
#if	LATTICE
        amg_flush();
        Close(terminal);
#endif
#if	AZTEC
        amg_flush();
	Enable_Abort = 1;	/* Fix for Manx */
        Close(terminal);
#endif
#endif

#if     VMS
        int     status;
        int     iosb[2];

        ttflush();
        status = SYS$QIOW(EFN, iochan, IO$_SETMODE, iosb, 0, 0,
                 oldmode, sizeof(oldmode), 0, 0, 0, 0);
	if (status == SS$_IVCHAN)
		return;	/* already closed it */
        if (status!=SS$_NORMAL || (iosb[0]&0xFFFF)!=SS$_NORMAL)
		ExitProgram(status);
        status = SYS$DASSGN(iochan);
        if (status != SS$_NORMAL)
		ExitProgram(status);
#endif
#if     MSDOS && (HP150 == 0) && LATTICE
	/* restore the ctrl-break interrupt */
	rg.h.ah = 0x33;		/* control-break check dos call */
	rg.h.al = 1;		/* set the current state */
	rg.h.dl = 1;		/* set it ON */
	intdos(&rg, &rg);	/* go for it! */
#endif
#if	!VMS
	ttclean(TRUE);
#endif
}

void
ttclean(f)
int f;
{
	if (f) {
		movecursor(term.t_nrow, ttcol); /* don't care about column */
		TTputc('\n');
		TTputc('\r');
	}
	TTflush();
	TTclose();
	TTkclose();
}

void
ttunclean()
{
	TTkopen();	/* xterm */
}

/*
 * Write a character to the display. On VMS, terminal output is buffered, and
 * we just put the characters in the big array, after checking for overflow.
 * On CPM terminal I/O unbuffered, so we just write the byte out. Ditto on
 * MS-DOS (use the very very raw console output routine).
 */
void
ttputc(c)
int c;
{
#if     AMIGA
        scrn_tmp[scrn_tmp_p++] = c;
        if(scrn_tmp_p>=AMG_MAXBUF)
                amg_flush();
#endif
#if	ST520 && MEGAMAX
	Bconout(2,c);
#endif
#if     VMS
        if (nobuf >= NOBUF)
                ttflush();
        obuf[nobuf++] = c;
#endif
#if     CPM
        bios(BCONOUT, c, 0);
#endif
#if	MSDOS && !IBMPC
#if     MWC86
	putcnb(c);
#else	/* LATTICE || AZTEC || TURBO */
	bdos(6, c, 0);
#endif
#endif	/* MSDOS && !IBMPC */
}

#if	AMIGA
amg_flush()
{
        if(scrn_tmp_p)
                Write(terminal,scrn_tmp,scrn_tmp_p);
        scrn_tmp_p = 0;
}
#endif

/*
 * Flush terminal buffer. Does real work where the terminal output is buffered
 * up. A no-operation on systems where byte at a time terminal I/O is done.
 */
void
ttflush()
{
#if     AMIGA
        amg_flush();
#endif
#if     VMS
        int     status;
        int     iosb[2];

        status = SS$_NORMAL;
        if (nobuf != 0) {
                status = SYS$QIOW(EFN, iochan, IO$_WRITELBLK|IO$M_NOFORMAT,
                         iosb, 0, 0, obuf, nobuf, 0, 0, 0, 0);
                if (status == SS$_NORMAL)
                        status = iosb[0] & 0xFFFF;
                nobuf = 0;
        }
#if NEVER
        return (status);
#endif
#endif

#if     CPM
#endif

#if     MSDOS
#endif
}

/*
 * Read a character from the terminal, performing no editing and doing no echo
 * at all. More complex in VMS that almost anyplace else, which figures. Very
 * simple on CPM, because the system can do exactly what you want.
 */
int
ttgetc()
{
#if     AMIGA
	{
        char ch;

        amg_flush();
        Read(terminal, &ch, 1L);
        return(255 & (int)ch);
	}
#endif
#if	ST520 && MEGAMAX
	{
	long ch;

/*
 * blink the cursor only if nothing is happening, this keeps the
 * cursor on steadily during movement making it easier to track
 */
	STcurblink(TRUE);  /* the cursor blinks while we wait */
	ch = Bconin(2);
	STcurblink(FALSE); /* the cursor is steady while we work */
	STscancode = (ch >> 16) & 0xff;
       	return(255 & (int)ch);
	}
#endif
#if     VMS
	{
        int     status;
        int     iosb[2];
        int     term[2];

        while (ibufi >= nibuf) {
		term[0] =
		term[1] = 0;
		if (!typahead()) {
                        status = SYS$QIOW(EFN, iochan, IO$_READLBLK,
                                 iosb, 0, 0, ibuf, 1, 0, term, 0, 0);
                        if (status != SS$_NORMAL
                        || (status = (iosb[0]&0xFFFF)) != SS$_NORMAL)
				ExitProgram(status);
                        nibuf = (iosb[0]>>16) + (iosb[1]>>16);
                }
        }
        return (ibuf[ibufi++] & 0xFF);    /* Allow multinational  */

	}
#endif
#if     CPM
	{
        return (biosb(BCONIN, 0, 0));

	}
#endif

#if MSDOS
	/*
	 * If we've got a mouse, poll waiting for mouse movement and mouse
	 * clicks until we've got a character to return.
	 */
# if OPT_MS_MOUSE
	if (ms_exists()) {
		for(;;) {
			if (typahead())
				break;
			ms_processing();
		}
	}
# endif /* OPT_MS_MOUSE */
#if     MWC86
	return getcnb();
#endif
#if	MSC || TURBO
	return getch();
#endif
#if	((!(MSC||TURBO) && NEWDOSCC) || LATTICE || AZTEC)
	{
	int c;

	/* if a char already is ready, return it */
	if (nxtchar >= 0) {
		c = nxtchar;
		nxtchar = -1;
		return(c);
	}

	/* call the dos to get a char */
	rg.h.ah = 7;		/* dos Direct Console Input call */
	intdos(&rg, &rg);
	c = rg.h.al;		/* grab the char */
	return(c & 0xff);
	}
#endif
#endif	/* MSDOS */

}


/* typahead:	Check to see if any characters are already in the
		keyboard buffer
*/
int
typahead()
{
	if (tungotc > 0)
		return TRUE;

#if	X11
	return x_is_pasting();
#endif

#if	VMSVT
	if (ibufi >= nibuf) {
		int	status,
			iosb[2],
			term[2];

                ibufi = 0;
                term[0] = 0;
                term[1] = 0;

                status = SYS$QIOW(EFN, iochan, IO$_READLBLK|IO$M_TIMED,
                         iosb, 0, 0, ibuf, NIBUF, 0, term, 0, 0);
                if (status != SS$_NORMAL)
			ExitProgram(status);
                status = iosb[0] & 0xFFFF;
                if (status!=SS$_NORMAL && status!=SS$_TIMEOUT)
			ExitProgram(status);
                nibuf = (iosb[0]>>16) + (iosb[1]>>16);
                return (nibuf > 0);
	}
	return TRUE;
#endif

#if	MSDOS && NEWDOSCC
	return (kbhit() != 0);
#endif

#if	MSDOS && (LATTICE || AZTEC || MWC86)
	int c;		/* character read */
	int flags;	/* cpu flags from dos call */

	if (nxtchar >= 0)
		return(TRUE);

	rg.h.ah = 6;	/* Direct Console I/O call */
	rg.h.dl = 255;	/*         does console input */
#if	LATTICE || AZTEC
	flags = intdos(&rg, &rg);
#else
	intcall(&rg, &rg, 0x21);
	flags = rg.x.flags;
#endif
	c = rg.h.al;	/* grab the character */

	/* no character pending */
	if ((flags & 0x40) != 0)
		return(FALSE);

	/* save the character and return true */
	nxtchar = c;
	return(TRUE);
#endif
}

#endif /* not UNIX */
