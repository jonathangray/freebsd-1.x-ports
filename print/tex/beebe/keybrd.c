/***********************************************************************

This package  implements a  simple keyboard  input interface  to  permit
character-at-a-time  input,  plus  optional  suppression  of   character
translation.

This is one of the areas where operating systems (even variants of Unix)
are horribly  non-standard,  and the  problem  is not  really  addressed
satisfactorily by either the emerging draft ANSI C Standard (01-Oct-86),
or the IEEE Trial Use  Standard: Portable Operating System for  Computer
Environments (POSIX), IEEE Std 1003.1, (14-Apr-86).

This package is intentionally simple--only one keyboard input device  is
supported (NOT Unix stdin!), and where possible, it is implemented  with
rather ordinary Unix library calls.  The functionality provided is open,
close, flush,  get, unget,  set-mode, and  inquire-mode.  All  functions
(except kbfile()) use the stdio.h EOF value to signal an error, and none
set the global  errno flag (that  is not the  prerogative of  non-system
software).

User programs  need  only use  a  '#include "key.h"'  statement  to  get
definitions of all visible flags, functions, and types in this  package.
Global data  which is  private to  this package  is declared  static  to
ensure just that.

The mode parameter set in calls  to kbopen() and kbmode(), and  returned
by  them,  is  simply  a  collection  of  bit  flags,  but  for   future
expandability, it has a special type  KEYMODE, instead of being an  int.
The mode flags require some explanation.  They are based on the terminal
I/O model  in  Berkeley  Unix.  Essentially  three  terminal  modes  are
supported: normal,  cbreak, and  raw.

In normal mode,  keyboard input is  handled like fopen()  / getchar()  /
fclose(), so that input editing characters are supported (and handled by
the terminal driver, not this package!).  Input may be available  before
a <CR> is typed  (e.g. 4.3BSD, TOPS-20, MS-DOS),  or may not (System  V,
VMS).

In cbreak mode, input editing characters should still work, but input is
guaranteed to be  available before a  <CR> is typed,  provided that  the
operating  system  supports  that  (CDC  and  IBM  do  not).   Interrupt
characters (CTL-C,  CTL-Z, et  al) are  not readable--they  still  cause
interrupts which are  not handled here.   Other control characters  (HT,
ESC, et al) must be readable.

In raw mode, all characters in the host character set may be input,  and
no character-initiated interrupts are possible.

These three modes  are selected  by OR'ing into  the mode  value one  of
KB_CBREAK, KB_NORMAL, or  KB_RAW; they  are mutually  exclusive, and  an
error is raised  if more  than one is  specified.

The current mode may be  saved (internally) with KB_SAVE, restored  with
KB_RESTORE, or inquired  with KB_INQUIRE;  any other  KB_xxxx flags  set
with these are ignored, and the  current mode is not changed.  Only  one
level of save/restore is  provided; successive restore requests  without
an intervening save are permissible, and simply restore the saved  mode.
The intent is that save/restore normally only be done once at job  start
and end.

Normally, input  characters  are not  echoed  to the  terminal;  if  the
side-effect of echoing is wanted, it must be requested by KB_ECHO.

The effect  of  these  modes  on  input  (and  output)  via  other  file
descriptors is operating-system dependent.  On Unix, the terminal  modes
are job-wide, and persist after a program terminates.  On MS-DOS and VAX
VMS, they affect terminal I/O, but only for the duration of the job, and
only for  that file  descriptor.   On TOPS-20,  some  of the  modes  can
persist after the job (e.g.  terminal width and pause-on-end-of-page).

All functions return the EOF error indicator if the keyboard file is not
open.  This  can  be done  with  an  initial call  to  kbopen(mode);  on
success, it returns the argument mode value.

When processing is complete, kbclose() should be called.  Further  calls
of the  kbxxxx() functions  will return  error codes  until kbopen()  is
called again.  kbclose() must restore the  terminal to the state it  was
in when  kbopen() was  called;  kbmode() does  not necessarily  do  that
(since the default mode for kbopen() is no echo).

kbget()  and  kbunget(c)   provide  for   single-character  input,   and
single-character push-back.   kbget()  returns  a  non-negative  ordinal
value on success, and EOF on error.  The pushed-back character given  to
kbunget(c) need  not be  one obtained  from the  last call  to  kbget().
Attempts to push back more than one character are ignored, and result in
an EOF  error return.	Depending  on the  mode (raw,  cbreak,  normal),
kbget() may not be able to return an end-of-file indicator; it cannot in
raw mode, but for  portability, EOF returns from  kbget() should not  be
expected.

kbflush() simply flushes typeahead.  It returns EOF only if the keyboard
file is not open; otherwise it returns 0.

In many applications, it  is often important to  have the capability  of
determining whether or nor  input is available.   For example, a  screen
display algorithm probably should suspend  display if input is  awaiting
that is  likely  to change  the  state  of the  screen.   The  kbinput()
function is therefore  an important  one; it returns  0 if  no input  is
available to be  read by kbget(),  returns EOF on  error, and  otherwise
returns a  positive number  which  in some  implementations may  be  the
number of unread input characters, and in others may be arbitrary.  This
function is  one  which is  difficult  to implement  on  some  operating
systems, with Unix System III and V being the worst.  The implementation
below for the latter  is borrowed from  Marc Rochkind's excellent  book,
"Advanced Unix Programming", Prentice-Hall (1985).

There is one  other function  provided, kbfile(); it  returns the  FILE*
pointer for the  keyboard file, or  (FILE*)NULL is none  is open, or  if
this package is not implemented with  standard C I/O routines.  Its  use
is strongly discouraged,  but the author's  long programming  experience
has shown that on rare occasions, it may be useful.  Character push-back
with direct  calls  to  ungetc(c,kbfile()) will  probably  not  work  as
expected, but other stdio functions should.  fopen() and fclose() should
not be called with the kbfile() value; that would corrupt internal  data
structures here.

Most  functions   are  necessarily   internally  system-dependent,   and
preprocessor conditionals  encapsulate the  gory details.   Preprocessor
flags are  as  follows;  at  least  one  implementation  flag,  and  one
compatible operating system flag, must be chosen:

C Implementations:
	ATT		-- AT&T Unix (System III, V)
	BSD		-- Berkeley 4.xBSD
	HPUX		-- HP 9000 series Unix (System V based)
	IBM_PC_MICROSOFT-- IBM PC Microsoft Version 3.x or later C compiler
	KCC_20		-- SRI's KCC Compiler on TOPS-20
	PCC_20		-- Portable C Compiler on TOPS-20

Operating systems:
	OS_PCDOS	-- IBM (and clones) PC DOS and MS DOS
	OS_TOPS20	-- DEC-20 TOPS-20
	OS_UNIX		-- Unix (almost any variant)
	OS_VAXVMS	-- VAX VMS

Testing of this package has been carried on TOPS-20 (KCC-20 and  PCC-20)
on DEC-20/60, , VAX VMS (CC) on VAX 8600, 4.3BSD Unix on VAX 8600,  HPUX
on HP 9000, and PC DOS (IBM  PC XT).  It appears to work  satisfactorily
on all of these, with the single exception that CTL-C handling on PC DOS
is not restored  after a switch  from raw mode;  under all modes,  CTL-C
still echoes  as  ^C<CR><LF>.	Using  DOS  interrupt  0x33  instead  of
signal() to save and  restore the ctl-break status  does not provide  an
acceptable alternative; CTL-C always terminates execution.  Further work
is necessary to resolve this.

[08-Apr-87]
***********************************************************************/

#include <stdio.h>
#include "keydef.h"

#ifndef OS_PCDOS
#define OS_PCDOS 0
#endif

#ifndef IBM_PC_MICROSOFT
#define IBM_PC_MICROSOFT 0
#endif

#ifndef OS_TOPS20
#define OS_TOPS20 0
#endif

#ifndef KCC_20
#define KCC_20 0
#endif

#ifndef PCC_20
#define PCC_20 0
#endif

#ifndef OS_UNIX
#define OS_UNIX 0
#endif

#ifndef ATT
#define ATT 0
#endif

#ifndef BSD
#define BSD 0
#endif

#ifndef HPUX
#define HPUX 0
#endif

#ifndef OS_VAXVMS
#define OS_VAXVMS 0
#endif

#if    OS_TOPS20
#if    (KCC_20 | PCC_20)
#else
#undef PCC_20
#define PCC_20 1			/* PCC-20 is default for Tops-20 */
#endif
#endif

#if    (OS_PCDOS | OS_TOPS20 | OS_UNIX | OS_VAXVMS)
#else
#undef OS_UNIX
#define OS_UNIX 1			/* Unix is default operating system */
#endif

#if    (ATT | BSD | HPUX)
#else
#undef BSD
#define BSD 1				/* BSD is default Unix variant */
#endif

#if    OS_PCDOS
#include <conio.h>
#include <signal.h>

#define TTYOPENFLAGS "rb"
#define TTYNAME "con"

#define interrupt catch_int /* MSC Version 5.0 raises syntax error otherwise */
static int (*signal_status)();
static void interrupt();
#endif /* OS_PCDOS */

#if    OS_TOPS20
#define TTYNAME "TTY:"
static int w_rfmod;			/* terminal mode word */
static int w_rpcap;			/* process capability word */
static int w_rtiw;			/* terminal interrupt word */
static int w_morxo;			/* end-of-page mode */
static int kbrestore();			/* restore terminal settings */
/*
** KCC-20 and PCC-20 have  similar enough JSYS  interfaces that we  just
** use KCC symbolic names,  and then redefine  them for PCC-20.   PCC-20
** has a working ioctl()  that could be  used for Unix-style  processing
** too, although it has a bug in that it resets the terminal width to 0,
** and disables pause at end of page.  It therefore seems easier to  use
** direct Monitor calls both both KCC-20 and PCC-20.
*/

#if    KCC_20
#include <jsys.h>
#include <monsym.h>
#define jfnof(x)	_CTTRM		/* we use direct JSYS'es for now */
#define TTYASCMOD	FLD(_TTASC,TT_DAM)
#define TTYBINMOD	FLD(_TTBIN,TT_DAM)
#define TTYDAM		FLD(-1,TT_DAM)
#define TTYECHO		FLD(-1,TT_ECO)
#define TTYOPENFLAGS	"rb8"
#define TTYPGM		FLD(-1,TT_PGM)
#endif /* KCC_20 */

#if    PCC_20
#include <file.h>
#include <monsym.h>
#undef jfnof
#define jfnof(x) _CTTRM			/* we use direct JSYS'es for now */
/* Convert names in better KCC-20 style to PCC-20 style */
#define BIN		JSbin
#define DOBE		JSdobe
#define MTOPR		JSmtopr
#define RFMOD		JSrfmod
#define RPCAP		JSrpcap
#define RTIW		JSrtiw
#define SC_CTC		makefield(SC_ctc,-1)	/* WARNING: PCC bug--see below */
#define SFMOD		JSsfmod
#define SIBE		JSsibe
#define STIW		JSstiw
#define STPAR		JSstpar
#define TTYASCMOD	makefield(TT_dam,TTasc)
#define TTYBINMOD	makefield(TT_dam,TTbin)
#define TTYDAM		makefield(TT_dam,-1)
#define TTYPGM		makefield(TT_pgm,-1)
#define TTYECHO		makefield(TT_eco,-1)
#define TTYOPENFLAGS	"rb"
#define _CTTRM		CTtrm
#define _FHJOB		0777773
#define _FHSLF		FHslf
#define _MOXOF		MOxof
#define _MOOFF		MOoff
#define _MORXO		MOrxo
#endif /* PCC_20 */

#endif /* OS_TOPS20 */

#if    OS_UNIX

#if    (ATT | HPUX)
typedef int BOOLEAN;
#define FALSE 0
#define TRUE 1

#include <fcntl.h>
#include <termio.h>

static struct termio tty;
static struct termio ttysave;
static int setblock();
#endif

#if    BSD
#include <sgtty.h>
#include <sys/ioctl.h>

static struct sgttyb tty;
static struct sgttyb ttysave;
#endif

#define TTYOPENFLAGS "r"
#define TTYNAME "/dev/tty"
#endif /* OS_UNIX */

#if    OS_VAXVMS
#include <ssdef.h>
#include <descrip.h>
#include <iodef.h>
#include <ttdef.h>
#include <tt2def.h>

#define TTYOPENFLAGS "rb"
#define TTYNAME ctermid((char*)NULL)
static int status;			/* system service status */
static int tt_channel = -1;		/* terminal channel for image QIO's */
static int iomask;			/* QIO flag mask */
static $DESCRIPTOR(sys_in,"TT:");	/* terminal descriptor */

static struct
{
    unsigned char class;
    unsigned char type;
    unsigned short buffer_size;
    unsigned long tt;
    unsigned long tt2;
} mode_buf,mode_save;

static struct
{
    unsigned short typeahead_count;
    unsigned char first_char;
    unsigned char reserved[5];
} typeahead_buf;

#define FAILED(status) (~(status) & 1)	/* failure if LSB is 0 */

#endif /* OS_VAXVMS */

/*====================================================================*/

#define BADMODE ((KEYMODE)(EOF))	/* unset mode value */
#define EMPTY 32767			/* flag for empty pushback buffer */

static KEYMODE kbcurrmode = BADMODE;	/* current mode */
static KEYMODE kbsavedmode = BADMODE;	/* saved mode */
static int kbpushback = EMPTY;		/* character pushback buffer */
static FILE* kbfp = (FILE*)NULL;	/* keyboard file pointer */
static int kbinit = 0;			/* initialization flag */

#if    OS_PCDOS
/*====================================================================*/

static void
interrupt()
{
    if (kbpushback == EMPTY)
	kbpushback = 3;			/* CTL-C */
}
#endif /* OS_PCDOS */

/*====================================================================*/

int
kbclose()				/* close the keyboard file */
{					/* return EOF on error, 0 on success */
    if (!kbinit)
	return (EOF);
    kbinit = 0;

#if    OS_PCDOS
    (void)signal(SIGINT,signal_status);
    return (0);
#endif /* OS_PCDOS */

#if    OS_TOPS20
    return (kbrestore());
#endif /* OS_TOPS20 */


#if    OS_UNIX
#if    (ATT | HPUX)
    if (ioctl(fileno(kbfp),TCSETAF,&ttysave) == -1)
	return (EOF);
#endif /* (ATT | HPUX) */
#if    BSD
    if (ioctl(fileno(kbfp),TIOCSETP,&ttysave) == -1)
	return (EOF);
#endif /* BSD */

    return (kbfp == (FILE *)NULL ? EOF : fclose(kbfp));
#endif /* OS_UNIX */

#if    OS_VAXVMS
    status = sys$qiow(0,tt_channel,IO$_SETMODE,0,0,0, &mode_save,12,0,0,0,0);
    if (FAILED(status))
	return (EOF);
    return (0);
#endif /* OS_VAXVMS */
}

/*====================================================================*/

FILE*
kbfile()				/* return keyboard file pointer */
{
    return (kbinit ? kbfp : (FILE*)NULL);
}

/*====================================================================*/

int
kbflush()				/* flush current input buffer */
{
    if (!kbinit)
	return (EOF);
    while (kbinput() > 0)
	(void)kbget();
    return (0);
}


/*====================================================================*/

int
kbget()					/* blocking read of character */
{			/* return character, or EOF on end-of-file or error */
    int c;

    if (!kbinit)
	return (EOF);
    else if (kbpushback == EMPTY)
    {

#if    OS_PCDOS
	c = (kbcurrmode & KB_ECHO) ? getche() : getch();
#endif /* OS_PCDOS */

#if    OS_TOPS20
	ac1 = _CTTRM;
	c = ((jsys(BIN,acs) == JSok) ? ac2 : EOF);
#endif /* OS_TOPS20 */


#if    OS_UNIX
#if    (ATT | HPUX)
	if (setblock(fileno(kbfp),TRUE) == EOF)
	    return (EOF);
	if (read(fileno(kbfp),&c,1) <= 0)
	    c = EOF;
#endif /* (ATT | HPUX) */
#if    BSD
	c = getc(kbfp);
#endif /* BSD */
#endif /* OS_UNIX */

#if    OS_VAXVMS
	status = sys$qiow(0,tt_channel,iomask,0,0,0,&c,1,0,0,0,0);
	if (FAILED(status))
	    c = EOF;
#endif /* OS_VAXVMS */

    }
    else
    {
	c = kbpushback;
	kbpushback = EMPTY;
    }
    return (c == EOF ? c : (int)(0377 & c));	/* make sure it is 8-bit value */
}

/*====================================================================*/

int
kbinput()				/* input available? */
{ /* return count of available input characters, 0 if none, or EOF on error */

    if (!kbinit)
	return (EOF);
    else if (kbpushback != EMPTY)
	return (1);
    else
    {
#if    OS_PCDOS
	return (kbhit() ? 1 : 0);	/* otherwise returns 255 */
#endif /* OS_PCDOS */

#if    OS_TOPS20
	ac1 = jfnof(fileno(kbfp));
	if (jsys(SIBE,acs) == JSok)
	    return (0);			/* input buffer empty */
	else
	    return (ac2);		/* input character count */
#endif /* OS_TOPS20 */

#if    OS_UNIX
#if    (ATT | HPUX)
	{
	    char c;

	    if (setblock(fileno(kbfp),FALSE) == EOF)
		return (EOF);
	    switch (read(fileno(kbfp),&c,1))
	    {
	    case -1:
		return (EOF);

	    case 0:
		return (0);

	    default:
		kbpushback = (int)c;
		return (1);
	    }
	}
#endif /* (ATT | HPUX) */
#if    BSD
	{
	    int count;

	    return ((ioctl(fileno(kbfp), FIONREAD, &count) == -1) ? EOF : count);
	}
#endif /* BSD */
#endif /* OS_UNIX */

#if    OS_VAXVMS
	status = sys$qiow(0,tt_channel,IO$_SENSEMODE | IO$M_TYPEAHDCNT,
	    0,0,0,&typeahead_buf,8,0,0,0,0);
	return (FAILED(status) ? EOF : typeahead_buf.typeahead_count);
#endif /* OS_VAXVMS */
    }
}

/*====================================================================*/

KEYMODE
kbmode(mode)				/* keyboard mode */
KEYMODE mode;			/* return new mode on success, EOF on error */
{
    if (!kbinit)
	return (EOF);

    switch (mode & (KB_INQUIRE | KB_RESTORE | KB_SAVE))
    {
    case 0:
	break;				/* must be a set-mode operation  */

    case KB_INQUIRE:
	return (kbcurrmode);

    case KB_RESTORE:
	if (kbsavedmode = BADMODE)
	    return (BADMODE);
	else
	{
	    kbcurrmode = kbsavedmode;
	    kbcurrmode &= ~(KB_INQUIRE | KB_RESTORE | KB_SAVE);
	    return (kbmode(kbcurrmode));
	}

    case KB_SAVE:
	kbsavedmode = kbcurrmode;
	return (kbcurrmode);

    default:				/* do not allow mixed mode flags */
	return (BADMODE);
    }

#if    OS_TOPS20
    if (kbrestore() == EOF)		/* restore terminal settings */
	return (BADMODE);
#endif /* OS_TOPS20 */

    switch (mode & (KB_CBREAK | KB_NORMAL | KB_RAW))	/* set-mode request */
    {
#if    OS_PCDOS
    case KB_CBREAK:
    case KB_NORMAL:
	(void)signal(SIGINT,signal_status);
	break;

    case KB_RAW:
	(void)signal(SIGINT,interrupt);
	break;
#endif /* OS_PCDOS */

#if    OS_TOPS20
    case KB_CBREAK:
    case KB_NORMAL:
	ac1 = jfnof(fileno(kbfp));
	ac2 = w_rfmod & ~TTYECHO;	/* clear TT%ECO field (want no echo) */
	if (jsys(SFMOD,acs) != JSok)
	    return (BADMODE);
	if (jsys(STPAR,acs) != JSok)
	    return (BADMODE);
	break;

    case KB_RAW:
	/* Wait for output to flush; if the screen is at end-of-page, we
	are waiting for the user to type CTL-Q, which will have no
	effect once we enter raw mode, and the terminal will be hung! */
	ac1 = _CTTRM;
	if (jsys(DOBE,acs) != JSok)	/* dismiss until output buffer empty */
	    return (BADMODE);

	ac1 = jfnof(fileno(kbfp));
	ac2 = w_rfmod;
	ac2 &= ~(TTYECHO | TTYDAM | TTYPGM);	/* clear fields */
	ac2 |= TTYBINMOD;		/* set binary mode */
	if (jsys(SFMOD,acs) != JSok)
	    return (BADMODE);
	if (jsys(STPAR,acs) != JSok)
	    return (BADMODE);

	ac1 = _FHJOB;

#if    KCC_20
	if (w_rpcap & SC_CTC)		/* then have CTL-C capability */
#endif /* KCC_20 */
#if    PCC_20
	ac3 = 0600000000000;		/* PCC BUG: 0400000000000 becomes 0! */
	ac3 &=0200000000000;		/* so make it on-the-fly */
	if (w_rpcap & ac3)
#endif /* PCC_20 */
	    ac2 &= 077;			/* read all ctl-chars */
	else
	    ac2 &= 0040000000077;	/* read all ctl-chars but CTL-C */
	if (jsys(STIW,acs) != JSok)	/* reset terminal interrupt word */
	    return (BADMODE);

	ac1 = _CTTRM;
	ac2 = _MOXOF;
	ac3 = _MOOFF;			/* disable pause at end-of-page */
	if (jsys(MTOPR,acs) != JSok)	/* otherwise we can hang waiting for */
	    return (BADMODE);		/* a CTL-Q which is eaten as input! */

	break;
#endif /* OS_TOPS20 */

#if    OS_UNIX
#if    (ATT | HPUX)
    case KB_CBREAK:
	tty = ttysave;
	tty.c_iflag &= ~(INLCR | ICRNL | IUCLC | ISTRIP | IXON | BRKINT);
	tty.c_lflag &= ~(ECHO | ICANON);
	tty.c_cc[4] = 5;		/* MIN */
	tty.c_cc[5] = 2;		/* TIME */
	if (ioctl(fileno(kbfp),TCSETAF,&tty) == -1)
	    return (BADMODE);
	break;

    case KB_NORMAL:
	tty = ttysave;
	tty.c_lflag &= ~ECHO;
	tty.c_cc[4] = 5;		/* MIN */
	tty.c_cc[5] = 2;		/* TIME */
	if (ioctl(fileno(kbfp),TCSETAF,&tty) == -1)
	    return (BADMODE);
	break;

    case KB_RAW:
	tty = ttysave;
	tty.c_iflag &= ~(INLCR | ICRNL | IUCLC | ISTRIP | IXON | BRKINT);
	tty.c_lflag &= ~(ICANON | ISIG | ECHO);
	tty.c_cc[4] = 5;		/* MIN */
	tty.c_cc[5] = 2;		/* TIME */
	if (ioctl(fileno(kbfp),TCSETAF,&tty) == -1)
	    return (BADMODE);
	break;
#endif /* (ATT | HPUX) */

#if    BSD
    case KB_CBREAK:
	tty = ttysave;
	tty.sg_flags &= ~(CRMOD | ECHO | LCASE);
	tty.sg_flags |= CBREAK;
	if (ioctl(fileno(kbfp),TIOCSETP,&tty) == -1)
	    return (BADMODE);
	break;

    case KB_NORMAL:
	tty = ttysave;
	tty.sg_flags &= ~(CBREAK | CRMOD | ECHO | LCASE | RAW);
	if (ioctl(fileno(kbfp),TIOCSETP,&tty) == -1)
	    return (BADMODE);
	break;

    case KB_RAW:
	tty = ttysave;
	tty.sg_flags &= ~(CRMOD | ECHO | LCASE | TANDEM);
	tty.sg_flags |= (CBREAK | RAW);
	if (ioctl(fileno(kbfp),TIOCSETP,&tty) == -1)
	    return (BADMODE);

	break;
#endif /* BSD */
#endif /* OS_UNIX */

#if    OS_VAXVMS
    case KB_CBREAK:
    case KB_NORMAL:
	mode_buf = mode_save;
	mode_buf.tt &= ~TT$M_WRAP;
	status = sys$qiow(0,tt_channel,IO$_SETMODE,0,0,0,&mode_buf,12,0,0,0,0);
	if (FAILED(status))
	    return (EOF);
	iomask = IO$_TTYREADALL | IO$M_NOECHO;
	break;

    case KB_RAW:
	mode_buf = mode_save;
	mode_buf.tt &= ~(TT$M_TTSYNC | TT$M_WRAP);
	mode_buf.tt2 |= TT2$M_PASTHRU;
	mode_buf.tt2 &= ~TT2$M_XON;
	status = sys$qiow(0,tt_channel,IO$_SETMODE,0,0,0,&mode_buf,12,0,0,0,0);
	if (FAILED(status))
	    return (EOF);
	iomask = IO$_TTYREADALL | IO$M_NOECHO;
	break;
#endif /* OS_VAXVMS */

    default:				/* do not allow mixed mode flags */
	return (BADMODE);
    }
    kbcurrmode = mode;
    kbcurrmode &= ~(KB_INQUIRE | KB_RESTORE | KB_SAVE);

    if (mode & KB_ECHO)
    {
#if    OS_PCDOS
	/* NO-OP */ ;
#endif /* OS_PCDOS */

#if    OS_TOPS20
	ac1 = jfnof(fileno(kbfp));
	ac2 = w_rfmod | TTYECHO;	/* set echo mode */
	if (jsys(SFMOD,acs) != JSok)
	    return (BADMODE);
#endif /* OS_TOPS20 */


#if    OS_UNIX
#if    (ATT | HPUX)
	/* ??? do not yet know how to handle this ??? */
#endif /* (ATT | HPUX) */
#if    BSD
	if (ioctl(fileno(kbfp),TIOCGETP,&tty) == -1)
	    return (BADMODE);
	tty.sg_flags |= ECHO;
	if (ioctl(fileno(kbfp),TIOCSETP,&tty) == -1)
	    return (BADMODE);
#endif /* BSD */
#endif /* OS_UNIX */

#if    OS_VAXVMS
	iomask &= ~IO$M_NOECHO;		/* turn off no-echo flag */
#endif /* OS_VAXVMS */
    }
    return (kbcurrmode);
}

/*====================================================================*/

int
kbopen(mode)			/* open keyboard with mode (KB_xxxx flags) */
int mode;			/* return 0 on success, EOF on error */
{
    if (kbinit)				/* ignore if already open */
	return (EOF);

#if    OS_PCDOS
    kbfp = (FILE*)NULL;			/* we use direct console input */
    signal_status = signal(SIGINT,SIG_IGN);
    (void)signal(SIGINT,signal_status);
#endif /* OS_PCDOS */

#if    OS_TOPS20
    kbfp = (FILE*)NULL;			/* we use direct JSYS'es */

    /* save terminal characteristics for later modification and restoration */
    ac1 = _CTTRM;
    if (jsys(RFMOD,acs) != JSok)	/* request terminal mode word */
	return (EOF);
    w_rfmod = ac2;

    ac1 = _FHSLF;
    if (jsys(RPCAP,acs) != JSok)	/* request process capabilities */
	return (EOF);
    w_rpcap = ac2;

    ac1 = _FHJOB;
    if (jsys(RTIW,acs) != JSok)		/* request terminal interrupt word */
	return (EOF);
    w_rtiw = ac2;

    ac1 = _CTTRM;
    ac2 = _MORXO;
    if (jsys(MTOPR,acs) != JSok)	/* request end-of-page mode */
	return (EOF);
    w_morxo = ac3;
#endif /* OS_TOPS20 */

#if    OS_UNIX
    if ((kbfp = fopen(TTYNAME,TTYOPENFLAGS)) == (FILE*)NULL)
	return (EOF);

#if    (ATT | HPUX)
    if (ioctl(fileno(kbfp),TCGETA,&ttysave) == -1)
	return (EOF);
#endif

#if    BSD
    if (ioctl(fileno(kbfp),TIOCGETP,&ttysave) == -1)
	return (EOF);
#endif /* BSD */
#endif /* OS_UNIX */

#if    OS_VAXVMS
    kbfp = (FILE*)NULL;			/* we use direct system calls */

    status = sys$assign(&sys_in,&tt_channel,0,0);
    if (FAILED(status))
	return (EOF);
    status = sys$qiow(0,tt_channel,IO$_SENSEMODE,0,0,0, &mode_save,12,0,0,0,0);
    if (FAILED(status))
	return (EOF);
#endif /* OS_VAXVMS */

    kbinit = 1;
    if (kbmode(mode) == (KEYMODE)EOF)
    {
	if (kbfp != (FILE*)NULL)
	    (void)fclose(kbfp);
	kbinit = 0;
	kbfp = (FILE*)NULL;
	return (EOF);
    }

    return (0);
}

#if    OS_TOPS20
/*====================================================================*/
static int
kbrestore()				/* restore terminal settings */
{					/* return 0 on success, EOF on error */
    ac1 = jfnof(fileno(kbfp));
    ac2 = w_rfmod;
    if (jsys(SFMOD,acs) != JSok)	/* set program-related modes */
	return (EOF);
    if (jsys(STPAR,acs) != JSok)	/* set device-related modes */
	return (EOF);

    ac1 = _FHJOB;
    ac2 = w_rtiw;
    if (jsys(STIW,acs) != JSok)		/* set terminal interrupt word */
	return (EOF);

    ac1 = _CTTRM;
    ac2 = _MOXOF;
    ac3 = w_morxo;
    if (jsys(MTOPR,acs) != JSok)	/* restore end-of-page mode */
	return (EOF);

    return (0);
}

#endif /* OS_TOPS20 */

/*====================================================================*/

int
kbunget(c)	/* unget keyboard character (1-level pushback) */
char c;		/* return c, or EOF on error (already pushed back character) */
{
    if (!kbinit)
	return (EOF);
    else if (kbpushback != EMPTY)
	return (EOF);
    else
    {
	kbpushback = (int)c;
	return (kbpushback);
    }
}


#if    OS_UNIX
#if    (ATT | HPUX)
static int
setblock(fd,on)		/* turn blocking on or off */
			/* return EOF on error, 0 on success */
int fd;			/* file descriptor */
BOOLEAN on;		/* FALSE == turn off, TRUE == turn on */
{
    static int blockf;
    static int nonblockf;
    static BOOLEAN first = TRUE;
    int flags;

    if (first)
    {
	first = FALSE;
	if ((flags = fcntl(fd,F_GETFL,0)) == -1)
	    return (EOF);
	blockf = flags & ~O_NDELAY;	/* turn off O_NDELAY */
	nonblockf = flags | O_NDELAY;	/* turn on O_NDELAY */
    }
    if (fcntl(fd,F_SETFL,on ? blockf : nonblockf) == -1)
	return (EOF);
    return (0);
}
#endif /* (ATT | HPUX) */
#endif /* OS_UNIX */
