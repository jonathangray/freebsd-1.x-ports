#include "LYCurses.h"
#include "HTUtils.h"
#include "LYUtils.h"
#include "LYGlobalDefs.h"
#include "LYSignal.h"
#include "LYClean.h"
/*
 * These are routines to start and stop curses and to cleanup
 * the screen at the end
 *
 */

PRIVATE int dumbterm PARAMS((char *terminal));
BOOLEAN LYCursesON = FALSE;

PUBLIC void start_curses NOARGS
{
#ifndef VMS
    static BOOLEAN first_time = TRUE;

	/* if we're not VMS then only do initsrc() one time
	 * and one time only!
	 * if we are VMS then do initsrc() everytime start_curses()
	 * is called!
	 */
    if(first_time) {
        initscr();	/* start curses */
#ifdef __FreeBSD__
	/* disable clear to EOL feature -- FreeBSD curses misuses it */
	CE = 0;
#endif
        first_time = FALSE;
    }

#else
    initscr();	/* start curses */
#endif /* VMS */

    /* nonl();   *//* seems to slow things down */

#ifdef VMS
    crmode();
    raw();
#else
    cbreak();
    signal(SIGINT, cleanup_sig);
#endif /* VMS */

    noecho();

#ifndef VMS
#ifndef NO_KEYPAD
    keypad(stdscr,TRUE);
#endif /* NO_KEYPAD */ 
#endif /* not VMS */

    fflush(stdin);
    fflush(stdout);

    LYCursesON = TRUE;
}


PUBLIC void stop_curses NOARGS
{
    echo();

    endwin();	/* stop curses */

    fflush(stdout);

    LYCursesON = FALSE;

}


#ifdef VMS
/*
 * check terminal type, start curses & setup terminal
 */
PUBLIC BOOLEAN setup ARGS1(char *,terminal)
{
    int c;
    char *dummy, *cp, term[81];

    /*
     * get terminal type, and convert to lower case
     */
    longname(dummy, term);
    for (cp=term; *cp!='\0'; cp++)
	if (isupper(*cp))
	    *cp = tolower(*cp);

    printf("Terminal = %s\n", term);
    sleep(1);
    if (strncmp(term, "vt1", 3) != 0 &&
	strncmp(term, "vt2", 3) != 0 &&
	strncmp(term, "vt3", 3) != 0 &&
	strncmp(term, "vt4", 3) != 0 &&
	strncmp(term, "vt52", 4) != 0 ) {
	printf(
	    "You must use a vt100, 200, etc. terminal with this program.\n");
	printf("Proceed (n/y)? ");
	c = getchar();
	if (c != 'y' && c != 'Y') {
	    printf("\n");
	    return(FALSE);
	}
    }

    ttopen();
    start_curses();

    LYlines = LINES;
    LYcols = COLS;

    return(TRUE);
}

#else	/* not VMS */
/*
 * check terminal type, start curses & setup terminal
 */
PUBLIC BOOLEAN setup ARGS1(char *,terminal)
{
    static char term_putenv[120];
    char term[120];
    char buffer[120];

 /* if the display was not set by a command line option then see 
  * if it is available from the environment 
  */
    display = getenv("DISPLAY");

    if(terminal != NULL) {
	sprintf(term_putenv,"TERM=%s",terminal);
	(void) putenv(term_putenv);
    }

	/* query the terminal type */
    if(dumbterm(getenv("TERM"))) {
	printf("\n\n  Your Terminal type is unknown!\n\n");
	printf("  Enter a terminal type: [vt100] ");
	gets(buffer);

	if(strlen(buffer) == 0)
	    strcpy(buffer,"vt100"); 
	
	sprintf(term_putenv,"TERM=%s", buffer);
	putenv(term_putenv);  /* */
	printf("\nTERMINAL TYPE IS SET TO %s\n",getenv("TERM"));
    }


    start_curses();

#ifndef NO_TTYTYPE
    /* get terminal type (strip 'dec-' from vms style types) */
    if (strncmp(ttytype, "dec-vt", 6) == 0) {
	strcpy(term, ttytype+4);
	(void) setterm(term);
    }
#endif /*  NO_TTYTYPE */

    LYlines = LINES;
    LYcols = COLS;

    return(1);
}

PRIVATE int dumbterm ARGS1(char *,terminal)
{
    int dumb = FALSE;

    if (!strcasecomp(terminal, "network") ||
	!strcasecomp(terminal, "unknown") ||
	!strcasecomp(terminal, "dialup")  ||
	!strcasecomp(terminal, "dumb")    ||
	!strcasecomp(terminal, "switch")  ||
	!strcasecomp(terminal, "ethernet")  )
	dumb = TRUE;
    return(dumb);
}
#endif /* VMS */

#ifdef VMS
/*
 *	Cut-down termio --
 *		Do character-oriented stream input for Jeff.
 *		Code ripped off from Micro-Emacs 3.7 by Daniel Lawrence.
 *
 *		Ever-so-slightly modified by Kathryn Huxtable.  29-Jan-1991.
 *		Cut down for Lou.  8 Sep 1992.
 *		Cut down farther for Lou.  19 Apr 1993.
 *			We don't set PASSALL or PASTHRU since we don't
 *			want to block CTRL/C, CTRL/Y, CTRL/S or CTRL/Q.
 *			Simply setting NOECHO and doing timed reads
 *			is sufficient.
 *              Further mods by Fote.  29-June-1993
 *			ttopen() and ttclose() are now terminal initialization
 *			 and restoration procedures, called once at startup
 *			 and at exit, respectively, of the LYNX image.
 *			ttclose() should be called before an exit from LYNX
 *			 no matter how the exit is invoked.
 *			setup(terminal) does the ttopen().
 *			cleanup() calls cleanup_files() and ttclose().
 *			ttgetc() now handles NOECHO and NOFLITR (instead of
 *			 setting the terminal itself to NOECHO in ttopen()).
 *			vsignal() added for handling both Ctrl-C *and* Ctrl-Y
 *			 interrupts, and disabling system response to Ctrl-T.
 *              Further mods by Fote.  15-Dec-1993
 *			Added edit handler in ttopen() which will invoke
 *			 VMSexit() and behave intelligently on ACCVIO's.
 *              Further mods by Fote.  29-Dec-1993
 *			Simplified ttgetc().
 *		Further mods by Fote.  16-Jan-1994
 *			Added code in ttopen() which will invoke VMSVersion()
 *			 to get the version of VMS as VersionVMS for use by
 *			 by new or modified interrupt or spawning routines.
 *		Further mods by Fote.  27-Jan-1994
 *			Added back a typeahead() which supports 'z' or 'Z' as
 *			an "Zap transfer" command via HTCheckForInterrupt()
 *			in LYUtils.c.
 */

#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#include <stdlib.h>
#include <msgdef.h>
#include <ttdef.h>
#include <tt2def.h>
#include <libclidef.h>
#include <clidef.h>
#include <syidef.h>
#ifdef signal
#undef signal
#endif /* signal */
#include <signal.h>
#ifdef system
#undef system
#endif /* system */
#include <processes.h>

#ifndef CLI$M_TRUSTED
#define CLI$M_TRUSTED 64 /* May not be in the compiler's clidef.h	*/
#endif

#define	TRUE	1			/* True value			*/
#define	FALSE	0			/* False value			*/

#define EFN	0			/* Event flag			*/

static	unsigned char buffer[20];	/* Input buffer			*/
static	int	in_pos, in_len;		/* For escape sequences		*/
static	int	oldmode[3];		/* Old TTY mode bits		*/
static	int	newmode[3];		/* New TTY mode bits		*/
static	short	iochan;			/* TTY I/O channel		*/
static  $DESCRIPTOR(term_nam_dsc,"TT"); /* Descriptor for iochan	*/
static	int	mask = LIB$M_CLI_CTRLY|LIB$M_CLI_CTRLT; /* ^Y and ^T	*/
static	int 	old_msk;		/* Saved control mask		*/
static	short	trap_flag = FALSE;	/* TRUE if AST is set		*/
BOOLEAN DidCleanup = FALSE;		/* Exit handler flag		*/
static char VersionVMS[20];		/* Version of VMS		*/

PUBLIC int VMSVersion ARGS2(char *,VerString, int,VerLen)
{
     int status, i = SYI$_VERSION, verlen = 0;
     struct dsc$descriptor version;
     char *m;

     version.dsc$a_pointer = VerString;
     version.dsc$w_length = VerLen - 1;
     version.dsc$b_dtype = DSC$K_DTYPE_B;
     version.dsc$b_class = DSC$K_CLASS_S;

     status = lib$getsyi(&i, 0, &version, &verlen, 0, 0);
     if (!(status&1) || verlen == 0)
	  return 0;

     /* Cut out trailing spaces */
     for (m=VerString+verlen, i=verlen-1; i > 0 && VerString[i] == ' '; --i)
	  *(--m) = '\0';

     return strlen(VerString)+1;	/* Transmit ending 0 too */
}

PUBLIC void VMSexit NOARGS
{
    /*
     * If we get here and DidCleanup is not set, it was via an
     *  ACCVIO, so make *sure* we attempt a cleanup and reset
     *  the terminal.
     */
     if (!DidCleanup) {
          fprintf(stderr,"\nPlease report this error to Lou Montulli:");
          fprintf(stderr,"\n    montulli@ukanaix.cc.ukans.edu");
          fprintf(stderr,"\nPress RETURN to clean up: ");
	  (void) getchar();
          cleanup();
     }
}

/*
 *	TTOPEN --
 *		This function is called once to set up the terminal
 *		device streams.	 It translates TT until it finds
 *		the terminal, then assigns a channel to it, sets it
 *		to EDIT, and sets up the Ctrl-C and Ctrl-Y interrupt
 *		handling.
 */

PUBLIC int ttopen NOARGS
{
	extern	void cleanup_sig();
	int	iosb[2];
	int	status;
	static unsigned long condition;
	static struct _exit_block {
	    unsigned long forward;
	    unsigned long address;
	    unsigned long zero;
	    unsigned long condition;
	} exit_handler_block;

	status = SYS$ASSIGN( &term_nam_dsc, &iochan, 0, 0 );
	if( status != SS$_NORMAL )
		exit( status );

	status = SYS$QIOW( EFN, iochan, IO$_SENSEMODE, &iosb, 0, 0,
			  &oldmode, sizeof(oldmode), 0, 0, 0, 0 );
	if( status != SS$_NORMAL )
		exit( status );

	status = iosb[0] & 0xFFFF;
	if( status != SS$_NORMAL )
		exit( status );

	newmode[0] = oldmode[0];
	newmode[1] = oldmode[1];
	newmode[2] = oldmode[2] | TT2$M_EDIT;

	status = SYS$QIOW( EFN, iochan, IO$_SETMODE, &iosb, 0, 0,
			  &newmode, sizeof(newmode), 0, 0, 0, 0 );
	if( status != SS$_NORMAL )
		exit( status );

	status = iosb[0] & 0xFFFF;
	if( status != SS$_NORMAL )
		exit( status );

	/* declare the exit handler block */
	exit_handler_block.forward   = 0;
	exit_handler_block.address   = (unsigned long) &VMSexit;
	exit_handler_block.zero      = 0;
	exit_handler_block.condition = (unsigned long) &condition;
	status = sys$dclexh(&exit_handler_block);
	if (status != SS$_NORMAL)
		exit( status );

	/* set the AST */
	lib$disable_ctrl(&mask, &old_msk);
	trap_flag = TRUE;
	status = SYS$QIOW ( EFN, iochan,
			    IO$_SETMODE|IO$M_CTRLCAST|IO$M_CTRLYAST,
			    &iosb, 0, 0,
			    &cleanup_sig, SIGINT, 0, 0, 0, 0 );
	if ( status != SS$_NORMAL ) {
		lib$enable_ctrl(&old_msk);
		exit ( status );
	}

	/* Get the version of VMS */
	if (VMSVersion(VersionVMS, 20) < 3)
		/* Load zeros on error */
		strcpy(VersionVMS, "V0.0-0");

	return(0);
}	/*  ttopen  */

/*
 *	TTCLOSE --
 *		This function gets called just before we go back home
 *		to the command interpreter.  It puts the terminal back
 *		in a reasonable state.
 */

PUBLIC int ttclose NOARGS
{
	int	status;
	int	iosb[1];

	status = SYS$QIOW( EFN, iochan, IO$_SETMODE, &iosb, 0, 0,
			  &oldmode, sizeof(oldmode), 0, 0, 0, 0 );

	if( status != SS$_NORMAL || (iosb[0] & 0xFFFF) != SS$_NORMAL )
		exit( status );

	if (trap_flag) {
	    status = SYS$DASSGN (iochan);
	    status = lib$enable_ctrl(&old_msk);
	    trap_flag = FALSE;
	}
	return(0);
}	/*  ttclose  */

/*
 *	TTGETC --
 *		Read a character from the terminal, with NOECHO and NOFILTR.
 */

PUBLIC int ttgetc NOARGS
{
     int status;
     unsigned short iosb[4];
     
     if (in_pos < in_len)
          return(buffer[in_pos++]);

     status = SYS$QIOW (EFN, iochan,
     			IO$_READVBLK|IO$M_NOECHO|IO$M_NOFILTR,
     			&iosb, 0, 0,
			&buffer, 1, 0, 0, 0, 0);
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

/*
 *	TYPEAHEAD -- Fote Macrides 27-Jan-1994
 *		Check whether a keystroke has been entered, and return
 *		 it, or -1 if none was entered.
 */

PUBLIC int typeahead NOARGS
{
     int status;
     unsigned short iosb[4];
     
     if (in_pos < in_len)
          return(buffer[in_pos++]);

again:
     status = SYS$QIOW (EFN, iochan,
     			IO$_READVBLK|IO$M_TIMED|IO$M_NOECHO|IO$M_NOFILTR,
     			&iosb, 0, 0,
			&buffer, 1, 0, 0, 0, 0);
     if ((status&1) == 1)
          status = iosb[0];
     if (status == SS$_PARTESCAPE) {
	  /* escape sequence in progress, finish reading it */
	  goto again;
     }

     in_pos = 1;
     in_len = iosb[1] + iosb[3];
     if (status == SS$_TIMEOUT)
         return(-1);
     return (buffer[0]);
}

/*
 *	VSIGNAL -- Fote Macrides 29-Jun-1993
 *		Sets up AST for both Ctrl-C and Ctrl-Y, with system response
 *		 to Ctrl-T disabled.  If called with a sig other than SIGINT,
 *		 it will use the C library's system(sig, func).
 *              The equivalent of vsignal(SIGINT, cleanup_sig) is done on
 *               intialization by ttopen(), so don't do it again.
 *		vsignal(SIGINT, SIG_DFL) is treated as a call to ttclose().
 *              Call vsignal(SIGINT, SIG_IGN) before system() calls to
 *               enable Ctrl-C and Ctrl-Y in the subprocess, and then call
 *               vsignal(SIG_INT, cleanup_sig) on return from the subprocess.
 *		For func's which set flags and do not invoke an exit from
 *		 LYNX, the func should reassert itself.
 *              The VMS signal() calls do not fully emulate the Unix calls,
 *		 and vsignal() is just a "helper", also not a full emulation.
 */

PUBLIC void *vsignal (sig,func)
int sig;
void (*func)();
{
	int status;
	short iosb[4];
	static int SIG_IGN_flag;

	/* Pass all signals other than SIGINT to signal() */
	/* Also pass SIGINT to signal() if we're dumping  */
	if (sig != SIGINT || dump_output_immediately) {
	    signal(sig, func);
	    return;
	}

	/* If func is SIG_DFL, treat it as ttclose() */
	if (func == SIG_DFL) {
	    ttclose();
	    return;
	}

	/* Clear any previous AST */
	if (trap_flag) {
	    status = SYS$DASSGN (iochan);
	    status = lib$enable_ctrl(&old_msk);
	    trap_flag = FALSE;
	}

	/* If func is SIG_IGN, leave the TT channel closed and the  */
	/* system response to interrupts enabled for system() calls */
	if (func == SIG_IGN)
	    return;

	/* If we get to here, we have a LYNX func, so set the AST */
	lib$disable_ctrl(&mask, &old_msk);
	trap_flag = TRUE;
	status = SYS$ASSIGN (&term_nam_dsc, &iochan, 0, 0 );
	status = SYS$QIOW ( EFN, iochan,
			    IO$_SETMODE|IO$M_CTRLCAST|IO$M_CTRLYAST,
			    &iosb, 0, 0,
			    func, SIGINT, 0, 0, 0, 0 );

}	/* vsignal */

/*
 *  DCLspawn_exception, spawn_DCLprocess, DCLsystem -- F.Macrides 16-Jan-1994
 *	Exception-handler routines for regulating interrupts and enabling
 *	Control-T during spawns.  Includes TRUSTED flag for versions of VMS
 *	which require it in captive accounts.  This code should be used
 *	instead of the VAXC or DECC system(), by including LYSystem.h in
 *	modules which have system() calls.  It helps ensure that we return
 *	to Lynx instead of breaking out to DCL if a user issues interrupts
 *	or generates an ACCVIO during spawns.
 */

PRIVATE int DCLspawn_exception ARGS2(int *,sigarr, int *,mecharr)
{
     int status;
     
     status = lib$sig_to_ret(sigarr, mecharr);
     return(SS$_UNWIND);
}

PRIVATE int spawn_DCLprocess ARGS1(char *,command)
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
     else {
	  if(VersionVMS[1] >= '6')
	       /** Include TRUSTED flag **/
               status = lib$spawn(&command_desc,0,0,&CLI$M_TRUSTED);
	  else
	       status = lib$spawn(&command_desc);
     }
     return(status);
}

PUBLIC int DCLsystem ARGS1(char *,command)
{
     int status;
     extern void controlc();
     
     vsignal(SIGINT, SIG_IGN);
     status = spawn_DCLprocess(command);
     vsignal(SIGINT, cleanup_sig);
     if ((status&1) != 1)
         return(-1);
     else
     	 return(0);
}
#endif /* VMS */

