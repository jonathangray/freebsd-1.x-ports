#include "LYCurses.h"
#include "HTUtils.h"
#include "LYUtils.h"
#include "LYSignal.h"
#include "LYClean.h"
#include "LYGlobalDefs.h"

#ifdef VMS
BOOLEAN HadVMSInterrupt = FALSE;
#endif /* VMS */

/*
 * Interrupt handler.  Stop curses and exit gracefully.
 */
PUBLIC void cleanup_sig ARGS1(int,sig)
{

#ifdef VMS
    if (!dump_output_immediately) {
	/* Reassert the AST  */
	(void) signal (SIGINT, cleanup_sig);
	HadVMSInterrupt = TRUE;
	if (!LYCursesON)
	    return;
        /* Refresh screen to get rid of "cancel" message, then query */
	clearok(curscr, TRUE);
	refresh();
	/* Ask if exit is intended */
	statusline("Really exit from Lynx? [Y] ");
	if(toupper(LYgetch()) == 'N')
	    return;
    }
#endif /* VMS */

    /* ignore further interrupts */     /*  mhc: 11/2/91 */
    (void) signal (SIGHUP, SIG_IGN);

#ifdef VMS  /* use ttclose() from cleanup() for VMS if not dumping */
    if (dump_output_immediately)
#else /* not VMS */
    (void) signal (SIGINT, SIG_IGN);
#endif /* VMS */

    (void) signal (SIGTERM, SIG_IGN);

    if (sig != SIGHUP) {
	if (!dump_output_immediately)
	    cleanup(); /* <==also calls cleanup_files() */
	printf("\nExiting via interrupt: exit(%d)\n",sig);
	fflush(stdout);
    } else {
	cleanup_files();
    }

#ifdef TRAVERSAL
    dump_traversal_history();
#endif /* TRAVERSAL */

#ifdef VMS
    exit(0);
#else /* not VMS */
    exit(sig);
#endif /* VMS */
}

/*
 * called by Interrupt handler or at quit time.  
 * Erases the temporary files that lynx created
 * temporary files are removed by tempname 
 * which created them
 */
PUBLIC void cleanup_files NOARGS
{
    char filename[120];

	tempname(filename, REMOVE_FILES);
	
}

PUBLIC void cleanup NOARGS
{
#ifdef VMS
    extern BOOLEAN DidCleanup;
#endif /* VMS */

    /* cleanup signals - just in case */
    /* ignore further interrupts */     /*  mhc: 11/2/91 */
    (void) signal (SIGHUP, SIG_IGN);
    (void) signal (SIGTERM, SIG_IGN);

#ifndef VMS  /* use ttclose() from cleanup() for VMS */
    (void) signal (SIGINT, SIG_IGN);
#endif /* not VMS */

    move(LYlines-1, 0);
    clrtoeol();

    stop_bold();
    stop_underline();
    stop_reverse();
    refresh();

    stop_curses();

    cleanup_files();
#ifdef VMS
    ttclose();
    DidCleanup = TRUE;
#endif /* VMS */

   fflush(stdout);
}

