/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 *
 *****************************************************************************/

/* signals.c
 *
 *   Signal handling for xdbx and dbx.
 *
 *   kill_hanlder():	For SIGINT, SIGQUIT, SIGILL, SIGBUS, SIGTERM
 *			print error message and exit with signal status.
 *   quit_handler():	SIGCHLD, wait for dbx to die and exit gracefully.
 *   stop_handler():	SIGTSTP, stop dbx process, then stop own process.
 *   cont_handler():	SIGCONT, continue dbx process.
 *   trap_signals():	Install signal handlers.
 */

#include <signal.h>
#include <sys/wait.h>
#include "global.h"

/*  Kill the dbx child process and then exits. */
/*  ARGSUSED */
static void kill_handler(sig, code, scp, addr)
    int sig, code;
    struct sigcontext *scp;
    char *addr;
{
    if (FalseSignal) {
	FalseSignal = FALSE;
	return;
    }
    kill(dbxpid, SIGKILL);
    switch (sig) {
      case SIGINT  : fprintf(stderr, "Interrupt\n"); break;
      case SIGQUIT : fprintf(stderr, "Quit\n"); break;
      case SIGILL  : fprintf(stderr, "Illegal instruction\n"); break;
      case SIGBUS  : fprintf(stderr, "Bus error\n"); break;
      case SIGSEGV : fprintf(stderr, "Segmentation violation\n"); break;
      case SIGTERM : fprintf(stderr, "Soft kill\n"); break;
    }
    exit(sig);
}


static void quit_handler()
{
#ifdef SYSV 
    int status;
#else
    union wait status;
#endif /* SYSV */

    /*  wait for the child to report its status; if the child has died, 
     *  exit gracefully.
     */
#ifdef SYSV 
#ifdef SVR4
	waitpid((pid_t)0, &status, WNOHANG|WUNTRACED);	/* (MJH) */
#else
    waitpid(&status, NULL , WNOHANG|WUNTRACED);
#endif /* SVR4 */
#else
    wait3(&status, WNOHANG|WUNTRACED, NULL);
#endif /* SYSV */

    if ((WIFEXITED(status) || WIFSIGNALED(status)) && !WIFSTOPPED(status)) {
    	exit(1);
    }
}


static void stop_handler()
{
    if (dbxpid)
	kill(dbxpid, SIGSTOP);	/* stop dbx process */
    kill(0, SIGSTOP);		/* stop own process */
}


static void cont_handler()
{
    if (dbxpid) {
	sleep(1);		/* we need this */
	kill(dbxpid, SIGCONT);	/* continue dbx after stop */
    }
}


/*
 *  Trap signals to xdbx so that the child process can be handled properly.
 */
void trap_signals()
{
    signal(SIGINT,  kill_handler);
    signal(SIGQUIT, kill_handler);
    signal(SIGILL,  kill_handler);
    signal(SIGBUS,  kill_handler);
    signal(SIGSEGV, kill_handler);
    signal(SIGTERM, kill_handler);

    signal(SIGTSTP, stop_handler);	/* stop signal from keyboard */
    signal(SIGCONT, cont_handler);	/* continue after stop */
    signal(SIGCHLD, quit_handler);	/* child status has changed */
}
