
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

/*
 * signals -
 *	An Xt Intrinsics signal handler developed based on discussions
 *	in comp.windows.x and written by someone who wishes to be
 *	anonymous. [Modified by me -- M.S.]
 *
 *	A pipe is created and the read side is passed off to
 *	XtAppAddInput().  Everytime a signal occurs a byte, indicating
 *	which signal, is written by the signal handler on the write
 *	side of the pipe.  This causes the Intrinsics to call the
 *	input handler which then invokes the correct callback.
 *
 *	The potential for deadlock exists if the pipe is ever filled!
 *
 * $Id: SeSig.c,v 1.1 1994/02/24 22:13:06 jkh Exp $
 * $Log: SeSig.c,v $
 * Revision 1.1  1994/02/24 22:13:06  jkh
 * Initial revision
 *
 * Revision 1.1  92/12/10  08:51:01  ware
 * Initial revision
 *
 */

#include <X11/Intrinsic.h>
#include <signal.h>		/* signal stuff */
#include <unistd.h>
#include <sys/types.h>		/* for pipe */
#include <string.h>		/* for memset */
#include "SeSig.h"

#if __STDC__ == 1
#if defined(SVR4)
/*
 * This is a hack. On the system V release 4 unix, NSIG *is* defined
 * but it NOT defined if the compiler is in ANSI mode. Presumably the
 * use of this symbol should be avoided.
 */
#ifndef NSIG
#define NSIG (SIGXFSZ+1)
#endif
#endif
#endif

static _XoSignalData sig_info[NSIG + 1];	/* NSIG is max signal value */

static int      pipefd[2];	/* the input & output pipes */

static void     _xsig_handler(
#if NeedFunctionPrototypes
			       int sig,
			       int code
#endif
);

static void
                _xsig_pipe_handler(
#if NeedFunctionPrototypes
				    XtPointer client_data,
				    int *source,
				    XtInputId * id
#endif
);

/*
 * XoAppAddSignal -
 *	Install a handler for a particular signal.  There can be only
 *	a single handler per signal (it might be nice to use a callback).
 */

XoSignalCallbackProc
XoAppAddSignal(context, sig, handler, client_data)
     XtAppContext    context;	/* application context */
     int             sig;	/* which signal */
     XoSignalCallbackProc handler;	/* the handler */
     XtPointer       client_data;	/* private data */
{
  static int      firsttime = True;

  /*
	 * We need to create the pipe and tell the intrinsics about
	 * the new file descriptor
	 */
  if (firsttime) {
    firsttime = False;
    pipe(pipefd);
    XtAppAddInput(context, pipefd[0],
		  (XtPointer) XtInputReadMask,
		  _xsig_pipe_handler, (XtPointer) NULL);
  }
  sig_info[sig].handler = handler;
  sig_info[sig].client_data = client_data;
  return (XoSignalCallbackProc)signal(sig, (void (*)())_xsig_handler);
}				/* XoAppAddSignal */

/*
 * XoAppRemoveSignal -
 *	Uninstalls a handler for a particular signal.  The values
 *	of handler and client_data most match in order to remove the
 *	particular signal handler.  If there are no more remaining
 *	signal handlers for that signal then SIG_DFL is installed.
 *
 *	Of course, the current implementation only allows one handler
 *	per signal but in the future when multiple ones are added this
 *	will continue to work.  The application context is not used
 *	and is left merely for consistency.
 */

void
XoAppRemoveSignal(context, sig)
     XtAppContext    context;	/* application context */
     int             sig;	/* which signal */
{
  signal(sig, SIG_DFL);	       /* restore old signal handler */
  sig_info[sig].handler = (XoSignalCallbackProc) NULL;
  sig_info[sig].client_data = NULL;
}				/* XoAppRemoveSignal */

void
XoAppIgnoreSignal(context, sig)
     XtAppContext    context;	/* application context */
     int             sig;	/* which signal */
{
  signal(sig, SIG_IGN);	       /* ignore signal */
  sig_info[sig].handler = (XoSignalCallbackProc) NULL;
  sig_info[sig].client_data = NULL;
}				/* XoAppIgnoreSignal */

/*
 * _xsig_handler -
 *	the actual signal handler (custom), writes a byte to a pipe
 */
static void
_xsig_handler(sig, code)
     int             sig;
     int             code;
{
  char            sig_value;

  sig_value = sig;
  write(pipefd[1], &sig_value, 1);
}				/* _xsig_handler */

/*
 * _xsig_pipe_handler -
 *	reads input from the pipe and executes the corresponding callback.
 */

static void
_xsig_pipe_handler(client_data, source, id)
     XtPointer       client_data;
     int            *source;
     XtInputId      *id;
{
  unsigned char   sig_value;
  int             sig;

  read(pipefd[0], &sig_value, 1);
  sig = sig_value;
  if (sig > 0 && sig < NSIG && sig_info[sig].handler)
    (*sig_info[sig].handler) (sig, sig_info[sig].client_data);
}				/* _xsig_pipe_handler */
