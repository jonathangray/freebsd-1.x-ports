/* exp_poll.c - poll() interface for Expect

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

/*

This file contains code designed to work on SVR>=3 systems.  The logic
closely parallels interact_select.c.  If your system supports select,
you may use that file instead, however some SV systems (e.g., HPUX)
implement select incorrectly, so I would recommend using poll.

*/

#include "expect_cf.h"
#include <stdio.h>
#include <errno.h>
#include <poll.h>

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include "tcl.h"
#include "exp_prog.h"
#include "exp_command.h"
#include "exp_event.h"

static struct pollfd *fds;
static int maxfds;

/*ARGSUSED*/
void
exp_arm_background_filehandler(m)
int m;
{
}

/*ARGSUSED*/
void
exp_disarm_background_filehandler(m)
int m;
{
}

/*ARGSUSED*/
void
exp_disarm_background_filehandler_force(m)
int m;
{
}

/*ARGSUSED*/
void
exp_unblock_background_filehandler(m)
int m;
{
}

/*ARGSUSED*/
void
exp_block_background_filehandler(m)
int m;
{
}

/*ARGSUSED*/
void
exp_event_disarm(fd)
int fd;
{
}

/* this returns a printable representation of a poll revent.  I only have */
/* to handle the ones that occurs when event is set to 0. */
static
char *
bad_poll_type(x)
int x;
{
	static char msg[30];

	switch (x) {
	case POLLERR: return("POLLERR");
	case POLLHUP: return("POLLHUP");
	case POLLNVAL: return("POLLNVAL");
	default:
		sprintf(msg,"unknown poll event (%d)",x);
		return(msg);
	}
}

/* returns status, one of EOF, TIMEOUT, ERROR or DATA */
int
exp_get_next_event(interp,masters, n,master_out,timeout,key)
Tcl_Interp *interp;
int *masters;
int n;
int *master_out;	/* out variable */
int timeout;
int key;
{
	static rr = 0;	/* round robin ptr */

	int msec;	/* milliseconds to wait */
	int i;	/* index into in-array */
	int pc;

	for (i=0;i<n;i++) {
		struct exp_f *f;
		int m;

		rr++;
		if (rr >= n) rr = 0;

		m = masters[rr];
		f = exp_fs + m;

		if (f->key != key) {
			f->key = key;
			f->force_read = FALSE;
			*master_out = m;
			return(EXP_DATA_OLD);
		} else if ((!f->force_read) && (f->size != 0)) {
			*master_out = m;
			return(EXP_DATA_OLD);
		}
	}

	msec = ((timeout == -1)?-1:(1000 * timeout));

	if (n > maxfds) {
		if (fds) free((char *)fds);
		fds = (struct pollfd *)ckalloc(n*sizeof(struct pollfd));
		maxfds = n;
	}
	
	for (i = 0;i < n;i++) {
		fds[i].fd = masters[i];
		fds[i].events = POLLIN;
		/* apparently, no need to clear revents */
	}

 restart:
	if (tcl_AsyncReady) {
		int rc = Tcl_AsyncInvoke(interp,TCL_OK);
		if (rc != TCL_OK) return(exp_tcl2_returnvalue(rc));

		/* anything in the environment could have changed */
		/* so revalidate fds */
		for (i=0;i<n;i++) {
			if (!exp_fs[masters[i]].valid) {
				exp_error(interp,"spawn id %d has become invalid due to an asynchronous event\n",masters[i]);
				return TCL_ERROR;
			}
		}
	}

	pc = poll(fds,n,msec);
	if (pc > 0) {
	    for (i=0;i<n;i++) {
		rr++;
		if (rr >= n) rr = 0;	/* ">" catches previous readys that */
				/* used more fds then we're using now */

		if (fds[rr].revents) {
			if (fds[rr].revents & POLLIN) {
				*master_out = masters[rr];
				return(EXP_DATA_NEW);
			} else {
				exp_debuglog(interp,"poll[%d].revent = %s\r\n",
					fds[masters[rr]],
					bad_poll_type(fds[rr].revents));
				return(EXP_EOF);
			}
		}
	    }
	} else if (pc == 0) {
		return(EXP_TIMEOUT);
	} else {
		/* pc == -1 */
		switch (errno) {
		case EBADF:
		    /* someone is rotten */
		    for (i=0;i<n;i++) {
			if (fds[i].revents & POLLNVAL) {
				exp_error(interp,"spawn_id %d invalid",masters[i]);
				return(EXP_TCLERROR);
			}
		   }
		case EINTR:
		case EAGAIN:
			/* probably not necessary to update "msec" */
			goto restart;
		default:
			/* not prepared to handle anything else */
			exp_error(interp,"poll: %s",Tcl_PosixError(interp));
			return(EXP_TCLERROR);
		}
	}
	/*NOTREACHED*/
}

/*ARGSUSED*/
int
exp_get_next_event_info(interp,fd,ready_mask)
Tcl_Interp *interp;
int fd;
int ready_mask;
{
}

/* have no idea if this works, but it seems reasonable (given that it is a */
/* hack).  Let me know if it works, or you fix it to make it work - DEL */
/* There is no portable way to do sub-msecond sleeps on such a system, so */
/* do the next best thing (without a busy loop) and fake it: sleep the right */
/* amount of time over the long run.  Note that while "subtotal" isn't */
/* reinitialized, it really doesn't matter for such a gross hack as random */
/* scheduling pauses will easily introduce occasional one second delays. */
int	/* returns TCL_XXX */
exp_usleep(interp,usec)
Tcl_Interp *interp;
long usec;		/* microseconds */
{
	static subtotal = 0;	/* microseconds */
	int msec;		/* milliseconds */
	struct pollfd pf;	/* some systems need this, even tho unused */

	subtotal += usec;
	/* if less then 1 msec pause, do nothing but remember it */
	if (subtotal < 1000) return TCL_OK;
	msec = subtotal/1000;
	subtotal = subtotal%1000;
 restart:
	if (tcl_AsyncReady) {
		int rc = Tcl_AsyncInvoke(interp,TCL_OK);
		if (rc != TCL_OK) return(rc);
	}

	if (-1 == poll(&pf,(unsigned long)0,msec) && errno == EINTR) {
		goto restart;
	}
	return TCL_OK;
}

/* set things up for later calls to event handler */
void
exp_init_event()
{
	maxfds = 5;	/* pick a reasonable number */
	fds = (struct pollfd *)ckalloc(maxfds * sizeof(struct pollfd));

	exp_event_exit = 0;

}

void
exp_background_error(interp)
Tcl_Interp *interp;
{
	exp_background_error_default(interp);
}
