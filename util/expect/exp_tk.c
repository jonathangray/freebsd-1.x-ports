/* exp_tk.c - Tk event interface for Expect

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

/* Notes:
I'm only a little worried because Tk does not check for errno == EBADF
after calling select.  I imagine that if the user passes in a bad file
descriptor, we'll never get called back, and thus, we'll hang forever
- it would be better to at least issue a diagnostic to the user.

Another possible problem: Tk does not do file callbacks round-robin.

Another possible problem: Calling Create/DeleteFileHandler
before/after every Tcl_Eval... in expect/interact could be very
expensive.

*/


#include "expect_cf.h"
#include <stdio.h>
#include <errno.h>

#ifdef HAVE_PTYTRAP
#  include <sys/ptyio.h>
#endif

#include "tk.h"

#include "exp_prog.h"
#include "exp_command.h"	/* for struct exp_f defs */
#include "exp_event.h"

/* Tk_DoOneEvent will call our filehandler which will set the following vars */
/* enabling us to know where and what kind of I/O we can do */
/*#define EXP_SPAWN_ID_BAD	-1*/
/*#define EXP_SPAWN_ID_TIMEOUT	-2*/	/* really indicates a timeout */

static int ready_fd = EXP_SPAWN_ID_BAD;
static int ready_mask;
static int default_mask = TK_READABLE | TK_EXCEPTION;
#if 0
#ifdef HAVE_PTYTRAP
static int default_mask = TK_READABLE | TK_EXCEPTION;
#else
static int default_mask = TK_READABLE;
#endif
#endif

/* reduce calls to malloc/free inside Tk_...FileHandler */
/* Tk insists on having a valid proc here even though it isn't used */
#define Fast_Tk_DeleteFileHandler(fd,dummyproc) \
		Tk_CreateFileHandler(fd,0,dummyproc,(ClientData)0)

static void
exp_arm_background_filehandler_force(m)
int m;
{
	Tk_CreateFileHandler(m,
		TK_READABLE|TK_EXCEPTION,
		exp_background_filehandler,
		(ClientData)(exp_fs+m));

	exp_fs[m].bg_status = armed;
}

void
exp_arm_background_filehandler(m)
int m;
{
	switch (exp_fs[m].bg_status) {
	case unarmed:
		exp_arm_background_filehandler_force(m);
		break;
	case disarm_req_while_blocked:
		exp_fs[m].bg_status = blocked;	/* forget request */
		break;
	case armed:
	case blocked:
		/* do nothing */
		break;
	}
}

void
exp_disarm_background_filehandler(m)
int m;
{
	switch (exp_fs[m].bg_status) {
	case blocked:
		exp_fs[m].bg_status = disarm_req_while_blocked;
		break;
	case armed:
		Tk_DeleteFileHandler(m);
		break;
	case disarm_req_while_blocked:
	case unarmed:
		/* do nothing */
		break;
	}
}

/* ignore block status and forcibly disarm handler - called from exp_close. */
/* After exp_close returns, we will not have an opportunity to disarm */
/* because the fd will be invalid, so we force it here. */
void
exp_disarm_background_filehandler_force(m)
int m;
{
	switch (exp_fs[m].bg_status) {
	case blocked:
	case disarm_req_while_blocked:
	case armed:
		exp_fs[m].bg_status = unarmed;
		Tk_DeleteFileHandler(m);
		break;
	case unarmed:
		/* do nothing */
		break;
	}
}

/* this can only be called at the end of the bg handler in which */
/* case we know the status is some kind of "blocked" */
void
exp_unblock_background_filehandler(m)
int m;
{
	switch (exp_fs[m].bg_status) {
	case blocked:
		exp_arm_background_filehandler_force(m);
		break;
	case disarm_req_while_blocked:
		exp_disarm_background_filehandler_force(m);
		break;
	}
}

/* this can only be called at the beginning of the bg handler in which */
/* case we know the status must be "armed" */
void
exp_block_background_filehandler(m)
int m;
{
	Fast_Tk_DeleteFileHandler(m,exp_background_filehandler);
	exp_fs[m].bg_status = blocked;
}


static void exp_filehandler(clientData,mask)
ClientData clientData;
int mask;
{
	if (ready_fd == (int)clientData) {
		Fast_Tk_DeleteFileHandler(ready_fd,exp_filehandler);
		exp_fs[ready_fd].fg_armed = FALSE;
	} else {
		ready_fd = (int)clientData;
		ready_mask = mask;
	}
}

/*ARGSUSED*/
static void
exp_timehandler(clientData)
ClientData clientData;
{
	/* clientData == &timer_fired */
	*(int *)clientData = TRUE;	

	/* old implementation */
	/* ready_fd = EXP_SPAWN_ID_TIMEOUT; */
}

#if 0
void
exp_disarm_event_handler(count,fds)
int count;
int *fds;
{
	int i;

	for (i=0;i<count;i++) {
		Tk_DeleteFileHandler(fds[i]);
	}
}


static void
exp_arm_event_handlers(count,fds)
int count;
int *fds;
{
	int i;
	int flag = TK_READABLE;

	flag |= TK_EXCEPTION;
#if 0
#ifdef HAVE_PTYTRAP
	flag |= TK_EXCEPTION;
#endif
#endif

	for (i=0;i<count;i++) {
		Tk_CreateFileHandler(fds[i],flag,exp_filehandler,(ClientData)fds[i]);
	}
}
#endif

void
exp_event_disarm(fd)
int fd;
{
	Tk_DeleteFileHandler(fd);
}

/* returns status, one of EOF, TIMEOUT, ERROR or DATA */
/*ARGSUSED*/
int exp_get_next_event(interp,masters, n,master_out,timeout,key)
Tcl_Interp *interp;
int *masters;
int n;			/* # of masters */
int *master_out;	/* 1st ready master, not set if none */
int timeout;		/* seconds */
int key;
{
	static rr = 0;	/* round robin ptr */
	int i;	/* index into in-array */
#ifdef HAVE_PTYTRAP
	struct request_info ioctl_info;
#endif

	int old_async_count = exp_async_count;

	int timer_created = FALSE;
	int timer_fired = FALSE;
	Tk_TimerToken timetoken;/* handle to Tk timehandler descriptor */

	for (;;) {
		int m;
		struct exp_f *f;

		/* if anything has been touched by someone else, report that */
		/* an event has been received */

		for (i=0;i<n;i++) {
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

		if (!timer_created) {
			if (timeout >= 0) {
				timetoken = Tk_CreateTimerHandler(1000*timeout,
						exp_timehandler,
						(ClientData)&timer_fired);
				timer_created = TRUE;
			}
		}

		for (;;) {
			int j;

			/* make sure that all fds that should be armed are */
			for (j=0;j<n;j++) {
				int k = masters[j];

				if (!exp_fs[k].fg_armed) {
					Tk_CreateFileHandler(k,default_mask,
							     exp_filehandler,
							     (ClientData)k);
					exp_fs[k].fg_armed = TRUE;
				}
			}

			Tk_DoOneEvent(0);	/* do any event */

			if (exp_async_count != old_async_count) {
				for (i=0;i<n;i++) {
					if (!exp_fs[masters[i]].valid) {
						exp_error(interp,"spawn id %d has become invalid due to an asynchronous event\n",masters[i]);
						return TCL_ERROR;
					}
				}
				old_async_count = exp_async_count;
			}

			if (timer_fired) return(EXP_TIMEOUT);
			if (ready_fd == EXP_SPAWN_ID_BAD) continue;

			/* if it was from something we're not looking for at */
			/* the moment, ignore it */
			for (j=0;j<n;j++) {
				if (ready_fd == masters[j]) goto found;
			}
			/* not found */
			Fast_Tk_DeleteFileHandler(ready_fd,exp_filehandler);
			exp_fs[ready_fd].fg_armed = FALSE;
			ready_fd = EXP_SPAWN_ID_BAD;
			continue;
		found:
			*master_out = ready_fd;
			ready_fd = EXP_SPAWN_ID_BAD;

			if (timer_created) Tk_DeleteTimerHandler(timetoken);

			/* this test should be redundant but SunOS */
			/* raises both READABLE and EXCEPTION (for no */
			/* apparent reason) when selecting on a plain file */
			if (ready_mask & TK_READABLE) {
				return EXP_DATA_NEW;
			}

			/* ready_mask must contain TK_EXCEPTION */
#ifndef HAVE_PTYTRAP
			return(EXP_EOF);
#else
			if (ioctl(*master_out,TIOCREQCHECK,&ioctl_info) < 0) {
				exp_debuglog("ioctl error on TIOCREQCHECK: %s", Tcl_PosixError(interp));
				return(EXP_TCLERROR);
			}
			if (ioctl_info.request == TIOCCLOSE) {
				return(EXP_EOF);
			}
			if (ioctl(*master_out, TIOCREQSET, &ioctl_info) < 0) {
				exp_debuglog("ioctl error on TIOCREQSET after ioctl or open on slave: %s", strerror(errno));
			}
			/* presumably, we trapped an open here */
			continue;
#endif /* !HAVE_PTYTRAP */
		}
	}
}

/* Having been told there was an event for a specific fd, get it */
/* returns status, one of EOF, TIMEOUT, ERROR or DATA */
/*ARGSUSED*/
int
exp_get_next_event_info(interp,fd,ready_mask)
Tcl_Interp *interp;
int fd;
int ready_mask;
{
#ifdef HAVE_PTYTRAP
	struct request_info ioctl_info;
#endif

	if (ready_mask & TK_READABLE) return EXP_DATA_NEW;

	/* ready_mask must contain TK_EXCEPTION */

#ifndef HAVE_PTYTRAP
	return(EXP_EOF);
#else
	if (ioctl(fd,TIOCREQCHECK,&ioctl_info) < 0) {
		exp_debuglog("ioctl error on TIOCREQCHECK: %s",
				Tcl_PosixError(interp));
		return(EXP_TCLERROR);
	}
	if (ioctl_info.request == TIOCCLOSE) {
		return(EXP_EOF);
	}
	if (ioctl(fd, TIOCREQSET, &ioctl_info) < 0) {
		exp_debuglog("ioctl error on TIOCREQSET after ioctl or open on slave: %s", strerror(errno));
	}
	/* presumably, we trapped an open here */
	/* call it an error for lack of anything more descriptive */
	/* it will be thrown away by caller anyway */
	return EXP_TCLERROR;
#endif
}

/*ARGSUSED*/
int	/* returns TCL_XXX */
exp_usleep(interp,usec)
Tcl_Interp *interp;
long usec;
{
	int timer_fired = FALSE;

	Tk_CreateTimerHandler(usec/1000,exp_timehandler,(ClientData)&timer_fired);

	while (1) {
		Tk_DoOneEvent(0);
		if (timer_fired) return TCL_OK;

		if (ready_fd == EXP_SPAWN_ID_BAD) continue;

		Fast_Tk_DeleteFileHandler(ready_fd,exp_filehandler);
		exp_fs[ready_fd].fg_armed = FALSE;
		ready_fd = EXP_SPAWN_ID_BAD;
	}
}

static char destroy_cmd[] = "destroy .";

static void
exp_event_exit_real(interp)
Tcl_Interp *interp;
{
	Tcl_Eval(interp,destroy_cmd);
}

/* set things up for later calls to event handler */
void
exp_init_event()
{
	exp_event_exit = exp_event_exit_real;
}

void
exp_background_error(interp)
Tcl_Interp *interp;
{
	Tk_BackgroundError(interp);
}
