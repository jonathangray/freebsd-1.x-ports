/* exp_command.c - the bulk of the Expect commands

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

#include "expect_cf.h"

#include <stdio.h>
#include <sys/types.h>
/*#include <sys/time.h> seems to not be present on SVR3 systems */
/* and it's not used anyway as far as I can tell */

/* AIX insists that stropts.h be included before ioctl.h, because both */
/* define _IO but only ioctl.h checks first.  Oddly, they seem to be */
/* defined differently! */
#ifdef HAVE_STROPTS_H
#  include <sys/stropts.h>
#endif
#include <sys/ioctl.h>

#ifdef HAVE_SYS_FCNTL_H
#  include <sys/fcntl.h>
#else
#  include <fcntl.h>
#endif
#include <sys/file.h>
#include "exp_tty.h"

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <errno.h>
#include <signal.h>

#if defined(SIGCLD) && !defined(SIGCHLD)
#define SIGCHLD SIGCLD
#endif

#ifdef HAVE_PTYTRAP
#include <sys/ptyio.h>
#endif


#include <math.h>		/* for log/pow computation in send -h */
#include <ctype.h>		/* all this for ispunct! */

#include "tclInt.h"		/* need OpenFile */
/*#include <varargs.h>		tclInt.h drags in varargs.h.  Since Pyramid */
/*				objects to including varargs.h twice, just */
/*				omit this one. */

#include "tcl.h"
#include "string.h"
#include "exp_rename.h"
#include "exp_prog.h"
#include "exp_command.h"
#include "exp_log.h"
#include "exp_event.h"
#include "exp_pty.h"
#ifdef TCL_DEBUGGER
#include "Dbg.h"
#endif

#define SPAWN_ID_VARNAME "spawn_id"

int getptymaster();
int getptyslave();

int exp_forked = FALSE;		/* whether we are child process */

/* the following are just reserved addresses, to be used as ClientData */
/* args to be used to tell commands how they were called. */
/* The actual values won't be used, only the addresses, but I give them */
/* values out of my irrational fear the compiler might collapse them all. */
static int sendCD_error = 2;	/* called as send_error */
static int sendCD_user = 3;	/* called as send_user */
static int sendCD_proc = 4;	/* called as send or send_spawn */
static int sendCD_tty = 6;	/* called as send_tty */

struct exp_f *exp_fs = 0;		/* process array (indexed by spawn_id's) */
int exp_fd_max = -1;		/* highest fd */

int expect_key;			/* no need to initialize */

#ifdef HAVE_PTYTRAP
/* slaveNames provides a mapping from the pty slave names to our */
/* spawn id entry.  This is needed only on HPs for stty, sigh. */
static Tcl_HashTable slaveNames;
#endif /* HAVE_PTYTRAP */

/* Do not terminate format strings with \n!!! */
/*VARARGS*/
void
exp_error(va_alist)
va_dcl
{
	Tcl_Interp *interp;
	char *fmt;
	va_list args;

	va_start(args);
	interp = va_arg(args,Tcl_Interp *);
	fmt = va_arg(args,char *);
	vsprintf(interp->result,fmt,args);
	va_end(args);
}

/* returns handle if fd is usable, 0 if not */
struct exp_f *
exp_fd2f(interp,fd,opened,adjust,msg)
Tcl_Interp *interp;
int fd;
int opened;		/* check not closed */
int adjust;		/* adjust buffer sizes */
char *msg;
{
	if (fd >= 0 && fd <= exp_fd_max && (exp_fs[fd].valid)) {
		struct exp_f *f = exp_fs + fd;

		/* following is a little tricky, do not be tempted do the */
		/* 'usual' boolean simplification */
		if ((!opened) || !f->user_closed) {
			if (adjust) exp_adjust(f);
			return f;
		}
	}

	exp_error(interp,"%s: invalid spawn id (%d)",msg,fd);
	return(0);
}

#if 0
/* following routine is not current used, but might be later */
/* returns fd or -1 if no such entry */
static int
pid_to_fd(pid)
int pid;
{
	int fd;

	for (fd=0;fd<=exp_fd_max;fd++) {
		if (exp_fs[fd].pid == pid) return(fd);
	}
	return 0;
}
#endif

/* Tcl needs commands in writable space */
static char close_cmd[] = "close";

/* prevent an fd from being allocated */
void
exp_busy(fd)
int fd;
{
	int x = open("/dev/null",0);
	if (x != fd) {
		fcntl(x,F_DUPFD,fd);
		close(x);
	}
	exp_close_on_exec(fd);
}

/* called just before an exp_f entry is about to be invalidated */
void
exp_f_prep_for_invalidation(interp,f)
Tcl_Interp *interp;
struct exp_f *f;
{
	int fd = f - exp_fs;

	exp_ecmd_remove_fd_direct_and_indirect(interp,fd);

	exp_close_count++;

	if (f->buffer) {
		ckfree(f->buffer);
		f->buffer = 0;
		f->msize = 0;
		f->size = 0;
		f->printed = 0;
		f->echoed = 0;
		if (f->fg_armed) {
			exp_event_disarm(f-exp_fs);
			f->fg_armed = FALSE;
		}
		ckfree(f->lower);
	}
	f->fg_armed = FALSE;
}

/*ARGSUSED*/
void
exp_trap_on(master)
int master;
{
#ifdef HAVE_PTYTRAP
	if (master == -1) return;
	exp_slave_control(master,1);
#endif /* HAVE_PTYTRAP */
}

int
exp_trap_off(name)
char *name;
{
#ifdef HAVE_PTYTRAP
	int master;
	struct exp_f *f;
	int enable = 0;

	Tcl_HashEntry *entry = Tcl_FindHashEntry(&slaveNames,name);
	if (!entry) {
		debuglog("exp_trap_off: no entry found for %s\n",name);
		return -1;
	}

	f = (struct exp_f *)Tcl_GetHashValue(entry);
	master = f - exp_fs;

	exp_slave_control(master,0);

	return master;
#else
	return name[0];	/* pacify lint, use arg and return something */
#endif
}

/*ARGSUSED*/
void
sys_close(fd,f)
int fd;
struct exp_f *f;
{
	/* Ignore close errors.  Some systems are really odd and */
	/* return errors for no evident reason.  Anyway, receiving */
	/* an error upon pty-close doesn't mean anything anyway as */
	/* far as I know. */
	close(fd);

#ifdef HAVE_PTYTRAP
	if (f->slave_name) {
		Tcl_HashEntry *entry;

		entry = Tcl_FindHashEntry(&slaveNames,f->slave_name);
		Tcl_DeleteHashEntry(entry);

		ckfree(f->slave_name);
		f->slave_name = 0;
	}
#endif
}

int exp_close_count = 0;

int
exp_close(interp,fd)
Tcl_Interp *interp;
int fd;
{
	char *argv[3];

	struct exp_f *f = exp_fd2f(interp,fd,1,0,"close");
	if (!f) return(TCL_ERROR);

	f->user_closed = TRUE;

	if (f->tcl_handle) {
		int rc;

		argv[0] = close_cmd;
		argv[1] = f->tcl_handle;
		argv[2] = 0;

		/* ClientData isn't used by Tcl_CloseCmd so we can afford */
		/* to pass a 0, otherwise I don't know what we'd do! */
		Tcl_ResetResult(interp);
		rc = Tcl_CloseCmd((ClientData)0,interp,2,argv);
		f->sys_closed = TRUE;

		ckfree(f->tcl_handle);
		f->tcl_handle = 0;

		if (rc != TCL_OK) return rc;
	} else {
		if (f->slave_fd) close(f->slave_fd);
		sys_close(fd,f);
	}

	exp_f_prep_for_invalidation(interp,f);

	if (f->user_waited) {
		f->valid = FALSE;
	} else {
		exp_busy(fd);
		f->sys_closed = FALSE;
	}

	return(TCL_OK);
}

static struct exp_f *
fd_new(fd,pid)
int fd;
int pid;
{
	int i, low;
	struct exp_f *newfs;	/* temporary, so we don't lose old exp_fs */

	/* resize table if nec */
	if (fd > exp_fd_max) {
		if (!exp_fs) {	/* no fd's yet allocated */
			newfs = (struct exp_f *)ckalloc(sizeof(struct exp_f)*(fd+1));
			low = 0;
		} else {		/* enlarge fd table */
			newfs = (struct exp_f *)ckrealloc((char *)exp_fs,sizeof(struct exp_f)*(fd+1));
			low = exp_fd_max+1;
		}
		exp_fs = newfs;
		exp_fd_max = fd;
		for (i = low; i <= exp_fd_max; i++) { /* init new fd entries */
			exp_fs[i].valid = FALSE;
		}
	}

	/* this could happen if user does "spawn -open stdin" I suppose */
	if (exp_fs[fd].valid) return exp_fs+fd;

	/* close down old table entry if nec */
	exp_fs[fd].pid = pid;
	exp_fs[fd].size = 0;
	exp_fs[fd].msize = 0;
	exp_fs[fd].buffer = 0;
	exp_fs[fd].printed = 0;
	exp_fs[fd].echoed = 0;
	exp_fs[fd].rm_nulls = exp_default_rm_nulls;
	exp_fs[fd].parity = exp_default_parity;
	exp_fs[fd].key = expect_key++;
	exp_fs[fd].force_read = FALSE;
	exp_fs[fd].fg_armed = FALSE;
	exp_fs[fd].tcl_handle = 0;
	exp_fs[fd].slave_fd = 0;
#ifdef HAVE_PTYTRAP
	exp_fs[fd].slave_name = 0;
#endif /* HAVE_PTYTRAP */
	exp_fs[fd].umsize = exp_default_match_max;
	exp_fs[fd].valid = TRUE;
	exp_fs[fd].user_closed = FALSE;
	exp_fs[fd].sys_closed = FALSE;
	exp_fs[fd].user_waited = FALSE;
	exp_fs[fd].sys_waited = FALSE;
	exp_fs[fd].bg_interp = 0;
	exp_fs[fd].bg_status = unarmed;
	exp_fs[fd].bg_ecount = 0;

	return exp_fs+fd;
}

#if 0
void
exp_global_init(eg,duration,location)
struct expect_global *eg;
int duration;
int location;
{
	eg->ecases = 0;
	eg->ecount = 0;
	eg->i_list = 0;
	eg->duration = duration;
	eg->location = location;
}
#endif

void
exp_init_spawn_id_vars(interp)
Tcl_Interp *interp;
{
	Tcl_SetVar(interp,"user_spawn_id",EXP_SPAWN_ID_USER_LIT,0);
	Tcl_SetVar(interp,"error_spawn_id",EXP_SPAWN_ID_ERROR_LIT,0);

	/* note that the user_spawn_id is NOT /dev/tty which could */
	/* (at least in theory anyway) be later re-opened on a different */
	/* fd, while stdin might have been redirected away from /dev/tty */

	if (exp_dev_tty != -1) {
		char dev_tty_str[10];
		sprintf(dev_tty_str,"%d",exp_dev_tty);
		Tcl_SetVar(interp,"tty_spawn_id",dev_tty_str,0);
	}
}

void
exp_init_spawn_ids()
{
	/* note whether 0,1,2 are connected to a terminal so that if we */
	/* disconnect, we can shut these down.  We would really like to */
	/* test if 0,1,2 are our controlling tty, but I don't know any */
	/* way to do that portably.  Anyway, the likelihood of anyone */
	/* disconnecting after redirecting to a non-controlling tty is */
	/* virtually zero. */

	fd_new(0,isatty(0)?exp_getpid:EXP_NOPID);
	fd_new(1,isatty(1)?exp_getpid:EXP_NOPID);
	fd_new(2,isatty(2)?exp_getpid:EXP_NOPID);

	if (exp_dev_tty != -1) {
		fd_new(exp_dev_tty,exp_getpid);
	}

	/* really should be in interpreter() but silly to do on every call */
	exp_adjust(&exp_fs[0]);
}

void
exp_close_on_exec(fd)
int fd;
{
	(void) fcntl(fd,F_SETFD,1);
}

#define STTY_INIT	"stty_init"

#if 0
static void
show_pgrp(fd,string)
int fd;
char *string;
{
	int pgrp;

	fprintf(stderr,"getting pgrp for %s\n",string);
	if (-1 == ioctl(fd,TIOCGETPGRP,&pgrp)) perror("TIOCGETPGRP");
	else fprintf(stderr,"%s pgrp = %d\n",string,pgrp);
	if (-1 == ioctl(fd,TIOCGPGRP,&pgrp)) perror("TIOCGPGRP");
	else fprintf(stderr,"%s pgrp = %d\n",string,pgrp);
	if (-1 == tcgetpgrp(fd,pgrp)) perror("tcgetpgrp");
	else fprintf(stderr,"%s pgrp = %d\n",string,pgrp);
}

static void
set_pgrp(fd)
int fd;
{
	int pgrp = getpgrp(0);
	if (-1 == ioctl(fd,TIOCSETPGRP,&pgrp)) perror("TIOCSETPGRP");
	if (-1 == ioctl(fd,TIOCSPGRP,&pgrp)) perror("TIOCSPGRP");
	if (-1 == tcsetpgrp(fd,pgrp)) perror("tcsetpgrp");
}
#endif

/*ARGSUSED*/
static void
set_slave_name(f,name)
struct exp_f *f;
char *name;
{
#ifdef HAVE_PTYTRAP
	int newptr;
	Tcl_HashEntry *entry;

	/* save slave name */
	f->slave_name = ckalloc(strlen(exp_pty_slave_name)+1);
	strcpy(f->slave_name,exp_pty_slave_name);

	entry = Tcl_CreateHashEntry(&slaveNames,exp_pty_slave_name,&newptr);
	Tcl_SetHashValue(entry,(ClientData)f);
#endif /* HAVE_PTYTRAP */
}

/* arguments are passed verbatim to execvp() */
/*ARGSUSED*/
static int
Exp_SpawnCmd(clientData,interp,argc,argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int slave;
	int pid;
	char **a;
	/* tell Saber to ignore non-use of ttyfd */
	/*SUPPRESS 591*/
	int ttyfd;
	int master;
	int ttyinit = TRUE;
	int ttycopy = TRUE;
	int echo = TRUE;
	int console = FALSE;
	int pty_only = FALSE;
	char *argv0 = argv[0];
	char *openarg = 0;
	FILE *readfilePtr;
	FILE *writefilePtr;
	int rc, wc;
	char *stty_init;
	int slave_write_ioctls = 1;
		/* by default, slave will be write-ioctled this many times */
	int slave_opens = 3;
		/* by default, slave will be opened this many times */
		/* first comes from initial allocation */
		/* second comes from stty */
		/* third is our own signal that stty is done */

	int sync_fds[2];
	char sync_byte;

	char buf[4];		/* enough space for a string literal */
				/* representing a file descriptor */
	Tcl_DString dstring;
	Tcl_DStringInit(&dstring);

	argc--; argv++;

	for (;argc>0;argc--,argv++) {
		if (streq(*argv,"-nottyinit")) {
			ttyinit = FALSE;
			slave_write_ioctls--;
			slave_opens--;
		} else if (streq(*argv,"-nottycopy")) {
			ttycopy = FALSE;
		} else if (streq(*argv,"-noecho")) {
			echo = FALSE;
		} else if (streq(*argv,"-console")) {
			console = TRUE;
		} else if (streq(*argv,"-pty")) {
			pty_only = TRUE;
		} else if (streq(*argv,"-open")) {
			openarg = argv[1];
			argc--; argv++;
		} else break;
	}

	if (openarg && (argc != 0)) {
		exp_error(interp,"usage: -open [fileXX]\n");
		return TCL_ERROR;
	}

	if (!pty_only && !openarg && (argc == 0)) {
		exp_error(interp,"usage: spawn program [args]");
		return(TCL_ERROR);
	}

	stty_init = exp_get_var(interp,STTY_INIT);
	if (stty_init) {
		slave_write_ioctls++;
		slave_opens++;
	}

/* any extraneous ioctl's that occur in slave must be accounted for
when trapping, see below in child half of fork */
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(sun) && !defined(hp9000s300)
	slave_write_ioctls++;
	slave_opens++;
#endif

	exp_pty_slave_name = 0;

	if (!openarg) {
		if (echo) {
			exp_log(0,"%s ",argv0);
			for (a = argv;*a;a++) {
				exp_log(0,"%s ",*a);
			}
			exp_nflog("\r\n",0);
		}

		if (0 > (master = getptymaster())) {
			exp_error(interp,"too many programs spawned? - out of ptys");
			return(TCL_ERROR);
		}
#ifdef PTYTRAP_DIES
		if (!pty_only) exp_slave_control(master,1);
#endif /* PTYTRAP_DIES */

#define SPAWN_OUT "spawn_out"
		Tcl_SetVar2(interp,SPAWN_OUT,"slave,name",exp_pty_slave_name,0);
	} else {
		if (echo) exp_log(0,"%s [open ...]\r\n",argv0);

		rc = Tcl_GetOpenFile(interp,openarg,0,1,&readfilePtr);
		wc = Tcl_GetOpenFile(interp,openarg,1,1,&writefilePtr);

		/* fail only if both descriptors are bad */
		if (rc == TCL_ERROR && wc == TCL_ERROR) {
			return TCL_ERROR;		
		}

		master = fileno((rc == TCL_OK)?readfilePtr:writefilePtr);
	}

	/* much easier to set this, than remember all masters */
	exp_close_on_exec(master);

	if (openarg || pty_only) {
		struct exp_f *f;

		f = fd_new(master,EXP_NOPID);

		if (openarg) {
			/* save file# handle */
			f->tcl_handle = ckalloc(strlen(openarg)+1);
			strcpy(f->tcl_handle,openarg);

			/* save fd handle for output */
			if (wc == TCL_OK) {
				f->tcl_output = fileno(writefilePtr);
				exp_close_on_exec(master);
			} else {
				/* if we actually try to write to it at some */
				/* time in the future, then this will cause */
				/* an error */
				f->tcl_output = master;
			}
		}

		if (exp_pty_slave_name) set_slave_name(f,exp_pty_slave_name);

		/* make it appear as if process has been waited for */
		f->sys_waited = TRUE;
		f->wait = 0;

		/* tell user id of new process */
		sprintf(buf,"%d",master);
		Tcl_SetVar(interp,SPAWN_ID_VARNAME,buf,0);

		if (!openarg) {
			char value[20];

			/* pty only */
			if (0 > (f->slave_fd = getptyslave(ttycopy,ttyinit,
					stty_init))) {
				exp_error("open(slave pty): %s\r\n",strerror(errno));
				return TCL_ERROR;
			}

			exp_slave_control(master,1);

			sprintf(value,"%d",f->slave_fd);
			Tcl_SetVar2(interp,SPAWN_OUT,"slave,fd",value,0);
		}
		sprintf(interp->result,"%d",EXP_NOPID);
		debuglog("spawn: returns {%s}\r\n",interp->result);

		return TCL_OK;
	}

	if (NULL == (argv[0] = Tcl_TildeSubst(interp,argv[0],&dstring))) {
		goto parent_error;
	}

	pipe(sync_fds);

	if ((pid = fork()) == -1) {
		exp_error(interp,"fork: %s",Tcl_PosixError(interp));
		goto parent_error;
	}

	if (pid) { /* parent */
		struct exp_f *f;

		close(sync_fds[1]);

		f = fd_new(master,pid);

		if (exp_pty_slave_name) set_slave_name(f,exp_pty_slave_name);

#ifdef CRAY
		setptypid(pid);
#endif


#if PTYTRAP_DIES
#ifdef HAVE_PTYTRAP

		while (slave_opens) {
			int cc;
			cc = exp_wait_for_slave_open(master);
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(sun) && !defined(hp9000s300)
			if (cc == TIOCSCTTY) slave_opens = 0;
#endif
			if (cc == TIOCOPEN) slave_opens--;
			if (cc == -1) {
				exp_error(interp,"failed to trap slave pty");
				goto parent_error;
			}
		}

#if 0
		/* trap initial ioctls in a feeble attempt to not block */
		/* the initially.  If the process itself ioctls */
		/* /dev/tty, such blocks will be trapped later */
		/* during normal event processing */

		/* initial slave ioctl */
		while (slave_write_ioctls) {
			int cc;

			cc = exp_wait_for_slave_open(master);
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(sun) && !defined(hp9000s300)
			if (cc == TIOCSCTTY) slave_write_ioctls = 0;
#endif
			if (cc & IOC_IN) slave_write_ioctls--;
			else if (cc == -1) {
				exp_error(interp,"failed to trap slave pty");
				goto parent_error;
			}
		}
#endif /*0*/

#endif /* HAVE_PTYTRAP */
#endif /* PTYTRAP_DIES */

		/*
		 * wait for slave to initialize pty before allowing
		 * user to send to it
		 */ 

		debuglog("parent: waiting for sync byte\r\n");
		rc = read(sync_fds[0],&sync_byte,1);
		if (rc == -1) {
			errorlog("parent sync byte read: %s\r\n",strerror(errno));
			exit(-1);
		}

		/* turn on detection of eof */
		/* this should be done as quickly as possible.  Conceivably */
		/* child could die quickly enough that we are not ready */
		/* for the eof.  Fortunately, the child has a lot of work */
		/* to do so this seems unlikely. */
		exp_slave_control(master,1);

#if 0
		/* this logic is really no good, because child might */
		/* echo it if driver is set to echo */

		/* tell slave to go on now */
		debuglog("parent: telling child to go ahead\r\n");
		wc = write(master," ",1);
		if (wc == -1) {
			errorlog("parent sync byte write: %s\r\n",strerror(errno));
			exit(-1);
		}

		debuglog("parent: now unsynchronized from child\r\n");
#endif

		close(sync_fds[0]);

		/* tell user id of new process */
		sprintf(buf,"%d",master);
		Tcl_SetVar(interp,SPAWN_ID_VARNAME,buf,0);

		sprintf(interp->result,"%d",pid);
		debuglog("spawn: returns {%s}\r\n",interp->result);

		Tcl_DStringFree(&dstring);
		return(TCL_OK);
parent_error:
		Tcl_DStringFree(&dstring);
		return TCL_ERROR;
	}

	/* child process - do not return from here!  all errors must exit() */

	close(sync_fds[0]);

	if (exp_dev_tty != -1) {
		close(exp_dev_tty);
		exp_dev_tty = -1;
	}

#ifdef CRAY
	(void) close(master);
#endif

/* ultrix (at least 4.1-2) fails to obtain controlling tty if setsid */
/* is called.  setpgrp works though.  */
#if defined(POSIX) && !defined(ultrix)
#define DO_SETSID
#endif
#ifdef __convex__
#define DO_SETSID
#endif

#ifdef DO_SETSID
	setsid();
#else
#ifdef SYSV3
#ifndef CRAY
	setpgrp();
#endif /* CRAY */
#else /* !SYSV3 */
#ifdef MIPS_BSD
	/* required on BSD side of MIPS OS <jmsellen@watdragon.waterloo.edu> */
#	include <sysv/sys.s>
	syscall(SYS_setpgrp);
#endif
	setpgrp(0,0);
/*	setpgrp(0,getpid());*/	/* make a new pgrp leader */

/* Pyramid lacks this defn */
#ifdef TIOCNOTTY
	ttyfd = open("/dev/tty", O_RDWR);
	if (ttyfd >= 0) {
		(void) ioctl(ttyfd, TIOCNOTTY, (char *)0);
		(void) close(ttyfd);
	}
#endif /* TIOCNOTTY */

#endif /* SYSV3 */
#endif /* DO_SETSID */
	close(0);
	close(1);
	/* leave 2 around awhile for stderr-related stuff */

	/* since we closed fd 0, open of pty slave must return fd 0 */

	/* since getptyslave may have to run stty, (some of which work on fd */
	/* 0 and some of which work on 1) do the dup's inside getptyslave. */

	if (0 > (slave = getptyslave(ttycopy,ttyinit,stty_init))) {
		errorlog("open(slave pty): %s\r\n",strerror(errno));
		exit(-1);
	}
	/* sanity check */
	if (slave != 0) {
		errorlog("getptyslave: slave = %d but expected 0\n",slave);
		exit(-1);
	}

/* The test for hpux may have to be more specific.  In particular, the */
/* code should be skipped on the hp9000s300 and hp9000s720 (but there */
/* is no documented define for the 720!) */

#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(sun) && !defined(hpux)
	/* 4.3+BSD way to acquire controlling terminal */
	/* according to Stevens - Adv. Prog..., p 642 */
	if (ioctl(0,TIOCSCTTY,(char *)0) < 0) {
		errorlog("failed to get controlling terminal using TIOCSCTTY");
		exit(-1);
	}
#endif

#ifdef CRAY
 	(void) setsid();
 	(void) ioctl(0,TCSETCTTY,0);
 	(void) close(0);
 	if (open("/dev/tty", O_RDWR) < 0) {
 		errorlog("open(/dev/tty): %s\r\n",strerror(errno));
 		exit(-1);
 	}
 	(void) close(1);
 	(void) close(2);
 	(void) dup(0);
 	(void) dup(0);
	setptyutmp();	/* create a utmp entry */

	/* _CRAY2 code from Hal Peterson <hrp@cray.com>, Cray Research, Inc. */
#ifdef _CRAY2
	/*
	 * Interpose a process between expect and the spawned child to
	 * keep the slave side of the pty open to allow time for expect
	 * to read the last output.  This is a workaround for an apparent
	 * bug in the Unicos pty driver on Cray-2's under Unicos 6.0 (at
	 * least).
	 */
	if ((pid = fork()) == -1) {
		errorlog("second fork: %s\r\n",strerror(errno));
		exit(-1);
	}

	if (pid) {
 		/* Intermediate process. */
		int status;
		int timeout;
		char *t;

		/* How long should we wait? */
		if (t = exp_get_var(interp,"pty_timeout"))
			timeout = atoi(t);
		else if (t = exp_get_var(interp,"timeout"))
			timeout = atoi(t)/2;
		else
			timeout = 5;

		/* Let the spawned process run to completion. */
 		while (wait(&status) < 0 && errno == EINTR)
			/* empty body */;

		/* Wait for the pty to clear. */
		sleep(timeout);

		/* Duplicate the spawned process's status. */
		if (WIFSIGNALED(status))
			kill(getpid(), WTERMSIG(status));

		/* The kill may not have worked, but this will. */
 		exit(WEXITSTATUS(status));
	}
#endif /* _CRAY2 */
#endif /* CRAY */

	if (console) exp_console_set();

	/* by now, fd 0 and 1 point to slave pty, so fix 2 */
	close(2);
	fcntl(0,F_DUPFD,2);	/* duplicate 0 onto 2 */

#if 0
	/* avoid fflush of cmdfile since this screws up the parents seek ptr */
	/* There is no portable way to fclose a shared read-stream!!!! */
	if (exp_cmdfile && (exp_cmdfile != stdin))
		(void) close(fileno(exp_cmdfile));
	if (logfile) (void) fclose(logfile);
	if (debugfile) (void) fclose(debugfile);
#endif
	/* (possibly multiple) masters are closed automatically due to */
	/* earlier fcntl(,,CLOSE_ON_EXEC); */

	/* tell parent that we are done setting up pty */
	/* The actual char sent back is irrelevant. */

	/* debuglog("child: telling parent that pty is initialized\r\n");*/
	wc = write(sync_fds[1]," ",1);
	if (wc == -1) {
		errorlog("child: sync byte write: %s\r\n",strerror(errno));
		exit(-1);
	}
	close(sync_fds[1]);

#if 0
	/* wait for master to let us go on */
	debuglog("child: waiting for go ahead from parent\r\n");

	close(master);	/* force master-side close so we can read */
	rc = read(0,&sync_byte,1);
	if (rc == -1) {
		errorlog("child: sync byte read: %s\r\n",strerror(errno));
		exit(-1);
	}

	debuglog("child: now unsynchronized from parent\r\n");
#endif

	/* So much for close-on-exec.  Tcl doesn't mark its files that way */
	/* everything has to be closed explicitly. */
	if (exp_close_in_child) (*exp_close_in_child)();

        (void) execvp(argv[0],argv);
	/* Unfortunately, by now we've closed fd's to stderr, logfile and
		debugfile.
	   The only reasonable thing to do is to send back the error as
	   part of the program output.  This will be picked up in an
	   expect or interact command.
	*/
	errorlog("%s: %s\r\n",argv[0],strerror(errno));
	exit(-1);
	/*NOTREACHED*/
}

/*ARGSUSED*/
static int
Exp_ExpPidCmd(clientData,interp,argc,argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	struct exp_f *f;
	int m = -1;

	argc--; argv++;

	for (;argc>0;argc--,argv++) {
		if (streq(*argv,"-i")) {
			argc--; argv++;
			if (!*argv) goto usage;
			m = atoi(*argv);
		} else goto usage;
	}

	if (m == -1) {
		if (exp_update_master(interp,&m,0,0) == 0) return TCL_ERROR;
	}

	if (0 == (f = exp_fd2f(interp,m,1,0,"exp_pid"))) return TCL_ERROR;

	sprintf(interp->result,"%d",f->pid);
	return TCL_OK;
 usage:
	exp_error(interp,"usage: -i spawn_id");
	return TCL_ERROR;
}

/*ARGSUSED*/
static int
Exp_GetpidDeprecatedCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	debuglog("getpid is deprecated, use pid\r\n");
	sprintf(interp->result,"%d",getpid());
	return(TCL_OK);
}

/* returns current master (via out-parameter) */
/* returns f or 0, but note that since exp_fd2f calls tcl_error, this */
/* may be immediately followed by a "return(TCL_ERROR)"!!! */
struct exp_f *
exp_update_master(interp,m,opened,adjust)
Tcl_Interp *interp;
int *m;
int opened;
int adjust;
{
	char *s = exp_get_var(interp,SPAWN_ID_VARNAME);
	*m = (s?atoi(s):EXP_SPAWN_ID_USER);
	return(exp_fd2f(interp,*m,opened,adjust,(s?s:EXP_SPAWN_ID_USER_LIT)));
}

/*ARGSUSED*/
static int
Exp_SleepCmd(clientData,interp,argc,argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	argc--; argv++;

	if (argc != 1) {
		exp_error(interp,"must have one arg: seconds");
		return TCL_ERROR;
	}

	return(exp_usleep(interp,(long)(atof(*argv)*1000000L)));
}

/* write exactly this many bytes, i.e. retry partial writes */
/* returns 0 for success, -1 for failure */
static int
exact_write(fd,buffer,rembytes)
int fd;
char *buffer;
int rembytes;
{
	int cc;

	while (rembytes) {
		if (-1 == (cc = write(fd,buffer,rembytes))) return(-1);
		/* if (0 == cc) return(-1); can't happen! */

		buffer += cc;
		rembytes -= cc;
	}
	return(0);
}

struct slow_arg {
	int size;
	long time;		/* microseconds */
};

/* returns 0 for success, -1 for failure */
static int
get_slow_args(interp,x)
Tcl_Interp *interp;
struct slow_arg *x;
{
	float ftime;

	int sc;		/* return from scanf */
	char *s = exp_get_var(interp,"send_slow");
	if (!s) {
		exp_error(interp,"send -s: send_slow has no value");
		return(-1);
	}
	if (2 != (sc = sscanf(s,"%d %f",&x->size,&ftime))) {
		exp_error(interp,"send -s: found %d value(s) in send_slow but need 2",sc);
		return(-1);
	}
	if (x->size <= 0) {
		exp_error(interp,"send -s: size (%d) in send_slow must be positive", x->size);
		return(-1);
	}
	x->time = ftime*1000000L;
	if (x->time <= 0) {
		exp_error(interp,"send -s: time (%f) in send_slow must be larger",ftime);
		return(-1);
	}
	return(0);
}

/* returns 0 for success, -1 for failure, pos. for Tcl return value */
static int
slow_write(interp,fd,buffer,rembytes,arg)
Tcl_Interp *interp;
int fd;
char *buffer;
int rembytes;
struct slow_arg *arg;
{
	int rc;

	while (rembytes > 0) {
		int len;

		len = (arg->size<rembytes?arg->size:rembytes);
		if (0 > exact_write(fd,buffer,len)) return(-1);
		rembytes -= arg->size;
		buffer += arg->size;

		/* skip sleep after last write */
		if (rembytes > 0) {
			rc = exp_usleep(interp,arg->time);
			if (rc>0) return rc;
		}
	}
	return(0);
}

struct human_arg {
	float alpha;		/* average interarrival time in seconds */
	float alpha_eow;	/* as above but for eow transitions */
	float c;		/* shape */
	float min, max;
};

/* returns -1 if error, 0 if success */
static int
get_human_args(interp,x)
Tcl_Interp *interp;
struct human_arg *x;
{
	int sc;		/* return from scanf */
	char *s = exp_get_var(interp,"send_human");

	if (!s) {
		exp_error(interp,"send -h: send_human has no value");
		return(-1);
	}
	if (5 != (sc = sscanf(s,"%f %f %f %f %f",
			&x->alpha,&x->alpha_eow,&x->c,&x->min,&x->max))) {
		exp_error(interp,"send -h: found %d value(s) in send_human but need 5",sc);
		return(-1);
	}
	if (x->alpha < 0 || x->alpha_eow < 0) {
		exp_error(interp,"send -h: average interarrival times (%f %f) must be non-negative in send_human", x->alpha,x->alpha_eow);
		return(-1);
	}
	if (x->c <= 0) {
		exp_error(interp,"send -h: variability (%f) in send_human must be positive",x->c);
		return(-1);
	}
	x->c = 1/x->c;

	if (x->min < 0) {
		exp_error(interp,"send -h: minimum (%f) in send_human must be non-negative",x->min);
		return(-1);
	}
	if (x->max < 0) {
		exp_error(interp,"send -h: maximum (%f) in send_human must be non-negative",x->max);
		return(-1);
	}
	if (x->max < x->min) {
		exp_error(interp,"send -h: maximum (%f) must be >= minimum (%f) in send_human",x->max,x->min);
		return(-1);
	}
	return(0);
}

/* Compute random numbers from 0 to 1, for expect's send -h */
/* This implementation sacrifices beauty for portability */
static float
unit_random()
{
	/* current implementation is pathetic but works */
	/* 99991 is largest prime in my CRC - can't hurt, eh? */
	return((float)(1+(rand()%99991))/99991.0);
}

void
exp_init_unit_random()
{
	srand(getpid());
}

/* This function is my implementation of the Weibull distribution. */
/* I've added a max time and an "alpha_eow" that captures the slight */
/* but noticable change in human typists when hitting end-of-word */
/* transitions. */
/* returns 0 for success, -1 for failure, pos. for Tcl return value */
static int
human_write(interp,fd,buffer,arg)
Tcl_Interp *interp;
int fd;
char *buffer;
struct human_arg *arg;
{
	char *sp;
	float t;
	float alpha;
	int wc;
	int in_word = TRUE;

	debuglog("human_write: avg_arr=%f/%f  1/shape=%f  min=%f  max=%f\r\n",
		arg->alpha,arg->alpha_eow,arg->c,arg->min,arg->max);

	for (sp = buffer;*sp;sp++) {
		/* use the end-of-word alpha at eow transitions */
		if (in_word && (ispunct(*sp) || isspace(*sp)))
			alpha = arg->alpha_eow;
		else alpha = arg->alpha;
		in_word = !(ispunct(*sp) || isspace(*sp));

		t = alpha * pow(-log((double)unit_random()),arg->c);

		/* enforce min and max times */
		if (t<arg->min) t = arg->min;
		else if (t>arg->max) t = arg->max;

/*fprintf(stderr,"\nwriting <%c> but first sleep %f seconds\n",*sp,t);*/
		/* skip sleep before writing first character */
		if (sp != buffer) {
			wc = exp_usleep(interp,(long)(t*1000000));
			if (wc > 0) return wc;
		}

		wc = write(fd,sp,1);
		if (0 > wc) return(wc);
	}
	return(0);
}

struct exp_i *exp_i_pool = 0;
struct exp_fd_list *exp_fd_list_pool = 0;

#define EXP_I_INIT_COUNT	10
#define EXP_FD_INIT_COUNT	10

struct exp_i *
exp_new_i()
{
	int n;
	struct exp_i *i;

	if (!exp_i_pool) {
		/* none avail, generate some new ones */
		exp_i_pool = i = (struct exp_i *)ckalloc(
			EXP_I_INIT_COUNT * sizeof(struct exp_i));
		for (n=0;n<EXP_I_INIT_COUNT-1;n++,i++) {
			i->next = i+1;
		}
		i->next = 0;
	}

	/* now that we've made some, unlink one and give to user */

	i = exp_i_pool;
	exp_i_pool = exp_i_pool->next;
	i->value = 0;
	i->variable = 0;
	i->fd_list = 0;
	i->ecount = 0;
	i->next = 0;
	return i;
}

struct exp_fd_list *
exp_new_fd(val)
int val;
{
	int n;
	struct exp_fd_list *fd;

	if (!exp_fd_list_pool) {
		/* none avail, generate some new ones */
		exp_fd_list_pool = fd = (struct exp_fd_list *)ckalloc(
			EXP_FD_INIT_COUNT * sizeof(struct exp_fd_list));
		for (n=0;n<EXP_FD_INIT_COUNT-1;n++,fd++) {
			fd->next = fd+1;
		}
		fd->next = 0;
	}

	/* now that we've made some, unlink one and give to user */

	fd = exp_fd_list_pool;
	exp_fd_list_pool = exp_fd_list_pool->next;
	fd->fd = val;
	return fd;
}

void
exp_free_fd(fd_first)
struct exp_fd_list *fd_first;
{
	struct exp_fd_list *fd, *penultimate;

	if (!fd_first) return;

	/* link entire chain back in at once by first finding last pointer */
	/* making that point back to pool, and then resetting pool to this */

	/* run to end */
	for (fd = fd_first;fd;fd=fd->next) {
		penultimate = fd;
	}
	penultimate->next = exp_fd_list_pool;
	exp_fd_list_pool = fd_first;
}

/* free a single fd */
void
exp_free_fd_single(fd)
struct exp_fd_list *fd;
{
	fd->next = exp_fd_list_pool;
	exp_fd_list_pool = fd;
}

void
exp_free_i(interp,i,updateproc)
Tcl_Interp *interp;
struct exp_i *i;
Tcl_VarTraceProc *updateproc;	/* proc to invoke if indirect is written */
{
	if (i->next) exp_free_i(interp,i->next,updateproc);

	exp_free_fd(i->fd_list);

	if (i->direct == EXP_INDIRECT) {
		Tcl_UntraceVar(interp,i->variable,
			TCL_GLOBAL_ONLY|TCL_TRACE_WRITES,
			updateproc,(ClientData)i);
	}

	/* here's the long form
	   if duration & direct	free(var)  free(val)
		PERM	  DIR	    		1
		PERM	  INDIR	    1		1
		TMP	  DIR
		TMP	  INDIR			1
	   Also if i->variable was a bogus variable name, i->value might not be
	   set, so test i->value to protect this
	   TMP in this case does NOT mean from the "expect" command.  Rather
	   it means "an implicit spawn id from any expect or expect_XXX
	   command".  In other words, there was no variable name provided.
	*/
	if (i->value
	   && (((i->direct == EXP_DIRECT) && (i->duration == EXP_PERMANENT))
		|| ((i->direct == EXP_INDIRECT) && (i->duration == EXP_TEMPORARY)))) {
		ckfree(i->value);
	} else if (i->duration == EXP_PERMANENT) {
		if (i->value) ckfree(i->value);
		ckfree(i->variable);
	}

	i->next = exp_i_pool;
	exp_i_pool = i;
}

/* generate a descriptor for a "-i" flag */
/* cannot fail */
struct exp_i *
exp_new_i_complex(interp,arg,duration,updateproc)
Tcl_Interp *interp;
char *arg;		/* spawn id list or a variable containing a list */
int duration;		/* if we have to copy the args */
			/* should only need do this in expect_before/after */
Tcl_VarTraceProc *updateproc;	/* proc to invoke if indirect is written */
{
	struct exp_i *i;
	char **stringp;

	i = exp_new_i();

	i->direct = (isdigit(arg[0]) || (arg[0] == '-'))?EXP_DIRECT:EXP_INDIRECT;
	if (i->direct == EXP_DIRECT) {
		stringp = &i->value;
	} else {
		stringp = &i->variable;
	}

	i->duration = duration;
	if (duration == EXP_PERMANENT) {
		*stringp = ckalloc(strlen(arg)+1);
		strcpy(*stringp,arg);
	} else {
		*stringp = arg;
	}

	i->fd_list = 0;
	exp_i_update(interp,i);

	/* if indirect, ask Tcl to tell us when variable is modified */

	if (i->direct == EXP_INDIRECT) {
		Tcl_TraceVar(interp, i->variable,
			TCL_GLOBAL_ONLY|TCL_TRACE_WRITES,
			updateproc, (ClientData) i);
	}

	return i;
}

void
exp_i_add_fd(i,fd)
struct exp_i *i;
int fd;
{
	struct exp_fd_list *new_fd;

	new_fd = exp_new_fd(fd);
	new_fd->next = i->fd_list;
	i->fd_list = new_fd;
}

/* this routine assumes i->fd is meaningful */
void
exp_i_parse_fds(i)
struct exp_i *i;
{
	char *p = i->value;

	/* reparse it */
	while (1) {
		int m;
		int negative = 0;
		int valid_spawn_id = 0;

		m = 0;
		while (isspace(*p)) p++;
		for (;;p++) {
			if (*p == '-') negative = 1;
			else if (isdigit(*p)) {
				m = m*10 + (*p-'0');
				valid_spawn_id = 1;
			} else if (*p == '\0' || isspace(*p)) break;
		}

		/* we either have a spawn_id or whitespace at end of string */

		/* skip whitespace end-of-string */
		if (!valid_spawn_id) break;

		if (negative) m = -m;

		exp_i_add_fd(i,m);
	}
}
	
/* updates a single exp_i struct */
void
exp_i_update(interp,i)
Tcl_Interp *interp;
struct exp_i *i;
{
	char *p;	/* string representation of list of spawn ids */

	if (i->direct == EXP_INDIRECT) {
		p = Tcl_GetVar(interp,i->variable,TCL_GLOBAL_ONLY);
		if (!p) {
			p = "";
			exp_debuglog(interp,"warning: indirect variable %s undefined",i->variable);
		}

		if (i->value) {
			if (streq(p,i->value)) return;

			/* replace new value with old */
			ckfree(i->value);
		}
		i->value = ckalloc(strlen(p)+1);
		strcpy(i->value,p);

		exp_free_fd(i->fd_list);
		i->fd_list = 0;
	} else {
		/* no free, because this should only be called on */
		/* "direct" i's once */
		i->fd_list = 0;
	}
	exp_i_parse_fds(i);
}

struct exp_i *
exp_new_i_simple(fd,duration)
int fd;
int duration;		/* if we have to copy the args */
			/* should only need do this in expect_before/after */
{
	struct exp_i *i;

	i = exp_new_i();

	i->direct = EXP_DIRECT;
	i->duration = duration;

	exp_i_add_fd(i,fd);

	return i;
}

/*ARGSUSED*/
static int
Exp_SendLogCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	char *string;
	int len;

	argv++;
	argc--;

	if (argc) {
		if (streq(*argv,"--")) {
			argc--; argv++;
		}
	}

	if (argc != 1) {
		exp_error(interp,"usage: send [args] string");
		return TCL_ERROR;
	}

	string = *argv;

	len = strlen(string);

	if (debugfile) fwrite(string,1,len,debugfile);
	if (logfile) fwrite(string,1,len,logfile);

	return(TCL_OK);
}


/* I've rewritten this to be unbuffered.  I did this so you could shove */
/* large files through "send".  If you are concerned about efficiency */
/* you should quote all your send args to make them one single argument. */
/*ARGSUSED*/
static int
Exp_SendCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int m = -1;	/* spawn id (master) */
	int rc; 	/* final result of this procedure */
	struct human_arg human_args;
	struct slow_arg slow_args;
#define SEND_STYLE_STRING_MASK	0x07	/* mask to detect a real string arg */
#define SEND_STYLE_PLAIN	0x01
#define SEND_STYLE_HUMAN	0x02
#define SEND_STYLE_SLOW		0x04
#define SEND_STYLE_ZERO		0x10
#define SEND_STYLE_BREAK	0x20
	int send_style = SEND_STYLE_PLAIN;
	int want_cooked = TRUE;
	char *string;		/* string to send */
	int len;		/* length of string to send */
	int zeros;		/* count of how many ascii zeros to send */

	char *i_masters = 0;
	struct exp_fd_list *fd;
	struct exp_i *i;
	char *arg;

	argv++;
	argc--;
	while (argc) {
		arg = *argv;
		if (arg[0] != '-') break;
		arg++;
		if (exp_flageq1('-',arg)) {			/* "--" */
			argc--; argv++;
			break;
		} else if (exp_flageq1('i',arg)) {		/* "-i" */
			argc--; argv++;
			if (argc==0) {
				exp_error(interp,"usage: -i spawn_id");
				return(TCL_ERROR);
			}
			i_masters = *argv;
			argc--; argv++;
			continue;
		} else if (exp_flageq1('h',arg)) {		/* "-h" */
			argc--; argv++;
			if (-1 == get_human_args(interp,&human_args))
				return(TCL_ERROR);
			send_style = SEND_STYLE_HUMAN;
			continue;
		} else if (exp_flageq1('s',arg)) {		/* "-s" */
			argc--; argv++;
			if (-1 == get_slow_args(interp,&slow_args))
				return(TCL_ERROR);
			send_style = SEND_STYLE_SLOW;
			continue;
		} else if (exp_flageq1('0',arg)) {		/* "-0" */
			argc--; argv++;
			if (!*argv) zeros = 1;
			else {
				zeros = atoi(*argv);
				argc--; argv++;
				if (zeros < 1) return TCL_OK;
			}
			send_style = SEND_STYLE_ZERO;
			string = "<zero(s)>";
			continue;
		} else if (exp_flageq("raw",arg,1)) {		/* "-raw" */
			argc--; argv++;
			want_cooked = FALSE;
			continue;
		} else if (exp_flageq("break",arg,1)) {		/* "-break" */
			argc--; argv++;
			send_style = SEND_STYLE_BREAK;
			string = "<break>";
			continue;
		} else {
			exp_error(interp,"usage: unrecognized flag <%s>",arg);
			return TCL_ERROR;
		}
	}

	if (send_style & SEND_STYLE_STRING_MASK) {
		if (argc != 1) {
			exp_error(interp,"usage: send [args] string");
			return TCL_ERROR;
		}
		string = *argv;
	}
	len = strlen(string);

	if (clientData == &sendCD_user) m = 1;
	else if (clientData == &sendCD_error) m = 2;
	else if (clientData == &sendCD_tty) m = exp_dev_tty;
	else if (!i_masters) {
		/* we really do want to check if it is open */
		/* but since stdin could be closed, we have to first */
		/* get the fd and then convert it from 0 to 1 if necessary */
		if (0 == exp_update_master(interp,&m,0,0))
			return(TCL_ERROR);
	}

	/* if master != -1, then it holds desired master */
	/* else i_masters does */

	if (m != -1) {
		i = exp_new_i_simple(m,EXP_TEMPORARY);
	} else {
		i = exp_new_i_complex(interp,i_masters,FALSE,(Tcl_VarTraceProc *)0);
	}

#define send_to_stderr	(clientData == &sendCD_error)
#define send_to_proc	(clientData == &sendCD_proc)
#define send_to_user	((clientData == &sendCD_user) || \
			 (clientData == &sendCD_tty))

	if (send_to_proc) {
		want_cooked = FALSE;
		debuglog("send: sending {%s} to {",dprintify(string));
		/* if closing brace doesn't appear, that's because an error */
		/* was encountered before we could send it */
	} else {
		if (debugfile)
			fwrite(string,1,len,debugfile);
		if ((send_to_user && logfile_all) || logfile)
			fwrite(string,1,len,logfile);
	}

	for (fd=i->fd_list;fd;fd=fd->next) {
		m = fd->fd;

		if (send_to_proc) {
			debuglog(" %d ",m);
		}

		/* true if called as Send with user_spawn_id */
		if (exp_is_stdinfd(m)) m = 1;

		/* check validity of each - i.e., are they open */
		if (0 == exp_fd2f(interp,m,1,0,"send")) {
			rc = TCL_ERROR;
			goto finish;
		}
		/* Check if Tcl is using a different fd for output */
		if (exp_fs[m].tcl_handle) {
			m = exp_fs[m].tcl_output;
		}

		if (want_cooked) string = exp_cook(string,&len);

		switch (send_style) {
		case SEND_STYLE_PLAIN:
			rc = exact_write(m,string,len);
			break;
		case SEND_STYLE_SLOW:
			rc = slow_write(interp,m,string,len,&slow_args);
			break;
		case SEND_STYLE_HUMAN:
			rc = human_write(interp,m,string,&human_args);
			break;
		case SEND_STYLE_ZERO:
			for (;zeros>0;zeros--) rc = write(m,"",1);
			/* catching error on last write is sufficient */
			break;
		case SEND_STYLE_BREAK:
			exp_tty_break(interp,m);
			break;
		}

		if (rc != 0) {
			if (rc == -1) {
				exp_error(interp,"write(spawn_id=%d): %s",m,Tcl_PosixError(interp));
				rc = TCL_ERROR;
			}
			goto finish;
		}
	}
	if (send_to_proc) debuglog("}\r\n");

	rc = TCL_OK;
 finish:
	exp_free_i(interp,i,(Tcl_VarTraceProc *)0);
	return rc;
}

/*ARGSUSED*/
static int
Exp_LogFileCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	static Tcl_DString dstring;
	static int first_time = TRUE;

	if (first_time) {
		Tcl_DStringInit(&dstring);
		first_time = FALSE;
	}

	/* when this function returns, we guarantee that if logfile_all */
	/* is TRUE, then logfile is non-zero */

	if (argc > 1 && streq(argv[1],"-info")) {
		if (logfile_all) strcat(interp->result,"-a ");
		if (logfile) strcat(interp->result,Tcl_DStringValue(&dstring));
		return TCL_OK;
	}

	logfile_all = FALSE;

	argv++; argc--;

	while (argc) {
		if (0 != strcmp(*argv,"-a")) break;
		argc--;argv++;
		logfile_all = TRUE;
	}

	/* note that logfile_all may be TRUE here, even if logfile is zero */

	if (argc > 1) {
		/* too many arguments */
		goto usage;
	}

	if (argc == 0) {
		if (logfile_all) {
			goto usage;
		} else if (logfile) {
			fclose(logfile);
			logfile = 0;
		/* } else {
			asked to close file but not open, ignore */
		}
	} else {
		if (logfile) fclose(logfile);
		argv[0] = Tcl_TildeSubst(interp, argv[0],&dstring);
		if (argv[0] == NULL) {
			return(TCL_ERROR);
		} else {
			/* Tcl_TildeSubst doesn't store into dstring */
			/* if no ~, so force string into dstring */
			if (Tcl_DStringValue(&dstring)[0] == '\0') {
				Tcl_DStringAppend(&dstring,argv[0],-1);
			}
		}

		if (NULL == (logfile = fopen(argv[0],"a"))) {
			exp_error(interp,"%s: %s",argv[0],Tcl_PosixError(interp));
			logfile_all = FALSE;
			Tcl_DStringFree(&dstring);
			return(TCL_ERROR);
		}
		setbuf(logfile,(char *)0);
		exp_close_on_exec(fileno(logfile));
	}

	return TCL_OK;
 usage:
	exp_error(interp,"usage: log_file [[-a] file]");
	return TCL_ERROR;
}

/*ARGSUSED*/
static int
Exp_LogUserCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int old_loguser = loguser;

	if (argc == 0 || (argc == 2 && streq(argv[1],"-info"))) {
		/* do nothing */
	} else if (argc == 2) {
		if (0 == atoi(argv[1])) loguser = FALSE;
		else loguser = TRUE;
	} else {
		exp_error(interp,"usage: [-info|1|0]");
	}

	sprintf(interp->result,"%d",old_loguser);

	return(TCL_OK);
}

#ifdef TCL_DEBUGGER
/*ARGSUSED*/
static int
Exp_DebugCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int now = FALSE;	/* soon if FALSE, now if TRUE */
	int exp_tcl_debugger_was_available = exp_tcl_debugger_available;

	if (argc > 3) goto usage;

	if (argc == 1) {
		sprintf(interp->result,"%d",exp_tcl_debugger_available);
		return TCL_OK;
	}

	argv++;

	while (*argv) {
		if (streq(*argv,"-now")) {
			now = TRUE;
			argv++;
		}
		else break;
	}

	if (!*argv) {
		if (now) {
			Dbg_On(interp,1);
			exp_tcl_debugger_available = 1;
		} else {
			goto usage;
		}
	} else if (streq(*argv,"0")) {
		Dbg_Off(interp);
		exp_tcl_debugger_available = 0;
	} else {
		Dbg_On(interp,now);
		exp_tcl_debugger_available = 1;
	}
	sprintf(interp->result,"%d",exp_tcl_debugger_was_available);
	return(TCL_OK);
 usage:
	exp_error(interp,"usage: [[-now] 1|0]");
	return TCL_ERROR;
}
#endif

/*ARGSUSED*/
static int
Exp_ExpInternalCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	static Tcl_DString dstring;
	static int first_time = TRUE;
	int fopened = FALSE;

	if (first_time) {
		Tcl_DStringInit(&dstring);
		first_time = FALSE;
	}

	if (argc > 1 && streq(argv[1],"-info")) {
		if (debugfile) {
			sprintf(interp->result,"-f %s ",
				Tcl_DStringValue(&dstring));
		}
		strcat(interp->result,((exp_is_debugging==0)?"0":"1"));
		return TCL_OK;
	}

	argv++;
	argc--;
	while (argc) {
		if (!streq(*argv,"-f")) break;
		argc--;argv++;
		if (argc < 1) goto usage;
		if (debugfile) fclose(debugfile);
		argv[0] = Tcl_TildeSubst(interp, argv[0],&dstring);
		if (argv[0] == NULL) goto error;

		if (NULL == (debugfile = fopen(*argv,"a"))) {
			exp_error(interp,"%s: %s",*argv,Tcl_PosixError(interp));
			goto error;
		}
		setbuf(debugfile,(char *)0);
		exp_close_on_exec(fileno(debugfile));
		fopened = TRUE;
		argc--;argv++;
	}

	if (argc != 1) goto usage;

	/* if no -f given, close file */
	if (fopened == FALSE && debugfile) {
		fclose(debugfile);
		debugfile = 0;
		Tcl_DStringFree(&dstring);
	}

	exp_is_debugging = atoi(*argv);
	return(TCL_OK);
 usage:
	exp_error(interp,"usage: [-f file] expr");
 error:
	Tcl_DStringFree(&dstring);
	return TCL_ERROR;
}

char *exp_onexit_action = 0;

/*ARGSUSED*/
static int
Exp_ExitCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int value = 0;

	argv++;

	if (*argv) {
		if (exp_flageq(*argv,"-onexit",3)) {
			argv++;
			if (*argv) {
				int len = strlen(*argv);
				if (exp_onexit_action)
					ckfree(exp_onexit_action);
				exp_onexit_action = ckalloc(len + 1);
				strcpy(exp_onexit_action,*argv);
			} else if (exp_onexit_action) {
				Tcl_AppendResult(interp,exp_onexit_action,(char *)0);
			}
			return TCL_OK;
		} else if (exp_flageq(*argv,"-noexit",3)) {
			argv++;
			exp_exit_handlers(interp);
			return TCL_OK;
		}
	}

	if (*argv) {
		if (Tcl_GetInt(interp, *argv, &value) != TCL_OK) {
			return TCL_ERROR;
		}
	}

	exp_exit(interp,value);
	/*NOTREACHED*/
}

/*ARGSUSED*/
static int
Exp_CloseCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int onexec_flag = FALSE;	/* true if -onexec seen */
	int close_onexec;
	int slave_flag = FALSE;
#if 0
	int slave;
#endif
	int m = -1;

	int argc_orig = argc;
	char **argv_orig = argv;

	argc--; argv++;

	for (;argc>0;argc--,argv++) {
		if (streq(*argv,"-i")) {
			argc--; argv++;
			if (!*argv) {
				exp_error(interp,"usage: -i spawn_id");
				return(TCL_ERROR);
			}
			m = atoi(*argv);
		} else if (streq(*argv,"-slave")) {
			slave_flag = TRUE;
		} else if (streq(*argv,"-onexec")) {
			argc--; argv++;
			if (!*argv) {
				exp_error(interp,"usage: -onexec 0|1");
				return(TCL_ERROR);
			}
			onexec_flag = TRUE;
			close_onexec = atoi(*argv);
		} else break;
	}

	if (argc) {
		/* doesn't look like our format, it must be a Tcl-style file */
		/* handle.  Lucky that formats are easily distinguishable. */
		/* Historical note: we used "close"  long before there was a */
		/* Tcl builtin by the same name. */

		/* don't know what is formally correct as 1st arg, but I see */
		/* the code doesn't use it anyway */
		Tcl_ResetResult(interp);
		return(Tcl_CloseCmd(clientData,interp,argc_orig,argv_orig));
	}

	if (m == -1) {
		if (exp_update_master(interp,&m,1,0) == 0) return(TCL_ERROR);
	}

	if (slave_flag) {
		struct exp_f *f = exp_fd2f(interp,m,1,0,"-slave");
		if (!f) return TCL_ERROR;

		if (f->slave_fd) {
			close(f->slave_fd);
			f->slave_fd = 0;

			exp_slave_control(m,1);

			return TCL_OK;
		} else {
			exp_error(interp,"no such slave");
			return TCL_ERROR;
		}
	}

	if (onexec_flag) {
		/* heck, don't even bother to check if fd is open or a real */
		/* spawn id, nothing else depends on it */
		fcntl(m,F_SETFD,close_onexec);
		return TCL_OK;
	}

	return(exp_close(interp,m));
}

/*ARGSUSED*/
static void
tcl_tracer(clientData,interp,level,command,cmdProc,cmdClientData,argc,argv)
ClientData clientData;
Tcl_Interp *interp;
int level;
char *command;
int (*cmdProc)();
ClientData cmdClientData;
int argc;
char *argv[];
{
	int i;

	/* come out on stderr, by using errorlog */
	errorlog("%2d",level);
	for (i = 0;i<level;i++) exp_nferrorlog("  ",0/*ignored - satisfy lint*/);
	errorlog("%s\r\n",command);
}

/*ARGSUSED*/
static int
Exp_StraceCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	static int trace_level = 0;
	static Tcl_Trace trace_handle;

	if (argc > 1 && streq(argv[1],"-info")) {
		sprintf(interp->result,"%d",trace_level);
		return TCL_OK;
	}

	if (argc != 2) {
		exp_error(interp,"usage: trace level");
		return(TCL_ERROR);
	}
	/* tracing already in effect, undo it */
	if (trace_level > 0) Tcl_DeleteTrace(interp,trace_handle);

	/* get and save new trace level */
	trace_level = atoi(argv[1]);
	if (trace_level > 0)
		trace_handle = Tcl_CreateTrace(interp,
				trace_level,tcl_tracer,(ClientData)0);
	return(TCL_OK);
}

/* following defn's are stolen from tclUnix.h */

/*
 * The type of the status returned by wait varies from UNIX system
 * to UNIX system.  The macro below defines it:
 */

#ifndef NO_UNION_WAIT
#   define WAIT_STATUS_TYPE union wait
#else
#   define WAIT_STATUS_TYPE int
#endif

/*
 * Supply definitions for macros to query wait status, if not already
 * defined in header files above.
 */

#ifndef WEXITSTATUS
#   define WEXITSTATUS(stat) (((*((int *) &(stat))) >> 8) & 0xff)
#endif

/*ARGSUSED*/
static int
Exp_WaitCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int m;		/* master waited for */
	struct exp_f *f;	/* ditto */

	int result = 0;		/* 0 means child was successfully waited on */
				/* -1 means an error occurred */
				/* -2 means no eligible children to wait on */
#define NO_CHILD -2

	if (argc == 1) {
		if (0 == exp_update_master(interp,&m,0,0))
			return TCL_ERROR;
	} else if (argc == 2 || argc > 3) {
		exp_error(interp,"usage: -i spawn_id");
		return(TCL_ERROR);
	} else if (streq(argv[1],"-i") && argv[2]) {
		m = atoi(argv[2]);
	} else {
		exp_error(interp,"usage: -i spawn_id");
		return(TCL_ERROR);
	}

	if (m != EXP_SPAWN_ID_ANY) {
		if (0 == exp_fd2f(interp,m,0,0,"wait")) {
			return TCL_ERROR;
		}

		f = exp_fs + m;

		/* check if waited on already */
		/* things opened by "open" are marked sys_waited already */
		if (!f->sys_waited) {
			while (1) {
				if (tcl_AsyncReady) {
					int rc = Tcl_AsyncInvoke(interp,TCL_OK);
					if (rc != TCL_OK) return(rc);
				}

				result = waitpid(f->pid,&f->wait,0);
				if (result == f->pid) break;
				if (result == -1) {
					if (errno == EINTR) continue;
					else break;
				}
			}
		}
	} else {
		/* wait for any of our own */
		/* we call waitpid rather than wait to avoid running into */
		/* someone else's processes.  Yes, according to Ousterhout */
		/* this is the best way to do it. */

		for (m=0;m<=exp_fd_max;m++) {
			f = exp_fs + m;
			if (!f->valid) continue;
			if (f->pid == exp_getpid) continue; /* skip ourself */
			if (f->user_waited) continue;	/* one wait only! */
			if (f->sys_waited) break;
		   restart:
			result = waitpid(f->pid,&f->wait,WNOHANG);
			if (result == f->pid) break;
			if (result == 0) continue;	/* busy, try next */
			if (result == -1) {
				if (errno == EINTR) goto restart;
				else break;
			}
		}
		if (m > exp_fd_max) {
			result = NO_CHILD;	/* no children */
			Tcl_ReapDetachedProcs();
			exp_rearm_sigchld(interp);
		}
	}

	/* return {pid {wait status or error message}} */
	/* non-portable assumption that pid_t can be printed with %d */

	if (result == -1) {
		sprintf(interp->result,"%d %d -1 %d",f->pid,m,errno);
		Tcl_PosixError(interp);
	} else if (result == NO_CHILD) {
		interp->result = "no children";
		return TCL_ERROR;
	} else {
		sprintf(interp->result,"%d %d 0 %d",
					f->pid,m,WEXITSTATUS(f->wait));
	}
			
	f->sys_waited = TRUE;
	f->user_waited = TRUE;

	/* if user has already called close, make sure fd really is closed */
	/* and forget about this entry entirely */
	if (f->user_closed) {
		if (!f->sys_closed) {
			sys_close(m,f);
		}
		f->valid = FALSE;
	}
	return ((result == -1)?TCL_ERROR:TCL_OK);
}

/*ARGSUSED*/
static int
Exp_ForkCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	if (argc > 1) {
		exp_error(interp,"usage: fork");
		return(TCL_ERROR);
	}

	if ((exp_getpid = fork()) == -1) {
		exp_error(interp,"fork: %s",Tcl_PosixError(interp));
		return TCL_ERROR;
	}

	if (0 == exp_getpid) exp_forked = TRUE;

	sprintf(interp->result,"%d",exp_getpid);
	debuglog("fork: returns {%s}\r\n",interp->result);
	return(TCL_OK);
}

/*ARGSUSED*/
static int
Exp_DisconnectCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	/* tell Saber to ignore non-use of ttyfd */
	/*SUPPRESS 591*/
	int ttyfd;

	if (argc > 1) {
		exp_error(interp,"usage: disconnect");
		return(TCL_ERROR);
	}

	if (exp_disconnected) {
		exp_error(interp,"already disconnected");
		return(TCL_ERROR);
	}
	if (!exp_forked) {
		exp_error(interp,"can only disconnect child process");
		return(TCL_ERROR);
	}
	exp_disconnected = TRUE;

	/* ignore hangup signals generated by testing ptys in getptymaster */
	/* and other places */
	signal(SIGHUP,SIG_IGN);

	/* reopen prevents confusion between send/expect_user */
	/* accidentally mapping to a real spawned process after a disconnect */
	if (exp_fs[0].pid != EXP_NOPID) {
		exp_close(interp,0);
		open("/dev/null",0);
		fd_new(0, EXP_NOPID);
	}
	if (exp_fs[1].pid != EXP_NOPID) {
		exp_close(interp,1);
		open("/dev/null",1);
		fd_new(1, EXP_NOPID);
	}
	if (exp_fs[2].pid != EXP_NOPID) {
		/* reopen stderr saves error checking in error/log routines. */
		exp_close(interp,2);
		open("/dev/null",1);
		fd_new(2, EXP_NOPID);
	}

	Tcl_UnsetVar(interp,"tty_spawn_id",TCL_GLOBAL_ONLY);

#ifdef DO_SETSID
	setsid();
#else
#ifdef SYSV3
	/* put process in our own pgrp, and lose controlling terminal */
#ifdef sysV88
	/* With setpgrp first, child ends up with closed stdio */
	/* according to Dave Schmitt <daves@techmpc.csg.gss.mot.com> */
	if (fork()) exit(0);
	setpgrp();
#else
	setpgrp();
	/*signal(SIGHUP,SIG_IGN); moved out to above */
	if (fork()) exit(0);	/* first child exits (as per Stevens, */
	/* UNIX Network Programming, p. 79-80) */
	/* second child process continues as daemon */
#endif
#else /* !SYSV3 */
#ifdef MIPS_BSD
	/* required on BSD side of MIPS OS <jmsellen@watdragon.waterloo.edu> */
#	include <sysv/sys.s>
	syscall(SYS_setpgrp);
#endif
	setpgrp(0,0);
/*	setpgrp(0,getpid());*/	/* put process in our own pgrp */

/* Pyramid lacks this defn */
#ifdef TIOCNOTTY
	ttyfd = open("/dev/tty", O_RDWR);
	if (ttyfd >= 0) {
		/* zap controlling terminal if we had one */
		(void) ioctl(ttyfd, TIOCNOTTY, (char *)0);
		(void) close(ttyfd);
	}
#endif /* TIOCNOTTY */

#endif /* SYSV3 */
#endif /* DO_SETSID */
	return(TCL_OK);
}

/*ARGSUSED*/
static int
Exp_OverlayCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int newfd, oldfd;
	int dash_name = 0;
	char *command;

	argc--; argv++;
	while (argc) {
		if (*argv[0] != '-') break;	/* not a flag */
		if (streq(*argv,"-")) {		/* - by itself */
			argc--; argv++;
			dash_name = 1;
			continue;
		}
		newfd = atoi(argv[0]+1);
		argc--; argv++;
		if (argc == 0) {
			exp_error(interp,"overlay -# requires additional argument");
			return(TCL_ERROR);
		}
		oldfd = atoi(argv[0]);
		argc--; argv++;
		debuglog("overlay: mapping fd %d to %d\r\n",oldfd,newfd);
		if (oldfd != newfd) (void) dup2(oldfd,newfd);
		else debuglog("warning: overlay: old fd == new fd (%d)\r\n",oldfd);
	}
	if (argc == 0) {
		exp_error(interp,"need program name");
		return(TCL_ERROR);
	}
	command = argv[0];
	if (dash_name) {
		argv[0] = ckalloc(1+strlen(command));
		sprintf(argv[0],"-%s",command);
	}

	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
        (void) execvp(command,argv);
	exp_error(interp,"execvp(%s): %s\r\n",argv[0],Tcl_PosixError(interp));
	return(TCL_ERROR);
}

#if 0
/*ARGSUSED*/
int
cmdReady(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	char num[4];	/* can hold up to "999 " */
	char buf[1024];	/* can easily hold 256 spawn_ids! */
	int i, j;
	int *masters, *masters2;
	int timeout = get_timeout();

	if (argc < 2) {
		exp_error(interp,"usage: ready spawn_id1 [spawn_id2 ...]");
		return(TCL_ERROR);
	}

	masters = (int *)ckalloc((argc-1)*sizeof(int));
	masters2 = (int *)ckalloc((argc-1)*sizeof(int));

	for (i=1;i<argc;i++) {
		j = atoi(argv[i]);
		if (!exp_fd2f(interp,j,1,"ready")) {
			ckfree(masters);
			return(TCL_ERROR);
		}
		masters[i-1] = j;
	}
	j = i-1;
	if (TCL_ERROR == ready(masters,i-1,masters2,&j,&timeout))
		return(TCL_ERROR);

	/* pack result back into out-array */
	buf[0] = '\0';
	for (i=0;i<j;i++) {
		sprintf(num,"%d ",masters2[i]); /* note extra blank */
		strcat(buf,num);
	}
	ckfree(masters); ckfree(masters2);
	Tcl_Return(interp,buf,TCL_VOLATILE);
	return(TCL_OK);
}
#endif

/*ARGSUSED*/
int
Exp_InterpreterCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	if (argc != 1) {
		exp_error(interp,"no arguments allowed");
		return(TCL_ERROR);
	}

	return(exp_interpreter(interp));
	/* errors and ok, are caught by exp_interpreter() and discarded */
	/* to return TCL_OK, type "return" */
}

/* this command supercede's Tcl's builtin CONTINUE command */
/*ARGSUSED*/
int
Exp_ExpContinueDeprecatedCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	if (argc == 1) return(TCL_CONTINUE);
	else if (argc == 2) {
		if (streq(argv[1],"-expect")) {
			debuglog("continue -expect is deprecated, use exp_continue\r\n");
			return(EXP_CONTINUE);
		}
	}
	exp_error(interp,"usage: continue [-expect]\n");
	return(TCL_ERROR);
}

/* this command supercede's Tcl's builtin CONTINUE command */
/*ARGSUSED*/
int
Exp_ExpContinueCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	if (argc == 1) return EXP_CONTINUE;
	exp_error(interp,"usage: expect_continue\n");
	return(TCL_ERROR);
}

/* most of this is directly from Tcl's definition for return */
/*ARGSUSED*/
int
Exp_InterReturnCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	/* let Tcl's return command worry about args */
	/* if successful (i.e., TCL_RETURN is returned) */
	/* modify the result, so that we will handle it specially */

	int result = Tcl_ReturnCmd(clientData,interp,argc,argv);
	if (result == TCL_RETURN)
		result = EXP_TCL_RETURN;
	return result;
}

/*ARGSUSED*/
int
Exp_OpenCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	char *tcl_handle = 0;
	FILE *fp;
	struct exp_f *f;
	int m = -1;

	argc--; argv++;

	for (;argc>0;argc--,argv++) {
		if (streq(*argv,"-i")) {
			argc--; argv++;
			if (!*argv) {
				exp_error(interp,"usage: -i spawn_id");
				return TCL_ERROR;
			}
			m = atoi(*argv);
		} else break;
	}

	if (m == -1) {
		if (exp_update_master(interp,&m,0,0) == 0) return TCL_ERROR;
	}

	if (0 == (f = exp_fd2f(interp,m,1,0,"exp_open"))) return TCL_ERROR;

	/* remove from Expect's memory in anticipation of passing to Tcl */

	if (f->tcl_handle) {
		tcl_handle = f->tcl_handle;
	} else {
		/* User probably forgot to close this earlier. */
		if (f->slave_fd) close(f->slave_fd);
	}
	exp_f_prep_for_invalidation(interp,f);
	f->valid = FALSE;

	if (tcl_handle) {
		/* already allocated by Tcl */
		Tcl_SetResult(interp,tcl_handle,TCL_DYNAMIC);
	} else {
		if (!(fp = fdopen(m,"r+"))) {
			exp_error(interp,"fork: %s",Tcl_PosixError(interp));
			return TCL_ERROR;
		}
		Tcl_EnterFile(interp,fp,TCL_FILE_READABLE|TCL_FILE_WRITABLE);
	}
	return TCL_OK;
}

/* return 1 if a string is substring of a flag */
/* this version is the code used by the macro that everyone calls */
int
exp_flageq_code(flag,string,minlen)
char *flag;
char *string;
int minlen;		/* at least this many chars must match */
{
	for (;*flag;flag++,string++,minlen--) {
		if (*string == '\0') break;
		if (*string != *flag) return 0;
	}
	if (*string == '\0' && minlen <= 0) return 1;
	return 0;
}

void
exp_create_commands(interp,c)
Tcl_Interp *interp;
struct exp_cmd_data *c;
{
	Interp *iPtr = (Interp *) interp;
	char cmdnamebuf[80];

	for (;c->name;c++) {
		int create = FALSE;
		/* if already defined, don't redefine */
		if (c->flags & EXP_REDEFINE) create = TRUE;
		else if (!Tcl_FindHashEntry(&iPtr->commandTable,c->name)) {
			create = TRUE;
		}
		if (create) {
			Tcl_CreateCommand(interp,c->name,c->proc,
				c->data,exp_deleteProc);
		}
		if (!(c->name[0] == 'e' &&
		      c->name[1] == 'x' &&
		      c->name[2] == 'p')
		    && !(c->flags & EXP_NOPREFIX)) {
			sprintf(cmdnamebuf,"exp_%s",c->name);
			Tcl_CreateCommand(interp,cmdnamebuf,c->proc,
				c->data,exp_deleteProc);
		}
	}
}

extern int cmdExpectVersion();

static struct exp_cmd_data cmd_data[]  = {
{"close",	Exp_CloseCmd,	0,	EXP_REDEFINE},
#ifdef TCL_DEBUGGER
{"debug",	Exp_DebugCmd,	0,	0},
#endif
{"exp_internal",Exp_ExpInternalCmd,	0,	0},
{"disconnect",	Exp_DisconnectCmd,	0,	0},
{"exit",	Exp_ExitCmd,	0,	EXP_REDEFINE},
{"continue",	Exp_ExpContinueDeprecatedCmd,0,EXP_NOPREFIX|EXP_REDEFINE},
{"exp_continue",Exp_ExpContinueCmd,0,	0},
{"fork",	Exp_ForkCmd,	0,	0},
{"exp_pid",	Exp_ExpPidCmd,	0,	0},
{"getpid",	Exp_GetpidDeprecatedCmd,0,	0},
{"interpreter",	Exp_InterpreterCmd,	0,	0},
{"log_file",	Exp_LogFileCmd,	0,	0},
{"log_user",	Exp_LogUserCmd,	0,	0},
{"exp_open",	Exp_OpenCmd,	0,	0},
{"overlay",	Exp_OverlayCmd,	0,	0},
{"inter_return",Exp_InterReturnCmd,	0,	0},
{"send",	Exp_SendCmd,	(ClientData)&sendCD_proc,	0},
{"send_spawn",	Exp_SendCmd,	(ClientData)&sendCD_proc,	0},/*deprecat*/
{"send_error",	Exp_SendCmd,	(ClientData)&sendCD_error,	0},
{"send_log",	Exp_SendLogCmd,	0,	0},
{"send_tty",	Exp_SendCmd,	(ClientData)&sendCD_tty,	0},
{"send_user",	Exp_SendCmd,	(ClientData)&sendCD_user,	0},
{"sleep",	Exp_SleepCmd,	0,	0},
{"spawn",	Exp_SpawnCmd,	0,	0},
{"strace",	Exp_StraceCmd,	0,	0},
{"wait",	Exp_WaitCmd,	0,	0},
{0}};

void
exp_init_most_cmds(interp)
Tcl_Interp *interp;
{
	exp_create_commands(interp,cmd_data);

#ifdef HAVE_PTYTRAP
	Tcl_InitHashTable(&slaveNames,TCL_STRING_KEYS);
#endif /* HAVE_PTYTRAP */

	exp_close_in_child = exp_close_tcl_files;
}
