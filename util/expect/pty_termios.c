/* pty_termios.c - routines to allocate ptys - termios version

Written by: Don Libes, NIST, 2/6/90

This file is in the public domain.  However, the author and NIST
would appreciate credit if you use this file or parts of it.

*/

#include <stdio.h>
#include <signal.h>

#if defined(SIGCLD) && !defined(SIGCHLD)
#define SIGCHLD SIGCLD
#endif

#include "expect_cf.h"
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_SYSMACROS_H
#include <sys/sysmacros.h>
#endif

#ifdef HAVE_PTYTRAP
#include <sys/ptyio.h>
#endif

#include <sys/file.h>

#ifdef HAVE_SYS_FCNTL_H
#  include <sys/fcntl.h>
#else
#  include <fcntl.h>
#endif

#ifdef HAVE_PTMX
#  include <sys/stropts.h>
#endif

#include "exp_win.h"

#include "exp_tty_in.h"
#include "exp_rename.h"
#include "exp_pty.h"

void debuglog();

extern int errno;
/*extern char *sys_errlist[];*/

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

/* very old SGIs prefer _getpty over ptc */
#if defined(HAVE__GETPTY) && defined(HAVE_PTC) && !defined(HAVE_GETPTY)
#undef HAVE_PTC
#endif

#if defined(HAVE_PTC)
static char slave_name[] = "/dev/ttyqXXX";
/* some machines (e.g., SVR4.0 StarServer) have all of these and */
/* HAVE_PTC works best */
#undef HAVE_GETPTY
#undef HAVE__GETPTY
#endif

#if defined(HAVE__GETPTY) || defined(HAVE_PTC_PTS) || defined(HAVE_PTMX)
static char *slave_name;
#endif

#if defined(HAVE_GETPTY)
#include <sys/vty.h>
static char master_name[MAXPTYNAMELEN];
static char slave_name[MAXPTYNAMELEN];
#endif

#if !defined(HAVE_GETPTY) && !defined(HAVE__GETPTY) && !defined(HAVE_PTC) && !defined(HAVE_PTC_PTS) && !defined(HAVE_PTMX)
#ifdef HAVE_PTYM
static char	master_name[] = "/dev/ptym/ptyXX";
static char	slave_name[] = "/dev/pty/ttyXX";
static char	*slave_bank;
static char	*slave_num;
#else
static char	master_name[] = "/dev/ptyXX";
static char	slave_name [] = "/dev/ttyXX";
#endif
#endif

static char	*tty_type;		/* ptr to char [pt] denoting
					   whether it is a pty or tty */
static char	*tty_bank;		/* ptr to char [p-z] denoting
					   which bank it is */
static char	*tty_num;		/* ptr to char [0-f] denoting
					   which number it is */
char *exp_pty_slave_name;



#if 0
static void
pty_stty(s,name)
char *s;		/* args to stty */
char *name;		/* name of pty */
{
#define MAX_ARGLIST 10240
	char buf[MAX_ARGLIST];	/* overkill is easier */
	RETSIGTYPE (*old)();	/* save old sigalarm handler */
	int pid;
	
	old = signal(SIGCHLD, SIG_DFL);
	switch (pid = fork()) {
	case 0: /* child */
		exec_stty("/bin/stty","/bin/stty",s);
		break;
	case -1: /* fail */
	default: /* parent */
		waitpid(pid);
		break;
	}

	signal(SIGCHLD, old);	/* restore signal handler */
}

exec_stty(s)
char *s;
{
	char *args[50];
	char *cp;
	int argi = 0;
	int quoting = FALSE;
	int in_token = FALSE;	/* TRUE if we are reading a token */

	args[0] = cp = s;
	while (*s) {
		if (quoting) {
			if (*s == '\\' && *(s+1) == '"') { /* quoted quote */
				s++;	/* get past " */
				*cp++ = *s++;
			} else 	if (*s == '\"') { /* close quote */
				end_token
				quoting = FALSE;
			} else *cp++ = *s++; /* suck up anything */
		} else if (*s == '\"') { /* open quote */
			in_token = TRUE;
			quoting = TRUE;
			s++;
		} else if (isspace(*s)) {
			end_token
		} else {
			*cp++ = *s++;
			in_token = TRUE;
		}
	}
	end_token
	args[argi] = (char *) 0; /* terminate argv */
	execvp(args[0],args);
}
#endif /*0*/

static void
pty_stty(s,name)
char *s;		/* args to stty */
char *name;		/* name of pty */
{
#define MAX_ARGLIST 10240
	char buf[MAX_ARGLIST];	/* overkill is easier */
	RETSIGTYPE (*old)();	/* save old sigalarm handler */

#ifdef STTY_READS_STDOUT
	sprintf(buf,"/bin/stty %s > %s",s,name);
#else
	sprintf(buf,"/bin/stty %s < %s",s,name);
#endif
	old = signal(SIGCHLD, SIG_DFL);
	system(buf);
	signal(SIGCHLD, old);	/* restore signal handler */
}

int exp_dev_tty;	/* file descriptor to /dev/tty or -1 if none */
static int knew_dev_tty;/* true if we had our hands on /dev/tty at any time */

#if 0
#ifdef TIOCGWINSZ
static struct winsize winsize = {0, 0};
#endif
#if defined(TIOCGSIZE) && !defined(TIOCGWINSZ)
static struct ttysize winsize = {0, 0};
#endif
#endif

exp_tty exp_tty_original;

#define GET_TTYTYPE	0
#define SET_TTYTYPE	1
static void
ttytype(request,fd,ttycopy,ttyinit,s)
int request;
int fd;
		/* following are used only if request == SET_TTYTYPE */
int ttycopy;	/* true/false, copy from /dev/tty */
int ttyinit;	/* if true, initialize to sane state */
char *s;	/* stty args */
{
	if (request == GET_TTYTYPE) {
#ifdef POSIX
		if (-1 == tcgetattr(fd, &exp_tty_original)) {
#else
		if (-1 == ioctl(fd, TCGETS, (char *)&exp_tty_original)) {
#endif
			knew_dev_tty = FALSE;
			exp_dev_tty = -1;
		}
		exp_window_size_get(fd);
	} else {	/* type == SET_TTYTYPE */
		if (ttycopy && knew_dev_tty) {
#ifdef POSIX
			(void) tcsetattr(fd, TCSADRAIN, &exp_tty_original);
#else
			(void) ioctl(fd, TCSETS, (char *)&exp_tty_original);
#endif

			exp_window_size_set(fd);
		}

#ifdef __CENTERLINE__
#undef DFLT_STTY
#define DFLT_STTY "sane"
#endif

/* Apollo Domain doesn't need this */
#ifdef DFLT_STTY
		if (ttyinit) {
			/* overlay parms originally supplied by Makefile */
			debuglog("getptyslave: (default) stty %s\n",DFLT_STTY);
			pty_stty(DFLT_STTY,slave_name);
		}
#endif

		/* lastly, give user chance to override any terminal parms */
		if (s) {
			/* give user a chance to override any terminal parms */
			debuglog("getptyslave: (user-requested) stty %s\n",s);
			pty_stty(s,slave_name);
		}
	}
}

void
exp_init_pty()
{
#if !defined(HAVE_GETPTY) && !defined(HAVE__GETPTY) && !defined(HAVE_PTC) && !defined(HAVE_PTC_PTS) && !defined(HAVE_PTMX)
#ifdef HAVE_PTYM
	static char dummy;
	tty_bank =  &master_name[strlen("/dev/ptym/pty")];
	tty_num  =  &master_name[strlen("/dev/ptym/ptyX")];
	slave_bank = &slave_name[strlen("/dev/pty/tty")];
	slave_num  = &slave_name[strlen("/dev/pty/ttyX")];
#else
	tty_bank =  &master_name[strlen("/dev/pty")];
	tty_num  =  &master_name[strlen("/dev/ptyp")];
	tty_type =   &slave_name[strlen("/dev/")];
#endif

#endif /* HAVE_PTYM */


	exp_dev_tty = open("/dev/tty",O_RDWR);
	knew_dev_tty = (exp_dev_tty != -1);
	if (knew_dev_tty) ttytype(GET_TTYTYPE,exp_dev_tty,0,0,(char *)0);
}

#ifndef R_OK
/* 3b2 doesn't define these according to jthomas@nmsu.edu. */
#define R_OK 04
#define W_OK 02
#endif

int
getptymaster()
{
	char *hex, *bank;
	struct stat stat_buf;
	int master = -1;

#if defined(HAVE_PTMX) || defined(HAVE_PTMX_BSD)
#if defined(HAVE_PTMX_BSD)
        if ((master = open("/dev/ptmx_bsd", O_RDWR)) == -1) return(-1);
#else
	if ((master = open("/dev/ptmx", O_RDWR)) == -1) return(-1);
#endif
	if ((slave_name = (char *)ptsname(master)) == NULL || unlockpt(master) || grantpt(master)) {
		close(master);
		return(-1);
	}
	(void) ioctl(master,TIOCFLUSH,(char *)0);

	exp_pty_slave_name = slave_name;
	return(master);
#endif

#if defined(HAVE__GETPTY)		/* SGI needs it this way */
	slave_name = _getpty(&master, O_RDWR, 0600, 0);
	if (slave_name == NULL)
		return (-1);	
	exp_pty_slave_name = slave_name;
	return(master);
#endif

#if defined(HAVE_PTC) && !defined(HAVE__GETPTY)	/* old SGI, version 3 */
	master = open("/dev/ptc", O_RDWR);
	if (master >= 0) {
		int ptynum;

		if (fstat(master, &stat_buf) < 0) {
			close(master);
			return(-1);
		}
		ptynum = minor(stat_buf.st_rdev);
		sprintf(slave_name,"/dev/ttyq%d",ptynum);
	}
	exp_pty_slave_name = slave_name;
	return(master);
#endif

#if defined(HAVE_GETPTY) && !defined(HAVE__GETPTY)
	master = getpty(master_name, slave_name, O_RDWR);
	/* is it really necessary to verify slave side is usable? */
	exp_pty_slave_name = slave_name;
	return master;
#endif

#if defined(HAVE_PTC_PTS)
	master = open("/dev/ptc",O_RDWR);
	if (master >= 0) {
		/* never fails */
		slave_name = ttyname(master);
	}
	exp_pty_slave_name = slave_name;
	return(master);
#endif

#if !defined(HAVE_GETPTY) && !defined(HAVE__GETPTY) && !defined(HAVE_PTC) && !defined(HAVE_PTC_PTS) && !defined(HAVE_PTMX)
	if (exp_pty_test_start() == -1) return -1;

	for (bank = "pqrstuvwxyzPQRSTUVWXYZ";*bank;bank++) {
		*tty_bank = *bank;
		*tty_num = '0';
		if (stat(master_name, &stat_buf) < 0) break;
		for (hex = "0123456789abcdef";*hex;hex++) {
			*tty_num = *hex;
#ifdef HAVE_PTYM
			*slave_bank = *tty_bank;
			*slave_num = *tty_num;
#else
			strcpy(slave_name,master_name);
			*tty_type = 't';
#endif
			master = exp_pty_test(master_name,slave_name,*tty_bank,*tty_num);
			if (master >= 0) goto done;
		}
	}
 done:
#if 0
#ifdef HAVE_PTYTRAP
	/* Turn on trapping of close, open and */
	/* ioctl requests from the slave.*/
	{
		int enable = 1;
		ioctl(master, TIOCTRAP, &enable);
	}
#endif /* HAVE_PTYTRAP */
#endif

	exp_pty_test_end();
	exp_pty_slave_name = slave_name;
	return(master);
#endif
}

/* if slave is opened in a child, slave_control(1) must be executed after */
/*   master is opened (when child is opened is irrelevent) */
/* if slave is opened in same proc as master, slave_control(1) must executed */
/*   after slave is opened */
/*ARGSUSED*/
void
exp_slave_control(master,control)
int master;
int control;	/* if 1, enable pty trapping of close/open/ioctl */
{
#ifdef HAVE_PTYTRAP
	ioctl(master, TIOCTRAP, &control);
#endif /* HAVE_PTYTRAP */
}

int
getptyslave(ttycopy,ttyinit,stty_args)
int ttycopy;
int ttyinit;
char *stty_args;
{
	int slave, slave2;
	char buf[10240];

	if (0 > (slave = open(slave_name, O_RDWR))) return(-1);

#if defined(HAVE_PTMX_BSD)
        if (ioctl (slave, I_LOOK, buf) != 0)
                if (ioctl (slave, I_PUSH, "ldterm")) {
                        debuglog("ioctl(%s,I_PUSH,\"ldterm\") = %s\n",strerror(errno));
        }
#else
#if defined(HAVE_PTMX)
	if (ioctl(slave, I_PUSH, "ptem")) {
		debuglog("ioctl(%s,I_PUSH,\"ptem\") = %s\n",strerror(errno));
	}
	if (ioctl(slave, I_PUSH, "ldterm")) {
		debuglog("ioctl(%s,I_PUSH,\"ldterm\") = %s\n",strerror(errno));
	}
	if (ioctl(slave, I_PUSH, "ttcompat")) {
		debuglog("ioctl(%s,I_PUSH,\"ttcompat\") = %s\n",strerror(errno));
	}
#endif
#endif

	if (0 == slave) {
		/* if opened in a new process, slave will be 0 (and */
		/* ultimately, 1 and 2 as well) */

		/* duplicate 0 onto 1 to prepare for stty */
		fcntl(0,F_DUPFD,1);
	}

	ttytype(SET_TTYTYPE,slave,ttycopy,ttyinit,stty_args);

#if 0
#ifdef HAVE_PTYTRAP
	/* do another open, to tell master that slave is done fiddling */
	/* with pty and master does not have to wait to do further acks */
	if (0 > (slave2 = open(slave_name, O_RDWR))) return(-1);
	close(slave2);
#endif /* HAVE_PTYTRAP */
#endif

	(void) exp_pty_unlock();
	return(slave);
}

#ifdef HAVE_PTYTRAP
#include <sys/ptyio.h>
#include <sys/time.h>

/* This function attempts to deal with HP's pty interface.  This
function simply returns an indication of what was trapped (or -1 for
failure), the parent deals with the details.

Originally, I tried to just trap open's but that is not enough.  When
the pty is initialized, ioctl's are generated and if not trapped will
hang the child if no further trapping is done.  (This could occur if
parent spawns a process and then immediatley does a close.)  So
instead, the parent must trap the ioctl's.  It probably suffices to
trap the write ioctl's (and tiocsctty which some hp's need) -
conceivably, stty could be smart enough not to do write's if the tty
settings are already correct.  In that case, we'll have to rethink
this.

Suggestions from HP engineers encouraged.  I cannot imagine how this
interface was intended to be used!

*/
   
int
exp_wait_for_slave_open(fd)
int fd;
{
	fd_set excep;
	struct timeval t;
	struct request_info ioctl_info;
	int rc;
	int found = 0;

	int maxfds = sysconf(_SC_OPEN_MAX);

	t.tv_sec = 30;	/* 30 seconds */
	t.tv_usec = 0;

	FD_ZERO(&excep);
	FD_SET(fd,&excep);

	rc = select(maxfds,
		(SELECT_MASK_TYPE *)0,
		(SELECT_MASK_TYPE *)0,
		(SELECT_MASK_TYPE *)&excep,
		&t);
	if (rc != 1) {
		debuglog("spawned process never started, errno = %d\n",errno);
		return(-1);
	}
	if (ioctl(fd,TIOCREQCHECK,&ioctl_info) < 0) {
		debuglog("ioctl(TIOCREQCHECK) failed, errno = %d\n",errno);
		return(-1);
	}

	found = ioctl_info.request;

	debuglog("trapped pty op = %x",found);
	if (found == TIOCOPEN) {
		debuglog(" TIOCOPEN");
	} else if (found == TIOCCLOSE) {
		debuglog(" TIOCCLOSE");
	}

#ifdef TIOCSCTTY
	if (found == TIOCSCTTY) {
		debuglog(" TIOCSCTTY");
	}
#endif

	if (found & IOC_IN) {
		debuglog(" IOC_IN (set)");
	} else if (found & IOC_OUT) {
		debuglog(" IOC_OUT (get)");
	}

	debuglog("\n");

	if (ioctl(fd, TIOCREQSET, &ioctl_info) < 0) {
		debuglog("ioctl(TIOCREQSET) failed, errno = %d\n",errno);
		return(-1);
	}
	return(found);
}
#endif
