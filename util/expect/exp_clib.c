/* exp_clib.c - top-level functions in the expect C library, libexpect.a

Written by: Don Libes, libes@cme.nist.gov, NIST, 12/3/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#include "expect_cf.h"
#include <stdio.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#ifdef HAVE_SYS_FCNTL_H
#  include <sys/fcntl.h>
#else
#  include <fcntl.h>
#endif
#include <signal.h>
/*#include <memory.h> - deprecated - ANSI C moves them into string.h */
#include "string.h"
#include <varargs.h>
#include <errno.h>
#include "exp_rename.h"
#define EXP_DEFINE_FNS
#include "expect.h"
#include "exp_int.h"

#include "exp_printify.h"

/*#ifndef _STDLIB*/
/*char *malloc();*/
/*void exit();*/
/*#endif*/
#include "stdlib.h"
/*extern char *realloc();*/

#define EXP_MATCH_MAX	2000
/* public */
char *exp_buffer = 0;
char *exp_buffer_end = 0;
char *exp_match = 0;
char *exp_match_end = 0;
int exp_match_max = EXP_MATCH_MAX;	/* bytes */
int exp_full_buffer = FALSE;		/* don't return on full buffer */
int exp_remove_nulls = TRUE;
int exp_timeout = 10;			/* seconds */
int exp_pty_timeout = 5;		/* seconds - see CRAY below */
int exp_autoallocpty = TRUE;		/* if TRUE, we do allocation */
int exp_pty[2];				/* master is [0], slave is [1] */
int exp_pid;
char *exp_stty_init = 0;		/* initial stty args */
int exp_ttycopy = TRUE;			/* copy tty parms from /dev/tty */
int exp_ttyinit = TRUE;			/* set tty parms to sane state */
int exp_console = FALSE;		/* redirect console */

jmp_buf exp_readenv;		/* for interruptable read() */
int exp_reading = FALSE;	/* whether we can longjmp or not */

void debuglog();
int getptymaster();
int getptyslave();
int Exp_StringMatch();

#define sysreturn(x)	return(errno = x, -1)

void exp_init_pty();

extern char *tclRegexpError;	/* declared in tclInt.h */

/* returns fd of master side of pty */
int
exp_spawnv(file,argv)
char *file;
char *argv[];	/* some compiler complains about **argv? */
{
	int cc;
	int ttyfd;
	int sync_fds[2];
	char sync_byte;
#ifdef PTYTRAP_DIES
	int slave_write_ioctls = 1;
		/* by default, slave will be write-ioctled this many times */
#endif

	static int first_time = TRUE;

	if (first_time) {
		first_time = FALSE;
		exp_init_pty();
	}

	if (!file || !argv) sysreturn(EINVAL);
	if (!argv[0] || strcmp(file,argv[0])) {
		debuglog("expect: warning: file (%s) != argv[0] (%s)\n",
			file,
			argv[0]?argv[0]:"");
	}

#ifdef PTYTRAP_DIES
/* any extraneous ioctl's that occur in slave must be accounted for
when trapping, see below in child half of fork */
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(sun) && !defined(hp9000s300)
	slave_write_ioctls++;
#endif
#endif /*PTYTRAP_DIES*/

	if (exp_autoallocpty) {
		if (0 > (exp_pty[0] = getptymaster())) sysreturn(ENODEV);
	}
	fcntl(exp_pty[0],F_SETFD,1);	/* close on exec */
#ifdef PTYTRAP_DIES
	exp_slave_control(exp_pty[0],1);*/
#endif

	pipe(sync_fds);

	if ((exp_pid = fork()) == -1) return(-1);
	if (exp_pid) {
		/* parent */
		close(sync_fds[1]);
		if (!exp_autoallocpty) close(exp_pty[1]);

#ifdef PTYTRAP_DIES
#ifdef HAVE_PTYTRAP
		if (exp_autoallocpty) {
			/* trap initial ioctls in a feeble attempt to not */
			/* block the initially.  If the process itself */
			/* ioctls /dev/tty, such blocks will be trapped */
			/* later during normal event processing */

			while (slave_write_ioctls) {
				int cc;

				cc = exp_wait_for_slave_open(exp_pty[0]);
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(sun) && !defined(hp9000s300)
				if (cc == TIOCSCTTY) slave_write_ioctls = 0;
#endif
				if (cc & IOC_IN) slave_write_ioctls--;
				else if (cc == -1) {
					printf("failed to trap slave pty");
					return -1;
				}
			}
		}
#endif
#endif PTYTRAP_DIES

		/*
		 * wait for slave to initialize pty before allowing
		 * user to send to it
		 */ 

		debuglog("parent: waiting for sync byte\r\n");
		cc = read(sync_fds[0],&sync_byte,1);
		if (cc == -1) {
			fprintf(stderr,"parent sync byte read: %s\r\n",strerror(errno));
			exit(-1);
		}

		/* turn on detection of eof */
		/* this should be done as quickly as possible.  Conceivably */
		/* child could die quickly enough that we are not ready */
		/* for the eof.  Fortunately, the child has a lot of work */
		/* to do so this seems unlikely. */
		exp_slave_control(exp_pty[0],1);

		close(sync_fds[0]);

		return(exp_pty[0]);
	}

	/* child process - do not return from here!  all errors must exit() */

	close(sync_fds[0]);

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

#ifdef TIOCNOTTY
	ttyfd = open("/dev/tty", O_RDWR);
	if (ttyfd >= 0) {
		(void) ioctl(ttyfd, TIOCNOTTY, (char *)0);
		(void) close(ttyfd);
	}
#endif /* TIOCNOTTY */

#endif /* SYSV3 */
#endif /* DO_SETSID */


	if (exp_autoallocpty) {
	    close(0);
	    close(1);
	    /* leave 2 around awhile for stderr-related stuff */

	    /* since we closed fd 0, open of pty slave must return fd 0 */

	    if (0 > (exp_pty[1] = getptyslave(exp_ttycopy,exp_ttyinit,
						exp_stty_init))) {
		fprintf(stderr,"open(slave pty): %s\n",strerror(errno));
		exit(-1);
	    }
	    /* sanity check */
	    if (exp_pty[1] != 0) {
		fprintf(stderr,"getptyslave: slave = %d but expected 0\n",
								exp_pty[1]);
		exit(-1);
	    }
	} else {
		if (exp_pty[1] != 0) {
			close(0);
			fcntl(exp_pty[1],F_DUPFD,0);
		}
		close(1);
		fcntl(0,F_DUPFD,1);
		close(exp_pty[1]);
	}



/* The test for hpux may have to be more specific.  In particular, the */
/* code should be skipped on the hp9000s300 and hp9000s720 (but there */
/* is no documented define for the 720!) */

#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(sun) && !defined(hpux)
	/* 4.3+BSD way to acquire controlling terminal */
	/* according to Stevens - Adv. Prog..., p 642 */
	if (ioctl(0,TIOCSCTTY,(char *)0) < 0) {
		fprintf(stderr,"failed to get controlling terminal using TIOCSCTTY");
		exit(-1);
	}
#endif

#ifdef CRAY
 	(void) setsid();
 	(void) ioctl(0,TCSETCTTY,0);
 	(void) close(0);
 	if (open("/dev/tty", O_RDWR) < 0) {
 		fprintf(stderr,"open(/dev/tty): %s\r\n",strerror(errno));
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
		fprintf(stderr,"second fork: %s\r\n",strerror(errno));
		exit(-1);
	}

	if (pid) {
 		/* Intermediate process. */
		int status;
		int timeout;
		char *t;

		/* How long should we wait? */
		timeout = exp_pty_timeout;

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

#ifdef TIOCCONS
	if (exp_console) {
		int on = 1;
		if (ioctl(0,TIOCCONS,(char *)&on) == -1) {
			fprintf(stderr, "spawn %s: cannot open console, check permissions of /dev/console\n",argv[0]);
			exit(-1);
		}
	}
#endif /* TIOCCONS */

	/* by now, fd 0 and 1 point to slave pty, so fix 2 */
	close(2);
	fcntl(0,F_DUPFD,2);	/* duplicate 0 onto 2 */

	/* tell parent that we are done setting up pty */
	/* The actual char sent back is irrelevant. */

	/* debuglog("child: telling parent that pty is initialized\r\n");*/
	cc = write(sync_fds[1]," ",1);
	if (cc == -1) {
		fprintf(stderr,"child: sync byte write: %s\r\n",strerror(errno));
		exit(-1);
	}
	close(sync_fds[1]);

	/* (possibly multiple) masters are closed automatically due to */
	/* earlier fcntl(,,CLOSE_ON_EXEC); */

	/* just in case, allow user to explicitly close other files */
	if (exp_close_in_child) (*exp_close_in_child)();

        (void) execvp(argv[0],argv);
	/* Unfortunately, by now we've closed fd's to stderr, logfile and
		debugfile.
	   The only reasonable thing to do is to send back the error as
	   part of the program output.  This will be picked up in an
	   expect or interact command.
	*/
	fprintf(stderr,"execvp(%s): %s\n",file,strerror(errno));
	exit(-1);
	/*NOTREACHED*/
}

/* returns fd of master side of pty */
/*VARARGS*/
int
exp_spawnl(va_alist)
va_dcl
{
	va_list args;
	int i;
	char *arg, **argv;

	va_start(args);
	for (i=0;;i++) {
		arg = va_arg(args,char *);
		if (!arg) break;
	}
	va_end(args);
	if (i == 0) sysreturn(EINVAL);
	if (!(argv = (char **)malloc((i+1)*sizeof(char *)))) sysreturn(ENOMEM);
	va_start(args);
	for (i=0;;i++) {
		argv[i] = va_arg(args,char *);
		if (!argv[i]) break;
	}
	i = exp_spawnv(argv[0],argv+1);
	free((char *)argv);
	return(i);
}

/* remove nulls from s.  Initially, the number of chars in s is c, */
/* not strlen(s).  This count does not include the trailing null. */
/* returns number of nulls removed. */
static int
rm_nulls(s,c)
char *s;
int c;
{
	char *s2 = s;	/* points to place in original string to put */
			/* next non-null character */
	int count = 0;
	int i;

	for (i=0;i<c;i++,s++) {
		if (0 == *s) {
			count++;
			continue;
		}
		if (count) *s2 = *s;
		s2++;
	}
	return(count);
}

static int i_read_errno;/* place to save errno, if i_read() == -1, so it
			   doesn't get overwritten before we get to read it */

/*ARGSUSED*/
static void
sigalarm_handler(n)
int n;			/* signal number, unused by us */
{
#ifdef REARM_SIG
	signal(SIGALRM,sigalarm_handler);
#endif

	longjmp(exp_readenv,1);
}

/* interruptable read */
static int
i_read(fd,fp,buffer,length,timeout)
int fd;
FILE *fp;
char *buffer;
int length;
int timeout;
{
	int cc = -2;

	/* since setjmp insists on returning 1 upon longjmp(,0), */
	/* longjmp(,2 (EXP_RESTART)) instead. */

	/* no need to set alarm if -1 (infinite) or 0 (poll with */
	/* guaranteed data) */

	if (timeout > 0) alarm(timeout);

	/* restart read if setjmp returns 0 (first time) or 2 (EXP_RESTART). */
	/* abort if setjmp returns 1 (EXP_ABORT). */
	if (EXP_ABORT != setjmp(exp_readenv)) {
		exp_reading = TRUE;
		if (fd != -1) cc = read(fd,buffer,length);
		else {
			int c;
			c = getc(fp);
			if (c == EOF) {
				if (feof(fp)) cc = 0;
				else cc = -1;
			} else {
				buffer[0] = c;
				cc = 1;
			}
		}
#if 0
		/* can't get fread to return early! */
		else {
			if (!(cc = fread(buffer,1,length,fp))) {
				if (ferror(fp)) cc = -1;
			}
		}
#endif
		i_read_errno = errno;	/* errno can be overwritten by the */
					/* time we return */
	}
	exp_reading = FALSE;

	if (timeout > 0) alarm(0);
	return(cc);
}

static unsigned int bufsiz = 2*EXP_MATCH_MAX;

/* I tried really hard to make the following two functions share the code */
/* that makes the ecase array, but I kept running into a brick wall when */
/* passing var args into the funcs and then again into a make_cases func */
/* I would very much appreciate it if someone showed me how to do it right */

/* takes triplets of args, with a final "exp_last" arg */
/* triplets are type, pattern, and then int to return */
/* returns negative value if error (or EOF/timeout) occurs */
/* some negative values can also have an associated errno */

/* the key internal variables that this function depends on are:
	exp_buffer
	exp_buffer_end
	exp_match_end
*/
static int
expectv(fd,fp,ecases)
int fd;
FILE *fp;
struct exp_case *ecases;
{
	int cc = 0;		/* number of chars returned in a single read */
	int buf_length;		/* numbers of chars in exp_buffer */
	int old_length;		/* old buf_length */

	unsigned int new_siz;	/* this is just match_max*2 for efficiency */
	char *new_buf;
	struct exp_case *ec;	/* points to current ecase */

	time_t current_time;	/* current time (when we last looked)*/
	time_t end_time;	/* future time at which to give up */
	int remtime;		/* remaining time in timeout */

	if (exp_buffer == 0) {
		if (!(exp_buffer = malloc((unsigned)(bufsiz+1))))
			sysreturn(ENOMEM);
		exp_buffer_end = exp_buffer;
		exp_match_end = exp_buffer;
	}

	buf_length = exp_buffer_end - exp_match_end;
	if (buf_length) {
		/*
		 *take end of previous match to end of buffer
		 * and copy to beginning of buffer
		 */
		memcpy(exp_buffer,exp_match_end,buf_length);
	}			
	exp_buffer_end = exp_buffer + buf_length;
	*exp_buffer_end = '\0';

	if (!ecases) sysreturn(EINVAL);

	/* compile if necessary */
	for (ec=ecases;ec->type != exp_end;ec++) {
		if ((ec->type == exp_regexp) && !ec->re) {
			tclRegexpError = 0;
			if (!(ec->re = TclRegComp(ec->pattern))) {
				fprintf(stderr,"regular expression %s is bad: %s",ec->pattern,tclRegexpError);
				sysreturn(EINVAL);
			  }
		  }
	}

	/* get the latest buffer size.  Double the user input for two */
	/* reasons.  1) Need twice the space in case the match */
	/* straddles two bufferfuls, 2) easier to hack the division by */
	/* two when shifting the buffers later on */
	if (bufsiz != (new_siz = 2*exp_match_max)) {
		if (0 == (new_buf = realloc(exp_buffer,new_siz+1)))
			sysreturn(ENOMEM);
		bufsiz = new_siz;
		exp_buffer = new_buf;
		exp_buffer_end = exp_buffer + buf_length;
	}

	if (exp_timeout != -1) signal(SIGALRM,sigalarm_handler);

	/* remtime and current_time updated at bottom of loop */
	remtime = exp_timeout;

	time(&current_time);
	/* if user sets timeout to 0 (i.e. poll), this indicates */
	/* we should do one read and return */
	/* Hopefully, the buffer is big enough and will get it all! */
	end_time = current_time + remtime;

	for (;;) {
		/* when buffer fills, copy second half over first and */
		/* continue, so we can do matches over multiple buffers */
		if (buf_length == bufsiz) {
			int first_half, second_half;

			if (exp_full_buffer) return EXP_FULLBUFFER;

			first_half = bufsiz/2;
			second_half = bufsiz - first_half;

			memcpy(exp_buffer,exp_buffer+first_half,second_half);
			buf_length = second_half;
			exp_buffer_end = exp_buffer + second_half;
		}

		if ((exp_timeout != -1) && (remtime < 0)) {
			return EXP_TIMEOUT;
		} else {
			cc = i_read(fd,fp,
					exp_buffer_end,
					bufsiz - buf_length,
					remtime);
		}

		if (cc == 0) return(EXP_EOF);		/* normal EOF */
		else if (cc == -1) {			/* abnormal EOF */
			/* ptys produce EIO upon EOF - sigh */
			if (i_read_errno == EIO) {
				/* convert to EOF indication */
				return(EXP_EOF);
			}
			sysreturn(i_read_errno);
		} else if (cc == -2) return(EXP_TIMEOUT);

		old_length = buf_length;
		buf_length += cc;
		exp_buffer_end += buf_length;

		if (logfile_all || (loguser && logfile)) {
			fwrite(exp_buffer + old_length,1,cc,logfile);
		}
		if (loguser) fwrite(exp_buffer + old_length,1,cc,stdout);
		if (debugfile) fwrite(exp_buffer + old_length,1,cc,debugfile);

		/* if we wrote to any logs, flush them */
		if (debugfile) fflush(debugfile);
		if (loguser) {
			fflush(stdout);
			if (logfile) fflush(logfile);
		}

		/* remove nulls from input, so we can use C-style strings */
		/* doing it here lets them be sent to the screen, just */
		/*  in case they are involved in formatting operations */
		if (exp_remove_nulls) {
			buf_length -= rm_nulls(exp_buffer + old_length, cc);
		}
		/* cc should be decremented as well, but since it will not */
		/* be used before being set again, there is no need */
		exp_buffer_end = exp_buffer + buf_length;
		*exp_buffer_end = '\0';

		debuglog("expect: does {%s} match ",exp_printify(exp_buffer));
		/* pattern supplied */
		for (ec=ecases;ec->type != exp_end;ec++) {
			int matched = -1;

			debuglog("{%s}? ",exp_printify(ec->pattern));
			if (ec->type == exp_glob) {
				int offset;
				matched = Exp_StringMatch(exp_buffer,ec->pattern,&offset);
				if (matched >= 0) {
					exp_match = exp_buffer + offset;
					exp_match_end = exp_match + matched;
				}
			} else if (ec->type == exp_exact) {
				char *p = strstr(exp_buffer,ec->pattern);
				if (p) {
					exp_match = p;
					exp_match_end = p + strlen(ec->pattern);
				}
			} else if (ec->type == exp_null) {
				char *p;

				for (p=exp_buffer;p<exp_buffer_end;p++) {
					if (*p == 0) {
						matched = 1;
						exp_match = p;
						exp_match_end = p+1;
					}
				}
			} else {
				tclRegexpError = 0;
				if (TclRegExec(ec->re,exp_buffer,exp_buffer)) {
					matched = 1;
					exp_match = ec->re->startp[0];
					exp_match_end = ec->re->endp[0];
				} else if (tclRegexpError) {
			    		fprintf(stderr,"r.e. match (pattern %s) failed: %s",ec->pattern,tclRegexpError);
				}
			}

			if (matched != -1) {
				debuglog("yes\nexp_buffer is {%s}\n",
						exp_printify(exp_buffer));
				return(ec->value);
			} else debuglog("no\n");
		}

		/* indicate poll failed */
		if (exp_timeout == 0) return EXP_TIMEOUT;

		if (exp_timeout != -1) {
			time(&current_time);
			remtime = end_time - current_time;
		}
	}
}

int
exp_fexpectv(fp,ecases)
FILE *fp;
struct exp_case *ecases;
{
	return(expectv(-1,fp,ecases));
}

int
exp_expectv(fd,ecases)
int fd;
struct exp_case *ecases;
{
	return(expectv(fd,(FILE *)0,ecases));
}

/*VARARGS*/
int
exp_expectl(va_alist)
va_dcl
{
	va_list args;
	int fd;
	struct exp_case *ec, *ecases;
	int i;
	enum exp_type type;

	va_start(args);
	fd = va_arg(args,int);
	/* first just count the arg sets */
	for (i=0;;i++) {
		type = va_arg(args,enum exp_type);
		if (type == exp_end) break;

		/* Ultrix 4.2 compiler refuses enumerations comparison!? */
		if ((int)type < 0 || (int)type >= (int)exp_bogus) {
			fprintf(stderr,"bad type (set %d) in exp_expectl\n",i);
			sysreturn(EINVAL);
		}

		va_arg(args,char *);		/* COMPUTED BUT NOT USED */
		if (type == exp_compiled) {
			va_arg(args,regexp *);	/* COMPUTED BUT NOT USED */
		}
		va_arg(args,int);		/* COMPUTED BUT NOT USED*/
	}
	va_end(args);

	if (!(ecases = (struct exp_case *)
				malloc((1+i)*sizeof(struct exp_case))))
		sysreturn(ENOMEM);

	/* now set up the actual cases */
	va_start(args);
	va_arg(args,int);		/*COMPUTED BUT NOT USED*/
	for (ec=ecases;;ec++) {
		ec->type = va_arg(args,enum exp_type);
		if (ec->type == exp_end) break;
		ec->pattern = va_arg(args,char *);
		if (ec->type == exp_compiled) {
			ec->re = va_arg(args,regexp *);
		} else {
			ec->re = 0;
		}
		ec->value = va_arg(args,int);
	}
	va_end(args);
	i = expectv(fd,(FILE *)0,ecases);

	for (ec=ecases;ec->type != exp_end;ec++) {
		/* free only if regexp and we compiled it for user */
		if (ec->type == exp_regexp) {
			free((char *)ec->re);
		}
	}
	free((char *)ecases);
	return(i);
}

int
exp_fexpectl(va_alist)
va_dcl
{
	va_list args;
	FILE *fp;
	struct exp_case *ec, *ecases;
	int i;
	enum exp_type type;

	va_start(args);
	fp = va_arg(args,FILE *);
	/* first just count the arg-pairs */
	for (i=0;;i++) {
		type = va_arg(args,enum exp_type);
		if (type == exp_end) break;

		/* Ultrix 4.2 compiler refuses enumerations comparison!? */
		if ((int)type < 0 || (int)type >= (int)exp_bogus) {
			fprintf(stderr,"bad type (set %d) in exp_expectl\n",i);
			sysreturn(EINVAL);
		}

		va_arg(args,char *);		/* COMPUTED BUT NOT USED */
		if (type == exp_compiled) {
			va_arg(args,regexp *);	/* COMPUTED BUT NOT USED */
		}
		va_arg(args,int);		/* COMPUTED BUT NOT USED*/
	}
	va_end(args);

	if (!(ecases = (struct exp_case *)
					malloc((1+i)*sizeof(struct exp_case))))
		sysreturn(ENOMEM);

	va_start(args);
	va_arg(args,FILE *);		/*COMPUTED, BUT NOT USED*/
	for (ec=ecases;;ec++) {
		ec->type = va_arg(args,enum exp_type);
		if (ec->type == exp_end) break;
		ec->pattern = va_arg(args,char *);
		if (ec->type == exp_compiled) {
			ec->re = va_arg(args,regexp *);
		} else {
			ec->re = 0;
		}
		ec->value = va_arg(args,int);
	}
	va_end(args);
	i = expectv(-1,fp,ecases);

	for (ec=ecases;ec->type != exp_end;ec++) {
		/* free only if regexp and we compiled it for user */
		if (ec->type == exp_regexp) {
			free((char *)ec->re);
		}
	}
	free((char *)ecases);
	return(i);
}

/* like popen(3) but works in both directions */
FILE *
exp_popen(program)
char *program;
{
	FILE *fp;
	int ec;

	if (0 > (ec = exp_spawnl("sh","sh","-c",program,(char *)0))) return(0);
	if (!(fp = fdopen(ec,"r+"))) return(0);
	setbuf(fp,(char *)0);
	return(fp);
}

int
exp_disconnect()
{
	int ttyfd;

#ifndef EALREADY
#define EALREADY 37
#endif

	/* presumably, no stderr, so don't bother with error message */
	if (exp_disconnected) sysreturn(EALREADY);
	exp_disconnected = TRUE;

	freopen("/dev/null","r",stdin);
	freopen("/dev/null","w",stdout);
	freopen("/dev/null","w",stderr);

#ifdef POSIX
	setsid();
#else
#ifdef SYSV3
	/* put process in our own pgrp, and lose controlling terminal */
	setpgrp();
	signal(SIGHUP,SIG_IGN);
	if (fork()) exit(0);	/* first child exits (as per Stevens, */
	/* UNIX Network Programming, p. 79-80) */
	/* second child process continues as daemon */
#else /* !SYSV3 */
#ifdef MIPS_BSD
	/* required on BSD side of MIPS OS <jmsellen@watdragon.waterloo.edu> */
#	include <sysv/sys.s>
	syscall(SYS_setpgrp);
#endif
	setpgrp(0,getpid());	/* put process in our own pgrp */
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
#endif /* POSIX */
	return(0);
}
