/*	npopen:  like popen, but grabs stderr, too
 *		written by John Hutchinson, heavily modified by Paul Fox
 *
 * $Log: npopen.c,v $
 * Revision 1.1  1994/02/01 03:29:35  jkh
 * Initial revision
 *
 * Revision 1.29  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.28  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.27  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.26  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.25  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.24  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.23  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.22  1993/04/09  13:41:01  pgf
 * include sys/wait.h on LINUX, to get prototype
 *
 * Revision 1.21  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.20  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.19  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.18  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.17  1993/02/16  20:53:45  pgf
 * look up "shell" variable every time, since it can change
 *
 * Revision 1.16  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.15  1992/12/04  09:21:52  foxharp
 * don't close the half of a child's io that we're _not_ using
 *
 * Revision 1.14  1992/12/03  00:32:59  foxharp
 * new system_SHELL and exec_sh_c routines
 *
 * Revision 1.13  1992/05/25  21:07:48  foxharp
 * extern func declarations moved to header
 *
 * Revision 1.12  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.11  1992/03/25  19:13:17  pgf
 * BSD portability changes
 *
 * Revision 1.10  1992/03/19  23:25:04  pgf
 * linux port, and default NOFILE to 20 (bogus)
 *
 * Revision 1.9  1991/11/18  08:33:25  pgf
 * added missing arg to strrchr
 *
 * Revision 1.8  1991/11/16  18:36:46  pgf
 * no #define for strrchr needed here
 *
 * Revision 1.7  1991/10/22  14:08:23  pgf
 * took out old ifdef BEFORE code
 *
 * Revision 1.6  1991/10/21  13:39:54  pgf
 * plugged file descriptor leak
 *
 * Revision 1.5  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.4
 * date: 1991/08/06 15:30:25;
 * took out setbuf(NULL)'s, as an experiment, on Dave's suggestion
 * 
 * revision 1.3
 * date: 1991/06/25 14:22:33;
 * defensinve checks against fdopen failure
 * 
 * revision 1.2
 * date: 1991/04/04 09:38:39;
 * support for bidirectional pipes
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:49;
 * initial vile RCS revision
 */

#include "estruct.h"
#include "edef.h"

#if UNIX

#include <sys/param.h>

#if LINUX
#include <sys/wait.h>
#endif

#if OPT_EVAL
#define	user_SHELL()	gtenv("shell")
#else
#define	user_SHELL()	getenv("SHELL")
#endif

#define R 0
#define W 1

static int pipe_pid;


FILE *
npopen (cmd, type)
char *cmd, *type;
{
	FILE *ff;

	if (*type != 'r' && *type != 'w')
		return NULL;

	if (*type == 'r') {
		if (inout_popen(&ff, (FILE **)0, cmd) != TRUE)
			return NULL;
		return ff;
	} else {
		if (inout_popen((FILE **)0, &ff, cmd) != TRUE)
			return NULL;
		return ff;
	}
}

int
inout_popen(fr, fw, cmd)
FILE **fr, **fw;
char *cmd;
{
	int rp[2];
	int wp[2];
	

	if (pipe(rp))
		return FALSE;
	if (pipe(wp))
		return FALSE;
		
	pipe_pid = softfork();
	if (pipe_pid < 0)
		return FALSE;

	if (pipe_pid) { /* parent */

		if (fr) {
			*fr = fdopen (rp[0], "r");
			if (*fr == NULL) {
				(void)fprintf(stderr,"fdopen r failed\n");
				abort();
			}
		} else {
			(void)close(rp[0]);
		}
		(void) close (rp[1]);

		if (fw) {
			*fw = fdopen (wp[1], "w");
			if (*fw == NULL) {
				(void)fprintf(stderr,"fdopen w failed\n");
				abort();
			}
		} else {
			(void)close(wp[1]);
		}
		(void) close (wp[0]);
		return TRUE;

	} else {			/* child */

		if (fw) {
			(void)close (0);
			if (dup (wp[0]) != 0) {
				write(2,"dup 0 failed\r\n",15);
				exit(-1);
			}
		}
		(void) close (wp[1]);
		if (fr) {
			(void)close (1);
			if (dup (rp[1]) != 1) {
				write(2,"dup 1 failed\r\n",15);
				exit(-1);
			}
			(void)close (2);
			if (dup (rp[1]) != 2) {
				write(1,"dup 2 failed\r\n",15);
				exit(-1);
			}
		} else {
			(void) close (rp[1]);
		}
		(void) close (rp[0]);
		exec_sh_c(cmd);

	}
	return TRUE;
}

void
npclose (fp)
FILE *fp;
{
	int child;
	(void)fflush(fp);
	(void)fclose(fp);
	while ((child = wait ((int *)0)) != pipe_pid) {
		if (child < 0 && errno == EINTR) {
			(void) kill (SIGKILL, pipe_pid);
		}
	}
}

void
exec_sh_c(cmd)
char *cmd;
{
	char *sh, *shname;
	int i;

#ifndef NOFILE
# define NOFILE 20
#endif
	/* Make sure there are no upper inherited file descriptors */
	for (i = 3; i < NOFILE; i++)
		(void) close (i);

	if ((sh = user_SHELL()) == NULL || *sh == '\0') {
		sh = "/bin/sh";
		shname = "sh";
	} else {
		shname = last_slash(sh);
		if (shname == NULL) {
			shname = sh;
		} else {
			shname++;
			if (*shname == '\0')
				shname = sh;
		}
	}

	if (cmd)
		(void) execlp (sh, shname, "-c", cmd, 0);
	else
		(void) execlp (sh, shname, 0);
	write(2,"exec failed\r\n",14);
	exit (-1);
}

int
system_SHELL(cmd)
char *cmd;
{
	int cpid;

	cpid = softfork();
	if (cpid < 0) {
		write(2,"cannot fork\n",13);
		return cpid;
	}

	if (cpid) { /* parent */
		int child;
		beginDisplay;
		while ((child = wait ((int *)0)) != cpid) {
			if (child < 0 && errno == EINTR) {
				(void) kill (SIGKILL, cpid);
			}
		}
		endofDisplay;
		return 0;
	} else {
		exec_sh_c(cmd);
		write(2,"cannot exec\n",13);
		return -1;
	}

}

int
softfork()
{
	/* Try & fork 5 times, backing off 1, 2, 4 .. seconds each try */
	int fpid;
	int tries = 5;
	unsigned slp = 1;

	while ((fpid = fork ()) < 0) {
		if (--tries == 0)
			return -1;
		(void) sleep (slp);
		slp <<= 1;
	}
	return fpid;
}

#endif

#if MSDOS
#include <fcntl.h>		/* defines O_RDWR */
#include <io.h>			/* defines 'dup2()', etc. */

static	int	createTemp P(( char * ));
static	void	deleteTemp P(( void ));
static	void	closePipe P(( FILE *** ));
static	FILE *	readPipe P(( char *, int, int ));

static	FILE **	myPipe;		/* current pipe-file pointer */
static	FILE **	myWrtr;		/* write-pipe pointer */
static	char *	myName[2];	/* name of temporary file for pipe */
static	char *	myCmds;		/* command to execute on read-pipe */
static	int	myRval;		/* return-value of 'system()' */

static int
createTemp (type)
char	*type;
{
	register int n = (*type == 'r');
	register int fd;

#if WATCOM
	myName[n] = tmpnam((char *)0);
#else
	myName[n] = tempnam(TMPDIR, type);
#endif
	if (myName[n] == 0)
		return -1;
	(void)close(creat(myName[n], 0666));
	if ((fd = open(myName[n], O_RDWR)) < 0) {
		deleteTemp();
		return -1;
	}
	return fd;
}

static void
deleteTemp ()
{
	register int n;

	for (n = 0; n < 2; n++) {
		if (myName[n] != 0) {
			(void)unlink(myName[n]);
			FreeAndNull(myName[n]);
		}
	}
}

static void
closePipe(pp)
FILE	***pp;
{
	if (*pp != 0) {
		if (**pp != 0) {
			(void)fclose(**pp);
			**pp = 0;
		}
		*pp = 0;
	}
}

static FILE *
readPipe(cmd, in, out)
char	*cmd;
int	in;
int	out;
{
	/* save and redirect stdin, stdout, and stderr */
	int	old0 = dup(0);
	int	old1 = dup(1);
	int	old2 = dup(2);

	if (in >= 0)	{
		dup2(in, 0);
		close(in);
	}
	dup2(out, 1);
	dup2(out, 2);

	myRval = system(cmd);

	/* restore old std... */
	dup2(old0, 0); close(old0);
	dup2(old1, 1); close(old1);
	dup2(old2, 2); close(old2);

	/* rewind command output */
	lseek(out, 0L, 0);
	return fdopen(out, "r");
}

FILE *
npopen (cmd, type)
char *cmd, *type;
{
	FILE *ff;

	if (*type == 'r') {
		if (inout_popen(&ff, (FILE **)0, cmd) == TRUE)
			return ff;
	} else if (*type == 'w') {
		if (inout_popen((FILE **)0, &ff, cmd) == TRUE)
			return ff;
	}
	mlerror("pipe");
	return NULL;
}

/*
 * Create pipe with either write- and/or read-semantics.  Fortunately for us,
 * on MSDOS, we don't need both at the same instant.
 */
int
inout_popen(fr, fw, cmd)
FILE **fr, **fw;
char *cmd;
{
	char	*type = (fw != 0) ? "w" : "r";
	FILE	*pp;
	int	fd;

	/* Create the file that will hold the pipe's content */
	if ((fd = createTemp(type)) < 0)
		return FALSE;

	if (fw == 0) {
		*fr = pp = readPipe(cmd, -1, fd);
		myWrtr = 0;
		myPipe = 0;
		myCmds = 0;
	} else {
		*fw = pp = fdopen(fd, type);
		myPipe = fr;
		myWrtr = fw;
		myCmds = strmalloc(cmd);
	}
	return (pp != 0);
}

/*
 * If we were writing to a pipe, invoke the read-process with stdin set to the
 * temporary-file.  This is used in the filter-buffer code, which needs both
 * read- and write-pipes.
 */
void
npflush ()
{
	if (myCmds != 0) {
		if (myWrtr != 0) {
			(void)fflush(*myWrtr);
#if UNUSED
			fclose(*myWrtr);
			*myWrtr = fopen(myName[0], "r");
#endif
			rewind(*myWrtr);
			*myPipe = readPipe(myCmds, fileno(*myWrtr), createTemp("r"));
		}
		FreeAndNull(myCmds);
	}
}

void
npclose (fp)
FILE *fp;
{
	closePipe(&myWrtr);
	closePipe(&myPipe);
	deleteTemp();
}

int
softfork()	/* dummy function to make filter-region work */
{
	return 0;
}
#endif
