#ifndef lint
static	char	*rcsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/batch.c,v 1.1 1993/07/19 20:04:30 nate Exp $";
#endif
/*
 * Batch subroutine for Cnews.
 * Cooperates with C news input subsystem.
 *	newsboot must be told to run partial batches left at a crash.
 *
***************************************************************************
This work in its current form is Copyright 1989 Stan Barber
and is based on the work of Henry Spencer and Geoff Collyer at the University
of Toronto. This software may be distributed freely as long as no profit is
made from such distribution and this notice is reproducted in whole.
***************************************************************************
This software is provided on an "as is" basis with no guarantee of 
usefulness or correctness of operation for any purpose, intended or
otherwise. The author is in no way liable for this software's performance
or any damage it may cause to any data of any kind anywhere.
***************************************************************************
*/
#include "common.h"
#include <signal.h>
#ifdef sparc
#include <vfork.h>
#endif
#ifdef BATCHED_INPUT
#define YES 1
#define NO 0

/* imports */
extern time_t time();
extern char *malloc(), *mktemp(), *index(), *rindex();
/* forwards */
static char *strsave();
#ifdef XFER_TIMEOUT
static int xfer_timeout();
#endif
static int cpstdin();
static int appbatch();
static int enqueue();

/* private data */
static char tempfile[256];
static int xfer_lines, old_xfer_lines;

static char art[COPYSIZE];		/* entire article, if it fits */
static char *endart = art;		/* points just past end of article */
static int incore = YES;

#ifdef NONEWSRUN
static int uniq = 0;                    /* unique counter for this process */
static int in_batchdir = NO;

#endif /* NONEWSRUN */
static struct batch_file {
	char *name;
	FILE *file;
	char isopen;
	time_t start;			/* time of creation */
	off_t size;			/* current size */
	int arts;			/* number of articles */
} btch = { NULL, NULL, NO, 0, 0 };

/*
 * stash stdin (up to ".") on the end of the batch input file.
 * kick newsrun if the batch is non-empty and too big or too old.
 *
 * Parameters:
 *	"cont_code" is the response code to transmit on successful startup.
 *	"err_code" is the response code to transmit when something goes wrong.
 *
 * Returns: -1 on non-zero return from child, 0 on error before fork/exec, 1 else.
 * Side effects: Creates and removes temporary file; accepts input from client.
 *		Can time out if XFER_TIMEOUT is defined.
 */
int
batch_input_article(cont_code, err_code, errbuf, msg_id)
int cont_code, err_code;
char *errbuf;
char *msg_id;
{
	int status = 1;			/* okay status */

	/* protect locking */
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);

#ifdef NONEWSRUN
	/* This really should be done in main.c so that we don't have to
	   check each time we get a new article - sigh */

	if (!in_batchdir)
		if (chdir(INDIR) < 0)
			syslog(LOG_ERR, "chdir(%s) failed", INDIR);
		else
			in_batchdir = YES;

#endif /* NONEWSRUN */
	if (btch.name == NULL) {
		/* BATCH_FILE may trigger unprivileged() */
		btch.name = mktemp(strsave(BATCH_FILE));
	}
	if (btch.name == NULL)
		return 0;
	tempfile[0] = '\0';
#ifdef UMASK
	(void) umask(UMASK);
#endif
	/* may create tempfile */
	if (!cpstdin(cont_code, err_code, errbuf, msg_id))
		return 0;
#ifdef POSTER
	if (tempfile[0])
		(void) chown(tempfile, uid_poster, gid_poster);
#endif
	status = appbatch();
	btch.arts++;
	if (tempfile[0] != '\0')
		(void) unlink(tempfile);
	if (status == 1 && oktorunbatch())
		status = enqueue(cont_code, err_code, errbuf);
	return status;
}

int						/* boolean */
oktorunbatch()
{
	struct stat stbuf;

	if (!btch.isopen || fstat(fileno(btch.file), &stbuf) < 0)
		return NO;
	btch.size = stbuf.st_size;
	return(btch.arts >= TOOMANY || btch.size > TOOBIG ||
	    btch.size > 0 && time((time_t *)NULL) - btch.start > TOOOLD);
}

/*
 * Copy standard input (up to a "." line) to art, if it fits,
 * else to a temporary file.
 */
/* ARGSUSED errbuf */
static int					/* boolean: got article ok? */
cpstdin(cont_code, err_code, errbuf, msg_id)
int cont_code, err_code;
char *errbuf, *msg_id;
{
	register FILE *tfp = NULL;
	register char *cp, *realline;
	char line[NNTP_STRLEN];
	int toobig = NO;

	/* TODO: is this right?  used to open here, with errors here */
	printf("%d Ok\r\n", cont_code);
	(void) fflush(stdout);

	xfer_lines = old_xfer_lines = 0;
	incore = YES;
	art[0] = '\0';
	endart = art;
#ifdef XFER_TIMEOUT
	signal(SIGALRM, xfer_timeout);
	(void) alarm(XFER_TIMEOUT);
#endif
	while (fgets(line, sizeof line, stdin) != NULL) {
		xfer_lines++;
		if ((cp = rindex(line, '\r')) != NULL ||
		    (cp = rindex(line, '\n')) != NULL)
			*cp = '\0';			/* nuke CRLF */
		if (line[0] == '.' && line[1] == '\0')
			break;				/* article end: exit */
		/* remove hidden dot if present */
		realline = (line[0] == '.'? line+1: line);
		if (toobig) {				/* straight to disk */
			(void) fputs(realline, tfp);
			(void) putc('\n', tfp);
		} else {
			int len = strlen(realline);

			/*
			 * Does art have room to append realline + \n\0?
			 * If not, open temp file and dump art & realline there.
			 */
			if (sizeof art - (endart - art) < len + 1 + 1) {
				(void) strcpy(tempfile, "/tmp/rpostXXXXXX");
				(void) mktemp(tempfile);
				tfp = fopen(tempfile, "w");
				if (tfp == NULL) {
					printf("%d Cannot create temporary file.\r\n",
						err_code);
					(void) fflush(stdout);
					return 0;
				}
#ifdef OK_IN_MIDDLE_OKAY
				else {
					printf("%d Ok\r\n", cont_code);
					(void) fflush(stdout);
				}
#endif
				(void) fwrite(art, 1, endart - art, tfp);
				toobig = YES;
				incore = NO;
				art[0] = '\0';
				endart = art;
				(void) fputs(realline, tfp);
				(void) putc('\n', tfp);
			} else {
				/* fits: append realline\n to art at endart */
				(void) strcpy(endart, realline);
				endart += len;
				*endart++ = '\n';
				*endart = '\0';
			}
		}
	}
	if (tfp != NULL)
		(void) fclose(tfp);
#ifdef XFER_TIMEOUT
	(void) alarm(0);
	(void) signal(SIGALRM, SIG_DFL);
#endif

	/* See if the connection got closed somehow... */
	if (line[0] != '.' && line[1] != '\0') {
		if (tempfile[0] != '\0')
			(void) unlink(tempfile);
#ifdef SYSLOG
#ifdef LOG
		syslog(LOG_ERR,
		    "%s cpstdin: EOF before period on line by itself %s",
			hostname, msg_id);
#else
		syslog(LOG_ERR,
		    "cpstdin: EOF before period on line by itself %s", msg_id);
#endif
#endif
		return 0;
	}
	return 1;
}

static int
xfer_timeout()
{
#ifdef XFER_TIMEOUT
	if (old_xfer_lines < xfer_lines) {
		old_xfer_lines = xfer_lines;
		(void) alarm(XFER_TIMEOUT);
		return;
	}
	/* Timed out. */
	printf("%d timeout after %d seconds, closing connection.\r\n",
		ERR_FAULT, XFER_TIMEOUT);
	fflush(stdout);
#ifdef SYSLOG
#ifdef LOG
	syslog(LOG_ERR, "%s transfer_timeout", hostname);
#endif LOG
#endif
	(void) unlink(tempfile);
	exit(1);
#endif XFER_TIMEOUT
}

/*
 * Append "#! rnews count" and art (or tempfile) to batch file, locking
 * assumed. If batch file is too big or too old (but not empty), feed it
 * to newsrun.
 */
static int				/* same as batch_input_article */
appbatch()
{
	register FILE *tfp = NULL;
	register int bytes = 0;
	int status = 1;				/* okay status */
	long size = 0;
	char artbuf[COPYSIZE];
	struct stat stbuf;

	if (btch.file == NULL) {
		btch.file = fopen(btch.name, "a");
		if (btch.file == NULL) {
#ifdef SYSLOG
			syslog(LOG_ERR,"appbatch: fopen: %s: %m", btch.name);
#endif
			return 0;
		}
		btch.isopen = YES;
		btch.start = time(&btch.start);
		btch.size = 0;
		btch.arts = 0;
	}

	/* find article size and write the article */
	if (incore)
		size = endart - art;
	else {
		tfp = fopen(tempfile, "r");
		if (tfp == NULL)
			return 0;
		if (fstat(fileno(tfp), &stbuf) >= 0)
			size = stbuf.st_size;
	}
	(void) fprintf(btch.file, "#! rnews %ld %s\n", size, hostname);

	/* copy the article to the batch file */
	if (incore)
		(void) fwrite(art, 1, endart - art, btch.file);
	else {
		while ((bytes = fread(artbuf, 1, sizeof artbuf, tfp)) > 0)
			if (fwrite(artbuf, 1, bytes, btch.file) != bytes) {
#ifdef SYSLOG
				syslog(LOG_ERR,
				    "enqueue: fwrite can't write %s",
				    btch.name);
#endif
				status = 0;	/* hmm, #! count is off */
				break;
			}
		(void) fclose(tfp);
	}
	if (fflush(btch.file) == EOF) {
#ifdef SYSLOG
		syslog(LOG_ERR, "enqueue: fflush: %s", btch.name);
#endif
		status = 0;
	}
	return status;
}

/*
 * Enqueue any partial batch.  Called before exit.
 */
enqpartbatch(cont_code, err_code, errbuf)
int cont_code, err_code;
char *errbuf;
{
	struct stat stbuf;

	if (btch.isopen && fstat(fileno(btch.file), &stbuf) >= 0) {
		if (btch.size > 0)
			enqueue(cont_code, err_code, errbuf);
		else {
			(void) fclose(btch.file);
			btch.file = NULL;
			btch.isopen = NO;
			(void) unlink(btch.name); /* remove empty batch */
		}
	}
}

/* 
 * insert the batch file into the input subsystem queue by renaming
 * it to an all-numeric name, then kick newsrun to process it.
 * locks btch.name as appropriate.
 */
static int				/* same as batch_input_article */
enqueue(cont_code, err_code, errbuf)
int cont_code, err_code;
char *errbuf;
{
	time_t now;
	int pid, wpid, status, fd, exitstat;
	char permname[MAXDIGITS], *number = permname, *newsrun;
	struct stat stbuf;
#ifdef POSTER
	char *envp[4], user[sizeof(POSTER) + 5], logname[sizeof(POSTER) + 8];
	char *home;
#else
	char *envp[1];
#endif

	if (fclose(btch.file) != 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "enqueue: fclose: %m");
#endif
	}
	btch.file = NULL;
	btch.isopen = NO;
	btch.start = 0;
	btch.size = 0;
	btch.arts = 0;

	(void) fflush(stdout);
	(void) fflush(stderr);
#ifndef NONEWSRUN
	pid = fork();
	if (pid == -1) {
#ifdef SYSLOG
		syslog(LOG_ERR, "enqueue: fork: %m");
#endif
		return 0;
	} else if (pid != 0) {			/* parent */
		while ((wpid = wait(&status)) != -1 && wpid != pid)
			;
		exitstat = (status>>8)&0377;
#ifdef SYSLOG
		if (exitstat != 0) {
			syslog(LOG_ERR, " enqueue returned exit status 0%o",
				exitstat);
		}
#endif
		return exitstat != 0? -1 :1;
	}
#endif /* NONEWSRUN */
#ifdef POSTER
#ifndef USG
		if (getuid() == 0) initgroups(POSTER,gid_poster);
#endif
		(void) setgid(gid_poster);
		(void) setuid(uid_poster);
#endif
#ifndef NONEWSRUN
	/* child: must exit */
#ifdef SYSLOG
	/* Close in such a way that syslog() will know to reopen */
	closelog();
#endif
	for (fd = 3; fd < 20; fd++)
		(void) close(fd);
	if (chdir(INDIR) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "enqueue: chdir(%s): %m", INDIR);
#else
		;
#endif
	}

	/* rename btch.name to a number so newsrun will see it */
	sprintf(number, "%ld", (long)time(&now));
#else
	sprintf(number, "%d.%d", getpid(), uniq++);
#endif /* NONEWSRUN */
	while (link(btch.name, permname) < 0) {
		if (stat(btch.name, &stbuf) < 0)
			break;
		sleep(2);
#ifndef NONEWSRUN
		sprintf(number, "%ld", (long)time(&now));
#else
		sprintf(number, "%d.%d", getpid(), uniq++);
#endif /* NONEWSRUN */
	}
	if (unlink(btch.name) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "enqueue: cannot unlink %s: %m", btch.name);
#endif
	}
#ifndef NONEWSRUN

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);
	(void) fflush(stdout);
	(void) fflush(stderr);
	newsrun = strsave(NEWSRUN);
	if (newsrun == NULL)
		newsrun = "/usr/lib/newsbin/input/newsrun";

	/* Empty environment keeps cnews inews from telling lies */
#ifdef POSTER
	sprintf(user, "USER=%s", POSTER);
	sprintf(logname, "LOGNAME=%s", POSTER);
	if ((home = (char *)malloc(strlen(home_poster)+5)) != NULL)
		sprintf(home, "HOME=%s", home_poster);
	envp[0] = user;
	envp[1] = logname;
	envp[2] = home;
	envp[3] = 0;
#else
	envp[0] = 0;
#endif
#ifdef USG
 	/* execle() fails because newsrun is a shell procedure */
 	execle("/bin/sh", "sh", newsrun, (char *)NULL, envp);
#else
	execle(newsrun, newsrun, (char *)NULL, envp);
#endif
#ifdef SYSLOG
	syslog(LOG_ERR, "enqueue: execle(%s): %m", newsrun);
#endif
	exit(1);
	/* NOTREACHED */
#else
	return(1);
#endif /* NONEWSRUN */
}

static char *
strsave(s)
register char *s;
{
	register char *news = malloc((unsigned)(strlen(s) + 1));

	if (news != NULL)
		strcpy(news, s);
	return news;
}
#endif


