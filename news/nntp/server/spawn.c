#ifndef lint
static	char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/spawn.c,v 1.1 1993/07/19 20:04:32 nate Exp $";
#endif

#include "common.h"

#include <signal.h>
#ifdef sparc
#include <vfork.h>
#endif
#ifdef XFER_TIMEOUT
static int	xfer_lines;
static int	old_xfer_lines;
#endif

static char	tempfile[256];
#ifndef CNEWS
static char	badfile[256];
#endif

/*
 * spawn -- create a child process with the input from the client
 * as stdin.
 *
 *	Parameters:	"path" is the path of the program to invoke.
 *			"name" is the name to call the program.
 *			"flag" is a single flag to be passed to the program.
 *			"cont_code" is the response code to transmit
 *			on successful startup.
 *			"err_code" is the response code to transmit when
 *			something goes wrong.
 *
 *	Returns:	-1 on non-zero return from child,
 *			0 on error before fork/exec,
 *			1 otherwise.
 *
 *	Side effects:	Creates and removes temporary file;
 *			accepts input from client; forks and execs.
 *			Can time out if XFER_TIMEOUT is defined.
 */

spawn(path, name, flag, cont_code, err_code, errbuf, msg_id)
	char		*path;
	char		*name;
	char		*flag;
	int		cont_code;
	int		err_code;
	char		*errbuf;
	char		*msg_id;
{
	char		line[NNTP_STRLEN];
	register char	*cp;
	int		i, fd;
	int		fds[2];
	int		pid, npid;
	int		exit_status = 1;
#ifdef POSTER
	char *envp[4], user[sizeof(POSTER) + 5], logname[sizeof(POSTER) + 8];
	char *home;
#else
	char *envp[1];
#endif
#ifdef XFER_TIMEOUT
	int		xfer_timeout();
#endif
#ifdef USG
	int		status;
#else not USG
	union wait	status;
#endif not USG
	register FILE	*fp;

#ifdef CNEWS
	(void) strcpy(tempfile, "/usr/tmp/rpostXXXXXX");
#else
	sprintf(tempfile, "%s/.tmp/rpostXXXXXX",SPOOLDIR);
#endif
	(void) mktemp(tempfile);

	fp = fopen(tempfile, "w");
	if (fp == NULL) {
		printf("%d Cannot create temporary file.\r\n", err_code);
		(void) fflush(stdout);
		return (0);
	}

#ifdef AUTH
	/*
	* If this is a posting, rather than an XFER, leave a trail of
	* breadcrumbs by stuffing where it came from into the header
	*/
	if (cont_code == CONT_POST)
		fprintf(fp, "Nntp-Posting-Host: %s\n", hostname);
#endif AUTH

	printf("%d Ok\r\n", cont_code);
	(void) fflush(stdout);

#ifdef XFER_TIMEOUT
	xfer_lines = old_xfer_lines = 0;
	signal(SIGALRM, xfer_timeout);
	(void) alarm(XFER_TIMEOUT);
#endif

	while (fgets(line, sizeof(line), stdin) != NULL) {
#ifdef XFER_TIMEOUT
		xfer_lines++;
#endif
		if ((cp = index(line, '\r')) != NULL)
			*cp = '\0';
		else if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';

		if (line[0] == '.' && line[1] == '\0')
			break;

		if (line[0] == '.')
			fputs(line+1, fp);
		else
			fputs(line, fp);
		putc('\n', fp);
	}
	(void) fclose(fp);

#ifdef XFER_TIMEOUT
	(void) alarm(0);
	(void) signal(SIGALRM, SIG_DFL);
#endif

	/* See if the connection got closed somehow... */

	if (line[0] != '.' && line[1] != '\0') {
		(void) unlink(tempfile);
#ifdef SYSLOG
# ifdef LOG
		syslog(LOG_ERR,
		    "%s spawn: EOF before period on line by itself %s",
		    hostname, msg_id);
# else
		syslog(LOG_ERR,
		    "spawn: EOF before period on line by itself %s", msg_id);
# endif
#endif
		return (0);
	}
		
#ifdef POSTER
	if (tempfile[0])
		(void) chown(tempfile, uid_poster, gid_poster);
#endif

	/* Set up a pipe so we can see errors from rnews */

	if (pipe(fds) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "spawn: pipe: %m");
#endif
		(void) unlink(tempfile);
		return (-1);
	}

	/*
	 * Ok, now we have the article in "tempfile".  We
	 * should be able to fork off, close fd's 0 to 31 (or
	 * whatever), open "tempfile" for input, thus making
	 * it stdin, and then execle the inews.  We think.
	 */
#ifdef SYSLOG
	/*
	 * Close in such a way that syslog() will know to reopen.
	 * We must do this before the vfork() otherwise the parent
	 * will think the syslog fd is closed and open a new one,
	 * eventually using up all the available file descriptors.
	 */
	closelog();
#endif

	pid = vfork();
	if (pid == 0) {		/* We're in child */
#ifdef POSTER
#ifndef USG
		if (getuid() == 0) initgroups(POSTER,gid_poster);
#endif
		(void) setgid(gid_poster);
		(void) setuid(uid_poster);
#endif

		/* Set up stdout and stderr for child */

		if (fds[1] != 1) {
			(void) dup2(fds[1], 1);
			(void) close(fds[1]);
		}
		(void) dup2(1, 2);

		for (i = 3; i < 10; ++i) /* XXX but getdtablesize is too big */
			(void) close(i);

		fd = open(tempfile, O_RDONLY);
		if (fd != 0) {
			(void) dup2(fd, 0);
			(void) close(fd);
		}

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
 		/* execle() fails if name is a shell procedure */
 		execle("/bin/sh", "sh", path, flag, (char *)NULL, envp);
#else
		execle(path, name, flag, (char *) NULL, envp);
#endif
		fprintf(stderr, "spawn: execle(%s)", path);
		perror(path);
#ifdef SYSLOG
		syslog(LOG_ERR, "spawn: execle(%s): %m", path);
#endif
		_exit(-1);	/* Error */
		/*NOTREACHED*/
	} else {	/* We're in parent. */
		if (pid == -1) {
			/* fork failed! */
			printf("%d Cannot fork %s\r\n", err_code, path);
			(void) fflush(stdout);
#ifdef SYSLOG
			syslog(LOG_ERR, "spawn: fork: %m");
#endif
			(void) unlink(tempfile);
			return (0);
		}
		(void) close(fds[1]);
		fp = fdopen(fds[0], "r");
		if (fp == NULL) {
			printf("%d Cannot fdopen %s pipe\r\n", err_code, path);
			(void) fflush(stdout);
#ifdef SYSLOG
			syslog(LOG_ERR, "spawn: pipe: %m");
#endif
			(void) unlink(tempfile);
			return (0);
		}

		if (errbuf)
			*errbuf = '\0';

		while (fgets(line, sizeof (line), fp) != NULL) {
			if (line[0] != '\n') {
				if (errbuf) {
					if (cp = index(line, '\n'))
						*cp = '\0';
					(void) strcat(errbuf, line);
				}
#ifdef SYSLOG
				syslog(LOG_INFO, "%s: %s", path, line);
#endif
			}
		}

		while ((npid = wait(&status)) > 0)
			if (npid == pid) {
#ifdef USG
				exit_status = (status >> 8) & 0xff;
#else /* not USG */
				exit_status = status.w_T.w_Retcode;
#endif /*not USG */
				break;
			}

		(void) fclose(fp);
		(void) fflush(stdout);
		if (npid < 0) {
#ifdef SYSLOG
			syslog(LOG_ERR, "spawn: wait pid %d: %m", pid);
#endif
			return (-1);
		}

#ifdef notdef
		/* Make ctags happy */
		{
#endif
		if (exit_status != 0) {
#ifdef SYSLOG
			syslog(LOG_ERR, "spawn: %s exit status %d",
				path, exit_status);
#endif
			/*
			 * Save the tempfile away in .bad.
			 */
#ifdef CNEWS
			}
		(void) unlink(tempfile);
#else
			sprintf(badfile, "%s/.bad/nntpXXXXXX",SPOOLDIR);
			(void) mktemp(badfile);
			(void) rename(tempfile, badfile);
		} else {
			(void) unlink(tempfile);
		}
#endif			
		return (exit_status ? -1 : 1);
	}
}

#ifdef XFER_TIMEOUT

xfer_timeout()
{
	if (old_xfer_lines < xfer_lines) {
		old_xfer_lines = xfer_lines;
		(void) alarm(XFER_TIMEOUT);
		return;
	}

	/* Timed out. */

	printf("%d timeout after %d seconds, closing connection.\r\n",
		ERR_FAULT, XFER_TIMEOUT);
	fflush(stdout);

#ifdef LOG
	syslog(LOG_ERR, "%s transfer_timeout", hostname);
#endif LOG

	(void) unlink(tempfile);

	exit(1);
}

#endif XFER_TIMEOUT

