#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/serve.c,v 1.2 1994/04/25 23:58:27 adam Exp $";
#endif

/*
 * Main server routine
 */

#include "common.h"
#include <signal.h>
#ifdef USG
#include <sys/times.h>
#else
#include <sys/time.h>
#endif

#ifdef LOG
# ifndef USG
#  include <sys/resource.h>
# endif not USG
#endif

#ifdef TIMERS
#include "timer.h"
#endif

extern	int	ahbs(), group(), help(), ihave();
extern	int	list(), newgroups(), newnews(), nextlast(), post();
extern	int	slave(), stat(), xhdr();

extern int errno;

#ifdef AUTH
extern	int	doauth();
#endif AUTH

static struct cmdent {
	char	*cmd_name;
	int	authreq;	/* 0=none,1=userpass */
	int	(*cmd_fctn)();
} cmdtbl[] = {
#ifdef AUTH
	"authcap",	0,	doauth,
	"authinfo",	0,	doauth,
	"authsys",	0,	doauth,
#endif AUTH
	"article",	0,	ahbs,
	"body",		0,	ahbs,
	"group",	0,	group,
	"head",		0,	ahbs,
	"help",		0,	help,
	"ihave",	1,	ihave,
	"last",		0,	nextlast,
	"list",		0,	list,
	"newgroups",	0,	newgroups,
	"newnews",	0,	newnews,
	"next",		0,	nextlast,
	"post",		1,	post,
	"slave",	0,	slave,
	"stat",		0,	ahbs,
#ifdef XHDR
	"xhdr",		0,	xhdr,
#endif XHDR
};
#define NUMCMDS (sizeof(cmdtbl) / sizeof(struct cmdent))

#ifdef TIMEOUT
static void timeout();
#endif
#ifdef LOGINCHECK
static void logincheck();
static int firstlogincheck;
#endif
#ifdef BATCHED_INPUT
static void batchcheck();
#endif

#ifdef TIMERS
static struct timer timers[] = {
#ifdef TIMEOUT
	{ timeout, 1, TIMEOUT, 0 },
#endif
#ifdef LOGINCHECK
	{ logincheck, 0, LOGINCHECK, 0 },
#endif
#ifdef BATCHCHECK
	{ batchcheck, 1, BATCHCHECK, 0 },
#endif
};
#define NTIMERS (sizeof(timers) / sizeof(struct timer))
#endif

static char *stats_init();
#ifdef LOG
static void stats_finish();
#endif

#ifdef AUTH
extern int	Needauth;
extern char	User[];
#endif AUTH

/*
 * serve -- given a connection on stdin/stdout, serve
 *	a client, executing commands until the client
 *	says goodbye.
 *
 *	Parameters:	None.
 *
 *	Returns:	Exits.
 *
 *	Side effects:	Talks to client, does a lot of
 *			stuff.
 */

serve()
{
	char		line[NNTP_STRLEN];
	char		host[MAXHOSTNAMELEN];
	char		gdbuf[MAXBUFLEN];
	char		**argp;
	char		*timeptr, *cp;
	int		argnum, i;
#ifdef POSTER
	struct passwd	*pp;
#endif
#ifdef LOG
	grps_acsd = arts_acsd = 0;
#endif

	/* Not all systems pass fd's 1 and 2 from inetd */

	(void) close(1);
	(void) close(2);
	(void) dup(0);
	(void) dup(0);

	/* If we're ALONE, then we've already opened syslog */

#ifndef ALONE
# ifdef SYSLOG
#  ifdef BSD_42
	openlog("nntpd", LOG_PID);
#  else
	openlog("nntpd", LOG_PID, SYSLOG);
#  endif
# endif
#endif

	timeptr = stats_init();

#ifdef ALONE
#ifndef USG
	(void) signal(SIGCHLD, SIG_IGN);
#endif not USG
#endif

	/* Ignore SIGPIPE, since we'll see closed connections with read */

	(void) signal(SIGPIPE, SIG_IGN);

	/* Get permissions and see if we can talk to this client */
#ifdef AUTH
	Needauth = 1;
	strcpy(User,"");
#endif AUTH
	host_access(&canread, &canpost, &canxfer, gdbuf);

	if (gethostname(host, sizeof(host)) < 0)
		(void) strcpy(host, "Amnesiac");

#ifdef SETPROCTITLE
	setproctitle("%s", hostname);
#endif

	if (!canread && !canxfer) {
		printf("%d %s NNTP server can't talk to you.  Goodbye.\r\n",
			ERR_ACCESS, host);
		(void) fflush(stdout);
		(void) fclose(stdout);
#ifdef SYSLOG
		syslog(LOG_INFO, "%s refused connection", hostname);
#endif
		exit(1);
	}

#ifdef LOGINCHECK
	firstlogincheck = 1;
	logincheck();
	firstlogincheck = 0;
#endif

	if ( !canpost && !canread && !space(MINFREE)) {
		printf("%d %s NNTP server out of space. Try later.\r\n",
			ERR_GOODBYE, host);
		(void) fflush(stdout);
#ifdef SYSLOG
		syslog(LOG_INFO, "%s no space", hostname);
#endif
		exit(1);
	}

	/* If we can talk, proceed with initialization */

	ngpermcount = get_nglist(&ngpermlist, gdbuf);

#ifdef POSTER
	pp = getpwnam(POSTER);
	if (pp != NULL) {
		uid_poster = pp->pw_uid;
		gid_poster = pp->pw_gid;
		home_poster = pp->pw_dir;
	} else
#endif
		uid_poster = gid_poster = 0;

#ifndef FASTFORK
	num_groups = 0;
	num_groups = read_groups();	/* Read in the active file */
#else
	signal(SIGALRM, SIG_IGN);	/* Children don't deal with */
					/* these things */
#endif
	/*
	 * num_groups may be zero if expire is running and the active
	 * file is locked. (Under System V with lockf, for example.)
	 * Or, something may be really screwed up....
	 */
	if (num_groups == 0){ /* can't get a group list */
		printf("%d %s NNTP server unavailable. Try later.\r\n",
			ERR_FAULT, host);
		(void) fflush(stdout);
#ifdef SYSLOG
		syslog(LOG_INFO, "%s no groups", hostname);
#endif
		exit(1);
	}

	art_fp = NULL;
	argp = (char **) NULL;		/* for first time */

	if ((cp = index(timeptr, '\n')) != NULL)
		*cp = '\0';
	else
		timeptr = "Unknown date";
#ifdef AUTH
	printf("%d %s NNTP[auth] server version %s ready at %s (%s).\r\n",
#else
	printf("%d %s NNTP server version %s ready at %s (%s).\r\n",
#endif
		canpost ? OK_CANPOST : OK_NOPOST,
		host, nntp_version,
		timeptr,
		canpost ? "posting ok" : "no posting");
	(void) fflush(stdout);

	/*
	 * Now get commands one at a time and execute the
	 * appropriate routine to deal with them.
	 */
#ifdef TIMERS
	timer_init(timers, NTIMERS);
#endif
	for (;;) {
#ifdef TIMERS
		/* Don't try to read input unless there is some */
		if (!timer_sleep(timers, NTIMERS))
			continue;
#endif
		if (fgets(line, sizeof(line), stdin) == NULL)
			break;
		/* Strip trailing CR-LF */
		cp = line + strlen(line) - 1;
		while (cp >= line && (*cp == '\n' || *cp == '\r'))
			*cp-- = '\0';
#ifdef DEBUG
		if (debug)
			syslog(LOG_DEBUG, "<- \"%s\"", line);
#endif

		/* Null command */
		if ((argnum = parsit(line, &argp)) == 0)
			continue;

		/* a motion to adjourn is always in order */
		if (!strcasecmp(argp[0], "quit"))
			break;

		for (i = 0; i < NUMCMDS; ++i)
			if (!strcasecmp(cmdtbl[i].cmd_name, argp[0]))
				break;

		if (i < NUMCMDS) {
#ifdef SETPROCTITLE
			setproctitle("%s %s", hostname, argp[0]);
#endif
#ifdef AUTH
			/* authentication required? */
			if (cmdtbl[i].authreq == 1 && Needauth)
				{
printf("%d Authentication required for command\r\n", ERR_NOAUTH);
				(void) fflush(stdout);
				continue;
				}
#endif AUTH
			(*cmdtbl[i].cmd_fctn)(argnum, argp);
		} else {
#ifdef SYSLOG
			syslog(LOG_INFO, "%s unrecognized %s", hostname, line);
#endif
			printf("%d Command unrecognized.\r\n", ERR_COMMAND);
			(void) fflush(stdout);
		}
	}

	printf("%d %s closing connection.  Goodbye.\r\n", OK_GOODBYE, host);

	(void) fflush(stdout);

#ifdef BATCHED_INPUT
	batchcheck();
#endif

#ifdef SYSLOG
	if (ferror(stdout))
		syslog(LOG_ERR, "%s disconnect: %m", hostname);
#ifdef LOG
	stats_finish();
#endif
#endif

#ifdef PROFILE
	profile();
#endif
	exit(0);
}

#ifdef TIMEOUT
/*
 * Called after TIMEOUT seconds of idle time to shut things down.
 * XXX stats are not reported when this occurs
 */
static void
timeout()
{

	printf("%d Timeout after %d seconds, closing connection.\r\n",
	    ERR_FAULT, TIMEOUT);
#ifdef SYSLOG
	syslog(LOG_NOTICE, "%s timeout", hostname);
#endif
	(void) fflush(stdout);
#ifdef BATCHED_INPUT
	batchcheck();
#endif
#ifdef LOG
	stats_finish();
#endif
#ifdef PROFILE
	profile();
#endif
	exit(1);
}
#endif

#ifdef LOGINCHECK
/*
 * Called ever LOGINCHECK seconds to see if logins have been disabled.
 * If so, shut down.
 * XXX stats are not reported when this occurs
 */
static void
logincheck()
{
	char host[MAXHOSTNAMELEN];

	if (access(NOLOGIN, F_OK) < 0)
		return;
	if (gethostname(host, sizeof(host)) < 0)
		(void) strcpy(host, "Amnesiac");
	printf("%d Logins are disabled on NNTP server %s. Try again later.\r\n",
	    ERR_ACCESS, host);
	(void) fflush(stdout);
#ifdef SYSLOG
	syslog(LOG_INFO, "%s logins disabled%s",
	    hostname, firstlogincheck ? "" : " (kicked out)");
#endif
#ifdef BATCHED_INPUT
	batchcheck();
#endif
#ifdef LOG
	stats_finish();
#endif
#ifdef PROFILE
	profile();
#endif
	exit(1);
}
#endif

#ifdef BATCHED_INPUT
/*
 * Called after BATCHCHECK seconds of idle time and at the end
 * of a session to see if a batch needs to be launched.
 */
static void
batchcheck()
{
	char errbuf[2 * NNTP_STRLEN];

	enqpartbatch(CONT_XFER, ERR_XFERFAIL, errbuf);
}
#endif

/*
 * Stats stuff
 */
static double		Tstart, Tfinish;
static double		user, sys;
#ifdef USG
static time_t		start, finish;
#else /* not USG */
static struct timeval	start, finish;
#endif /* not USG */

static char *
stats_init()
{
	extern char	*ctime();

#ifdef USG
	(void) time(&start);
	Tstart = (double) start;
	return(ctime(&start));
#else /* not USG */
	(void) gettimeofday(&start, (struct timezone *)NULL);
	Tstart = (double) start.tv_sec + ((double)start.tv_usec)/1000000.0;
	return(ctime(&start.tv_sec));
#endif /* not USG */
}

#ifdef LOG
static void
stats_finish()
{
	char		buf[NNTP_STRLEN];
# ifdef USG
	struct tms	cpu;
# else /* not USG */
	struct rusage	me, kids;
# endif /* not USG */

#ifdef USG
	(void) time(&finish);
	Tfinish = (double) finish;

#ifndef HZ
#define	HZ	60.0	/* typical system clock ticks - param.h */
#endif /* not HZ */

	(void) times(&cpu);
	user = (double)(cpu.tms_utime + cpu.tms_cutime) / HZ;
	sys  = (double)(cpu.tms_stime + cpu.tms_cstime) / HZ;
#else /* not USG */
	(void) gettimeofday(&finish, (struct timezone *)NULL);
	Tfinish = (double) finish.tv_sec + ((double)finish.tv_usec)/1000000.0;

	(void) getrusage(RUSAGE_SELF, &me);
	(void) getrusage(RUSAGE_CHILDREN, &kids);

	user = (double) me.ru_utime.tv_sec + me.ru_utime.tv_usec/1000000.0 +
		kids.ru_utime.tv_sec + kids.ru_utime.tv_usec/1000000.0;
	sys = (double) me.ru_stime.tv_sec + me.ru_stime.tv_usec/1000000.0 +
		kids.ru_stime.tv_sec + kids.ru_stime.tv_usec/1000000.0;
#endif /* not USG */
	if (grps_acsd)
		syslog(LOG_INFO, "%s exit %d articles %d groups",
			hostname, arts_acsd, grps_acsd);
	if (nn_told)
		syslog(LOG_INFO, "%s newnews_stats told %d took %d",
			hostname, nn_told, nn_took);
	if (ih_accepted || ih_rejected || ih_failed)
		syslog(LOG_INFO,
			"%s ihave_stats accepted %d rejected %d failed %d",
			hostname,
			ih_accepted,
			ih_rejected,
			ih_failed);
	(void) sprintf(buf, "user %.3f system %.3f elapsed %.3f",
		user, sys, Tfinish - Tstart);
	syslog(LOG_INFO, "%s times %s", hostname, buf);
}
#endif LOG
