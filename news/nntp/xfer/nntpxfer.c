#ifndef lint
static char * scsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/xfer/nntpxfer.c,v 1.2 1994/04/25 23:58:34 adam Exp $";
#endif
/*
 * nntpxfer
 *
 * Connects to the specified nntp server, and transfers all new news
 * since the last successful invocation.
 *
 * last successful invocation date and time are stored in a file at
 * /usr/spool/news/nntp.<hostname> as 
 *	groups YYMMDD HHMMSS distributions\n
 * in case you need to edit it.  You can also override this on 
 * the command line in the same format, in which case the file won't
 * be updated.
 *
 *	Brian Kantor, UCSD 1986
 * (some bug fixes by ambar@athena.mit.edu)
 * Modified to use NNTP distribution conf.h file and nntpxmit's get_tcp_conn.c
 * subroutines so that nntpxfer could be used on more systems.
 * Stan Barber, November 7, 1989 <sob@bcm.tmc.edu>
 *
 */

#include "../common/conf.h"
#ifdef DEBUG
#undef SYSLOG
#endif

#include <sys/types.h>
#ifdef NDIR
#ifdef M_XENIX
#include <sys/ndir.h>
#else
#include <ndir.h>
#endif
#else
#include <sys/dir.h>
#endif
#ifdef USG
#include <time.h>
#else
#include <sys/time.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <setjmp.h>
#ifndef NONETDB
#include <netdb.h>
#endif
#include <signal.h>
#ifdef SYSLOG
#ifdef FAKESYSLOG
#include "../server/fakesyslog.h"
#else
#include <syslog.h>
#endif
#endif

#ifdef DBM
# ifdef DBZ
#   include <dbz.h>
#else /* DBZ */
# undef NULL
# include <dbm.h>
# undef NULL
# define NULL	0
#endif /* DBZ */
#endif  /* DBM */

#ifdef NDBM
#include <ndbm.h>
#include <fcntl.h>
static DBM *db = NULL;
#endif
#ifndef TIMEOUT
#define TIMEOUT (30*60)
#endif
#ifndef MAX_ARTICLES
#define MAX_ARTICLES 4096
#endif

char	*malloc();
char	*strcpy();
char	*strcat();
char	*rindex();
long	time();
u_long	inet_addr();

extern int errno;
char *artlist[MAX_ARTICLES];
int server;			/* stream socket to the nntp server */
FILE * rd_fp, * wr_fp;
int newart, dupart, misart;
char * Pname;

main(argc, argv)
int argc;
char *argv[];
{
	FILE *dtfile;		/* where last xfer date/time stored */
	char buf[BUFSIZ];
	char lastdate[16];
	char distributions[BUFSIZ];
	char dtname[128];
	char newsgroups[BUFSIZ];
	char lasttime[16];
	int i;
	int omitupdate = 0;		/* 1 = don't update datetime */
	long clock;
	long newdate, newtime;
	struct tm *now;
	Pname = ((Pname = rindex(argv[0], '/')) ? Pname + 1 : argv[0]);
	/* OPTIONS
		argv[1] MUST be the host name
		argv[2-4] MAY be "newsgroups YYMMDD HHMMSS"
			argv[5] MAY be distributions
		(otherwise use 2-4/5 from the file
		"/usr/spool/news/nntp.hostname")
	*/

	if (argc != 2 && argc != 5 && argc != 6)
		{
		(void) printf("Usage: %s host [groups YYMMDD HHMMSS [<dist>]]\n",
			argv[0]);
		exit(1);
		}
	
	if (argc > 2)
		{
		omitupdate=1;
		(void) strcpy(newsgroups, argv[2]);
		(void) strcpy(lastdate, argv[3]);
		(void) strcpy(lasttime, argv[4]);
		(void) strcpy(distributions, "");
		if (argc > 5)
			(void) strcpy(distributions, argv[5]);
		}
	else
		{
		(void) sprintf(dtname, "%s/nntp.%s",SPOOLDIR,argv[1]);
		dtfile = fopen(dtname, "r");
		if (dtfile == (FILE *) 0)
			{
			(void) printf("%s not found; using * 860101 000000 \n", 
				dtname);
			(void) strcpy(newsgroups, "*");
			(void) strcpy(lastdate, "860101");
			(void) strcpy(lasttime, "000000");
			(void) strcpy(distributions, "");
			}
		else
			{
			if (fscanf(dtfile, "%s %s %s %s",
				newsgroups, lastdate, lasttime, distributions) < 3)
				{
				(void) printf("%s invalid; using * 860101 000000\n",
					dtname);
				(void) strcpy(newsgroups, "*");
				(void) strcpy(lastdate, "860101");
				(void) strcpy(lasttime, "000000");
				(void) strcpy(distributions, "");
				}
			(void) fclose(dtfile);
			}
		clock = time((long *)0);
		now = gmtime(&clock);
		newdate = (now->tm_year * 10000) +
			((now->tm_mon + 1) * 100) + now->tm_mday;
		newtime = (now->tm_hour * 10000) +
			(now->tm_min * 100) + now->tm_sec;
#ifdef DEBUG
	printf("server is %s\n",argv[1]);
	printf("lastdate is %s\n",lastdate);
	printf("lasttime is %s\n",lasttime);
	printf("newsgroups is '%s'\n",newsgroups);
	printf("distributions is '%s'\n",distributions);
#endif
		}
#ifdef SYSLOG
#ifdef BSD_42
	openlog("nntpxfer", LOG_PID);
#else
	openlog("nntpxfer", LOG_PID, SYSLOG);
#endif
#endif

#ifdef DBM
	if (dbminit(HISTORY_FILE) < 0)
		{
#ifdef SYSLOG
		syslog(LOG_ERR,"couldn't open history file: %m");
#else
		perror("nntpxfer: couldn't open history file");
#endif
		exit(1);
		}
#endif
#ifdef NDBM
 	if ((db = dbm_open(HISTORY_FILE, O_RDONLY, 0)) == NULL) 
 		{
#ifdef SYSLOG
 		syslog(LOG_ERR,"couldn't open history file: %m");
#else
 		perror("nntpxfer: couldn't open history file");
#endif
 		exit(1);
 		}
#endif
	if ((server = get_tcp_conn(argv[1],"nntp")) < 0) 
		{
#ifdef SYSLOG
		syslog(LOG_ERR,"could not open socket: %m");
#else
		perror("nntpxfer: could not open socket");
#endif
		exit(1);
		}
	if ((rd_fp = fdopen(server,"r")) == (FILE *) 0){
#ifdef SYSLOG
		syslog(LOG_ERR,"could not fdopen socket: %m");
#else
		perror("nntpxfer: could not fdopen socket");
#endif
		exit(1);
		}

#ifdef SYSLOG
	syslog(LOG_DEBUG,"connected to nntp server at %s", argv[1]);
#endif
#ifdef DEBUG
	printf("connected to nntp server at %s\n", argv[1]);
#endif
	/*
	* ok, at this point we're connected to the nntp daemon 
	* at the distant host.
	*/
	/* get the greeting herald */
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
#ifdef SYSLOG
		syslog(LOG_NOTICE,"protocol error: got '%s'\n", buf);
#else
		(void) printf("%s: protocol error: got '%s'\n", Pname,buf);
#endif
		(void) close(server);
		exit(1);
		}


	/* first, tell them we're a slave process to get priority */
	sockwrite("SLAVE");
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
#ifdef SYSLOG
		syslog(LOG_NOTICE,"protocol error: got '%s'", buf);
#else
		(void) printf("%s: protocol error: got '%s'\n", Pname,buf);
#endif
		(void) close(server);
		exit(1);
		}
	
	/* now, ask for a list of new articles */
	if (strlen(distributions))
		(void) sprintf(buf,"NEWNEWS %s %s %s GMT <%s>", 
			newsgroups, lastdate, lasttime, distributions);
	else
		(void) sprintf(buf,"NEWNEWS %s %s %s GMT", 
			newsgroups, lastdate, lasttime);
	sockwrite(buf);
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
#ifdef SYSLOG
		syslog(LOG_NOTICE,"protocol error: got '%s'", buf);
#else
		(void) printf("%s: protocol error: got '%s'\n", Pname,buf);
#endif
		(void) close(server);
		exit(1);
		}
	/* and here comes the list, terminated with a "." */
#ifdef DEBUG
	(void) printf("data\n");
#endif
	dupart = newart = 0;
	while (1)
		{
		(void) sockread(buf);
		if (!strcmp(buf,"."))
			break;
		if (wewant(buf))
			{
			if (newart >= MAX_ARTICLES)
				{
				omitupdate=1;
				continue;
				}
			artlist[newart] = malloc((unsigned)(strlen(buf)+1));
			(void) strcpy(artlist[newart], buf);
			newart++;
			}
		else
			dupart++;
		}
#ifdef DEBUG
	(void) printf(".\n%d new, %d dup articles\n", newart, dupart);
#endif

	/* now that we know which articles we want, retrieve them */
	for (i=0; i < newart; i++)
		(void) artfetch(artlist[i]);

#ifdef DEBUG
	(void) printf("%d missing articles\n", misart);
#endif
	/* we're all done, so tell them goodbye */
	sockwrite("QUIT");
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
#ifdef SYSLOG
		syslog(LOG_NOTICE,"error: got '%s'", buf);
#else
		(void) printf("%s: error: got '%s'\n", Pname,buf);
#endif
		(void) close(server);
		exit(1);
		}
	(void) close(server);

	/* do we want to update the timestamp file? */
	if (!omitupdate)
		{
		(void) sprintf(buf, "%s %06d %06d %s\n",
			newsgroups, newdate, newtime, distributions);
#ifdef DEBUG
		(void) printf("updating %s:\n\t%s\n", dtname, buf);
#endif
		dtfile = fopen(dtname, "w");
		if (dtfile == (FILE *) 0)
			{
			perror(dtname);
			exit(1);
			}
		(void) fputs(buf,dtfile);
		(void) fclose(dtfile);
		}
	exit(0);
}

artfetch(articleid)
char *articleid;
	{
#ifdef DEBUG
	int lines = 0;
#endif
	char buf[BUFSIZ];
	FILE *inews;

	/* now, ask for the article */
	(void) sprintf(buf,"ARTICLE %s", articleid);
	sockwrite(buf);
	(void) sockread(buf);
#ifdef DEBUG
	(void) printf("%s\n", buf);
#endif
	if (buf[0] == '4')	/* missing article, just skipit */
		{
		misart++;
		return(0);
		}

	if (buf[0] != '2')	/* uh-oh, something's wrong! */
		{
#ifdef SYSLOG
		syslog(LOG_NOTICE,"protocol error: got '%s'", buf);
#else
		(void) printf("%s: protocol error: got '%s'\n", Pname, buf);
#endif
		(void) close(server);
		exit(1);
		}
#ifdef DEBUG
	(void) printf("command: %s\n", RNEWS);
#endif
	if ( (inews = popen(RNEWS, "w")) == (FILE *) 0)
		{
		perror(RNEWS);
		exit(1);
		}

	/* and here comes the article, terminated with a "." */
#ifdef DEBUG
	(void) printf("data\n");
#endif
	while (1)
		{
		(void) sockread(buf);
		if (buf[0] == '.' && buf[1] == '\0')
			break;
#ifdef DEBUG
		lines++;
#endif
		(void) strcat(buf,"\n");
		(void) fputs(((buf[0] == '.') ? buf + 1 : buf),
			   inews);
		}
#ifdef DEBUG
	(void) printf(".\n%d lines\n", lines);
#endif
	(void) fflush(inews);
	(void) pclose(inews);
	return(0);
        }

static	jmp_buf	SFGstack;

static SIGRET
to_sfgets()
{
	longjmp(SFGstack, 1);
}

int
sockread(buf)
char *buf;
{
	int	esave, rz;
	char * ret;
	if (setjmp(SFGstack)) {
		(void) alarm(0);	/* reset alarm clock */
		(void) signal(SIGALRM, SIG_DFL);
#ifdef __FreeBSD__
		rd_fp->_flags |= __SERR;	/* set stdio error */
#else
		rd_fp->_flag |= _IOERR;	/* set stdio error */
#endif
#ifndef ETIMEDOUT
		errno = EPIPE;		/* USG doesn't have ETIMEDOUT */
#else
		errno = ETIMEDOUT;		/* connection timed out */
#endif
#ifdef SYSLOG
		syslog(LOG_ERR,"nntpxfer: read error on server socket: %m");
#else
		(void) perror("nntpxfer: read error on server socket");
#endif
		(void) close(server);
		exit(1);
	}
	(void) signal(SIGALRM, to_sfgets);
	(void) alarm(TIMEOUT);
	ret  = fgets(buf, BUFSIZ, rd_fp);
	esave = errno;
	(void) alarm(0);			/* reset alarm clock */
	(void) signal(SIGALRM, SIG_DFL);	/* reset SIGALRM */
	errno = esave;
	rz = strlen(buf);
	buf[rz-2] = '\0';
	if (ret  == (char * ) 0) {
#ifdef SYSLOG
    		syslog(LOG_ERR,"nntpxfer: read error on server socket: %m");
#else
		(void) perror("nntpxfer: read error on server socket");
#endif
		(void) fclose(rd_fp);
		exit(1);
	}
	return(0);
}

sockwrite(buf)
char *buf;
	{
	register int sz;
	char buf2[BUFSIZ];
#ifdef DEBUG
	(void) printf(">>> %s\n", buf);
#endif
	(void) strcpy(buf2,buf);
	(void) strcat(buf2,"\r\n");
	sz = strlen(buf2);
	if (write(server,buf2,sz) != sz)
		{
#ifdef SYSLOG
		syslog(LOG_ERR,"nntpxfer: write error on server socket");
#else
		(void) printf("nntpxfer: write error on server socket\n");
#endif
		(void) close(server);
		exit(1);
		}
	}

int
wewant(articleid)
char *articleid;
	{
#if defined(DBM) || defined(NDBM)
	datum k, d;
#else
	FILE *k;
	char *histfile();
	FILE *histfp;		/* USG history file */
	char line[BUFSIZ];
	int len;
#endif
	char id[BUFSIZ];
	char *p;

	/* remove any case sensitivity */
	(void) strcpy(id, articleid);
	p = id;
#ifndef CNEWS
	while (*p)
		{
		if (isupper(*p))
			*p = tolower(*p);
		p++;
		}
#endif
#if defined(DBM) || defined(NDBM)
	k.dptr = id;
	k.dsize = strlen(articleid) + 1;

#ifdef DBM
	d = fetch(k);
#else
 	d = dbm_fetch(db, k);
#endif
	if (d.dptr)
		{
#ifdef DEBUG
		(void) printf("dup: '%s'\n", articleid);
#endif
		return(0);
		}
#ifdef DEBUG
	(void) printf("new: '%s'\n", articleid);
#endif
	return(1);
#else
	histfp = fopen(histfile(articleid), "r");
	if (histfp == NULL) 
		{
#ifdef DEBUG
		(void) printf("new: '%s'\n", articleid);
#endif
		return(1);
		}
	len = strlen(articleid);
	while (fgets(line, sizeof (line), histfp))
		if (!strncmp(articleid, line, len))
			break;

	if (feof(histfp)) {
		(void) fclose(histfp);
#ifdef DEBUG
		(void) printf("new: '%s'\n", articleid);
#endif
		return (1);
	}
	(void) fclose(histfp);
#ifdef DEBUG
	(void) printf("dup: '%s' %s\n", articleid,line);
#endif
	return(0);
#endif
}

#ifdef USGHIST
/*
** Generate the appropriate history subfile name
*/
char *
histfile(hline)
char *hline;
{
	char chr;	/* least significant digit of article number */
	static char subfile[BUFSIZ];

	chr = findhfdigit(hline);
	sprintf(subfile, "%s.d/%c", HISTORY_FILE, chr);
	return subfile;
}

findhfdigit(fn)
char *fn;
{
	register char *p;
	register int chr;
	extern char * index();

	p = index(fn, '@');
	if (p != NULL && p > fn)
		chr = *(p - 1);
	else
		chr = '0';
	if (!isdigit(chr))
		chr = '0';
	return chr;
}
#endif
char *
errmsg(code)
int code;
{
	extern int sys_nerr;
	extern char *sys_errlist[];
	static char ebuf[6+5+1];

	if (code > sys_nerr || code < 0) {
		(void) sprintf(ebuf, "Error %d", code);
		return ebuf;
	} else
		return sys_errlist[code];
}

