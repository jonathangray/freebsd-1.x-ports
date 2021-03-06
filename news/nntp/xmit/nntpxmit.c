#ifndef lint
static char * rcsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/xmit/nntpxmit.c,v 1.1 1993/07/19 20:04:35 nate Exp $";
#endif
/* nntpxmit - transmit netnews articles across the internet with nntp
**
** This program is for transmitting netnews articles between sites
** that offer the NNTP service, internet style. There are two forms
** of article transmission that can be used in this environment, since
** the communication is interactive (and relatively more immediate,
** when compared to batched file transfer protocols, like UUCP). They
** are: active send (I have `x', do you want it?) and polling (what
** have you gotten lately?).
**
** 		A C T I V E   S E N D
**
** Sites on the UUCP network generally use active send, without asking
** in advance (that is, unless you got an article from your neighbor,
** or their site is listed in the Path: header already, you assume
** they don't have it and send it along). There is an ihave/sendme
** protocol for doing active send over batched links, but I claim that
** it won't work well because of the high latency between queueing
** and actual transfer that UUCP links typically have. That is, you'll
** still end up with a high rate of duplicate articles being sent over
** that type of link.
**
** With NNTP-based IHAVE, the update window in which another site can
** give the remote the article you just offered him is the time between
** the remote telling you it doesn't have the article, and your
** completed transfer of the article (pretty small). In practice, we
** still get duplicates, but generally from two problems: synchronized
** transmission of an article from two different neighbors (this can
** only be fixed by putting inews(1) into nntpd), and by articles
** being accepting during an expire(1) run (expire locks out inews
** processing while it is running, and articles collect until expire
** is done; since accepted article message-ids aren't added to
** the history file until expire is done, several clients can offer
** you the same article, and you'll accept all the copies offered you.
** When rnews gets run after expire, it will reject the duplicates).
**
** 		P O L L I N G
**
** Polling presents some article and distribution security problems,
** because the server has no contol over what a transmission client
** will ask for, and it must therefore control what it tells a client
** in response to a query.
**
** Articles that appear in local newsgroup hierarchies, or appear in
** the generally distributed USENET newsgroups with local distributions
** have to be filtered out from the list of message-IDs that the server
** gives to a client in response to a NEWNEWS query, or filtered when
** the server fetches the articles off the disk in response to an
** ARTICLE command (and therefore has complete access to the required
** information). Otherwise, distributions will leak.
**
** The other problem with polling is that a good client should keep track
** of when it last successfully polled a server, so that it doesn't force
** the server to dump its entire history file across the network, and this
** involves more file locking and manipulations routines.
**
** nntpxmit only implements active send, for now.
**
** Erik E. Fair <fair@ucbarpa.berkeley.edu>, Dec 4, 1987
** Stan Barber <sob@bcm.tmc.edu>, Jan 1, 1989
*/

#include "../common/conf.h"
#include "nntpxmit.h"
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#if defined(BSD_42) || defined(BSD_43)
#include <sys/resource.h>
#else
#include <sys/times.h>
extern	time_t	time();
#endif
#include <sys/file.h>
#include <fcntl.h>
#include <signal.h>
#ifdef USG
#include "sysexits.h"
#else
#include <sysexits.h>
#endif
#ifdef	SYSLOG
#ifdef FAKESYSLOG
#include "../server/fakesyslog.h"
#else
#include <syslog.h>
#endif
#endif	/* SYSLOG */
#include "../common/nntp.h"
#include "llist.h"

#define	MAXFNAME	BUFSIZ	/* maximum filename size - big enough? */
#define	FCLOSE(fp)	if (fp) (void) fclose(fp); (fp) = (FILE *)NULL

char	*getline();
char	*getmsgid();
char	*errmsg();
void	requeue();
SIGRET	catchsig();
void	restsig();
void	logstats();
void	log();
int	interrupted();

/*
** Globals that certain things need.
**
** Various subroutines want the program name to report errors.
** The queue file, queue file pointer and current article name are
** there to write out the state of the queue file from a signal handler
** (that is, the list of unsent and (possibly) failed articles) so
** that when next we try sending to a given remote site, we don't send
** stuff we've already sent.
*/
char	*Pname;			/* this program's invocation name */
char	*Host;			/* current remote host */
char	*Qfile;			/* current queue file we're operating on */
FILE	*Qfp;			/* the (FILE *) for above */
char	Article[MAXFNAME];	/* current article filename */

/*
** Some flags, toggled by arguments
*/
#define	TOGGLE(boolean)	(boolean) = !(boolean)
char	Debug = FALSE;
char	Report_Stats = TRUE;
char	ReQueue_Fails = TRUE;

char	*USAGE = "USAGE: nntpxmit [-d][-s][-r][-T][-F][-D] hostname|hostname:file [...]";
char	*Fmt = "%s localhost %s[%d]: %s\n";
char	*E_fopen = "fopen(%s, \"%s\"): %s";
char	*E_unlk = "unlink(%s): %s";

ll_t	FailedArticles;		/* list of failed articles */

struct {
	u_long	offered;
	u_long	accepted;
	u_long	rejected;
	u_long	failed;
} Stats = {0L, 0L, 0L, 0L};

double Tbegin, Tend;		/* transfer timestamps */

extern	int	errno;
extern 	int	strncmp();
extern	char	*rindex();
extern	char	*index();
extern	char	*mktemp();
extern	char	*strcpy();
extern	char	*strcat();

#ifdef	USG
void
bzero(s, l)
register caddr_t s;
register int	l;
{
	while(l-- > 0) *s++ = 0;
}
#endif	/* USG */

main(ac, av)
int	ac;
char	*av[];
{
	register int	i;
	int	transport = T_IP_TCP;	/* default is IP/TCP */
	int	isQfile = TRUE;		/* file arg is a Queue file */
#if	defined(BSD_42) || defined(BSD_43)
	struct timeval tod;
	struct timezone tz;

	(void) gettimeofday(&tod, &tz);
	Tbegin = tod.tv_sec + (double)tod.tv_usec/1000000.;
#else
	Tbegin = (double) time((time_t *)NULL);
#endif

	Pname = ((Pname = rindex(av[0],'/')) ? Pname + 1 : av[0]);
	
	if (ac < 2) {
		fprintf(stderr, "%s: %s\n", Pname, USAGE);
		exit(EX_USAGE);
	}

#ifdef	SYSLOG
	/* 4.2 BSD openlog has only two args */
#ifdef	BSD_42
	(void) openlog(Pname, LOG_PID);
#else
	(void) openlog(Pname, LOG_PID, SYSLOG);
#endif	/* BSD_42 */
#endif	/* SYSLOG */

	for(i = 1; i < ac; i++) {
		if (av[i][0] == '-') {
			switch(av[i][1]) {
			case 'T':
				transport = T_IP_TCP;
				break;
			case 'D':
				transport = T_DECNET;
				break;
			case 'F':
				transport = T_FD;
				break;
			case 's':
				TOGGLE(Report_Stats);
				break;
			case 'd':
				TOGGLE(Debug);
				break;
			case 'r':
				TOGGLE(ReQueue_Fails);
				break;
			case 'a':
				isQfile = FALSE;
				break;
			default:
				fprintf(stderr, "%s: no such option: -%c\n",
					Pname, av[i][1]);
				fprintf(stderr, "%s: %s\n", Pname, USAGE);
				exit(EX_USAGE);
			}
			continue;
		}

		/*
		** OK, it wasn't an option, therefore it must be a
		** hostname, filename pair.
		**
		** If the user typed host::file, then it's DECNET,
		** whether they remembered the "-D" option or not.
		*/
		Host = av[i];
		if ((Qfile = index(Host, ':')) != (char *)NULL) {
			if (Qfile[1] == ':') {
				transport = T_DECNET;
				*Qfile++ = '\0';
			} else if (transport != T_FD)
				transport = T_IP_TCP;
			*Qfile++ = '\0';
		} else
			Qfile = Host;

		bzero((caddr_t)&Stats, sizeof(Stats));
		if (isQfile) {
			if (sendnews(Host, transport, Qfile, isQfile) && Report_Stats) {
				logstats();
			}
		} else {
			/* one-shot */
			(void) strcpy(Article, Qfile);
			exit(sendnews(Host, transport, Qfile, isQfile) ? EX_OK : EX_TEMPFAIL);
		}
	}
	exit(EX_OK);
}

/*
** Calculate how much time we've used,
** and report that (and the transfer statistics).
**
*/
void
logstats()
{
	static double ouser = 0.0, osys = 0.0;
	double user, sys;
	char buf[BUFSIZ];
#if	defined(BSD_42) || defined(BSD_43)
	struct rusage self, kids;
	struct timeval tod;
	struct timezone tzdummy;

	(void) getrusage(RUSAGE_SELF, &self);
	(void) getrusage(RUSAGE_CHILDREN, &kids);
	(void) gettimeofday(&tod, &tzdummy);

	Tend = tod.tv_sec + (double)tod.tv_usec/1000000.;

	user = self.ru_utime.tv_sec + kids.ru_utime.tv_sec +
		(double) self.ru_utime.tv_usec/1000000. +
		(double) kids.ru_utime.tv_usec/1000000.;
	
	sys = self.ru_stime.tv_sec + kids.ru_stime.tv_sec +
		(double) self.ru_stime.tv_usec/1000000. +
		(double) kids.ru_stime.tv_usec/1000000.;
#else
#define	HZ	60.0	/* typical system clock ticks - param.h */
	struct tms	cpu;

	(void) times(&cpu);

	Tend = (double) time((time_t *)NULL);
	user = (double)(cpu.tms_utime + cpu.tms_cutime) / HZ;
	sys  = (double)(cpu.tms_stime + cpu.tms_cstime) / HZ;
#endif
	sprintf(buf,
		"%s stats %lu offered %lu accepted %lu rejected %lu failed",
		Host, Stats.offered, Stats.accepted, Stats.rejected,
		Stats.failed);
	log(L_INFO, buf);
	sprintf(buf, "%s xmit user %.3f system %.3f elapsed %.3f",
		Host, (user - ouser), (sys - osys), (Tend - Tbegin));
	log(L_INFO, buf);
	/* reset reference point */
	Tbegin = Tend;	
	ouser = user;
	osys = sys;
}

/*
** Given a hostname to connect to, and a file of filenames (which contain
** netnews articles), send those articles to the named host using NNTP.
**
** Return code behavior is different depending upon isQfile.
**
**	TRUE	- return TRUE if we contacted the remote and started
**		  transferring news - this is to decide whether to
**		  record CPU and transfer statistics.
**
**	FALSE	- a one-shot file transfer - return TRUE or FALSE depending
**		  upon whether we successfully transferred the one article.
*/
sendnews(host, transport, file, isQfile)
char	*host, *file;
int	transport, isQfile;
{
#ifdef	FTRUNCATE
	char	*mode = "r+";		/* so we can use ftruncate() */
#else
	char	*mode = "r";
#endif	/* FTRUNCATE */
	char	*msgid;

	if ((Qfp = fopen(file, mode)) == (FILE *)NULL) {
		char	buf[BUFSIZ];

		sprintf(buf, E_fopen, file, mode, errmsg(errno));
		log(L_WARNING, buf);
		return(FALSE);
	}

	/*
	** interlock with other copies of this process.
	** non-blocking.
	*/
	if (isQfile) {
		if (!lockfd(fileno(Qfp), file, DONT_BLOCK)) {
			FCLOSE(Qfp);
			return(FALSE);
		}
	}

	/*
	** Open a connection to the remote server
	*/
	if (hello(host, transport) == FAIL) {
		FCLOSE(Qfp);
		return(FALSE);
	}

	if (isQfile) {
		/*
		** We're sending a batch queue:
		**	open article
		**	get message-ID
		**	send "IHAVE <message-ID>" to remote
		**	read their reply
		**	send article if appropriate
		**	iterate to end of queue file
		*/
		catchsig(interrupted);

		while ((msgid = getline(Qfp, Article, sizeof(Article))) != NULL) {
			if (!sendarticle(host, Article, msgid)) {
				requeue(Article, msgid);
				Article[0] = '\0';
				cleanup();
				goodbye(DONT_WAIT);
				restsig();
				return(TRUE);
			}
		}

		cleanup();
		goodbye(WAIT);
		restsig();
		return(TRUE);
	} else {
		/*
		** Qfp is a netnews article - this is a one-shot
		** operation, exit code dependent upon remote's
		** acceptance of the article
		*/
		register int	retcode;

		FCLOSE(Qfp);
		retcode = sendarticle(host, file, (char *) NULL);
		goodbye(retcode ? WAIT : DONT_WAIT);
		return(retcode && Stats.accepted == 1 && Stats.failed == 0);
	}
}

/*
** Perform one transfer operation:
**	Give IHAVE command
**	Wait for reply, and send article if they ask for it
**	Wait for transfer confirmation, and requeue the article
**		if they drop it.
**	Watch all network I/O for errors, return FALSE if
**		the connection fails and we have to cleanup.
*/
sendarticle(host, file, msgid)
char	*host;
char	*file;
char	*msgid;
{
	register int	code;
	FILE	*fp = NULL;
	int	error;
	char	buf[BUFSIZ];
	char	*e_xfer = "%s xfer: %s";

	errno = 0;
	if (msgid == NULL || *msgid == '\0') {
		if ((msgid = getmsgid(file, &fp)) == NULL) {
			if (fp) { (void) fclose(fp); fp = NULL; }
			return TRUE;
		}
	}
	switch(code = ihave(msgid)) {
	case CONT_XFER:
		/*
		** They want it. Give it to 'em.
		*/
		if (!fp) { fp = fopen(file, "r"); }
		if (fp == NULL && errno != ENOENT) {
			/* Worse than "No such file or directory"? */
			sprintf(buf, E_fopen, file, "r", errmsg(errno));
			log(L_WARNING, buf);
			goodbye(DONT_WAIT);
			exit(EX_OSERR);
		}
		if (fp == NULL) {
			/* Hmph. The file didn't exist. */
			error = sendcmd(".");
		} else {
			error = !sendfile(fp);
			(void) fclose(fp);
			fp = NULL;
		}
		if (error) {
			sprintf(buf, "%s xfer: sendfile: %s",
				host, errmsg(errno));
			log(L_NOTICE, buf);
			Stats.failed++;
			if (fp) { (void) fclose(fp); fp = NULL; }
			return(FALSE);
		}
		/*
		** Did the article transfer OK?
		** Stay tuned to this same socket to find out!
		*/
		errno = 0;
		if ((code = readreply(buf, sizeof(buf))) != OK_XFERED) {
			Stats.failed++;
			if (code < 0) {
				if (errno > 0) {
					sprintf(buf, e_xfer, host, errmsg(errno));
					log(L_NOTICE, buf);
				} else {
					char errbuf[BUFSIZ];

					sprintf(errbuf, e_xfer, host, buf);
					log(L_NOTICE, errbuf);
				if (fp) { (void) fclose(fp); fp = NULL; }
				}
				return(FALSE);
			}
			if (ReQueue_Fails && code != ERR_XFERRJCT && fp != NULL) {
				requeue(Article, msgid);
				Article[0] = '\0';
			}
		}
		break;
	case ERR_GOTIT:
		/* they don't want it */
		break;
	case ERR_XFERFAIL:
		if (fp) { (void) fclose(fp); fp = NULL; }
		/* they can't do it right now, but maybe later */
		return(FALSE);
		break;
	default:
		if (code < 0) {
			if (errno > 0) {
				sprintf(buf, e_xfer, host, errmsg(errno));
				log(L_NOTICE, buf);
			} else {
				sprintf(buf, e_xfer, host, "ihave");
				log(L_NOTICE, buf);
			}
		} else {
			sprintf(buf, "%s improper response to IHAVE: %d while offering %s", host, code, Article);
			log(L_WARNING, buf);
			if (fp) { (void) fclose(fp); fp = NULL; }
		}
		return(FALSE);
	}
	if (fp) { (void) fclose(fp); fp = NULL; }
	return(TRUE);
}

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

/*
** strip leading and trailing spaces
*/
char *
sp_strip(s)
register char	*s;
{
	register char	*cp;

	if (s == NULL)
		return(NULL);

	if (*s == '\0')
		return(s);
	
	cp = &s[strlen(s) - 1];
	while(cp > s && isspace(*cp))
		cp--;

	*++cp = '\0';	/* zap trailing spaces */

	for(cp = s; *cp && isspace(*cp); cp++)
		continue;

	return(cp);	/* return pointer to first non-space */
}

/*
** convert `s' to lower case
*/
char *
lcase(s)
register char	*s;
{
	register char	*cp;

	if (s == (char *)NULL)
		return(s);

	for(cp = s; *cp != '\0'; cp++)
		if (isupper(*cp))
			*cp = tolower(*cp);
	return(s);
}

/*
** Get the message-id header field data with a minimum of fuss.
*/
char *
getmsgid(file, fpp)
char *file;
FILE **fpp;
{
	static	char	buf[BUFSIZ];
	static	char	msgid[] = "message-id";
	register char	*cp, *cp2;

	*fpp = fopen(file, "r");
	if (*fpp == NULL) return NULL;

	while(fgets(buf, sizeof(buf), *fpp) != NULL) {
		switch(buf[0]) {
		case '\n':
			(void) fclose(*fpp);
			*fpp = NULL;
			return NULL;	/* EOH, we failed */
		case 'M':
		case 'm':
			cp = index(buf, ':');
			if (cp == NULL) continue;
			*cp++ = '\0';
			if (strncmp(lcase(buf), msgid, strlen(msgid)) == 0) {
				/* dump extraneous trash - umass.bitnet */
				/* hope nobody quotes an '>' in a msgid */
				cp2 = index(cp, '>');
				if (cp2 != NULL) *++cp2 = '\0';
				(void) rewind(*fpp);
				return(sp_strip(cp));
			}
			break;
		}
	}
	(void) fclose(*fpp);
	*fpp = NULL;
	return NULL;	/* EOF, failed. */
}

#ifdef	notdef	/* nobody obeys the triply damned protocol anyway! */
/*
** Special characters, see RFC822, appendix D.
*/
isspecial(c)
char	c;
{
	char	*specials = "()<>@,;:\\\".[]";

	return(index(specials, c) != (char *)NULL ? TRUE : FALSE);
}

/*
** Check on the validity of an RFC822 message-id
**
** By The Book, RFC822 Appendix D.
**	msg-id		= "<" addr-spec ">"
**	addr-spec	= local-part "@" domain
**	local-part	= word *("." word)
**	word		= atom / quoted-string
**	domain 		= sub-domain *("." sub-domain)
**	sub-domain	= domain-ref / domain-literal
**	domain-ref	= atom
**	domain-literal	= "[" *(dtext / quoted-pair) "]"
**
** NOTE: close reading of the RFC822 spec indicates that a fully
**	qualified domain name (i.e. one with at least one dot) is
**	NOT required in the domain part of the addr-spec. However,
**	I've decided to be an asshole and require them, since we'll 
**	all die a slow death later on if I don't at this juncture.
**	To disable, if you disagree with me, see the last return
**	statement. - Erik E. Fair <fair@ucbarpa.berkeley.edu>
**	May 30, 1986
*/
msgid_ok(id)
register char	*id;
{
	register Langle = FALSE;
	register Rangle = FALSE;
	register local_part = FALSE;
	register at = FALSE;
	register dot = FALSE;

	/* skip up to the opening angle bracket */
	if (id == (char *)NULL || (id = index(id, '<')) == (char *)NULL)
		return(FALSE);		/* don't waste my time! */

	for(; *id != '\0'; id++) {
		switch(*id) {
		case '<':
			if (Langle) return(FALSE);
			Langle = local_part = TRUE;
			break;
		case '>':
			if (Rangle || !Langle || !at) return(FALSE);
			else Rangle = TRUE;
			break;
		case '@':		/* should be a domain spec */
			at = TRUE;
			local_part = FALSE;
			break;
		case '.':
			dot = at;
			break;
		case '\\':
			/*
			** quoted pair; this disallows NULs, but how
			** many mailers would die if someone used one?
			*/
			if (!local_part || (*++id) == '\0') return(FALSE);
			break;
		case '"':
			/*
			** quoted string
			*/
			if (!local_part) return(FALSE);
			do {
				switch(*++id) {
				case '\\':
					if ((*++id) == '\0') return(FALSE);
					break;
				case '\r':
					return(FALSE);
				}
			} while(*id != '\0' && *id != '"');
			break;
		case '[':
			/*
			** domain literal
			*/
			if (local_part) return(FALSE);
			do {
				switch(*++id) {
				case '\\':
					if ((*++id) == '\0') return(FALSE);
					break;
				case '\r':
					return(FALSE);
				}
			} while(*id != '\0' && *id != ']');
			break;
		default:
			if (!isascii(*id) || iscntrl(*id) || isspace(*id) || isspecial(*id))
				return(FALSE);	/* quit immediately */
			break;
		}
	}
	return(at && dot && Langle && Rangle);
}
#else notdef

/*
** Simpleton's check for message ID syntax.
** A concession to the realities of the ARPA Internet.
*/
msgid_ok(s)
register char *s;
{
	register char	c;
	register in_msgid = FALSE;

	if (s == (char *)NULL)
		return(FALSE);

	while((c = *s++) != '\0') {
		if (!isascii(c) || iscntrl(c) || isspace(c))
			return(FALSE);
		switch(c) {
		case '<':
			in_msgid = TRUE;
			break;
		case '>':
			return(in_msgid);
		}
	}
	return(FALSE);
}
#endif	/* notdef */

/*
** Read the header of a netnews article, snatch the message-id therefrom,
** and ask the remote if they have that one already.
*/
ihave(id)
char	*id;
{
	register int	code;
	char	buf[BUFSIZ];

	if (id == NULL || *id == '\0') {
		/*
		** something botched locally with the article
		** so we don't send it, but we don't break off
		** communications with the remote either.
		*/
		sprintf(buf, "%s: message-id missing!", Article);
		log(L_DEBUG, buf);
		return(ERR_GOTIT);
	}

	if (!msgid_ok(id)) {
		sprintf(buf, "%s: message-id syntax error: %s", Article, id);
		log(L_DEBUG, buf);
		return(ERR_GOTIT);
	}

again:
	sprintf(buf, "IHAVE %s", id);
	Stats.offered++;

	switch(code = converse(buf, sizeof(buf))) {
	case CONT_XFER:
		Stats.accepted++;
		return(code);
	case ERR_GOTIT:
		Stats.rejected++;
		return(code);
#ifdef AUTH
	case ERR_NOAUTH:
		xmitauth(Host);
		goto again;
#endif
	default:
		return(code);
	}
}

/*
** Read the next line from fp into line,
** break it apart into filename and message-id,
** and return a pointer to the message-id.
** Returns "" if no message-id.
** Returns NULL at end of file.
*/
char *
getline(fp, line, len)
FILE	*fp;
char	*line;
int	len;
{
	register char	*cp;

	do {
		if (fgets(line, len, fp) == NULL) return NULL;
		line[len - 1] = '\0';

		cp = index(line, '\n');
		if (cp != NULL) *cp = '\0';
	} while (line[0] == '\0');

	cp = &line[0];
	while (*cp != '\0' && !isspace(*cp)) ++cp;
	if (*cp != '\0') {
		*cp++ = '\0';
		while (*cp != '\0' && isspace(*cp)) ++cp;
		/* cp now points to the message-id, if any. */
	}
	return cp;
}

/*
** OK, clean up any mess and requeue failed articles
*/
cleanup()
{
	dprintf(stderr, "%s: cleanup()\n", Pname);
	if (Qfp == (FILE *)NULL || Qfile == (char *)NULL)
		return;

	if ((ReQueue_Fails && Stats.failed > 0) || !feof(Qfp)) {
		rewrite();
	} else {
		/*
		** Nothing to clean up after, reset stuff and
		** nuke the queue file.
		*/
		requeue((char *)NULL, (char *)NULL);
		if (feof(Qfp)) {
			dprintf(stderr, "%s: unlink(%s)\n", Pname, Qfile);
			if (unlink(Qfile) < 0) {
				char	buf[BUFSIZ];

				sprintf(buf, E_unlk, Qfile, errmsg(errno));
				log(L_WARNING, buf);
			}
		}
		FCLOSE(Qfp);
	}
}
 
/*
** Add an article file name to an allocated linked list,
** so that we can rewrite it back to the queue file later.
** Calling this with a NULL pointer resets the internal pointer.
*/
void
requeue(article, msgid)
char *msgid;
char *article;
{
	char buf[BUFSIZ];
	static ll_t *lp = &FailedArticles;

	if (article == (char *)NULL) {
		dprintf(stderr, "%s: requeue(): reset\n", Pname);
		goto reset;		/* this is for our static pointer */
	}

	if (*article == '\0')
		return;

	(void) strcpy(buf, article);
	if (msgid != NULL && *msgid != '\0') {
		(void) strcat(strcat(buf, " "), msgid);
	}

	dprintf(stderr, "%s: requeue(%s)\n", Pname, buf);
	if ((lp = l_alloc(lp, buf, strlen(buf) + 1)) == (ll_t *)NULL) {
		fprintf(stderr, "%s: requeue(%s) failed, dumping fail list\n",
			Pname, buf);
		/*
		** Wow! Did you know that this could blow the stack
		** if we recurse too deeply? I sure didn't!
		*/
reset:
		l_free(&FailedArticles);
		lp = &FailedArticles;
	}
}

/*
** Note that if I'm not running as "news" or "usenet" (or whatever
** account is supposed to own netnews), the resultant file will be the
** wrong ownership, permissions, etc.
*/
rewrite()
{
	register ll_t	*lp;
	register FILE	*tmpfp;
	register int	nart = 0;
	char	*mode = "w+";
	static char template[] = "/tmp/nntpxmitXXXXXX";
	char	buf[BUFSIZ];
	static char	*tempfile = (char *)NULL;

	dprintf(stderr, "%s: rewrite(%s)\n", Pname, Qfile);

	if (tempfile == (char *)NULL)	/* should only need this once */
		tempfile = mktemp(template);

	if ((tmpfp = fopen(tempfile, mode)) == (FILE *)NULL) {
		sprintf(buf, E_fopen, tempfile, mode, errmsg(errno));
		log(L_WARNING, buf);
		FCLOSE(Qfp);
		return;
	}

	/*
	** Requeue the rest of the queue file first,
	** so that failed articles (if any) go to the end
	** of the new file.
	*/
	if (!feof(Qfp)) {
		dprintf(stderr, "%s: copying the unused portion of %s to %s\n",
			Pname, Qfile, tempfile);
		while(fgets(buf, sizeof(buf), Qfp) != (char *)NULL)
			(void) fputs(buf, tmpfp);
	}

	/*
	** Here we write out the filenames of articles which
	** failed at the remote end.
	*/
	dprintf(stderr, "%s: writing failed article filenames to %s\n",
		Pname, tempfile);
	L_LOOP(lp, FailedArticles) {
		fprintf(tmpfp, "%s\n", lp->l_item);
		nart++;
	}
	dprintf(stderr, "%s: wrote %d article filenames to %s\n",
		Pname, nart, tempfile);

	(void) fflush(tmpfp);
	/*
	** If writing the temp file failed (maybe /tmp is full?)
	** back out and leave the queue file exactly as it is.
	*/
	if (ferror(tmpfp)) {
		sprintf(buf, "rewrite(): copy to %s failed", tempfile);
		log(L_WARNING, buf);
		(void) fclose(tmpfp);
		FCLOSE(Qfp);
		if (unlink(tempfile) < 0) {
			sprintf(buf, E_unlk, tempfile, errmsg(errno));
			log(L_WARNING, buf);
		}
		requeue((char *)NULL,(char *)NULL);	/* reset */
		return;
	}

	rewind(tmpfp);
#ifdef	FTRUNCATE
	rewind(Qfp);
	if (ftruncate(fileno(Qfp), (off_t)0) < 0) {
		sprintf(buf, "ftruncate(%s, 0): %s", Qfile, errmsg(errno));
		log(L_WARNING, buf);
		FCLOSE(Qfp);
		(void) fclose(tmpfp);
		if (unlink(tempfile) < 0) {
			sprintf(buf, E_unlk, tempfile, errmsg(errno));
			log(L_WARNING, buf);
		}
		requeue((char *)NULL,(char *)NULL);	/* reset */
		return;
	}
#else
	FCLOSE(Qfp);	/* we just nuked our lock here (lockfd) */
	if ((Qfp = fopen(Qfile, mode)) == (FILE *)NULL) {
		sprintf(buf, E_fopen, Qfile, mode, errmsg(errno));
		log(L_WARNING, buf);
		(void) fclose(tmpfp);
		if (unlink(tempfile) < 0) {
			sprintf(buf, E_unlk, tempfile, errmsg(errno));
			log(L_WARNING, buf);
		}
		requeue((char *)NULL,(char *)NULL);	/* reset */
		return;
	}
	/* Try to get our lock back (but continue whether we do or not) */
	(void) lockfd(fileno(Qfp), Qfile, DONT_BLOCK);
#endif	/* FTRUNCATE */

	dprintf(stderr, "%s: copying %s back to %s\n", Pname, tempfile, Qfile);
	while(fgets(buf, sizeof(buf), tmpfp) != (char *)NULL)
		(void) fputs(buf, Qfp);

	(void) fflush(Qfp);
	if (ferror(Qfp)) {
		sprintf(buf, "rewrite(): copy to %s failed", Qfile);
		log(L_WARNING, buf);
	}
	(void) fclose(tmpfp);
	FCLOSE(Qfp);
	if (unlink(tempfile) < 0) {
		sprintf(buf, E_unlk, tempfile, errmsg(errno));
		log(L_WARNING, buf);
	}
	requeue((char *)NULL,(char *)NULL);		/* reset */
	dprintf(stderr, "%s: rewrite(%s): done\n", Pname, Qfile);
	return;
}

/*
** Signal stuff
**
** There's probably too much stuff to do in this signal
** handler, but we're going to exit anyway...
*/
interrupted(sig)
int	sig;
{
	char buf[BUFSIZ];

#ifndef RELSIG
	catchsig(SIG_IGN);	/* for System V - hope we're quick enough */
#endif	/* RELSIG */
	sprintf(buf, "%s signal %d", Host, sig);
	log(L_NOTICE, buf);
	requeue(Article,(char *)NULL);
	cleanup();
	if (Report_Stats)
		logstats();
	goodbye(DONT_WAIT);
	exit(EX_TEMPFAIL);
}

struct {
	int	signo;
	ifunp	state;
} SigList[] = {
	{SIGHUP},
	{SIGINT},
	{SIGQUIT},
	{SIGTERM},
	{NULL}
};

SIGRET
catchsig(handler)
ifunp	handler;
{
	register int	i;

	if (handler != SIG_IGN) {
		for(i = 0; SigList[i].signo != NULL; i++) {
			SigList[i].state = signal(SigList[i].signo, handler);
		}
	} else {
		for(i = 0; SigList[i].signo != NULL; i++) {
			(void) signal(SigList[i].signo, handler);
		}
	}
}

void
restsig()
{
	register int	i;

	for(i = 0; SigList[i].signo != NULL; i++) {
		if (SigList[i].state != (ifunp)(-1))
			(void) signal(SigList[i].signo, SigList[i].state);
	}
}

/*
** log stuff
*/
void
log(importance, error)
int	importance;
char	*error;
{
	int skip = FALSE;
	FILE	*report = (importance == L_INFO ? stdout : stderr);
	fprintf(report, "%s: %s\n", Pname, error);
#ifdef	SYSLOG 
	switch(importance) {
#ifdef LOG
	case L_DEBUG:	importance = LOG_DEBUG;		break;
#else
	case L_DEBUG:	skip = TRUE;			break;
#endif
	case L_INFO:	importance = LOG_INFO;		break;
	case L_NOTICE:	importance = LOG_NOTICE;	break;
	case L_WARNING:	importance = LOG_WARNING;	break;
	default:	importance = LOG_DEBUG;		break;
	}
	if (skip == FALSE) syslog(importance, error);
#endif	/* SYSLOG */
}

/*
** Lock a file descriptor
**
** NOTE: if the appropriate system calls are unavailable,
** this subroutine is a no-op.
*/
lockfd(fd, file, non_blocking)
int	fd, non_blocking;
char	*file;			/* just for error reporting */
{
	char	buf[BUFSIZ];
#ifdef	USG
#ifdef	F_TLOCK
	if (lockf(fd, (non_blocking ? F_TLOCK : F_LOCK), 0) < 0) {
		if (errno != EACCES) {
			sprintf(buf, "lockf(%s): %s\n", file, errmsg(errno));
			log(L_WARNING, buf);
		}
		return(FALSE);
	}
#endif	/* F_TLOCK */
#else
#ifdef	LOCK_EX
	if (flock(fd, LOCK_EX|(non_blocking ? LOCK_NB : 0)) < 0) {
		if (errno != EWOULDBLOCK) {
			sprintf(buf, "flock(%s): %s\n", file, errmsg(errno));
			log(L_WARNING, buf);
		}
		return(FALSE);
	}
#endif	/* LOCK_EX */
#endif	/* USG */
	return(TRUE);
}
