/*
 * relaynews - relay Usenet news (version C)
 * See the file COPYRIGHT for the copyright notice.
 *
 * relaynews should be setuid-news and setgid-news on systems that lack a
 * mkdir system call, thus requiring the invocation of a setuid-root mkdir
 * command.  You'll need to install setnewsids setuid-root if
 * setuid(geteuid()) doesn't work on such systems (e.g. on V7 and possibly
 * System III).
 *
 * Written by Geoff Collyer, 15-20 November 1985 and revised periodically
 * since.
 *
 * relaynews parses article headers, rejects articles by newsgroup &
 * message-id, files articles, updates the active & history files,
 * transmits articles, and honours (infrequent) control messages, which do
 * all sorts of varied and rococo things.  Control messages are implemented
 * by separate programs.  relaynews reads a "sys" file to control the
 * transmission of articles but can function as a promiscuous leaf node
 * without one.  See Internet RFC 1036 for the whole story and RFC 850
 * for background.
 *
 * A truly radical notion: people may over-ride via environment variables
 * the compiled-in default directories so IHCC kludges are not needed and
 * testing is possible (and encouraged) in alternate directories.  This
 * does cause a loss of privilege, to avoid spoofing.
 *
 * The disused old unbatched ihave/sendme protocol is gone because it was
 * too wasteful; use the batched form instead (see the ihave sys flag
 * ("I") instead).
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <signal.h>		/* to make locking safe */
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include <sys/stat.h>

#include "libc.h"
#include "news.h"
#include "config.h"
#include "fgetmfs.h"
#include "active.h"
#include "caches.h"
#include "fileart.h"
#include "headers.h"
#include "history.h"
#include "transmit.h"

/*
 * setuid-root program to set ids to news/news & re-exec rnews with
 * NEWSPERMS in the environment to break loops.
 */
#ifndef SETNEWSIDS
#define SETNEWSIDS "setnewsids"
#endif

/* exports */
char *progname = "relaynews";
boolean okrefusal = YES;			/* okay to refuse articles? */
char *exclude = NULL;				/* site to exclude, for erik */
boolean histreject = NO;			/* keep history of rejects? */
long staledays = 0;			/* articles stale after this many days */
boolean genxref = NO;			/* iff true, always generate Xref: */
boolean blvxref = NO;			/* iff true, believe Xref: contents */
char *blvsite = NULL;			/* ... for this site only. */
boolean dupsokay = NO;			/* iff true, allow duplicates ... */
char *dupsite = NULL;			/* ... from this site only. */

/* internal */
static boolean userealids = NO;
static boolean uunlink = NO;
static char incsfx[] = "in.coming";	/* need this for -u, alas */

/* imports */
extern int optind;			/* set by getopt */
extern char *optarg;
extern statust cpinsart();		/* from procart.c */

/* forwards */
extern void prelude(), setids(), procopts(), redirectlogs(), logfile();
extern void getwdandcd();
extern statust procargs(), relnmprocess(), process(), unbatch();
extern boolean batchln();
FORWARD boolean debugon();
FORWARD long maxlong();

/*
 * main - take setuid precautions, switch to "news" ids, ignore signals,
 * handle options, lock news system, process files & unlock news system.
 */
int
main(argc, argv)
int argc;
char *argv[];
{
	statust status = ST_OKAY;
	int redirlogs = 0;	/* redirect n std output streams to logs */
	char *origdir = NULL;		/* current directory at start */

	if (argc > 0)
		progname = argv[0];
#ifdef CSRIMALLOC
	mal_debug(0);	/* was 2; 3 is too slow */
	mal_leaktrace(0);	/* was 1 */
#endif
	prelude(argv);		/* various precautions; switch to "news" */

	/* ignore signals (for locking). relaynews runs quickly, so don't worry. */
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGTERM, SIG_IGN);
	(void) signal(SIGPIPE, SIG_IGN);	/* we check write returns */

	procopts(argc, argv, &origdir, &redirlogs, &okrefusal);

	(void) morefds();		/* ask Unix for more descriptors */
	newslock();			/* done here due to dbm internal cacheing */
	if (redirlogs > 0) {
		redirectlogs(redirlogs); /* redirect std output streams to logs */
#ifdef MANYERRORS
		(void) putc('\n', stderr);	/* leave a blank line */
		/* prints "Jun  5 12:34:56" */
		timestamp(stderr, (time_t *)NULL);
		(void) putc('\n', stderr);
#endif
	}

	getwdandcd(argc, argv, &origdir);
	status |= procargs(argc, argv, &origdir);

	status |= synccaches();		/* being cautious: write & close caches */
	status |= closehist();
	(void) fflush(stdout);		/* log file */
	(void) fflush(stderr);		/* errlog file */

#ifdef notdef
#ifdef CSRIMALLOC
	mal_dumpleaktrace(fileno(stderr));
#endif
#endif
	newsunlock();
	exit(status);
	/* NOTREACHED */
}

/*
 * reset various environmental things for safety: umask, alarm,
 * environment variables (PATH, IFS), standard file descriptors,
 * user & group ids.
 */
void
prelude(argv)				/* setuid daemon prelude */
char **argv;
{
	register char *newpath;

	(void) umask(newsumask());
	(void) alarm(0);		/* cancel any pending alarm */
	/* TODO: suppress chatter, lock freeing on failure here */
	newpath = str3save("PATH=", newspath(), "");
	if (newpath == NULL)
		exit(1);		/* no chatter until stdfdopen */
	if (putenv(newpath) ||
	    putenv("SHELL=/bin/sh") ||
	    putenv("IFS= \t\n"))
		exit(1);		/* no chatter until stdfdopen */
	closeall(1);			/* closes all but std descriptors */
	stdfdopen();			/* ensure open standard descriptors */
	setids(argv);			/* change of real and effective ids */
}

/*
 * change real and effective ids to real ids if unprivileged() is called,
 * else to effective ("news") ids.  ctlfile((char *)0) will trigger a call
 * to unprivileged() if any environment variables override the default
 * path names.  unprivileged() in turn sets userealids.
 *
 * If setuid(geteuid()) fails, try execing a small, setuid-root program
 * to just do "getpwnam(), getgrnam() (with NEWSPERMS set), setgid(),
 * setuid()," and exec this program again.  If NEWSPERMS is set,
 * the failure is a fatal error (recursive loop).
 * This program (relaynews) can be setuid-news.
 *
 * The peculiar tests for failure (getuid() != newsuid) are to work
 * around a Xenix bug which returns 0 from setuid() upon failure.
 */
void
setids(argv)
char **argv;
{
	int newsuid, newsgid;

	(void) ctlfile((char *)NULL);
	if (userealids)
		newsuid = getuid(), newsgid = getgid();
	else
		newsuid = geteuid(), newsgid = getegid();
	if (setgid(newsgid) < 0 || setuid(newsuid) < 0 ||
	    getgid() != newsgid || getuid() != newsuid) {
		if (getenv("NEWSPERMS") != 0)
			error("recursive loop setting ids", "");
		/*
		 * normally we would use execvp, but for security SETNEWSIDS
		 * had better be an absolute path to a binary.
		 */
		/* TODO: ctlfile -> binfile */
		execv(ctlfile(SETNEWSIDS), argv);
		error("can't exec `%s' to set ids", ctlfile(SETNEWSIDS));
		/* NOTREACHED */
	}
	/* we are now running as news, so you can all relax */
}

/*
 * parse options and set flags
 */
void
procopts(argc, argv, origdirp, redirlogsp, okrefusalp)
int argc;
char **argv;
char **origdirp;
int *redirlogsp;
boolean *okrefusalp;
{
	int c, errflg = 0;
	char *incdir;

	while ((c = getopt(argc, argv, "a:b:d:gino:rsx:u")) != EOF)
		switch (c) {
		case 'a':
			dupsokay = YES;
			dupsite = optarg;
			break;
		case 'b':
			blvxref = YES;
			blvsite = optarg;
			break;
		case 'd':		/* -d debug-options; thanks, henry */
			if (!debugon(optarg))
				errflg++;	/* debugon has complained */
			break;
		case 'g':
			genxref = YES;
			break;
		case 'i':		/* redirect stdout to log (inews) */
			*redirlogsp = 1; /* just stdout */
			break;
		case 'n':		/* nntp mode: keep history of rejects */
			histreject = YES;
			break;
		case 'o':
			/* "oldness": drop articles older than this many days */
			staledays = atol(optarg);
			break;
		case 'r':	/* redirect std. ostreams to logs (rnews) */
			*redirlogsp = 2; /* stdout & stderr */
			break;
		case 's':		/* dropping input is serious (inews) */
			*okrefusalp = NO;
			break;
		case 'u':		/* unlink good batches when done */
			incdir = fullartfile(incsfx);
			if (cwdis(incdir)) {
				uunlink = YES;
				*origdirp = strsave(incdir);
			} else
				(void) fprintf(stderr,
			"%s: -u ignored - current directory is not %s\n",
					progname, incdir);
			break;
		case 'x':		/* -x site: don't send to site */
			/* you're welcome, erik */
			/* erik says he only needs one -x per inews */
			if (exclude != NULL) {
				(void) fprintf(stderr,
					"%s: more than one -x site (%s)\n",
					progname, optarg);
				errflg++;
			} else
				exclude = optarg;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg) {
		(void) fprintf(stderr,
"usage: %s [-ginrsu][-d fhlmt][-x site][-o days][-b xrefsite][-a dupsite]\n",
			progname);
		exit(1);
	}
}

/*
 * called if NEWSARTS, NEWSCTL, NEWSBIN, etc. are non-standard.
 * this may be due to legitimate testing, but we can't tell.
 * the error message will at least be seen by a human trying to
 * track down a problem, even if stderr isn't normally seen.
 */
void
unprivileged(reason)
char *reason;
{
	userealids = YES;
	(void) fprintf(stderr,
	"%s: warning: renouncing setuid due to nonstandard `%s' in environment\n",
							progname, reason);
}

STATIC boolean
debugon(dbopt)
register char *dbopt;
{
	statust status = YES;

	for (; *dbopt != '\0'; dbopt++)
		switch (*dbopt) {
		case 'f':
			filedebug(YES);
			break;
		case 'h':
			hdrdebug(YES);
			break;
		case 'l':
			lockdebug(YES);
			break;
		case 'm':
			matchdebug(YES);
			break;
		case 't':
			transdebug(YES);
			break;
		default:
			status = NO;	/* unknown debugging option */
			(void) fprintf(stderr, "%s: bad -d %c\n",
				progname, *dbopt);
			break;
		}
	return status;
}

/*
 * Redirect stdout or stderr into log files at known locations.
 */
void
redirectlogs(count)
int count;
{
	if (count > 0)
		logfile(stdout, ctlfile("log"));
	if (count > 1)
		logfile(stderr, ctlfile("errlog"));
}

void
logfile(stream, name)			/* redirect stream into name */
FILE *stream;
char *name;
{
	if (freopen(name, "a", stream) == NULL)
		errunlock("can't redirect standard stream to `%s'", name);
}

/*
 * if argv contains relative file name arguments, save current directory name
 * in malloced memory, through origdirp.
 * then change directory to the spool directory ($NEWSARTS).
 */
void
getwdandcd(argc, argv, origdirp)
int argc;
char **argv;
char **origdirp;
{
	register int argind;
	boolean needpwd = NO;
	char dirtmp[MAXPATH];			/* much bigger than needed */

	if (*origdirp == NULL) {			/* not yet set? */
		for (argind = optind; argind < argc; argind++)
			if (argv[argind][0] != FNDELIM)
				needpwd = YES;

		*origdirp = "/???";		/* pessimism */
		if (needpwd && getcwd(dirtmp, sizeof dirtmp) != 0)
			*origdirp = dirtmp;
		*origdirp = strsave(*origdirp);	/* save a smaller copy */
	}
	cd(fullartfile((char *)NULL));		/* move to spool directory */
}

int						/* actually boolean */
cwdis(dir)
char *dir;
{
	struct stat dotstat, dirstat;
	static char dot[] = ".";		/* name of current dir. */

	if (stat(dir, &dirstat) < 0) {
		warning("no %s directory", dir);
		return NO;
	}
	if (stat(dot, &dotstat) < 0) {
		warning("no %s directory", dot);
		return NO;
	}
	return dotstat.st_dev == dirstat.st_dev &&
	       dotstat.st_ino == dirstat.st_ino;
}

/*
 * process files named as arguments (or implied)
 */
statust
procargs(argc, argv, origdirp)
int argc;
char **argv;
char **origdirp;
{
	register statust status = ST_OKAY;

	if (optind == argc)
		status |= process(stdin, "(stdin)");
	else
		for (; optind < argc; optind++)
			status |= relnmprocess(argv[optind], *origdirp);
	nnfree(origdirp);
	return status;
}

statust
relnmprocess(name, origdir)		/* process a (relative) file name */
char *name, *origdir;
{
	register statust status = ST_OKAY;
	register FILE *in;
	register char *fullname = (name[0] != FNDELIM?
		str3save(origdir, SFNDELIM, name): strsave(name));

	in = fopenwclex(fullname, "r");
	if (in != NULL) {
		status |= process(in, fullname);
		(void) nfclose(in);
		/*
		 * in unlink mode, try to unlink good batches in a
		 * known-safe place, but not very hard.
		 * main() ensured that relative names are relative to
		 * $NEWSARTS/in.coming.
		 */
		if (uunlink && !(status&(ST_DROPPED|ST_SHORT)) &&
		    strchr(name, FNDELIM) == NULL) {
			char *incname = str3save(incsfx, SFNDELIM, name);

			(void) unlink(incname);
			free(incname);
		}
	}
	free(fullname);
	return status;
}

/*
 * process - process input file
 * If it starts with '#', assume it's a batch and unravel it,
 * else it's a single article, so just inject it.
 */
statust
process(in, inname)
FILE *in;
char *inname;
{
	register int c;

	if ((c = getc(in)) == EOF)
		return ST_OKAY; 		/* normal EOF */
	(void) ungetc(c, in);
	if (c == '#')
		return unbatch(in, inname);
	else
		/* -SIZENUL is to avoid overflow later during +SIZENUL */
		return cpinsart(in, inname, maxlong() - SIZENUL, NO);
}

/*
 * compute the largest number that can be stored in a long.  in theory, 
 * #define MAXLONG ((long)(~(unsigned long)0 >> 1))
 * will do the job, but old compilers don't have "unsigned long", don't
 * like casts in initialisers, or otherwise miscompute.
 */
STATIC long
maxlong()
{
	register int bits = 0;
	register unsigned word = 1;		/* "unsigned" avoids overflow */
	static long savemaxlong = 0;

	if (savemaxlong > 0)
		return savemaxlong;
	for (bits = 0, word = 1; word != 0; word <<= 1)
		bits++;
	/* bits/sizeof word = bits per char; all bits on but the sign bit */
	savemaxlong = ~(1L << (bits/sizeof word * sizeof savemaxlong - 1));
	if (savemaxlong <= 0) {			/* sanity check */
		errno = 0;
		errunlock("maxlong is non-positive; your compiler is broken", "");
	}
	return savemaxlong;
}

/*
 * Unwind "in" and insert each article.
 * For each article, call cpinsart to copy the article from "in" into
 * a (temporary) file in the news spool directory and rename the temp file
 * to the correct final name if it isn't right already.
 *
 * If the unbatcher gets out of sync with the input batch, the unbatcher
 * will print and discard each input line until it gets back in sync.
 */
statust
unbatch(in, inname)
register FILE *in;
char *inname;
{
	register int c;
	/* register */ char *line;
	register statust status = ST_OKAY;
	long charcnt;

	while (!(status&ST_NEEDATTN) && (c = getc(in)) != EOF) {
		(void) ungetc(c, in);
		while ((line = fgetms(in)) != NULL &&
		    !batchln(line, &charcnt)) {		/* returns charcnt */
			status |= ST_DROPPED;
			(void) fprintf(stderr,
			    "%s: unbatcher out of synch, tossing: ",
				progname);
		    	(void) fputs(line, stderr);
			free(line);
		}
		nnfree(&line);			/* free "#! rnews n" */
		if (!feof(in) && charcnt > 0)	/* anything to do? */
			status |= cpinsart(in, inname, charcnt, YES);
	}
	if (ferror(in))
		errunlock("error reading `%s'", inname);
	return status;
}

/*
 * Is line a batcher-produced line (#! rnews count)?
 * If so, return the count through charcntp.
 * This is slightly less convenient than sscanf, but a lot smaller.
 */
boolean
batchln(line, charcntp)
register char *line;
register long *charcntp;
{
	register char *countp;
	static char batchtext[] = "#! rnews ";

	countp = line + STRLEN(batchtext);
	if (STREQN(line, batchtext, STRLEN(batchtext)) &&
	    isascii(*countp) && isdigit(*countp)) {
		*charcntp = atol(countp);
		return YES;
	} else {
		*charcntp = 0;
		return NO;
	}
}
