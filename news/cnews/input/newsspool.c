/*
 * newsspool - copy incoming news into incoming directory
 *
 * The -i option relies on the parent setting (and exporting) $PATH.
 *
 * -Log-
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include "libc.h"
#include "news.h"
#include "config.h"

#ifndef lint
static char RCSid[] = "$Header: /a/cvs/386BSD/ports/news/cnews/input/newsspool.c,v 1.1 1993/08/27 02:47:21 alm Exp $";
#endif

#ifndef MAXTRIES
#define	MAXTRIES	100	/* limit on attempts to make links */
#endif

int debug = 0;
char *progname;

extern void error(), exit();
#ifdef UTZOOERR
extern char *mkprogname();
#else
#define	mkprogname(a)	(a)
#endif

char buf[BUFSIZ*16];	/* try to get a batch in a few gulps */
int immed = 0;		/* try an immediate newsrun? */
char *suffix = ".t";	/* suffix for filename, default is plain text */
char grade[3] = "";	/* 3 = digit, period, NUL */

void process();
FILE *outopen();
void outclose();
extern time_t time();
char *outname();

/*
 - main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	FILE *in;
	struct stat statbuf;
	extern int optind;
	extern char *optarg;
	extern FILE *efopen();
	void process();

	progname = mkprogname(argv[0]);

	while ((c = getopt(argc, argv, "ig:d")) != EOF)
		switch (c) {
		case 'i':	/* try immediate newsrun */
			immed++;
			break;
		case 'g':	/* grade */
			if (strchr("0123456789", *optarg) == NULL)
				error("invalid grade `%s'", optarg);
			sprintf(grade, "%c.", *optarg);
			break;
		case 'd':	/* Debugging. */
			debug++;
			setbuf(stderr, (char *)NULL);
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg) {
		fprintf(stderr, "usage: %s [file] ...\n", progname);
		exit(2);
	}

	/* probe to get unprivileged() called if necessary */
	(void) ctlfile((char *)NULL);

	/* mktemp() uses access(2) [ARGH!] so minimize chances of trouble */
	(void) setgid(getegid());
	(void) setuid(geteuid());

	(void) umask(newsumask());

	if (optind >= argc)
		process(stdin, "stdin");
	else
		for (; optind < argc; optind++)
			if (STREQ(argv[optind], "-"))
				process(stdin, "-");
			else {
				in = efopen(argv[optind], "r");
				if (fstat(fileno(in), &statbuf) < 0)
					error("can't fstat `%s'", argv[optind]);
				process(in, argv[optind]);
				(void) fclose(in);
			}

	if (immed) {
		/* execlp because shell files may not be directly execable */
		execlp(binfile("input/newsrun"), "newsrun", (char *)NULL);
		error("attempt to run newsrun failed!", "");
	}
	exit(0);
}

/*
 * process - process input file
 */
/* ARGSUSED */
void
process(in, inname)
FILE *in;
char *inname;
{
	register int count;
	register int firstblock;
	FILE *out;
	register char *p;
	register int n;
	char *name;
	register int wrotesome = 0;

	name = outname();
	out = outopen(name);

	/* do the copying */
	firstblock = 1;
	while ((count = fread(buf, sizeof(char), sizeof(buf), in)) > 0) {
		if (firstblock) {
			n = cunskip(buf, count);
			p = buf + n;
			count -= n;
			firstblock = 0;
		} else
			p = buf;
		if (count > 0) {
			n = fwrite(p, sizeof(char), count, out);
			if (n != count)
				error("write error in output to `%s'", name);
			wrotesome = 1;
		}
	}

	outclose(out, name, wrotesome);
}

/*
 - outname - construct name for the temporary output file
 */
char *
outname()
{
	register char *p;

	p = strsave(fullartfile("in.coming/nspool.XXXXXX"));
	mktemp(p);
	return(p);
}

/*
 - outopen - acquire an output file
 */
FILE *
outopen(name)
char *name;
{
	FILE *f;

	f = fopen(name, "w");
	if (f == NULL)
		error("unable to create temporary `%s'", name);
	if (debug)
		fprintf(stderr, "output into %s\n", name);

	return(f);
}

/*
 - outclose - close output file, moving it to the right place
 *
 * Names are based on the current time in hopes of keeping input in order.
 */
void
outclose(f, tmpname, wrotesome)
FILE *f;
char *tmpname;
int wrotesome;			/* did anything actually get written to it? */
{
	register char *p;
	register char *name;
	register int ntries;
	time_t now;

	if (nfclose(f) == EOF)
		error("close error on file `%s'", tmpname);
	if (!wrotesome) {
		(void) unlink(tmpname);
		return;
	}

	if (!mkinperm(tmpname, grade, suffix))
		error("couldn't move %s into the in.coming queue", tmpname);
	if (debug)
		fprintf(stderr, "succeeded\n");
}

/*
 - cunskip - inspect block for silly #! cunbatch headers, classify input
 */
int				/* number of chars at start to skip */
cunskip(bufp, count)
char *bufp;
int count;
{
	static char goop[] = "cunbatch";
#	define	GOOPLEN	(sizeof(goop)-1)	/* strlen(goop) */
	static char suf[] = ".Z";
	static char goop7[] = "c7unbatch";
#	define	GOOP7LEN	(sizeof(goop7)-1)	/* strlen(goop7) */
	static char suf7[] = ".7";
	static char comp[] = "\037\235";	/* compress's magic no. */
	register char *p;
	register int nleft;
#	define	MINCBATCH	5		/* one character, compressed */

	nleft = count;
	p = bufp;

	if (nleft < 2)				/* no room for a header */
		return(0);

	if (p[0] == comp[0] && p[1] == comp[1]) {	/* compressed */
		if (nleft < MINCBATCH)
			return(count);
		suffix = suf;
		return(0);
	}

	if (*p++ != '#' || *p++ != '!')		/* doesn't start with #! */
		return(0);
	nleft -= 2;

	/* skip space */
	while (nleft > 0 && (*p == ' ' || *p == '\t')) {
		p++;
		nleft--;
	}

	/* recognize headers (the +1s ensure room for the newline) */
	if (nleft >= GOOPLEN+1 && STREQN(p, goop, GOOPLEN)) {
		p += GOOPLEN;
		nleft -= GOOPLEN;
		suffix = suf;
	} else if (nleft >= GOOP7LEN+1 && STREQN(p, goop7, GOOP7LEN)) {
		p += GOOP7LEN;
		nleft -= GOOP7LEN;
		suffix = suf7;
	} else					/* no header */
		return(0);

	/* skip more space */
	while (nleft > 0 && (*p == ' ' || *p == '\t')) {
		p++;
		nleft--;
	}

	if (nleft == 0 || *p++ != '\n')		/* didn't end properly */
		return(0);

	if (nleft < MINCBATCH)			/* null batch */
		return(count);
	return(p - bufp);
}

/*
 - unprivileged - drop setuidness if configuration is overridden
 */
void
unprivileged(reason)
char *reason;
{
	setgid(getgid());
	setuid(getuid());
	fprintf(stderr, "%s: renouncing setuid due to nonstandard `%s' in environment\n",
							progname, reason);
}
