/*
 * canonhdr - canonicalise RFC 1036 header
 *	always capitalise header keywords
 *	optionally canonicalise dates in Date: and Expires: headers
 *	optionally convert 822 headers to 1036 headers
 */

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include "news.h"
#include "libc.h"

/* imports */
extern int optind;
extern char *optarg;
extern FILE *efopen();
extern char *strsave(), *str3save();

/* exports */
char *progname;
int debug;

/* privates */
static int convdates = 0;
static int convhdrs = 0;

/* forwards */
char *mailtonews(), *canondate();

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;

	progname = argv[0];
	while ((c = getopt(argc, argv, "dm")) != EOF)
		switch (c) {
		case 'd':
			++convdates;
			break;
		case 'm':
			++convhdrs;
			sethdrrfc(822);
			break;
		default:
			errflg++;
			break;
		}
	if (errflg) {
		(void) fprintf(stderr, "usage: %s [-dm] [file]...\n", progname);
		exit(2);
	}

	if (optind >= argc)
		process(stdin, "stdin");
	else
		for (; optind < argc; optind++)
			if (STREQ(argv[optind], "-"))
				process(stdin, "-");
			else {
				FILE *in = efopen(argv[optind], "r");

				process(in, argv[optind]);
				(void) fclose(in);
			}
	exit(0);
}

/*
 * process - process input file
 */
process(in, inname)
FILE *in;
char *inname;
{
	register char *hdr, *nhdr;
	int ishdr = YES;
	long nolimit = -1;
	static int washdr = YES;
	static int dateseen = NO;
	static char datenm[] =    "Date: ";
	static char expiresnm[] = "Expires: ";

	if (!washdr)
		return;
	while ((hdr = gethdr(in, &nolimit, &ishdr)) != NULL && ishdr) {
		register char *cp;
		static char canonmsgid[] = "Message-Id:";
		static char magicmsgid[] = "Message-ID:";

		/* capitalise first letter of each word, Message-ID: special */
		for (cp = hdr; *cp != ':' && *cp != '\0'; cp++)
			if (cp == hdr || cp[-1] == '-') {
				if (isascii(*cp) && islower(*cp))
					*cp = toupper(*cp);
			} else
				if (isascii(*cp) && isupper(*cp))
					*cp = tolower(*cp);
		if (STREQN(hdr, canonmsgid, STRLEN(canonmsgid)))
			(void) strncpy(hdr, magicmsgid, STRLEN(magicmsgid));

		/* optionally convert 822 headers to 1036 headers */
		if (convhdrs)
			nhdr = mailtonews(hdr);
		else
			nhdr = hdr;

		/* optionally convert dates */
		if (convdates && STREQN(nhdr, datenm, STRLEN(datenm))) {
			dateseen = YES;
			(void) fputs(datenm, stdout);
			(void) fputs(canondate(nhdr, nhdr+STRLEN(datenm), 1),
				stdout);
		} else if (convdates &&
		    STREQN(nhdr, expiresnm, STRLEN(expiresnm)) &&
		    nonnull(nhdr+STRLEN(expiresnm))) {
			(void) fputs(expiresnm, stdout);
			(void) fputs(canondate(nhdr, nhdr+STRLEN(expiresnm), 0),
				stdout);
		} else
			(void) fputs(nhdr, stdout);
		/* must not free hdr; gethdr will do so automatically */
		if (convhdrs)
			free(nhdr);
	}
	if (hdr != NULL)
		free(hdr);
	if (!ishdr)
		washdr = NO;
	if (convdates && !dateseen) {
		(void) fputs(datenm, stdout);
		(void) fputs(canondate("now", "now", 0), stdout);
	}
}

int
nonnull(s)
char *s;
{
	register char *nwp = skipsp(s);

	return *nwp != '\n' && *nwp != '\0';
}

char *						/* malloced */
mailtonews(hdr)
char *hdr;
{
	register char *p;

	p = strchr(hdr, ':');
	if (p == NULL)
		return strsave(hdr);
	p++;					/* point just past colon */
	if (*p == '\t')
		*p = ' ';
	if (*p == ' ')
		return strsave(hdr);
	else {
		/* a spaceless colon; this means war! */
		register char *nhdr = emalloc(strlen(hdr) + 1 + 1);
		register int keyp1len = p - hdr;	/* keyword & colon */

		(void) memcpy(nhdr, hdr, keyp1len);	/* keyword & colon */
		nhdr[keyp1len] = ' ';
		(void) strcpy(nhdr + keyp1len + 1, hdr + keyp1len);
		return nhdr;
	}
}

#define HIGH(nn) ((nn) / 10)
#define LOW(nn)  ((nn) % 10)

char *
canondate(hdr, vulgdate, abs)
char *hdr, *vulgdate;
int abs;					/* flag: absolute date? */
{
	register struct tm *tm;
	time_t date;
	char *copydate;
	static char chtime[128];
	static char *days[] =
		{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
	static char *months[] = {
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
	};

	copydate = strsave(vulgdate);
	if (abs)
		date = getabsdate(copydate, (struct timeb *)NULL);
	else					/* TODO: use getreldate here */
		date = getdate(copydate, (struct timeb *)NULL);
	free(copydate);
	if (date < 0) {
		(void) fprintf(stderr, "%s: bad date in header: ", progname);
		(void) fputs(hdr, stderr);
		exit(1);
	}
	tm = gmtime(&date);
	(void) sprintf(chtime, "%s, %d %s %d %d%d:%d%d:%d%d GMT\n",
		days[tm->tm_wday],
		tm->tm_mday, months[tm->tm_mon], tm->tm_year + 1900,
		HIGH(tm->tm_hour), LOW(tm->tm_hour),
		HIGH(tm->tm_min),  LOW(tm->tm_min),
		HIGH(tm->tm_sec),  LOW(tm->tm_sec));
	return chtime;
}
