/*
 * gngp - globally match newsgroup pattern and print
 *	like grep, but for newsgroup patterns instead of regular expressions
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "fgetfln.h"
#include "ngmatch.h"

/* imports */
extern FILE *efopen();

char *progname;
int debug = 0;

/*
 * if true, match only ng at start of line, followed by whitespace or newline.
 */
static int anchored = 0;
static int reverse = 0;	/* iff true, reverse argument & file roles */
static int exclude = 0;	/* iff true, print lines *not* matched */
static NGPAT *ngpat;

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, status = 0, errflg = 0;
	char *patarg;

	progname = argv[0];
	while ((c = getopt(argc, argv, "adrv")) != EOF)
		switch (c) {
		case 'a':		/* anchored at start of line */
			anchored++;
			break;
		case 'd':
			matchdebug(1);	/* all debugging on */
			debug++;
			break;
		case 'r':	/* reverse roles: ngs in arg., patterns in file */
			reverse++;
			break;
		case 'v':
			exclude++;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg || optind == argc) {
		(void) fprintf(stderr, "usage: %s [-adrv] ng_pattern [file...]\n",
			progname);
		exit(2);
	}
	patarg = argv[optind];
	if (!reverse) {
		ngpat = ngparse(patarg);
		if (ngpat == NULL)
			error("can't parse pattern `%s'", patarg);
	}
	if (optind == argc-1)
		status |= process(patarg, stdin, "stdin");
	else {
		while (++optind < argc) {
			FILE *in = efopen(argv[optind], "r");

			status |= process(patarg, in, argv[optind]);
			(void) fclose(in);
		}
	}
	exit(status != 0? 0: 1);
}

/*
 * process - process input file
 */
process(pattern, in, inname)
register char *pattern;
register FILE *in;
char *inname;
{
	register char *line;
	register int status = 0;

	while ((line = fgetln(in)) != NULL)
		if (anchored)
			status |= gngp(pattern, line);
		else {
			register char *start;

			for (start = line; *start != '\0'; start++)
				status |= gngp(pattern, start);
		}
	return status;
}

int
gngp(pattern, text)
register char *pattern, *text;
{
	register int returned;
	register char *whitesp;
	register char savewhite;

	if (anchored)
		/* strpbrk(text, " \t\n") is too slow; do it long-hand */
		for (whitesp = text; (savewhite = *whitesp) != '\0'; whitesp++)
			if (isascii(savewhite) && isspace(savewhite)) {
				*whitesp = '\0';
				break;
			}

	if (!reverse)
		returned = ngpatmat(ngpat, text);
	else
		returned = ngmatch(text, pattern);
	if (exclude)
		returned = !returned;

	if (anchored)
		*whitesp = savewhite;

	if (returned)
		(void) fputs(text, stdout);
	return returned;
}
