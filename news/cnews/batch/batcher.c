/*
 * batcher - send a bunch of news articles as an unbatch script
 *
 * Usage: batcher listfile
 *
 *	where listfile is a file containing a list, one per line, of
 *	names of files containing articles.  Only the first
 *	field of each line is looked at, so there can be more if needed
 *	for other things.  Non-absolute pathnames are understood to lie
 *	under the current directory; chdiring to the right place is the
 *	parent's problem.
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fgetmfs.h"

#ifndef READSIZE
#define READSIZE 8192	/* allows for even 4.2 worst case file systems */
#endif
char buffer[READSIZE];

char *progname;

int debug = 0;			/* Debugging? */

main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	extern int optind;
	extern char *optarg;
	register FILE *list;
	char *article;
	int ret;

	progname = argv[0];
	while ((c = getopt(argc, argv, "x")) != EOF)
		switch (c) {
		case 'x':	/* Debugging. */
			debug++;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind != argc-1) {
		(void) fprintf(stderr,
			"Usage: batcher listfile\n");
		exit(2);
	}

	list = fopen(argv[optind], "r");
	if (list == NULL)
		error("unable to open `%s'", argv[optind]);

	while ((article = fgetms(list)) != NULL) {
		process(article);
		free(article);
	}
	if (!feof(list))
		error("fgetms failure (read error or out of memory) in `%s'",
								argv[optind]);

	exit(0);
}

/*
 - process - process an article
 */
process(article)
register char *article;
{
	register int artfile;
	register int count;
	struct stat sbuf;
	register char *endp;

	endp = strchr(article, '\t');
	if (endp == NULL)
		endp = strchr(article, ' ');
	if (endp == NULL)
		endp = strchr(article, '\n');
	if (endp != NULL)
		*endp = '\0';

	artfile = open(article, 0);
	if (artfile < 0) {
		/*
		 * Can't read the article.  This isn't necessarily a
		 * disaster, since things like cancellations will do
		 * this.  Mumble and carry on.
		 */
		if (debug)
			warning("can't find `%s'", article);
		return;
	}

	if (fstat(artfile, &sbuf) < 0)
		error("internal disaster, can't fstat", "");

	printf("#! rnews %ld\n", sbuf.st_size);
	while ((count = read(artfile, buffer, sizeof buffer)) > 0)
		if (fwrite(buffer, sizeof(char), count, stdout) != count)
			error("write failure in `%s'", article);
	if (count < 0)
		error("read failure in `%s'", article);

	(void) close(artfile);
}
