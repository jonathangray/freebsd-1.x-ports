/*
 * actflag group-list - compute overriding active file flag for groups
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include "news.h"
#include "config.h"
#include "active.h"

/* flag field values */
#define FLAGOKAY 'y'		/* ordinary unmoderated group */
#define FLAGBAD 'n'		/* unmoderated but locally-restricted group */
#define FLAGMOD 'm'		/* moderated group */
#define FLAGNEVER 'x'		/* unwanted group: don't file in this one */
#define FLAGGOTO '='		/* see another group (following) instead */

/* imports */
extern int optind;
extern char *optarg;

/* exports */
char *progname = "";
int debug;

/* forwards */
char *actflag();

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;

	if (argc > 0)
		progname = argv[0];
	while ((c = getopt(argc, argv, "d")) != EOF)
		switch (c) {
		case 'd':
			++debug;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg || optind != argc - 1) {
		(void) fprintf(stderr, "usage: %s [-d] grouplist\n", progname);
		exit(2);
	}
	(void) fputs(actflag(argv[optind]), stdout);
	(void) putchar('\n');
	exit(0);
}

/*
 * actflag - compute dominant active file flag for a list of newsgroups
 */
char *
actflag(ngs)
register char *ngs;
{
	register char *comma, *flagp, *status = strsave(""), *nlp;
	char *actent;

	actread();
	/* quit early if we find a moderated group */
	for (; ngs != NULL && *status != FLAGMOD; ngs = comma) {
		comma = strchr(ngs, NGSEP);
		if (comma != NULL)
			*comma = '\0';		/* will be restored below */

		actent = actlook(ngs);
		if (actent != NULL) {		/* group known locally? */
			flagp = findflag(actent);
			switch (*flagp) {
			case FLAGBAD:
			case FLAGNEVER:
			case FLAGMOD:
				switch (*status) {
				case FLAGBAD:
				case FLAGNEVER:
					break;
				default:
					free(status);
					nlp = strchr(flagp, '\n');
					if (nlp != NULL)
						*nlp = '\0';
					status = str3save(flagp, " ", ngs);
					if (nlp != NULL)
						*nlp = '\n';
					break;
				}
				break;
			case FLAGOKAY:
			default:
				switch (*status) {
				case FLAGBAD:
				case FLAGNEVER:
					break;
				default:
					free(status);
					status = strsave("y");
					break;
				}
				break;
			}
		}
		if (comma != NULL)
			*comma++ = NGSEP;	/* step past comma */
	}
	return status;
}
