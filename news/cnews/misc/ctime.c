/*
 * ctime time_t ... - print the ascii time of time_t(s)
 */

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>

/* privates */
static struct timeb ftnow;
static int gmt = 0;

char *progname;

/* imports */
extern long atol();
extern char *malloc(), *ctime(), *asctime();
extern struct tm *gmtime();
extern time_t time();

/* Forwards. */
extern void process();
extern int optind;
extern char *optarg;

/*
 - main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	register int c, errflg = 0;

	progname = argv[0];
	ftime(&ftnow);

	while ((c = getopt(argc, argv, "u")) != EOF)
		switch (c) {
		case 'u':
			++gmt;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind == argc) {
		(void) fprintf(stderr, "Usage: %s [-u] ascii_time ...\n",
			       progname);
		exit(2);
	}

	for (; optind < argc; optind++)
		process(argv[optind]);
	exit(0);
}

/*
 * process - print time_t of tm
 */
void
process(tms)
char *tms;
{
	time_t tm;

	if (strcmp(tms, "now") == 0)
		tm = time(&tm);
	else
		tm = atol(tms);
	if (gmt) {
		register struct tm *prstime = gmtime(&tm);

		(void) fputs(asctime(prstime), stdout);
	} else
		(void) fputs(ctime(&tm), stdout);
}
