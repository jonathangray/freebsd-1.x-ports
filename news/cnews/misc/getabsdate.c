/*
 * getabsdate absolute_date ... - convert absolute_date to seconds since epoch
 */

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>

/* privates */
static struct timeb ftnow;
static int exitstatus = 0;

char *progname;

/* imports */
extern long atol();
extern char *malloc();
extern struct tm *gmtime();
extern time_t time();
extern int optind;
extern char *optarg;

extern char *strsave();
extern time_t getabsdate();

/* Forwards. */
extern void process();

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

	while ((c = getopt(argc, argv, "")) != EOF)
		switch (c) {
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind == argc) {
		(void) fprintf(stderr, "Usage: %s ascii_time ...\n", progname);
		exit(2);
	}

	for (; optind < argc; optind++)
		process(argv[optind]);
	exit(exitstatus);
}

/*
 * process - print time_t of tm
 */
void
process(timestr)
char *timestr;
{
	register time_t tstime;
	register char *copy = strsave(timestr);

	if (copy == NULL) {
		exitstatus = 1;
		return;
	}
	tstime = getabsdate(copy, &ftnow);
	if (tstime < 0) {
		(void) fprintf(stderr, "%s: `%s' not a valid date\n",
			       progname, timestr);
		exitstatus = 1;
	} else
		(void) printf("%ld\n", tstime);
	free(copy);
}

unprivileged()			/* strsave requires this; ugh */
{
}
