/*
 * getdate ascii_time ... - print the time_t of ascii_time(s)
 */

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>

#define	DAY	(24L*60L*60L)

struct timeb ftnow;
int exitstatus = 0;

char *progname;

extern long atol();
extern char *malloc();
extern struct tm *gmtime();
extern time_t time();

extern time_t getdate();

/* Forwards. */
extern void process();

/*
 - main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	register int c;
	register int errflg = 0;
	extern int optind;
	extern char *optarg;

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
process(tm)
char *tm;
{
	time_t it;

	if (strcmp(tm, "now") == 0)
		it = time((time_t *)NULL);
	else
		it = getdate(tm, &ftnow);
	if (it < 0) {
		(void) fprintf(stderr, "%s: `%s' not a valid date\n", progname, tm);
		exitstatus = 1;
	} else
		(void) printf("%ld\n", it);
}
