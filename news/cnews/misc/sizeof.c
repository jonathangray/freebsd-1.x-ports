/*
 * sizeof - report total size of files
 *
 * You might ask, why couldn't this be a shell program invoking ls -l?
 * Well, apart from the variations in the format of ls -l, there's also
 * the problem that the Berkloid ls -l doesn't follow symlinks unless
 * asked to (with an unportable option).
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

int debug = 0;
int indiv = 0;
char *progname;

extern void error(), exit();

/*
 - main - do it all
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	struct stat statbuf;
	extern int optind;
	extern char *optarg;
	register off_t total = 0;

	progname = argv[0];

	while ((c = getopt(argc, argv, "ix")) != EOF)
		switch (c) {
		case 'i':	/* Individual files. */
			indiv = 1;
			break;
		case 'x':	/* Debugging. */
			debug++;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind >= argc) {
		fprintf(stderr, "usage: %s ", progname);
		fprintf(stderr, "file ...\n");
		exit(2);
	}

	for (; optind < argc; optind++)
		if (stat(argv[optind], &statbuf) >= 0) {
			total += statbuf.st_size;
			if (debug || indiv)
				printf("%s %ld\n", argv[optind],
						(long)statbuf.st_size);
		}
	if (!indiv)
		printf("%ld\n", (long)total);
	exit(0);
}
