/*
 * the main program for do(statfs, ustat, ultrix, ...)
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>

long bperi = 1000000000L;	/* how many bytes per inode? */

int debug = 0;
char *progname;

extern void error(), exit();

/*
 - main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	extern int optind;
	extern char *optarg;
	long process();
	extern long atol();
	extern double atof();
	register long n;

	progname = argv[0];

	while ((c = getopt(argc, argv, "e:d")) != EOF)
		switch (c) {
		case 'e':	/* Estimated bytes/inode. */
			bperi = atol(optarg);
			break;
		case 'd':	/* Debugging. */
			debug++;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg || optind != argc-4 || !num(argv[optind]) ||
						!num(argv[optind+2]) ||
						!num(argv[optind+3])) {
		fprintf(stderr, "usage: %s [-e estsize]", progname);
		fprintf(stderr, "filesize fileonfs wantspace wantinodes\n");
		exit(2);
	}

	n = process((long)atof(argv[optind]), argv[optind+1],
			atol(argv[optind+2]), atol(argv[optind+3]), bperi);
	printf("%ld\n", n);
	exit(0);
}

/*
 - num - is a string numeric?
 */
int				/* predicate */
num(s)
char *s;
{
	return(strspn(s, "0123456789.eE+-") == strlen(s));
}
