/*
 * newslock - simple, unbroken version of ln(1) for shell-program locking
 *
 * (System V has broken ln(1) itself.)
 */
#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
	if (argc != 3) {
		fprintf(stderr, "Usage: %s tempname lockname\n", argv[0]);
		exit(2);
	}

	if (link(argv[1], argv[2]) < 0)
		exit(1);
	else
		exit(0);
	/* NOTREACHED */
}
