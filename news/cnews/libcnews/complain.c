#include <stdio.h>

/*
 - complain - lodge a complaint
 */
void
complain(s1, s2)
char *s1;
char *s2;
{
	extern char *progname;

	(void) fprintf(stderr, "%s: ", progname);
	(void) fprintf(stderr, s1, s2);
	(void) putc('\n', stderr);
}
