/*
 * warning - print best error message possible and clear errno
 */

#include <stdio.h>
#include <errno.h>
#ifndef __STDC__
extern int errno;
#endif
#include <string.h>

void
warning(s1, s2)
char *s1;
char *s2;
{
	char *cmdname;
	register int saverrno = errno;
	extern char *progname;
	extern char *getenv();

	(void) fflush(stdout);				/* hack */
	cmdname = getenv("CMDNAME");
	if (cmdname != NULL && *cmdname != '\0')
		fprintf(stderr, "%s:", cmdname);	/* No space after :. */
	if (progname != NULL)
		fprintf(stderr, "%s: ", progname);
	fprintf(stderr, s1, s2);
	if (saverrno != 0)
		fprintf(stderr, " (%s)", strerror(saverrno));
	fprintf(stderr, "\n");
	(void) fflush(stderr);
	errno = 0;
}
