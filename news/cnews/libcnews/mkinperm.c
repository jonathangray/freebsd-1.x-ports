/*
 - mkinperm - move in.coming temporary to a unique permanent name
 *
 * Names are based on the current time in hopes of keeping input in order.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#ifndef EEXIST
#define	EEXIST	0
#endif
#include "libc.h"
#include "news.h"
#include "config.h"
/* #include "mkinperm.h" */

#ifndef MAXTRIES
#define	MAXTRIES	100	/* limit on attempts to make links */
#endif

#define JAN1991 (time_t)662706000

int mkinpdebug = 0;
char *progname;

extern void exit();
extern time_t time();

int						/* true iff succeeded */
mkinperm(tmpname, grade, class)
char *tmpname;
char *grade;		/* 3 chars = digit, period, NUL */
char *class;		/* suffix for filename, default is plain text */
{
	register char *p;
	register char *name;
	register int ntries;
	register time_t now;
	register int uniq = '0';

	p = fullartfile("in.coming/");
	name = nemalloc(strlen(p) + 20);	/* plenty for a number */
	(void) strcpy(name, p);
	p = name + strlen(name);

	now = time((time_t *)NULL) - JAN1991;	/* reduce range */
	for (ntries = 0; ; ntries++) {
		(void) sprintf(p, "%s%ld%c%s", grade, now, uniq, class);
		if (mkinpdebug)
			(void) fprintf(stderr, "trying renaming to %s\n", name);
		if (link(tmpname, name) >= 0)
			break;		/* NOTE BREAK OUT */
		if (errno != EEXIST)	/* something strange is wrong */
			return NO;
		errno = 0;
		if (ntries > MAXTRIES)	/* sanity check */
			return NO;

		if (++uniq == '9'+1)
			uniq = 'a';
		else if (uniq == 'z'+1)
			uniq = 'A';
		else if (uniq == 'Z'+1) {
			(void) sleep(2);
			now = time((time_t *)NULL) - JAN1991; /* reduce range */
			uniq = '0';
		}
		if (mkinpdebug)
			(void) fprintf(stderr, "link to %s failed\n", name);
	}

	if (mkinpdebug)
		(void) fprintf(stderr, "succeeded\n");
	(void) unlink(tmpname);
	return YES;
}
