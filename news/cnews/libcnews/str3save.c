#include <stdio.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"

/*
 - str3save - malloc space for 3 strings, concatenated, and concatenate them
 * This may seem kind of ad-hoc, but it's just right for filename work.
 */
char *
str3save(s1, s2, s3)
char *s1;
char *s2;
char *s3;
{
	register char *p;
	static char *empty = "";

	if (s1 == NULL)
		s1 = empty;
	if (s2 == NULL)
		s2 = empty;
	if (s3 == NULL)
		s3 = empty;

	p = nemalloc((unsigned)(strlen(s1) + strlen(s2) + strlen(s3) + 1));
	(void) strcpy(p, s1);
	(void) strcat(p, s2);
	(void) strcat(p, s3);
	return(p);
}
