#include "sccs.h"

SCCSID(@(#)concat.c	8.1	12/31/84)

/*
**  STRING CONCATENATE
**
**	The strings `s1' and `s2' are concatenated and stored into
**	`s3'.  It is ok for `s1' to equal `s3', but terrible things
**	will happen if `s2' equals `s3'.  The return value is is a
**	pointer to the end of `s3' field.
*/

char *
concat(char *s1, char *s2, char *s3)
{
	register char		*p;
	register char		*q;

	p = s3;
	q = s1;
	while (*q)
		*p++ = *q++;
	q = s2;
	while (*q)
		*p++ = *q++;
	*p = 0;
	return (p);
}
