/* memxxx.c - memxxx routines for machines that don't have them */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_memxxx_sccsid[] = "@(#)memxxx.c	1.6 26/4/92 (UKC)";

#include <stdlib.h>

#include "ifdefs.h"

#ifdef ARCH_CLIPPER
#define NO_MEMXXX
#endif

#ifdef NO_MEMXXX
char *
memcpy(dst, src, nbytes)
register char *dst;
register const char *src;
int nbytes;
{
	char *res;

	res = dst;
	while (--nbytes >= 0)
		*dst++ = *src++;
	return res;
}

int
memcmp(m1, m2, nbytes)
register char *m1, *m2;
int nbytes;
{
	for (; --nbytes >= 0; ++m1, ++m2)
		if (*m1 != *m2)
			return m1 - m2;
	return 0;
}

char *
memset(m, c, nbytes)
register char *m;
int c, nbytes;
{
	char *res;

	res = m;
	while (--nbytes >= 0)
		*m++ = c;
	return res;
}

char *
memchr(m, c, nbytes)
register char *m;
int c, nbytes;
{
	register char *lim;

	for (lim = m + nbytes; m < lim; ++m)
		if (*m == c)
			return m;
	return NULL;
}
#endif
