/* $Id: memset.c,v 1.1 1994/04/16 21:39:00 sean Exp $ */

#include <string.h>

void *
memset(ap, c, n)
	void *ap;
	register int c;
	register size_t n;
{
	register char *p = ap;

	if (n++ > 0)
		while (--n > 0)
			*p++ = c;
	return ap;
}

