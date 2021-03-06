/* memset.c,v 1.1.1.1 1993/05/21 05:37:53 cgd Exp */

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

