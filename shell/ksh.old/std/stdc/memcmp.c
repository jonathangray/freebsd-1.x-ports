/* memcmp.c,v 1.1.1.1 1993/05/21 05:37:51 cgd Exp */

#include <string.h>

int
memcmp(dap, sap, n)
	const void *dap;
	const void *sap;
	register size_t n;
{
	register const unsigned char *dp = (unsigned char const *) dap;
	register const unsigned char *sp = (unsigned char const *) sap;

	if (n++ > 0)
		while (--n > 0)
			if (*dp++ != *sp++)
				return *--dp - *--sp; /* (int)? */
	return 0;
}

