/* $Id: memcmp.c,v 1.1 1994/04/16 21:38:57 sean Exp $ */

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

