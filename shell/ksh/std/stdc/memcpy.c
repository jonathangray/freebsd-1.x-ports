/* $Id: memcpy.c,v 1.1 1994/04/16 21:39:00 sean Exp $ */

#include <string.h>

void *
memcpy(dap, sap, n)
	void *dap;
	const void *sap;
	register size_t n;
{
	register char *dp = dap, *sp = (void*) sap;

	if (n++ > 0)
		while (--n > 0)
			*dp++ = *sp++;
	return dap;
}

