#include <string.h>
/* strcpy.c,v 1.1.1.1 1993/05/21 05:37:53 cgd Exp */

/*
 * strcpy - copy string src to dst
 */
char *				/* dst */
strcpy(dst, src)
char *dst;
const char *src;
{
	register char *dscan;
	register const char *sscan;

	dscan = dst;
	sscan = src;
	while ((*dscan++ = *sscan++) != '\0')
		continue;
	return(dst);
}
