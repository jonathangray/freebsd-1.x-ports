#include <string.h>
/* strncpy.c,v 1.1.1.1 1993/05/21 05:37:52 cgd Exp */

/*
 * strncpy - copy at most n characters of string src to dst
 */
char *				/* dst */
strncpy(dst, src, n)
char *dst;
const char *src;
size_t n;
{
	register char *dscan;
	register const char *sscan;
	register size_t count;

	dscan = dst;
	sscan = src;
	count = n;
	while (--count >= 0 && (*dscan++ = *sscan++) != '\0')
		continue;
	while (--count >= 0)
		*dscan++ = '\0';
	return(dst);
}
