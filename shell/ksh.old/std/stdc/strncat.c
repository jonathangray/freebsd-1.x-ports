#include <string.h>
/* strncat.c,v 1.1.1.1 1993/05/21 05:37:52 cgd Exp */

/*
 * strncat - append at most n characters of string src to dst
 */
char *				/* dst */
strncat(dst, src, n)
char *dst;
const char *src;
size_t n;
{
	register char *dscan;
	register const char *sscan;
	register size_t count;

	for (dscan = dst; *dscan != '\0'; dscan++)
		continue;
	sscan = src;
	count = n;
	while (*sscan != '\0' && --count >= 0)
		*dscan++ = *sscan++;
	*dscan++ = '\0';
	return(dst);
}
