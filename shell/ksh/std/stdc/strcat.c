#include <string.h>
/* $Id: strcat.c,v 1.1 1994/04/16 21:39:00 sean Exp $ */

/*
 * strcat - append string src to dst
 */
char *				/* dst */
strcat(dst, src)
char *dst;
const char *src;
{
	register char *dscan;
	register const char *sscan;

	for (dscan = dst; *dscan != '\0'; dscan++)
		continue;
	sscan = src;
	while ((*dscan++ = *sscan++) != '\0')
		continue;
	return(dst);
}
