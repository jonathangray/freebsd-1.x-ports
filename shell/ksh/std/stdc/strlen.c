#include <string.h>
/* $Id: strlen.c,v 1.1 1994/04/16 21:39:01 sean Exp $ */

/*
 * strlen - length of string (not including NUL)
 */
size_t
strlen(s)
const char *s;
{
	register const char *scan;
	register size_t count;

	count = 0;
	scan = s;
	while (*scan++ != '\0')
		count++;
	return(count);
}
