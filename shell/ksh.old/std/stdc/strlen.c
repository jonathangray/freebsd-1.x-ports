#include <string.h>
/* strlen.c,v 1.1.1.1 1993/05/21 05:37:53 cgd Exp */

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
