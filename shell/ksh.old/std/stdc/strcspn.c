#include <string.h>
/* strcspn.c,v 1.1.1.1 1993/05/21 05:37:52 cgd Exp */

/*
 * strcspn - find length of initial segment of s consisting entirely
 * of characters not from reject
 */

size_t
strcspn(s, reject)
const char *s;
const char *reject;
{
	register const char *scan;
	register const char *rscan;
	register size_t count;

	count = 0;
	for (scan = s; *scan != '\0'; scan++) {
		for (rscan = reject; *rscan != '\0';)	/* ++ moved down. */
			if (*scan == *rscan++)
				return(count);
		count++;
	}
	return(count);
}
