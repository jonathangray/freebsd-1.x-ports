#ifndef lint
static char *RCSid = "strstr.c,v 1.1.1.1 1993/05/21 05:37:53 cgd Exp";
#endif

#include "stddef.h"

/*
 * strstr - find first occurrence of wanted in s
 */

char *				/* found string, or NULL if none */
strstr(s, wanted)
const char *s;
const char *wanted;
{
	register const char *scan;
	register size_t len;
	register char firstc;

	/*
	 * The odd placement of the two tests is so "" is findable.
	 * Also, we inline the first char for speed.
	 * The ++ on scan has been moved down for optimization.
	 */
	firstc = *wanted;
	len = strlen(wanted);
	for (scan = s; *scan != firstc || strncmp(scan, wanted, len) != 0; )
		if (*scan++ == '\0')
			return(NULL);
	return(scan);
}
