/*
 * strsave - like strdup, but error if can't allocate
 */

#include <stdio.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"

/*
 * Copy "s" into malloced memory, if any is available.
 * If not, unlock the news system, print a message and exit,
 * else return the address of the malloced memory.
 */
char *
strsave(s)
register char *s;
{
	register char *news = nemalloc((unsigned)strlen(s) + SIZENUL);

	(void) strcpy(news, s);
	return news;
}
