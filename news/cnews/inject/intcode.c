/*
 * intcode - print an integer compactly but safely
 *	(using RFC 822/1036 safe character set)
 */


#include <stdio.h>
#include <sys/types.h>

/* private data */
#ifdef notdef
/*
 * This alphabet is safe according to RFCs 822 and 1036 and excludes
 * characters likely to be munged by Bitnet ( ^ ` { | } ~ ).
 * Some B Newses fold case, so we can't use both cases.
 */
static char alphabet[] = "!#$%&'*+-=?_0123456789abcdefghijklmnopqrstuvwxyz";
#endif
/*
 * However, we are using this smaller alphabet in the interests of
 * extreme caution, since a message-id is pretty fundamental to netnews
 * and only the alphanumerics seem really safe from munging.
 */
static char alphabet[] = "0123456789ABCDEFGHIJKLMnopqrstuvwxyz";

#define RADIX (sizeof alphabet - 1)

intcode(tm)
time_t tm;
{
	register time_t nextl = tm / RADIX;

	if (nextl > 0)
		intcode(nextl);
	(void) putchar(alphabet[tm % RADIX]);
}
