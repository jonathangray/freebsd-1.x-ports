/*
 * make a string all lower-case.
 */

#include <ctype.h>

strlower(s)
register char *s;
{
	for (; *s != '\0'; ++s)
		if (isascii(*s) && isupper(*s))
			*s = tolower(*s);
}
