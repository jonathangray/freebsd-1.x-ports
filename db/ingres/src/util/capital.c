#include <ctype.h>

#include "sccs.h"

SCCSID(@(#)capital.c	8.1	12/31/84)

void
capital(char *lower, char *upper)
{
	register char	*l, *u;

	l = lower;
	u = upper;
	while (*l) {
		if (*l == ' ') {
			do {
				*u++ = *l++;
			} while (*l);
			break;
		}
		if (islower(*l)) {
			*u++ = toupper(*l);
			l++;
		} else {
			*u++ = *l++;
		}
	}
	*u = '\0';
}
