#include "sccs.h"
#include <useful.h>

#include "protos.h"

SCCSID(@(#)ztack.c	8.2	2/8/85)

/*
**  LOCAL STRING CONCATENATE
**	Strings `a' and `b' are concatenated and left in an
**	internal buffer.  A pointer to that buffer is returned.
**
**	Ztack can be called recursively as:
**		ztack(ztack(ztack(w, x), y), z);
*/

char *
ztack(register char *a, register char *b)
{
	register char	*c;
	static char	buf[CONCAT_BUFSZ];
	
	c = buf;
	
	while (*a) {
		*c++ = *a++;
	}
	while (*b) {
		*c++ = *b++;
	}
	*c = '\0';
	if (buf[CONCAT_BUFSZ - 1] != 0) {
		syserr("ztack overflow: %s", buf);
	}
	return (buf);
}
