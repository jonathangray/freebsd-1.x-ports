#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <useful.h>
#include "sccs.h"

SCCSID(@(#)bequal.c	8.2	2/5/85)

/*
**  BLOCK EQUALITY TEST
**
**	blocks `a' and `b', both of length `l', are tested
**		for absolute equality.
**	Returns one for equal, zero otherwise.
*/
int
bequal(void *a, void *b, register int l)
{
	char	*cp1;
	char	*cp2;

	if ((cp1 = (char *) a) == (char *) NULL ||
	    (cp2 = (char *) b) == (char *) NULL) {
		return(FALSE);
	}
	while (l-- > 0) {
		if (*cp1++ != *cp2++) {
			return(FALSE);
		}
	}
	return(TRUE);
}
