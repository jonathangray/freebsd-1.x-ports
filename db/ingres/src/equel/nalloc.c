#include "sccs.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)nalloc.c	8.1	12/31/84)

/*
**	NALLOC --
**		Dynamic allocation routine which
** 		merely calls alloc(III), returning 
**		0 if no core and a pointer otherwise.
**
*/

char *
nalloc(int s)
{
	return((char *) xalloc(s, 0, 0));
}

/*
**	SALLOC -- allocate
**		place for string and initialize it,
**		return string or 0 if no core.
**
*/
char *
salloc(char *s)
{
	register char	*cp;

	if ((cp = xalloc(strlen(s) + 1, 0, 0)) != (char *) NULL) {
		smove(s, cp);
	}
	return((char *)cp);
}
