#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <useful.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)sequal.c	8.2	2/5/85)

/*
**  SEQUAL -- string equality test
**
**	null-terminated strings `a' and `b' are tested for
**		absolute equality.
**	returns one if equal, zero otherwise.
*/
int
sequal(register char *a, register char *b)
{
	return(strcmp(a, b) == 0);
}
