#include "sccs.h"

#include "protos.h"

SCCSID(@(#)pad.c	8.1	12/31/84)

/*
**  PAD STRING OUT WITH BLANKS
**
**	This routine is an in-place pmove which always pads
**	with blanks.
*/

void
pad(char *s, int n)
{
	register char	*ss;

	ss = s;
	pmove(ss, ss, n, ' ');
}
