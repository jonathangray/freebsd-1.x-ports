#include "sccs.h"

SCCSID(@(#)clrmem.c	8.1	12/31/84)

/*
**  CLRMEM -- clear a block of memory
**
**	A block of memory is set to zero.  If we use assembly
**	language, this can be potentially very fast.
**
**	Parameters:
**		p -- a pointer to the area to be cleared.
**		l -- the length of the area in bytes.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		none
*/

void
clrmem(void *p, register int l)
{
	register char	*cp;

	for (cp = (char *) p ; l-- > 0 ; ) {
		*cp++ = 0;
	}
}
