#include "sccs.h"

SCCSID(@(#)bitpos.c	8.1	12/31/84)

/*
**  FIND HIGH ORDER BIT POSITION
**
**	The position of the highest ordered one bit in `wd' is
**	found and returned.  Bits are numbered 0 -> 15, from
**	right (low-order) to left (high-order) in word.
*/

int
bitpos(register int wd)
{
	register int	i, j;
	register int	pos;

	pos = -1;

	for (i = 1, j = 0; wd; i <<= 1, j++) {
		if (wd & i) {
			pos = j;
			wd &= ~i;
		}
	}

	return (pos);
}
