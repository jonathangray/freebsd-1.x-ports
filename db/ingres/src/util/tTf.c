#include "sccs.h"

SCCSID(@(#)tTf.c	8.1	12/31/84)

int
tTf(int m, int n)
{
	extern char	tTany;
	extern short	*tT;

	if (!tTany)
		return (0);
	if (n < 0)
		return (tT[m]);
	else
		return ((tT[m] >> n) & 01);
}
