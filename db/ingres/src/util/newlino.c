#include <ingres.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)newlino.c	8.1	12/31/84)

/*
** newlino finds a free line number which it returns and adjusts
**	the next line counter (Nxtlino) by the length of the tuple (len).
**	This routine is used to recover unused sections of the
**	line number table (Acc_head->am_linev).
*/
int
newlino(int len)
{
	register int	newlno, nextlno;
	register short	*lp;

	nextlno = Acc_head->am_nextline;
	lp = &Acc_head->am_linev[0];
	for (newlno = 0; newlno < nextlno; newlno++) {
		if (*lp == 0) {
			/* found a free line number */
			*lp = Acc_head->am_linev[-nextlno];
			Acc_head->am_linev[-nextlno] += len;
			return (newlno);
		}
		lp--;
	}

	/* no free line numbers. use am_nextline */
	Acc_head->am_linev[-(nextlno + 1)] = *lp + len;
	Acc_head->am_nextline++;
	return (nextlno);
}
