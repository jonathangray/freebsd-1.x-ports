#include <stdio.h>

#include "endian.h"

#include <ingres.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)utility.c	8.2	12/8/85)

int
dumptid(tid_t *tid)
{
	long	pageid;

	pluck_page(tid, &pageid);
	printf("tid: %ld/%d\n", pageid, (tid->line_id & I1MASK));
	return (0);
}

/*
**	struct for extracting page number from a tid
**	and storing in a long
**
**	We want the line number (lpgx) to be in the low-order part of
**	a long.  Since SUN's and VAXes have the order of the
**	words reversed, this structure must be different.
*/

struct lpage {
#ifdef LITTLE_ENDIAN
	char	lpg2, lpg1, lpg0, lpgx;
#else
	char	lpgx, lpg0, lpg1, lpg2;
#endif
};

/*  PLUCK_PAGE
**
**	pluck_page extracts the three byte page_id from a tid_t
**	and puts it into a long variable with proper allignment.
*/
int
pluck_page(tid_t *t, long *var)
{
	register struct lpage	*v;

	v = (struct lpage *) var;
	v->lpg0 = t->pg0;
	v->lpg1 = t->pg1;
	v->lpg2 = t->pg2;
	v->lpgx = 0;
	return (0);
}

/*	stuff_page is the reverse of pluck_page	*/
int
stuff_page(tid_t *t, long *var)
{
	register struct lpage	*v;

	v = (struct lpage *) var;
	t->pg0 = v->lpg0;
	t->pg1 = v->lpg1;
	t->pg2 = v->lpg2;
	return (0);
}
