#include <ingres.h>
#include <catalog.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)get_p_tid.c	8.2	1/15/85)

/*
**  GET_P_TID -- Get the primary tid for a relation for locking
**
**	Finds the correct tid for locking the relation. If the
**	relation is a primary relation, then the tid of the
**	relation is returned.
**
**	If the relation is a secondary index then the tid of the
**	primary relation is returned.
**
**	Parameters:
**		des - an open descriptor for the relation.
**		tidp - a pointer to a place to store the tid.
**
**	Returns:
**		none
**
**	Side Effects:
**		alters the value stored in "tidp",
**		may cause access to the indices relation
**
**	Called By:
**		modify
*/
void
get_p_tid(register desc_t *d, register tid_t *tp)
{
	register int	i;
	index_t	indkey, itup;
	desc_t		ides;
	extern desc_t	Inddes;

	if (d->d_r.r_indexed < 0) {
		/* this is a secondary index. lock the primary rel */
		opencatalog("indices", OR_READ);
		ingres_setkey(&Inddes, &indkey, d->d_r.r_owner, IOWNERP);
		ingres_setkey(&Inddes, &indkey, d->d_r.r_id, IRELIDI);
		if (getequal(&Inddes, &indkey, &itup, tp))
			syserr("No prim for %.14s", d->d_r.r_id);

		if ((i = openr(&ides, OR_RELTID, itup.i_relname)) != 0)
			syserr("openr prim %d,%.14s", i, itup.i_relname);

		bmove(&ides.d_tid, tp, sizeof(*tp));
	} else {
		bmove(&d->d_tid, tp, sizeof(*tp));
	}
}
