#include <stdio.h>

#include <ingres.h>
#include <access.h>
#include <catalog.h>
#include <btree.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)insert.c	8.1	12/31/84)

/*
**	INSERT - add a new tuple to a relation
**
**	Insert puts a given tuple into a relation in
**	the "correct" position.
**
**	If insert is called with checkdups == TRUE then
**	the tuple will not be inserted if it is a duplicate
**	of some already existing tuple. If the relation is a
**	heap then checkdups is made false.
**
**	Tid will be set to the tuple id where the
**	tuple is placed.
**
**	returns:
**		<0  fatal error
**		0   success
**		1   tuple was a duplicate
**		2   bad lid
*/
int
insert(desc_t *d, tid_t *tid, void *tuplearg, bool checkdups)
{
	register int	i;
	int		need;
	char		*tp;
	long		lid[MAXLID];
	char		btree[MAX_NAME_SIZE + 4];
	tid_t		tidpos;
	short		nolid;
	char		*tuple;

	tuple = (char *) tuplearg;
#ifdef xATR1
	if (tTf(24, 0)) {
		printf("insert:%.14s,", d->d_r.r_id);
		dumptid(tid);
		printup(d, tuple);
	}
#endif

	if (d->d_r.r_dim != 0)
		checkdups = FALSE;

	/* determine how much space is needed for tuple */
	need = canonical(d, tuple);

	/* find the "best" page to place tuple */
	if ((i = findbest(d, tid, tuple, need, checkdups)) != 0)
		return (i);

	if (d->d_r.r_dim > 0) {
		/* get lids and check for errors */
		btreename(d->d_r.r_id, btree);
		tp = tuple + d->d_r.r_width - LIDSIZE * d->d_r.r_dim;
		bmove(tp, lid, LIDSIZE * d->d_r.r_dim);
		nolid = 0;
		for (i = 0; i < d->d_r.r_dim; ++i) {
			if (lid[i] < 0 || (lid[i] > 0 && nolid))
				return(2);
			nolid = !(lid[i]);
		}
	}

	if (d->d_r.r_dim > 0) {
		if (insert_mbtree(d, btree, lid, (long *) tid, &tidpos) < 0)
			return(2);
		tp = tuple + d->d_r.r_width - LIDSIZE * d->d_r.r_dim;
		bmove(lid, tp, d->d_r.r_dim * LIDSIZE);
	}

	/* put tuple in position "tid" */
	put_tuple(tid, Acctuple, need);

	d->d_addc++;

	return (0);
}
