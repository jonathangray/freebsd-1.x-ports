#include <ingres.h>
#include <access.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)purgetup.c	8.1	12/31/84)

/*
**	Remove tuples from the specified system relation.
**
**	'Desa' is a descriptor for a system relation and
**	key[12] and dom[12] are the keys and domain numbers
**	to match on for the delete.
**	All the tuples in 'desa' with key1 and key2
**	are deleted from the relation.
*/
void
purgetup(register desc_t *d, char *key1, int dom1, char *key2, int dom2)
{
	tid_t		tid, limtid;
	register int	i;
	char		tupkey[MAX_TUP_SIZE], tuple[MAX_TUP_SIZE];

	ingres_setkey(d, tupkey, key1, dom1);
	ingres_setkey(d, tupkey, key2, dom2);
	if ((i = find(d, EXACTKEY, &tid, &limtid, tupkey)) != 0)
		syserr("purgetup:find:%d", i);
	while ((i = get(d, &tid, &limtid, tuple, TRUE)) == 0) {
		if (kcompare(d, tuple, tupkey) == 0)
			if ((i = delete(d, &tid)) != 0)
				syserr("attflush: delete %d", i);
	}

	if (i < 0)
		syserr("purgetup:get %.14s:%d", d->d_r.r_id, i);
}
