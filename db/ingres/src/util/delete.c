#include <stdio.h>

#include <ingres.h>
#include <access.h>
#include <catalog.h>
#include <batch.h>
#include <btree.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)delete.c	8.1	12/31/84)

/*
**	Delete - delete the tuple specified by tid
**
**	Delete removes the tuple specified by tid
**	and reclaims the tuple space.
**
**	returns:
**		<0  fatal error
**		0   success
**		2   tuple specified by tid aleady deleted
*/
int
delete(desc_t *dx, tid_t *tidx)
{
	register desc_t	*d;
	register tid_t	*tid;
	register int	i;
	char		btree[MAX_NAME_SIZE + 4];
	tid_t		tidloc;
	long		lid[MAXLID];

	d = dx;
	tid = tidx;

#ifdef xATR1
	if (tTf(24, 8)) {
		printf("delete: %.14s,", d->d_r.r_id);
		dumptid(tid);
	}
#endif

	if ((i = get_page(d, tid)) != 0)
		return (i);

	if ((i = invalid(tid)) != 0)
		return (i);

	i = tup_len(tid);

	del_tuple(tid, i);

	if (d->d_r.r_dim > 0) {
		/* remove corresponding lid from B-Tree */
		btreename(d->d_r.r_id, btree);
		search_btree(*tid, &tidloc);
		get_lid(&tidloc, lid);
		if (fwrite(lid, 1, LIDSIZE * d->d_r.r_dim, Del_infp) != LIDSIZE * d->d_r.r_dim)
			syserr("write error");
		++Del_cnt;
	}

	d->d_addc--;

	return (0);
}
