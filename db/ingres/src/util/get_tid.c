#include <stdio.h>

#include <btree.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)get_tid.c	8.3	1/31/86)

/*	GET_TID -- BTree retrieval routine
**
**	Retrieve the tid corresponding to the given lid
**
**	Parameters :
**		tree - BTree filename (I)
**		lid - given lid (I)
**		tid_id - storage information of location of the tid (O)
**
**	Returns :
**		> 0 	tid
**		-1 	lid corresponds to very last lid in tree
**		-2   	lid does not exist
*/

long
get_tid(long rootpage, long lid, locator_t *tid_id)
{
	long		pgno, sum, k, t;
	register int	i;
	bt_node_t	p;

#ifdef xATR1
	if (tTf(24, 0))
		printf("getting btree tid for lid %ld in rootpage %d\n", lid, rootpage);
#endif

	
	if (lid <= 0)
		return(-2);	/* negative lids nonexistent */

	get_node(rootpage, &p);
	pgno = rootpage;
	sum = 0;
	t = 0;

	/* find the leaf node containing desired tid */
	while (p.nodetype != 'L') {
		if (t == -1) {
			/* continue along path to very last lid */
			i = p.nelmts - 1;
		} else {
			/* find pointer in node which will lead down to proper leaf */
			for (i = 0, k = p.node.intnode.key[0]; sum + k < lid && i < p.nelmts; ) {
				sum += k;
				if (++i < p.nelmts) {
					k = p.node.intnode.key[i];
				}
			}
	
			if (i >= p.nelmts) {
				if (sum < lid - 1) {
					return(-2);	/* lid doesn't exist */
				} else {
					--i;
					t = -1;
				}
			}
		}

		pgno = p.node.intnode.ptr[i];
		get_node(pgno, &p);
	}

	if (t == -1)
		/* new lid is to be inserted at very last leaf position */
		tid_id->offset = p.nelmts;
	else {
		/* search through the leaf for the proper tid */
		for (i = 0, ++sum, t = p.node.leafnode.tid_pos[p.node.leafnode.tid_loc[0]]; sum < lid && i < p.nelmts; ) {
			++sum;
			++i;
			if (i < p.nelmts)
				t = p.node.leafnode.tid_pos[p.node.leafnode.tid_loc[i]];
		}
		if (i >= p.nelmts) {
			if (sum < lid)
				return(-2);	/* lid doesn't exist */
			else
				t = -1;
		}
		tid_id->offset = i;
	}

	bmove(&p, &tid_id->page, sizeof(p));
	tid_id->pageno = pgno;

#ifdef xATR1
	if (tTf(24, 0)) {
		printf("Main relation tid found in tree:");
		dumptid((tid_t *) &t);
		printf("Btree tid location:");
		dumptid((tid_t *) tid_id);
	}
#endif

	return(t);
}
