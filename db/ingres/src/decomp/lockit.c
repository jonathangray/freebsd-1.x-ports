#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include "globs.h"
#include <lock.h>
#include "sccs.h"

#define INGRES_IUTIL
#include "protos.h"

SCCSID(@(#)lockit.c	8.1	12/31/84)

/*
**  LOCKIT -- sets relation locks for integrity locking
**
**	Parameters:
**		root- the root of a query tree;
**		resvar- index of variable to be updated.
**
*/
void
lockit(qtree_t *root, int resvar)
{
	register qtree_t	*r;
	register int	i, j;
	tid_t		vlist[MAX_RANGES];
	int		bmap, cv;
	char		mode;
	int		skvr;
	int		redo;
	desc_t		*d;
	long		lvar;
	tid_t		restid;
	int		k;

	r = root;
	(void) memset(&restid, 0, sizeof(restid));
	bmap = r->sym.value.sym_root.lvarm | r->sym.value.sym_root.rvarm;
	if (resvar >= 0) {
		bmap |= (01 << resvar);
	} else {
		lvar = -1;
		(void) memcpy(&restid, &lvar, sizeof(restid));
	}
	i = 0;
	/* put relids of relations to be locked into vlist
	   check for and remove duplicates */
	for (j = 0; j < MAX_RANGES; j++) {
		if (bmap & (01 << j)) {
			d = openr1(j);
			if (j == resvar) {
				(void) memcpy(&restid, &d->d_tid,
							sizeof(d->d_tid));
			}
			for (k = 0; k < i; k++) {
				if (memcmp(&vlist[k], &d->d_tid,
						sizeof(d->d_tid)) == 0) {
					break;
				}
			}
			if (k == i) {
				(void) memcpy(&vlist[i++], &d->d_tid,
							sizeof(d->d_tid));
			}
		}
	}
	cv = i;
/*
 *	set the locks: set the first lock with the sleep option
 *			set other locks checking for failure;
 *			if failure, release all locks, sleep on blocking
 *			lock.
 */
	skvr = -1;
	do {
		/* skvr is the index of the relation already locked
		   try to lock the remaining relations */
		redo = FALSE;
		for (i = 0; i < cv; i++) {
			if (i != skvr) {
				mode = (memcmp(&restid, &vlist[i],
						sizeof(restid)) == 0) ?
						M_EXCL : M_SHARE;
				if (setrll(A_RTN, &vlist[i], mode) < 0) {
					/* a lock request failed */
					/* release all locks */
					unlall();
					/* wait on problem lock*/
					setrll(A_SLP, &vlist[i], mode);
					skvr = i;
					redo = TRUE;
					/* reset the other locks */
					break;
				}
			}
		}
	} while (redo);
}
