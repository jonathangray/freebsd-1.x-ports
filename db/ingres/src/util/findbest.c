#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)findbest.c	8.1	12/31/84)

/*
**	FINDBEST -- find best page for tuple
**
**	Find an appropriate page to put a tuple.
**	If HEAP then any page with room will do. If none
**	can be found, then use the last page.
**	If it is a user relation and a page was found but
**	was full, use it anyway. This can happen only on a
**	modify (which has checkdups turned off).
**
**	For ISAM or HASH look for a page on the same mainpage
**	chain. Duplicate checking must not be enforced.
**
**	The first page to use will be returned in tid in either
**	case.
*/
void
find_page(desc_t *dx, tid_t *tid, int need)
{
	register desc_t		*d;
	register accbuf_t	*b, *maxbf;
	int			heap;
	long			mainpg;

	d = dx;
	maxbf = NULL;
	heap = (M_TYPEOF(d->d_r.r_spec) == M_HEAP);
	pluck_page(tid, &mainpg);
	mainpg++; /* mainpage in buffer points to next higher mainpage */

	/* scan all current bufs looking for one belonging to this relation */
	for (b = Acc_head; b != (accbuf_t *) NULL; b = b->am_next) {
		if (memcmp(&d->d_tid, &b->am_rel, sizeof(d->d_tid)) == 0 &&
		    !(b->am_flags & BUF_DIRECT) &&
		    (heap || (b->am_mainpg == mainpg))) {
			if (space_left(b) >= need) {
				/* use this page */
				stuff_page(tid, &b->am_curpg);
				return;
			}

			/* save buffer of largest page */
			if (maxbf == NULL || maxbf->am_curpg < b->am_curpg)
				maxbf = b;
		}
	}

	if (heap) {
		last_page(d, tid, maxbf);
	} else {
		/* if we found a full page of a user's relation,use it */
		if (maxbf && (d->d_r.r_status & S_CATALOG) == 0)
			stuff_page(tid, &maxbf->am_curpg);
	}
}

/*
**	Findbest - find the "best" place to put a tuple.
**	Findbest does not actually put the tuple but rather
**	returns and allocates the tid for the tuple.
**
**	The initial part of the algorithm depends on whether
**	the relation is a heap or not.
**
**	If the relation is a heap, if there is a current page
**	with room for the tuple, that page is used. Otherwise
**	the last page of the heap is considered.
**
**	If the relation is hash or isam, then "find" is used
**	to determine the primary page for the tuple.
**
**	If necessary, findbest will allocate an overflow page
**	if there is not sufficient room for the tuple otherwise.
**
**	If checkdups is TRUE and the relation is not a heap,
**	findbest will check for duplicates.
**
**	Returns:
**
**		0 tuple not a duplicate, tid allocated
**		1 tuple a duplicate of the tuple at tid
*/
int
findbest(desc_t *dx, tid_t *tidx, char *tuple, int need, bool checkdups)
{
	register desc_t	*d;
	register tid_t	*tid;
	register int	i;
	tid_t		temptid;

	d = dx;
	tid = tidx;


	if (M_TYPEOF(d->d_r.r_spec) == M_HEAP) {
		checkdups = FALSE;
		/* determine a page to place tuple in heap relation */
		find_page(d, tid, need);

	} else {
		/* find a suitable page for isam or hash */
		/* determine primary page */
		if ((i = find(d, FULLKEY, tid, tid, tuple))  != 0){
			return (i);	/* fatal error */
		}

		/* If we are not checking for duplicates then take any
		** convenient page linked to the main page current indicated
		** in "tid"
		*/
		if (!checkdups)
			find_page(d, tid, need);
	}

	/* search the chain of pages looking for a spot */
	for (;;) {
		if ((i = get_page(d, tid)) != 0)
			break;		/* fatal error */

		/* if tuple is duplicate, drop out */
		if (checkdups && dup_check(d, tid, tuple)) {
			i = 1;
			break;
		}

		/* is there space on this page */
		if (space_left(Acc_head) >= need)
			break;	/* found a page to use */

		/* no space yet. look on next overflow page */
		if (Acc_head->am_overflowpg) {
			stuff_page(tid, &Acc_head->am_overflowpg);
			continue;
		}

		/* no space. allocate new overflow page */
		if ((i = add_ovflo(d, tid)) != 0)
			break;		/* fatal error */
	}

	/* check for dups on remaining overflow pages */
	/* check only if there hasn't been a dup or a page error */
	if (i == 0 && checkdups && Acc_head->am_overflowpg) {
		stuff_page(&temptid, &Acc_head->am_overflowpg);
		if ((i = scan_dups(d, &temptid, tuple)) != 0) {
			bmove((char *) &temptid, (char *) tid, sizeof(temptid));
			/* tid of duplicate */
		}
	}

	/* if tuple isn't a duplicate, allocate a line number */
	if (i == 0) {
		tid->line_id = newlino(need);
	}

#ifdef xATR1
	if (tTf(27, 0)) {
		printf("findbest ret %d,", i);
		dumptid(tid);
	}
#endif
	return (i);
}
