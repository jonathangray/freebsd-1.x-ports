#include <ingres.h>
#include <symbol.h>
#include <access.h>
#include <lock.h>
#include <range.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)scan_dups.c	8.1	12/31/84)


/*
**	Scan for duplicates. Start with the page
**	specified by tid and
**	continue until there are no more pages
**
**	Scan_dups guarantees that the same page will
**	be in the buffer on entry and exit
**
**	returns:
**		<0 if fatal error
**		0  if not duplicate
**		1  if duplicate
*/
int
scan_dups(desc_t *d, tid_t *tid, char *tuple)
{
	register int	dup, pagerr;
	tid_t		savetid;

	stuff_page(&savetid, &Acc_head->am_curpg);

	Acclock = FALSE;	/* turn concurrency off */
	dup = 0;	/* assume success */

	do {
		if ((pagerr = get_page(d, tid)) != 0) {
			break;	/* fatal error */
		}

		if ((dup = dup_check(d, tid, tuple)) != 0) {
			break;	/* found duplicate */
		}

		stuff_page(tid, &Acc_head->am_overflowpg);
	} while (Acc_head->am_overflowpg);

	if (pagerr || ((pagerr = get_page(d, &savetid)) != 0)) {
		dup = pagerr;	/* return fatal page error */
	}

	Acclock = TRUE;		/* turn concurency on */
	return (dup);
}

/*
**  DUP_CHECK -- check current page for a duplicate of tuple;
**
**	returns:
**		0 if not duplicate
**		1 if duplicate
*/
int
dup_check(desc_t *d, tid_t *tid, char *tuple1)
{
	register int	i, dom;
	short		*lp;
	int		len, lastline, tups_equal;
	char		*tup1, *tup2;
	char		tuple2[MAX_TUP_SIZE];

	/* determine starting and ending points */
	lastline = Acc_head->am_nextline;
	lp = Acc_head->am_linev;

	/* check each tuple for duplication */
	for (i = 0; i < lastline; i++) {
		/* is this line used? */
		if (*lp--) {
			/* yes. get tuple and check it */
			tid->line_id = i;
			tup2 = getint_tuple(d, tid, tuple2);
			tup1 = tuple1;
			tups_equal = TRUE;	/* assume equal */

			/* now check each domain for duplication */
			for (dom = 1; dom <= d->d_r.r_attrc; dom++) {
				len = d->d_len[dom] & I1MASK;
				if (d->d_fmt[dom] == CHAR_CONST) {
					tups_equal = scompare(tup1, len, tup2, len) == 0;
				} else {
					tups_equal = bequal(tup1, tup2, len);
				}
				if (!tups_equal) {
					/* not duplicates */
					break;
				}
				tup1 += len;
				tup2 += len;
			}
			if (tups_equal) {
				return (1);	/* duplicate */
			}
		}
	}
	return (0);	/* no duplicate */
}
