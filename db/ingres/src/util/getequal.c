#include <stdio.h>

#include <ingres.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)getequal.c	8.1	12/31/84)


/*
**	getequal - get the first tuple equal to the provided key
**	
**	GETEQUAL is used to do a keyed retrieval of a single
**	tuple in cases where the calling program knows the key to
**	be unique.  SETKEY must be called first to set all desired
**	domain values.  GETEQUAL will return the first tuple with
**	equality on all of the specified domains.
**	The tuple is returned in TUPLE.
**	
** NOTE:
**	This function used to call get() for the scan, I have exploded the
**	call for efficency reasons. I also call get_page only when we leave a
**	page boundary.
**
**	function values:
**	
**		<0 fatal error
**		 0  success
**		 1  no match
**
**	Trace Flags:
**		23.8-15
*/

int
getequal(desc_t *d, void *keyval, void *tupleval, tid_t *tid)
{
	long	lpageid,pageid;
	tid_t		limtid;
	register	char	*tuple;
	register	char	*key;
	register	int	i;

	key = keyval;
	tuple = tupleval;

#ifdef xATR1
	if (tTf(23, 8)) {
		printf("getequal: %.14s,", d->d_r.r_id);
		printdesc(d);
		printup(d, key);
	}
#endif
	if ((i = find(d, EXACTKEY, tid, &limtid, key)) != 0)
		return (i);

	pluck_page(&limtid, &lpageid);
	if ((i = get_page(d, tid)) != 0)
		return ( i );

	for (;;) {

		do {
			while (((++(tid->line_id)) & I1MASK) >= Acc_head->am_nextline) {
				tid->line_id = -1;
				pageid = Acc_head->am_overflowpg;
				stuff_page(tid, &pageid);
				if (pageid == 0) {
					pageid = Acc_head->am_mainpg;
					stuff_page(tid, &pageid);
					if (pageid == 0 || pageid == lpageid + 1)
						return (1);
				}
				if ((i = resetacc(Acc_head)) != 0)
					return (i);
				if ((i = get_page(d, tid)) != 0)
					return (i);
			}
		} while (!Acc_head->am_linev[-(tid->line_id & I1MASK)]);

		get_tuple(d,tid,tuple);

		if (!kcompare(d, key, tuple)) {
#ifdef xATR2
			if (tTf(23, 9)) {
				printf("getequal: ");
				dumptid(tid);
				printf("getequal: ");
				printup(d, tuple);
			}
#endif
			return (0);
		}
	}
}
