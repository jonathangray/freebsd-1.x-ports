#include <stdio.h>

#include <ingres.h>
#include <access.h>
#include <catalog.h>
#include <batch.h>
#include <btree.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)replace.c	8.2	2/8/85)

#define	SAMETUP		0
#define	SAMEKEYS	1
#define	DIFFTUP		2

/*
**  REPLACE - replace an already existing tuple
**
**	Replace will replace the tuple specified by tid_t
**	with the new tuple. An attempt is made to not
**	move the tuple if at all possible.
**
**	Three separate conditions are dealt with. If the
**	new tuple is the same as the old tuple, a return 
**	of zero occures and the page is not changed.
**
**	If the keys(if any) are the same and the canonical
**	tuple lengths are the same, then the new tuple will
**	be placed in the same location.
**
**	If the lengths or the keys are different, then the
**	tuple is deleted and the new tuple inserted
**
**	Checkdups specifies whether to check for duplicates.
**	If the new tuple is a duplicate of one already there,
**	then the tuple at tid_t is deleted
**
**	Returns:
**		<0  fatal error
**		 1(DUPTUP)  new tuple was duplicate of returned tid
**		 2(DELTUP) tuple identified by tid has been deleted
**		 3(BADLID) bad lid
**
**		If replace returns 1 then tid is set to the
**		duplicate tuple. This is necessary for updating
**		secondary indices.
**
**	Trace Flags:
**		24.4-7
*/

int
replace(desc_t *d, tid_t *tid, void *tuple, int checkdups)	
{
	register int	i;
	int		j;
	char		oldtuple[MAX_TUP_SIZE];
	tid_t		primtid, tidpos;
	long		primpage, old_lid[MAXLID], new_lid[MAXLID], page, t;
	int		need, same, numatts;
	int		len, oldlength;
	char		*new, *old, *oldt;
	char		btree[MAX_NAME_SIZE + 4];
	tid_t		oldtid;
	int		lidwid, compare;
	locator_t	temp;

#ifdef xATR1
	if (tTf(24, 4)) {
		printf("replace: %.14s,", d->d_r.r_id);
		dumptid(tid);
		printf("replace: ");
		printup(d, tuple);
	}
#endif

	t = 0;
	compare = 0;

	/* make tuple canonical */
	need = canonical(d, tuple);

	/* if heap or ordered, no dup checking */
	if (M_TYPEOF(d->d_r.r_spec) == M_HEAP || d->d_r.r_dim > 0) {
		checkdups = FALSE;
	}

	if ((i = get_page(d, tid)) != 0) {
		return (i);	/* fatal error */
	}

	/* check if tid exists */
	if ((i = invalid(tid)) != 0) {
		return (i);	/* already deleted or invalid */
	}

	oldt = getint_tuple(d, tid, oldtuple);
	/* reset page back to main relation page */
	if ((i = get_page(d, tid)) != 0) {
		return(i);
	}
	oldlength = tup_len(tid);
	lidwid = LIDSIZE * d->d_r.r_dim;

	if (d->d_r.r_dim > 0) {
		/* extract lid values from tuples */
		btreename(d->d_r.r_id, btree);
		old = oldt + d->d_r.r_width  - lidwid;
		bmove(old, old_lid, lidwid);
		new = tuple + d->d_r.r_width - lidwid;
		bmove(new, new_lid, lidwid);
		compare = 0;
		for (i = 0; i < d->d_r.r_dim; ++i) {
			if (new_lid[i] > old_lid[i]) {
				compare = 1;
				break;
			}
			else if (new_lid[i] == old_lid[i])
				compare = -1;
			else {
				compare = 0;
				break;
			}
		}
		if (compare >= 0) {
		/* do insertion and deletion of new lid and old values in
		** order that insures that they will be placed in the proper
		** place
		*/
			if (compare == 1) {
				if (insert_mbtree(d, btree, new_lid, (long *) tid, &tidpos) < 0)
					return(BADLID);
				if (fwrite(old_lid, 1, lidwid, Del_infp) != lidwid)
					syserr("write error in replace");
				++Del_cnt;
			}
			else if (compare == 0) {
				page = RT;
				for (j = 0; j < d->d_r.r_dim; ++j) {
					if (new_lid[j] > 0 && (t = get_tid(page, new_lid[j], &temp)) > 0)
						page = t;
					else if (t == -1) {
						for (i = j + 1; i < d->d_r.r_dim; ++i) {
							if (new_lid[i] != 1 && new_lid[i] != 0)
								return(BADLID);
						}
						break;
					} else if (new_lid[j] == 0) {
						for (i = j + 1; i < d->d_r.r_dim; ++i) {
							if (new_lid[i] != 0)
								return(BADLID);
						}
						break;
					}
					else
						return(BADLID);
				}
				for (i = 0; i < d->d_r.r_dim; ++i)
					if (new_lid[i] < 0)
						return(BADLID);
				delete_btree(old_lid, d->d_r.r_dim);
				if (insert_mbtree(d, btree, new_lid, (long *) tid, &tidpos) < 0)
					return(BADLID);
			}
		}
	}

	/* check whether tuples are the same, different lengths, different keys */
	same = DIFFTUP;	/* assume diff lengths or keys */
	if (oldlength == need) {
		/* same size. check for same domains */
		same = SAMETUP;	/* assume identical */
		new = tuple;
		old = oldt;
		/* ignore lid field */
		numatts = d->d_r.r_attrc - d->d_r.r_dim;
		for (i = 1; i <= numatts; i++) {
			len = d->d_len[i] & I1MASK;
			if (icompare(new, old, d->d_fmt[i], len)) {
				if (d->d_iskey[i]) {
					same = DIFFTUP;
					break;
				}
				same = SAMEKEYS;
			}
			old += len;
			new += len;
		}
	}

#ifdef xATR2
	if (tTf(24, 5))
		printf("replace:same=%d\n", same);
#endif

	switch (same) {

	  case SAMETUP:
		/* new tuple same as old tuple */
		i = DUPTUP;	/* flag as duplicate */
		/* though character strings may compare equal,
		**  they can look different, so if they do look different
		**  go ahead and do the replace using put_tuple.  */
		if (!bequal(tuple, oldt, d->d_r.r_width - lidwid))
			goto puttuple;
		break;

	  case SAMEKEYS:
		/* keys the same, lengths the same, tuples different */
		if (checkdups) {
			/* This is either an ISAM or HASH file. If am_mainpg
			** is non-zero, then the primary page=am_mainpg -1.
			** Otherwise, "find" must be called to determine
			** the primary page
			*/
			if (Acc_head->am_mainpg) {
				primpage = Acc_head->am_mainpg -1;
				stuff_page(&primtid, &primpage);
			} else {
				if ((i = find(d, FULLKEY, &primtid, &primtid, tuple)) != 0)
					return (i);	/* fatal error */
				if ((i = get_page(d, tid)) != 0)	/* restore page for tuple */
					return (i);
			}
	
			if ((i = scan_dups(d, &primtid, tuple)) != 0) {
				if (i == DUPTUP) {
					del_tuple(tid, oldlength);	/* tuple a duplicate */
					d->d_addc--;
					/* copy tid of duplicate tuple */
					bmove(&primtid, tid, sizeof(primtid));
				}
				break;
			}
		}
		goto puttuple;

	  case DIFFTUP:
		/* keys different or lengths different */
		get_page(d, tid);
		del_tuple(tid, oldlength);
		bmove(tid, &oldtid, LIDSIZE);

		/* find where to put tuple */
		if ((i = findbest(d, tid, tuple, need, checkdups)) != 0) {
			d->d_addc--;
			break;
		}

		/* place new tuple in page */
	puttuple:
		put_tuple(tid, Acctuple, need);
		i = NEWTUP;

		if (same == DIFFTUP && d->d_r.r_dim > 0) {
			/* main tid value has changed, update btree */
			if (compare < 0)
				search_btree(oldtid, &tidpos);
			/* tid different, must be reflected in BTree */
			replace_btree(*tid, &tidpos);
		}
	}

#ifdef xATR1
	if (tTf(24, 6)) {
		printf("replace rets %d,", i);
		dumptid(tid);
	}
#endif
	return (i);
}
