#include <sys/types.h>

#include <stdio.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <symbol.h>
#include <access.h>
#include <batch.h>
#include <btree.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)btreeupdate.c	8.3	1/18/85)

void
btreeupdate(register desc_t *r)
{
	register int	i;
	int		j;
	int		mode;
	long		oldtid;
	long		newtid;
	long		tupcnt;
	tid_t		uptid;
	char		oldtup[MAX_TUP_SIZE];
	char		newtup[MAX_TUP_SIZE];
	char		oldkey[2 * LIDSIZE];
	char		newkey[2 * LIDSIZE];
	char		dumtup[2 * LIDSIZE];
	relation_t	rkey;
	relation_t	rtup;
	desc_t		b_desc;
	extern desc_t	Reldes;
	tid_t		tid, btid;
	char 		file[MAX_NAME_SIZE + 4];
	char		btree[MAX_NAME_SIZE + 4];
	long		oldlid[MAXLID];
	long		newlid[MAXLID];
	locator_t	tidpos;
	long		l;
	char		*tp;
	long		page;
	long		t;
	int		lidwid;
	int		dellevel;
	int		compare;

	i = 0;
#ifdef xZTR1
	if (tTf(47, 0))
		printf("BTREEUPDATE\n");
#endif

	mode = Batchhd.b_updtype;
	Batch_dirty = FALSE;
	opencatalog("relation", OR_READ);
	capital(trim_relname(r->d_r.r_id), file);
	ingres_setkey(&Reldes, &rkey, file, RELID);
	ingres_setkey(&Reldes, &rkey, r->d_r.r_owner, RELOWNER);

	if (!getequal(&Reldes, &rkey, &rtup, &tid)) {
		if ((i = openr(&b_desc, OR_WRITE, file)) != 0) {
			syserr("btreeupdate:can't openr %.12s %d", file, i);
		}
		/* reposition batch file to the beginning. */
		if ((i = lseek(Batch_fp, (off_t) 0, 0)) < 0) {
			syserr("secupdate:seek %d %d", i, Batch_fp);
		}
		Batch_cnt = BATCHSIZE;
		getbatch(&Batchhd, sizeof(Batchhd));	/* reread header */

		ingresname(r->d_r.r_id, r->d_r.r_owner, file);
		btreename(file, btree);
		setglobalint(BTREE_FD_NAME, open(btree, O_RDWR));
		if (getglobalint(BTREE_FD_NAME) < 0) {
			syserr("btreeupdate: can't open %s", btree);
		}

		/*
		** Start reading the batch file and updating
		** the secondary indices.
		*/
		l = r->d_addc;
		tupcnt = Batchhd.b_updc;
		lidwid = LIDSIZE * r->d_r.r_dim;
		dellevel = r->d_r.r_dim - 1;
		for (j = 0; j < r->d_r.r_dim; ++j) {
			if (Repl_cnt[j] > 0) {
				dellevel = j;
				break;
			}
		}
		while (tupcnt--) {
			getbatch(&oldtid, Batchhd.b_oldtidsize);
			getbatch(oldtup, Batchhd.b_oldtupsize);
			getbatch(newtup, Batchhd.b_newtupsize);
			getbatch(&newtid, Batchhd.b_newtidsize);

			clearkeys(&b_desc);
			/* if this is a replace or append form the new key */
			if (mode != mdDEL) {
				if (newtid < 0)
					continue;
				tp = newtup + Batchhd.b_newtupsize - lidwid;
				bmove(tp, newlid, lidwid);
				if (mode == mdREPL) {
					if (newlid[r->d_r.r_dim - 1] < 0)
						continue;
					tp = oldtup + Batchhd.b_oldtupsize - lidwid;
					bmove(tp, oldlid, lidwid);
					compare = 0;
					for (j = 0; j < r->d_r.r_dim; ++j) {
						if (newlid[j] > oldlid[j]) {
							compare = 1;
						}
						if (newlid[j] != oldlid[j])
							break;
					}
					if (compare == 1) {
						for (j = dellevel - 1; j >= 0; --j) {
							if (newlid[j] != oldlid[j]) {
								compare = 0;
								break;
							}
						}
					}
					if (compare == 1)
						/* adjust due to deleted lids */
						newlid[dellevel] -= Repl_cnt[dellevel];
				}
				page  =  RT;
				for (j = 0; j < r->d_r.r_dim; ++j) {
					if (!newlid[j])
						newlid[j] = 1;
					t = get_tid(page, newlid[j], &tidpos);
					page = t;
				}
				if (page != newtid) {
					/* try linear search of btree */
					lin_search(r->d_r.r_dim, newtid, &btid, newlid, Batchhd.b_updc);
					ingres_setkey(&b_desc, newkey, &newtid, 1);
				} else {
					ingres_setkey(&b_desc, newkey, &newtid, 1);
					stuff_page(&btid, &tidpos.pageno);
					btid.line_id = tidpos.page.node.leafnode.tid_loc[tidpos.offset];
				}
				ingres_setkey(&b_desc, newkey, &btid, 2);
#ifdef xZTR1
				if(tTf(47,0)) {
					printf("new key\n");
					printup(&b_desc, newkey);
				}
#endif
			}

			/* if this is delete or replace form the old key */
			if (mode != mdAPP) {
				ingres_setkey(&b_desc, oldkey, &oldtid, 1);
#ifdef xZTR1
				if(tTf(47,0)) {
					printf("old key\n");
					printup(&b_desc, oldkey);
				}
#endif
			}

			switch (mode) {

			  case mdDEL:
				if ((i = getequal(&b_desc, oldkey, dumtup, &uptid)) != 0) {
					if (i > 0)
						break;
					syserr("btreeupdate:getequal %d", i);
				}
				if ((i = delete(&b_desc, &uptid)) < 0)
					syserr("btreeupdate:delete %d", i);
				break;

			  case mdREPL:
				/* btree tid not provided */
				b_desc.d_given[2] = 0;
				if ((i = getequal(&b_desc, oldkey, dumtup, &uptid)) != 0) {
					if (Batch_recovery && i > 0)
						goto btreeinsert;
					printup(&b_desc, oldkey);
					syserr("btreeupdate:getequal-repl %d", i);
				}
				/* btree tid provided */
				b_desc.d_given[2] = 1;
				if ((i = replace(&b_desc, &uptid, newkey, TRUE)) != 0) {
					/* if newtuple is dup of old, ok */
					if (i == 1)
						break;
					/* if this is recovery and old tid not there, try an insert */
					if (Batch_recovery && i == 2)
						goto btreeinsert;
					syserr("secupdate:replace %d", i);
				}
				break;

			  case mdAPP:
			  btreeinsert:
				if ((i = insert(&b_desc, &uptid, newkey, TRUE)) < 0)
					syserr("secupdate:insert %d", i);
			}
		}
		if ((i = closer(&b_desc)) != 0) {
			syserr("btreeupdate:closer %.12s %d", file, i);
		}
		(void) close(getglobalint(BTREE_FD_NAME));
	}
	if (i < 0) {
		syserr("btreeupdate:bad get from indices %d", i);
	}
}
