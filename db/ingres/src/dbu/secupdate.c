#include <sys/types.h>

#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <symbol.h>
#include <access.h>
#include <batch.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)secupdate.c	8.3	2/8/85)

/*
** 	SECUPDATE - updates secondary indices
**
**
**	Parameters:
**		rel - relation being updated
**
**	Return Codes:
**		0
**
**	Trace Flags:
**		Z49, 49.7 49.8 49.15
**
**	Called by:
**		update()
**
*/
void
secupdate(register desc_t *r)
{
	register char	*p;
	register int	i;
	int		j;
	int		domcnt;
	int		mode;
	int		dom;
	long		tupcnt;
	long		oldtid;
	long		newtid;
	tid_t		lotid;
	tid_t		hitid;
	tid_t		uptid;
	char		oldtup[MAX_TUP_SIZE];
	char		newtup[MAX_TUP_SIZE];
	char		oldkey[MAX_TUP_SIZE];
	char		newkey[MAX_TUP_SIZE];
	char		dumtup[MAX_TUP_SIZE];
	index_t		itup;
	desc_t		si_desc;
	extern desc_t	Inddes;
	struct key_pt {
		char	*pt_old;
		char	*pt_new;
	};
	struct key_pt	keys[MAX_2ND_KEYS+1];

	mode = Batchhd.b_updtype;
	Batch_dirty = FALSE;
#ifdef xZTR1
	if (tTf(49, -1))
		printf("SECUPDATE\n");
#endif
	opencatalog("indices", OR_READ);
	ingres_setkey(&Inddes, &itup, r->d_r.r_id, IRELIDP);
	ingres_setkey(&Inddes, &itup, r->d_r.r_owner, IOWNERP);
	if ((i = find(&Inddes, EXACTKEY, &lotid, &hitid, &itup)) != 0) {
		syserr("secupdate:find indices %d", i);
	}

	/* update each secondary index */
	while(!(i = get(&Inddes, &lotid, &hitid, &itup, TRUE))) {
		/* check if the index is on the right relation */
#ifdef xZTR1
		if (tTf(49, 7)) {
			printup(&Inddes, (char *) &itup);
		}
#endif
		if (!bequal(itup.i_relname, r->d_r.r_id, MAX_NAME_SIZE) ||
			!bequal(itup.i_owner, r->d_r.r_owner, 2))
			continue;

		if ((i = openr(&si_desc, OR_WRITE, itup.i_index)) != 0)
			syserr("secupdate:can't openr %.12s %d", itup.i_index, i);
		/* reposition batch file to the beginning. */
		if ((i = lseek(Batch_fp, (off_t) 0, 0)) < 0)
			syserr("secupdate:seek %d %d", i, Batch_fp);
		Batch_cnt = BATCHSIZE;
		getbatch(&Batchhd, sizeof(Batchhd));	/* reread header */

		/* set up the key structure */
		p = itup.i_dom;
		for (domcnt = 0; domcnt < MAX_2ND_KEYS; domcnt++) {
			if ((dom = *p++) == 0)
				break;	/* no more key domains */
#ifdef xZTR1
			if (tTf(49, 15))
				
				printf("dom %d, sd_savedoff %d\n", dom, Batchhd.b_domv[dom].sd_savedoff);
#endif
			keys[domcnt].pt_old = &oldtup[Batchhd.b_domv[dom].sd_savedoff];
			keys[domcnt].pt_new = &newtup[r->d_off[dom]];
		}

		/* the last domain is the "tidp" field */
		keys[domcnt].pt_old = (char *) &oldtid;
		keys[domcnt].pt_new = (char *) &newtid;

		/*
		** Start reading the batch file and updating
		** the secondary indices.
		*/
		tupcnt = Batchhd.b_updc;
		while (tupcnt--) {
			getbatch(&oldtid, Batchhd.b_oldtidsize);
			getbatch(oldtup, Batchhd.b_oldtupsize);
			getbatch(newtup, Batchhd.b_newtupsize);
			getbatch(&newtid, Batchhd.b_newtidsize);

			/* if this is a replace or append form the new key */
			if (mode != mdDEL) {
				for (j = 0; j <= domcnt; j++)
					ingres_setkey(&si_desc, newkey, keys[j].pt_new, j+1);
#ifdef xZTR1
				if (tTf(49, 7))
					printup(&si_desc, newkey);
#endif
			}

			/* if this is delete or replace form the old key */
			if (mode != mdAPP) {
				for (j = 0; j <= domcnt; j++)
					ingres_setkey(&si_desc, oldkey, keys[j].pt_old, j+1);
#ifdef xZTR1
				if (tTf(49, 8))
					printup(&si_desc, oldkey);
#endif
			}

			switch (mode) {

			  case mdDEL:
				if ((i = getequal(&si_desc, oldkey, dumtup, &uptid)) != 0) {
					if (i > 0)
						break;
					syserr("secupdate:getequal %d", i);
				}
				if ((i = delete(&si_desc, &uptid)) < 0)
					syserr("secupdate:delete %d", i);
				break;

			  case mdREPL:
				/* if the newtup = oldtup then do nothing */
				if (bequal(oldkey, newkey, si_desc.d_r.r_width))
					break;
				if ((i = getequal(&si_desc, oldkey, dumtup, &uptid)) != 0) {
					if (Batch_recovery && i > 0)
						goto secinsert;
					syserr("secupdate:getequal-repl %d", i);
				}
				if ((i = replace(&si_desc, &uptid, newkey, TRUE)) != 0) {
					/* if newtuple is dup of old, ok */
					if (i == 1)
						break;
					/* if this is recovery and old tid not there, try an insert */
					if (Batch_recovery && i == 2)
						goto secinsert;
					syserr("secupdate:replace %d", i);
				}
				break;

			  case mdAPP:
			  secinsert:
				if ((i = insert(&si_desc, &uptid, newkey, TRUE)) < 0)
					syserr("secupdate:insert %d", i);
			}
		}
		if ((i = closer(&si_desc)) != 0)
			syserr("secupdate:closer %.12s %d", si_desc.d_r.r_id, i);
	}
	if (i < 0)
		syserr("secupdate:bad get from indices %d", i);
}
