#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <errno.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <pv.h>
#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <access.h>
#include <batch.h>
#include <btree.h>
#include <symbol.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)modupdate.c	8.6	6/12/88)

/*
** MODUPDATE
**	This routine is used to exicute the updates 
**	for modifies so they are recoverable.
**	It is also used by restore to complete an aborted modify.
**	During a restore the Batch_recover flag should be set to 1;
*/
int
modupdate(void)
{
	char			batchname[MAX_NAME_SIZE + 3];
	char			temprel[MAX_NAME_SIZE+ 3];
	char			relfile[MAX_NAME_SIZE + 3];
	char			btreesec[MAX_NAME_SIZE + 3];
	register int		i;
	register int		j;
	struct stat		sbuf;
	char			aflag;
	struct tup_id		tid;
	desc_t			desx;
	attr_t			attkey, atttup;
	relation_t		oldreltup;
	index_t			ikey, itup;
	paramv_t		newpv[2];
	extern desc_t		Inddes, Attdes, Reldes;
	register desc_t		*desp;
	char			btree[MAX_NAME_SIZE + 4], temp_btree[MAX_NAME_SIZE + 4];
	short			numatts;
	extern int		NLidKeys;

	desp =  &desx;
	concat(MODBATCH,Fileset,batchname);
	concat(MODTEMP, Fileset, temprel);

#ifdef xZTR1
	if (tTf(34, 8))
		printf("Modupdate: %s, %s\n",batchname, temprel);
#endif
	if ((Batch_fp = open(batchname, O_RDONLY)) < 0)
		syserr("MODUPDATE:Can't open %s", batchname);
	Batch_cnt = BATCHSIZE;
	Batch_dirty = FALSE;
	getbatch(desp, sizeof(*desp));
	ingresname(desp->d_r.r_id, desp->d_r.r_owner, relfile);

	if (!desp->d_r.r_dim || NLidKeys > 0) {
		/* don't loose old file before verifying new file */
		if (stat(temprel, &sbuf) >= 0) {
			unlink(relfile);	/* Ok if failure */
			errno = 0;
			if (link(temprel, relfile) == -1)
				syserr("MODUPDATE:Can't link: %s, %s", temprel, relfile);
			if (unlink(temprel) < 0)
				syserr("modupdate:unlink(%s)", temprel);
		}
	
		else
			if(stat(relfile, &sbuf) < 0 || !Batch_recovery)
				syserr("MODUPDATE:Relation and/or temporary files for %s are missing",
					relfile);
	}

	/* Update admin if this is relation or atribute relations */
	/* Should only happen in Sysmod				  */
	if ((aflag = bequal(desp->d_r.r_id, "attribute   ", MAX_NAME_SIZE)) ||
		bequal(desp->d_r.r_id, "relation    ", MAX_NAME_SIZE)) {
		ingresname(desp->d_r.r_id, desp->d_r.r_owner, temprel);
		if ((i = open("admin", O_RDWR)) < 0)
			syserr("MODUPDATE:Can't open admin file");
		if (lseek(i, (off_t) sizeof(Admin.ad_h), 0) < 0 ||
			(aflag && lseek(i, (long) sizeof(*desp), 1) < 0))
			syserr("MODUPDATE:Seek error");
		if (write(i, desp, sizeof(*desp)) != sizeof(*desp))
			syserr("MODUPDATE:Write error on admin");
		close(i);

		if (aflag) {
			closer(&Attdes);
			cleanrel(&Admin.ad_attr);
			close(Admin.ad_attr.d_fd);
			bmove(desp, &Admin.ad_attr, sizeof(*desp));
			ingresname(Admin.ad_attr.d_r.r_id, Admin.ad_attr.d_r.r_owner, temprel);
			if ((Admin.ad_attr.d_fd = open(temprel, O_RDWR)) < 0)
				syserr("MODUPDATE: open wr Admin.ad_attr %d", Admin.ad_attr.d_fd);
			Admin.ad_attr.d_opened = (Admin.ad_attr.d_fd + 1) * -5;
		} else {
			closer(&Reldes);
			cleanrel(&Admin.ad_rel);
			close(Admin.ad_rel.d_fd);
			bmove(desp, &Admin.ad_rel, sizeof(*desp));
			if ((Admin.ad_rel.d_fd = open(temprel, O_RDWR)) < 0)
				syserr("MODUPDATE: open Admin.ad_rel %d",
					Admin.ad_rel.d_fd);
			Admin.ad_rel.d_opened = (Admin.ad_rel.d_fd + 1) * -5;
			/* search the new relation relation for the new
			   relation tid for the attribute relation */
			/* but for now, we guess. */
			bzero(&Admin.ad_attr.d_tid,
			      sizeof(Admin.ad_attr.d_tid));
		}
	}

	if ((i = get(&Admin.ad_rel, &desp->d_tid, &desp->d_tid, &oldreltup, FALSE)) != 0)
		syserr("MODUPDATE: get oldrel=%d",i);

	btreename(desp->d_r.r_id, btree);
	if (oldreltup.r_dim > 0) {
		/* relation formerly ordered */
		capital(trim_relname(desp->d_r.r_id), btreesec);
		newpv[0].pv_val.pv_str = btreesec;
		newpv[1].pv_type = PV_EOF;
		if (destroy(1, newpv))
			syserr("can't destroy btreesec");
		unlink(btree);

		/* remove tuple corresponding to LID field from attribute
		** relation
		*/
		ingres_setkey(&Admin.ad_attr, &attkey, desp->d_r.r_id, ATTRELID);
		ingres_setkey(&Admin.ad_attr, &attkey, desp->d_r.r_owner, ATTOWNER);
		numatts = oldreltup.r_attrc - oldreltup.r_dim + 1;
		for (i = oldreltup.r_dim; i > 0; i--, numatts++) {
			ingres_setkey(&Admin.ad_attr, &attkey, &numatts, ATTID);
			if (getequal(&Admin.ad_attr, &attkey, &atttup, &tid) == 0)
				if (delete(&Admin.ad_attr, &tid) < 0)
					syserr("MODUPDATE: Can't delete LID field from attribute relation");
		}
	}

	/* update attribute relation */

	Admin.ad_attr.d_opened = (Admin.ad_attr.d_fd + 1) * -5;
	numatts = desp->d_r.r_attrc - desp->d_r.r_dim;
	for (i = numatts; i > 0; i--) {
		getbatch(&tid, sizeof(tid));
		getbatch(&atttup, sizeof(atttup));
		if ((j = replace(&Admin.ad_attr, &tid, &atttup, FALSE)) != 0)
			if (j < 0 || j == 2)
				syserr("MODUPDATE:Replace error(att): %d", j);
	}

	for (i = desp->d_r.r_dim; i > 0; i--) {
		/* insert tuple corresponding to LID field into attribute relation */
		getbatch(&atttup, sizeof(atttup));
		if ((j = insert(&Admin.ad_attr, &tid, &atttup, FALSE) < 0))
			syserr("MODUPDATE: Insert error (att): %d", j);
	}

	unlink(batchname);

	if ((i = cleanrel(&Admin.ad_attr)) != 0)
		syserr("MODUPDATE:clean att %d", i);

	/* update relation relation */

	if ((i = replace(&Admin.ad_rel, &desp->d_tid, desp, FALSE)) != 0)
		if (i < 0 || i == 2)
			syserr("MODUPDATE:Replace error(rel): %d", i);

	if ((i = cleanrel(&Admin.ad_rel)) != 0)
		syserr("MODUPDATE:clean rel %d", i);

	if (desp->d_r.r_dim > 0) {
		/* link temporary BTree file to permanent storage */
		concat(BTREE, Fileset, temp_btree);
		if (link(temp_btree, btree) == -1)
			syserr("MODUPDATE: can't link: %s, %s", temp_btree, btree);
		if (unlink(temp_btree) < 0)
			syserr("modupdate: unlink %s", temp_btree);
	}

	/* make the admin readonly */
	Admin.ad_attr.d_opened = (Admin.ad_attr.d_fd + 1) * 5;

	close(Batch_fp);

	/* if this is an index, change the relspec in the index catalog */
	if (oldreltup.r_indexed == SECINDEX) {
		opencatalog("indices", OR_WRITE);
		ingres_setkey(&Inddes, &ikey, desp->d_r.r_id, IRELIDI);
		ingres_setkey(&Inddes, &ikey, desp->d_r.r_owner, IOWNERP);
		if ((i = getequal(&Inddes, &ikey, &itup, &tid)) == 0) {
			itup.i_indrelspec = desp->d_r.r_spec;
			if ((i = replace(&Inddes, &tid, &itup, 0)) != 0)
				if (i < 0 || i == 2)
					syserr("MODUPDATE: rep(ix) %d", i);
		}
	} else if (desp->d_r.r_indexed == SECBASE) {
		/* destroy any secondary indices on this primary */
		opencatalog("indices", OR_WRITE);
		ingres_setkey(&Inddes, &ikey, desp->d_r.r_id, IRELIDP);
		ingres_setkey(&Inddes, &ikey, desp->d_r.r_owner, IOWNERP);
		while ((i = getequal(&Inddes, &ikey, &itup, &tid)) == 0) {
			newpv[0].pv_val.pv_str = itup.i_index;
			newpv[1].pv_type = PV_EOF;
			printf("destroying secondary index %s\n", trim_relname(itup.i_index));
			if (destroy(1, newpv))
				syserr("MODUPDATE:Can't destroy %s", itup.i_index);
		}
	}
	if (i < 0)
		syserr("MODUPDATE: geteq(ix)b %d", i);

	/* clean things up and exit */
#ifdef xZTR1
	if (tTf(34, 8))
		printf("Leaving modupdate\n");
#endif
	return (0);
}
