#include <sys/types.h>
#include <sys/stat.h>

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

#include <pv.h>
#include <ingres.h>
#include <resp.h>
#include <aux.h>
#include <symbol.h>
#include <access.h>
#include <batch.h>
#include <catalog.h>
#include <btree.h>
#include <version.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include <protos.h>

SCCSID(@(#)update.c	8.4	2/8/85)

/*
**	Update reads a batch file written by the
**	access method routines (openbatch, addbatch, closebatch)
**	and performs the updates stored in the file.
**
**	It assumes that it is running in the database. It
**	is driven by the data in the Batchhd struct (see ../batch.h).
**	If the relation has a secondary index then update calls
**	secupdate. As a last step the batch file is removed.
**
**	The global flag Batch_recovery is tested in case
**	of an error. It should be FALSE if update is being
**	run as the dbu deferred update processor. It should
**	be TRUE if it is being used as part of the recovery
**	procedure.
*/
int
update(int pc, paramv_t *pv)
{
	register int	i, mode;
	desc_t		rel, d;
	long		oldtid, tupcnt;
	char		oldtup[MAX_TUP_SIZE], newtup[MAX_TUP_SIZE];
	char		*tp;
	long		bad_lid[MAXLID], lid, old_lid[MAXLID], new_lid;
	char		bad[MAXLID][10];
	char		delbtree[MAX_NAME_SIZE + 4], replbtree[MAX_NAME_SIZE + 4];
	char		btree[MAX_NAME_SIZE + 4], out[MAX_NAME_SIZE + 4];
	char		filname[MAX_NAME_SIZE+4];
	struct stat	sbuf;
	long		num;
	int		first, end, batchcnt, j, k;
	FILE		*fp;
	long		temp;
	long		del_cnt;
	resp_t		*rp;
	extern desc_t	Btreesec;

	tp = (char *) NULL;
	batchcnt = 0;
#ifdef xZTR1
	if (tTf(48, -1))
		printf("Update on %s\n", batchname());
#endif
	/* set up to read batchhd */
	Batch_cnt = BATCHSIZE;	/* force a read on next getbatch */
	Batch_dirty = FALSE;
	if ((Batch_fp = open(batchname(), O_RDWR)) < 0)
		syserr("prim:can't open %s", batchname());
	getbatch(&Batchhd, sizeof(Batchhd));

	tupcnt = Batchhd.b_updc;
#ifdef xZTR1
	if (tTf(48, 0))
		printf("rel=%s tups=%ld\n", Batchhd.b_relname, tupcnt);
#endif
	rp = getresp();
	rp->resp_tups = 0;
	if (!tupcnt) {
		rmbatch();
		return (1);
	}

	/* update the primary relation */
	if ((i = openr(&rel, OR_WRITE, Batchhd.b_relname)) != 0) {
		syserr("prim:can't openr %s %d", Batchhd.b_relname, i);
	}
	if (rel.d_r.r_dim > 0) {
		bmove(rel.d_btree, &Btreesec, sizeof(Btreesec));
		setglobalint(BTREE_FD_NAME, rel.d_btreefd);
	}

	mode = Batchhd.b_updtype;

	if (rel.d_r.r_dim > 0) {
		/* create files necessary for updating btrees in specified order */
		concat(REPL_IN, Fileset, replbtree);
		if ((Repl_infp = fopen(replbtree, "w")) == NULL)
			syserr("can't open %s", replbtree);
		concat(DEL_IN, Fileset, delbtree);
		if ((Del_infp = fopen(delbtree, "w")) == NULL)
			syserr("can't open %s", delbtree);
	}
	Del_cnt = 0;
	for (i = 0; i < MAXLID; ++i) {
		Prev_lid[i] = 0;
		Repl_cnt[i] = 0;
	}

	if (rel.d_r.r_dim > 0) {
		if (tupcnt <= 1 || (mode != mdREPL && mode != mdAPP))
			fclose(Repl_infp);
		else {
			/* do replace's in ascending lid-value order */
			d.d_off[1] = 0;
			d.d_off[2] = Batchhd.b_oldtidsize + Batchhd.b_oldtupsize + Batchhd.b_newtupsize - LIDSIZE;
			for (i = 1; i <= rel.d_r.r_dim; ++i) {
				d.d_off[i+2] = d.d_off[i+1] + LIDSIZE;
				d.d_fmt[i+1] = INT_CONST;
				d.d_len[i+1] = LIDSIZE;
				d.d_given[i+1] = i;
			}
			d.d_fmt[i+1] = CHAR_CONST;
			d.d_len[i+1] = Batchhd.b_newtidsize;
			d.d_given[0] = 0;
			d.d_given[1] = i;
			d.d_given[i+1] = i + 1;
			d.d_fmt[1] = CHAR_CONST;
			d.d_len[1] = d.d_off[2];
			d.d_r.r_spec = M_ORDER;
			d.d_r.r_attrc = 2 + rel.d_r.r_dim;
			d.d_r.r_width = d.d_off[2] + LIDSIZE + Batchhd.b_newtidsize;
			/* extract information about tuples from batch file */
			if (stat(batchname(), &sbuf) < 0)
				syserr("bad file for stat %s", batchname());
			num = sbuf.st_size / (BATCHSIZE + IDSIZE);
			if (num >= 1)
				first = BATCHSIZE - Batch_cnt;
			else
				first = sbuf.st_size - sizeof(Batchhd) - IDSIZE;
			if ((i = fwrite(&Batchbuf.b_buf[Batch_cnt], 1, first, Repl_infp)) != first)
				syserr("can't write replace file");
			for (i = 2; i <= num; ++i) {
				Batch_cnt = BATCHSIZE;
				readbatch();
				if (fwrite(Batchbuf.b_buf, 1, BATCHSIZE, Repl_infp) != BATCHSIZE)
					syserr("can't write to replace file");
			}
			Batch_cnt = BATCHSIZE;
			readbatch();
			end = ((sbuf.st_size - BATCHSIZE - IDSIZE) % (BATCHSIZE + IDSIZE)) - IDSIZE;
			if (end > 0)
				if (fwrite(Batchbuf.b_buf, 1, end, Repl_infp) != end)
					syserr("can't write to replace file 2");
			fclose(Repl_infp);
			sortfile(replbtree, &d, FALSE);
			if ((Repl_outfp = fopen(ztack(REPL_OUT, Fileset), "r")) == NULL)
				syserr("can't open replace file in update for reading\n");
			concat("_SYStemp", Fileset, filname);
			/* rewrite in batch file in sorted order */
			if ((fp = fopen(filname, "w")) == NULL)
				syserr("can't open %s", filname);
			if ((k = fread(Batchbuf.b_buf, 1, first, Repl_outfp)) != first)
				syserr("read error0 from replace file %d", k);
			if (fwrite(Batchbuf.b_file, 1, IDSIZE, fp) != IDSIZE)
				syserr("write error in batch file");
			if (fwrite(&Batchhd, 1, sizeof(Batchhd), fp) != sizeof(Batchhd))
				syserr("write error in batch file");
			if (fwrite(Batchbuf.b_buf, 1, first, fp) != first)
				syserr("write error in batch file");
			for (i = 2; i <= num; ++i) {
				if ((k = fread(Batchbuf.b_buf, 1, BATCHSIZE, Repl_outfp)) != BATCHSIZE)
					syserr("read error1 in replace file %d",  k);
				if (fwrite(&Batchbuf, 1, BATCHSIZE + IDSIZE, fp) != BATCHSIZE + IDSIZE)
					syserr("write error into temp repl file");
			}
			if (end > 0) {
				if ((k = fread(Batchbuf.b_buf, 1, end, Repl_outfp)) != end)
					syserr("read error2 from replace file %d", k);
				if (fwrite(&Batchbuf, 1, end + IDSIZE, fp) != end + IDSIZE)
					syserr("write error into temp repl file");
			}
			fclose(fp);
			fclose(Repl_outfp);
			unlink(ztack(REPL_OUT, Fileset));
			rmbatch();
			if (link(filname, batchname()) == -1)
				syserr("can't link %s", batchname());
			unlink(filname);
			Batch_cnt = BATCHSIZE;
			Batch_dirty = FALSE;
			if ((Batch_fp = open(batchname(), O_RDWR)) < 0)
				syserr("can't open new batch file");
			getbatch(&Batchhd, sizeof(Batchhd));
		}
		unlink(replbtree);
	}

	while (tupcnt--) {
		getbatch(&oldtid, Batchhd.b_oldtidsize);	/* read old tid */
		getbatch(oldtup, Batchhd.b_oldtupsize);	/* and portions of old tuple */
		if (!rel.d_r.r_dim)
			getbatch(newtup, Batchhd.b_newtupsize);	/* and the newtup */
		else {
			if (Batchhd.b_newtupsize > 0) {
				getbatch(newtup, Batchhd.b_newtupsize - rel.d_r.r_dim * LIDSIZE);
				batchcnt = Batch_cnt;
				tp = newtup + Batchhd.b_newtupsize - rel.d_r.r_dim * LIDSIZE;
				getbatch(tp, rel.d_r.r_dim * LIDSIZE);
			}
		}

		switch (mode) {

		  case mdDEL:
			if ((i = delete(&rel, &oldtid)) < 0)
				syserr("prim:bad del %d %s", i, Batchhd.b_relname);
			break;

		  case mdREPL:
			if ((i = replace(&rel, &oldtid, newtup, TRUE)) != 0) {
				/* if newtuple is a duplicate, then ok */
				if (i == 1) {
					if (rel.d_r.r_dim)
						++rp->resp_tups;
					break;
				}
				if (i == 3) {
					bmove(newtup + rel.d_r.r_width - LIDSIZE, &new_lid, LIDSIZE);
					bmove(tp, bad_lid, LIDSIZE * rel.d_r.r_dim);
					for(j = 0; j < rel.d_r.r_dim; ++j)
						strcpy(bad[j], locv(bad_lid[j]));
					switch (rel.d_r.r_dim) {
					case 1:
						nferror(BADLID1, trim_relname(rel.d_r.r_id), bad[0], 0);
						break;
					case 2:
						nferror(BADLID2, trim_relname(rel.d_r.r_id), bad[0], bad[1], 0);
						break;
					case 3:
						nferror(BADLID3, trim_relname(rel.d_r.r_id), bad[0], bad[1], bad[2], 0);
						break;
					}					
					Batch_cnt = batchcnt + LIDSIZE * (rel.d_r.r_dim - 1);
					lid = -1;
					putbatch(&lid, LIDSIZE);
					break;
				}
				/* if this is recovery and oldtup not there, try to insert newtup */
				if (Batch_recovery && i == 2)
					goto upinsert;
				syserr("prim:Non-functional replace on %s (%d)", i, Batchhd.b_relname);
			}
			rp->resp_tups++;
			break;

		  case mdAPP:
		  upinsert:
			if ((i = insert(&rel, &oldtid, newtup, TRUE)) < 0)
				syserr("prim:bad insert %d %s", i, Batchhd.b_relname);
 			if (i == 2) {
				tp = newtup + rel.d_r.r_width - rel.d_r.r_dim * LIDSIZE;
				bmove(tp, bad_lid, LIDSIZE * rel.d_r.r_dim);
				for (j = 0; j < rel.d_r.r_dim; ++j)
					strcpy(bad[j], locv(bad_lid[j]));
				switch (rel.d_r.r_dim) {
				case 1:
					nferror(BADLID1, trim_relname(rel.d_r.r_id), bad[0], 0);
					break;
				case 2:
					nferror(BADLID2, trim_relname(rel.d_r.r_id), bad[0], bad[1], 0);
					break;
				case 3:
					nferror(BADLID3, trim_relname(rel.d_r.r_id), bad[0], bad[1], bad[2], 0);
					break;
				}					
				oldtid = -1;
			} else if (rel.d_r.r_dim > 0) {
				if (batchcnt + rel.d_r.r_dim * LIDSIZE > BATCHSIZE) {
					if ((j = lseek(Batch_fp, (off_t) -(Batch_cnt + BATCHSIZE + 2 * IDSIZE), 1)) < 0)
						syserr("Lseek error in update");
					readbatch();
				}
				Batch_cnt = batchcnt;
				tp  =  newtup + rel.d_r.r_width - LIDSIZE * rel.d_r.r_dim;
				putbatch(tp, rel.d_r.r_dim * LIDSIZE);
			}
			break;

		  default:
			syserr("prim:impossible mode %d", mode);
		}
		putbatch(&oldtid, Batchhd.b_newtidsize);	/* write new tid if necessary */
	}
	if (rel.d_r.r_dim > 0) {
		/* do deletions in decending lid-value order */
		fclose(Del_infp);
		if (Del_cnt != 0) {
			if (Del_cnt > 1) {
				d.d_off[0] = -LIDSIZE;
				d.d_given[0] = 0;
				for (i = 1; i <= rel.d_r.r_dim; ++i) {
					d.d_off[i] = d.d_off[i-1] + LIDSIZE;
					d.d_fmt[i] = INT_CONST;
					d.d_len[i] = LIDSIZE;
					d.d_given[i] = -i;
				}
				d.d_r.r_spec = M_COMPRESS(M_ORDER);
				d.d_r.r_attrc = rel.d_r.r_dim;
				d.d_r.r_width = LIDSIZE * rel.d_r.r_dim;
				sortfile(delbtree, &d, TRUE);
			}
			btreename(rel.d_r.r_id, btree);
			del_cnt = Del_cnt;
			if (del_cnt == 1)
				concat(DEL_IN, Fileset, out);
			else
				concat(DEL_OUT, Fileset, out);
			if ((Del_outfp = fopen(out, "r")) == NULL)
				syserr("can't open delete file in update for reading\n");
			while (del_cnt--) {
				if (fread(old_lid, 1, LIDSIZE * rel.d_r.r_dim, Del_outfp) != LIDSIZE * rel.d_r.r_dim)
					syserr("tup_buf read error");

				if (delete_btree(old_lid, rel.d_r.r_dim) < 0) {
					printf("DELETE ERROR: %s: bad lid(s)\n", trim_relname(rel.d_r.r_id));
					for (i = 0; i < rel.d_r.r_dim; ++i)
						printf("\tlid%d=%ld\n", i + 1, old_lid[i]);
					syserr("DELETE ERROR");
				}
			}
			fclose(Del_outfp);
			unlink(out);
		}
		unlink(delbtree);
	}

	/* fix the tupchanged count if delete or append */
	if (mode != mdREPL)
		rp->resp_tups = rel.d_addc >= 0 ? rel.d_addc : -rel.d_addc;
	/* close the relation but secupdate will still use the decriptor */
	temp = rel.d_addc;
	if ((i = closer(&rel)) != 0)
		syserr("prim:close err %d %s", i, Batchhd.b_relname);
	rel.d_addc = temp;
	batchflush();

	/* if this relation is indexed, update the indices */
	if (rel.d_r.r_indexed > 0)
		secupdate(&rel);
	if (rel.d_r.r_dim > 0)
		btreeupdate(&rel);
	rmbatch();

#ifdef xZTR1
	if (tTf(48, 2))
		printf("%ld tups changed\n", rp->resp_tups);
#endif
	return (0);
}
