#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <symbol.h>
#include <access.h>
#include <batch.h>
#include <btree.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)batch.c	8.2	1/17/85)

/*
**	Open batch prepares for batch processing.
**	1. If the batch is already open, return an error
**	2. Create the batch file.
**	3. clear domain flags.
**	4. If the relation is indexed, Identify all the domains
**		which must be saved to speed the index update.
**	5. Set up specifics for each type of update.
**	6. Write out the batch structure.
**
**	The following itemizes what is saved (in bytes):
**	f(si) means it's a function of the secondary index keys
**	space for newtid is saved only if there is a sec. index.
**
**			mdDEL	mdREPL	mdAPP
**
**	oldtid		4	4	0
**	oldtuple	f(si)	f(si)	0
**	newtuple	0	tupwid	tupwid
**	newtid		0	4	4
*/
int
openbatch(desc_t *rel_desc, desc_t *index_desc, int mode)
{
	register struct si_doms	*sp;
	register desc_t		*rel, *indx;
	int			i, j, saveoff, dom;
	char			*p;
	tid_t			lotid, hitid;
	index_t		itup;
	extern char		*Database;

	if (Batchhd.b_updtype)
		return (-1);	/* batch already open */
	rel = rel_desc;
	indx = index_desc;
	p = batchname();	/* form batch name */
#ifdef xATR1
	if (tTf(25, -1))
		printf("Openbatch %s\n", p);
#endif
	if ((Batch_fp = open(p, O_CREAT | O_TRUNC | O_WRONLY, FILEMODE)) < 0)
		syserr("openbatch:can't creat %s,%d", p, Batch_fp);
	Batch_cnt = 0;

	/* copy the important info */
	smove(Fileset, Batchbuf.b_file);
	smove(Database, Batchhd.b_dbname);
	bmove(rel->d_r.r_id, Batchhd.b_relname, MAX_NAME_SIZE);
	bmove(rel->d_r.r_owner, Batchhd.b_user, sizeof(rel->d_r.r_owner));
	Batchhd.b_updtype = mode;
	Batchhd.b_updc = 0;

	/* clear out the secondary index domain flags */
	sp = Batchhd.b_domv;	/* sp points to the structure */
	for (i = 1; i <= MAX_DOMAINS; i++) {
		sp->sd_domsize = 0;
		sp++;
	}
	Batchhd.b_domc = 0;

	/* set up the tid and tuple sizes by type of update */
	/* assume size of tido, tidn, and tupn */
	Batchhd.b_oldtidsize = sizeof(tid_t);	/* assume old tid is needed */
	Batchhd.b_oldtupsize = 0;	/* assume old tuple isn't needed */
	Batchhd.b_newtupsize = rel->d_r.r_width;	/* assume new tuple is needed */
	Batchhd.b_newtidsize = sizeof(tid_t);	/* assume space is needed for new tid */
	switch(Batchhd.b_updtype) {

	  case mdDEL:
		Batchhd.b_newtupsize = 0;	/* new tuple isn't needed */
		Batchhd.b_newtidsize = 0;	/* new tid isn't needed */

	  case mdREPL:
		break;

	  case mdAPP:
		Batchhd.b_oldtidsize = 0;	/* old tid isn't needed */
		break;

	  default:
		syserr("openbatch:mode %d", Batchhd.b_updtype);
	}
	/* if there are no secondary indices then tipn isn't needed, unless 
	** relation is ordered
	*/
	if (rel->d_r.r_indexed <= 0 && rel->d_r.r_dim <= 0)
		Batchhd.b_newtidsize = 0;

	/* if this relation has a secondary index or an ordered relation,
	** figure out what to save 
	*/
	if ((rel->d_r.r_indexed > 0 || rel->d_r.r_dim > 0) && mode != mdAPP) {
		if (rel->d_r.r_indexed > 0) {
			ingres_setkey(indx, (char *) &itup, rel->d_r.r_id, IRELIDP);
			ingres_setkey(indx, (char *) &itup, rel->d_r.r_owner, IOWNERP);
	
			if (find(indx, EXACTKEY, &lotid, &hitid, (char *) &itup))
				syserr("openbatch:bad find %.12s", rel);
	
			/* check each entry in "index" relation for useful index */
			while(!(i = get(indx, &lotid, &hitid, (char *) &itup, TRUE))) {
				if (!bequal(itup.i_relname, rel->d_r.r_id, MAX_NAME_SIZE) ||
				    !bequal(itup.i_owner, rel->d_r.r_owner, 2))
					continue;
				/* found one. copy the used domains */
				p = itup.i_dom;		/* get address of first */
				i = 6;
				while (i--) {
					if ((dom = *p++) == 0)
						break;	/* no more domains */
					sp = &Batchhd.b_domv[dom];
					if (sp->sd_domsize != 0)
						continue;	/* domain has already been done once */
					Batchhd.b_domc++;
					Batchhd.b_oldtupsize += rel->d_len[dom] & I1MASK;
					sp->sd_off = rel->d_off[dom];
					sp->sd_domsize = rel->d_len[dom] & I1MASK;
				}
			}
		}
		for (j = rel->d_r.r_dim - 1; j >= 0; --j) {
			/* point to lid field */
			sp = &Batchhd.b_domv[rel->d_r.r_attrc - j];
			Batchhd.b_domc++;
			Batchhd.b_oldtupsize += LIDSIZE;
			sp->sd_off = rel->d_off[rel->d_r.r_attrc - j];
			sp->sd_domsize = LIDSIZE;
		}
		if (i < 0)
			syserr("openbatch:bad get index %d", i);
		/* compute offsets of domains in saved "oldtuple" */
		saveoff = 0;
		sp = Batchhd.b_domv;
		i = Batchhd.b_domc;
		while (i--) {
			/* skip to next domain */
			while (sp->sd_domsize == 0)
				sp++;
			sp->sd_savedoff = saveoff;
			saveoff += sp->sd_domsize;
			sp++;
		}
	}
	wrbatch((char *) &Batchhd, sizeof(Batchhd));
	return (0);
}
/*
**  ADDBATCH -- add to batch file
*/
int
addbatch(tid_t *oldtid, char *newtuple, char *oldtuple)
{
	tid_t			newtid;
	register int		i;
	register struct si_doms	*sp;
	register char		*old;

#ifdef xATR1
	if (tTf(25,3))
		printf("addbatch\n");
#endif
	if (Batchhd.b_updtype == 0)
		return (-1);
	Batchhd.b_updc++;	/* increment the number of add batches */
	old = oldtuple;
	/* write out the old tid */
	wrbatch((char *) oldtid, Batchhd.b_oldtidsize);

	/* write out each of the old tuple domains */
	i = Batchhd.b_domc;	/* i get the number of domains */
	sp = Batchhd.b_domv;	/* sp points to the domain structures */

	while (i--) {
		/* skip to the next domain */
		while (sp->sd_domsize == 0)
			sp++;

		wrbatch(&old[sp->sd_off], sp->sd_domsize);
		sp++;
	}

	/* write out the new tuple */
	wrbatch(newtuple, Batchhd.b_newtupsize);

	/* reserve space for the new tid. Init to -1 */
	*((long *) &newtid) = -1;
	wrbatch((char *) &newtid, Batchhd.b_newtidsize);
	return (0);
}
/*
**  CLOSEBATCH -- close batch file
*/
int
closebatch(void)
{
	register int	i;

	if (Batchhd.b_updtype == 0)
		return (-1);
	flushbatch();	/* write out any remainder */
	if ((i = lseek(Batch_fp, (off_t) 0, 0)) == -1)
		syserr("closebatch:seek %d", i);
	wrbatch((char *) &Batchhd, sizeof(Batchhd));	/* update b_updc */
	flushbatch();
	if ((i = close(Batch_fp)) != 0)
		syserr("closebatch:close %d", i);
	Batchhd.b_updtype = 0;
	return (0);
}
