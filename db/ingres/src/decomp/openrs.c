#include <stdio.h>
#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "globs.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)openrs.c	8.2	1/15/85)


/* Defined constants for dtmode field above */
#define	DTALLOC		0	/* descriptor allocated */
#define	DTREL		1	/* has been openr'd -1 */
#define	DTATTS		2	/* has rel+atts but not opened */
#define	DTREAD		3	/* currently open for reading */
#define	DTWRITE		4	/* currently open for writing */



/* Allocation of descriptors */

/* Globals which count #files open and maximum # of files which can be open */


/*
** OPENRS -- routines associated with maintaining the range table for decomp
**
**	openrs(root) -- fill range table info about each relation.
**
**	closers() -- close all variables in range table.
**
**	openr1(varno) -- fill range table for a particular relation.
**
**	closer1(varno) -- close a particular relation.
**
**	readopen(varno) -- open a variable for reading. returns descriptor.
**
**	writeopen(varno) -- open a variable for writing. returns descriptor.
**
**	initdesc()	-- initialize the descriptor cache.
**
**	reldescrip(varno) -- returns descriptor for var (has rel/atts
**		but might not be open).
**
**	desc_get(relnum, flag) -- finds a desc_tab & alloctes it for relnum.
**
**	desc_lru()  -- returns least recently used desc_tab.
**
**	desc_top(desc_tab) -- makes desc_tab most recently used.
**
**	desc_last(desc_tab) -- makes desc_tab the least recently used.
**
**	Trace Flags:
**		62
*/
/*
** Initdesc -- initialize descriptors for range table
*/
void
initdesc(int mode)
{
	register struct desc_tab	*dt;
	register int			i;


	for (dt = De.de_desc, i = 0; dt <= &De.de_desc[MAXRELN - 1]; dt++, i++) {
		dt->dtmode = DTALLOC;
		dt->relnum = -2;	/* unused relnum value */
		dt->dtpos = i;		/* lru order */
	}

	/*
	** Determine number of available file descriptors.
	**	av_files gives number of files that are def. open
	**	for users.  if we will need to open a batch file,
	**	get rid of that also.
	*/

	De.de_dfiles = av_files();
	if (mode != mdRETR)
		De.de_dfiles--;
	De.de_dopnfiles = 0;
}

/*
**	Openrs -- open source relations for query. Fill values
**		in range table.
*/
void
openrs(qtree_t *root)
{
	register qtree_t	*r;
	register int	map, i;

	r = root;
	map = r->sym.value.sym_root.lvarm | r->sym.value.sym_root.rvarm;

#ifdef xDTR1
	if (tTf(62, 0))
		printf("OPENRS-root:%p,map:%o\n", r, map);
#endif

	for (i = 0; i < MAX_RANGES; i++)
		if (map & (01 << i))
			openr1(i);

}

/*
**	For text space reasons only, the close relation routine varies
**	between decomp and decomp70. In decomp, the relation is opened
**	only for reading and never for writing thus inpcloser() can be
**	called. For decomp70 closer() must be called. If there were no
**	text space shortage, then closer() could always be called.
**	The routine init_decomp() assigned the value to Des_closefunc.
*/

extern int	(*Des_closefunc)();	/* either &inpcloser or &closer */

void
desc_close(struct desc_tab *dt1)
{
	register struct desc_tab	*dt;
	register int			i;

	dt = dt1;

	if (dt->dtmode == DTREAD || dt->dtmode == DTWRITE) {
		if ((i = (*Des_closefunc)(&dt->desc)) != 0)
			syserr("desc_close:closer %d,%.12s", i, dt->desc.d_r.r_id);
		De.de_dopnfiles--;
		dt->dtmode = DTATTS;
	}
}

/*
**	Close all open relations.
**	If any relations were created but never
**	opened, destroy them. The only
**	situation under which that can occur
**	is when a rub-out occurs at an
**	in opportune moment or when an error
**	occurs in ovqp.
*/
void
closers(void)
{
	register int			i;
	register struct desc_tab	*dt;
	bool				dstr_flag;


	for (dt = De.de_desc; dt <= &De.de_desc[MAXRELN - 1]; dt++)
		desc_close(dt);

	/* destroy any temps */
	initp();	/* init parameters vector for destroys */
	dstr_flag = FALSE;
	while ((i = rnum_last()) != 0) {
		dstr_flag |= dstr_mark(i); /* indicate that there are relations to be destroyed */
	}

	if (dstr_flag)
		call_dbu(mdDESTROY, TRUE);
	else
		resetp();
}

/*
** Desc_lru -- return least recently used descriptor
*/

struct desc_tab *
desc_lru(void)
{
	register struct desc_tab	*dx;

	for (dx = De.de_desc; dx <= &De.de_desc[MAXRELN-1]; dx++) {
		if (dx->dtpos == MAXRELN - 1)
			return (dx);
	}
	syserr("desc_lru:no lru");
	/*NOTREACHED*/
	return((struct desc_tab *) NULL);
}

/*
** Desc_top -- make the desc_tab entry "dtx" the most recently used.
*/

void
desc_top(struct desc_tab *dt1)
{
	register struct desc_tab	*dt, *dx;
	register int			oldpos;

	dt = dt1;

	if ((oldpos = dt->dtpos) != 0) {
		/* descriptor isn't currently top */
		for (dx = De.de_desc; dx <= &De.de_desc[MAXRELN-1]; dx++)
			if (dx->dtpos < oldpos)
				dx->dtpos++;

		/* make descriptor first */
		dt->dtpos = 0;
	}
}
/*
**  DESC_GET
*/

struct desc_tab *
desc_get(int relnum, bool flag)
{
	register struct desc_tab	*dt, *ret;

	ret = NULL;

	/* search for one currently allocated */
	for (dt = &De.de_desc[0]; dt <= &De.de_desc[MAXRELN-1]; dt++) {
		if (dt->relnum == relnum) {
			ret = dt;
#ifdef xDTR1
			if (tTf(62, 3))
				printf("found desc for %d\n", relnum);
#endif
			break;
		}
	}

	if (ret == NULL && flag) {
		/* get a victim and deallocate desc */
		ret = desc_lru();

		/* deallocate */
#ifdef xDTR1
		if (tTf(62, 5))
			printf("trading %d for %d\n", ret->relnum, relnum);
#endif
		desc_close(ret);

		/* allocate */
		ret->relnum = relnum;
		ret->dtmode = DTALLOC;
	}

	if (ret != NULL)
		desc_top(ret);

	return (ret);
}

/*
** Desc_last -- make the desc_tab entry "dt" the least recently used.
*/

struct desc_tab *
desc_last(register struct desc_tab *dt)
{
	register int			oldpos;
	register struct desc_tab	*dx;

	oldpos = dt->dtpos;
	for (dx = De.de_desc; dx <= &De.de_desc[MAXRELN-1]; dx++)
		if (dx->dtpos > oldpos)
			dx->dtpos--;

	/* make descriptor last */
	dt->dtpos = MAXRELN - 1;
	return((struct desc_tab *) NULL);
}

/*
**	Openr1 -- open relation to get relation relation tuple
**
**	This will not open the relation for reading -- only
**	for getting the first part of the descriptor filled
*/

desc_t *
openr1(int var)
{
	register struct desc_tab	*dt;
	register struct rang_tab	*rp;
	register desc_t	*d;
	int				i;

	rp = &De.de_rangev[var];

#ifdef xDTR1
	if (tTf(62, 2))
		printf("openr1: var %d (%s)\t", var, rnum_convert(rp->relnum));
#endif

	dt = desc_get(rp->relnum, TRUE);

	if (dt->dtmode == DTALLOC) {
		if ((i = openr(&dt->desc, OR_RELTID, rnum_convert(rp->relnum))) != 0)
			syserr("openr1 open %d %s", i, rnum_convert(rp->relnum));
		dt->dtmode = DTREL;
	}

#ifdef xDTR1
	if (tTf(62, 2))
		printf("tups=%ld\n", dt->desc.d_r.r_tupc);
#endif

	d = &dt->desc;

	rp->rtspec = d->d_r.r_spec;
	rp->rtstat = d->d_r.r_status;
	rp->rtwid = d->d_r.r_width;
	rp->rtcnt = d->d_r.r_tupc;

	return (d);
}

/*
**  CLOSER1
*/
void
closer1(int var)
{
	register struct desc_tab	*dt;
	register struct rang_tab	*rp;
	register int			i;

	i = var;
	rp = &De.de_rangev[i];

#ifdef xDTR1
	if (tTf(62, 4))
		printf("closer1:var %d (%s)\n", i, rnum_convert(rp->relnum));
#endif
	if ((dt = desc_get(rp->relnum, FALSE)) != 0) {

		/* currently a descriptor for rel */
		desc_close(dt);

		dt->relnum = -2;
		desc_last(dt);

	}
}

void
desc_victim(void)
{
	register struct desc_tab	*dt, *old;

	old = NULL;
	for (dt = &De.de_desc[0]; dt <= &De.de_desc[MAXRELN-1]; dt++) {
		if (dt->dtmode == DTWRITE || dt->dtmode == DTREAD) {
			if (old == NULL || dt->dtpos > old->dtpos)
				old = dt;
		}
	}

	if (old == NULL)
		syserr("desc_victim:no victum %d,%d", De.de_dopnfiles, De.de_dfiles);
	desc_close(old);
}

/*
**	Openup -- make sure that the given descriptor is open
**		suitably for reading or writing.
*/
void
openup(struct desc_tab *dt1, int varno, int mode)
{
	register struct desc_tab	*dt;
	register int			md, openmd;
	int				i;
	char				rnam_tmp[MAX_NAME_SIZE+3];

	openmd = 0;
	/* quick check to handle typical case of rel being already open */
	md = mode;
	dt = dt1;
	if ((md != OR_WRITE && dt->dtmode == DTREAD) || dt->dtmode == DTWRITE)
		return;

	/* relation not opened correctly */
	switch (dt->dtmode) {

	  case DTALLOC:
		/*
		** Descriptor allocated but nothing else. If this
		** is for a variable then use openr1 to get range table
		** info. Else open directly.
		*/
		if (varno < 0) {
			/* open unassociated with a range table variable */
			openmd = md ? OR_WRITE : OR_READ;
			bmove(rnum_convert(dt->relnum), dt->desc.d_r.r_id, MAX_NAME_SIZE);
			break;
		}

		/* open for range table variable */
		openr1(varno);

		/* now fall through to DTREL case */

	  case DTREL:
		/* relation relation tuple present but nothing else */
		openmd = md ? OR_AWRITE : OR_AREAD;	/* open AREAD for read, AWRITE for write */
		break;

	  case DTATTS:
		/* relation & attributes filled but relation closed */
		openmd = md ? OR_REWRITE : OR_REREAD;
		break;
	  case DTREAD:
		/* relation open for reading but we need to write */
		desc_close(dt);

		openmd = OR_REWRITE;
		break;

	  default:
		syserr("openup:bad md %d", dt->dtmode);
	}

	/* close a previous file if necessary */
	if (De.de_dopnfiles == De.de_dfiles)
		desc_victim();	/* close oldest file */

	/* now open relation */
	bmove(dt->desc.d_r.r_id, rnam_tmp, MAX_NAME_SIZE + 3);
	if ((i = openr(&dt->desc, openmd, rnam_tmp)) != 0)
		syserr("openup:openr %d,%d,%.12s,%s", i, openmd, rnam_tmp, rnum_convert(dt->relnum));
	De.de_dopnfiles++;

	/* update mode of descriptor */
	dt->dtmode = md ? DTWRITE : DTREAD;
}

/*
**  READOPEN
*/

desc_t *
readopen(int var)
{
	register struct desc_tab	*dt;

	/* get descv for the relation */
	dt = desc_get(De.de_rangev[var].relnum, TRUE);

	if (!(dt->dtmode == DTREAD || dt->dtmode == DTWRITE)) {
		/* not open for reading or writing */
		openup(dt, var, OR_READ);	/* open for reading */
	}

	return (&dt->desc);
}

/*
**  WRITEOPEN
*/

desc_t *
writeopen(int var)
{
	register struct desc_tab	*dt;

	/* get descv for the relation */
	dt = desc_get(De.de_rangev[var].relnum, TRUE);

	if (dt->dtmode != DTWRITE) {
		/* not open for writing */
		openup(dt, var, OR_WRITE);	/* open for reading */
	}

	return (&dt->desc);
}

/*
**  SPECOPEN -- open for writing not associated with any variable
*/

desc_t *
specopen(int relnum)
{
	register struct desc_tab	*dt;

	dt = desc_get(relnum, TRUE);

	if (dt->dtmode != DTWRITE)
		openup(dt, -1, OR_WRITE);

	return (&dt->desc);
}

/*
**  SPECCLOSE
*/
void
specclose(int relnum)
{
	register struct desc_tab	*dt;

	if ((dt = desc_get(relnum, FALSE)) != 0) {
		desc_close(dt);
		desc_last(dt);
		dt->relnum = -2;
	}
}

