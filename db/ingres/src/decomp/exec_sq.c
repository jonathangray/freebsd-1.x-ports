#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include <pv.h>
#include "globs.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)exec_sq.c	8.1	12/31/84)

/*
** DECOMP2 -- Routines for executing detached sub-queries appearing
**	in sqlist. These routines include:
**
**	exec_sq -- execute sub-queries and update range table.
**
**	undo_sq -- restore range table and destroy temp rels.
**
**	reset_sq - restore range table and reset temp rels.
**
**	execsq1 -- call ovqp with subquery.
*/
/*
**  EXEC_SQ
**
**	Execute the subqueries in sqlist. Associated with
**	each sub-query is a relation number stored in sqrange.
**	If the sub-query has a non-null target list, the range
**	table is updated to reflect the new range of the relation.
**
**	If any sub-query is false, all subsequent ones are ignored
**	by ovqp and exec_sq returns the var number of the false subquery.
**
**	As a side effect, "disj" is incremented for each disjoint sub-query
**
**	Trace Flags:
**		35
*/
int
exec_sq(qtree_t **sqlist, int *sqrange, int *disj)
{
	register qtree_t	*sq;
	register int	i, qualfound;

#ifdef xDTR1
	if (tTf(35, 0))
		printf("EXEC_SQ--\n");
#endif

	*disj = 0;

	for (i = 0; i < MAX_RANGES; i++) {
		if ((sq = sqlist[i]) != 0) {
#ifdef xDTR1
			if (tTf(35, 1))
				printf("sq[%d]=%p\n", i, sq);
#endif
			qualfound = execsq1(sq, i, sqrange[i]);

#ifdef xDTR1
			if (tTf(35, 2))
				printf("qualfound=%d\n", qualfound);
#endif
			if (!qualfound) {
				return(i);
			}
			if (sq->left->sym.type != TREE) {
				/*
				** Update the range table and open
				** the relation's restricted replacement.
				*/
				new_range(i, sqrange[i]);
				openr1(i);
			} else {
				(*disj)++;
			}
		}
	}
	return (-1);
}
/*
**  UNDO_SQ
**
**	Undo the effects of one variable detachment on
**	the range table. The two parameters "limit" and
**	"maxlimit" describe how far down the list of
**	subqueries were processed.  Maxlimit represents
**	the furthest every attained and limit represents
**	the last variable processed the last time.
**
**	Trace Flags:
**		36
*/
void
undo_sq(qtree_t **sqlist, int *locrang, int *sqrange, int limit, int maxlimit, int reopen)
{
	register qtree_t	*sq;
	register int	i, lim;
	bool		dstr_flag;

	dstr_flag = 0;
#ifdef xDTR1
	if (tTf(36, 0))
		printf("UNDO_SQ--\n");
#endif

	initp();	/* setup parm vector for destroys */
	lim = limit == -1 ? MAX_RANGES : limit;
	if (maxlimit == -1)
		maxlimit = MAX_RANGES;

	for (i = 0; i < MAX_RANGES; i++)
		if ((sq = sqlist[i]) != 0) {
			if (sq->left->sym.type != TREE) {
				if (i < lim) {
					/* The query was run. Close the temp rel */
					closer1(i);
				}

				/* mark the temporary to be destroyed */
				dstr_mark(sqrange[i]);
				dstr_flag = TRUE;

				/* reopen the original relation. If maxlimit
				** never reached the variable "i" then the
				** original relation was never closed and thus
				** doesn't need to be reopened.
				*/
				rstrang(locrang, i);
				if (reopen && i < maxlimit)
					openr1(i);
			}
		}
	/* Only call destroy if there's something to destroy */
	if (dstr_flag)
		call_dbu(mdDESTROY, FALSE);
	else
		resetp();

}

/*
** Execsq1 -- call ovqp with mdRETR on temp relation
*/
int
execsq1(qtree_t *sq, int var, int relnum)
{
	register int	qualfound;

	De.de_sourcevar = var;
	De.de_newq = 1;
	qualfound = call_ovqp(sq, mdRETR, relnum);
	return (qualfound);
}

/*
**	Reset each relation until limit.
**	Reset will remove all tuples from the
**	relation but not destroy the relation.
**	The descriptor for the relation will be removed
**	from the cache.
**
**	The original relation is returned to
**	the range table.
**
**	If limit is -1 then all relations are done.
*/
void
reset_sq(qtree_t **sqlist, int *locrang, int limit)
{
	register qtree_t	*sq;
	register int	i, lim;
	int		old, reset;

	lim = limit == -1 ? MAX_RANGES : limit;
	reset = FALSE;
	initp();

	for (i = 0; i < lim; i++)
		if ((sq = sqlist[i]) && sq->left->sym.type != TREE) {
			old = new_range(i, locrang[i]);
			setp(PV_STR, rnum_convert(old), 0);
			specclose(old);
			reset = TRUE;
		}

	if (reset) {
		/*
		** Guarantee that OVQP will not reuse old
		** page of relation being reset
		*/
		De.de_newr = TRUE;
		call_dbu(mdRESETREL, FALSE);
	}
	else
		resetp();
}
