#include <stdio.h>

#include <ingres.h>
#include <tree.h>
#include <symbol.h>
#include <pv.h>
#include "globs.h"
#include <errors.h>
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)ageval.c	8.3	12/18/85)

/*
**	Determine if an aggregate contains any
**	prime aggregates. Note that there might
**	be more than one aggregate.
*/
int
prime(qtree_t *aop)
{
	register qtree_t	*a;

	a = aop;
	do {
		switch (a->sym.value.sym_op.opno) {
		  case opCOUNTU:
		  case opSUMU:
		  case opAVGU:
			return (TRUE);
		}
	} while ((a = a->left) != 0);
	return (FALSE);
}

/*
**	Remove dups from an unopened relation
**	by calling heapsort
*/
void
removedups(int var)
{
	register char	*p;

	closer1(var);	/* guarantee that relation has been closed */
	initp();
	p = rangename(var);	/* get name of relation */
#ifdef xDTR1
	if (tTf(41, 1)) {
		printf("removing dups from %s\n", p);
	}
#endif
	setp(PV_STR, p, 0);
	setp(PV_STR,"heapsort", 0);
	setp(PV_STR,"num", 0);
	call_dbu(mdMODIFY, FALSE);
}

/*
**	AGEVAL -- evaluate simple aggregate.
**
**	Ageval is passed the tree of a simple aggregate,
**	and an array of space to store the results. The
**	amount of space actually allocated is stored in
**	(*result)->sym.len
**
**	If the aggregate is unique (eg. countu, sumu, avgu)
**	or if the aggregate is multi-variable, special
**	processing is done. A temporary relation is formed
**	with a result domain for each single agg in the tree.
**	Decomp is called to retrieve
**	the values to be aggregated into that relation.
**
**	If the aggregate is unique, then duplicates are
**	removed from the temporary relation.
**
**	Next the aggregate is run on either the original relation
**	or on the temporary relation.
**
**	Finally the result is read from OVQP and if a
**	temporary relation was used, it is destroyed.
**
**	Trace Flags:
**		41
*/
qtree_t *
ageval(qtree_t *tree, qtree_t **result)
{
	register qtree_t	*aghead, *resdom, *aop;
	qtree_t		*newtree;
	qtree_t		*lnodv[MAX_DOMAINS + 2];
	int		agbuf[1+AGBUFSIZ/sizeof(int)];
	int		temp_relnum, i;

#ifdef xDTR1
	if (tTf(41, 2)) {
		printf("entered ageval\n");
		treepr(tree);
	}
#endif

	aghead = tree;
	aop = aghead->left;
	temp_relnum = NORESULT;

	/* if PRIME or multi-var, form aggregate domain in temp relation */
	if (prime(aop) || aghead->sym.value.sym_root.tvarc > 1) {
		initbuf((char *)agbuf, AGBUFSIZ, AGBUFFULL, derror);

		lnodv[lnode(aop, lnodv, 0)] = 0;

		/* create new tree for retrieve and give it the qualification */
		newtree = makroot((char *)agbuf);
		newtree->right = aghead->right;
		aghead->right = De.de_qle;

		/* put a resdom on new tree for each aop in orig tree */
		/* make each aop in orig tree reference new relation */
		for (i = 0; (aop = lnodv[i]) != 0 ; ) {

			/* create resdom for new tree */
			resdom = makresdom((char *)agbuf, aop);
			resdom->sym.value.sym_resdom.resno = ++i;
			resdom->right = aop->right;

			/* connect it to newtree */
			resdom->left = newtree->left;
			newtree->left = resdom;

			/* make orig aop reference new relation */
			aop->right = makavar(resdom, FREEVAR, i);
		}

		/* make result relation */
		temp_relnum = mak_t_rel(newtree, "a", -1);

		/* prepare for query */
		mapvar(newtree, 0);
		decomp(newtree, mdRETR, temp_relnum);
		De.de_rangev[FREEVAR].relnum = temp_relnum;
		De.de_sourcevar = FREEVAR;

		/* if prime, remove dups */
		if (prime(aghead->left)) {
			/* modify to heapsort */
			removedups(FREEVAR);
		}

	}

	De.de_newq = 1;
	De.de_newr = TRUE;

	/* call ovqp with no result relation */
	call_ovqp(aghead, mdRETR, NORESULT);
	De.de_newq = 0;

	/* pick up results */
	readagg_result(result);

	/* if temp relation was created, destroy it */
	if (temp_relnum != NORESULT)
		dstr_rel(temp_relnum);

	return((qtree_t *) NULL);
}

