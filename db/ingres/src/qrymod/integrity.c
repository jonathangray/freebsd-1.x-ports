#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <access.h>
#include <tree.h>
#include <symbol.h>
#include "qrymod.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)integrity.c	8.4	5/30/88)

/*
**  INTEGRITY.C -- Integrity Constraint Processor
**
**	This module contains the integrity constraint processor.  This
**	processor modifies the query to add integrity constraints.
**
**	Currently only single-variable aggregate-free constraints are
**	handled.  Thus the algorithm is reduced to scanning the tree
**	for each variable modified and appending the constraints for
**	that variable to the tree.
**
**	Parameters:
**		none
**
**	Returns:
**		The root of the modified tree.
**
**	Side Effects:
**		Much relation I/O.
**		Modifies the tree in place, so the previous one is
**			lost.
**
**	Trace Flags:
**		40 -> 49
*/

extern desc_t	Intdes;

#ifdef xQTR1
void
treeprn(qtree_t *p, char *str)
{
	printf("%s\n", str);
	treepr(p);
}
#endif

qtree_t *
integrity(qtree_t *root)
{
	register qtree_t		*r;
	short			dset[8];
	register qtree_t		*p;
	register int		i;
	qtree_t		*iqual;
	struct integrity	inttup, intkey;
	struct tup_id		hitid, lotid;

#ifdef xQTR1
	tTfp(40, -1, "\n->INTEGRITY\n");
#endif

	r = root;

	/*
	**  Check to see if we should apply the integrity
	**  algorithm.
	**
	**  This means checking to insure that we have an update
	**  and seeing if any integrity constraints apply.
	*/

	if (Qt.qt_qmode == mdRETR || Qt.qt_qmode == mdRET_UNI ||
	    (Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_status & S_INTEG) == 0) {
#ifdef xQTR2
		tTfp(40, 0, "->INTEGRITY: no integ\n");
#endif
		return(r);
	}

	/*
	**  Create a set of the domains updated in this query.
	*/

	for (i = 0; i < 8; i++)
		dset[i] = 0;
	for (p = r->left; p != NULL && p->sym.type != TREE; p = p->left) {
#ifdef xQTR3
		if (p->sym.type != RESDOM)
			syserr("integrity: RESDOM %d", p->sym.type);
#endif
		lsetbit(p->sym.value.sym_resdom.resno, dset);
	}

#ifdef xQTR3
	if (p == NULL)
		syserr("integrity: NULL LHS");
#endif
#ifdef xQTR1
	if (tTf(40, 1))
		pr_set(dset, "dset");
#endif
	
	/*
	**  Scan integrity catalog for possible tuples.  If found,
	**  include them in the integrity qualification.
	*/

	iqual = NULL;
	opencatalog("integrities", OR_READ);
	ingres_setkey(&Intdes, &intkey, Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_id, INTRELID);
	ingres_setkey(&Intdes, &intkey, Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_owner, INTRELOWNER);
	find(&Intdes, EXACTKEY, &lotid, &hitid, &intkey);

	while ((i = get(&Intdes, &lotid, &hitid, &inttup, TRUE)) == 0) {
		if (kcompare(&Intdes, &intkey, &inttup) != 0)
			continue;

#ifdef xQTR1
		if (tTf(40, 2)) {
			printup(&Intdes, &inttup);
			pr_set(inttup.intdomset, "tset");
		}
#endif
		
		/* check for some domain set overlap */
		for (i = 0; i < 8; i++)
			if ((dset[i] & inttup.intdomset[i]) != 0)
				break;

		if (i >= 8)
			continue;
		
		/* some domain matches, include in integrity qual */
		i = Qt.qt_resvar;
		p = gettree(Qt.qt_rangev[i].rngvdesc->d_r.r_id,
			    Qt.qt_rangev[i].rngvdesc->d_r.r_owner,
			    mdINTEG, inttup.inttree, FALSE);
#ifdef xQTR1
		if (tTf(40, 3))
			treeprn(p, "int_qual");
#endif

		/* trim off (null) target list */
		p = p->right;

		/* merge the 'integrity' var into the Qt.qt_resvar */
		i = inttup.intresvar;
		if (Qt.qt_remap[i] >= 0)
			i = Qt.qt_remap[i];
		mergevar(i, Qt.qt_resvar, p);

		/* add to integrity qual */
		if (iqual == NULL)
			iqual = p;
		else
			appqual(p, iqual);
	}
	if (i < 0)
		syserr("integrity: get %d", i);
	
	/*
	**  Clean up the integrity qualification so that it will merge
	**  nicely into the tree, and then append it to the user's
	**  qualification.
	*/

	if (iqual != NULL) {
		/* replace VAR nodes by corresponding user afcn */
		subsvars(&iqual, Qt.qt_resvar, r->left, Qt.qt_qmode);

		/* append to tree and normalize */
		appqual(iqual, r);
#ifdef xQTR3
		if (tTf(40, 8))
			treeprn(r, "Unnormalized tree");
#endif
		r->right = norml(trimqlend(r->right));
	}

#ifdef xQTR1
	if (tTf(40, 15))
		treeprn(r, "INTEGRITY->");
#endif

	return (r);
}
