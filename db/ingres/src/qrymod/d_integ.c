#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <tree.h>
#include <symbol.h>
#include <pv.h>
#include <resp.h>
#include <func.h>
#include "qrymod.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)d_integ.c	8.4	5/30/88)

/*
**  D_INTEG -- define integrity constraint
**
**	An integrity constraint (as partially defined by the last
**	tree defined by d_tree) is defined.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Activity in 'relation' and 'integrities' catalogs.
**
**	Trace Flags:
**		49
*/

extern desc_t	Intdes;
extern desc_t	Reldes;

int d_integ(int, paramv_t *);

extern short	tTqm[80];

func_t	DefIntFn = {
	"DINTEG",
	d_integ,
	null_fn,
	null_fn,
	NULL,
	0,
	tTqm,
	80,
	'Q',
	0
};

int
d_integ(int pc, paramv_t *pv)
{
	register int		i;
	register qtree_t	*t;		/* definition tree */
	struct integrity	inttup;
	struct tup_id		tid;
	register int		rv;		/* result variable */
	relation_t		relkey;
	relation_t		reltup;
	char			relid[MAX_NAME_SIZE];
	char			relowner[2];
	long			relstat;
	struct	qthdr		qt;
	resp_t			*rp;

	if (pv[0].pv_type != PV_QTREE) {
		syserr("d_integ: tree");
	}
	t = pv[0].pv_val.pv_qtree;
	rv = Qt.qt_resvar;

	/*
	**  Check for valid environment.
	**	The tree must exist, have a qualification, and have
	**	no target list.  The query mode must be mdINTEG.
	**
	**	User level stuff checks to see that this is single
	**	variable aggregate free, since that's all we know
	**	about thusfar.  Also, the relation in question must
	**	not be a view.
	*/

#ifdef xQTR3
	if (t == NULL) {
		syserr("d_integ: NULL tree");
	}
	if ((i = t->right->sym.type) != AND) {
		syserr("d_integ: qual %d", i);
	}
	if ((i = t->left->sym.type) != TREE) {
		syserr("d_integ: TL %d", i);
	}
	if (Qt.qt_qmode != mdINTEG) {
		syserr("d_integ: Qmode %d", Qt.qt_qmode);
	}
#endif
	
	/* check for aggregates */
	if (aggcheck(t)) {
		qmerror(NOAGGINT, -1, rv, 0);	/* aggregates in qual */
	}

	/* check for multi-variable */
	for (i = 0; i < MAX_RANGES; i++) {
		if (Qt.qt_rangev[i].rngvdesc == NULL) {
			continue;
		}
		if (i != rv) {
#ifdef xQTR3
			if (tTf(49, 1)) {
				printf("d_integ: Rv %d(%.14s) i %d(%.14s)\n",
				    rv, Qt.qt_rangev[rv].rngvdesc->d_r.r_id,
				    i, Qt.qt_rangev[i].rngvdesc->d_r.r_id);
			}
#endif
			qmerror(NOMULTIVAR, -1, rv, 0);	/* too many vars */
		}
	}


	/* check for the resultvariable being a real relation */
	if (BITISSET(S_VIEW, Qt.qt_rangev[rv].rngvdesc->d_r.r_status)) {
		qmerror(INTVIEW, -1, rv, 0);	/* is a view */
	}
	
	/* guarantee that you own this relation */
	if (!bequal(Usercode, Qt.qt_rangev[rv].rngvdesc->d_r.r_owner, USERCODE_SIZE)) {
		qmerror(MUSTOWN, -1, rv, 0);	/* don't own reln */
	}
	bmove(Qt.qt_rangev[rv].rngvdesc->d_r.r_id, relid, MAX_NAME_SIZE);
	bmove(Qt.qt_rangev[rv].rngvdesc->d_r.r_owner, relowner,
			sizeof(Qt.qt_rangev[rv].rngvdesc->d_r.r_owner));
	bmove(&Qt,&qt, sizeof(Qt));
	relstat = Qt.qt_rangev[rv].rngvdesc->d_r.r_status;

	/*
	**  Guarantee that the integrity constraint is true now.
	**	This involves issuing a retrieve statement for the
	**	inverse of the qualification.  The target list is
	**	already null, so we will get nothing printed out
	**	(only a return status).
	**
	**	We reset resp_tups if ok so that the user isn't annoyed
	**	by a tuple count.  On error, it is a count of the
	**	number of tuples that don't satisfy.
	*/

	Qt.qt_qmode = mdRETR;
	Qt.qt_resvar = -1;

	/* issue the invert of the query */
	issueinvert(t);
	rp = getresp();
	if (rp->resp_tups != 0) {
		qmerror(INITCONST, -1, rv, 0);	/* constraint not satisfied */
	}
	rp->resp_tups = -1;
	bmove(&qt,&Qt, sizeof(Qt));

	/*
	**  Set up the rest of the environment.
	*/

	opencatalog("integrities", OR_WRITE);
	clr_tuple(&Intdes, (char *) &inttup);
	Qt.qt_resvar = -1;
	Qt.qt_qmode = -1;

	/*
	**  Set up integrity relation tuple.
	**	The qualification will be scanned, and a set of
	**	domains referenced will be created.  Other stuff
	**	is filled in from the range table and from the
	**	parser.
	**
	**	The tree is actually inserted into the tree catalog
	**	in this step.  Extra information is cleared here.
	*/

	inttup.intresvar = rv;
	bmove(relid, inttup.intrelid, MAX_NAME_SIZE);
	bmove(relowner, inttup.intrelowner, sizeof(inttup.intrelowner));
	makeidset(rv, t, inttup.intdomset);
	inttup.inttree = puttree(t, inttup.intrelid, inttup.intrelowner, mdINTEG);

	/*
	**  Insert tuple into integrity catalog.
	*/

	i = insert(&Intdes, &tid, &inttup, FALSE);
	if (i < 0) {
		syserr("d_integ: insert");
	}
	if (noclose(&Intdes) != 0) {
		syserr("d_integ: noclose int");
	}

	/*
	**  Update r_status S_INTEG bit.
	*/

	if (!BITISSET(S_INTEG, relstat)) {
		opencatalog("relation", OR_WRITE);
		clearkeys(&Reldes);
		ingres_setkey(&Reldes, &relkey, inttup.intrelid, RELID);
		ingres_setkey(&Reldes, &relkey, inttup.intrelowner, RELOWNER);
		i = getequal(&Reldes, &relkey, &reltup, &tid);
		if (i != 0) {
			syserr("d_integ: geteq returns %d",i);
		}
		reltup.r_status |= S_INTEG;
		i = replace(&Reldes, &tid, &reltup, FALSE);
		if (i != 0) {
			syserr("d_integ: replace returns %d",i);
		}
		if (noclose(&Reldes) != 0) {
			syserr("d_integ: noclose rel");
		}
	}

	return (0);
}

void
makeidset(int varno, qtree_t *tree, short *dset)
{
	register int	vn;
	register qtree_t	*t;

	vn = varno;
	t = tree;

	while (t != (qtree_t *) NULL) {
		if (t->sym.type == VAR && t->sym.value.sym_var.varno == vn) {
			lsetbit(t->sym.value.sym_var.attno, dset);
		}
		
		/* handle left subtree recursively */
		makeidset(vn, t->left, dset);

		/* handle right subtree iteratively */
		t = t->right;
	}
}
