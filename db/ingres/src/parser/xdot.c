#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "parser.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)xdot.c	8.1	12/31/84)

/*
** XDOT
**	add to attribute stash any missing attributes in the
**	source relation and then build tree with all attribs
**	in the 'a_id' order.  This algorithm assumes that
**	the function 'attadd' insert attributes into the list
**	in 'a_id' order from 1 -> N.
*/
qtree_t *
xdot(int slot)
{
	PARRNG				*rptr;
	attr_t		tuple;
	register attr_t	*ktuple;
	attr_t		ktup;
	tid_t				tid;
	tid_t				limtid;
	qtree_t		*tempt;
	register qtree_t	*vnode;
	int				ik;
	register att_ent_t		*aptr;

	extern PARRNG			Parrng[];
	extern char			*Trname;
	extern desc_t			Attdes;

	rptr = &Parrng[slot];

#ifdef	xPTR2
	tTfp(35, 0, "ALL being processed for %12s\n",
	    rptr->vardesc.d_rangevar);
#endif

	if (rptr->vardesc.d_r.r_attrc <= 0)
		syserr("xdot: rptr->vardesc.d_r.r_attrc %d.\n", rptr->vardesc.d_r.r_attrc);
	/* if attstash is missing any attribs then fill in list */
	if (rptr->vardesc.d_r.r_attrc != attcount(slot)) {
		/* get all entries in attrib relation */
		clearkeys(&Attdes);
		ktuple = &ktup;
		ingres_setkey(&Attdes, ktuple, rptr->vardesc.d_r.r_id, ATTRELID);
		ingres_setkey(&Attdes, ktuple, rptr->vardesc.d_r.r_owner, ATTOWNER);
		if ((ik = find(&Attdes, EXACTKEY, &tid, &limtid, ktuple)) != 0)
			syserr("bad find in xdot '%d'", ik);
		while (!get(&Attdes, &tid, &limtid, &tuple, 1))
			if (!kcompare(&Attdes, &tuple, ktuple))
				/* add any that are not in the stash */
				if (!attfind(slot, tuple.a_name))
					attadd(slot, &tuple);
	}

	/* build tree for ALL */
	tempt = NULL;
	aptr = rptr->attlist;
	while (aptr != 0) {
		vnode = par_tree(NULL, NULL, VAR, sizeof(varnode_t), slot, aptr);
		Trname = aptr->atbname;
		tempt = addresdom(tempt, vnode);
		aptr = aptr->atbnext;
	}

#ifdef	xPTR3
	tTfp(35, 0, "end of xdot %12s\n", rptr->vardesc.d_rangevar);
#endif

	return(tempt);
}
