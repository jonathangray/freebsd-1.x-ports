#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)mapvar.c	8.1	12/31/84)

/*
**	MAPVAR -- construct variable maps for ROOT, AND, and AGHEAD nodes.
**	tl is a flag  which indicates if the target list should
**	be included in the mapping.  If tl = 0, it should; else it should not.
**
**	Trace Flags:
**		52
*/
int
mapvar(register qtree_t *t, int tl)
{
	register int	rmap, lmap;

	if (t == NULL) {
		return (0);
	}

#ifdef xDTR3
	if (tTf(52, 0)) {
		printf("mapvar(%p) %c\n", t, t->sym.type);
	}
#endif xDTR3

	switch (t->sym.type) {
	  case ROOT:
	  case AND:
	  case AGHEAD:
		/* map the right side */
		t->sym.value.sym_root.rvarm = rmap = mapvar(t->right, tl);

		/* map the left side or else use existing values */
		if (tl == 0) {
			t->sym.value.sym_root.lvarm = lmap = mapvar(t->left, tl);
			t->sym.value.sym_root.lvarc = bitcnt(lmap);
		} else {
			lmap = t->sym.value.sym_root.lvarm;
		}

		/* form map of both sides */
		rmap |= lmap;

		/* compute total var count */
		t->sym.value.sym_root.tvarc = bitcnt(rmap);

		return (rmap);

	  case VAR:
		if ((t = ckvar(t))->sym.value.sym_var.valptr) {
			return (0);	/* var is a constant */
		}
		return (01 << t->sym.value.sym_var.varno);
	}

	/* node is not a VAR, AND, ROOT, or AGHEAD */
	return (mapvar(t->left, tl) | mapvar(t->right, tl));
}
