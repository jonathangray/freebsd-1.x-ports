#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "globs.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)mklist.c	8.1	12/31/84)

/*
**  MKLIST
**
**	writes a list of query tree nodes in "OVQP order" --
**	that is, everything in postfix (endorder) except AND's and OR's
**	infixed (postorder) to OVQP.
**	called by call_ovqp().
*/


void
mklist(qtree_t *tree)
{
	register int	typ;
	register qtree_t 	*t;
	register int 	andor;

	t = tree;
	if (!t || (typ=t->sym.type)==TREE || typ==QLEND) 
		return;

	andor=0;
	mklist(t->left);
	if (typ==AND || typ==OR) {
		andor = 1;
		ovqpnod(t);
	}
	mklist(t->right);
	if (!andor)
		ovqpnod(t);
}
