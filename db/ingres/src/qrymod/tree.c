#include <ingres.h>
#include <tree.h>
#include <symbol.h>
#include "qrymod.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)tree.c	8.1	12/31/84)

/*
**  TREE -- create new tree node.
**
**	This is a stripped down version of the same thing in the
**	parser.
**
**	It only knows about lengths of zero and two.
**
**	Parameters:
**		lptr -- the left pointer.
**		rptr -- the right pointer.
**		typ -- the node type.
**		len -- the node length.
**		value -- the node value.
**
**	Returns:
**		A pointer to the created node.
**
**	Side Effects:
**		Space is taken from Qbuf.
*/


qtree_t *
qm_tree(qtree_t *lptr, qtree_t *rptr, char typ, int len, int value)
{
	register qtree_t	*tptr;
	register int	l;

	l = len;

	tptr = (qtree_t *) need(Qbuf, l + QT_HDR_SIZ);
	tptr->left = lptr;
	tptr->right = rptr;
	tptr->sym.type = typ;
	tptr->sym.len = l;

	if (l > 0)
		tptr->sym.value.sym_data.i2type = value;
	return (tptr);
}
