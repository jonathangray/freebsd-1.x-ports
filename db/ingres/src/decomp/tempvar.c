#include <stdio.h>

#include <ingres.h>
#include <tree.h>
#include <symbol.h>
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)tempvar.c	8.1	12/31/84)


/*
** TEMPVAR
**
**	This file contains routines associated with redefining
**	attribute numbers. This is needed when one variable sub queries
**	or reduction change the positions of attributes in a relation.
**	This file includes:
**
**	Tempvar -- Change the attribute numbers to new ones.
**
**	Origvar -- Restore attribute numbers back to their previous values.
**
**	Ckvar   -- Return the currently active VAR node
**
**	Trace Flags:
**		51
*/

/*
** Ckvar -- Return pointer to currently "active" VAR.
**
**	This routine guarantees that "t" will point to
**	the most current definition of the VAR.
*/
qtree_t *
ckvar(register qtree_t *t)
{
	if (t->sym.type != VAR) {
		syserr("ckvar: not a VAR %d", t->sym.type);
	}
#ifdef xDTR1
	if (tTf(51, 2)) {
		printf("ckvar: var %d.%d, type ",
		        t->sym.value.sym_var.varno, t->sym.value.sym_var.attno);
		xputchar(t->sym.value.sym_var.varfrmt);
		printf("%3d\n", t->sym.value.sym_var.varfrml);
	}
#endif
	while (t != (qtree_t *) NULL && t->sym.value.sym_var.varno < 0) {
		t = (qtree_t *) t->sym.value.sym_var.valptr;
	}
	if (t == (qtree_t *) NULL) {
		syserr("ckvar null valptr");
	}
	return (t);
}
/*
** Tempvar -- Replace a VAR attribute number with its new number.
**
**	Tempvar is given a list of subqueries which will potentially
**	alter the attribute numbers of VARs they reference. An attno
**	is changed by making the current VAR point to a new VAR node
**	which has the updated attno.
**
**	The new attno is determined from the target list of the subquery
**	for that VAR. The RESDOM number is the new attno and the VAR it
**	points to is the old attno. For example:
**		RESDOM/2 -> right = VAR 1/3
**	The right subtree of result domain 2 is domain 3 of variable 1.
**	Thus domain 3 should be renumbered to be domain 2.
*/
void
tempvar(register qtree_t *node, qtree_t **sqlist, char *buf)
{
	register qtree_t	*v, *sq;

	if (node == (qtree_t *) NULL) {
		return;
	}
	if (node->sym.type == VAR ) {
		node = ckvar(node);
		if ((sq = sqlist[node->sym.value.sym_var.varno]) != 0) {
			/* This var has a subquery on it */

			/* allocate a new VAR node */
			if (buf) {
				node->sym.value.sym_var.valptr = (ANYTYPE *) need(buf, QT_HDR_SIZ + sizeof(varnode_t));
				v = (qtree_t *) node->sym.value.sym_var.valptr;
				bmove(&node->sym, &v->sym, SYM_HDR_SIZ + sizeof(struct varnode) - sizeof(v));
				v->left = v->right = NULL;
				v->sym.value.sym_var.valptr = NULL;
				node->sym.value.sym_var.varno = -1;
			} else {
				v = node;
			}

			/* search for the new attno */
			for (sq = sq->left; sq->sym.type != TREE; sq = sq->left) {
				if (ckvar(sq->right)->sym.value.sym_var.attno == node->sym.value.sym_var.attno) {
	
					v->sym.value.sym_var.attno = sq->sym.value.sym_resdom.resno;
#ifdef xDTR1
					if (tTf(51, 3)) {
						printf("Tempvar:");
						nodepr(node);
					}
#endif

					return;
				}
			}
			syserr("tempvar:dom %d of %s missing", node->sym.value.sym_var.attno, rangename(node->sym.value.sym_var.varno));
		}
		return;
	}
	tempvar(node->left, sqlist, buf);
	tempvar(node->right, sqlist, buf);
}

/*
** Origvar -- Restore VAR node to previous state.
**
**	Origvar undoes the effect of tempvar. All vars listed
**	in the sqlist will have their most recent tempvar removed.
*/
void
origvar(register qtree_t *t, qtree_t **sqlist)
{
	register char	v;
	register qtree_t	*q;

	if (t == (qtree_t *) NULL) {
		return;
	}
	if (t->sym.type == VAR && t->sym.value.sym_var.varno < 0) {
		while ((v = (q = (qtree_t *) (t->sym.value.sym_var.valptr))->sym.value.sym_var.varno) < 0) {
			t = q;
		}
		if (sqlist[v]) {
			t->sym.value.sym_var.varno = v;
			t->sym.value.sym_var.valptr = NULL;
		}
		return;
	}
	origvar(t->left, sqlist);
	origvar(t->right, sqlist);
}
