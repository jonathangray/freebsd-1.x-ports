#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "globs.h"
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)makenode.c	8.1	12/31/84)

/*
**	Make a copy of a tree.
*/

qtree_t *
copytree(register qtree_t *r, char *buf)
{
	register qtree_t	*q;
	register int	length;

	if (r == NULL)
		return (0);

	length = r->sym.len & I1MASK;
	q = (qtree_t *) need(buf, length + QT_HDR_SIZ);
	bmove((char *)&r->sym, (char *)&q->sym, length + SYM_HDR_SIZ);

	q->left = copytree(r->left, buf);
	q->right = copytree(r->right, buf);

	return (q);
}
/*
**	Make a new copy of the root by making
**	new AND nodes and connecting them to the
**	same branches.
**
**	Trace Flags:
**		64
*/

qtree_t *
copy_ands(qtree_t *root, char *buf)
{
	register qtree_t	*q;
	register qtree_t	*x, *y;
	qtree_t		*newroot;
	register int	len;

#ifdef xDTR1
	if (tTf(64, -1))
		printf("COPY_ANDS");
#endif
	newroot = (qtree_t *) need(buf, 0);
	y = 0;

	for (q=root; q->sym.type != QLEND; q=q->right) {
		len = q->sym.len & I1MASK;
		x = (qtree_t *) need(buf, len + QT_HDR_SIZ);
		x->left = q->left;
		bmove((char *)&q->sym, (char *)&x->sym, len + SYM_HDR_SIZ);
		if (y)
			y->right = x;
		y = x;
	}
	y->right = q;

#ifdef xDTR1
	if (tTf(64, 0)) {
		printf("New tree\n");
		treepr(newroot);
	}
#endif
	return(newroot);
}



qtree_t *
makroot(char *buf)
{
	register qtree_t *s;

	s = (qtree_t *) need(buf, QT_HDR_SIZ + sizeof(rootnode_t));
	s->right = De.de_qle;
	s->left = De.de_tr;
	s->sym.value.sym_root.rootuser = FALSE;
	s->sym.value.sym_root.lvarm = 0;
	s->sym.value.sym_root.rvarm = 0;
	s->sym.value.sym_root.tvarc = 0;
	s->sym.value.sym_root.lvarc = 0;
	s->sym.type = ROOT;
	s->sym.len = sizeof(struct rootnode);
	return (s);
}


qtree_t *
makresdom(char *buf, qtree_t *node)
{
	register qtree_t	*res, *n;

	n = node;
	res = (qtree_t *) need(buf, QT_HDR_SIZ + sizeof(resdomnode_t));
	res->sym.type = RESDOM;
	res->sym.len = sizeof(resdomnode_t);
	if (n->sym.type == AOP) {
		res->sym.value.sym_resdom.resfrmt = n->sym.value.sym_op.agfrmt;
		res->sym.value.sym_resdom.resfrml = n->sym.value.sym_op.agfrml;
	} else {
		res->sym.value.sym_resdom.resfrmt = n->sym.value.sym_var.varfrmt;
		res->sym.value.sym_resdom.resfrml = n->sym.value.sym_var.varfrml;
	}
	return (res);
}

qtree_t *
makavar(qtree_t *node, int varnum, int attnum)
{
	register qtree_t	*avar, *n;

	n = node;

	avar = (qtree_t *) need(De.de_qbuf, QT_HDR_SIZ + sizeof(varnode_t));
	avar->left = avar->right = NULL;
	avar->sym.value.sym_var.valptr = NULL;
	avar->sym.type = VAR;
	avar->sym.len = sizeof(struct varnode);
	avar->sym.value.sym_var.varfrmt = n->sym.value.sym_var.varfrmt;
	avar->sym.value.sym_var.varfrml = n->sym.value.sym_var.varfrml;
	avar->sym.value.sym_var.varno = varnum;
	avar->sym.value.sym_var.attno = attnum;
#ifdef xDTR1
	if (tTf(64, 3)) {
		printf("makavar: node=%p  ", n);
		nodepr(avar);
	}
#endif
	return(avar);
}
