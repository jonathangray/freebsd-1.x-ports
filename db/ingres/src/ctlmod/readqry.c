#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "ctlmod.h"
#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)readqry.c	8.3	12/8/85)

/*
** READQRY
**
** 	Reads in query symbols from input pipe into core
**	locations and sets up information needed for later 
**	processing.
**
**	Returns ptr to root of querytree
**
**	Locbuf is a 'srcid_t' since that is the largest node of
**	a QMODE, SOURCEID, or RESULTVAR node.
*/

qtree_t *
readqry(int (*rdfn)(pb_t *s, char *nam, int cc), int fnparam, bool initialize)
/* int	(*rdfn)();	tree read function */
/* int	fnparam;	parameter to pass to rdfn */
/* bool	initialize;	if set, initialize Qbuf */
{
	register qtree_t	*q;
	register qtree_t	*rtval;
	register desc_t	*d;
	int		mark;
	int		i;
	int		j;

#ifdef xCTR1
	if (tTf(10, 8)) {
		printf("READQRY:\n");
	}
#endif

	if (initialize) {
		/* initialize for new query block */
		clrrange();
		Qt.qt_resvar = -1;
		Qt.qt_qmode = -1;
	}
	for (i = 0; i < MAX_RANGES; i++) {
		Qt.qt_remap[i] = i;
	}

	mark = markbuf(Qbuf);

	/* read symbols from input */
	for (;;) {
		freebuf(Qbuf, mark);
		q = readsym(rdfn, (char *) fnparam);
		switch (q->sym.type) {
		  case QMODE:
			if (Qt.qt_qmode != -1) {
				syserr("readqry: two qmodes");
			}
			Qt.qt_qmode = q->sym.value.sym_data.i2type;
			break;

		  case RESULTVAR:
			if (Qt.qt_resvar != -1) {
				syserr("readqry: two resultvars");
			}
			Qt.qt_resvar = q->sym.value.sym_data.i2type;
			break;

		  case SOURCEID:
			d = (desc_t *) xalloc(sizeof(*d), 0, 1);
			bmove((char *) &q->sym.value.sym_srcid.srcdesc, (char *) d, sizeof(*d));
			i = q->sym.value.sym_srcid.srcvar;
			j = declare(i, d);
			if (j != i && initialize) {
				syserr("readqry: declare(%d)=%d", i, j);
			}
			Qt.qt_remap[i] = j;
			break;

		  case TREE:	/* beginning of tree, no more other stuff */
			q = readtree(q, rdfn, (char *) fnparam);
			rtval = trbuild(q);
			if (rtval == NULL) {
				error(STACKFULL, 0);
			}
			return (rtval);

		  default:
			syserr("readqry: bad symbol %d", q->sym.type);
		}
	}
}
/*
**  READSYM
**	reads in one symbol from pipe into symbol struct.
*/

qtree_t *
readsym(int (*rdfn)(pb_t *s, char *nam, int cc), char *fnparam)
{
	register int	len;
	register int	t;
	register qtree_t	*q;
	int		rlen;

	q = (qtree_t *) need(Qbuf, QT_HDR_SIZ);
	if ((*rdfn)((pb_t *) fnparam, (char *) &q->sym, TYP_LEN_SIZ) < TYP_LEN_SIZ) {
		syserr("readsym: read sym");
	}
	rlen = len = q->sym.len & I1MASK;
	t = q->sym.type;

	switch (t) {
	  case AND:
		if (len < 6) {
			len = 6;
		}
		break;

	  case ROOT:
	  case AGHEAD:
		if (len < 8) {
			len = 8;
		}
		break;
	}

	q->sym.len = len;


	if (len != 0) {
		/* this will be contiguous with above need call */
		need(Qbuf, len);
	}
	if (rlen != 0) {
		if ((*rdfn)((pb_t *) fnparam, (char *) &q->sym.value, rlen) < rlen) 
			syserr("readsym: read val (sym=%d,%d)", t, rlen);
	}

	switch (t) {
	  case ROOT:
		q->sym.value.sym_root.rootuser = TRUE;
		break;
	  case AGHEAD:
		q->sym.value.sym_root.rootuser = FALSE;
		break;
	}
#ifdef xCTR1
	if (tTf(10, 9)) {
		nodepr(q);
	}
#endif xCTR1

	return (q);
}
/*
**  READTREE
**
** 	reads in tree symbols into a buffer up to a root (end) symbol
**
*/

qtree_t *
readtree(qtree_t *tresym, int (*rdfn)(pb_t *s, char *nam, int cc), char *fnparam)
{
	register qtree_t	*q;
	register qtree_t	*rtval;

	rtval = tresym;

	for(;;) {
		/* space for left & right pointers */
		q = readsym(rdfn, fnparam);
		if (q->sym.type == ROOT) {
			return (rtval);
		}
	}
}
/*
**  TRBUILD -- Rebuild a tree in memory
**
**	Trbuild is called with a pointer to the TREE node of
**	a query already in memory. It rebuilds the pointer
**	structure assuming the querytree is in postfix order.
**
**	Parameters:
**		bufptr - a pointer to the TREE node
**
**	Returns:
**		NULL - Internal stack overflow (MAX_STACK_SIZE)
**		pointer to the ROOT node
**
**	Side Effects:
**		All left & right pointers are rebuilt.
**
**	Called By:
**		readqry
**
**	Syserrs:
**		syserr if there are too many leaf nodes or too
**		few child nodes
*/

qtree_t *
trbuild(qtree_t *bufptr)
{
	register qtree_t 	**stackptr;
	register char	*p;
	register sym_t	*s;
	qtree_t		*treestack[MAX_STACK_SIZE];


	stackptr = treestack;

/*XXX*/	for (p = (char *) bufptr;; p += QT_HDR_SIZ + ((s->len + 3) & 0374)) {
		s = &((qtree_t *)p)->sym;
		((qtree_t *)p)->left = ((qtree_t *)p)->right = 0;

		/* reunite p with left and right children on stack, if any*/
		if (!leaf((qtree_t *) p)) {
			/* this node has children */
			if (s->type != UOP) {
				if (stackptr <= treestack) {
err:
					syserr("trbuild:too few nodes");
				} else {
					((qtree_t *)p)->right = *(--stackptr);
				}
			}
			if (s->type != AOP) {
				if (stackptr <= treestack) {
					goto err;
				} else {
					((qtree_t *)p)->left = *(--stackptr);
				}
			}
		}

		/*
		** If this is a ROOT node then the tree is complete.
		** verify that there are no extra nodes in the
		** treestack.
		*/
		if (s->type == ROOT) {
			/* root node */
			if (stackptr != treestack) {
				syserr("trbuild:xtra nodes");
			}
			return ((qtree_t *)p);
		}

		/* stack p */
		if (stackptr-treestack >= MAX_STACK_SIZE)  {
			return (NULL);	/* error:stack full */
		}
		*(stackptr++) = (qtree_t *) p;

	}
}

bool
leaf(qtree_t *p)
{
	switch (p->sym.type) {
	  case VAR:
	  case TREE:
	  case QLEND:
	  case INT_CONST:
	  case FLOAT_CONST:
	  case CHAR_CONST:
	  case COP:
		return(TRUE);

	  default:
		return(FALSE);
	}
}
