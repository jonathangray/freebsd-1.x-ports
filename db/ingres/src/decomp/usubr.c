#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include <access.h>
#include <pv.h>
#include "globs.h"
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)usubr.c	8.1	12/31/84)


/*
**	usubr.c
**
**	utility routines to handle setting up params, etc for DBU calls
*/

/*
**	gets format in ascii from RESDOM or AOP node
*/
static char *
getformat(qtree_t *p)
{

	static char	buf[10];
	register char	*b;

	b = buf;

	*b++ = p->sym.value.sym_op.opfrmt;
	itoa(p->sym.value.sym_op.opfrml & I1MASK, b);
	return(buf);
}
/*
 *	generate domain names, formats
 */
void
domnam(qtree_t **lnp, char *pre)
{

	register char 	suf, *n;
	char		name[MAX_NAME_SIZE];
	register qtree_t	**p;

	suf = '1';
	for (n=name; (*n++= *pre++) != 0 ;) {
	}
	*n-- = '\0';
	for (p = lnp; *p != NULL; p++) {
		*n = suf++;
		setp(PV_STR, name, 0);
		setp(PV_STR, getformat(*p), 0);
	}
}
/*
**	makes list of nodes (depth first)
*/
int
lnode(qtree_t *nod, qtree_t **lnodv, int count)
{
	register qtree_t	*q;
	register int	i;

	i = count;
	q = nod;

	if (q && q->sym.type != TREE) {
		i = lnode(q->left, lnodv, i);
		lnodv[i++] = q;
	}
	return(i);
}
/*
**	Put relation on list of relations to be
**	destroyed. A call to initp() must be
**	made before any calls to dstr_mark().
**
**	A call to call_dbu will actually have
**	the relations exterminated
**
**	Trace Flags:
**		65
*/
int
dstr_mark(int relnum)
{
	register char	*p;
	bool		dstr_flag;

	dstr_flag = FALSE;
	if (rnum_temp(relnum)) {
		p = rnum_convert(relnum);
#ifdef xDTR1
		if (tTf(65, 4))
			printf("destroying %s\n", p);
#endif
		setp(PV_STR, p, 0);
		specclose(relnum);	/* guarantee that relation is closed and descriptor destroyed */
		rnum_remove(relnum);
		dstr_flag = TRUE;	/* indicate that there are relations to be destroyed */
	}
	return(dstr_flag);
}
/*
**	Immediately destroys the relation if it is an _SYS
*/
void
dstr_rel(int relnum)
{
	initp();
	if (dstr_mark(relnum))
		call_dbu(mdDESTROY, FALSE);
	else
		resetp();
}

/*
**	Make a temporary relation to match
**	the target list of tree.
**
**	If rnum is positive, use it as the relation number,
**	Otherwise allocate a new one.
*/
int
mak_t_rel(qtree_t *tree, char *prefix, int rnum)
{
	qtree_t		*lnodv[MAX_DOMAINS + 1];
	register int	relnum;

	initp();
	setp(PV_STR, "0", 0);	/* initial relstat field */
	relnum = rnum < 0 ? rnum_alloc() : rnum;
	setp(PV_STR, rnum_convert(relnum), 0);
	lnodv[lnode(tree->left, lnodv, 0)] = NULL;
	domnam(lnodv, prefix);

	call_dbu(mdCREATE, FALSE);
	return (relnum);
}


qtree_t **
mksqlist(qtree_t *tree, int var)
{
	register qtree_t	**sq;
	register int	i;
	static qtree_t	*sqlist[MAX_RANGES];

	sq = sqlist;
	for (i = 0; i < MAX_RANGES; i++)
		*sq++ = 0;

	sqlist[var] = tree;
	return (sqlist);
}

long
rel_pages(long tupcnt, int width)
{
	register int	tups_p_page;

	tups_p_page = (PGSIZE - 12) / (width + 2);
	return ((tupcnt + tups_p_page - 1) / tups_p_page);
}
