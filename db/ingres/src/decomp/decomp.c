#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "globs.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)decomp.c	8.3	12/18/85)

/*
**	Decompz processes a one variable query
**	by calling call_ovqp().
**
**	Trace Flags:
**		33
*/

int
decompz(qtree_t *q, int qmode, int result_num)
{
	register qtree_t	*root;
	register int	qualfound;

	root = q;
	if (root->sym.value.sym_root.tvarc) {
		if (De.de_sourcevar < 0) {
			if ((De.de_sourcevar = selectv(root)) < 0) 
				return (FALSE);
		}
	} else {
		De.de_sourcevar = -1;
	}

	qualfound = call_ovqp(root, qmode, result_num);
	De.de_newq = 0;
	return (qualfound);
}
/*
** Decompx -- Initialize for multi-variable query.
**	All one-variable subqueries are run.
**	If the remaining query is still multi-var
**	then decision() is called; else decompz()
**	is called. The range table is restored
**	after the query is complete.
**	The tempvars from the exec_sq() are left on the
**	tree since it is presumed that the tree will be discarded
**	anyway.
**
**	Trace Flags:
**		31
*/
int
decompx(qtree_t *root, int qmode, int result_num)
{
	register int 	i, vc;
	int		disj;
	int  		sqbuf[1+SQSIZ/sizeof(int)];
	qtree_t 	*sqlist[MAX_RANGES];
	int		locrang[MAX_RANGES], sqrange[MAX_RANGES];

	vc = root->sym.value.sym_root.tvarc;
	initbuf((char *)sqbuf, SQSIZ, SQBUFFULL, derror);
	pull_sq(root, sqlist, locrang, sqrange, (char *)sqbuf);
	if ((i = exec_sq(sqlist, sqrange, &disj)) != -1) {
		undo_sq(sqlist, locrang, sqrange, i, i, FALSE);
		return (FALSE);
	}
	vc -= disj;
	tempvar(root, sqlist, (char *)sqbuf);
	if (pull_const(root, (char *)sqbuf) == 0)
		return (FALSE);
	if (vc <= 1) {
		De.de_sourcevar = -1;
		De.de_newq = 1;
		return (decompz(root, qmode, result_num));
	}
	i = decision(root, qmode, result_num, (char *)sqbuf);
	undo_sq(sqlist, locrang, sqrange, MAX_RANGES, MAX_RANGES, FALSE);
	return (i);
}

/*
** DECOMP -- Process a query given a query tree and range table.
**
**	Decomp processes any arbitrary query by converting it into
**	a sequence of "one variable queries"; eg. queries involving
**	at most one source relation. This file and decision.c contain
**	the principle decision making routines.
**
**	Decomp() is called with a pointer to a query tree, the mode
**	of the query (retrieve, append, etc.), and the internal name
**	of the result relation (if any). The routines included are:
**
**	Decomp  -- Opens the source relations and decides whether the
**			query is multi-variable or single/zero variable.
**
**	Decompx -- Takes a multivariable query, removes and executes any
**			one-var restrictions and passes the remaining query
**			to decompz (if one/zero variable) or decision().
**
**	Decompy -- Performs "tuple substitution" on a multi-var query,
**			does any new one-var restrictions and passes the
**			remaining query to decompz (if one/zero variable)
**			or decision().
**
**	Decompz -- Executes a one/zero variable query by calling call_ovqp().
*/
/*
**	Process query by calling either decompx for multivar
**	or decompz for 0 or 1 var query. Decomp() must guarantee
**	that the range table is the same upon exiting as it was
**	when entered. Newquery() and endquery() perform that function.
**
**	Trace Flags:
**		30
*/
int
decomp(qtree_t *q, int qmode, int result_num)
{
	register qtree_t 	*root;
	register int 	vc, i;
	int		locrange[MAX_RANGES];

	root = q;
	vc = root->sym.value.sym_root.tvarc;
#ifdef xDTR1
	if (tTf(30, 0))
		printf("DECOMP: %d-var query, result_num=%d\n", vc, result_num);
	if (tTf(30, 1)) {
		printf("DECOMP\n");
		treepr(root);
	}
#endif

	openrs(root);

	if (vc > 1) {
		newquery(locrange);
		i = decompx(root, qmode, result_num);
		endquery(locrange, FALSE);	/* don't reopen previous range */
	} else {
		De.de_newq = 1;
		De.de_sourcevar = -1;
		i = decompz(root, qmode, result_num);
	}
	
 	/*
	** remove the range variables that the view subsystem
	** might have placed in.
	*/
	clrrange();

	return (i);
}

/*
** Decompy -- decompose a multi-variable query by tuple substitution.
**	First a variable is selected
**	for substitution. Then for each tuple in the
**	selected variable, all one variable restrictions
**	are done (exec_sq) and the remaining query is
**	solved by calling either decompz() or recursively
**	decision().
**
**	The original tree and range table are guaranteed to
**	be the same on entry and exit (modulo the effects of
**	reformat()).
**
**	Trace Flags:
**		32
*/
int
decompy(qtree_t *q, int qmode, int result_num, char *sqbuf)
{
	register qtree_t	*root;
	register int	j, vc;
	desc_t		*d;
	qtree_t		*newroot;
	int		constl, sqcnt, var, srcvar, maxsqcnt;
	int		disj, tc, qtrue;
	tid_t		tid, hitid;
	char		*tuple;
	qtree_t		*sqlist[MAX_RANGES];
	int		sqmark, sqmark1;
	int		locrang[MAX_RANGES], sqrange[MAX_RANGES];

	root = q;
	vc = root->sym.value.sym_root.tvarc;

#ifdef xDTR1
	if (tTf(32, -1))
		printf("DECOMPY:%p,vc=%d\n", root, vc);
#endif

	sqmark = markbuf(sqbuf);
	constl = !root->sym.value.sym_root.lvarc;
	qtrue = FALSE;

	if ((var = selectv(root)) < 0)
		return (qtrue);
	d = readopen(var);	/* gets full descriptor for setvar & get */
	tuple = need(sqbuf, d->d_r.r_width);
	setvar(root, var, &tid, tuple);
	pull_sq(root, sqlist, locrang, sqrange, sqbuf);
	tempvar(root, sqlist, sqbuf);
	reformat(var, sqlist, locrang, sqbuf, root);
	vc--;
#ifdef xDTR2
	if (tTf(32, 0)) {
		printf("DECOMPY: &tup=%p, ", tuple);
		printdesc(d);
	}
#endif xDTR2

	/* HERE FOR MULTI-VAR SUBSTITUTION */
	sqmark1 = markbuf(sqbuf);
	De.de_newq = 1;
	tc = 0;
	sqcnt = maxsqcnt = 0;
	srcvar = -1;
	De.de_sourcevar = -1;
	find(readopen(var), NOKEY, &tid, &hitid, (void *) NULL);
	while (!(j=get(readopen(var), &tid, &hitid, tuple, NXTTUP))) {
#ifdef xDTR1
		if (tTf(32, 2)) {
			printf("Subst:");
			printup(readopen(var), tuple);
		}
#endif
		tc++;
		if (vc > 1) {
			reset_sq(sqlist, locrang, sqcnt);
			if ((sqcnt = exec_sq(sqlist, sqrange, &disj)) != -1) 
				continue;

			maxsqcnt = sqcnt;
			vc -= disj;
			if (vc <= 1) {
				De.de_sourcevar = srcvar;
				qtrue |= decompz(root, qmode, result_num);
				srcvar = De.de_sourcevar;
			} else {
				freebuf(sqbuf, sqmark1);
				newroot = copy_ands(root, sqbuf);
				qtrue |= decision(newroot, qmode, result_num, sqbuf);
			}
			vc += disj;
		}
		else
			qtrue |= decompz(root, qmode, result_num);

		/* check for early termination on constant Target list */
		if (constl && qtrue) 
			break;
	}
	if (j < 0) 
		syserr("decompy: bad get %d on %.12s", j, readopen(var)->d_r.r_id);

	/* undo the effect of pulling the sub queries */
	origvar(root, sqlist);
	undo_sq(sqlist, locrang, sqrange, sqcnt, maxsqcnt, TRUE);

	/* undo the setvar on the main tree and all subtrees */
	clearvar(root, var);
	for (j = 0; j < MAX_RANGES; j++)
		clearvar(sqlist[j], var);

	/* return any used buffer space */
	freebuf(sqbuf, sqmark);

#ifdef xDTR1
	if (tTf(32, 2))
		printf("tc[%.12s]=%d,qtrue=%d\n", rangename(var), tc, qtrue);
#endif

	return (qtrue);
}

