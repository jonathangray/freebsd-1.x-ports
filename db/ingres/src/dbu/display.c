#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <tree.h>
#include <symbol.h>
#include <access.h>
#include <func.h>
#include <pv.h>
#include <errors.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)display.c	8.3	2/8/85)

/*
**  DISPLAY -- display query corresponding to view, permission, 
**		or intgerity declaration
*/

extern short	tTdbu[];

func_t DsplayFn = {
	"DISPLAY",
	display,
	null_fn,
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};

/*
**  PR_DEF -- Print "define view" query of a view
**
**	Parameters:
**		relation -- relation in question
**		owner -- relowner
**
**	Returns:
**		none
**
**	Side Effects:
**		reads a tree, clears range table
**
**	Trace Flags:
**		33, 9
*/
void
pr_def(char *relation, char *owner)
{
	register qtree_t	*t;

#ifdef xZTR1
	if (tTf(50, 9))
		printf("pr_def(relation=\"%s\", owner=%s)\n", relation, owner);
#endif

	printf("View %s defined:\n\n", relation);
	clrrange();

	/* Treeid == 0 because views have only one definition */
	t = gettree(relation, owner, mdVIEW, 0,FALSE);
	pr_range();
	printf("define view ");
	pr_tree(t);
}

/*
**  PR_INT -- print an integrity definition given a integrities tuple
**
**	Parameters:
**		i -- integrity tuple
**
**	Returns:
**		none
**
**	Side Effects:
**		prints a query
**		reads a tree
*/
void
pr_int(register struct integrity *i, char *relid)
{
	register qtree_t	*t;

	clrrange();
	t = gettree(i->intrelid, i->intrelowner, mdINTEG, i->inttree, 0);
	printf("Integrity constraint %d -\n\n", i->inttree);
	pr_range();

	printf("define integrity on ");
	pr_rv(Qt.qt_resvar = i->intresvar);
	printf(" is "); 
	pr_qual(t->right);
	printf("\n\n\n");

}
/*
**  PR_INTEGRITY -- print out integrity constraints on a relation
**
**	Finds all integrity tuples for this unique relation, and
**	calls pr_int() to print a query from them.
**
**	Parameters:
**		relid -- rel name
**		relowner -- 2 byte owner id
**
**	Returns:
**		none
**
**	Side Effects:
**		file activity, query printing
**
**	Trace Flags:
**		33, 9
*/
void
pr_integrity(char *relid, char *relowner)
{
	extern desc_t		Intdes;
	tid_t			hitid, lotid;
	struct integrity	key, tuple;
	register int		i;


#ifdef xZTR1
	if (tTf(50, 9))
		printf("pr_integrity(relid =%s, relowner=%s)\n", 
			relid, relowner);
#endif

	printf("Integrity constraints on %s are:\n\n", relid);
	opencatalog("integrities", OR_READ);

	/* get integrities tuples for relid, relowner */
	clearkeys(&Intdes);
	ingres_setkey(&Intdes, &key, relid, INTRELID);
	ingres_setkey(&Intdes, &key, relowner, INTRELOWNER);
	if ((i = find(&Intdes, EXACTKEY, &lotid, &hitid, &key)) != 0)
		syserr("pr_integrity: find %d", i);
	for (;;) {
		if ((i = get(&Intdes, &lotid, &hitid, &tuple, TRUE)) != 0)
			break;
		if (kcompare(&Intdes, &tuple, &key) == 0)
			pr_int(&tuple, relid);
	}
	if (i != 1)
		syserr("pr_integrity: get %d", i);
}


/* 
** DISP -- display integrity, permit, or define query on a relation
**
**	Finds a relation owned by the user or the DBA and passes
**	the name and owner to the appropritae routine depending on
**	mode.
**
**	Parameters:
**		relation -- relation on which query is to be printed
**		mode -- the print mode:
**			4 -- view
**			5 -- permit
**			6 -- integrity
**
**	Returns:
**		0 -- success
**		1 -- no such relation, or none seeable by the user.
**		3 -- VIEW mode and relation not a view
**		4 -- PERMIT and no permissions on relation
**		5 -- INTEGRITY mode and no integrity constraints
**
**	Trace Flags:
**		33, 8
*/
int
disp(char *relation, int mode)
{
	desc_t		d;
	register int	i;
	extern char	*Resrel;

#ifdef xZTR1
	if (tTf(50, 8))
		printf("disp: relation %s\n", relation);
#endif

	Resrel = relation;
	i = openr(&d, OR_RELTID, relation);
	if (i > 0)
		return (1);
	else if (i < 0)
		syserr("disp: openr(%s) ret %d", relation, i);
	switch (mode) {
	  case 4:		/* View query */
		if (d.d_r.r_status & S_VIEW)
			pr_def(relation, d.d_r.r_owner);
		else 
			return (3);
		break;

	  case 5:
		if (pr_prot(relation, (relation_t *) &d))
			return (4);
		break;

	  case 6:
		if (d.d_r.r_status & S_INTEG)
			pr_integrity(relation, d.d_r.r_owner);
		else
			return (5);
		break;

	  default:
		syserr("disp: mode == %d", mode);
	}
	return (0);
}
/*
**  DISPLAY -- display query
**
**	Parameters:
**		pc -- number of args in pv
**		pv -- pv[i] == 4 for VIEW, 5 for PERMIT, 6 for INTEGRITY
**				where i % 2 == 0
**		pv[i] -- relation names for which pv[i-1] is mode
**				where i%2==1
**
**	Returns:
**		0 -- total success
**		err -- error number of last error
**
**	Side Effects:
**		prints out definition of appropriate characteristic of relation
**
**	Trace Flags:
**		33, -1
*/

int
display(int pc, paramv_t *pv)
{
	register int	ac;
	register paramv_t	*av;
	int		err;
	char		err_array[PV_MAXPC];
	int	mode;

#ifdef xZTR1
	if (tTf(50, -1)) {
		printf("display: ");
		prvect(pc, pv);
	}
#endif

	err = 0;
	if (pc % 2 != 0)
		syserr("display: bad param count %d", pc);
	opencatalog("tree", OR_READ);

	for (ac = 0, av = pv; ac < pc; av++, ac++) {
		mode = atoi(av->pv_val.pv_str);
		av++;
		err_array[ac++] = 0;
		err_array[ac] = disp(av->pv_val.pv_str, mode);
	}
	for (ac = 0, av = pv; ac < pc; av++, ac++) {
		if (err_array[ac])
			err = error(DISPERRBASE + err_array[ac], (av->pv_val).pv_str, 0);
	}
	return (err);
}
