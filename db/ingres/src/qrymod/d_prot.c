#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <access.h>
#include <tree.h>
#include <symbol.h>
#include <lock.h>
#include <pv.h>
#include <func.h>
#include "qrymod.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)d_prot.c	8.3	2/8/85)

/*
**  D_PROT -- define protection constraint
**
**	A protection constraint as partially defined by the last tree
**	defined by d_tree is defined.
**
**	The stuff that comes through the pipe as parameters is complex.
**	It comes as a sequence of strings:
**	 # The operation set, already encoded in the parser into a
**	   bit map.  If the PRO_RETR permission is set, the PRO_TEST
**	   and PRO_AGGR permissions will also be set.
**	 # The relation name.
**	 # The relation owner.
**	 # The user name.  This must be a user name as specified in
**	   the 'users' file, or the keyword 'all', meaning all users.
**	 # The terminal id.  Must be a string of the form 'ttyx' or
**	   the keyword 'all'.
**	 # The starting time of day, as minutes-since-midnight.
**	 # The ending time of day.
**	 # The starting day-of-week, with 0 = Sunday.
**	 # The ending dow.
**
**	The domain reference set is build automatically from the
**	target list of the tree.  Thus, the target list must exist,
**	but it is not inserted into the tree.  The target list must
**	be a flat sequence of RESDOM nodes with VAR nodes hanging
**	of the rhs; also, the VAR nodes must all be for Qt.qt_resvar.
**	If there is no target list on the tree, the set of all var-
**	iables is assumed.
**
**	The r_status field in the relation relation is updated to
**	reflect any changes.
**
**	It only makes sense for the DBA to execute this command.
**
**	If there is one of the special cases
**		permit all to all
**		permit retrieve to all
**	it is caught, and the effect is achieved by diddling
**	r_status bits instead of inserting into the protect catalog.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Activity in 'protect' and 'relation' catalogs.
**
**	Trace Flags:
**		59
*/

extern admin_t	Admin;
extern desc_t		Prodes;
extern desc_t		Reldes;

extern short	tTqm[80];

int d_prot(int pc, paramv_t *pv);

func_t	DefProFn = {
	"DPROT",
	d_prot,
	null_fn,
	null_fn,
	NULL,
	0,
	tTqm,
	80,
	'Q',
	0
};

int
d_prot(int pc, paramv_t *pv)
{
	protect_t	protup;
	struct tup_id	protid;
	protect_t	prokey;
	protect_t	proxtup;
	char		ubuf[MAX_LINE_SIZE + 1];
	register int	i;
	short	ix;
	register qtree_t	*t;
	qtree_t		*root;
	register char	*p;
	relation_t	reltup;
	relation_t	relkey;
	struct tup_id	reltid;
	int		relstat;
	int		all_pro;

	/*
	**  Fill in the protection tuple with the information
	**	from the parser, validating as we go.
	**
	**	Also, determine if we have a PERMIT xx to ALL
	**	with no further qualification case.  The variable
	**	'all_pro' is set to reflect this.
	*/

	clr_tuple(&Prodes, (char *) &protup);
	all_pro = TRUE;

	/* read operation set */
	if (pv->pv_type != PV_INT)
		syserr("d_prot: opset");
	protup.p_opset = pv->pv_val.pv_int;
	if ((protup.p_opset & PRO_RETR) != 0)
		protup.p_opset |= PRO_TEST | PRO_AGGR;
	pv++;

	/* read relation name */
	if (pv->pv_type != PV_STR)
		syserr("d_prot: relid");
	pmove(pv->pv_val.pv_str, protup.p_rel, MAX_NAME_SIZE, ' ');
	pv++;

	/* read relation owner */
	if (pv->pv_type != PV_STR)
		syserr("d_prot: relid");
	bmove(pv->pv_val.pv_str, protup.p_owner, sizeof(protup.p_owner));
	pv++;

	/* read user name */
	if (pv->pv_type != PV_STR)
		syserr("d_prot: user");
	if (strcmp(pv->pv_val.pv_str, "all") == 0)
		bmove("  ", protup.p_user, sizeof(protup.p_user));
	else {
		/* look up user in 'users' file */
		if (getnuser(pv->pv_val.pv_str, ubuf))
			qmerror(BADUSRNAME, -1, Qt.qt_resvar, pv->pv_val.pv_str, 0);
		for (p = ubuf; *p != ':' && *p != 0; p++)
			continue;
		bmove(++p, protup.p_user, sizeof(protup.p_user));
		/* XXX - agc assumes only 2 chars */
		if (p[0] == ':' || p[1] == ':' || p[2] != ':')
			syserr("d_prot: users %s", ubuf);
		all_pro = FALSE;
	}
	pv++;

	/* read terminal id */
	if (pv->pv_type != PV_STR) {
		syserr("d_prot: user");
	}
	if (strcmp(pv->pv_val.pv_str, "all") == 0) {
		pmove("", protup.p_term, sizeof(protup.p_term), ' ');
	} else {
		pmove(pv->pv_val.pv_str, protup.p_term, sizeof(protup.p_term), ' ');
		if (!isttyname(pv->pv_val.pv_str))
			qmerror(BADTERM, -1, Qt.qt_resvar, pv->pv_val.pv_str, 0);
		all_pro = FALSE;
	}
	pv++;

	/* read starting time of day */
	if (pv->pv_type != PV_INT)
		syserr("d_prot: btod");
	protup.p_tbegin = pv->pv_val.pv_int;
	if (pv->pv_val.pv_int > 0)
		all_pro = FALSE;
	pv++;
	
	/* read ending time of day */
	if (pv->pv_type != PV_INT)
		syserr("d_prot: etod");
	protup.p_tend = pv->pv_val.pv_int;
	if (pv->pv_val.pv_int < 24 * 60 - 1)
		all_pro = FALSE;
	pv++;

	/* read beginning day of week */
	if (pv->pv_type != PV_STR)
		syserr("d_prot: bdow");
	i = cvt_dow(pv->pv_val.pv_str);
	if (i < 0)
		qmerror(BADDOW, -1, Qt.qt_resvar, pv->pv_val.pv_str, 0);	/* bad dow */
	protup.p_dbegin = i;
	if (i > 0)
		all_pro = FALSE;
	pv++;

	/* read ending day of week */
	if (pv->pv_type != PV_STR)
		syserr("d_prot: edow");
	i = cvt_dow(pv->pv_val.pv_str);
	if (i < 0)
		qmerror(BADDOW, -1, Qt.qt_resvar, pv->pv_val.pv_str, 0);	/* bad dow */
	protup.p_dend = i;
	if (i < 6)
		all_pro = FALSE;
	pv++;

	/*
	**  Check for valid tree:
	**	There must be a tree defined, and all variables
	**	referenced must be owned by the current user; this
	**	is because you could otherwise get at data by
	**	mentioning it in a permit statement; see protect.c
	**	for a better explanation of this.
	*/

	if (pv->pv_type != PV_QTREE)
		syserr("d_prot: tree");
	root = (qtree_t *) pv->pv_val.pv_qtree;
	pv++;

	for (i = 0; i < MAX_VARS + 1; i++) {
		if (Qt.qt_rangev[i].rngvdesc == NULL)
			continue;
		if (!bequal(Qt.qt_rangev[i].rngvdesc->d_r.r_owner, Usercode, USERCODE_SIZE))
			qmerror(OWNEDNOT, -1, i, 0);
	}

	/* test for dba */
	if (!bequal(Usercode, Admin.ad_h.adm_owner, USERCODE_SIZE))
		qmerror(NOTDBA, -1, Qt.qt_resvar, 0);
	
	/* get domain reference set from target list */
	/* (also, find the TREE node) */
	t = root->left;
	if (t->sym.type == TREE) {
		for (i = 0; i < 8; i++)
			protup.p_domset[i] = -1;
	} else {
		for (i = 0; i < 8; i++)
			protup.p_domset[i] = 0;
		for (; t->sym.type != TREE; t = t->left) {
			if (t->right->sym.type != VAR ||
			    t->sym.type != RESDOM ||
			    t->right->sym.value.sym_var.varno != Qt.qt_resvar)
				syserr("d_prot: garbage tree");
			lsetbit(t->right->sym.value.sym_var.attno, protup.p_domset);
		}
		all_pro = FALSE;
	}

	/* trim off the target list, since it isn't used again */
	root->left = t;

	/*
	**  Check out the target relation.
	**	We first save the varno of the relation which is
	**	getting the permit stuff.  Also, we check to see
	**	that the relation mentioned is a base relation,
	**	and not a view, since that tuple would never do
	**	anything anyway.  Finally, we clear the Qt.qt_resvar
	**	so that it does not get output to the tree catalog.
	**	This would result in a 'syserr' when we tried to
	**	read it.
	*/

	protup.p_result = Qt.qt_resvar;
#ifdef xQTR3
	if (Qt.qt_resvar < 0)
		syserr("d_prot: Rv %d", Qt.qt_resvar);
#endif
	if (BITISSET(S_VIEW, Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_status))
		qmerror(NOTREALREL, -1, Qt.qt_resvar, 0);	/* is a view */

	/* clear the (unused) Qt.qt_qmode */
#ifdef xQTR3
	if (Qt.qt_qmode != mdPROT)
		syserr("d_prot: Qt.qt_qmode %d", Qt.qt_qmode);
#endif
	Qt.qt_qmode = -1;

	/*
	**  Check for PERMIT xx to ALL case.
	**	The r_status bits will be adjusted as necessary
	**	to reflect these special cases.
	**
	**	This is actually a little tricky, since we cannot
	**	afford to turn off any permissions.  If we already
	**	have some form of PERMIT xx to ALL access, we must
	**	leave it.
	*/

	relstat = Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_status;
	if (all_pro && (protup.p_opset & PRO_RETR) != 0) {
		if (protup.p_opset == -1)
			relstat &= ~S_PROTALL;
		else {
			relstat &= ~S_PROTRET;
			if ((protup.p_opset & ~(PRO_RETR|PRO_AGGR|PRO_TEST)) != 0) {
				/* some special case: still insert prot tuple */
				all_pro = FALSE;
			}
		}
	}
	else
		all_pro = FALSE;

	/* see if we are adding any tuples */
	if (!all_pro)
		relstat |= S_PROTUPS;
	
	/*
	**  Change relstat field in relation catalog if changed
	*/

	if (relstat != Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_status) {
		opencatalog("relation", OR_WRITE);
		ingres_setkey(&Reldes, &relkey, Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_id, RELID);
		ingres_setkey(&Reldes, &relkey, Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_owner, RELOWNER);
		i = getequal(&Reldes, &relkey, &reltup, &reltid);
		if (i != 0)
			syserr("d_prot: geteq %d", i);
		reltup.r_status = relstat;
		i = replace(&Reldes, &reltid, &reltup, FALSE);
		if (i != 0)
			syserr("d_prot: repl %d", i);
		if (noclose(&Reldes) != 0)
			syserr("d_prot: noclose(rel)");
	}

	Qt.qt_resvar = -1;

	if (!all_pro) {
		/*
		**  Output the created tuple to the protection catalog
		**  after making other internal adjustments and deter-
		**  mining a unique sequence number (with the protect
		**  catalog locked).
		*/

		if (root->right->sym.type != QLEND)
			protup.p_tree = puttree(root, protup.p_rel, protup.p_owner, mdPROT);
		else
			protup.p_tree = -1;

		/* compute unique permission id */
		opencatalog("protect", OR_WRITE);
		setrll(A_SLP, &Prodes.d_tid, M_EXCL);
		ingres_setkey(&Prodes, &prokey, protup.p_rel, PRORELID);
		ingres_setkey(&Prodes, &prokey, protup.p_owner, PRORELOWN);
		for (ix = 2; ; ix++) {
			ingres_setkey(&Prodes, &prokey, &ix, PROPERMID);
			i = getequal(&Prodes, &prokey, &proxtup, &protid);
			if (i < 0)
				syserr("d_prot: geteq");
			else if (i > 0)
				break;
		}
		protup.p_perm = ix;

		/* do actual insert */
		i = insert(&Prodes, &protid, &protup, FALSE);
		if (i < 0)
			syserr("d_prot: insert");
		if (noclose(&Prodes) != 0)
			syserr("d_prot: noclose(pro)");
		
		/* clear the lock */
		unlrl(&Prodes.d_tid);
	}
	return(0);
}

/*
**  CVT_DOW -- convert day of week
**
**	Converts the day of the week from string form to a number.
**
**	Parameters:
**		sdow -- dow in string form.
**
**	Returns:
**		0 -> 6 -- the encoded day of the week.
**		-1 -- error.
**
**	Side Effects:
**		none
**
**	Defines:
**		Dowlist -- a mapping from day of week to number.
**		cvt_dow
**
**	Called By:
**		d_prot
*/

struct downame {
	char	*dow_name;
	int	dow_num;
};

struct downame	Dowlist[] = {
	{ "sun",	0 },
	{ "sunday",	0 },
	{ "mon",	1 },
	{ "monday",	1 },
	{ "tue",	2 },
	{ "tues",	2 },
	{ "tuesday",	2 },
	{ "wed",	3 },
	{ "wednesday",	3 },
	{ "thu",	4 },
	{ "thurs",	4 },
	{ "thursday",	4 },
	{ "fri",	5 },
	{ "friday",	5 },
	{ "sat",	6 },
	{ "saturday",	6 },
	{ NULL }
};

int
cvt_dow(char *sdow)
{
	register struct downame	*d;
	register char		*s;

	s = sdow;

	for (d = Dowlist; d->dow_name != NULL; d++) {
		if (strcmp(d->dow_name, s) == 0) {
			return (d->dow_num);
		}
	}
	return (-1);
}

/*
**  ISTTYNAME -- "is a legal terminal name" predicate
**
**	Returns TRUE if the argument is a legal terminal name,
**	otherwise FALSE.
**
**	It may make sense to have this routine check if the given
**	file name really exists.
**
**	WARNING:
**		This routine may be installation-dependent!
**
**	Parameters:
**		n -- the name to check.
**
**	Returns:
**		TRUE -- n is a legal tty name at this installation.
**		FALSE -- otherwise.
**
**	Side Effects:
**		none
**
**	History:
**		8/1/79 (eric) -- written.
*/

int
isttyname(register char *n)
{
	return (strcmp(n, "console") == 0 || bequal(n, "tty", 3));
}
