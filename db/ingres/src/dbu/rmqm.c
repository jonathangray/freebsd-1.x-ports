#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <pv.h>
#include <ingres.h>
#include <access.h>
#include <aux.h>
#include <catalog.h>
#include <symbol.h>
#include <func.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#ifdef xZTR1
#define INGRES_CTLMOD
#endif
#include "protos.h"

SCCSID(@(#)rmqm.c	8.3	2/8/85)

/*
**  RMQM -- DBU to delete protection and integrity constraints
**
**	Trace Flags:
**		43
*/


extern	short	tTdbu[];

func_t RmqmFn = {
	"RMQM",
	dest_const,
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
**  R_RELSTAT -- set or reset bits in the relation.r_status field
**
**	Does the above for relation described by desc.
**
**	Parameters:
**		d -- relation to have relation.r_status field changed
**		bit -- bits to set or reset
**		action -- 0 reset, 1 set
**
**	Returns:
**		none
**
**	Side Effects:
**		relation is opened for READ/WRITE, r_status changed
**
**	Trace Flags:
**		43, 8
*/

void
r_relstat(register desc_t *d, int bit, int action)
{
	relation_t	tuple, key;
	tid_t		tid;
	register int	i;
	extern desc_t	Reldes;

#ifdef xZTR1
	if (tTf(43, 8))
		printf("r_relstat(bit=0%o, action %d)\n",
		bit, action);
#endif

	opencatalog("relation", OR_WRITE);
	clearkeys(&Reldes);
	ingres_setkey(&Reldes, &key, d->d_r.r_id, RELID);
	ingres_setkey(&Reldes, &key, d->d_r.r_owner, RELOWNER);
	if ((i = getequal(&Reldes, &key, &tuple, &tid)) != 0)
		syserr("r_relstat: getequal %s, %s, %d", d->d_r.r_id,
					d->d_r.r_owner, i);
	if (action) {
		if (tuple.r_status == (i = tuple.r_status | bit))
			return;
		tuple.r_status = i;
	} else {
		if (tuple.r_status == (i = tuple.r_status & ~bit))
			return;
		tuple.r_status = i;
	}
	if ((i = replace(&Reldes, &tid, &tuple, 0)) < 0 || i == 2)
		syserr("r_relstat: replace %d", i);
	if ((i = flush_rel(&Reldes, FALSE)) != 0)
		syserr("r_relstat: flush_rel(&Reldes) %d", i);
}
/*
**  CHK_CONST -- check constraint catlg for tuples for a rel, and reset relatin.r_status
**
**	Parameters:
**		r_desc -- reon desc for de-constrained relation
**		c_desc -- catalog desc
**		key -- catalog key (here unknown size)
**		tuple -- " tuple space " " " " "
**		relid -- relation name
**		id_attno -- attno of relid
**		relowner -- relation owner
**		own_attno -- relowner attno
**		bit -- bits to reset in relstat if there are no constraints left
**
**	Returns:
**		none
**
**	Side Effects:
**		reads catalog, maybe changes r_status field of relation
**		relations's r_desc tuple
**
**	Trace Flags:
**		43, 7
*/
void
chk_const(desc_t *r_desc, desc_t *c_desc, void *key, void *tuple, char *relid, int id_attno, char *relowner, int own_attno, int bit)
{
	tid_t		tid;
	register int	i;


#ifdef xZTR1
	if (tTf(43, 7))
		printf("chk_const: relid %s id_attno %d relowner %s own_attno %d bit 0%o)\n",
		relid, id_attno, relowner, own_attno, bit);
#endif

	clearkeys(c_desc);
	ingres_setkey(c_desc, key, relid, id_attno);
	ingres_setkey(c_desc, key, relowner, own_attno);
	if ((i = getequal(c_desc, key, tuple, &tid)) == 1)
		r_relstat(r_desc, bit, 0);
	else if (i < 0)
		syserr("chk_const: getequal %d", i);
}

/*
**  I_CAT -- prepare catalogs for deletin of constraint
**
**	Initializes treerelid, treeowner, and treetype fields
**	of tree key. Also relation id and owner fields of
**	appropriate catalog c_desc, with key 'key'.
**
**	Parameters:
**		c_name -- name of catalog for opencatalog
**		c_desc -- descriptor of catalog
**		key -- key for catalog
**		relid -- relation.relid for relation to be de-constrained
**		id_attno -- attno of relid in constraint catalog c_desc
**		relowner -- relation.relowner for rel to be de-constrained
**		own_attno -- attno of owner in constrain catalog
**		type -- treetype for tree tuple (depends on catalog)
**		tkey -- key for tree catalog
**
**	Returns:
**		none
**
**	Side Effects:
**		opencatalogs the constraint catalog c_desc, and the "tree" rel
**		for READ/WRITE. Sets keys.
**
**	Trace Flags:
**		43, 3
*/
void
i_cat(char *c_name, desc_t *c_desc, void *key, void *relid, int id_attno, char *relowner, int own_attno, int type, struct tree *tkey)
{
	extern desc_t	Treedes;
	char		realtype;

#ifdef xZTR1
	if (tTf(43, 3))
		printf("i_cat(c_name \"%s\", relid %s id_attno %d relowner %s own_attno %d type %d)\n", c_name, (char *) relid, id_attno, relowner, own_attno, type);
#endif

	realtype = type;
	opencatalog("tree", OR_WRITE);
	ingres_setkey(&Treedes, tkey, relid, TREERELID);
	ingres_setkey(&Treedes, tkey, relowner, TREEOWNER);
	ingres_setkey(&Treedes, tkey, &realtype, TREETYPE);
	opencatalog(c_name, OR_WRITE);
	clearkeys(c_desc);
	ingres_setkey(c_desc, key, relid, id_attno);
	ingres_setkey(c_desc, key, relowner, own_attno);
}

/*
**  DEL_TREE -- destroy a tree tuple with for a given treeid
**
**	Deletes all tuples from tree with 'treeid' and previously set
**	keys.
**
**	Parameters:
**		key -- tre key
**		treeid -- integer treeid
**
**	Returns:
**		none
**
**	Side Effects:
**		tree activity
**
**	Trace Flags:
**		43, 6
*/
void
del_tree(struct tree *key, int treeid)
{
	struct tree	tuple;
	tid_t		lotid, hitid;
	register int	i;
	register int	flag;
	short		realid;
	extern desc_t	Treedes;

#ifdef xZTR1
	if (tTf(43, 6))
		printf("del_tree(treeid=%d)\n", treeid);
#endif

	realid = treeid;
	ingres_setkey(&Treedes, key, &realid, TREEID);
	if ((i = find(&Treedes, EXACTKEY, &lotid, &hitid, key)) != 0)
		syserr("del_tree: bad find %d treeid %d", i, treeid);
	flag = 0;
	while (!(i = get(&Treedes, &lotid, &hitid, &tuple, TRUE))) {
		if (!kcompare(&Treedes, &tuple, key)) {
			if ((i = delete(&Treedes, &lotid)) != 0)
				syserr("del_tree: delete treeid %d %d", treeid, i);
			if (!flag)
				flag++;
		}
	}
	if (i != 1)
		syserr("del_tree: bad get %d", i);
	if (!flag)
		syserr("del_tree: no tuples qualified treeid %d", treeid);
	if ((i = flush_rel(&Treedes, FALSE)) != 0)
		syserr("del_tree: flush_rel(&Treedes) %d", i);
}
/*
**  DEL_ALL -- delete all constraints for a given relation
**
**	Deletes all constraints of a given type given by a constraint
**	catalog 'c_desc'. Note that Protection constraints 0 & 1, given
**	by relation.r_status field are not deleted here.
**
**	Parameters:
**		r_desc -- descriptor for relation to de-constrain (for
**			r_relstat)
**		c_desc -- constraint catalog descriptor
**		key -- c_desc's key
**		tuple -- c_desc's tuple (needed because sizeof tuple is not
**			known here, so must be allocated beforehand)
**		tkey -- tree key with TREERELID and TREERELOWNER setkeyed
**		bit -- bits in r_status to reset after deleting all constraints
**		tree_pred -- called with constraint tuple to determine
**			wether a tree tuple is present or not (as can happen
**			for protect catalog)
**		tree_field -- should return the treeid from tuple
**
**	Returns:
**		none
**
**	Side Effects:
**		tree and constraint catalog activity
**
**	Requires:
**		del_tree()
**		r_relstat()
**
**	Called By:
**		dest_????
**
**	Trace Flags:
**		43, 4
**
**	Syserrs:
**		bad find, get, delete, flush_rel
**
**	History:
**		1/10/79 -- (marc) written
*/
void
del_all(desc_t *r_desc, desc_t *c_desc, void *key, void *tuple, struct tree *tkey, int bit, int (*tree_pred)(void *tup), int (*tree_field)(void *tup))
{
	register int	i;
	tid_t		lotid;
	tid_t		hitid;

#ifdef xZTR1
	if (tTf(43, 4))
		printf("del_all(bit=0%o)\n", bit);
#endif

	if ((i = find(c_desc, EXACTKEY, &lotid, &hitid, key)) != 0)
		syserr("del_all: find %d", i);
	while (!(i = get(c_desc, &lotid, &hitid, tuple, TRUE))) {
		if (!kcompare(c_desc, tuple, key)) {
			/* for each constraint of for a relation */
			if ((i = delete(c_desc, &lotid)) != 0)
				syserr("del_all: delete %d", i);
			/* for crash recovery */
			if ((i = flush_rel(c_desc, FALSE)) != 0)
				syserr("del_all: flush_rel %d", i);
			/* if there is a tree tuple, destroy it */
			if ((*tree_pred)(tuple))
				del_tree(tkey, (*tree_field)(tuple));
		}
	}
	if (i != 1)
		syserr("del_all: get %d", i);
	/* turn off bit in r_status field */
	r_relstat(r_desc, bit, 0);
}

/*
**  DEL_INT -- delete from a constraint catalog a constraint
**
**	Parameters:
**		c_desc -- catalog descriptor
**		key -- catalog key
**		tuple -- catalog tuple (needed because tuple size unknown here)
**		tkey -- tree key with TREERELID and TREERELOWNER setkeyed
**		constid -- integer constraint id in string form
**		constattno -- attno of comstraint number in c_desc
**		tree_pred -- predicate on existence of tree tuple 
**		tree_field -- returns treeid from constrain tuple
**
**	Returns:
**		none
**
**	Side Effects:
**		constraint and tree catalog activity.
**		*constid set to 0 if constraint id exists.
**
**	Requires:
**		del_tree()
**
**	Called By:
**		dest_????
**
**	Trace Flags:
**		43, 5
**
**	Syserrs:
**		bad atoi (parser error), getequal, delete, flush_rel
**
**	History:
**		1/10/79 -- (marc) written
*/
void
del_int(desc_t *c_desc, void *key, void *tuple, struct tree *tkey, char *constid, int constattno, int (*tree_pred)(char *tup), int (*tree_field)(char *tup))
{
	tid_t		tid;
	register int	i;
	short		constnum;

#ifdef xZTR1
	if (tTf(43, 5))
		printf("del_int(constid=%s, constattno=%d)\n", 
	 	constid, constattno);
#endif

	constnum = atoi(constid);
	ingres_setkey(c_desc, key, &constnum, constattno);
	if (!(i = getequal(c_desc, key, tuple, &tid))) {
		if ((i = delete(c_desc, &tid)) != 0)
			syserr("del_int(%d) %d", constid, i);
		if ((*tree_pred)(tuple))
			del_tree(tkey, (*tree_field)(tuple));
		*constid = '\0';
		return;
	} else if (i != 1)
		syserr("dest_int: getequal %d", i);
	/* bad constnum */
}

/*
**  TREE_CONST -- True predicate
**
**	Called indirectly by routines wishing to know if
**	a integrity constraint has an associated tree tuple.
**	As this is always the case, returns TRUE always.
**
**	Parameters:
**		i -- integrity tuple
**
**	Returns:
**		TRUE
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		43, 9
*/
int
tree_const(struct integrity *i)
{
#ifdef xZTR1
	if (tTf(43, 9))
		printf("tree_const()\n");
#endif

	return (TRUE);
}
/*
**  TREE_PROT -- Protection tuple tree predicate
**
**	Called indirectly by routines wishing to know if
**	a protection constraint has an associated tree tuple.
**
**	Parameters:
**		p -- protect tuple
**
**	Returns:
**		TRUE -- if p->p_tree != -1
**		FLASE -- otherwise
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		43, 9
*/
int
tree_prot(protect_t *p)
{
#ifdef xZTR1
	if (tTf(43, 9))
		printf("tree_prot(p->p_tree=%d)\n", p->p_tree);
#endif

	if (p->p_tree == -1)
		return (FALSE);
	else
		return (TRUE);
}
/*
**  PROT_PROTREE -- get p_tree field of a protection tuple
**
**	Parameters:
**		p -- protect tuple
**
**	Returns:
**		p->p_tree
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		43, 9
*/
int
prot_protree(protect_t *p)
{
#ifdef xZTR1
	if (tTf(43, 9))
		printf("prot_protree(p_tree=%d)\n", p->p_tree);
#endif

	return (p->p_tree);
}
/*
**  INT_INTTREE -- get inttree field of a integrity tuple
**
**	Parameters:
**		i -- integrity tuple
**
**	Returns:
**		i->inttree
**
**	Side Effects:
**		none
**
**	Trace Flags:
**		43, 9
*/
int
int_inttree(struct integrity *i)
{
#ifdef xZTR1
	if (tTf(43, 9))
		printf("int_inttree(inttree=%d)\n", i->inttree);
#endif

	return (i->inttree);
}

/*
**  DEST_PROT -- directs destruction of protection constraints
**
**	Parameters:
**		desc -- descriptor for relation
**		intv -- PV_EOF terminated list of id strings, if first element
**		        is PV_EOF means "all"
**
**	Returns:
**		none
**
**	Side Effects:
**		deletes protection constraint. Activity on 'relation', 
**		protect, and tree.
**
**	Trace Flags:
**		43, 2
*/

int
dest_prot(desc_t *d, paramv_t *intv)
{
	extern desc_t	Prodes;
	protect_t	tuple, key;
	struct tree	tkey;
	register	i, j;
	int		propermid;

#ifdef xZTR1
	if (tTf(43, 2))
		printf("dest_prot((%s, %s)...)\n", d->d_r.r_id,
					d->d_r.r_owner);
#endif

	i_cat("protect", &Prodes, &key, d->d_r.r_id, PRORELID,
				d->d_r.r_owner, PRORELOWN, mdPROT, &tkey);

	if (intv[0].pv_type == PV_EOF) {
		/* destroy permit 'relation' ALL */
		if (!(d->d_r.r_status & S_PROTRET) || !(d->d_r.r_status & S_PROTALL))
			r_relstat(d, S_PROTRET | S_PROTALL, 1);
		if (!(d->d_r.r_status & S_PROTUPS))
			return (0);
		del_all(d, &Prodes, &key, &tuple, &tkey, S_PROTUPS,
			tree_prot, prot_protree);
		return (0);
	}
	/* destroy permit 'relation' int {, int} */
	for (i = 0; intv[i].pv_type != PV_EOF; i++) {
		propermid = atoi(intv[i].pv_val.pv_str);
		if (propermid == 0) {
			if (!(d->d_r.r_status & S_PROTALL)) {
				r_relstat(d, S_PROTALL, 1);
				intv[i].pv_val.pv_str = 0;
			}
			continue;
		} else if (propermid == 1) {
			if (!(d->d_r.r_status & S_PROTRET)) {
				r_relstat(d, S_PROTRET, 1);
				intv[i].pv_val.pv_str = 0;
			}
			continue;
		}
		del_int(&Prodes, &key, &tuple, &tkey, intv[i].pv_val.pv_str, PROPERMID, 
		tree_prot, prot_protree);
	}
	/* rescan to output error messages */
	for (j = 0; j < i; j++)
		if (intv[j].pv_val.pv_str && intv[j].pv_val.pv_str[0] )
			error(BADPROT, intv[j].pv_val.pv_str, 0);

	/* finally, check that there are still permissions
	** on the relation, if not must reset the S_PROTUPS bit in the relation
	** relation tuple for that relation's r_status.
	*/
	chk_const(d, &Prodes, &key, &tuple, d->d_r.r_id, PRORELID,
		d->d_r.r_owner, PRORELOWN, S_PROTUPS);
	return(0);
}

/*
**  DEST_INTEG -- directs destruction of integrity constraints
**
**	Parameters:
**		desc -- descriptor for relation
**		intv -- PV_EOF terminated list of id strings, if first element
**		        is PV_EOF means "all"
**
**	Returns:
**		none
**
**	Side Effects:
**		deletes integrity constraint. Activity on 'relation', integrities,
**		and tree.
*/
int
dest_integ(desc_t *d, paramv_t *intv)
{
	extern desc_t		Intdes;
	struct integrity	tuple, key;
	struct tree		tkey;
	register		i, j;

#ifdef xZTR1
	if (tTf(43, 1))
		printf("dest_integ((%s, %s)...)\n", d->d_r.r_id, d->d_r.r_owner);
#endif

	i_cat("integrities", &Intdes, &key, d->d_r.r_id, INTRELID,
			d->d_r.r_owner, INTRELOWNER, mdINTEG, &tkey);

	if (intv[0].pv_type == PV_EOF) {
		/* destroy integrity 'relation' ALL */
		if (!(d->d_r.r_status & S_INTEG))
			return (0);
		del_all(d, &Intdes, &key, &tuple, &tkey, S_INTEG,
			tree_const, int_inttree);
		return (0);
	}
	/* destroy integrity 'relation' int {, int} */
	for (i = 0; intv[i].pv_type != PV_EOF; i++)
		del_int(&Intdes, &key, &tuple, &tkey,
				intv[i].pv_val.pv_str, INTTREE,
				tree_const, int_inttree);

	/* rescan to output error messages */
	for (j = 0; j < i; j++)
		if (*(intv[j].pv_val.pv_str))
			error(BADINTEG, intv[j].pv_val.pv_str, 0);

	/* finally, check that there are still integrity constraints
	** on the relation, if not must reset the S_INTEG bit in the relation
	** relation tuple for that relation.
	*/
	chk_const(d, &Intdes, &key, &tuple, d->d_r.r_id, INTRELID,
				d->d_r.r_owner, INTRELOWNER, S_INTEG);
	return(0);
}

/*
**  DEST_CONST -- destroy constraints
**
**	Parameters:
**		pc -- number of parameters in pv
**		pv -- pv [0] == DESTPROT destroy permission
**			         == DESTINTEG destroy integrity constraint
**		        pv [1]    relation from which to destroy constraint
**		        pv [2] == if (pc != 2) relation from which to delete
**				     	constraints
**		        pv[3] ... pv[pc - 1] == id of constraint
**
**	Returns:
**		0
**
**	Side Effects:
**		destroys constraints. Involves activity on catalogs 'relation',
**		protect, integrities, and tree.
**
**	Trace Flags:
**		43, 0
*/
int
dest_const(int pc, paramv_t *pv)
{
	desc_t			d;
	register int		i;
	int			mode;
	extern admin_t	Admin;

#ifdef xZTR1
	if (tTf(43, 0)) {
		printf("dest_const: ");
		prvect(pc, pv);
	}
#endif

	if (!(Admin.ad_h.adm_flags & A_QRYMOD))
		return (0);
	i = openr(&d, OR_RELTID, pv[1].pv_val.pv_str);
	if (i < 0)
		syserr("dest_const: openr(%s) %d", pv[1].pv_val.pv_str, i);

	if (i == 1 || !bequal(Usercode, d.d_r.r_owner, USERCODE_SIZE)) {
		error(RELNOEXIST, pv[1].pv_val.pv_str, 0);
		return (0);
	}

	mode = atoi(pv[0].pv_val.pv_str);
	if (mode == DESTPROT)
		dest_prot(&d, &pv[2]);
	else if (mode == DESTINTEG)
		dest_integ(&d, &pv[2]);
	else
		syserr("dest_const: bad mode %d", mode);
	return (0);
}
