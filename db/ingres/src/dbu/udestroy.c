#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <btree.h>
#include <pv.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)udestroy.c	8.2	1/15/85)


/*
**  USERDESTROY -- auxiliary cleanup for destroy of a user relation
**
**	userdestroy is called during the destroy of a non system
**	relation. If the relation is indexed or is itself an index
**	then the appropriate action is taken. If it is indexed,
**	then all secondary indices on the relation are also destroyed.
**	If it is a secondary index, then the entry in the indices relation
**	is removed and the "r_indexed" bit on the primary relation is
**	cleared if this was the last index on the relation.
**
**	If the relation was a view or had integrity constraints or
**	protection constraints on it, then those definitions are
**	removed from the appropriate system catalogues.
**
**	Parameters:
**		reltup -- the relation relation tuple.
**
**	Returns:
**		none
**
**	Side Effects:
**		zero or more system catalogues will be updated.
**
**	Called By:
**		destroy
*/
void
userdestroy(relation_t *reltup)
{
	register int			i;
	register relation_t	*rel;
	struct tup_id			tid, limtid;
	char				newrelname[MAX_NAME_SIZE + 3];
	extern desc_t			Reldes, Attdes, Inddes;
	extern desc_t			Treedes, Intdes, Prodes;
	relation_t			relt, relk;
	index_t			indk, indt;
	char				btree[MAX_NAME_SIZE + 4];
	paramv_t				pv[2];

	rel = reltup;

	/* handle special case of destroying a secondary index */
	if (rel->r_indexed == SECINDEX) {
		opencatalog("indices", OR_WRITE);
		ingres_setkey(&Inddes, &indk, rel->r_id, IRELIDI);
		ingres_setkey(&Inddes, &indk, rel->r_owner, IOWNERP);
		if ((i = getequal(&Inddes, &indk, &indt, &tid)) != 0)
			syserr("destroy: geteq(ind,%.12s) %d", rel->r_id, i);

		/* remove entry in INDEX catalog */
		bmove(indt.i_relname, newrelname, MAX_NAME_SIZE);
		bmove(indt.i_owner, &newrelname[MAX_NAME_SIZE], sizeof(indt.i_owner));
		if ((i = delete(&Inddes, &tid)) != 0)
			syserr("DESTROY: delete(ind/%.12s) %d", rel->r_id, i);
		clearkeys(&Inddes);
		ingres_setkey(&Inddes, &indk, newrelname, IRELIDP);
		ingres_setkey(&Inddes, &indk, &newrelname[MAX_NAME_SIZE], IOWNERP);

		/* reset r_indexed field in relation catalog if no other indices exist on this primary */
		if (getequal(&Inddes, &indk, &indt, &tid) != 0) {
			clearkeys(&Reldes);
			ingres_setkey(&Reldes, &relk, newrelname, RELID);
			ingres_setkey(&Reldes, &relk, &newrelname[MAX_NAME_SIZE], RELOWNER);
			if ((i = getequal(&Reldes, &relk, &relt, &tid)) != 0)
				syserr("destroy: getequal(rel, %s) %d", newrelname, i);
			relt.r_indexed = 0;
			if ((i = replace(&Reldes, &tid, &relt, 0)) != 0)
				syserr("destroy: replace(rel) %d", i);
		}
	}

	if (rel->r_dim > 0) {
		/* remove old B-Tree file */
		btreename(rel->r_id, btree);
		if (unlink(btree) < 0)
			syserr("userdestroy: unlink %s", btree);
		/* remove btreesec file */
		capital(trim_relname(rel->r_id), btree);
		pv[0].pv_val.pv_str = btree;
		pv[1].pv_type = PV_EOF;
		if (destroy(1, pv))
			syserr("can't destroy btreesec");
	}

	/* check special case of destroying primary relation */
	if (rel->r_indexed > 0) {
		opencatalog("indices", OR_WRITE);
		ingres_setkey(&Inddes, &indk, rel->r_id, IRELIDP);
		ingres_setkey(&Inddes, &indk, rel->r_owner, IOWNERP);
		if ((i = find(&Inddes, EXACTKEY, &tid, &limtid, &indk)) != 0)
			syserr("destroy: find(ind,%.12s) %d", rel->r_id, i);
		while ((i = get(&Inddes, &tid, &limtid, &indt, TRUE)) == 0) {
			if (kcompare(&Inddes, &indk, &indt) != 0)
				continue;
			if ((i = delete(&Inddes, &tid)) != 0)
				syserr("DESTROY: delete(ind/%.12s) %d", rel->r_id, i);
			clearkeys(&Reldes);
			purgetup(&Reldes, indt.i_index, RELID, indt.i_owner, RELOWNER);
			if ((i = flush_rel(&Reldes, FALSE)) != 0)	/* flush for recovery & concurrency reasons */
				syserr("destroy:flush irel %d", i);
			purgetup(&Attdes, indt.i_index, ATTRELID, indt.i_owner, ATTOWNER);
			ingresname(indt.i_index, indt.i_owner, newrelname);
			if (unlink(newrelname))
				syserr("destroy: unlink(%s)", newrelname);
		}
		if (i < 0) {
			syserr("destroy: get(ind) %d", i);
		}
	}

	/* if any integrity constraints exist, remove them */
	if (rel->r_status & S_INTEG) {
		opencatalog("integrities", OR_WRITE);
		purgetup(&Intdes, rel->r_id, INTRELID, rel->r_owner, INTRELOWNER);
	}

	/* if any protection clauses exist, remove them */
	if (rel->r_status & S_PROTUPS) {
		opencatalog("protect", OR_WRITE);
		purgetup(&Prodes, rel->r_id, PRORELID, rel->r_owner, PRORELOWN);
	}

	/* remove any trees associated with the relation */
	if (rel->r_status & (S_PROTUPS | S_VIEW | S_INTEG)) {
		opencatalog("tree", OR_WRITE);
		purgetup(&Treedes, rel->r_id, TREERELID, rel->r_owner, TREEOWNER);
	}
}
