#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <pv.h>
#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <func.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)destroy.c	8.3	2/8/85)

extern	short	tTdbu[];

func_t DstroyFn = {
	"DESTROY",
	destroy,
	null_fn,
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};

static int
des(char *name)
{
	register int	i;
	register char	*relname;
	struct tup_id	tid;
	char		newrelname[MAX_NAME_SIZE + 3];
	extern desc_t	Reldes, Attdes;
	relation_t	relt, relk;

	relname = name;
#ifdef xZTR1
	tTfp(32, -1, "destroy: %s\n", relname);
#endif

	newrelname[MAX_NAME_SIZE + 2] = 0;

	/* get the tuple from relation relation */
	ingres_setkey(&Reldes, &relk, relname, RELID);
	ingres_setkey(&Reldes, &relk, Usercode, RELOWNER);
	if ((i = getequal(&Reldes, &relk, &relt, &tid)) != 0) {
		if (i < 0)
			syserr("DESTROY: geteq(rel/%s) %d", relname, i);
		return (nferror(RELNOEXIST, relname, 0));	/* nonexistant relation */
	}

	/* don't allow a system relation to be destroyed */
	if (relt.r_status & S_CATALOG)
		return (nferror(NODESTSYSREL, relname, 0));	/* attempt to destroy system catalog */

	if ((i = delete(&Reldes, &tid)) != 0)
		syserr("DESTROY: delete(rel) %d", i);

	/*
	** for concurrency reasons, flush the relation-relation page
	** where the tuple was just deleted. This will prevent anyone
	** from being able to "openr" the relation while it is being
	** destroyed. It also allows recovery to finish the destroy
	** it the system crashes during this destroy.
	*/
	if ((i = flush_rel(&Reldes, FALSE)) != 0)
		syserr("destroy:flush rel %d", i);

	purgetup(&Attdes, relt.r_id, ATTRELID, relt.r_owner, ATTOWNER);

	/*
	**	If this is a user relation, then additional processing
	**	might be needed to clean up indicies, protection constraints
	**	etc.
	*/
	if ((relt.r_status & S_CATALOG) == 0)
		userdestroy(&relt);

	if ((relt.r_status & S_VIEW) == 0) {
		ingresname(relname, Usercode, newrelname);
		if (unlink(newrelname) < 0)
			syserr("destroy: unlink(%.14s)", newrelname);
	}
	return (0);

}

/*
**  DESTROY RELATION
**
**	The relations in pv are destroyed.  This involves three steps:
**	1 - remove tuple from relation relation
**	2 - remove tuples from attribute relation
**	3 - unlink physical file
**
**	If the relation is a secondary index, the entry is removed
**	from the index relation, and the primary relation is set to
**	be "not indexed" (unless there is another index on it).
**
**	If the relation has an index, all its indices are also
**	destroyed.
**
**	If any errors occured while destroying the relations,
**	then the last error number is returned, otherwise
**	0 is returned.
**
**	If any query modification was defined on the relation,
**	the qrymod catalogues are updated.
**
**	Trace Flags:
**		32
*/
int
destroy(int pc, paramv_t *pv)
{
	register int	i, ret;
	register char	*name;

	opencatalog("relation", OR_WRITE);
	opencatalog("attribute", OR_WRITE);

	for (ret = 0; pc-- > 0; ) {
		name = ((pv++)->pv_val).pv_str;
		if ((i = des(name)) != 0)
			ret = i;
	}
	return (ret);
}

