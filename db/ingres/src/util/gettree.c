#include <stdio.h>

#include <ingres.h>
#include <catalog.h>
#include <tree.h>
#include <symbol.h>
#include <pv.h>
#include "sccs.h"

#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)gettree.c	8.2	1/15/85)


/*
**  RELNTRRD -- read tree from 'tree' relation
**
**	This looks exactly like the 'pipetrrd' call, except that info
**	comes from the 'tree' talolog instead of from the pipe.  It
**	must be initialized by calling it with a NULL pointer and
**	the segment name wanted as 'treeid'.
**
**	Parameters:
**		dummyx -- a placeholder parameter to make this
**			routine compatible with pb_get.
**		ptr -- NULL -- "initialize".
**			else -- pointer to read area.
**		cnt -- count of number of bytes to read.
**		treerelid -- if ptr == NULL, the relation name
**			associated with the tree; ignored otherwise.
**		treeowner -- if ptr == NULL, the owner of the relation
**			associated with the tree; ignored otherwise.
**		treetype -- if ptr == NULL, the type of the tree
**			(view, prot, etc.); ignored otherwise.
**		treeid -- if ptr == NULL, this is the tree id,
**			otherwise this parameter is not supplied.
**
**	Returns:
**		count of actual number of bytes read.
**
**	Side Effects:
**		activity in database.
**		static variables are adjusted correctly.  Note that
**			this routine can be used on only one tree
**			at one time.
*/
int
relntrrd(int dummyx, char *ptr, int cnt, char *treerelid, char *treeowner, char treetype, int treeid)
{
	static struct tree	trseg;
	static char		*trp;
	static short		seqno;
	static	int		bytesleft;
	register char		*p;
	register int		n;
	register int		i;
	struct tree		trkey;
	tid_t			tid;
	extern desc_t		Treedes;

	p = ptr;
	n = cnt;

	if (p == NULL) {
		/* initialize -- make buffer appear empty */
		trp = &trseg.treetree[sizeof(trseg.treetree)];
		bytesleft = 0;
		bmove(treerelid, trseg.treerelid, MAX_NAME_SIZE);
		bmove(treeowner, trseg.treeowner, sizeof(trseg.treeowner));
		trseg.treetype = treetype;
		trseg.treeid = treeid;
		seqno = 0;
		opencatalog("tree", OR_READ);

#ifdef xQTR2
		if (tTf(13, 6))
			printf("relntrrd: n=%.12s o=%.2s t=%d i=%d\n",
			    treerelid, treeowner, treetype, treeid);
#endif

		return (0);
	}

	/* fetch characters */
	while (n-- > 0) {
		/* check for segment empty */
		if ( bytesleft == 0 ) {
			/* then read new segment */
			clearkeys(&Treedes);
			ingres_setkey(&Treedes, &trkey, trseg.treerelid, TREERELID);
			ingres_setkey(&Treedes, &trkey, trseg.treeowner, TREEOWNER);
			ingres_setkey(&Treedes, &trkey, &trseg.treetype, TREETYPE);
			ingres_setkey(&Treedes, &trkey, &trseg.treeid, TREEID);
			ingres_setkey(&Treedes, &trkey, &seqno, TREESEQ);
			seqno++;
			if ((i = getequal(&Treedes, &trkey, &trseg, &tid)) != 0)
				syserr("relnrdtr: getequal %d", i);
			trp = &trseg.treetree[0];
			bytesleft = sizeof(trseg.treetree);
		}

		/* do actual character fetch */
		*p++ = *trp++;
		bytesleft--;
	}

	return (cnt);
}
