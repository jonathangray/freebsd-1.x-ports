#include <stdio.h>
#include <ingres.h>
#include <symbol.h>
#include <tree.h>
#include <pv.h>
#include "parser.h"
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)call_tree.c	8.1	12/31/84)

/*
**  CALL_TREE -- call the appropriate module below
**
**	Call_tree prepends a TREE node to the leftmost node on the tree,
**	adds the tree to the paramv_t, and does a CM call().
**
**	Parameters:
**		qmode -- qmode of query
**		dest -- module to call
**		err_fcn() -- function to call on error
**
**	Returns:
**		nothing
**
**	Trace Flags:
**		call_tree ~~ 44.0, 44.4
*/
void
call_tree(register int qmode, int dest, int (*err_fcn)(void))
{
	extern int	Resrng;
	extern qtree_t	*Lastree;

#ifdef	xPTR2
	tTfp(44, 0, "call_tree: qm=%d, dest=%d\n", qmode, dest);
#endif

	Qt.qt_qmode = qmode;

#ifdef	xPTR2

	if (tTf(44, 4))
		if (Resrng >= 0)
			printf("resvarno:%d\n", Resrng);
#endif

	Qt.qt_resvar = Resrng;

	/* the following attaches the TREE node to the far left of the tree */

	tlprepend(par_tree(NULL, NULL, TREE, 0, 0, 0), Lastree);

	setp(PV_QTREE, Lastree, 0);

	call(dest, err_fcn);

#ifdef	xPTR2
	tTfp(44, 5, "Call_tree: call returned\n");
#endif
}
