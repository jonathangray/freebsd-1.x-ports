#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include <catalog.h>
#include <pv.h>
#include <func.h>
#include "qrymod.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)d_view.c	8.1	12/31/84)

/*
**  D_VIEW -- define view
**
**	This procedure connects the tree in with the relation catalog
**	and inserts the view tree into the tree catalog.
**
**	The information in the pipe is expected to come as follows:
**		create for view, with S_VIEW bit set so that a
**			physical relation is not created.
**		define tree, which will put the translation tree
**			into the 'tree' catalog.
**		define view, which will connect the two together.
**			The first two absolutely must be done before
**			this step can be called.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		I/O in 'tree' catalog.
**
**	Trace Flags:
**		39
*/

extern desc_t	Reldes;

extern short	tTqm[80];
int d_view(int pc, paramv_t *pv);

func_t	DefViewFn = {
	"DVIEW",
	d_view,
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
d_view(int pc, paramv_t *pv)
{
	char		viewid[MAX_NAME_SIZE + 1];
	register qtree_t	*t;
	int		treeid;

	/*
	**  Read parameters.
	*/

	if (pv->pv_type != PV_STR)
		syserr("d_view: viewid");
	pmove(pv->pv_val.pv_str, viewid, MAX_NAME_SIZE, ' ');
	pv++;

	if (pv->pv_type != PV_QTREE)
		syserr("d_view: tree");
	t = (qtree_t *) pv->pv_val.pv_qtree;
	pv++;
	
#ifdef xQTR3
	/* do some extra validation */
	if (Qt.qt_qmode != mdVIEW)
		syserr("d_view: Qt.qt_qmode %d", Qt.qt_qmode);
	if (Qt.qt_resvar < 0)
		syserr("d_view: Rv %d", Qt.qt_resvar);
	if (Qt.qt_rangev[Qt.qt_resvar].rngvdesc == NULL ||
	    !bequal(Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_id, viewid, MAX_NAME_SIZE))
		syserr("d_view: rangev %d %.14s", Qt.qt_rangev[Qt.qt_resvar].rngvdesc,
		    Qt.qt_rangev[Qt.qt_resvar].rngvdesc->d_r.r_id);
#endif

	declare(Qt.qt_resvar, NULL);
	Qt.qt_resvar = -1;
	Qt.qt_qmode = -1;

	/* output tree to tree catalog */
	treeid = puttree(t, viewid, Usercode, mdVIEW);
	return(0);
}
