#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "ctlmod.h"
#include <ingres.h>
#include <tree.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)resetp.c	8.2	12/8/85)

/*
**  RESETP -- reset parameters from call.
**
**	This must be called after an initp-setp-call sequence to
**	reclaim the memory gathered by the initp.  It can also be
**	used in an initp-setp sequence to clean up if an error is
**	found.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Restores the old context.
**		Reclaims space from Qbuf.
**
**	Trace Flags:
**		4.4 - 4.7
*/
void
resetp(void)
{

#ifdef xCTR1
	if (tTf(4, 4))
		printf("resetp: ctx_new=%d _init=%d _link=%p _pmark=%d _cmark=%d\n",
		    Ctx.ctx_new, Ctx.ctx_init, Ctx.ctx_link, Ctx.ctx_pmark,
		    Ctx.ctx_cmark);
#endif

	/* don't do anything if no matching "initp" call */
	if (!Ctx.ctx_new)
		return;

	/* If the Current Qt header matches the new context clean things up */
	if ((char *)Ctx.ctx_link == Qt.qt_ctx)
		Qt.qt_ctx = NULL;

	/*
	**	The 'freebuf' to ctx_cmark will also deallocate
	**	any parameters, since ctx_pmark >= ctx_cmark
	**	always.
	*/

	if (Ctx.ctx_link != NULL)
		bmove((char *) Ctx.ctx_link, (char *) &Ctx, Ctx.ctx_size);
#ifdef xCM_DEBUG
	if (Ctx.ctx_pmark < Ctx.ctx_cmark)
		syserr("resetp: pmark %d cmark %d", Ctx.ctx_pmark, Ctx.ctx_cmark);
#endif
	freebuf(Qbuf, Ctx.ctx_cmark);
	tT = Ctx.ctx_tvect;
	setprocname(Ctx.ctx_name);
#ifdef xCTR1
	if (tTf(4, 5))
		lprintf("resetp: free %d\n", Ctx.ctx_cmark);
#endif

	/*
	**	If there was a Qt associated with this context,
	**	restore it also.  If not, but there was a Qt
	**	associated with the context we just deallocated,
	**	set Qt to be 'unallocated'.
	*/
	if (Ctx.ctx_qt != NULL) {
		/* first free any descriptors */
		clrrange();
		bmove((char *) Ctx.ctx_qt, (char *) &Qt, sizeof(Qt));
		xfree(Ctx.ctx_qt);
		Ctx.ctx_qt = NULL;
	} else if (Qt.qt_ctx == NULL) {
		Qt.qt_active = 0;
	}
}
