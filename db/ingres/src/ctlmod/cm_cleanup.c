#include <stdio.h>
#include <signal.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "ctlmod.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)cm_cleanup.c	8.1	12/31/84)

/*
**  CM_CLEANUP -- cleanup after interrupt or error.
**
**	This routine does things like call the interrupt cleanup
**	function, reset the input, etc.
**
**	Parameters:
**		typ -- the type of cleanup:
**			1 -- fatal error (from error [error.c]).
**			2 -- keyboard interrupt.
**
**	Returns:
**		never (uses non-local jump to ctlmod/main.c).
**
**	Side Effects:
**		Proc_name & Cm.cm_input are reset.
**
**	Trace Flags:
**		0
*/
void
cm_cleanup(int typ)
{
	register int		i;
	register func_t	*f;
	extern jmp_buf		CmReset;
	register ctx_t		*ctx;

#ifdef xCTR2
	if (tTf(0, 13))
		printf("cm_cleanup: %d\n", typ);
#endif

	/*
	**  Call all interrupt cleanup functions for active
	**	modules.
	*/

	for (i = 0; i < NumFunc; i++) {
		f = FuncVect[i];
		if (f->fn_active > 0) {
			setprocname(Ctx.ctx_name = f->fn_name);
			(*f->fn_cleanup)(typ);
		}
	}

	/* clean up memory */
	for (ctx = &Ctx; ctx != NULL; ctx = ctx->ctx_link) {
		if (ctx->ctx_qt != NULL) {
			xfree(ctx->ctx_qt);
		}
		if (ctx->ctx_glob != NULL) {
			bmove(ctx->ctx_glob, ctx->ctx_fn->fn_gptr, ctx->ctx_fn->fn_gsize);
			xfree(ctx->ctx_glob);
		}
	}

	/* return to top of loop */
	longjmp(CmReset, typ);
}
