#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <pv.h>
#include <func.h>
#include <tree.h>
#include "qrymod.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)qrymod.c	8.1	12/31/84)

/*
**  QRYMOD -- query modification process
**
**	This process modifies queries to implement views, protection,
**	and integrity.
**
**	Return Codes:
**		standard
**
**	Trace Flags:
**		none.
*/



desc_t		Prodes;		/* protection catalog descriptor */
desc_t		Reldes;		/* relation catalog descriptor */
desc_t		Treedes;	/* tree catalog descriptor */
desc_t		Intdes;		/* integrity catalog descriptor */
extern int	Equel;		/* equel flag */

#define TTYIDSIZE	8	/* length of tty id */

short	tTqm[80];
char	Terminal[TTYIDSIZE + 1];
void	qm_init(int argc, char **argv);
int	qrymod(int pc, paramv_t *pv);

func_t	QryModFn = {
	"QRYMOD",
	qrymod,
	qm_init,
	null_fn,
	(char *) &Qm,
	sizeof(Qm),
	tTqm,
	80,
	'Q',
	0,
};


void
qm_init(int argc, char **argv)
{
	char		*tty;

	/* determine user's terminal for protection algorithm */
	if ((tty = ttyname(1)) != NULL) {
		tty = strrchr(tty, '/') + 1; 
	}
	pmove((tty != NULL ? tty : " "), Terminal, TTYIDSIZE, ' ');
	Terminal[TTYIDSIZE] = '\0';
#ifdef xQTR1
	if (tTf(75, 0))
		printf("Terminal = \"%s\"\n", Terminal);
#endif
}

/*
**  QRYMOD -- main driver for query modification
**
**	Reads in the query tree, performs the modifications, writes
**	it out, and does process syncronization with below.  The
**	calling routine must sync with the process above.
**
**	Parameters:
**		pc -- parameter count (must = 1).
**		pv -- parameter vector:
**			pv[0] -- tree to modify.
**
**	Returns:
**		zero.
**
**	Side Effects:
**		The tree is modified to one that is guaranteed to
**		be directly processable.
**
**	Trace Flags:
**		none.
*/

int
qrymod(int pc, paramv_t *pv)
{
	register qtree_t	*root;

	/*
	**  Get parameters.
	*/

	if (pc != 1)
		syserr("pc=%d", pc);
	if (pv[0].pv_type != PV_QTREE)
		syserr("pv[0].type=%d", pv[0].pv_type);
	root = pv[0].pv_val.pv_qtree;

	/* view processing */
	root = view(root);

	/* integrity processing */
	root = integrity(root);

	/* protection processing */
	root = protect(root);

	return(0);
}
