#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "ctlmod.h"
#include "sccs.h"

SCCSID(@(#)cm_close.c	8.1	12/31/84)

/*
**  CM_CLOSE -- close the parser write pipe
**
**	This routine is a Kludge for use by the tty monitor only.
**	It is here because of the difficulty of getting at the
**	Cm struct from another directory.
*/
void
cm_close(void)
{
	close(Cm.cm_proc[1].pr_file);
}
