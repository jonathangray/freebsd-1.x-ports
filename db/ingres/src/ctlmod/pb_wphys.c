#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "tree.h"
#include "ctlmod.h"
#include <useful.h>
#include "pipes.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)pb_wphys.c	8.1	12/31/84)

/*
**  PB_WPHYS -- physical write on pipe
**
**	Parameters:
**		ppb -- a pointer to the data area.
**		fd -- the file descriptor.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Called By:
**		pb_write
**
**	Trace Flags:
**		none
*/
void
pb_wphys(register pb_t *ppb, register int fd)
{
	register int	i;

	i = write(fd, (char *) ppb, PB_IOSIZE);
	if (i != PB_IOSIZE) {
		pb_dump(ppb, TRUE);
		syserr("pb_wphys: write error: fd=%d, i=%d", fd, i);
	}
}
