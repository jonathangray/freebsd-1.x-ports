#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <pipes.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIpb_wphys.c	8.1	12/31/84)


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
*/
void
IIpb_wphys(register pb_t *ppb, int fd)
{
	register int	i;

	i = write(fd, ppb, PB_IOSIZE);
	if (i != PB_IOSIZE)
		IIsyserr("pb_wphys: write error");
}
