#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <pipes.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIpb_rphys.c	8.1	12/31/84)


/*
**  IIPB_RPHYS -- physical read on pipe
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
**	Trace Flags:
**		none
*/
int
IIpb_rphys(register pb_t *ppb, register int fd)
{
	return (read(fd, ppb, PB_IOSIZE));
}
