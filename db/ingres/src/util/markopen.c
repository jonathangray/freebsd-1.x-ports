#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

#include <errno.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <useful.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)markopen.c	8.2	1/6/85)

/*
**  MARKOPEN -- mark all open files
**
**	Marked files will not be closed later.
**
**	Parameters:
**		ovect -- pointer to bitmap of open files.
**
**	Returns:
**		none
**
**	Side Effects:
**		Sets *ovect to represent the open files.
*/

long	CmOfiles;	/* default set of files, used all over */

void
markopen(long *ovect)
{
	register int	i;
	struct stat	sbuf;

	if (ovect == NULL)
		ovect = &CmOfiles;

	*ovect = 0;
	for (i = 0; i < NOFILE; i++) {
		if (fstat(i, &sbuf) >= 0)
			*ovect |= 1 << i;
	}
	errno = 0;
}

/*
**  CLOSEALL -- close all open files (except marked files)
**
**	Parameters:
**		tell -- if set, report files that are open and should
**			not have been.
**		ovect -- vector of files to leave open.
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
void
closeall(int tell, long ovect)
{
	register int	i;

	ovect |= CmOfiles;

	for (i = 0; i < NOFILE; i++) {
		if (!BITISSET(1 << i, ovect))
			if (close(i) >= 0 && tell)
				lprintf("File %d open\n", i);
	}
}

/*
**	ADDMARKOPEN -- mark individial file descriptors as open
**
**	Marked descriptors will not be closed
**
**	Parameters:
**		long	pvect	pointer to file descriptor vector
**		int	fd	descriptor to mark
**
**	Returns:
**		nothing
**
**	Side effects:
**		none
*/
void
addmarkopen(long *pvect, int fd)
{
	*pvect |= 1 << fd;
}
