#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <access.h>
#include <batch.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)batchxtra.c	8.1	12/31/84)

/*
**  BATCHNAME -- create batch file name
*/

char *
batchname(void)
{
	extern char	*Fileset;

	return(ztack("_SYSbatch", Fileset));
}

int
rmbatch(void)
{
	register char	*p;
	register int	i;

	p = batchname();
	if ((i = close(Batch_fp)) != 0)
		syserr("rmbatch:can't close %s %d", p, i);
	if ((i = unlink(p)) != 0)
		syserr("rmbatch:can't unlink %s %d", p, i);
	Batchhd.b_updtype = 0;
	return (0);
}
