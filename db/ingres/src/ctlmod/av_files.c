#include <sys/types.h>
#include <sys/param.h>

#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include "ctlmod.h"
#include "sccs.h"

#define INGRES_IUTIL
#include "protos.h"

SCCSID(@(#)av_files.c	8.1	12/31/84)

/*
**  AV_FILES -- return number of files available for user files.
**
**	Takes NOFILE and reduces it by the number of files that are
**	open or could be open (for the catalog descriptors).
**
**	Parameters:
**		none.
**
**	Returns:
**		the number of available file descriptor to play with.
**
**	Side Effects:
**		none.
*/

extern struct desxx	Desxx[];

int
av_files(void)
{
	long		fopn;
	register int		nopn;
	register int		i;
	register struct desxx	*p;

	markopen(&fopn);
	nopn = 0;
	for (i = 0; i < 32; i++) {
		if (BITISSET(1 << i, fopn))
			nopn++;
	}

	/* now scan the descriptor cache */
	for (p = Desxx; p->cach_relname != NULL; p++) {
		if (p->cach_alias != NULL)
			continue;
		if (p->cach_desc->d_opened == 0)
			nopn++;
	}
	return (NOFILE - nopn);
}
