#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)clear.c	8.1	12/31/84)



/*
**  Clear query buffer
**	Flag f is set if called explicitly (with \q) and is
**	clear if called automatically.
**
**	Uses trace flag 3
*/

void
clear(char f)
{
	Autoclear = 0;

	/* TRUNCATE FILE & RETURN */
	if (freopen(Qbname, "w", Qryiop) == NULL)
		syserr("clear: open");
	if (Nodayfile >= 0 && f) {
		noise(1);
		printf("go\n");
	}
	if (f)
		clrline(0);  /* removes '\n' following '\g'etc  */
	Notnull = 0;
}
