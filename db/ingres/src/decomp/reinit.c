#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include <lock.h>
#include "sccs.h"

#define INGRES_IUTIL
#include "protos.h"

SCCSID(@(#)reinit.c	8.1	12/31/84)

/*
** REINIT -- reinitialize decomp upon end of query, error, or interrupt.
**	All open relations are closed, temp relations destroyed,
**	and relation locks released.
*/
void
reinit(void)
{
	closers();
	if (Lockrel) {
		/* release all relation locks */
		unlall();
	}
}
