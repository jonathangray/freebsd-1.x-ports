#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "globs.h"
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)error.c	8.1	12/31/84)

void
derror(int eno)
{
	endovqp(NOACK);
	reinit();
	error(eno, 0);
}
