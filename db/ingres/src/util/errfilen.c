#include <ingres.h>
#include <aux.h>
#include <version.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)errfilen.c	8.1	12/31/84)

/*
** Errfilen() -- Returns the pathname where the error file can be found
**
**	It is assumed that the error digit cannot be more than 999
*/

char *
errfilen(int digit)
{
	return (ztack(ztack(ztack(ztack(Pathname, "/files/error"), MAJOR_VERSION), "_"), iocv(digit)));
}
