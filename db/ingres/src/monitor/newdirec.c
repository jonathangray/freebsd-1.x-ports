#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)newdirec.c	8.1	12/31/84)

/*
**  CHANGE WORKING DIRECTORY
*/
void
newdirec(void)
{
	register char	*direc;

	direc = getfilenm();
	if (chdir(direc))
		printf("Cannot access directory \"%s\"\n", direc);
}
