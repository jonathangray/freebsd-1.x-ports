/*
 * fopenexcl(name) - fopen(name, "w") with error and errno==EEXIST,
 *	if name exists (V7/V8/V9)
 */

#include <stdio.h>
#include <errno.h>
#include "fixerrno.h"

#define F_OK 0

FILE *
fopenexcl(name)
register char *name;
{
	if (access(name, F_OK) >= 0) {	/* name exists */
		errno = EEXIST;
		return NULL;		/* refuse to write on name */
	} else		
		return fopen(name, "w");	/* try to create name */
}
