#include <stdio.h>

#include <ingres.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)ingresname.c	8.1	12/31/84)

/*
**  MAKE INGRES FILE NAME
**
**	The null-terminated string 'iname' is converted to a
**	file name as used by the ingres relations.  The name
**	of the relation is padded out to be MAX_NAME_SIZE bytes long,
**	and the two-character id 'id' is appended.  The whole
**	thing will be null-terminated and put into 'outname'.
**
**	'Outname' must be at least MAX_NAME_SIZE + 3 bytes long.
*/
void
ingresname(char *iname, char *id, char *outname)
{
	register char	*p;
	relation_t	r;

	p = outname;
	p = pmove(iname, p, MAX_NAME_SIZE, ' ');
	bmove(id, p, sizeof(r.r_owner));
	p[sizeof(r.r_owner)] = 0;
}
