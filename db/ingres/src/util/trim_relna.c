#include <ingres.h>
#include "sccs.h"

SCCSID(@(#)trim_relna.c	8.1	12/31/84)

/*
**  TRIM_RELNM -- trim blanks from relation name for printing
**
**	A relation name (presumably in 'ingresname' format: MAX_NAME_SIZE
**	characters long with no terminating null byte) has the
**	trailing blanks trimmed off of it into a local buffer, so
**	that it can be printed neatly.
**
**	Parameters:
**		name -- a pointer to the relation name
**
**	Returns:
**		a pointer to the trimmed relation name.
**
**	Side Effects:
**		none
*/

char *
trim_relname(char *name)
{
	register char	*old, *new;
	register int	i;
	static char	trimname[MAX_NAME_SIZE + 1];

	if ( name == (char *) 0 )
		return ( "" );
	old = name;
	new = trimname;
	i = MAX_NAME_SIZE;

	while (i--) {
		if ((*new++ = *old++) == ' ') {
			new--;
			break;
		}
	}

	*new = '\0';

	return (trimname);
}
