#include <stdio.h>
#include <ingres.h>
#include "sccs.h"

SCCSID(@(#)set_si_buf.c	8.1	12/31/84)

/*
**  SET_SI_BUF -- set standard input buffer unconditionally
**
**	This routine sets the standard input buffer to give
**	buffered input.  The buffer is contained internally.
**
**	Parameters:
**		none
**
**	Returns:
**		TRUE.
**
**	Side Effects:
**		The standard input is left buffered.
**
**	Trace Flags:
**		none
*/

int
set_si_buf(void)
{
	static char	buffer[BUFSIZ];

	setbuf(stdin, buffer);
	return (TRUE);
}
