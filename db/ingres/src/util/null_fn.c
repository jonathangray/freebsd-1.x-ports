#include "sccs.h"

SCCSID(@(#)null_fn.c	8.2	2/8/85)

/*
**  NULL_FN -- A null function
**
**	This routine does absolutely nothing at all.
**	(Small serious note: this routine is used in the definitions
**		of Ingres functions, when nothing is to be done. )
**
**	Algorithm:
**		none.
**
**	Parameters:
**		none.
**
**	Returns:
**		zero
**
**	Side Effects:
**		none.
**
**	Defined Constants:
**		none.
**
**	Defines:
**		null_fn
**
**	Requires:
**		nothing.
**
**	Required By:
**		Lots (this system doesn't do much).
**		Many Ingres function definitions.
**
**	Files:
**		none.
**
**	Compilation Flags:
**		none.
**
**	Trace Flags:
**		none.
**
**	Diagnostics:
**		none.
**
**	Syserrs:
**		none.
**
**	Deficiencies:
**		It should do nothing faster.
**
**	History:
**		5/12/80 (eric & polly) -- written.
**
**	Version:
**		8.2
**
**	WARNING:
**		Do not use this routine if you want to do something.
*/

void
null_fn(void)
{
}
