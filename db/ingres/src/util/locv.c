#include <stdio.h>

#include "sccs.h"
#include <useful.h>

SCCSID(@(#)locv.c	8.2	(Ingres)	2/8/85)

/*
** locv
**	Convert a long integer into a string.
**
** Notes:
**	This is a replacement for the locv routine that was
**	originally written in assembler. I am dropping a lot
**	of the general purpose stuff, and basically replacing
**	it with a sprintf for ease.
**
** Returns
**	A string representation of a long
*/
char *
locv(long i)
{
	static	char	buf[LONG_BUFSZ];

	(void) sprintf(buf,"%ld",i);
	return(buf);
}/* locv */
