#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>

#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIlength.c	8.1	12/31/84)



/*
**	determines the length of a string.
**	if a null byte cannot be found after 255 chars
**	the the length is assumed to be 255.
*/
int
IIlength(char *s)
{
	int	i;

	return(((i = strlen(s)) > MAX_FIELD_SIZE) ? MAX_FIELD_SIZE : i);
}
