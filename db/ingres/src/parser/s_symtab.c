#include <ingres.h>
#include <aux.h>
#include "scanner.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)s_symtab.c	8.2	3/23/85)

/*
** SYMENT
**	enter a symbol into the symbol table
*/
char *
syment(void *ptr, int len1)
{
	register char	*cp;
	register int	len;

	len = len1;
	cp = need(Sbuf, len);
	bmove(ptr, cp, len);
	return(cp);
}

/*
** FREESYM
**	free all entries in the symbol table
*/
void
freesym(void)
{
	initbuf(Sbuf, SBUFSIZ, SBUFOFLO, neederr);
}
