#include <ingres.h>
#include "scanner.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)s_inout.c	8.1	12/31/84)

/* TWO CHARACTER STACK FOR 'UNGETC' BACKUP */
char	Pchar[2];
int	Pctr;


/*
** BACKUP
** saves the character argument in the global stack 'Pchar'
**/
void
backup(char chr)
{
	extern int	yyline;

	if (Pctr == 2)
		syserr("overflow in backup()");
	Pchar[Pctr++] = chr;
	if (chr == '\n')
		yyline--;
}
