#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include  <ingres.h>
#include  "scanner.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)s_comment.c	8.2	2/8/85)

/*
** COMMENT
** scans comments (as delimited by the tokens 'Tokens.bgncmnt'
** and 'Tokens.endcmnt') and removes them from the query text.
*/
int
comment(void)
{
	register int		i, l;
	register struct optab	*op;
	register	char	*sp;
	char			buf[3];

	/* find the end_of_comment operator */
	for (op = Optab; op->term; op++)
		if (op->token == Tokens.endcmnt)
			break;
	if (!op->term)
		syserr("no end_of_comment token");

	/* scan for the end of the comment */
	l = strlen(op->term);
	for (i = 0,sp = buf; i < l; sp++, i++)		/* set up window on input */
		if ((*sp = get_scan(NORMAL)) <= 0)
			/* non-terminated comment */
			par_error(COMMTERM, FATAL, 0, 0, 0);		/* must end parsing */
	while (!bequal(buf, op->term, l)) {
		/* move window on input */
		for (sp = buf,i = 0; i < l-1; i++,sp++)
			*sp = *(sp+1);
		if (( *sp = get_scan(NORMAL)) <= 0)
			/* non terminated comment */
			par_error(COMMTERM, FATAL, 0, 0, 0);		/* must end parsing */
	}
	return (0);
}
