#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "buf.h"

#include "protos.h"

SCCSID(@(#)branch.c	8.1	12/31/84)



/*
**  BRANCH
**
**	The "filename" following the \b op must match the "filename"
**	which follows some \k command somewhere in this same file.
**	The input pointer is placed at that point if possible.  If
**	the label does not exist, an error is printed and the next
**	character read is an EOF.
**
**	Trace Flags:
**		33
*/
void
branch(void)
{
	register char	c;
	register int	i;

#ifdef xMTR2
	if (tTf(33, -1))
		printf(">>branch: ");
#endif

	/* see if conditional */
	while ((c = getch()) > 0)
		if (c != ' ' && c != '\t')
			break;
	if (c == '?') {
		/* got a conditional; evaluate it */
		Oneline = TRUE;
		macinit(getch, 0, 0);
		i = expr();

		if (i <= 0) {
			/* no branch */
#ifdef xMTR2
			if (tTf(33, 0))
				printf("no branch\n");
#endif
			getfilenm();
			return;
		}
	} else {
		ungetc(c, Input);
	}

	/* get the target label */
	if (branchto(getfilenm()) == 0)
		if (branchto(macro("{default}")) == 0) {
			GiveEof = TRUE;
			printf("Cannot branch\n");
		}
}

int
branchto(char *label)
{
	char		target[100];
	register char	c;

	smove(label, target);
/*	if (rewind(Input)) {
 *		printf("Cannot branch on a terminal\n");
 *		return (1);
 *	}
*/

	/* search for the label */
	while ((c = getch()) > 0) {
		if (c != '\\') {
			continue;
		}
		if (getescape(0) != C_MARK) {
			continue;
		}
		if (strcmp(getfilenm(), target) == 0) {
			return(1);
		}
	}
	return(0);
}
