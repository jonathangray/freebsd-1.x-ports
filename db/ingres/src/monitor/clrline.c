#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)clrline.c	8.1	12/31/84)



/*
**  Clear Input Line
**
**	This routine removes the newline following a monitor command
**	(such as \t, \g,  etc.)  Any other characters are processed.
**	Hence, \t\g\t will work.  It also maintains the
**	Newline flag on command lines.  It will make certain that
**	the current line in the query buffer ends with a newline.
**
**	The flag 'noprompt' if will disable the prompt character if set.
**	Otherwise, it is automatically printed out.
**
**	Uses trace flag 8
*/
void
clrline(int noprompt)
{
	register char	c;

	if (!Newline)
		putc('\n', Qryiop);
	Newline = TRUE;
	/* if char following is a newline, throw it away */
	c = getch();
	Prompt = (c == '\n');
	if (!Prompt)              /*  next char != newline K.Okamoto */ {
		ungetc(c, Input);
	} else {
		if (!noprompt)
			prompt(0);
	}
}
