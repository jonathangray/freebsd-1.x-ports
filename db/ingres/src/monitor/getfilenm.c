#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)getfilenm.c	8.1	12/31/84)



/*
**  GET FILE NAME
**
**	This routine collects a file name up to a newline and returns a
**	pointer to it.  Keep in mind that it is stored in a static
**	buffer.
**
**	Trace Flags:
**		40
*/

char *
getfilenm(void)
{
	static char	filename[81];
	register char	c;
	register int	i;
	register char	*p;

	Oneline = TRUE;
	macinit(getch, 0, 0);

	/* skip initial spaces */
	while ((c = macgetch()) == ' ' || c == '\t')
		continue;

	i = 0;
	for (p = filename; c > 0; ) {
		if (i++ <= 80)
			*p++ = c;
		c = macgetch();
	}
	*p = '\0';
	Prompt = Newline = TRUE;

#ifdef xMTR2
	if (tTf(40, 0))
		printf("filename \"%s\"\n", filename);
#endif
	Oneline = FALSE;
	getc(Input);
	return (filename);
}
