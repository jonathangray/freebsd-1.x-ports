/*
 * simulate 4.4BSD fgetline using the more general fgetfln primitives
 */

#include <stdio.h>
#include <fgetfln.h>

char *
fgetline(fp, lenp)
FILE *fp;
register int *lenp;
{
	register char *line = fgetfln(fp, -1, lenp);

	if (line != NULL)
		(void) dogets(line, lenp);	/* stomp innocent newline */
	return line;
}
