/*
 * csfgetln - read an arbitrarily long, possibly-continued line.
 * continuation rules are as per normal Unix conventions:
 * "# comment" lines are ignored; otherwise, "line\" is continued.
 * return value is malloced and must be freed by caller.
 */

#include <stdio.h>
#include <sys/types.h>
#include <fgetfln.h>
#include "libc.h"

#define COMMCHAR '#'
#define CONTCHAR '\\'

char *
csfgetln(fp, limit, skipflag)
FILE *fp;
register int limit;
int skipflag;			/* skip leading whitespace on continuations? */
{
	register char *line, *contlin, *oldline;
	int length = 0, contleng = 0;

	do {
		line = fgetfln(fp, limit, &length);
		if (line == NULL)
			return NULL;		/* EOF */
	} while (line[0] == COMMCHAR);		/* ignore comments */
	if (limit >= 0) {
		limit -= length;
		if (limit <= 0)
			return line;		/* enough bytes consumed */
	}
	line = strsave(line);
	while (length > 1 &&
	    line[length-2] == CONTCHAR && line[length-1] == '\n') {
		line[length-2] = '\0';		/* stomp "\\\n" */
		length -= 2;			/* compensate */
		contlin = fgetfln(fp, limit, &contleng);
		if (contlin == NULL)
			break;			/* EOF */
		oldline = line;
		if (skipflag)
			while (*contlin == ' ' || *contlin == '\t')
				contlin++, contleng--;
		line = str3save(oldline, "", contlin);
		free(oldline);
		length += contleng;
		if (limit >= 0) {
			limit -= contleng;
			if (limit <= 0)
				return line;	/* enough bytes consumed */
		}
	}
	return line;
}
