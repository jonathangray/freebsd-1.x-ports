/*
 * fgetmfs family emulation on top of fgetfln
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "news.h"
#include "libc.h"
#include <fgetfln.h>
#include <fgetmfs.h>		/* compatibility header */

char *
fgetmfs(fp, limit, cont)
FILE *fp;
int limit;
int cont;
{
	if (cont == CONT_NOSPC && limit == -1)
		return cfgetms(fp);
	else if (cont == CONT_NOSPC)
		return csfgetln(fp, limit, 1);
	else if (cont == CONT_SPC)
		return csfgetln(fp, limit, 0);
	else if (limit == -1)
		return fgetms(fp);
	else {
		register char *line = fgetfln(fp, limit, (int *)NULL);

		return line == NULL? NULL: strsave(line);
	}
}

char *
fgetms(fp)
FILE *fp;
{
	register char *line = fgetln(fp);

	return line == NULL? NULL: strsave(line);
}

char *
cfgetms(fp)
FILE *fp;
{
	return cfgetln(fp);
}
