/*
 * active file access functions (small, slow, on-disk version)
 */

#include <stdio.h>
#include <sys/types.h>
#include "news.h"
#include "fgetmfs.h"

/* private */
static long fpos = -1;		/* set by actfind, read by actwrnum */

/* ARGSUSED */
statust
actfload(fp)
FILE *fp;
{
	return ST_OKAY;
}

/* ARGSUSED */
statust
actfsync(fp)
FILE *fp;
{
	return ST_OKAY;
}

/*
 * search on stream "fp" for group "ng" and return a pointer
 * to the active file line for it, or 0 if none.
 * actload has previously been called.
 */
char *
actfind(fp, ng, nglen)
register FILE *fp;
register char *ng;
register int nglen;
{
	static char *line = NULL;

	if (line != NULL)
		free(line);
	rewind(fp);
	for (fpos = ftell(fp); (line = fgetms(fp)) != NULL;
	     fpos = ftell(fp), free(line))
		if (STREQN(line, ng, nglen) && line[nglen] == ' ')
			return line;		/* free on next entry */
	return NULL;
}

statust
actfwrnum(fp, line)		/* overwrite last line found with "line" */
register FILE *fp;
char *line;
{
	if (fseek(fp, fpos, 0) != EOF &&	/* back up */
	    fputs(line, fp) != EOF && fflush(fp) != EOF)
		return ST_OKAY;
	else
		return ST_DROPPED;
}
