/*
 * news sys file reading functions (fast, big, in-memory version)
 */

#include <stdio.h>
#include <sys/types.h>
#include "news.h"
#include "ngmatch.h"
#include "system.h"

/* imports */
extern struct system *currsys, *firstsys;

/* private */
static struct system *thissys = NULL;

void
remmysys(sys)				/* remember this system */
struct system *sys;
{
	thissys = sys;
}

struct system *
mysysincache()				/* optimisation */
{
	return thissys;
}

void
setupsys(fp)
FILE *fp;
{
	rewind(fp);
}

boolean
donesys()
{
	return NO;
}

/* ARGSUSED */
void
rewsys(fp)
FILE *fp;
{
	currsys = firstsys;
}

void
advcurrsys()	/* advance currsys to the next in-core sys entry, if any. */
{
	if (currsys != NULL)
		currsys = currsys->sy_next;
}
