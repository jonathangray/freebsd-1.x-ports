/*
 * news sys file reading functions (slow, small, on-disk version)
 */

#include <stdio.h>
#include <sys/types.h>
#include "news.h"
#include "ngmatch.h"
#include "system.h"

/* imports */
extern struct system *currsys, *firstsys;

/* forwards */
FORWARD void freecurrsys();

void
remmysys(sys)
struct system *sys;
{
	/* no cache */
}

struct system *
mysysincache()
{
	return NULL;
}

/* ARGSUSED */
void
setupsys(fp)				/* reuse currsys */
FILE *fp;
{
	freecurrsys();
}

boolean
donesys()
{
	if (firstsys != NULL) {		/* parsed an entry? */
		firstsys = NULL;	/* not cached, but currsys still valid */
		return YES;
	} else
		return NO;		/* was a comment */
}

void
rewsys(fp)
FILE *fp;
{
	if (fp != NULL)
		(void) rewind(fp);
}

void
advcurrsys()
{
	/*
	 * the sys file is not in core, so we must not change currsys
	 * to ensure that it gets freed later by freecurrsys().
	 */
}

/*
 * Free current sys entry & associated memory.  Zero currsys too.
 */
STATIC void
freecurrsys()
{
	if (currsys != NULL) {
		nnfree(&currsys->sy_name);
		nnfree(&currsys->sy_excl);
		/* allow sharing of ngs and distr */
		if (currsys->sy_ngs == currsys->sy_distr)
			currsys->sy_distr = NULL;
		else
			nnfree(&currsys->sy_distr);
		nnfree(&currsys->sy_ngs);
		nnfree(&currsys->sy_cmd);
		/* allow sharing of trngs and trdistr */
		if (currsys->sy_trdistr != currsys->sy_trngs)
			ngfree(currsys->sy_trdistr);
		currsys->sy_trdistr = NULL;
		ngfree(currsys->sy_trngs);
		currsys->sy_trngs = NULL;
		nnafree(&currsys);
	}
}
