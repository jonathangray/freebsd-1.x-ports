/*
 * active file access functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "libc.h"
#include "news.h"
#include "config.h"
#include "active.h"

/* ordinal numbers of fields */
#define CURRFIELD 2		/* current article # */
#define FLAGFIELD 4		/* y/n/m/x/= flag */

/* flag field values */
#define FLAGOKAY 'y'		/* ordinary unmoderated group */
#define FLAGBAD 'n'		/* unmoderated but locally-restricted group */
#define FLAGMOD 'm'		/* moderated group */
#define FLAGNEVER 'x'		/* unwanted group: don't file in this one */
#define FLAGGOTO '='		/* see another group (following) instead */

/* imports */
extern char *actfind();
extern statust actfload(), actfsync(), actfwrnum();

/* forwards */
extern char *findflag();
FORWARD char *fieldfind();

/* exports */
char actrelnm[] = "active";

/* privates */
static char *actmode = "r+";
static FILE *actfp = NULL;
static struct lastngcache {
	char *lnc_ng;			/* newsgroup name */
	char *lnc_line;			/* matching active file line */
} lnc = { NULL, NULL };

/*
 * return a pointer to the active file entry for ng
 * (or a pointed-to group (by ``=group'')), or 0 if no entry exists.
 * since actlook is called repeatedly for the same newsgroup,
 * actlook caches the last newsgroup looked-up and the result.
 */
char *
actlook(ang)
register char *ang;
{
	register char *ngline, *ng, *flag;
	register int loopbreak = 100;

	if (lnc.lnc_ng != NULL && STREQ(lnc.lnc_ng, ang))
		return lnc.lnc_line;

	if (actload() != ST_OKAY)
		return NULL;
	ng = strsave(ang);
	while ((ngline = actfind(actfp, ng, strlen(ng))) != NULL &&
	    (flag = findflag(ngline)) != NULL && *flag == FLAGGOTO &&
	    --loopbreak > 0) {
		free(ng);
	    	ng = strsvto(flag+1, '\n');	/* follow "=ng" pointer */
	}
	if (loopbreak <= 0)			/* "infinite" loop broken */
		ngline = NULL;

	nnfree(&lnc.lnc_ng);
	lnc.lnc_ng = ng;
	lnc.lnc_line = ngline;
	return ngline;
}

/*
 * Find the active entry for ng (or a pointed-to group (by ``=group''))
 * and add inc to its 2nd field (highest number).
 * Return the resultant number.
 */
long
incartnum(ng, inc)
char *ng;
int inc;
{
	char testnum[40];
	register char *line = actlook(ng);
	register long nextart = -1;

	if (line != NULL) {
		register char *artnum, *pastartnum;

		pastartnum = artnum = fieldfind(line, CURRFIELD);
		if (artnum == NULL)
			return nextart;
		while (isascii(*pastartnum) && isdigit(*pastartnum))
			++pastartnum;
		nextart = atol(artnum) + inc;

		/* update active file article # in place, from nextart */
		if (pastartnum-artnum > sizeof testnum ||
		    !ltozan(testnum, nextart, pastartnum-artnum) ||
		    !ltozan(artnum, nextart, pastartnum-artnum)) {
			(void) fprintf(stderr,
"%s: article number (%ld) too big for group `%s' active field of %d digits\n",
				progname, nextart, ng, pastartnum-artnum);
			return -1;
		}

		/* give the implementation a chance to write line to disk */
		if (actfwrnum(actfp, line) != ST_OKAY) {
			warning("can't update active file", "");
			nextart = -1;
		}
	}
	return nextart;
}

actread()				/* set mode: just reading */
{
	actmode = "r";
}

/*
 * Reload the active file cache.
 */
statust
actload()
{
	register statust status = ST_OKAY;

	if (actfp == NULL &&
	    (actfp = fopenwclex(ctlfile(actrelnm), actmode)) == NULL)
		status |= ST_DROPPED;
	status |= actfload(actfp);
	return status;
}

/*
 * Write back to disk the active file cache, if any, and flush the
 * last-newsgroup-cache, since it refers to the (now invalid) active file cache.
 */
statust
actsync()
{
	register statust status = ST_OKAY;

	if (actfp != NULL) {
		nnfree(&lnc.lnc_ng);
		lnc.lnc_ng = lnc.lnc_line = NULL;
		status |= actfsync(actfp);
		if (nfclose(actfp) == EOF || status != ST_OKAY) {
			warning("error writing `%s'", ctlfile(actrelnm));
			status |= ST_DROPPED;
		}
	}
	actfp = NULL;
	return status;
}

/*
 * Return YES iff any group in ngs (or a pointed-to group (by ``=group''))
 * matches thisflag.
 */
boolean
isflag(ngs, thisflag)
register char *ngs;
int thisflag;
{
	register char *newng, *flag, *ng;
	register boolean result = NO;

	for (ng = ngs; !result && ng != NULL; ng = newng) {
		newng = strchr(ng, NGSEP);
		if (newng != NULL)
			*newng = '\0';		/* restored below */

		flag = findflag(actlook(ng));
		if (flag != NULL && *flag == thisflag)
			result = YES;

		if (newng != NULL)
			*newng++ = NGSEP;	/* point at next group */
	}
	return result;
}

STATIC char *
fieldfind(ngline, fieldno)	/* return address of field "fieldno" in ngline */
register char *ngline;
register int fieldno;
{
	register int field;

	for (field = 1; ngline != NULL && field < fieldno; ++field) {
		ngline = strchr(ngline, ' ');
		if (ngline != NULL)
			ngline++;		/* point at next field */
	}
	return ngline;
}

char *
findflag(ngline)		/* return address of flag field in ngline */
register char *ngline;
{
	return fieldfind(ngline, FLAGFIELD);
}

/*
 * Are any groups in ngs moderated?
 */
boolean
moderated(ngs)
register char *ngs;
{
	return isflag(ngs, FLAGMOD);
}

/*
 * Are any groups in ngs unwanted?
 */
boolean
unwanted(ngs)
register char *ngs;
{
	return isflag(ngs, FLAGNEVER);
}

/*
 * Return 0 or a malloced newsgroup name corresponding to "ong",
 * but without an "=" flag in its active file entry.
 * This is done by tracing the chain of "=ng" pointers (in actlook()), if any.
 */
char *
realngname(ong)
char *ong;
{
	register char *ngline = actlook(ong);

	return (ngline == NULL? NULL: strsvto(ngline, ' '));
}
