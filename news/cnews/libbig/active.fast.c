/*
 * active file access functions (big, fast, in-memory version)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include <sys/stat.h>
#include "libc.h"
#include "news.h"
#include "config.h"
#include "active.h"

/* private */
static char *active = NULL;	/* cache: points at entire active file */
static int actsize;		/* bytes in active: type int fixed by fread */
static char **actlnps;		/* point at lines in active file */
static unsigned actlines;	/* lines in actlnps actually used */

/* imports from active.c */
extern char actrelnm[];

/* forwards */
FORWARD statust actmkindx();

statust
actfload(fp)
FILE *fp;
{
	statust status = ST_OKAY;

	if (fp != NULL && active == NULL) {
		struct stat sb;

		errno = 0;
		if (fstat(fileno(fp), &sb) < 0)
			warning("can't fstat `%s'", ctlfile(actrelnm));
		else if (actsize = sb.st_size, /* squeeze into an int */
		    (unsigned)actsize != sb.st_size)
			warning("`%s' won't fit into memory", ctlfile(actrelnm));
		else if ((active = malloc((unsigned)actsize+1)) == NULL)
			warning("can't allocate memory for `%s'",
				ctlfile(actrelnm));
		else {
			rewind(fp);
			/*
			 * If we read with fgetms, we might be able to avoid
			 * calling linescan().
			 */
			if (fread(active, 1, actsize, fp) != actsize) {
				warning("error reading `%s'", ctlfile(actrelnm));
				status |= ST_DROPPED;
			} else
				status |= actmkindx();
		}
		if (active == NULL)
			status |= ST_DROPPED;	/* give up! */
		if (status != ST_OKAY) {
			nnfree(&active);
			nnafree(&actlnps);
		}
	}
	return status;
}

static statust
actmkindx()			/* build actlnps index for active */
{
	register statust status = ST_OKAY;
	unsigned lnpsz;
	int maxlines;		/* should this really be long? */

	active[actsize] = '\0';		/* make a proper string */
	/* +1 for a possible partial line +1 for a dummy to check overflow */
	maxlines = charcount(active, '\n') + 2;
	lnpsz = sizeof(char *) * (long) maxlines;
	if (lnpsz != sizeof(char *) * (long)maxlines ||
	    (actlnps = (char **)malloc(lnpsz)) == NULL) {
		warning("`%s' index won't fit in memory", ctlfile(actrelnm));
	    	status |= ST_DROPPED;
	} else {
		actlnps[maxlines - 2] = "";	/* in case no partial line */
		actlnps[maxlines - 1] = "";	/* end sentinel */
		actlines = linescan(active, actlnps, maxlines);
		if (actlines >= maxlines) {
			(void) fprintf(stderr,
				"%s: too many newsgroups in `%s' (can't happen)\n",
				progname, ctlfile(actrelnm));
			status |= ST_DROPPED;
		}
	}
	return status;
}

/*
 * Store in lnarray the addresses of the starts of lines in s.
 * Return the number of lines found; if greater than nent,
 * store only nent and return nent.
 * Thus lnarray should be one bigger than needed to detect overflow.
 */
int
linescan(s, lnarray, nent)
char *s;
char **lnarray;
register int nent;
{
	register char **lnarrp = lnarray;
	register int i = 0;
	register char *nlp = s;

	if (i < nent)
		*lnarrp++ = nlp;
	while (++i < nent && (nlp = strchr(nlp, '\n')) != NULL && *++nlp != '\0')
		*lnarrp++ = nlp;
	return i;		/* number of addrs stored */
}

statust
actfsync(fp)			/* write to disk, fp is open */
FILE *fp;
{
	statust status = ST_OKAY;

	rewind(fp);
	if (active != NULL) {
		if (fwrite(active, actsize, 1, fp) != 1)
			status |= ST_DROPPED;	/* serious loss */
		nnfree(&active);
		nnafree(&actlnps);
	}
	return status;
}

/* ARGSUSED fp */
char *
actfind(fp, ng, nglen)
FILE *fp;
register char *ng;
register int nglen;
{
	register char *pos;
	register unsigned line = 0;

	while (pos = actlnps[line], line++ < actlines && pos[0] != '\0')
		if (STREQN(pos, ng, nglen) && pos[nglen] == ' ')
			return pos;
	return NULL;
}

/* ARGSUSED */
statust
actfwrnum(fp, pos)
FILE *fp;
char *pos;
{
	return ST_OKAY;
}
