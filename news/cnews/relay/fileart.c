/*
 * fileart - file an article, given its temporary file name and its headers
 *
 * It may be desirable to, some day, prevent cross-postings across
 * "universes", where a universe might be "alt" or "comp,news".
 *
 * There are three classes of newsgroup for the purposes of filing:
 * "wanted" (in the active file and missing the "x" flag);
 * "not wanted" ("x"ed in active, or not in active and not matched by sys
 *	file's subscription list for this machine), so ignore it; or
 * "don't know it" (not in active and matched by subscription list,
 *	so file the article in junk once, iff there are no good groups).
 * junk *must* be in the active file or it's an error (ST_DROPPED),
 * but junk may have an "x" flag to prevent filing.
 *
 * Use the active file 'x' flag to snuff groups quietly, even when your
 * subscription list permits them, without filing in junk.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>

#include "libc.h"
#include "news.h"
#include "config.h"
#include "control.h"
#include "active.h"
#include "fileart.h"
#include "mkdirs.h"
#include "headers.h"
#include "article.h"
#include "history.h"
#include "ngmatch.h"
#include "system.h"

#define XREFDELIM ':'

static long artnum;			/* asgnartnum sets artnum */
static int goodngs;			/* asgnartnum reads goodngs */

static boolean debug = NO;

/* imports from news */
extern void prefuse();

/* forwards */
FORWARD void asgnartnum(), gotgoodng(), mkjunklink(), mklinks();
FORWARD boolean openorlink(), mkonelink(), tryartnum();

void
filedebug(state)		/* set debugging state */
boolean state;
{
	debug = state;
}

/*
 * File in the spool directory the article in art & fill in art->a_files.
 * Generate Xref: header if needed (successfully cross-posted).
 * (Thus must be called before emitting any article body!)
 *
 * If a_unlink is true, there is a temp file, so openfirst should
 * be false, and vice versa.
 *
 * If openfirst (!art->a_unlink) is true, fill in a_tmpf with the name of
 * the first link, fopen it (into art->a_artf), and make any remaining links.
 * If openfirst is false, just make links to a_tmpf, which is already
 * open as art->a_artf.  openfirst means "Newsgroups:" was seen in time.
 *
 * Could make empty files for cross-posting "links" here, but make copies into
 * them later.  This sort of thing is needed on old SysVs with NEWSARTS split
 * across partitions (and apparently on Plan 9).
 */
void
fileart(art)
register struct article *art;
{
	register boolean openfirst = !art->a_unlink;
	int junkgroups = 0;		/* count "junked" groups */
	char artnumstr[MAXCOMP];	/* article number in ascii */
	extern boolean genxref;		/* TODO: move into a header */

	if (art->a_filed)
		return;			/* don't file twice */
	artnum = 0;
	goodngs = 0;
	mklinks(art, openfirst, artnumstr, &junkgroups);
	mkjunklink(art, openfirst, artnumstr, &junkgroups);
	/* -g or article crossposted, and article is open? */
	if ((genxref && goodngs > 0 || goodngs > 1) && art->a_artf != NULL)
		emitxref(art);
}

/*
 * extract list of newsgroups (and possibly article numbers) from article
 * headers and history file, as options indicate.  If article numbers are
 * being dictated by incoming Xref: or old history entry, the article numbers
 * will be attached to the end of the group names by a colon as in Xref:
 * (e.g. comp.lang.c:25780 general:12).
 */
char *
extngs(art)
register struct article *art;
{
	register char *ngs = NULL, *site, *groups;
	extern boolean blvxref, dupsokay;	/* TODO: put in header */
	extern char *blvsite, *dupsite;		/* TODO: put in header */

	if (blvxref) {
		if (art->h.h_xref == NULL)
			(void) fprintf(stderr,
				"%s: no Xref: in believe-Xref mode\n",
				progname);
		else {
			for (site = groups = skipsp(art->h.h_xref);
			     *groups != '\0' &&
			     isascii(*groups) && !isspace(*groups); groups++)
				;		/* skip over site name */
			if (*groups != '\0')
				*groups++ = '\0'; /* terminate site name */
			groups = skipsp(groups);
			if (!STREQ(site, blvsite))
				(void) fprintf(stderr,
	"%s: article received from site `%s', not `%s' in believe-Xref mode\n",
					progname, site, blvsite);
			else
				ngs = groups;	/* site is cool; rest is ngs */
		}
	} else if (dupsokay) {
		groups = gethistory(art->h.h_msgid);	/* TODO: free this */
		if (groups == NULL)
			(void) fprintf(stderr,
				"%s: no history entry in duplicate-feed mode\n",
				progname);
		else if ((groups = findfiles(groups)) == NULL)
			(void) fprintf(stderr,
			"%s: expired history entry in duplicate-feed mode\n",
				progname);
		else {
			site = strsvto(art->h.h_path, '!');
			if (!STREQ(site, dupsite))
				(void) fprintf(stderr,
"%s: article received from site `%s', not `%s' in duplicate-feed mode\n",
					progname, site, dupsite);
			else
				/* convert group/art list to group:art list */
				for (ngs = groups; *groups != '\0'; groups++)
					if (*groups == FNDELIM)
						*groups = XREFDELIM;
			free(site);
		}
	} else {
		groups = art->h.h_ctlcmd != NULL? CONTROL: art->h.h_ngs; /* NCMP */
		if (strchr(groups, XREFDELIM) != NULL)
			(void) fprintf(stderr,
				"%s: colon not permitted in Newsgroups: list\n",
				progname);
		else
			ngs = groups;
	}
	if (ngs == NULL)
		art->a_status |= ST_DROPPED;		/* bummer */
	else
		/* convert any spaces to commas for fileart */
		for (groups = ngs; *groups != '\0'; groups++)
			if (*groups == ' ')
				*groups = NGSEP;
	return ngs;
}

/*
 * Store in spooldir.  Link temp file to spooldir/ng/article-number
 * for each ng.  Control messages go in CONTROL, never in all.all.ctl.
 */
STATIC void
mklinks(art, openfirst, artnumstr, junkgroupsp)
register struct article *art;
boolean openfirst;
char *artnumstr;
int *junkgroupsp;
{
	register char *ngs = extngs(art), *ng;
	register char *comma, *numb;

	if (art->a_status&ST_REFUSED)
		(void) fprintf(stderr,
		"%s: mklinks called with ST_REFUSED set (can't happen)\n",
			progname);
	for (; ngs != NULL; ngs = comma) {
		STRCHR(ngs, NGSEP, comma);
		if (comma != NULL)
			*comma = '\0';		/* will be restored below */

		numb = strchr(ngs, XREFDELIM);
		if (numb != NULL)		/* number supplied? */
			*numb++ = '\0';		/* cleave number from group */

		/*
		 * Map group names in Newsgroups: header to local group
		 * names for filing.
		 */
		ng = realngname(ngs);
		if (ng == NULL)
			ng = strsave(ngs);

		/* attempt to file */
		asgnartnum(art, openfirst, ng, numb, artnumstr);

		/*
		 * If no such group in active or link failed, and the group
		 * wasn't 'x'ed in active, but our subscription list permits
		 * this group, then set flag to file it under "junk" later.
		 */
		if ((artnum < 1 || art->a_status != ST_OKAY) &&
		    art->a_status != ST_REFUSED &&
		    ngpatmat(oursys()->sy_trngs, ng))
		    	++*junkgroupsp;
		/*
		 * If article # was assigned & link succeeded,
		 * update art->a_files list for history.
		 */
		if (artnum >= 1 && art->a_status == ST_OKAY)
			gotgoodng(art, ng, artnumstr);
		free(ng);

		if (numb != NULL)		/* number supplied? */
			*--numb = XREFDELIM;	/* restore lost byte */
		if (comma != NULL)
			*comma++ = NGSEP;	/* step past comma */

		/* asgnartnum refused just this ng */
		art->a_status &= ~ST_REFUSED;
	}
}

/*
 * File once in "junk" iff no ngs were filed due to absence from
 * active, but some were permitted by sys.  This will make one junk
 * link, no matter how many bad groups, and only if all are bad
 * (e.g. rec.drugs,talk.chew-the-fat).
 */
STATIC void
mkjunklink(art, openfirst, artnumstr, junkgroupsp)
register struct article *art;
boolean openfirst;
char *artnumstr;
int *junkgroupsp;
{
	if (goodngs != 0)
		return;		/* shouldn't be here, with valid groups */

	if (*junkgroupsp > 0) {
		/*
		 * All groups were "junked".  Try to file this article in
		 * junk.
		 */
		asgnartnum(art, openfirst, JUNK, (char *)NULL, artnumstr);
		if (artnum >= 1 && art->a_status == ST_OKAY) {
			gotgoodng(art, JUNK, artnumstr);
			art->a_status |= ST_JUNKED;
		} else
		/* couldn't file article in junk.  why? */
		if (art->a_status&ST_REFUSED) {	/* junk is 'x'ed */
			prefuse(art);
			(void) printf(
		"no known groups in `%s' and %s group is excluded in active\n",
				art->h.h_ngs, JUNK);
		} else {			/* junk is missing? */
			static boolean warned = NO;

			art->a_status |= ST_REFUSED|ST_DROPPED;
			if (!warned) {
				warned = YES;
				(void) fprintf(stderr,
		"%s: can't file in %s group; is it absent from active?\n",
					progname, JUNK);
			}
			prefuse(art);
			(void) printf(
			"no known groups in `%s' and no %s group\n",
				art->h.h_ngs, JUNK);
		}
	} else {
		extern boolean histreject;

		/*
		 * Groups were permitted by subscription list, but all
		 * were 'x'ed in active, or otherwise refused.
		 */
		if (histreject)
			history(art, NOLOG);
		prefuse(art);
		(void) printf("all groups `%s' excluded in active\n",
			art->h.h_ngs);
		art->a_status |= ST_REFUSED;
	}
}

/*
 * Append ng/artnumstr to art's list of files, and bump goodngs.
 */
STATIC void
gotgoodng(art, ng, artnumstr)
struct article *art;
char *ng, *artnumstr;
{
	++goodngs;
	histupdfiles(art, ng, artnumstr);
}

/*
 * Assign a permanent name and article number to the temporary name
 * art->a_tmpf in newsgroup "ng" & store the ascii form of the article
 * number into "artnumstr", returning the article number in "artnum".
 * If numb is non-null, it's the ascii article number to file under.
 *
 * If openfirst is true and goodngs is zero, set inname to artname,
 * fopen artname and store the result in art->a_artf.
 */
STATIC void
asgnartnum(art, openfirst, ng, numb, artnumstr)
struct article *art;
boolean openfirst;				/* open first link? */
register char *ng;				/* read-only */
char *numb, *artnumstr;
{
	register char *slashng;			/* a group, slashed */

	/* field active 'x' flag: don't file this group, quietly */
	if (unwanted(ng)) {
		artnum = -1;
		art->a_status |= ST_REFUSED;
		return;
	}

	slashng = strsave(ng);
	mkfilenm(slashng);			/* relative to spooldir */
	if (numb != NULL) {			/* number supplied? */
		artnum = atol(numb);		/* believe number */
		if (!tryartnum(art, openfirst, slashng, artnumstr)) {
			(void) fprintf(stderr,
		"%s: article # %ld in directory `%s' supplied but occupied!\n",
				progname, artnum, slashng);
			art->a_status |= ST_DROPPED;
		}
	} else
		while ((artnum = nxtartnum(ng)) >= 1)
			if (tryartnum(art, openfirst, slashng, artnumstr))
				break;
	free(slashng);
}

/*
 * Construct a link name (slashng/artnum) for this article,
 * and try to link art to it.
 *
 * We changed directory to spooldir in main(), so the generated name
 * is relative to spooldir, therefore artname can be used as is.
 *
 * Return value is identical to mkonelink's.
 */
STATIC boolean
tryartnum(art, openfirst, slashng, artnumstr)
register struct article *art;
boolean openfirst;
register char *slashng;
char *artnumstr;			/* side-effect returned here */
{
	register char *artname;		/* article file name */
	register boolean ret;

	(void) sprintf(artnumstr, "%ld", artnum);
	artname = nemalloc((unsigned) (strlen(slashng) +
		STRLEN(SFNDELIM) + strlen(artnumstr) + SIZENUL));
	(void) strcpy(artname, slashng);
	(void) strcat(artname, SFNDELIM);
	(void) strcat(artname, artnumstr);
#ifdef notdef
	char *tartname = strsave(artfile(artname));
	free(artname);
	artname = tartname;
#endif
	ret = mkonelink(art, artname, openfirst);
	free(artname);
	return ret;
}

/*
 * Try to link art to artname.
 * If the attempt fails, maybe some intermediate directories are missing,
 * so create any missing directories and try again.  If the second attempt
 * also fails, look at errno; if it is EEXIST, artname already exists
 * (presumably because the active file is out of date, or the existing
 * file is a directory such as net/micro/432), so indicate that higher
 * levels should keep trying, otherwise we are unable to create the
 * necessary directories, so complain and set bad status in art.
 *
 * Returns YES iff there is no point in trying to file this article again,
 * usually because it has been successfully filed, but sometimes because
 * the necessary directories cannot be made.
 */
STATIC boolean
mkonelink(art, artname, openfirst)
register struct article *art;
register char *artname;
boolean openfirst;
{
	if (openorlink(artname, art, openfirst, goodngs))
		return YES;
	else {
		(void) mkdirs(artname, getuid(), getgid());
		if (openorlink(artname, art, openfirst, goodngs))
			return YES;
		else if (errno != EEXIST) {
			warning("can't link to `%s'", artname);
			art->a_status |= ST_DROPPED;
			return YES;		/* hopeless - give up */
		} else
			return NO;
	}
}

/*
 * Try to make a link of art (actually art->a_tmpf) to artname.
 * If no links have been made yet, record and open artname iff no
 * link yet exists to that name (by any file, to avoid overwriting
 * existing articles, e.g. due to an out-of-date active file).
 * If links already exist, try to make artname a link to the first link
 * (art->a_tmpf).
 *
 * Liberally sprinkled with debugging output, as this is the heart
 * of article filing.
 */
STATIC boolean
openorlink(artname, art, openfirst, goodngcnt)
register char *artname;
register struct article *art;
boolean openfirst;			/* open art's first link? */
int goodngcnt;				/* count of good news groups */
{
	register boolean worked;		/* open or link worked? */

	errno = 0;			/* paranoia */
	if (openfirst && goodngcnt == 0) {
		if (debug)
			(void) fprintf(stderr, "opening `%s'... ", artname);
		nnfree(&art->a_tmpf);
		art->a_tmpf = strsave(artname);
		art->a_artf = fopenexcl(art->a_tmpf);
		worked = art->a_artf != NULL;
	} else {
		if (debug)
			(void) fprintf(stderr, "linking `%s' to `%s'... ",
				art->a_tmpf, artname);
		worked = link(art->a_tmpf, artname) == 0;
		if (!worked)
			/*
			 * If art->a_tmpf really *is* a temporary name (because
			 * the whole header didn't fit in core), then this will
			 * produce a symbolic link to a non-existent name when
			 * art->a_tmpf is unlinked, which will be soon.
			 * Moral: don't run Eunice on your PDP-11.
			 */
			worked = symlink(fullartfile(art->a_tmpf), artname)==0;
	}
	if (debug)
		if (worked)
			(void) fprintf(stderr, "success.\n");
		else
			warning("failed.", "");
	return worked;
}

#if 0
/* called after copyart */
mkcopies(art)
register struct article *art;
{
	if (any links failed & empty files could be made)
		for (each failed link)
			copy open 1st link to this one;
}
#endif
