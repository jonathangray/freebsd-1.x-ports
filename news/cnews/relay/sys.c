/*
 * news sys file reading functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include <sys/stat.h>

#include "libc.h"
#include "fgetmfs.h"
#include "news.h"
#include "config.h"
#include "batchnames.h"
#include "ngmatch.h"
#include "system.h"

#ifndef CMDPFX
#define CMDPFX "uux - -r -z "	/* prefix of default command */
#endif
#define CMDSFX "!rnews"		/* suffix of same */

/* private */
static FILE *fp = NULL;			/* stream for ctlfile(filerelname) */
static char filerelname[] = "sys";	/* filename relative to $NEWSCTL */

/* forward decls */
FORWARD char *parsecolon(), *reparse();
FORWARD void readsys(), parsesysln(), parse(), parseflags(), newartfile();
FORWARD int spacein();

/* exports */
struct system *firstsys = NULL;	/* cache: 1st sys of in-core sys file */
struct system *currsys = NULL;	/* current system */

/* imports */
extern boolean donesys();
extern struct system *mysysincache();
extern void rewsys(), remmysys(), advcurrsys(), setupsys();

struct system *
oursys()			/* return our sys entry */
{
	register struct system *sys = mysysincache();
	static struct system fakesys;

	if (sys == NULL) {
		register char *host = hostname();

		rewsys(fp);
		while ((sys = nextsys()) != NULL && !STREQ(sys->sy_name, host))
			;
		if (sys == NULL) {
			/* no entry: cook one up; no need to malloc members */
			sys = &fakesys;
			sys->sy_name = host;
			sys->sy_excl = NULL;
			sys->sy_ngs = "all";
			sys->sy_distr = sys->sy_ngs;
			sys->sy_flags = 0;
			sys->sy_lochops = 0;
			sys->sy_cmd = "";
			sys->sy_trngs = ngparse(strsave(sys->sy_ngs));
			sys->sy_trdistr = sys->sy_trngs;
			sys->sy_next = NULL;
		}
		remmysys(sys);			/* for future reference */
	}
	return sys;
}

/*
 * Return the next sys entry, which may span multiple lines.
 * Returned pointer points at a struct whose lifetime (and that of its
 * members) is not promised to last beyond the next call to nextsys();
 * copy it and its pointed-to strings if you need them for longer.
 *
 * Note that readsys() reads one entry on small systems, but the entire
 * sys file on big systems, so the common code in this file is subtle.
 */
struct system *
nextsys()
{
	struct system *retsys;

	if (firstsys == NULL && fp == NULL)
		if ((fp = fopenwclex(ctlfile(filerelname), "r")) == NULL)
			return NULL;
	if (fp != NULL && firstsys == NULL)
		readsys();
	retsys = currsys;
	advcurrsys();
	return retsys;
}

/*
 * On small systems, read one entry; else read whole sys file (done once only).
 * Ignores '#' comments and blank lines; uses cfgetms to read possibly-
 * continued lines of arbitrary length.
 */
STATIC void
readsys()
{
	register char *sysline;

	setupsys(fp);		/* reuse currsys or rewind sys file */
	while ((sysline = cfgetms(fp)) != NULL) {
		if (sysline[0] != '#' && sysline[0] != '\n')
			parsesysln(sysline);
		free(sysline);
		if (donesys())		/* early exit if on disk (small) */
			return;
	}
	(void) nfclose(fp);
	fp = NULL;
	rewsys(fp);
}

static char *curr, *next;			/* parsing state */

/*
 * Parse (and modify) sysline into *currsys, which is malloced here
 * and freed iff on a small system, in readsys(), see freecursys().
 *
 * Side-effect: sysline has a trailing newline removed.
 */
STATIC void
parsesysln(sysline)
register char *sysline;
{
	register struct system *sysp =(struct system *)nemalloc(sizeof *sysp);
	char *flagstring;

	trim(sysline);
	next = sysline;
	parse(&sysp->sy_name);
	parse(&sysp->sy_ngs);
	parse(&flagstring);
	parse(&sysp->sy_cmd);
	errno = 0;
	if (spacein(sysp->sy_name))
		errunlock(
		"whitespace in system name (or exclusions) of sys entry for `%s'",
			sysp->sy_name);
	if (spacein(sysp->sy_ngs))
		errunlock(
		"whitespace in newsgroups (or distributions) of sys entry for `%s'",
			sysp->sy_name);
	if (spacein(flagstring))
		errunlock("whitespace in flags of sys entry for `%s'",
			sysp->sy_name);
	/* could check for extra fields here */

	parseflags(flagstring, sysp);
	free(flagstring);		/* malloced by parse */
	sysp->sy_next = NULL;

	/* reparse for embedded slashes */
	sysp->sy_excl = reparse(sysp->sy_name, '/');
	sysp->sy_distr = reparse(sysp->sy_ngs, '/');
	/*
	 * N.B.: ngparse will chew up sy_ngs.  not copying sy_ngs saves
	 * a lot of memory when you have a big sys file.
	 */
	sysp->sy_trngs = ngparse(sysp->sy_ngs);
	if (sysp->sy_distr == NULL) {	/* default distr is ngs... */
		sysp->sy_distr =   sysp->sy_ngs;
		sysp->sy_trdistr = sysp->sy_trngs;
	} else {
		if (strchr(sysp->sy_distr, '/') != NULL)
			errunlock(
			"slash in distribution subfield of sys entry for `%s'",
				sysp->sy_name);
		/* N.B.: ngparse will chew up sy_distr. */
		sysp->sy_trdistr = ngparse(sysp->sy_distr);
	}

	sysdeflt(sysp);			/* fill in any defaults */

	/* stash *sysp away on the tail of the current list of systems */
	*(firstsys == NULL? &firstsys: &currsys->sy_next) = sysp;
	currsys = sysp;
}

STATIC int
spacein(s)
register char *s;
{
	return strchr(s, ' ') != NULL || strchr(s, '\t') != NULL;
}

/*
 * fill in defaults in sysp.
 *
 * expand a name of "ME" to hostname().
 * If an empty batch file name was given, supply a default
 * ($NEWSARTS/BTCHPFX system BTCHSFX).
 * Prepend $NEWSARTS/BTCHDIR to relative file names.
 * If an empty command was given, supply a default (uux - -r -z system!rnews).
 * (This *is* yucky and uucp-version-dependent.)
 */
void
sysdeflt(sysp)
register struct system *sysp;
{
	if (STREQ(sysp->sy_name, "ME")) {
		free(sysp->sy_name);	/* malloced by parse */
		sysp->sy_name = strsave(hostname());
	}
	if (sysp->sy_flags&FLG_BATCH && sysp->sy_cmd[0] == '\0') {
		register char *deffile =
			str3save(BTCHPFX, sysp->sy_name, BTCHSFX);

		/* frees old sysp->sy_cmd, deffile */
		newartfile(sysp, deffile);
	} else if (sysp->sy_flags&FLG_BATCH && sysp->sy_cmd[0] != FNDELIM) {
		register char *absfile = str3save(BTCHDIR, sysp->sy_cmd, "");

		/* frees old sysp->sy_cmd, absfile */
		newartfile(sysp, absfile);
	} else if (!(sysp->sy_flags&FLG_BATCH) && sysp->sy_cmd[0] == '\0') {
		free(sysp->sy_cmd);	/* malloced by parse */
		sysp->sy_cmd = str3save(CMDPFX, sysp->sy_name, CMDSFX);
	}
}

STATIC void
newartfile(sysp, file)		/* replace sysp->sy_cmd with artfile(file) */
register struct system *sysp;
register char *file;
{
	free(sysp->sy_cmd);		/* malloced by parse */
#ifdef BATCHSPOOL				/* UUNET special */
	sysp->sy_cmd = str3save(BATCHSPOOL, SFNDELIM, file);
#else
	sysp->sy_cmd = strsave(artfile(file));
#endif
	free(file);
}

/*
 * Parse "next" to colon into malloced storage, return its ptr via "into".
 * *into is freed iff on a small system, in readsys(), see freecursys().
 */
STATIC void
parse(into)
register char **into;
{
	curr = next;
	if (curr == NULL)
		*into = strsave("");
	else {
		next = parsecolon(curr);
		*into = strsave(curr);
	}
}

STATIC char *
parsecolon(line)		/* return NULL or ptr. to byte after colon */
char *line;
{
	register char *colon;

	STRCHR(line, ':', colon);
	if (colon != NULL)
		*colon++ = '\0';
	return colon;
}

/*
 * replace "delim" in "field" with a NUL and return the address of the byte
 * after the NUL (the address of the second subfield), or NULL if no
 * "delim" was present.
 */
STATIC char *
reparse(field, delim)
char *field;
int delim;
{
	register char *delimp = strchr(field, delim);

	if (delimp != NULL)
		*delimp++ = '\0';
	return delimp;
}

/*
 * Parse sys file flags into sysp.
 */
STATIC void
parseflags(flags, sysp)
register char *flags;
register struct system *sysp;
{
	sysp->sy_flags = 0;
	sysp->sy_lochops = 0;		/* default L value */
	errno = 0;
	for (; *flags != '\0'; flags++)
		switch (*flags) {
		case 'A':
			errunlock("A news format not supported", "");
			/* NOTREACHED */
		case 'B':		/* mostly harmless */
			break;
		case 'f':
			sysp->sy_flags |= FLG_BATCH|FLG_SZBATCH;
			break;
		case 'F':
			sysp->sy_flags |= FLG_BATCH;
			break;
		case 'I':		/* NNTP hook: write msgids, !files */
			sysp->sy_flags |= FLG_BATCH|FLG_IHAVE;
			break;
		case 'L':		/* Ln */
			sysp->sy_flags |= FLG_LOCAL;
			sysp->sy_lochops = 0;
			while (isascii(flags[1]) && isdigit(flags[1])) {
				sysp->sy_lochops *= 10;
				sysp->sy_lochops += *++flags - '0';
			}
			break;
		case 'm':		/* send only moderated groups */
			sysp->sy_flags |= FLG_MOD;
			break;
		case 'N':
			errunlock(
	"The N flag is a wasteful old kludge; see the I flag instead.", "");
			/* NOTREACHED */
		case 'n':		/* NNTP hook: write files+msgids */
			sysp->sy_flags |= FLG_BATCH|FLG_NBATCH;
			break;
		case 'u':		/* send only unmoderated groups */
			sysp->sy_flags |= FLG_UNMOD;
			break;
		case 'U':		/* mostly harmless */
			break;
		case 'H':		/* write history entry? sorry */
		case 'S':	/* duplicate the work of the shell.  bzzt */
		case 'M':		/* multicast: obs., see batcher */
		case 'O':		/* multicast: obs., see batcher */
		default:
			errunlock("unknown sys flag `%s' given", flags);
			/* NOTREACHED */
		}
}

void
rewndsys()
{
	rewsys(fp);
}
