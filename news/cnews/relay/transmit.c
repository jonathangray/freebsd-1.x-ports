/*
 * transmit - transmit incoming articles to neighbouring machines
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "config.h"
#include "headers.h"
#include "active.h"
#include "article.h"
#include "msgs.h"
#include "ngmatch.h"
#include "system.h"
#include "trbatch.h"
#include "transmit.h"

/* forwards */
FORWARD boolean oktransmit();
FORWARD void ejaculate(), trappend();
FORWARD char *pctsubst();

/* private */
static boolean debug = NO;

void
transdebug(state)
boolean state;
{
	debug = state;
}

/*
 * For each system in "sys" other than this one,
 * transmit this article when its ng pattern matches
 * art->h.h_distr (which may be just a copy of art->h.h_ngs).
 */
void
transmit(art, exclude)
register struct article *art;
char *exclude;					/* no copy to this site */
{
	register struct system *sys;
	register int bsysno = 0;	/* ordinal # of batch sys entry */

	rewndsys();
	if (debug)
		(void) fprintf(stderr, "just rewound sys file\n");
	while ((sys = nextsys()) != NULL) {
		if (debug) {
			(void) fprintf(stderr, "sy_name=%s sy_ngs=",
				sys->sy_name);
			ngprint(sys->sy_trngs, stderr);
			(void) fprintf(stderr, " sy_distr=");
			ngprint(sys->sy_trdistr, stderr);
			(void) fprintf(stderr, "\n");
		}
		if (oktransmit(art, sys, exclude))
			ejaculate(art, sys, bsysno);
		if (sys->sy_flags&FLG_BATCH)
			++bsysno;
	}
	if (debug)
		(void) fprintf(stderr, "just finished reading sys file\n");
}

/*
 * Is it okay to send the article corresponding to "art" to site "sys",
 * excluding site "exclude"?
 *
 * If L(n) flag is on, must have been posted within n hops of here.
 * Never send to this site, nor the "exclude" site, nor any site with a host
 * in sys->sy_excl named in Path:, nor any site named in Path:.
 *
 * Newsgroups: must match sys's subscription list.
 * Distribution: must match sys's distribution list.  (RFC 850 is wrong:
 * Distribution:s are *not* patterns, they are lists.  See RFC 1036.)
 *
 * If m flag is on, group(s) must be moderated; if u flag is on,
 * must be unmoderated.  (If both are on, act as if neither is on.)
 */
STATIC boolean
oktransmit(art, sys, exclude)
register struct article *art;
register struct system *sys;
char *exclude;				/* no copy to him */
{
	register int flags = sys->sy_flags;
	register char *site = sys->sy_name;
	register char *path;
	register int result = YES;
	static char *canpath;
	static long lastid = -1;

	if (art->a_id != lastid) {	/* new article? */
		lastid = art->a_id;
		nnfree(&canpath);
		canpath = canonpath(art->h.h_path, art->h.h_approved, art->h.h_sender);
#ifdef notdef			/* DEBUG */
		fprintf(stderr, "path=%s canonpath=%s\n", art->h.h_path,
			canpath);
#endif
	}
	path = canpath;
	if (flags&FLG_LOCAL && hopcount(path) > sys->sy_lochops ||
	    STREQ(hostname(), site) ||
	    exclude != NULL && STREQ(exclude, site) || hostin(site, path) ||
	    sys->sy_excl != NULL && anyhostin(sys->sy_excl, path) ||
	    !ngpatmat(sys->sy_trngs, art->h.h_ngs) ||
	    !ngpatmat(sys->sy_trdistr, art->h.h_distr))
		result = NO;
	else if (flags&(FLG_MOD|FLG_UNMOD)) {	/* u, m flag selection */
		if ((flags&(FLG_MOD|FLG_UNMOD)) != (FLG_MOD|FLG_UNMOD))
			result = (flags&FLG_MOD? moderated(art->h.h_ngs):
						!moderated(art->h.h_ngs));
	}
	return result;
}

/*
 * Send the article denoted by art to the system denoted by sys.
 *
 * When a filename is needed, we use the first one in art->a_files
 * rather than art->a_tmpf because we want a permanent name.
 *
 * Side-effect: prints the system name on stdout for logging.
 */
STATIC void
ejaculate(art, sys, bsysno)
register struct article *art;
register struct system *sys;
int bsysno;
{
	register char *fullname;
	register char *filename = first(art->a_files);

	if (debug)
		(void) fprintf(stderr, "transmitting %s to %s\n",
			art->h.h_msgid, sys->sy_name);
    	(void) printf(" %s", sys->sy_name);	/* logging */

	mkfilenm(filename);
	fullname = strsave(artfile(filename));	/* N.B.: relative path */
	free(filename);

	if (sys->sy_flags&FLG_BATCH)
    		trbatch(art, sys, fullname, bsysno);
	else
		trcmd(art, sys, fullname);
	free(fullname);
}

/*
 * Execute sys->sy_cmd with the current article as stdin
 * and filename substituted for %s in sys->sy_cmd (if any).
 *
 * Search path includes $NEWSCTL/bin and $NEWSBIN/relay.
 * redirect stdin to prevent consuming my stdin & so cmd's stdin
 * is filename by default.
 */
void
trcmd(art, sys, filename)
struct article *art;
struct system *sys;
char *filename;
{
	register char *cmd, *substcmd;
	int exitstat;
	char *s1, *s2, *s3, *pfx;
	static char *ctlcmd = NULL, *bincmd = NULL;

	if (ctlcmd == NULL)
		ctlcmd = strsave(ctlfile("bin"));
	if (bincmd == NULL)
		bincmd = strsave(binfile("relay"));

	s1 = str3save("PATH=", ctlcmd, ":");
	s2 = str3save(bincmd, ":", newspath());
	s3 = str3save("; <", filename, " (");
	pfx = str3save(s1, s2, s3);
	free(s1);
	free(s2);
	free(s3);

	substcmd = pctsubst(sys->sy_cmd, filename);
	if (substcmd == NULL)
		art->a_status |= ST_DROPPED;
	else {
		cmd = str3save(pfx, substcmd, ")");
		free(substcmd);
	
		exitstat = system(cmd);
		if (exitstat != 0) {
			art->a_status |= ST_DROPPED;
			(void) fprintf(stderr, "%s: `", progname);
			(void) fputs(cmd, stderr);
			(void) fprintf(stderr, "' returned exit status 0%o\n",
				exitstat);
		}
		free(cmd);
	}
	free(pfx);
}

/*
 * We avoid sprintf if syscmd contains no %, thus avoiding the 128-byte
 * restriction on printf output (see printf(3) BUGS, at least in V7).
 */
STATIC char *					/* malloced storage */
pctsubst(cmd, file)	/* copy cmd, replace %s in it with file, return copy */
char *cmd, *file;
{
	register char *copy = NULL, *percent, *pcent2 = NULL;
	register int format;

	percent = strchr(cmd, '%');
	if (percent == NULL)
		copy = strsave(cmd);
	else {
		++percent;
		format = *percent;
		if (format != '\0')
			percent++;
		pcent2 = strchr(percent, '%');
		if (pcent2 != NULL)
			(void) fprintf(stderr, "%s: `%s' contains two %%'s\n",
				progname, cmd);
		else if (format != 's' && format != '%')
			(void) fprintf(stderr,
				"%s: `%s' contains %%%c, not %%s\n",
				progname, cmd, format);
		else {
			copy = nemalloc((unsigned)
					(strlen(cmd) + strlen(file) + SIZENUL));
			(void) sprintf(copy, cmd, file);
		}
	}
	return copy;
}

/*
 * Append "filename" to sys->sy_cmd.  bsysno is the ordinal # of this batch
 * sys line.  If bsysno is low enough, use the batchfile cache of batch file
 * descriptors.
 */
void
trbatch(art, sys, filename, bsysno)
register struct article *art;
struct system *sys;
char *filename;
register int bsysno;
{
	register struct batchfile *bf = bfopen(sys->sy_cmd, bsysno);

	if (bf == NULL || bf->bf_str == NULL)
		art->a_status |= ST_DROPPED;
	else {
		trappend(art, sys, bf, filename);
		art->a_status |= bffkclose(bsysno);
	}
}

/*
 * package up article filename, message-id, size, batch file name
 * and formatting flags for batch file "bf".
 */
STATIC void
trappend(art, sys, bf, artname)
register struct article *art;
register struct system *sys;
struct batchfile *bf;
char *artname;
{
	int flag = (sys->sy_flags&FLG_IHAVE? 'I':
		   (sys->sy_flags&FLG_NBATCH? 'n':
		   (sys->sy_flags&FLG_SZBATCH? 'f': 'F')));

	if (!bfappend(bf, flag, sys->sy_cmd,
	    artname, art->h.h_msgid, art->a_charswritten))
		fulldisk(art, bf->bf_name);
}

/*
 * really close all the open batch files
 */
statust
trclose()
{
	return bfrealclose();
}
