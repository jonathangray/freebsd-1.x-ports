/*
 * process a single incoming article
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/timeb.h>		/* solely for getindate call */
#include "libc.h"
#include "news.h"
#include "active.h"
#include "control.h"
#include "headers.h"
#include "article.h"
#include "history.h"
#include "io.h"
#include "msgs.h"
#include "ngmatch.h"
#include "system.h"
#include "transmit.h"

#define DAY (24L*60L*60L)

/*
 * seconds of slop permitted: article dates may be this many seconds in the
 * future.  It should be an hour, but for sites (e.g. in Australia) that
 * emit local time incorrectly labelled as GMT.  They really should fix
 * their software, but in the mean time, a day's slop will prevent their
 * articles from being dropped.
 */
#define CLOCKSLOP DAY

/*
 * COPYSIZE is the length of a bulk-copying buffer: the bigger the better,
 * though fewer than 3% of articles exceed 8192 bytes (may 1988).
 * It holds header lines first, and later holds bytes of the body.
 * This buffer is allocated once at the start and never deallocated.
 */
#ifndef COPYSIZE
#ifdef SMALLMEM
#define COPYSIZE BUFSIZ		/* conserve memory at the expense of speed */
#else
#define COPYSIZE 8192		/* big enough even for worst-case 4.2bsd blocks */
#endif				/* SMALLMEM */
#endif				/* COPYSIZE */

/* imports */
extern char *exclude;		/* for erik */
extern long staledays;		/* from relaynews.c */
extern boolean dupsokay;	/* from relaynews.c */
extern void decline();

/* forwards */
extern void tossorfile(), surveydamage(), reject(), prefuse(), uninsart();
extern char *hdrcopy();
FORWARD void copyart(), cpybody(), insart();
FORWARD statust snuffmayreturn();

/*
 * Copy the article on "in" to a temporary name in the news spool directory,
 * unlink temp name; *or* copy into the final names, if known early enough.
 * (Sets a_tmpf in or near hdrmunge() or hdrdump().)
 * If the spool file opened, install the article it contains.
 */
statust
cpinsart(in, inname, maxima, blvmax)
FILE *in;
register char *inname;
long maxima;
boolean blvmax;				/* believe maxima? */
{
	register struct article *artp;
	register statust status;
	struct article art;

	artp = &art;
	artinit(artp);
	artp->a_blvmax = blvmax;
	artp->a_unread = maxima;

	/*
	 * copyart() may reject() the article, and may fill the disk.
	 * it calls fileart and logs rejected articles.  it may call uninsart.
	 */
	copyart(artp, in, inname);

	if (artp->a_status&ST_REFUSED) {
		/* no good ngs (in fileart) or reject()ed; not serious */
		artp->a_status &= ~ST_REFUSED;
		/* paranoia; shouldn't happen */
		nnfclose(artp, &artp->a_artf, inname);
	} else if (artp->a_artf == NULL) {
		warning("can't open spool file `%s'", artp->a_tmpf);
		artp->a_status |= ST_DROPPED;
	} else {
		nnfclose(artp, &artp->a_artf, inname);
		insart(artp);	/* logs accepted art.s during transmission */
		if (artp->a_status&ST_JUNKED) {	/* yer welcome, henry */
			artp->a_status &= ~ST_JUNKED;
			timestamp(stdout, (time_t *)NULL);
			(void) printf(" %s j %s junked due to groups `%s'\n",
				sendersite(nullify(artp->h.h_path)),
				artp->h.h_msgid, artp->h.h_ngs);
		}
	}
	status = artp->a_status;
	artfree(artp);
	return status;
}

/*
 * Copy the next charcnt bytes of "in" (may be not a disk file)
 * to a permanent file under a (possibly) temporary name.
 * After the headers are seen, accept or reject the article.
 * If rejected and the headers fit in core, no files will be opened.
 * Must munge certain headers on the way & remember certain values.
 * hdrmunge() or hdrdump() sets art->a_tmpf & art->a_artf.
 * Unlink art->a_tmpf, if a temporary link.
 */
/* ARGSUSED inname */
STATIC void
copyart(art, in, inname)
register struct article *art;
register FILE *in;
char *inname;
{
	boolean installed = YES;
	char *body;

	body = hdrcopy(art, in);
	hdrdeflt(&art->h);
	tossorfile(art, &installed);
	/* assertion: header values (art->h) can be forgotten here */
	cpybody(art, in, body);
	surveydamage(art, &installed);
}

/*
 * The loop copies header lines from input to output or a
 * header output cache.  On exit, hdr will contain the first
 * non-header line, if any, left over from the end of header copying.
 *
 * Some people think the loop is ugly; I'm not sure why.
 * If the byte count is positive, read a line; if it doesn't return
 * EOF and is a header, then adjust byte count, stash and munge headers.
 * strlen(line) must be computed before hdrstash is called,
 * as hdrstash (and thus hdrdigest) removes newlines.
 */
char *					/* first body line, from gethdr */
hdrcopy(art, in)
register struct article *art;
FILE *in;
{
	register char *hdr = NULL;
	long limit = art->a_unread + SIZENUL;
	int is_hdr = NO;

	/*
	 * TODO: Cope with NULs in input, which bugger fgets and friends
	 * and throw off our byte count, thus buggering unbatching.
	 */
	while (limit > SIZENUL && (hdr = gethdr(in, &limit, &is_hdr)) != NULL &&
	    is_hdr) {
	    	hdrdigest(art, hdr, strlen(hdr));
		hdr = NULL;			/* freed inside gethdr */
	}
	/* If we read a body line, gethdr has adjusted limit appropriately. */
	art->a_unread = limit - SIZENUL;
	/*
	 * RFC 822 defines the message header as ending at a blank line,
	 * *not* at the first line that cannot syntactically be a header nor
	 * a header continuation.  As a result of this stunning bit of
	 * brilliance, we can end up with non-header lines in the message
	 * header, though they are illegal.
	 *
	 * Don't print anything if this article has already been refused.
	 */
	if (!is_hdr && hdr != NULL && *hdr != '\n' &&
	    !(art->a_status&ST_REFUSED)) {
		register char *hdrnonl = strsave(hdr);

#ifdef notdef
		art->a_badhdr = YES;	
#endif
		trim(hdrnonl);
		decline(art);
		prefuse(art);
		(void) printf(
		"article \"header\" contains non-RFC-1036-header line `%s'\n",
			hdrnonl);
		free(hdrnonl);
	}
	/* if is_hdr, there is no body: header fills limit */
	return (is_hdr? NULL: hdr);
}

/*
 * Either reject the article described by art, or accept it and file it.
 * If rejecting it, remove any links and give back assigned #'s
 * (art->a_artf may still be open; arguably uninsart should close it).
 * If accepting it, dump any saved headers and file the article.
 * Unlink art->a_tmpf if it's a temporary link.
 */
void
tossorfile(art, installedp)
register struct article *art;
boolean *installedp;
{
	reject(art);				/* duplicate, etc.? */
	if (art->a_status&(ST_DROPPED|ST_REFUSED)) {
		uninsart(art);
		*installedp = NO;
	} else
		hdrdump(art, ALLHDRS);		/* ALLHDRS triggers fileart */

	if (art->a_unlink) {
		/* a_tmpf has had links made to it, so it can be removed. */
		if (unlink(art->a_tmpf) < 0) {
			warning("copyart can't unlink `%s'", art->a_tmpf);
			art->a_status |= ST_ACCESS;
		}
		art->a_unlink = NO;		/* caution */
	}
}

/*
 * Copy article body.
 * body will contain the first non-header line, if any,
 * left over from the end of header copying.  Write it.
 * Copy at most COPYSIZE bytes of body at a time and exactly art->a_unread
 * bytes in total, barring EOF or a full disk. Then "block" is no longer needed.
 * Force the article to disk, mostly for the benefit of control message
 * processing.
 *
 * The copying buffer, block, is static because it is used repeatedly
 * and persists through most of execution, so dynamic allocation
 * and deallocation seems wasteful, but also for the benefit
 * of compilers for odd machines (e.g. PE, 370s) which make
 * implementing "large" automatic arrays difficult.
 */
STATIC void
cpybody(art, in, body)
register struct article *art;
FILE *in;
register char *body;
{
	register int readcnt;
	static char block[COPYSIZE];

	if (body != NULL) {			/* read too far? */
		register int bodylen = strlen(body);

		if (art->a_artf != NULL &&
		    fwrite(body, 1, bodylen, art->a_artf) != bodylen)
			fulldisk(art, spoolnm(art));
		art->a_charswritten += bodylen;
	}
	for (; art->a_unread > 0 && !(art->a_status&ST_NEEDATTN) && !feof(in) &&
	    (readcnt = fread(block, 1, (int)min(art->a_unread, COPYSIZE), in)) >
	    0; art->a_unread -= readcnt, art->a_charswritten += readcnt)
		if (art->a_artf != NULL &&
		    fwrite(block, 1, readcnt, art->a_artf) != readcnt)
			fulldisk(art, spoolnm(art));
	if (art->a_artf != NULL && fflush(art->a_artf) == EOF)
		fulldisk(art, spoolnm(art));
}

/*
 * If not yet uninstalled, and the disk filled (or the news system was found
 * to be otherwise unwell), uninstall this article
 * to remove any (zero-length) links and decrement the active article number.
 * The ST_NEEDATTN status will prevent a history entry being generated later.
 */
void
surveydamage(art, installedp)
register struct article *art;
register boolean *installedp;
{
	if (art->a_unread > 0 && art->a_blvmax) {
		(void) fprintf(stderr, "%s: article %s short by %ld bytes\n",
			progname, (art->h.h_msgid != NULL? art->h.h_msgid: ""),
			(long)art->a_unread);
		art->a_status |= ST_SHORT;  /* NB.: don't uninstall this art. */
	}
	if (*installedp && art->a_status&ST_NEEDATTN) {
		uninsart(art);
		*installedp = NO;
	}
#ifdef WATCHCORE
	{
		char stbot;
		extern char *sbrk();

		printf("debugging memory use: top of data=%u",
			(unsigned)sbrk(0));
		printf(", bottom of stack=%u\n", (unsigned)&stbot);
	}
#endif
}

/*
 * If nothing has gone wrong yet,
 * install the article on art->a_tmpf or art->a_files:
 * The article should have been accepted and filed in copyart().
 * Add history entries for the article.  Log arrival.
 * Transmit the article to our neighbours.
 * Process control mess(age)es.  ctlmsg can call transmit(fakeart,x)
 * and generate log lines for cancels and ihave/sendme.
 */
STATIC void
insart(art)
register struct article *art;
{
	if (!(art->a_status&(ST_DROPPED|ST_REFUSED|ST_NEEDATTN))) {
		if (!art->a_filed)			/* paranoia */
			(void) fprintf(stderr, "%s: %s not filed by copyart!\n",
				progname, art->h.h_msgid);
		if (dupsokay) {
			time_t now;

			timestamp(stdout, &now);
			if (printf(" %s + %s", /* TODO: special code for dup? */
			    sendersite(nullify(art->h.h_path)),
			    nullify(art->h.h_msgid)) == EOF)
				fulldisk(art, "stdout");
		} else
			history(art, STARTLOG);	/* history may be unwritable */
		if (art->a_status&(ST_DROPPED|ST_REFUSED|ST_NEEDATTN)) {
			uninsart(art);		/* t'was; can't keep article */
			(void) putchar('\n');	/* ends the log line */
		} else {
			/* transmit() writes system names on stdout */
			transmit(art, exclude);
			(void) putchar('\n');	/* ends the log line */
			ctlmsg(art);		/* NCMP */
		}
#ifdef FLUSHLOG
		(void) fflush(stdout);		/* crash-proofness */
#endif
	}
	art->a_status &= ~ST_REFUSED;	/* refusal is quite casual & common */
}

/*
 * Reject articles.  This can be arbitrarily picky.
 * Only the headers are used to decide, so this can be called before
 * the article is filed but after all the headers are read.
 * Try to put the fastest tests first, especially if they often result
 * in rejections.
 */
void
reject(art)
register struct article *art;
{
	register struct headers *hdrs = &art->h;
	register char *ngs = hdrs->h_ngs;
	register char *errstr;
	register time_t date;
	static time_t now, datestale;
	extern time_t getindate();

	if (art->a_status&ST_REFUSED)
		return;			/* already rejected */
	if (now == 0) {
		now = time(&now);
		datestale = now - staledays*DAY;
	}
	errstr = hdrreq(hdrs);
	if (errstr != NULL) {
		prefuse(art);
		(void) fputs(errstr, stdout);
#ifdef notdef
	} else if (art->a_badhdr) {
		prefuse(art);
		(void) fputs("article \"header\" contains non-header line\n",
			stdout);
#endif
	} else if (!msgidok(art))
		(void) putchar('\n');	/* msgidok complained; end log line */
	else if (hdrs->h_approved == NULL && moderated(ngs)) {
		prefuse(art);
		(void) printf("unapproved article in moderated group(s) `%s'\n",
			ngs);
	} else if ((date = getindate(hdrs->h_date, (struct timeb *)NULL)) ==
	    -1) {
		prefuse(art);
		(void) printf("unparsable Date: `%s'\n", hdrs->h_date);
	} else if (date > now + CLOCKSLOP) {
		prefuse(art);
		(void) printf("Date: too far in the future: `%s'\n",
			hdrs->h_date);
	} else if (staledays > 0 && date < datestale) {
		prefuse(art);
		(void) printf("ancient date `%s'\n", hdrs->h_date);
	} else if (strchr(ngs, ' ') != NULL) {
		prefuse(art);
		(void) printf("space in groups `%s'\n", ngs);
	} else if (alreadyseen(hdrs->h_msgid)) {
		if (dupsokay)
			return;
		prefuse(art);
		(void) fputs("duplicate\n", stdout);
	} else if (hopcount(hdrs->h_path) > 0 &&
	    !ngpatmat(oursys()->sy_trngs, ngs)) {
		extern boolean histreject;

		/*
		 * non-local article, with all bad groups.
		 * (local articles with bad groups will be bounced
		 * by fileart when the groups aren't in active.)
		 */
		if (histreject)
			history(art, NOLOG);
		prefuse(art);
		(void) printf("no subscribed groups in `%s'\n", ngs);
	} else
		return;			/* art was accepted */
	decline(art);
}

/*
 * print the leader of a refusal message about the article in "art".
 */
void
prefuse(art)
register struct article *art;
{
	timestamp(stdout, (time_t *)NULL);
	(void) printf(" %s - %s ", sendersite(nullify(art->h.h_path)),
		nullify(art->h.h_msgid));
}

/*
 * "Uninstall" an article: remove art->a_files (permanent names) and
 * a_tmpf (temporary name if a_unlink set), and return assigned article #'s.
 * If a_unlink isn't set, a_tmpf is a copy of the first link in art->a_files.
 * Must be called before history() is called (or after it has failed),
 * else there will be a history entry for the article, but no spool files.
 * insart() need not be called first.
 */
void
uninsart(art)
register struct article *art;
{
	if (art->a_unlink && art->a_tmpf != NULL) {
		(void) unlink(art->a_tmpf);	/* I don't wanna know... */
		art->a_unlink = NO;
	}
	/* return article numbers (YES) & ignore unlink errors */
	(void) snuffmayreturn(art->a_files, YES);
}

statust
snufffiles(filelist)		/* just unlink all files in filelist */
char *filelist;
{
	/* don't return article numbers (NO) & return unlink errors */
	return snuffmayreturn(filelist, NO);
}

/*
 * Unlink all files in filelist, and optionally return article numbers.
 * When removing a link, note any failure, but don't issue an error message.
 * For one thing, cancel controls fail routinely because the article has been
 * removed manually or never existed (a previous cancel arrived before its
 * subject and generated a fake history entry).
 */
STATIC statust
snuffmayreturn(filelist, artret)
char *filelist;
boolean artret;		/* return article numbers & note unlink errors? */
{
	register statust status = ST_OKAY;
	register char *arts, *spacep, *slashp, *artnm;

	/* this is a deadly tedious job and I really should automate it */
	for (arts = filelist; arts != NULL && arts[0] != '\0';
	     arts = (spacep == NULL? NULL: spacep+1)) {
		spacep = strchr(arts, ' ');
		if (spacep != NULL)
			spacep[0] = '\0';	/* will be restored below */
		artnm = strsave(arts);
		if (spacep != NULL)
			spacep[0] = ' ';	/* restore space */

		slashp = strchr(artnm, FNDELIM);
		if (slashp != NULL)
			slashp[0] = '\0';	/* will be restored below */
		if (artret)
			/* prevartnum will complain on i/o error to active */
			(void) prevartnum(artnm); /* return assigned # */
		if (slashp != NULL)
			slashp[0] = FNDELIM;	/* restore slash */

		mkfilenm(artnm);
		if (unlink(artnm) < 0)
			status |= ST_ACCESS;
		free(artnm);
	}
	return status;
}
