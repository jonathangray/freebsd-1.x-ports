/*
 * history file bashing
 *
 * B 2.10.3+ rnews puts out a leading space before received
 * time if the article contains an Expires: header; tough.
 * C news does this right instead of compatibly.
 *
 * The second history field is really two: time-received and Expires: value,
 * separated by a tilde.  This is an attempt at partial compatibility with
 * B news, in that C expire can cope with B news history files.
 *
 * There is no point to storing seek offsets in network byte order in the
 * dbm file, since dbm files are machine-dependent and so can't be shared
 * by dissimilar machines anyway.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>		/* for memcpy */
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "config.h"
#include "dbz.h"
#include "fgetmfs.h"
#include "headers.h"
#include "article.h"
#include "history.h"
#include "msgs.h"

#define HISTNAME "history"	/* name of the history file in $NEWSCTL */
#define FIELDSEP '\t'
#define SUBFIELDSEP '~'

/* give 0 & 2 pretty, SVIDish names */
#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_END 2
#endif

/* private data */
static FILE *fp = NULL;
static char *filename;		/* absolute name of the ascii history file */
static boolean writable;

/* libdbm imports */
extern int dbminit(), store();
extern datum fetch();

/* other imports */
extern void prefuse();
extern boolean okrefusal;	/* flag from command line */

/* forward decls */
FORWARD datum getposhist();
FORWARD void mkhistent(), sanitise(), subsanitise();
void decline();

STATIC void
histname()
{
	if (filename == NULL)
		filename = strsave(ctlfile(HISTNAME));
}

/*
 * open the history files: ascii first, then dbm.
 * Try a+ mode first, then r mode, as dbm(3) does nowadays,
 * so that this routine can be used by any user to read history files.
 */
STATIC boolean
openhist()
{
	histname();
	if (fp == NULL) {
		if ((fp = fopenclex(filename, "a+")) != NULL)
			writable = YES;
		else if ((fp = fopenwclex(filename, "r")) != NULL)
			writable = NO;
		/* else fp==NULL and fopenwclex just complained */

		errno = 0;
		if (fp != NULL && dbminit(filename) < 0) {
			/*
			 * no luck.  dbm's dbminit will have just honked (on
			 * stdout, alas) but dbz's won't have, so bitch.
			 */
			warning(
		"database files for `%s' incomprehensible or unavailable",
				filename);
			(void) nfclose(fp);	/* close ascii file */
			fp = NULL;		/* and mark it closed */
		}
	}
	return fp != NULL;
}

STATIC datum
getposhist(msgid)		/* return seek offset of history entry */
char *msgid;
{
	register char *clnmsgid;
	datum msgidkey, keypos;

	msgidkey.dptr = NULL;
	msgidkey.dsize = 0;
	if (!openhist())
		return msgidkey;
	clnmsgid = strsave(msgid);
	sanitise(clnmsgid);
	msgidkey.dptr = clnmsgid;
	msgidkey.dsize = strlen(clnmsgid) + SIZENUL;
	keypos = dbzfetch(msgidkey);		/* offset into ascii file */
	free(clnmsgid);
	return keypos;
}

boolean
alreadyseen(msgid)		/* return true if found in the data base */
char *msgid;
{
	datum posdatum;

	posdatum = getposhist(msgid);
	return posdatum.dptr != NULL;
}

char *				/* NULL if no history entry; else malloced */
gethistory(msgid)		/* return existing history entry, if any */
char *msgid;
{
	long pos = 0;
	datum posdatum;

	posdatum = getposhist(msgid);
	if (posdatum.dptr != NULL && posdatum.dsize == sizeof pos) {
		static char *histent = NULL;

		(void) memcpy((char *)&pos, posdatum.dptr, sizeof pos); /* align */
		nnfree(&histent);
		if (fseek(fp, pos, SEEK_SET) != -1 &&
		    (histent = fgetms(fp)) != NULL)
			return histent;		/* could note move from EOF */
	}
	return NULL;
}

/*
 * Return a pointer to the "files" field of a history entry.
 * Side-effect: trims \n from the history entry.
 */
char *
findfiles(histent)
char *histent;
{
	register char *tabp;

	trim(histent);
	/* find start of 2nd field (arrival~expiry) */
	tabp = strchr(histent, FIELDSEP);
	if (tabp == NULL)
		return NULL;				/* mangled entry */
	/* find start of 3rd field (files list) */
	else if ((tabp = strchr(tabp + 1, FIELDSEP)) == NULL)
		return NULL;			/* cancelled or expired art. */
	else
		return tabp + 1;
}

/*
 * Generate a history entry from art.
 * The history entry will have tabs and newlines deleted from the
 * interior of fields, to keep the file format sane.
 * Optionally print the start of an "accepted" log file line (no \n)
 * (transmit() prints site names).
 */
void
history(art, startlog)
register struct article *art;
boolean startlog;
{
	register char *msgid, *expiry;
	time_t now;

	if (!msgidok(art))		/* complains in log if unhappy */
		return;			/* refuse to corrupt history */
	msgid = strsave(nullify(art->h.h_msgid));
	sanitise(msgid);	/* RFC 1036 forbids whitespace in msg-ids */
	expiry = strsave(nullify(art->h.h_expiry));
	sanitise(expiry);
	subsanitise(expiry);

	if (startlog) {
		timestamp(stdout, &now);
		if (printf(" %s + %s", sendersite(nullify(art->h.h_path)),
		    msgid) == EOF)
			fulldisk(art, "stdout");
	} else
		now = time(&now);
	if (!openhist())
		art->a_status |= ST_DROPPED|ST_NEEDATTN;	/* serious */
	else if (!writable) {
		(void) fprintf(stderr, "%s: no write permission on `%s'\n",
			progname, filename);
		art->a_status |= ST_DROPPED|ST_NEEDATTN;	/* serious */
	} else if (fseek(fp, 0L, SEEK_END) == -1) {
		/* could avoid fseek if still at EOF */
		warning("can't seek to end of `%s'", filename);
		art->a_status |= ST_DROPPED;
	} else
		mkhistent(art, msgid, now, expiry);
	free(msgid);
	free(expiry);
}

void
decline(art)					/* mark art as undesirable */
struct article *art;
{
	art->a_status |= ST_REFUSED|(okrefusal? 0: ST_DROPPED);
}

char *
ismsgidbad(msgid)				/* if bad, return error */
register char *msgid;
{
	if (msgid == NULL || msgid[0] == '\0')
		return "missing Message-ID";
	else if (strchr(msgid, '@') == NULL)
		return "no @ in Message-ID";
	else if (strchr(msgid, ' ') != NULL || strchr(msgid, '\t') != NULL)
		return "whitespace in Message-ID";
	else if (msgid[0] != '<' || msgid[strlen(msgid)-1] != '>')
		return "Message-ID not bracketed by <>";
	else
		return NULL;
}

int
msgidok(art)					/* if bad, complain in log */
register struct article *art;
{
	register char *err = ismsgidbad(art->h.h_msgid);

	if (err == NULL)
		return YES;
	else {
		prefuse(art);
		(void) fputs(err, stdout);
		decline(art);
		return NO;
	}
}

/*
 * Internal interface to generate a history file entry,
 * assuming all sanity checking has been done already.
 * Record the (msgid, position) pair in the data base.
 *
 * The fflush is crash-proofing.
 */
STATIC void
mkhistent(art, msgid, now, expiry)
register struct article *art;
char *msgid, *expiry;
time_t now;
{
	long pos;
	datum msgidkey, posdatum;

	pos = ftell(fp);  /* get seek ptr for dbm; could keep track instead */
	if (fprintf(fp, "%s%c%ld%c%s", msgid, FIELDSEP, now, SUBFIELDSEP, expiry)
	    == EOF)
		fulldisk(art, filename);
	/* don't write 3rd field for cancelled but unseen articles */
	if (art->a_files != NULL && art->a_files[0] != '\0')
		if (fprintf(fp, "%c%s", FIELDSEP, art->a_files) == EOF)
			fulldisk(art, filename);
	(void) putc('\n', fp);
	if (fflush(fp) == EOF)
		fulldisk(art, filename);

	msgidkey.dptr = msgid;
	msgidkey.dsize = strlen(msgid) + SIZENUL;
	posdatum.dptr = (char *)&pos;
	posdatum.dsize = sizeof pos;
	if (dbzstore(msgidkey, posdatum) < 0)
		fulldisk(art, filename);
}

/*
 * Turn \n & FIELDSEP into ' ' in s.
 */
STATIC void
sanitise(s)
register char *s;
{
	for (; *s != '\0'; ++s)
		if (*s == FIELDSEP || *s == '\n')
			*s = ' ';
}

/*
 * Turn SUBFIELDSEP into ' ' in s.
 */
STATIC void
subsanitise(s)
register char *s;
{
	for (; *s != '\0'; ++s)
		if (*s == SUBFIELDSEP)
			*s = ' ';
}

/*
 * Generate a fake history file entry, given a message-id, an Expires:
 * value, and a "file" list ("net.foo/123").
 */
statust
fakehist(fkmsgid, fkexpiry, fkfiles)
char *fkmsgid, *fkexpiry, *fkfiles;
{
	struct article art;

	artinit(&art);
	art.h.h_msgid = fkmsgid;
	art.h.h_expiry = fkexpiry;
	art.a_files = fkfiles;
	history(&art, STARTLOG);
	return art.a_status;
}

/*
 * Append "group/artnumstr" to the file list in *art.
 */
void
histupdfiles(art, group, artnumstr)
register struct article *art;
register char *group;
register char *artnumstr;
{
	unsigned addlen = strlen(group) + STRLEN(SFNDELIM) +
		strlen(artnumstr) + SIZENUL;

	art->a_filed = YES;			/* make a note */
	if (art->a_files == NULL) {
		art->a_files = nemalloc(addlen);
		art->a_files[0] = '\0';
	} else {
		art->a_files = realloc(art->a_files, (unsigned)
			strlen(art->a_files) + STRLEN(" ") + addlen);
		if (art->a_files == NULL)
			errunlock("can't grow a_files", "");
		(void) strcat(art->a_files, " ");
	}
	(void) strcat(art->a_files, group);	/* normal case */
	(void) strcat(art->a_files, SFNDELIM);
	(void) strcat(art->a_files, artnumstr);
}

statust
closehist()
{
	register statust status = ST_OKAY;

	if (fp != NULL) {
		/* dbmclose is only needed by dbz, to flush statistics to disk */
		if (dbmclose() < 0) {
			warning("error closing dbm history file", "");
			status |= ST_DROPPED;
		}
		if (nfclose(fp) == EOF) {
			warning("error closing history file", "");
			status |= ST_DROPPED;
		}
		fp = NULL;		/* mark file closed */
	}
	return status;
}
