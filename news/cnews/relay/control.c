/*
 * NCMP (netnews control message protocol).
 * Implement the Usenet control messages, as per RFCs 1036 and 850.
 * These are fairly infrequent and can afford to be done by
 * separate programs.  They are:
 *
 * control messages that (request a) change (in) the local system:
 *	cancel message-ID		restricted to Sender: else From: (or
 *					root?), in theory
 *	newgroup groupname [moderated]	must be Approved:
 *	rmgroup groupname		must be Approved:;
 *					allow some local control
 *	checkgroups			harass $NEWSMASTER about "deviations"
 *					in active; incompletely specified
 *
 * control messages that cause mail back to Reply-To: else From:
 *	sendsys [site]
 *	senduuname
 *	version
 *
 * the "ihave/sendme" protocol to minimise traffic volume and maximise delay
 * between this site and another
 * (ihave/sendme is semi-documented in the RFCs, kludgey and broken in B2.10.):
 *	ihave [message-ID-list] remotesys	generate a sendme for remotesys
 *						from message-ID-list
 *	sendme [message-ID-list] remotesys	send articles named to remotesys
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>

#include "libc.h"
#include "news.h"
#include "case.h"
#include "config.h"
#include "control.h"
#include "headers.h"
#include "article.h"
#include "caches.h"
#include "history.h"

#define NO_FILES ""
#define SUBDIR binfile("ctl")		/* holds shell scripts */
#ifdef FASTNGMATCH
#define OLDCNTRL "all.all.ctl"
#endif
#define SFXOLDCNTRL ".ctl"

/*
 * These are shell meta-characters, except for /, which is included
 * since it allows people to escape from the control directory.
 */
#define SHELLMETAS "\\<>|^&;\n({$=*?[`'\"/"

/* imports from news */
extern statust snufffiles(); 
extern void ihave(), sendme();

/* forwards */
FORWARD statust cancelart();
FORWARD void runctlmsg(), bombctlmsg();

/*
 * Implement control message specified in "art".
 * Because newgroup and rmgroup may modify the active file, for example,
 * we must flush in-core caches to disk first and reload them afterward.
 * We handle cancels in this process for speed and dbm read/write access.
 * We handle ihave & sendme in this process for dbm read access and
 * to work around syntax restrictions (<>).
 *
 * In future, one could pass header values to scripts as arguments or
 * in environment, as NEWS* variables, to save time in the scripts.
 */
void
ctlmsg(art)
register struct article *art;
{
	register char *inname = art->a_tmpf, *ctlcmd = art->h.h_ctlcmd;
	int pid, deadpid;
	int wstatus;
	static char nmcancel[] = "cancel ";
	static char nmihave[] = "ihave ";
	static char nmsendme[] = "sendme ";

	if (ctlcmd == NULL)
		ctlcmd = art->h.h_etctlcmd;
	if (ctlcmd == NULL)
		return;
	if (STREQN(ctlcmd, nmcancel, STRLEN(nmcancel))) {
		art->a_status |= cancelart(ctlcmd + STRLEN(nmcancel));
		return;
	}
	if (CISTREQN(ctlcmd, nmcancel, STRLEN(nmcancel)-1) &&
	    (!isascii(ctlcmd[STRLEN(nmcancel)-1]) ||
	     !isalpha(ctlcmd[STRLEN(nmcancel)-1]))) {
		/* should really just log this */
		errno = 0;
		warning("malformed cancel `%s'", ctlcmd);
		/* no need to return bad status; happens all the time */
		return;
	}
	if (STREQN(ctlcmd, nmihave, STRLEN(nmihave))) {
		ihave(ctlcmd + STRLEN(nmihave), art);
		return;
	}
	if (STREQN(ctlcmd, nmsendme, STRLEN(nmsendme))) {
		sendme(ctlcmd + STRLEN(nmsendme), art);
		return;
	}

	art->a_status |= actsync();
	(void) fflush(stdout);
	(void) fflush(stderr);

	pid = fork();
	if (pid == 0)				/* child? */
		runctlmsg(ctlcmd, inname);
	else if (pid == -1)
		warning("fork failed", "");

	/* lint complains about &wstatus on 4.2+BSD; too bad, lint's wrong. */
	while ((deadpid = wait(&wstatus)) != pid && deadpid != -1)
		;

	/* wrong kid returned, fork failed or child screwed up? */
	if (deadpid == -1 || pid == -1 || wstatus != 0)
		art->a_status |= ST_DROPPED;	/* admin got err.msg. by mail */
	/* let lazy evaluation load the caches */
}

boolean
safecmd(cmd)			/* true if it's safe to system(3) cmd */
register char *cmd;
{
	register char *s;

	for (s = cmd; *s != '\0'; s++)
		if (STREQN(s, "..", STRLEN("..")))
			return NO;
	for (s = SHELLMETAS; *s != '\0'; s++)
		if (strchr(cmd, *s) != NULL)
			return NO;
	return YES;
}

/*
 * In theory (RFC 1036 nee 850), we should verify that the user issuing
 * the cancel (the Sender: of this article or From: if no Sender) is the
 * Sender: or From: of the original article or the local super-user.
 *
 * In practice, this is a lot of work and since anyone can forge news
 * (and thus cancel anything), not worth the effort.
 *
 * Ignore ST_ACCESS while cancelling an already-seen article since the
 * article may have been cancelled before or may have a fake history entry
 * because the cancel arrived before the article.
 *
 * If the article being cancelled has not been seen yet, generate a history
 * file entry for the cancelled article in case it arrives after the cancel
 * control.  The history file entry will cause the cancelled article to be
 * rejected as a duplicate.
 */
STATIC statust
cancelart(msgidstr)
char *msgidstr;
{
	register char *wsp;
	register char *msgid = strsave(msgidstr);
	register int idbytes;
	register char *wholemsgid = msgid;
	register statust status = ST_OKAY;

	/* skip leading whitespace in msgid */
	while (*msgid != '\0' && isascii(*msgid) && isspace(*msgid))
		++msgid;
	/* replace trailing whitespace with NULs; `wsp >= msgid' is not safe */
	idbytes = strlen(msgid);
	for (wsp = msgid + idbytes - 1; idbytes-- > 0 &&
	    isascii(*wsp) && isspace(*wsp); --wsp)
		*wsp = '\0';

	if (alreadyseen(msgid)) {
		register char *histent, *filelist;

		histent = gethistory(msgid);
		if (histent != NULL && (filelist = findfiles(histent)) != NULL)
			status |= snufffiles(filelist) & ~ST_ACCESS;
	} else {
		status |= fakehist(msgid, DEFEXP, NO_FILES);	/* start log */
		(void) putchar('\n');		/* end log line */
	}
	free(wholemsgid);
	return status;
}

/*
 * Execute a non-builtin control message by searching $NEWSCTL/bin and
 * $NEWSBIN/ctl for the command named by the control message.
 * runctlmsg is called from a child of relaynews, so it must always
 * call _exit() rather than exit() to avoid flushing stdio buffers.
 *
 * Enforce at least minimal security: the environment was standardised at
 * startup, including PATH and IFS; close non-standard file descriptors;
 * reject shell metacharacters in ctlcmd.
 */
STATIC void
runctlmsg(ctlcmd, inname)			/* child process */
register char *ctlcmd, *inname;
{
	register char *cmd, *s1, *s2, *s3;
	register int cmdstat;

	nolock();
	closeall(1);
	if (!safecmd(ctlcmd)) {
		errno = 0;
		warning("control `%s' looks unsafe to execute", ctlcmd);
		(void) fflush(stderr);
		_exit(0);		/* it's okay; happens all the time */
	}
	s1 = str3save("PATH=", ctlfile("bin"), ":");
	s2 = str3save(SUBDIR, "; ", "");
	s3 = str3save(ctlcmd, " <", inname);
	cmd = str3save(s1, s2, s3);
	free(s1);
	free(s2);
	free(s3);
	/* TODO: use fork, putenv, execlp here instead of system? */
	cmdstat = system(cmd);
	if (cmdstat != 0)
		bombctlmsg(cmd, cmdstat);
	free(cmd);
	_exit(0);
}

/*
 * Notify the local news administrator by mail that "cmd" failed
 * with "cmdstat" status, and _exit with bad status (again avoid stdio
 * buffer flushing in the child).
 * TODO: just log the failure; don't send mail.
 */
STATIC void
bombctlmsg(cmd, cmdstat)
char *cmd;
int cmdstat;
{
	register char *mailcmd, *s1;
	register FILE *mailf;

	s1 = str3save("PATH=", newspath(), " mail ");
	mailcmd = str3save(s1, newsmaster(), "");
	if (s1 == NULL || mailcmd == NULL) {
		warning("can't allocate memory in bombctlmsg", "");
		(void) fflush(stderr);
		_exit(1);
	}
	free(s1);
	mailf = popen(mailcmd, "w");
	if (mailf == NULL)
		mailf = stderr;
	(void) fprintf(mailf,
		"%s: control message `%s' exited with status 0%o\n",
		progname, cmd, cmdstat);
	(void) fflush(mailf);
	if (mailf != stderr)
		(void) pclose(mailf);
	free(mailcmd);
	_exit(1);
}

/*
 * we must be excessively paranoid in oldctl and hackoldctl since when we
 * are called, required headers have not been checked for presence, so any
 * header pointers could be NULL.
 */
STATIC boolean
oldctl(hdrs)				/* true iff ngs match OLDCNTRL */
register struct headers *hdrs;
{
#ifdef FASTNGMATCH
	return ngmatch(OLDCNTRL, nullify(hdrs->h_ngs));
#else
	register int ngslen = strlen(nullify(hdrs->h_ngs));

	/*
	 * Strictly match the equivalent of all.all.ctl.  RFC 1036 suggests
	 * that messages whose ngs match "all.all.ctl" should be interpreted
	 * as control messages.  I think this has long out-lived its
	 * usefulness and should be nuked, along with the cmsg bogosity.
	 * As a compromise, implement the matching as strictly as possible
	 * to minimise successful matches.
	 */
	return ngslen < STRLEN(SFXOLDCNTRL)? NO:
		STREQ(&hdrs->h_ngs[ngslen-STRLEN(SFXOLDCNTRL)], SFXOLDCNTRL) &&
			charcount(hdrs->h_ngs, NGDELIM) == 2 &&
			charcount(hdrs->h_ngs, NGSEP) == 0;
#endif					/* FASTNGMATCH */
}

hackoldctl(hdrs)			/* Handle the all.all.ctl hack. */
register struct headers *hdrs;
{
	if (hdrs->h_ctlcmd == NULL && oldctl(hdrs))
		hdrs->h_ctlcmd = strsave(nullify(hdrs->h_subj));
}

char *
hackhybrid(line)
register char *line;
{
	static char stupersedes[] = "Supersedes:";
	static char alsocan[] =     "Also-Control: cancel ";

	return CISTREQN(line, stupersedes, STRLEN(stupersedes))?
	    str3save(alsocan, "", &line[STRLEN(stupersedes)]): strsave(line);
}
