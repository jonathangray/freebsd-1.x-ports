#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/post.c,v 1.1 1993/07/19 20:04:31 nate Exp $";
#endif

#include "common.h"

/*
 * POST
 *
 * Post an article to a set of newsgroups.
 */

post(argc, argv)
	int	argc;
	char	*argv[];
{
	char	errbuf[2 * NNTP_STRLEN];
	int	retcode, blocks;

	if (!canpost) {
		printf("%d Sorry, you're not allowed to post.\r\n",
			ERR_NOPOST);
#ifdef LOG
			syslog(LOG_INFO, "%s post rejected", hostname);
#endif
		(void) fflush(stdout);
		return;
	}

#ifdef POSTER
	if (uid_poster == 0) {
		printf("%d User %s does not exist!  Can't post.\r\n",
			ERR_POSTFAIL, POSTER);
#ifdef SYSLOG
		syslog(LOG_ERR, "post: User %s does not exist.", POSTER);
#endif
		(void) fflush(stdout);
		return;
	}
#endif

	if (!space(MINFREE - POSTBUFFER)) {
	    /* force error reporting code into sending */
	    /* an out-of-space error message	       */
	    if (gethostname(errbuf, MAXHOSTNAMELEN) < 0)
		(void) strcpy(errbuf, "Amnesiac");

	    (void) strcat(errbuf, " NNTP server out of space. Try later.");

	    retcode = 0;		/* indicates that an error occurred */
	}
	else retcode =
#ifdef CNEWS
	    spawn(inews, "inews", "-W", CONT_POST, ERR_POSTFAIL, errbuf,
		"<none>");
#else
	    spawn(inews, "inews", "-h", CONT_POST, ERR_POSTFAIL, errbuf,
		"<none>");
#endif
	if (retcode <= 0)
		printf("%d %s\r\n", ERR_POSTFAIL, errbuf);
	else if (retcode > 0)
		printf("%d Article posted successfully.\r\n", OK_POSTED);
	(void) fflush(stdout);

#ifdef LOG
	syslog(LOG_INFO, "%s post %s", hostname,
			retcode == 1 ? "succeeded" : "failed");
#endif
}
