#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/ihave.c,v 1.1 1993/07/19 20:04:30 nate Exp $";
#endif

#include "common.h"

#ifdef LOG
int	ih_accepted;
int	ih_rejected;
int	ih_failed;
#endif LOG

/*
 * IHAVE <messageid>
 *
 * Accept an article for transferral if we haven't seen it before.
 */

ihave(argc, argv)
	int		argc;
	char		*argv[];
{
	char		errbuf[2 * NNTP_STRLEN];
	int		retcode;
	register char	*cp;
  
	if (!canxfer)
		{
#ifdef LOG
		syslog(LOG_INFO, "%s ihave attempted without permission",
			hostname);
#endif
		printf("%d You do not have transfer permission\r\n",
			ERR_GOODBYE);
		(void) fflush(stdout);
		return;
		}

	if (argc != 2) {
		printf("%d Usage: IHAVE <message-id>.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	cp = gethistent(argv[1], 1);
	if (cp != NULL) {
		printf("%d Got it.\r\n", ERR_GOTIT);
		(void) fflush(stdout);
#ifdef LOG
		ih_rejected++;
#ifdef IHAVE_DEBUG
		syslog(LOG_DEBUG, "%s ihave %s rejected", hostname, argv[1]);
#endif IHAVE_DEBUG
#endif LOG
		return;
	}

	if (!space(MINFREE)) {
	    /* force error reporting code into sending */
	    /* an out-of-space error message	       */
	    if (gethostname(errbuf, MAXHOSTNAMELEN) < 0)
		(void) strcpy(errbuf, "Amnesiac");

	    (void) strcat(errbuf, " NNTP server out of space. Try later.");

	    retcode = 0;		/* indicates that an error occurred */
	} else 
#ifdef BATCHED_INPUT
	    /* C news input hook */
	    retcode = batch_input_article(CONT_XFER, ERR_XFERFAIL,
		errbuf, argv[1]);
#else
	    retcode = spawn(rnews, "rnews", (char *) 0, CONT_XFER,
		ERR_XFERFAIL, errbuf, argv[1]);
#endif

	if (retcode <= 0){
               /* Reject if "*:<optional_whitespace>inbound*", else fail */
               register int i;
 
               i = ERR_XFERFAIL;
               if (cp = index(errbuf,':')) {
                       for (++cp; isspace(*cp); ++cp)
                               ;
                       if (strncasecmp(cp, "inbound", 7) == 0)
                               i = ERR_XFERRJCT;
               }
               printf("%d %s\r\n", i, errbuf);
       }
        else
               printf("%d Thanks.\r\n", OK_XFERED);
        (void) fflush(stdout);

#ifdef LOG
	if (retcode == 1)
		ih_accepted++;
	else
		ih_failed++;
		
#ifdef IHAVE_DEBUG
	syslog(LOG_DEBUG, "%s ihave %s accepted %s",
		hostname, argv[1], retcode == 1 ? "succeeded" : "failed");
#endif IHAVE_DEBUG
#endif LOG

}
