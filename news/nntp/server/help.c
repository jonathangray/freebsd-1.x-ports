#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/help.c,v 1.2 1994/04/25 23:58:20 adam Exp $";
#endif

#include "common.h"

/*
 * HELP
 *
 * Provide a naive user with a brief help message.
 *
 */

help(argc, argv)
int	argc;
char	*argv[];
{
	printf("%d This server accepts the following commands:\r\n", INF_HELP);
	printf("ARTICLE     BODY         GROUP\r\n");
	printf("HEAD        LAST         LIST\r\n");
	printf("NEXT        POST         QUIT\r\n");
	printf("STAT        NEWGROUPS    HELP\r\n");
	printf("IHAVE       NEWNEWS      SLAVE\r\n");
	printf("\r\nAdditionally, the following extension is supported:\r\n\r\n");
	printf("XHDR        Retrieve a single header line from a range of articles.\r\n");
	printf("\r\n");
	printf("Bugs to Stan Barber (Internet: nntp@tmc.edu; UUCP: ...!bcm!nntp)\r\n");
	printf(".\r\n");
	(void) fflush(stdout);
}
