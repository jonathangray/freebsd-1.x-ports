#ifndef lint
static char	*sccsid = "@(#)ahbs.c	1.8	(Berkeley) 1/11/88";
#endif

#include "common.h"

static char	*verbiage[] = {
	"head and body follow",
	"head follows",
	"body follows",
	"request text separately"
};

/*
 * {ARTICLE,HEAD,BODY,STAT} <messageid>|articlenum
 *
 * Retrieve article, head, body, or stat, depending on the
 * command we were invoked with.
 */

ahbs(argc, argv)
	int		argc;
	char		*argv[];
{
	char		artbuf[MAXPATHLEN], art_id[MAXBUFLEN];
	register char	c;
	int		method;
	register FILE	*fp;		/* For Message-ID retrieval only */

	if (argc > 2) {
		printf("%d Usage: %s <message-id>|article_number.\r\n",
			ERR_CMDSYN,argv[0]);
		(void) fflush(stdout);
		return;
	}

	if ((c = *argv[0]) == 'a' || c == 'A')
		method = ARTICLE;
	else if ((c == 's' || c == 'S'))
		method = STAT;
	else
		method = ((c == 'h' || c == 'H') ? HEAD : BODY);

	if (argc == 2 && *argv[1] == '<') {	/* Message ID */
		fp = openartbyid(argv[1]);
		if (fp == NULL) {
			printf("%d No article by message-id %s, sorry.\r\n",
				ERR_NOART, argv[1]);
			(void) fflush(stdout);
			return;
		}
		if (check_ngperm(fp) == 0) {
			printf("%d Can't give that to you, sorry.\r\n",
				ERR_ACCESS);
			(void) fflush(stdout);
			(void) fclose(fp);
			return;
		}
		printf("%d 0 %s Article retrieved, %s.\r\n",
			OK_ARTICLE + method, argv[1], verbiage[method]);
		spew(fp, method);
		(void) fclose(fp);
#ifdef LOG
		if (nn_told)
			nn_took++;
#endif
		return;
	}

	/* Else we're trying to read */

	if (!canread) {
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	}

	if (!ingroup) {
		printf("%d You are not currently in a newsgroup.\r\n",
			ERR_NCING);
		(void) fflush(stdout);
		return;
	}

	if (argc == 1) {
		if (art_ptr < 0 || art_ptr >= num_arts) {
			printf("%d No article is currently selected.\r\n",
				ERR_NOCRNT);
			(void) fflush(stdout);
			return;
		}
		(void) sprintf(artbuf, "%d", art_array[art_ptr]);
	} else
		(void) strcpy(artbuf, argv[1]);

	if (!valid_art(artbuf)) {
		printf("%d Invalid article number: %s.\r\n",
			ERR_NOARTIG, artbuf);
		(void) fflush(stdout);
		return;
	}

	while (open_valid_art(artbuf, art_id) == NULL) {
		if (argc > 1) {
			printf("%d Invalid article number: %s.\r\n",
				ERR_NOARTIG, artbuf);
			(void) fflush(stdout);
			return;
		} else {
			if (++art_ptr >= num_arts) {
				printf("%d Invalid article number.\r\n",
					ERR_NOARTIG);
				(void) fflush(stdout);
				return;
			}
			(void) sprintf(artbuf, "%d", art_array[art_ptr]);
		}
	}

	printf("%d %s %s Article retrieved; %s.\r\n",
		OK_ARTICLE + method, artbuf, art_id, verbiage[method]);

	spew(art_fp, method);

	if (argc > 1)
		art_ptr = findart(artbuf);
}
