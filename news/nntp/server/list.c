#ifndef lint
static char	*sccsid = "@(#)list.c	1.11	(Berkeley) 5/11/89";
#endif

#include "common.h"

/*
 * LIST
 *
 * List active newsgroups, newsgroup descriptions, and distributions.
 *
 */

list(argc, argv)
	int		argc;
	char		*argv[];
{
	char		line[NNTP_STRLEN];
	char		*grparray[2];
	char		*filename;
	char		*items;
	char		*format;
	register char	*cp;
	register FILE	*list_fp;
	
	if (argc == 1 || (argc == 2 && !strcasecmp(argv[1],"active"))){
                num_groups = read_groups();
                if (num_groups == 0){ /* can't get a group list */
                  printf("%d Group update failed. Try later.\r\n",
                         ERR_FAULT);
                  (void) fflush(stdout);
#ifdef LOG
                  syslog(LOG_INFO, "%s group update failed in LIST", hostname);
#endif
                  exit(1);
                }
		filename = activefile;
		items = "active newsgroups";
		format = "Newsgroups in form \"group high low y/n/m\".";
	} else if (argc == 2 && !strcasecmp(argv[1],"distributions")){
		filename = distributionsfile;
		items = "newsgroup distributions";
		format = "Distributions in form \"area description\".";
	} else if (argc == 2 && !strcasecmp(argv[1],"newsgroups")){
		filename = newsgroupsfile;
		items = "newsgroup descriptions";
		format = "Descriptions in form \"group description\".";
	} else {
		printf("%d Usage: LIST [ACTIVE|NEWSGROUPS|DISTRIBUTIONS]\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	grparray[0] = line;
	grparray[1] = NULL;

	list_fp = fopen(filename, "r");

	if (list_fp == NULL) {
		printf("%d No list of %s available.\r\n", ERR_FAULT,
			items);
		(void) fflush(stdout);
#ifdef SYSLOG
		syslog(LOG_ERR, "list: fopen %s: %m", filename);
#endif
		return;
	}

	printf("%d %s\r\n",OK_GROUPS,format);

	while (fgets(line, sizeof(line), list_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if (ngpermcount) {
			if (ngmatch(s1strneql, ALLBUT,
			    ngpermlist, ngpermcount,
			    grparray, 1) == 0)
				continue;
		}
		else if (ALLBUT==0) break; /* ngpermcnt==0 && ALLBUT == 0 means
					    * don't print the list, right? */
		putline(line);
	}
	(void) fclose(list_fp);

	putline(".");
	(void) fflush(stdout);
}
