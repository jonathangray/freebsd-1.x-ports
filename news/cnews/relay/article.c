/*
 * article creation and destruction
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "headers.h"
#include "article.h"

void
artinit(art)
register struct article *art;
{
	static long uniqno = 0;

	art->a_status = ST_OKAY;
	hdrinit(&art->h);
	art->a_haccum = NULL;
	art->a_hnext = NULL;
	art->a_hpalloced = 0;
	art->a_hpused = 0;
	art->a_hptrs = NULL;
	art->a_hbytesleft = 0;
	art->a_files = NULL;
	art->a_tmpf = NULL;
	art->a_artf = NULL;
	art->a_unlink = NO;
	art->a_filed = NO;
	art->a_xref = NO;
	art->a_blvmax = NO;
	art->a_charswritten = 0;
	art->a_unread = 0;
	art->a_id = uniqno++;
	art->a_badhdr = NO;
}

void
artfree(art)
register struct article *art;
{
	freeheaders(&art->h);
	/* a_haccum is currently not malloced */
	art->a_hptrs = NULL;		/* don't free a_hptrs; see hdrsave() */
	nnfree(&art->a_files);
	nnfree(&art->a_tmpf);
	if (art->a_artf != NULL) {
		(void) fprintf(stderr, "%s: a_artf still open in artfree()\n",
			progname);
		if (nfclose(art->a_artf) == EOF) {
			art->a_status |= ST_DROPPED;
			warning("error closing %s", art->a_tmpf);
		}
		art->a_artf = NULL;
	}
}
