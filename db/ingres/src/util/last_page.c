#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>

#include <ingres.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)last_page.c	8.2	1/17/85)

/*
**	LAST_PAGE -- computes a tid for the last page in the relation.
*/
int
last_page(desc_t *d, tid_t *tid, accbuf_t *buf)
{
	long		lpage;
	struct stat	stats;

	if ((buf != (accbuf_t *) NULL) &&
	    (M_TYPEOF(d->d_r.r_spec) == M_HEAP) &&
	    (buf->am_mainpg == 0) &&
	    (buf->am_overflowpg == 0)) {
		lpage = buf->am_curpg;
	} else {
		if (fstat(d->d_fd, &stats))
			syserr("last_page: fstat err %.14s", d->d_r.r_id);
		lpage = stats.st_size / PGSIZE - 1;
#ifdef xATR2
		if (tTf(26, 8))
			printf("fstat-lp %.12s %ld\n", d->d_r.r_id, lpage);
#endif
	}
	stuff_page(tid, &lpage);
	tid->line_id = 0;
	return (0);
}
