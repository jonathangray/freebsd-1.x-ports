#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <access.h>
#include <lock.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)cslocks.c	8.2	6/12/88)

lock_req_t	Lock;

/*
 *	setcsl- set a critical section lock
 */
int
setcsl(tid_t *rtid)
{
#ifdef xATR1
	if ( tTf(28,0) ) {
		printf(" setcsl ");
		dumptid(rtid);
	}
#endif

	if (Alockdes < 0) {
		return(1);
	}
	Lock.lract = A_SLP;	/* sleep while waiting on lock */
	Lock.lrtype = T_CS;	/* critical section lock */
	Lock.lrmod = M_EXCL;	/* exclusive access */
	/* copy relid */
	(void) memcpy(Lock.lrel, rtid, sizeof(*rtid));
	/* zero out pageid */
	(void) memset(Lock.lpage, 0, sizeof(Lock.lpage));
	return(dolock(&Lock));
}



/*
 *	unlcs- unlock a critical section
 */
int
unlcs(tid_t *rtid)
{
#ifdef xATR1
	if (tTf(28, 1)) {
		printf(" unlcs ");
		dumptid(rtid);
	}
#endif
	if (Alockdes < 0) {
		return(1);
	}
	Lock.lract = A_RLS1;
	Lock.lrtype = T_CS;
	/* copy relation identifier */
	(void) memcpy(Lock.lrel, rtid, sizeof(Lock.lrel));
	/* zero out page id */
	(void) memset(Lock.lpage, 0, sizeof(Lock.lpage));
	return(dolock(&Lock));
}
