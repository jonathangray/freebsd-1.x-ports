#include <stdio.h>

#include <ingres.h>
#include <access.h>
#include <lock.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)pglocks.c	8.2	6/12/88)

lock_req_t	Lock;
/*
 *	setpgl- sets a lock for the access buffer
 */
int
setpgl(accbuf_t *buf)
{
	register accbuf_t	*b;
	int	ret_val;

#ifdef xATR1
	if ( tTf(28,2) ) {
		printf(" setpgl pg=%ld rel", buf->am_curpg);
		dumptid(&buf->am_rel);
	}
#endif
	if (Alockdes < 0)
		return(1);
	b = buf;
	Lock.lract = A_SLP;	/* wait for lock */
	Lock.lrtype = T_PAGE;	/* page lock */
	Lock.lrmod = M_EXCL;	/* exclusive lock */
	/* copy relation id */
	bmove(&b->am_rel, Lock.lrel, sizeof(Lock.lrel));
	/* copy page id */
	bmove(&b->am_curpg, Lock.lpage, sizeof(Lock.lpage));
	ret_val = dolock(&Lock);
	b->am_flags |= BUF_LOCKED;
	return (ret_val);
}
/*
 *	unlpg- releases a page lock
 */
int
unlpg(accbuf_t *buf)
{
	register accbuf_t	*b;
	int	ret_val;

#ifdef xATR1
	if (tTf(28, 3)) {
		printf(" unlpg page %ld rel", buf->am_curpg);
		dumptid(&buf->am_rel);
	}
#endif
	if (Alockdes < 0)
		return(1);
	b = buf;
	Lock.lract = A_RLS1;
	/* copy relation id */
	bmove(&b->am_rel, Lock.lrel, sizeof(Lock.lrel));
	Lock.lrtype = T_PAGE;	/* page lock */
	/* copy page id */
	bmove(&b->am_curpg, Lock.lpage, sizeof(Lock.lpage));
	b->am_flags &= ~BUF_LOCKED;
	ret_val = dolock(&Lock);
	return (ret_val);
}
/*
 *	unlall - release all locks held by this process
 */
int
unlall(void)
{
	int	ret_val;

#ifdef xATR1
	if (tTf(28, 6))
		printf(" unlall\n");
#endif

	Acclock = TRUE;	/* reset page lock flag just in case */
	if (Alockdes < 0)
		return(1);
	Lock.lract = A_RLSA;
	ret_val = dolock(&Lock);
	return (ret_val);
}
