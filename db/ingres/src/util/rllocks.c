#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <signal.h>

#include <ingres.h>
#include <lock.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)rllocks.c	8.2	6/12/88)

lock_req_t	Lock;

/*
 *	setrll- set a relation lock
 */
int
setrll(char act, tid_t *rtid, char mod)
{
#ifdef xATR1
	if ( tTf(28,4) ) {
		printf(" setrll act=%d md=%o ", act, mod);
		dumptid(rtid);
	}
#endif
	if (Alockdes < 0)
		return(1);
	Lock.lract = act;	/* sleep (act = 2) or error return (act = 1)*/
	Lock.lrtype = T_REL;	/* relation lock */
	Lock.lrmod = mod;	/* exclusive (mod = 1) or shared (mod = 2)*/
	(void) memcpy(Lock.lrel, rtid, sizeof(*rtid));
	(void) memset(Lock.lpage, 0, sizeof(Lock.lpage));

	return(dolock(&Lock));
}

/*
 *	unlrl- unlock a relation lock
 */
int
unlrl(tid_t *rtid)
{
#ifdef xATR1
	if (tTf(28, 5)) {
		printf(" unlrl ");
		dumptid(rtid);
	}
#endif
	if (Alockdes < 0)
		return (1);
	Lock.lract = A_RLS1;
	Lock.lrtype = T_REL;	/* relation lock */
	(void) memcpy(Lock.lrel, rtid, sizeof(*rtid));
	(void) memset(Lock.lpage, 0, sizeof(Lock.lpage));
	return(dolock(&Lock));
}
