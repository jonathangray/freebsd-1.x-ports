#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <signal.h>

#include <ingres.h>
#include <ildr.h>
#include <lock.h>
#include "sccs.h"

#include "protos.h"

SCCSID(%W%	%G%)

/*
 *	LOCKREQ.C
 *
 *	This file contains code for the client half of the lock
 *	driver system.
 *
 */

int
dolock(lock_req_t *lock)
{
	struct	Lockreq		slock;
	int		ret_val;
	extern	int		Alockdes;

	/* translate lock to slock */
	slock.lr_act = lock->lract;
	slock.lr_type = lock->lrtype;
	slock.lr_mod = lock->lrmod;
	/* this bcopy copys three char [4] fields into one char [12] field
	   KEYSIZE happens to equal 12 */
	bcopy(lock->dbnode, slock.lr_key, KEYSIZE);	/* XXX */
	if (write(Alockdes, &slock, sizeof(slock)) != sizeof(slock))
	  syserr("lock write fail");
	if (read(Alockdes, &ret_val, sizeof(ret_val)) != sizeof(ret_val))
	  syserr("lock read fail");
	/* translate ret_val */
	return(ret_val);
}
