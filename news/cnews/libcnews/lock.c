/*
 * C news system locking.
 * It's compatible with B 2.10.1 news, except that locks are never
 * declared stale (blow 'em away in /etc/rc).
 * Only permit relaynews to run on a file server to make this sane.
 */

#include <stdio.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "config.h"

#define LOCKNAME "LOCK"
#define LOCKTEMP "LOCKTMXXXXXX"
#define INTERVAL 25		/* seconds to sleep on a busy lock */

static boolean debug = NO;
static boolean mylock = NO;

void
lockdebug(state)
boolean state;
{
	debug = state;
}

/*
 * lock the news system.
 * create a temporary name in $NEWSCTL for linking, store my pid in it.
 * repeatedly try to link the temporary name to LOCKNAME.
 */
void
newslock()
{
	register char *tempnm, *lockfile;
	register FILE *tempfp;
	int locktries = 0;

	tempnm = strsave(ctlfile(LOCKTEMP));
	(void) mktemp(tempnm);
	tempfp = fopen(tempnm, "w");
	if (tempfp == NULL)
		error("can't create lock temporary `%s'", tempnm);
	(void) fprintf(tempfp, "%d\n", getpid());
	(void) fclose(tempfp);

	lockfile = strsave(ctlfile(LOCKNAME));
	while (link(tempnm, lockfile) < 0) {
		if (errno != EEXIST)
			error("can't link `%s' to LOCK", tempnm);
		/*
		 * Could decide here if the lock is stale.
		 * If so, remove it and try again to lock.
		 */
		if (debug && ++locktries == 1)
			(void) printf("%s: sleeping on LOCK\n", progname);
		sleep(INTERVAL);
	}
	free(lockfile);
	(void) unlink(tempnm);
	free(tempnm);
	mylock = YES;
}

void
newsunlock()
{
	if (mylock) {
		(void) unlink(ctlfile(LOCKNAME));
		mylock = NO;
	}
}

nolock() /* called in a child process to prevent lock release on error */
{
	mylock = NO;
}

void
errunlock(fmt, s)		/* like error(3), but unlock before exit */
char *fmt, *s;
{
	warning(fmt, s);
	newsunlock();
	exit(1);
	/* NOTREACHED */
}
