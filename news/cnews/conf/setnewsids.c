/*
 * setnewsids - sets ids to news/news, execs relay/relaynews.  Should be
 *	setuid-root.  also add NEWSPERMS to the environment.
 */

#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>

#include "news.h"
#include "libc.h"
#include "config.h"

#ifndef RELAYNEWS
#define RELAYNEWS binfile("relay/relaynews")
#endif
#ifndef NEWSUSER
#define NEWSUSER "news"
#endif
#ifndef NEWSGROUP
#define NEWSGROUP "news"
#endif

char *progname;

static int userealids = NO;

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	extern int optind;
	extern char *optarg;

	progname = argv[0];

	/* setuid daemon prelude; various precautions */
	(void) umask(newsumask());
	(void) alarm(0);	/* cancel any pending alarm */
	/*
	 * Reset certain environment variables to sane values.
	 */
	if (putenv("PATH=/bin:/usr/bin") ||
	    putenv("IFS= \t\n"))
		exit(1);
	closeall(1);		/* closes all but std descriptors */
	stdfdopen();		/* ensure standard descriptors are open */

	setids();		/* change of real and effective ids */
	/* we are now running as news, so you can all relax */

	if (putenv("NEWSPERMS="))	/* avoid loops with this marker */
		exit(1);
	execv(RELAYNEWS, argv);	/* re-run relaynews */
	error("can't exec %s", RELAYNEWS);
}

setids()			/* change of real and effective ids */
{
	int newsuid = getuid(), newsgid = getgid();	/* default to real ids */

	(void) ctlfile((char *)NULL);	/* trigger unprivileged(), set userealids */
	if (!userealids) {
		register struct passwd *pwp;
		register struct group *grp;

		pwp = getpwnam(NEWSUSER);
		if (pwp != NULL) {
			newsuid = pwp->pw_uid;
			newsgid = pwp->pw_gid;
		}
		(void) endpwent();
		grp = getgrnam(NEWSGROUP);
		if (grp != NULL)
			newsgid = grp->gr_gid;
		(void) endgrent();
	}
	if (setgid(newsgid) < 0 || setuid(newsuid) < 0 ||
	    getgid() != newsgid || getuid() != newsuid)	/* xenix workaround */
		error("set[ug]id failed", "");
	/* we are now running as news, so you can all relax */
}

void
unprivileged()			/* called if NEWSARTS, NEWSCTL or NEWSBIN present */
{
	userealids = YES;
}
