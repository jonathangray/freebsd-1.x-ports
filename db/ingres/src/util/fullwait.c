#include <sys/types.h>

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <useful.h>
#include <errno.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)fullwait.c	8.3	2/8/85)

/*
**  FULLWAIT -- performs a wait(II) primitive for a given child.
**
**	The wait primitive is executed as many times as needed to get
**	termination status for a given child.  The case of an interrupt
**	during the wait is handled.  No other children may die during
**	the wait.  Also, the child must die "normally", that is, as the
**	result of an exit() system call, or from an interrupt.
**
**	Parameters:
**		"child" -- the pid of the child process to wait for,
**			returned from the fork() system call.
**		"name" -- a character string representing the name of
**			the calling routine; printed on syserr's.
**	Returns:
**		The exit parameter of the child process.
*/
int
fullwait(int child, char *name)
{
	int	st;
	register int	i;
	register char	*n;
	register char	*coredump;

	n = name;

	/* wait for a child to die */
	while ((i = wait(&st)) != child) {
		/* it is not the child we want; chew on it a little more */
		if (i != -1)
			syserr("%s: unexpected child: pid %d st 0%o", n, i, st);

		/* check for interrupted system call */
		if (errno != EINTR) {
			/* wait with no child */
			syserr("%s: no child", n);
		}
		errno = 0;

		/* dropped out from signal: reexecute the wait */
	}

	/* check termination status */
	i = st & I1MASK;
	if (i > 2) {
		/* child collapsed */
		if (i & 0200) {
			coredump = " -- core dumped";
			i &= 0177;
		} else {
			coredump = "";
		}
		syserr("bad status %s: stat %d%s", n, i, coredump);
	}

	/* return exit status */
	return (st >> 8);
}
