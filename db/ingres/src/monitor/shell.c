#include <sys/types.h>
#include <sys/param.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)shell.c	8.1	12/31/84)



/*
**  SHELL -- call unix shell
**
**	The UNIX shell is called.  Shell() first tries to call an
**	alternate shell defined by the macro {shell}, and if it fails
**	calls /bin/sh.
**
**	If an argument is supplied, it is a shell file which is
**	supplied to the shell.
**
**	Trace Flags:
**		34
*/
void
shell(void)
{
	register int	i;
	register char	*p;
	register char	*shellfile;

	shellfile = getfilenm();
	if (*shellfile == 0)
		shellfile = 0;

	fclose(Qryiop);
	if ((Xwaitpid = fork()) == -1)
		syserr("shell: fork");
	if (Xwaitpid == 0) {
		setuid(getuid());
		setgid(getgid());
		for (i = 3; i < NOFILE; i++)
			close(i);
		p = macro("{shell}");
#ifdef xMTR3
		tTfp(34, 0, "{shell} = '%o'\n", p);
#endif
		if (p != 0) {
			execl(p, p, shellfile, Qbname, 0);
			printf("Cannot call %s; using /bin/sh\n", p);
		}
		execl("/bin/sh", "sh", shellfile, Qbname, 0);
		syserr("shell: exec");
	}

	if (Nodayfile >= 0)
		printf(">>shell\n");
	/* wait for shell to complete */
	xwait();
}
