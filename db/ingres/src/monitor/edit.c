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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)edit.c	8.1	12/31/84)

/*
**  CALL TEXT EDITOR
**
**	The UNIX text editor is called.  The actual call is to
**	the macro {editor}.  If that fails, /bin/ed is called.
**	This routine suppressed the autoclear function.
**
**	Uses trace flag 4
*/
void
edit(void)
{
	register int	i;
	register char	*p;
	register char	*editfile;

	editfile = getfilenm();
	if (*editfile == 0)
		editfile = Qbname;

	Autoclear = 0;
	fclose(Qryiop);

	/* FORK SENTRY PROCESS & INVOKE THE EDITOR */
	if ((Xwaitpid = fork()) < 0)
		syserr("edit: fork");
	if (Xwaitpid == 0) {
		setuid(getuid());
		setgid(getgid());
		for (i = 3; i < NOFILE; i++) {
			close(i);
		}
		p = getenv("VISUAL");
		if (p == (char *) NULL) {
			p = getenv("EDITOR");
		}
		if (p == (char *) NULL) {
			p = macro("{editor}");
		}
		if (p != (char *) NULL) {
			execl(p, p, editfile, 0);
			printf("Cannot call %s; using /bin/ed\n", p);
		}
		execl("/bin/ed", "ed", editfile, 0);
		syserr("edit: exec");
	}

	/* WAIT FOR SENTRY TO DIE */
	if (Nodayfile >= 0) {
		printf(">>ed\n");
	}
	xwait();
}
