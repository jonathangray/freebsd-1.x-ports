#include <sys/types.h>

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <stdio.h>
#include <errno.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <symbol.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIexit.c	8.1	12/31/84)


/*
**	Exit ingres --
**		waits for the parser to return,
**		catching all children's death till then
**		or till an error return. 
**		In case wait(II) is interrupted while waiting,
**		as evidenced by errno == 4, waiting will resume.
**
*/

void
IIexit(void)
{
	register int	ndx;
	register int	pidptr;
	register int	err;
	int		status;
	int		pidlist[10];

#ifdef xETR1
	if (IIdebug)
		printf("IIexit\n");
#endif

	if (close(IIw_down) || close(IIr_down))
		IIsyserr("IIexit:cant't close");

	pidptr = 0;
	err = 0;
	errno = 0;
	while ((ndx = wait(&status)) != IIingpid && (ndx != -1 || errno == 4)) {
		if (ndx == -1) {
			errno = 0;
			continue;
		}
#ifdef xETR1
		if (IIdebug)
			printf("caught pid %u death, stat %d, %d\n",
				ndx, status >> 8, status & 0177);
#endif

		pidlist [pidptr++] = ndx;
		if ((status & 0177) != 0) {
			printf("%d: Abnormal Termination %d", ndx, status & 0177);
			if ((status & 0200) != 0)
				printf(" -- Core Dumped");
			printf("\n");
			err++;
		}
	}
	if (err) {
		printf("pid list:");
		for (ndx = 0; ndx < pidptr; ndx++)
			printf(" %u", pidlist[ndx]);
		printf("\n");
	}

	IIingpid = 0;
}
