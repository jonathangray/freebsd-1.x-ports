/*
 * stdfdopen - ensure that the standard i/o descriptors are open,
 *	to avoid mayhem.
 */

#include <stdio.h>
#include <errno.h>
#ifndef __STDC__
extern int errno;
#endif
#include <sys/types.h>
#include <sys/stat.h>

#ifndef NSYSFILE
#define NSYSFILE 3					/* hmm, not on V8 */
#endif

void
stdfdopen()			/* ensure standard descriptors are open */
{
	register int fd;
	struct stat stbuf;

	for (fd = 0; fd < NSYSFILE; fd++)
		if (fstat(fd, &stbuf) < 0 && errno == EBADF)
			if (open("/dev/null", 2) != fd)	/* open read/write */
				exit(1);		/* bad news */
}
