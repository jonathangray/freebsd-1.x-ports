/*
 * 4.2BSD mkdir simulation
 */

#include <stdio.h>
#include <errno.h>
#include "fixerrno.h"
#include <sys/types.h>	/* argh */
#include "libc.h"

/* system call returns */
#define SYS_OK 0
#define SYS_ERR (-1)

#define UMASK_MASK 0777

#define STRLEN(s) (sizeof (s) - 1)		/* s must be a char array */

int
mkdir(dir, mode)
char *dir;
int mode;
{
	register char *cbuf = malloc((unsigned)STRLEN("mkdir ") + strlen(dir) + 1);
	register int oldmask, ret;

	if (cbuf == NULL) {
		errno = ENOMEM;			/* kludge */
		return SYS_ERR;
	}
	oldmask = umask(0);
	(void) umask(~(mode & ~oldmask) & UMASK_MASK);

	(void) sprintf(cbuf, "mkdir %s", dir);
	ret = (system(cbuf) != 0? SYS_ERR: SYS_OK);
	if (ret == SYS_ERR)
		errno = EINVAL;			/* kludge */

	(void) umask(oldmask);
	free(cbuf);
	return ret;
}
