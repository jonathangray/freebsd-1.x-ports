/*
 * fopenexcl(name) - fopen(name, "w") with error if name exists (USG)
 */

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>

FILE *
fopenexcl(name)
register char *name;
{
	/* This is the cheaper way. */
	register int fd = open(name, O_WRONLY|O_CREAT|O_EXCL, 0666);

	if (fd < 0)
		return NULL;		/* name existed or couldn't be made */
	else
		return fdopen(fd, "w");
}
