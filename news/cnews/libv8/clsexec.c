/*
 * set close on exec (on Unix)
 */

#include <stdio.h>
#include <sgtty.h>

void
fclsexec(fp)
FILE *fp;
{
	(void) ioctl(fileno(fp), FIOCLEX, (struct sgttyb *)NULL);
}

