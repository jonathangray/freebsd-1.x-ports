/*
 * set close on exec (on Uglix)
 */

#include <stdio.h>
#include <fcntl.h>

void
fclsexec(fp)
FILE *fp;
{
	(void) fcntl(fileno(fp), F_SETFD, 1);
}
