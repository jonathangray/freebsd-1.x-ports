/*
 * set close on exec (on Berklix)
 */

#include <stdio.h>
#include <sgtty.h>

void
fclsexec(fp)
FILE *fp;
{
	(void) ioctl(fileno(fp), FIOCLEX, (char *)NULL);
}
