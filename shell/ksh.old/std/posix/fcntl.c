/* fcntl emulation */
/* fcntl.c,v 1.1.1.1 1993/05/21 05:37:50 cgd Exp */

#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

#if _V7

#include <sgtty.h>

int
fcntl(fd, cmd, arg)
	int fd, cmd, arg;
{
	switch (cmd) {
	  case F_SETFD:		/* set fd flags */
		ioctl(fd, (arg&FD_CLEXEC) ? FIOCLEX : FIONCLEX, (char *)NULL);
		break;
	  case F_DUPFD:		/* dup fd */
		/* this one is fun. find an unused fd >= arg and dup2 */
		break;
	}
	return 0;
}

#endif

