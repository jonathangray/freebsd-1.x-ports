/* misc. POSIX emulation */

#ifndef lint
static char *RCSid = "unistd.c,v 1.1.1.1 1993/05/21 05:37:50 cgd Exp";
#endif

#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#if _V7 || _BSD

char *
getcwd(buf, len)
	char *buf;
	size_t len;
{
	char cwd [1024];
	extern char *getwd();
	if (getwd(cwd) == NULL)
		return NULL;
	if (strlen(cwd)+1 >= len) {
		errno = ERANGE;
		return NULL;
	}
	return strcpy(buf, cwd);
}

#endif

#if _V7

long
ulimit(cmd, limit)
	int cmd;
	long limit;
{
	return 0;
}

#endif

