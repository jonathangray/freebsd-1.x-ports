/*
 * Uglix gethostname simulation
 */

#include <sys/types.h>
#include <sys/utsname.h>

#define min(a, b) ((a) < (b)? (a): (b))

int
gethostname(buf, size)
char *buf;
int size;
{
	struct utsname ugnm;
	char *strncpy();

	if (uname(&ugnm) < 0)
		return -1;
	(void) strncpy(buf, ugnm.nodename, min(sizeof ugnm.nodename, size));
	return 0;
}
