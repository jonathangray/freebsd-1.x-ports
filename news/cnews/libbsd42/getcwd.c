/*
 * SystemV getcwd simulation on 4.2BSD
 */

#include <stdio.h>
#include <string.h>
#include <sys/param.h>

/* imports from libc */
extern char *getwd();

char *
getcwd(path, size)
register char *path;
int size;
{
	if (size >= MAXPATHLEN)
		return getwd(path);
	else {
		char wd[MAXPATHLEN];

		if (getwd(wd) == 0)
			return 0;
		else {
			(void) strncpy(path, wd, size-1);
			path[size-1] = '\0';
			return path;
		}
	}
}
