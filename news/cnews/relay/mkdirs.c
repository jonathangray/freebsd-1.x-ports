/*
 * mkdirs - make the directories implied by `name'
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "libc.h"
#include "news.h"

/*
 * Given a/b/c/d, try to make any of a, a/b, a/b/c and a/b/c/d which are missing;
 * stop on first failure.
 * Returns success.
 */
boolean
mkdirs(name, uid, gid)
register char *name;
int uid, gid;
{
	register char *cp;
	register int isthere = YES;
	struct stat stbuf;

	for (cp = name; isthere && *cp != '\0'; cp++)
		if (*cp == FNDELIM) {
			*cp = '\0';
			isthere = stat(name, &stbuf) >= 0;
			if (!isthere) {
				isthere = mkdir(name, 0777) >= 0;
				(void) chown(name, uid, gid);
			}
			*cp = FNDELIM;
		}
	return isthere;
}
