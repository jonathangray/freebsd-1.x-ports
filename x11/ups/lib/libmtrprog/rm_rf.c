/* rm_rf.c - recursively remove a directory tree */ 

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char rm_rf_sccsid[] = "@(#)rm_rf.c	1.8 20/5/92 (UKC)";

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/param.h>		/* for MAXPATHLEN */
#include <errno.h>

#include <string.h>

#if defined(NO_DIRENT) || defined(clipper)
#include <sys/dir.h>
#undef dirent
#define dirent direct
#else
#include <dirent.h>
#endif

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "utils.h"

static int rm_rf PROTO((char *name, int namelen));

int
remove_directory_tree(name)
const char *name;
{
	char path[MAXPATHLEN + 1];

	if (strlen(name) > MAXPATHLEN) {
		errf("Path (%s) too long - max %d characters", name, MAXPATHLEN);
		return -1;
	}

	return rm_rf(strcpy(path, name), MAXPATHLEN);
}

/*  Remove name, and if name is a directory, any files below name.
 *
 *  namelen is the length of the string buffer that name is in - this
 *  should be long enough for any path name (names are tacked onto the
 *  end of the name string, although it is unaltered on return from this
 *  function).
 */
static int
rm_rf(name, namelen)
char *name;
int namelen;
{
	DIR *dirp;
	int res;
	struct dirent *de;
	char *end, *newname;
	struct stat stbuf;

	if (lstat(name, &stbuf) != 0) {
		errf("Can't stat %s (%m)", name);
		return(-1);
	}
	if ((stbuf.st_mode & S_IFMT) != S_IFDIR) {
		if (unlink(name) != 0) {
			errf("Can't unlink %s (%m)", name);
			return(-1);
		}
		return(0);
	}

	if ((dirp = opendir(name)) == NULL) {
		errf("Can't open directory %s (%m)", name);
		return(-1);
	}

	end = name + strlen(name);
	res = 0;
	while ((de = readdir(dirp)) != NULL) {
		newname = de->d_name;
		if (strcmp(newname, ".") == 0 || strcmp(newname, "..") == 0)
			continue;
		if (end - name + strlen(newname) + 2 >= namelen) {
			errf("Name (%s/%s) too long", name, newname);
			res = -1;
			break;
		}
		*end = '/';
		(void) strcpy(end + 1, newname);
		res = rm_rf(name, namelen);
		*end = '\0';
		if (res != 0)
			break;
	}

	closedir(dirp);
	if (res == 0) {
		if (rmdir(name) != 0) {
			errf("Can't remove directory %s (%m)", name);
			res = -1;
		}
	}
	return(res);
}
