/* apply_to_files_in.c - apply a function to all files in a directory */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_apply_to_files_in_sccsid[] = "@(#)appltofl.c	1.8 20/5/92 (UKC)";

#include "ifdefs.h"

#include <sys/types.h>
#include <errno.h>
#include <string.h>

#if defined(NO_DIRENT) || defined(clipper)
#include <sys/dir.h>
#undef dirent
#define dirent direct
#else
#include <dirent.h>
#endif

#include <local/ukcprog.h>

#include "utils.h"

/*  Apply func to all the files in directory dirname.  Return -1 and print an
 *  error message on stderr if there was an error reading the directory.
 *
 *  Return -1 and stop reading the directory if func returns non-zero.
 *
 *  Return 0 if we get the end of the directory without errors.
 */
int
apply_to_files_in(dirname, func, arg)
const char *dirname;
int (*func)PROTO((const char *func_dirname, const char *filename, char *arg));
char *arg;
{
	DIR *dirp;
	struct dirent *dp;
	bool readdir_error;

	if ((dirp = opendir(dirname)) == NULL) {
		errf("Can't open directory %s (%m)", dirname);
		return -1;
	}

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		readdir_error = errno != 0;
		if (dp == NULL)
			break;
		if (strcmp(dp->d_name, ".") != 0 && strcmp(dp->d_name, "..") != 0) {
			if ((*func)(dirname, dp->d_name, arg) != 0)
				break;
		}
	}

	/*  We would like to check whether closedir succeeded, but it
	 *  is declared as void on some systems.
	 */
	closedir(dirp);

	if (dp != NULL)
		return -1;
	if (readdir_error) {
		errf("Error reading directory %s (%m)", dirname);
		return -1;
	}
	return 0;
}
