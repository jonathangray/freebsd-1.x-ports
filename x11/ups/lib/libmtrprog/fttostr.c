/* filetype_to_string.c - convert a file mode to an ls type string */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_filetype_to_string_sccsid[] = "@(#)fttostr.c	1.4 26/4/92 (UKC)";

#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <local/ukcprog.h>

#include "utils.h"

const char *
filetype_to_string(mode)
int mode;
{
	static char buf[100];

	switch (mode & S_IFMT) {
		case S_IFDIR:	return "directory";
		case S_IFCHR:	return "character special file";
		case S_IFBLK:	return "block special file";
		case S_IFREG:	return "regular file";
#ifdef S_IFLNK
		case S_IFLNK:	return "symbolic link";
#endif
#ifdef S_IFSOCK
		case S_IFSOCK:	return "socket";
#endif
#ifdef S_IFIFO
		case S_IFIFO:	return "named pipe";
#endif
	}
	sprintf(buf, "file with unknown mode 0%o", mode);
	return buf;
}
