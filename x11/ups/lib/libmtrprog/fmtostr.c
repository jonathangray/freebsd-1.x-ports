/* filemode_to_string.c - convert a file mode to an ls type string */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_filemode_to_string_sccsid[] = "@(#)fmtostr.c	1.4 26/4/92 (UKC)";

#include <sys/types.h>
#include <sys/stat.h>

#include <local/ukcprog.h>

#include "utils.h"

const char *
filemode_to_string(mode)
int mode;
{
#define OWNER	0
#define GROUP	3
#define OTHER	6
#define RPERM	1
#define WPERM	2
#define XPERM	3
#define S_ANYEXEC	(S_IEXEC | (S_IEXEC>>3) | (S_IEXEC>>6))
	static char perms[10];
	int offset, tch;
	
	for(offset = 6; offset >= 0; offset -= 3) {
		perms[offset+RPERM] = mode & (S_IREAD  >> offset) ? 'r' : '-';
		perms[offset+WPERM] = mode & (S_IWRITE >> offset) ? 'w' : '-';
		perms[offset+XPERM] = mode & (S_IEXEC  >> offset) ? 'x' : '-';
	}
	if (mode & S_ISUID)
		perms[OWNER+XPERM] = (mode & S_IEXEC) ? 's' : 'S';
	if (mode & S_ISGID)
		perms[GROUP+XPERM] = (mode & (S_IEXEC>>3)) ? 's' : 'S';
	if (mode & S_ISVTX)
		perms[OTHER+XPERM] = (mode & S_ANYEXEC) ? 't' : 'T';

	switch (mode & S_IFMT) {
		case S_IFDIR:	tch = 'd';	break;
		case S_IFCHR:	tch = 'c';	break;
		case S_IFBLK:	tch = 'b';	break;
		case S_IFREG:	tch = '-';	break;
#ifdef S_IFLNK
		case S_IFLNK:	tch = 'l';	break;
#endif
#ifdef S_IFSOCK
		case S_IFSOCK:	tch = 's';	break;
#endif
#ifdef S_IFIFO
		case S_IFIFO:	tch = 'p';	break;
#endif
		default:	tch = '?';	break;
	}
	*perms = tch;
	return perms;
}
