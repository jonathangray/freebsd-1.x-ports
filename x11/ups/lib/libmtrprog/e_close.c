/* e_close.c - error checking interface to close */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_e_close_sccsid[] = "@(#)e_close.c	1.7 26/4/92 (UKC)";

#include <sys/types.h>
#include <sys/file.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "utils.h"

int
e_close(fd, filename)
int fd;
const char *filename;
{
	int res;

	if ((res = close(fd)) != 0)
		errf("Close of %s (fd %d) failed (%m)", filename, fd);
	return res;
}
