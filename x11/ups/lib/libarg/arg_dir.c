/* arg_dir.c - set of dirfunc routines for globbing against the file system */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_dir_sccsid[] = "@(#)arg_dir.c	1.11 20/5/92 (UKC)";

#include <sys/types.h>
#include <sys/stat.h>

#if defined(NO_DIRENT) || defined(clipper)
#include <sys/dir.h>
#undef dirent
#define dirent direct
#else
#include <dirent.h>
#endif

#include <local/ukcprog.h>

#include "arg.h"

static int fs_isdir PROTO((const char *path));
static int fs_opendir PROTO((const char *path, long *p_dirid));
static const char *fs_readdir PROTO((long dirid));
static void fs_closedir PROTO((long dirid));

static int
fs_isdir(path)
const char *path;
{
	struct stat stbuf;

	return stat(path, &stbuf) == 0 && (stbuf.st_mode & S_IFMT) == S_IFDIR;
}

static int
fs_opendir(path, p_dirid)
const char *path;
long *p_dirid;
{
	DIR *dirp;

	if ((dirp = opendir(path)) == NULL)
		return -1;
	*p_dirid = (long) dirp;
	return 0;
}

static const char *
fs_readdir(dirid)
long dirid;
{
	struct dirent *d;

	d = readdir((DIR *)dirid);
	return (d != NULL) ? d->d_name : NULL;
}

static void
fs_closedir(dirid)
long dirid;
{
	closedir((DIR *)dirid);
}

static dirfuncs_t Fs_df = {
	fs_isdir,
	fs_opendir,
	fs_readdir,
	fs_closedir
};

dvec_t
arg_glob_cpat(cpat, buf, buflen)
const char *cpat;
char *buf;
int buflen;
{
	return arg_gen_glob_cpat(cpat, buf, buflen, &Fs_df);
}

dvec_t
arg_glob(p_pat)
const char **p_pat;
{
	return arg_gen_glob(p_pat, &Fs_df);
}
