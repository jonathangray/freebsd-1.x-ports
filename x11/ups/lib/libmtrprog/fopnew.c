/* fopen_new_file.c - fopen a file but refuse to overwrite an existing one */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_fopen_new_file_sccsid[] = "@(#)fopnew.c	1.5 26/4/92 (UKC)";

#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include <errno.h>
#include <stdio.h>
#include <local/ukcprog.h>

#include "utils.h"

FILE *
fopen_new_file(name)
const char *name;
{
	struct stat stbuf;
	FILE *fp;

	if (strcmp(name, "-") == 0)
		return stdout;

	if (stat(name, &stbuf) == 0) {
		errf("File \"%s\" already exists", name);
		return NULL;
	}
	if (errno != ENOENT) {
		errf("Can't stat file \"%s\" (%m)", name);
		return NULL;
	}

	if ((fp = fopen(name, "w")) == NULL)
		errf("Can't create %s (%m)", name);
	return fp;
}

int
fclose_new_file(fp, name)
FILE *fp;
const char *name;
{
	int res;

	if (fp == stdout)
		name = "the standard output";

	res = 0;

	if (ferror(fp) || fflush(fp) == EOF) {
		errf("Error writing to %s (%m)", name);
		res = -1;
	}

	if (fp != stdout && fclose(fp) == EOF && res == 0) {
		errf("Error closing %s (%m)", name);
		res = -1;
	}

	return res;
}
