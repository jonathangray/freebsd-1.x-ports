/* Copyright (C) 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gdevpipe.c */
/* %pipe% file device for Ghostscript */
#include "stdio_.h"
#include "gstypes.h"
#include "stream.h"
#include "filedev.h"

/* popen isn't POSIX-standard, so we declare it here. */
extern FILE *popen();
extern int pclose();

/* The pipe file device */
private fdev_proc_fopen(pipe_fopen);
private fdev_proc_fclose(pipe_fclose);
const file_device gs_fdev_pipe = {
	"pipe",
	{ fdev_no_init, fdev_no_open_device,
	  fdev_os_open_file, pipe_fopen, pipe_fclose,
	  fdev_no_delete_file, fdev_no_rename_file,
	  fdev_no_enumerate_files, NULL, NULL
	}
};

/* The file device procedures */

private FILE *
pipe_fopen(file_device *fdev, const char *fname, const char *access)
{	return popen(fname, access);
}

private int
pipe_fclose(file_device *fdev, FILE *file)
{	pclose(file);
	return 0;
}
