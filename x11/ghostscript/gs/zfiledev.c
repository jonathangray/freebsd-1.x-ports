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

/* zfiledev.c */
/* File device implementation for Ghostscript */
#include "stdio_.h"
#include "string_.h"
#include "ghost.h"
#include "gp.h"
#include "errors.h"
#include "oper.h"
#include "stream.h"
#include "filedev.h"			/* must come after stream.h */
#include "files.h"

extern const uint file_default_buffer_size;

/* Routines in zfile.c */
extern stream *file_alloc_stream(P0());
extern int file_close_disable(P1(stream *));
extern int file_close_finish(P1(stream *));

/* The OS (%os%) device. */
fdev_proc_open_file(fdev_os_open_file);		/* os_open */
private fdev_proc_fopen(os_fopen);
private fdev_proc_fclose(os_fclose);
private fdev_proc_delete_file(os_delete);
private fdev_proc_rename_file(os_rename);
private fdev_proc_enumerate_files(os_enumerate);
private const file_device fdev_os = {
	"os",
	{ fdev_no_init, fdev_no_open_device,
	  fdev_os_open_file, os_fopen, os_fclose,
	  os_delete, os_rename,
	  os_enumerate, gp_enumerate_files_next, gp_enumerate_files_close
	}
};

/* The special devices. */
#define fdev_special(dname, init, open)\
  { dname,\
     { init, open, fdev_no_open_file, fdev_no_fopen, fdev_no_fclose,\
       fdev_no_delete_file, fdev_no_rename_file,\
       fdev_no_enumerate_files\
     }\
  }

#define stdin_buf_size 1
stream *gs_stream_stdin;	/* exported for zfileio.c only */
private fdev_proc_init(stdin_init);
private fdev_proc_open_device(stdin_open);
private const file_device fdev_stdin =
  fdev_special("stdin", stdin_init, stdin_open);

#define stdout_buf_size 128
stream *gs_stream_stdout;	/* exported for zfileio.c only */
private fdev_proc_init(stdout_init);
private fdev_proc_open_device(stdout_open);
private const file_device fdev_stdout =
  fdev_special("stdout", stdout_init, stdout_open);

#define stderr_buf_size 128
stream *gs_stream_stderr;	/* exported for zfileio.c only */
private fdev_proc_init(stderr_init);
private fdev_proc_open_device(stderr_open);
private const file_device fdev_stderr =
  fdev_special("stderr", stderr_init, stderr_open);

#define lineedit_buf_size 160
private byte *lineedit_buf;	/****** QUESTIONABLE ******/
private fdev_proc_init(lineedit_init);	/****** QUESTIONABLE ******/
private fdev_proc_open_device(lineedit_open);
private const file_device fdev_lineedit =
  fdev_special("lineedit", lineedit_init, lineedit_open);

private fdev_proc_open_device(statementedit_open);
private const file_device fdev_statementedit =
  fdev_special("statementedit", fdev_no_init, statementedit_open);

/*
 * The table of all known file devices.  The first entry must be fdev_os,
 * since it is the default for files with no explicit device specified.
 *
 * The table can be extended by configurable "resource" declarations,
 * like drivers, operators, etc.
 */
/* Declare additional file devices as extern first. */
#define file_device_(fdev) extern const file_device fdev;
#include "gconfig.h"
#undef file_device_

const file_device *file_device_table[] = {
	&fdev_os,
	&fdev_stdin, &fdev_stdout, &fdev_stderr,
	&fdev_lineedit, &fdev_statementedit,
#define file_device_(fdev) &fdev,
#include "gconfig.h"
#undef file_device_
	NULL
};

/* ------ Initialization ------ */

private void
zfiledev_init(void)
{	/* Run the one-time initialization of each file device. */
	const file_device **pfdev = &file_device_table[0];
	for ( ; *pfdev; pfdev++ )
		((*pfdev)->procs.init)((file_device *)*pfdev);
}

/* ------ Default (unimplemented) file device procedures ------ */

int
fdev_no_init(file_device *fdev)
{	return 0;
}

int
fdev_no_open_device(file_device *fdev, const char *access, stream **ps)
{	return_error(e_invalidfileaccess);
}

int
fdev_no_open_file(file_device *fdev, const char *fname, uint namelen,
  const char *access, stream **ps)
{	return_error(e_invalidfileaccess);
}

FILE *
fdev_no_fopen(file_device *fdev, const char *fname, const char *access)
{	return NULL;
}

int
fdev_no_fclose(file_device *fdev, FILE *file)
{	return_error(e_ioerror);
}

int
fdev_no_delete_file(file_device *fdev, const char *fname)
{	return_error(e_invalidfileaccess);
}

int
fdev_no_rename_file(file_device *fdev, const char *from, const char *to)
{	return_error(e_invalidfileaccess);
}

file_enum *
fdev_no_enumerate_files(file_device *fdev, const char *pat, uint patlen,
  const gs_memory_procs *mprocs)
{	return NULL;
}

/* ------ %os% ------ */

/* The open_file routine is exported for pipes. */
int
fdev_os_open_file(file_device *fdev, const char *fname, uint len,
  const char *file_access, stream **ps)
{	return file_open_stream(fname, len, file_access,
				file_default_buffer_size, ps,
				fdev->procs.fopen);
}

private FILE *
os_fopen(file_device *fdev, const char *fname, const char *access)
{	return fopen(fname, access);
}

private int
os_fclose(file_device *fdev, FILE *file)
{	fclose(file);
	return 0;
}

private int
os_delete(file_device *fdev, const char *fname)
{	return (unlink(fname) == 0 ? 0 : e_ioerror);
}

private int
os_rename(file_device *fdev, const char *from, const char *to)
{	return (rename(from, to) == 0 ? 0 : e_ioerror);
}

private file_enum *
os_enumerate(file_device *fdev, const char *pat, uint patlen,
  const gs_memory_procs *mprocs)
{	return gp_enumerate_files_init(pat, patlen, mprocs);
}

/* ------- %stdin, %stdout, and %stderr ------ */

private int
stdin_init(file_device *fdev)
{
	/****** stdin SHOULD NOT LINE-BUFFER ******/

	byte *buf = (byte *)gs_malloc(stdin_buf_size, 1, "stdin_init");
	gs_stream_stdin = (stream *)gs_malloc(1, sizeof(stream), "stdin_init");
	sread_file(gs_stream_stdin, gs_stdin, buf, stdin_buf_size);
	s_init_read_id(gs_stream_stdin);
	gs_stream_stdin->procs.close = file_close_finish;
	return 0;
}

private int
stdin_open(file_device *fdev, const char *access, stream **ps)
{	if ( strcmp(access, "r") )
		return_error(e_invalidfileaccess);
	*ps = gs_stream_stdin;
	return 0;
}

private int
stdout_init(file_device *fdev)
{	byte *buf = (byte *)gs_malloc(stdout_buf_size, 1, "stdout_init");
	gs_stream_stdout = (stream *)gs_malloc(1, sizeof(stream), "stdout_init");
	swrite_file(gs_stream_stdout, gs_stdout, buf, stdout_buf_size);
	s_init_write_id(gs_stream_stdout);
	gs_stream_stdout->procs.close = file_close_finish;
	return 0;
}

private int
stdout_open(file_device *fdev, const char *access, stream **ps)
{	if ( strcmp(access, "w") )
		return_error(e_invalidfileaccess);
	*ps = gs_stream_stdout;
	return 0;
}

private int
stderr_init(file_device *fdev)
{	byte *buf = (byte *)gs_malloc(stderr_buf_size, 1, "stderr_init");
	gs_stream_stderr = (stream *)gs_malloc(1, sizeof(stream), "stderr_init");
	swrite_file(gs_stream_stderr, gs_stderr, buf, stderr_buf_size);
	s_init_write_id(gs_stream_stderr);
	gs_stream_stderr->procs.close = file_close_finish;
	return 0;
}

private int
stderr_open(file_device *fdev, const char *access, stream **ps)
{	if ( strcmp(access, "w") )
		return_error(e_invalidfileaccess);
	*ps = gs_stream_stderr;
	return 0;
}

/* ------ %lineedit and %statementedit ------ */

private int
lineedit_init(file_device *fdev)
{
	/****** QUESTIONABLE ******/

	lineedit_buf =
		(byte *)gs_malloc(lineedit_buf_size, 1, "lineedit_init");
	return 0;
}

private int
lineedit_open(file_device *fdev, const char *access, stream **ps)
{	uint count;
	int code;
	stream *s;
	if ( strcmp(access, "r") )
		return_error(e_invalidfileaccess);
	s = file_alloc_stream();
	if ( s == 0 )
		return_error(e_VMerror);
	code = zreadline_from(lineedit_buf, lineedit_buf_size, &count,
			      gs_stream_stdin);
	if ( code != 1 )
		return (code < 0 ? code : e_undefinedfilename /* EOF */);
	sread_string(s, lineedit_buf, count);
	s->save_close = s->procs.close;
	s->procs.close = file_close_disable;
	*ps = s;
	return 0;
}

private int
statementedit_open(file_device *fdev, const char *access, stream **ps)
{	/* NOT IMPLEMENTED PROPERLY YET */
	return lineedit_open(fdev, access, ps);
}

/* ------ Initialization procedure ------ */

op_def zfiledev_op_defs[] = {
	op_def_end(zfiledev_init)
};
