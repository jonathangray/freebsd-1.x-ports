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

/* filedev.h */
/* Definition of a file `device' object */

/*
 * Note that file devices have nothing to do with Ghostscript output
 * devices.  See section 3.8.2 of the PostScript Language Reference Manual,
 * Second Edition, for more information.
 */

struct file_device_s;		/* defined here */
typedef struct file_device_s file_device;

struct file_device_procs_s;	/* defined here */
typedef struct file_device_procs_s file_device_procs;

#ifndef file_enum_DEFINED		/* also defined in gp.h */
#  define file_enum_DEFINED
struct file_enum_s;	/* opaque to client, defined by implementors */
typedef struct file_enum_s file_enum;
#endif

/* Definition of file device procedures */
/* Note that file names for delete, rename, and fopen are C strings, */
/* not pointer + length. */

struct file_device_procs_s {

#define fdev_proc_init(proc)\
  int proc(P1(file_device *fdev))
	fdev_proc_init((*init));	/* one-time initialization */

#define fdev_proc_open_device(proc)\
  int proc(P3(file_device *fdev, const char *access, stream **ps))
	fdev_proc_open_device((*open_device));

#define fdev_proc_open_file(proc)\
  int proc(P5(file_device *fdev, const char *fname, uint namelen,\
	      const char *access, stream **ps))
	fdev_proc_open_file((*open_file));

#define fdev_proc_fopen(proc)\
  FILE *proc(P3(file_device *fdev, const char *fname, const char *access))
	fdev_proc_fopen((*fopen));

#define fdev_proc_fclose(proc)\
  int proc(P2(file_device *fdev, FILE *file))
	fdev_proc_fclose((*fclose));

#define fdev_proc_delete_file(proc)\
  int proc(P2(file_device *fdev, const char *fname))
	fdev_proc_delete_file((*delete_file));

#define fdev_proc_rename_file(proc)\
  int proc(P3(file_device *fdev, const char *from, const char *to))
	fdev_proc_rename_file((*rename_file));

#define fdev_proc_enumerate_files(proc)\
  file_enum *proc(P4(file_device *fdev, const char *pat, uint patlen,\
		     const gs_memory_procs *mprocs))
	fdev_proc_enumerate_files((*enumerate_files));

#define fdev_proc_enumerate_next(proc)\
  uint proc(P3(file_enum *pfen, char *ptr, uint maxlen))
	fdev_proc_enumerate_next((*enumerate_next));

#define fdev_proc_enumerate_close(proc)\
  void proc(P1(file_enum *pfen))
	fdev_proc_enumerate_close((*enumerate_close));

};

/* The following typedef is needed because ansi2knr can't handle */
/* fdev_proc_fopen((*procname)) in a formal argument list. */
typedef fdev_proc_fopen((*fdev_proc_fopen_t));

/* Default implementations of procedures */
fdev_proc_init(fdev_no_init);
fdev_proc_open_device(fdev_no_open_device);
fdev_proc_open_file(fdev_no_open_file);
fdev_proc_fopen(fdev_no_fopen);
fdev_proc_fclose(fdev_no_fclose);
fdev_proc_delete_file(fdev_no_delete_file);
fdev_proc_rename_file(fdev_no_rename_file);
fdev_proc_enumerate_files(fdev_no_enumerate_files);
/* The %os% implemention of open_file */
fdev_proc_open_file(fdev_os_open_file);

/* Finally, the device structure itself. */

struct file_device_s {
	const char *dname;		/* the file device name */
	const file_device_procs procs;
};
