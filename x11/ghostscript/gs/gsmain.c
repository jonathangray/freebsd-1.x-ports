/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gsmain.c */
/* Common support for Ghostscript front ends */
#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "gp.h"
#include "gsmatrix.h"			/* for gxdevice.h */
#include "gxdevice.h"
#include "gserrors.h"
#include "estack.h"
#include "ostack.h"			/* put here for files.h */
#include "stream.h"			/* for files.h */
#include "files.h"
#include "iscan.h"
#include "main.h"
#include "store.h"

/* ------ Exported data ------ */

uint gs_memory_chunk_size = 20000;
int gs_user_errors;

/* File name search paths */
const char **gs_lib_paths;
private int gs_lib_count;
char *gs_lib_env_path;

/* ------ Imported data ------ */

/* Configuration information imported from gconfig.c. */
extern gx_device *gx_device_list[];
extern const char *gs_lib_default_path;
extern const char *gs_init_file;
extern ref gs_init_file_array[];

/* Imported from gsmisc.c */
extern int gs_log_errors;
extern FILE *gs_debug_out;

/* ------ Forward references ------ */

private int gs_run_init_file(P2(int *, ref *));
private int gs_run_file_open(P2(const char *, ref *));

/* ------ Initialization ------ */

/* Remember how much initialization has been done. */
private int init1_done, init2_done;

/* A handy way to declare and execute an initialization procedure: */
#define call_init(proc)\
{ extern void proc(P0()); proc(); }

/* Initialization to be done before anything else. */
void
gs_init0(FILE *in, FILE *out, FILE *err, int max_lib_paths)
{	/* Set the Ghostscript versions of stdin/out/err. */
	gs_stdin = in;
	gs_stdout = out;
	gs_stderr = err;
	gs_debug_out = gs_stdout;
	/* Do platform-dependent initialization. */
	/* We have to do this as the very first thing, */
	/* because it detects attempts to run 80N86 executables (N>0) */
	/* on incompatible processors. */
	gp_init();
	/* Initialize the file search paths */
	gs_lib_env_path = 0;
	gs_lib_paths =
		(const char **)gs_malloc(max_lib_paths + 3, sizeof(char *),
					 "gs_lib_paths array");
	gs_lib_count = 0;
	gs_user_errors = 1;
	gs_log_errors = 0;
	/* Reset debugging flags */
	memset(gs_debug, 0, 128);
	init1_done = init2_done = 0;
}

/* Initialization to be done before constructing any objects. */
void
gs_init1(void)
{	if ( !init1_done )
	   {	alloc_init(&gs_default_memory_procs, gs_memory_chunk_size);
		call_init(name_init)
		call_init(obj_init)		/* requires name_init */
		call_init(scan_init)		/* ditto */
		init1_done = 1;
	   }
}

/* Initialization to be done before running any files. */
void
gs_init2(void)
{	gs_init1();
	if ( !init2_done )
	   {	int code, exit_code;
		ref error_object;
		call_init(igs_init)
		call_init(zop_init)
		interp_init(1);		/* requires obj_init */
		call_init(op_init)	/* requires obj_init, scan_init */
		/* Set up the array of additional initialization files. */
		{	ref *ifp = gs_init_file_array;
			ref ifa;
			for ( ; ifp->value.bytes != 0; ifp++ )
			  r_set_size(ifp, strlen((const char *)ifp->value.bytes));
			make_tasv(&ifa, t_array, a_readonly,
				  ifp - gs_init_file_array, refs,
				  gs_init_file_array);
			initial_enter_name("INITFILES", &ifa);
		}
		/* Execute the standard initialization file. */
		code = gs_run_init_file(&exit_code, &error_object);
		if ( code < 0 )
		{	if ( code != gs_error_Fatal )
				gs_debug_dump_stack(code, &error_object);
			gs_exit_with_code((exit_code ? exit_code : 2), code);
		}
		init2_done = 1;
	   }
   }

/* Add a library search path to the list. */
void
gs_add_lib_path(const char *lpath)
{	gs_lib_paths[gs_lib_count] = lpath;
	gs_lib_count++;
	gs_set_lib_paths();
}

/* ------ Execution ------ */

/* Complete the list of library search paths. */
void
gs_set_lib_paths(void)
{	const char **ppath = &gs_lib_paths[gs_lib_count];
	if ( gs_lib_env_path != 0 ) *ppath++ = gs_lib_env_path;
	if ( gs_lib_default_path != 0 ) *ppath++ = gs_lib_default_path;
	*ppath = 0;
}

/* Open a file, using the search paths. */
int
gs_lib_open(const char *file_name, ref *pfile)
{	/* This is a separate procedure only to avoid tying up */
	/* extra stack space while running the file. */
#define maxfn 200
	byte fn[maxfn];
	uint len;
	return lib_file_open(file_name, strlen(file_name), fn, maxfn,
			     &len, pfile);
}

/* Open and execute a file. */
int
gs_run_file(const char *file_name, int user_errors, int *pexit_code, ref *perror_object)
{	ref initial_file;
	int code = gs_run_file_open(file_name, &initial_file);
	if ( code < 0 ) return code;
	return gs_interpret(&initial_file, user_errors, pexit_code, perror_object);
}
private int
gs_run_file_open(const char *file_name, ref *pfref)
{	gs_set_lib_paths();
	if ( gs_lib_open(file_name, pfref) < 0 )
	{	eprintf1("Can't find initialization file %s.\n", file_name);
		return_error(gs_error_Fatal);
	}
	r_set_attrs(pfref, a_execute + a_executable);
	return 0;
}

/* Open and run the very first initialization file. */
private int
gs_run_init_file(int *pexit_code, ref *perror_object)
{	ref initial_file;
	ref first_token;
	int code;
	const char *file_name = gs_init_file;
	/* Check to make sure the first token is an integer */
	/* (for the version number check.) */
	code = gs_run_file_open(file_name, &initial_file);
	if ( code < 0 )
		return code;
	code = scan_token(initial_file.value.pfile, 0, &first_token);
	if ( code < 0 || !r_has_type(&first_token, t_integer) )
	{	eprintf1("Initialization file %s is not compatible with this interpreter.\n", file_name);
		*pexit_code = 255;
		return_error(gs_error_Fatal);
	}
	*++osp = first_token;
	return gs_interpret(&initial_file, gs_user_errors, pexit_code, perror_object);
}

/* Run a string. */
int
gs_run_string(const char *str, int user_errors, int *pexit_code, ref *perror_object)
{	ref fref;
	int code;
	gs_set_lib_paths();
	code = file_read_string((const byte *)str, (uint)strlen(str), &fref);
	if ( code < 0 ) return code;
	r_set_attrs(&fref, a_executable);
	code = gs_interpret(&fref, user_errors, pexit_code, perror_object);
	file_close(&fref);
	return code;
}

/* ------ Termination ------ */

/* Free all resources and exit. */
void gs_malloc_release(P0());
int gs_closedevice(P1(gx_device *));
void
gs_finit(int exit_status, int code)
{	gx_device **pdev = gx_device_list;
	fflush(stderr);			/* in case of error exit */
	for ( ; *pdev != 0; pdev++ )
		gs_closedevice(*pdev);
	/* Do the equivalent of a restore "past the bottom". */
	/* This will release all memory, close all open files, etc. */
	if ( init1_done )
		alloc_restore_all();
	/* Do platform-specific cleanup. */
	gp_exit(exit_status, code);
}
void
gs_exit_with_code(int exit_status, int code)
{	gs_finit(exit_status, code);
	exit(exit_status);
}
void
gs_exit(int exit_status)
{	gs_exit_with_code(exit_status, 0);
}

/* ------ Debugging ------ */

/* Debugging code */
extern void debug_print_ref(P1(const ref *));
extern void debug_dump_refs(P3(const ref *, uint, const char *));

/* Dump the stacks after interpretation */
void
gs_debug_dump_stack(int code, ref *perror_object)
{	zflush(osp);	/* force out buffered output */
	dprintf1("\nUnexpected interpreter error %d.\n", code);
	if ( perror_object != 0 )
	{	dputs("Error object: ");
		debug_print_ref(perror_object);
		dputc('\n');
	}
	debug_dump_refs(osbot, osp + 1 - osbot, "Operand stack");
	debug_dump_refs(esbot, esp + 1 - esbot, "Execution stack");
}

/* Log an error return */
#ifdef DEBUG
int
gs_log_error(int err, const char _ds *file, int line)
{	if ( gs_log_errors )
	  { if ( file == NULL )
	      dprintf1("Returning error %d.\n", err);
	    else
	      dprintf3("%s(%d): Returning error %d.\n",
		       (char *)file, line, err);
	  }
	return err;
}
#endif
