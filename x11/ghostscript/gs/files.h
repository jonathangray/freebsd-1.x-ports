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

/* files.h */
/* Common declarations for zfile.c and zfileio.c */
/* Requires stream.h */

/*
 * File objects store a pointer to a stream in value.pfile.
 * A file object is valid if its "size" matches the read_id or write_id
 * (as appropriate) in the stream it points to.  This arrangement
 * allows us to detect closed files reliably, while allowing us to
 * reuse closed streams for new files.
 */
#define fptr(pref) (pref)->value.pfile
#define make_file(pref,a,id,s)\
  make_tasv(pref,t_file,a,id,pfile,s)

/* The standard files. */
extern stream *gs_stream_stdin;
extern stream *gs_stream_stdout;
extern stream *gs_stream_stderr;
/* An invalid file. */
extern stream *invalid_file_entry;

/* Macros for checking file validity. */
#define check_file_access(svar,op,acc)\
   {	svar = fptr(op);	/* do first, acc may refer to it */\
	if ( !(acc) ) return_error(e_invalidaccess);\
   }
#define check_file_ref(svar,op,acc)\
   {	check_type(*(op), t_file);\
	check_file_access(svar, op, acc);\
   }
#define check_file(svar,op)\
	check_file_ref(svar, op, (svar->read_id | svar->write_id) == r_size(op))

/*
 * If a file is open for both reading and writing, its read_id, write_id,
 * and stream procedures and modes reflect the current mode of use;
 * an id check failure will switch it to the other mode.
 */
int file_switch_to_read(P1(ref *));
#define check_read_file(svar,op)\
   {	check_read_type(*(op), t_file);\
	check_file_access(svar, op,\
		(svar->read_id == r_size(op) || file_switch_to_read(op)));\
   }
int file_switch_to_write(P1(ref *));
#define check_write_file(svar,op)\
   {	check_write_type(*(op), t_file);\
	check_file_access(svar, op,\
		(svar->write_id == r_size(op) || file_switch_to_write(op)));\
   }

/* Procedures exported by zfile.c. */
	/* for gs.c */
FILE *lib_fopen(P1(const char *));
	/* for gsmain.c */
int lib_file_open(P6(const char *, uint, byte *, uint, uint *, ref *));
int file_read_string(P3(const byte *, uint, ref *));
	/* for os_open in zfiledev.c */
/*int file_open_stream(P6(const byte *, uint, const char *, uint,
  stream **, fdev_proc_fopen_t));*/
	/* for zfilter.c */
int filter_open(P5(const char *, uint, ref *, const stream_procs _ds *,
  stream **));
	/* for zfiledev.c */
int file_close_finish(P1(stream *));
int file_close_disable(P1(stream *));
	/* for gsmain.c, interp.c */
int file_close(P1(ref *));
	/* for zfiledev.c */
stream *file_alloc_stream(P0());
	/* for isave.c */
void file_save(P0());
/*void file_restore(P1(const alloc_save *));*/

/* Procedures exported by zfileio.c. */
	/* for zfiledev.c */
int zreadline_from(P4(byte *, uint, uint *, stream *));
	/* for zstring.c */
int ztoken_file(P1(os_ptr));
	/* for interp.c */
int file_check_read(P2(ref *, stream **));
	/* for file_close_finish in zfile.c */
void check_close_current_file(P1(stream *));
