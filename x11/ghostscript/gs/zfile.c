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

/* zfile.c */
/* Non-I/O file operators for Ghostscript */
#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "gp.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "estack.h"			/* for filenameforall, file_close */
#include "ilevel.h"			/* %names only work in Level 2 */
#include "iutil.h"
#include "save.h"			/* for restore */
#include "stream.h"
#include "filedev.h"			/* must come after stream.h */
#include "files.h"			/* ditto */
#include "store.h"

/* Import the file device table from zfiledev.c. */
extern file_device *file_device_table[];
#define fdev_default file_device_table[0]

/* Forward references: file name parsing. */
/* Parsed file name type.  Note that the file name may be either a */
/* Ghostscript string (no terminator) or a C string (null terminator). */
typedef struct parsed_file_name_s {
	file_device *fdev;
	const char *fname;
	uint len;
} parsed_file_name;
private int parse_file_name(P2(os_ptr, parsed_file_name *));
private int parse_real_file_name(P3(os_ptr, parsed_file_name *,
  const char *));
private void free_real_file_name(P2(parsed_file_name *, const char *));

/* Forward references: file opening. */
int file_open(P6(const byte *, uint, const char *, uint, ref *, stream **));
stream *file_alloc_stream(P0());
int file_open_stream(P6(const byte *, uint, const char *,
  uint, stream **, fdev_proc_fopen_t));
private void make_stream_file(P3(ref *, stream *, const char *));

/* Forward references: other. */
private int file_close_file(P1(stream *));

/* Imported from gs.c */
extern char **gs_lib_paths;		/* search path list, */
					/* terminated by a null pointer */

/*
 * Since there can be many file objects referring to the same file/stream,
 * we can't simply free a stream when we close it.  On the other hand,
 * we don't want freed streams to clutter up memory needlessly.
 * Our solution is to retain the freed streams, and reuse them.
 * To prevent an old file object from being able to access a reused stream,
 * we keep a serial number in each stream, and check it against a serial
 * number stored in the file object (as the "size"); when we close a file,
 * we increment its serial number.  If the serial number ever overflows,
 * we leave it at zero, and do not reuse the stream.
 * (This will never happen.)
 *
 * Storage management for this scheme is a little tricky.
 * We maintain an invariant that says that a stream opened at a given
 * save level always uses a stream structure allocated at that level.
 * By doing this, we don't need to keep track separately of streams open
 * at a level vs. streams allocated at a level; this simplifies things.
 */

/*
 * The chain of all streams allocated at the current level.
 * We need this so that we can do the right thing for restore.
 * Note that this chain includes both open and closed files.
 */
private ref file_list_ref;		/* t_file */
#define file_list file_list_ref.value.pfile

/* File buffer sizes.  For real files, this is arbitrary, */
/* since the C library does its own buffering in addition. */
/* stdout and stderr use smaller buffers, */
/* on the assumption that they are usually not real files. */
/* The buffer size for type 1 encrypted files is NOT arbitrary: */
/* it must be at most 512. */
#define default_buffer_size 512
const uint file_default_buffer_size = default_buffer_size;

/* An invalid file object */
stream *invalid_file_entry;

/* Initialize the file table */
private void
zfile_init(void)
{
	invalid_file_entry =
		(stream *)gs_malloc(1, sizeof(stream), "zfile_init");
	s_disable(invalid_file_entry);
	s_init_no_id(invalid_file_entry);

	/* Initialize the bookkeeping lists. */
	
	make_file(&file_list_ref, 0, 0, 0);
}

/* <name_string> <access_string> file <file> */
int
zfile(register os_ptr op)
{	char file_access[3];
	parsed_file_name pname;
	const byte *astr;
	int code;
	stream *s;
	check_read_type(*op, t_string);
	astr = op->value.const_bytes;
	switch ( r_size(op) )
	   {
	case 2:
		if ( astr[1] != '+' )
			return_error(e_invalidfileaccess);
		file_access[1] = '+';
		file_access[2] = 0;
		break;
	case 1:
		file_access[1] = 0;
		break;
	default:
		return_error(e_invalidfileaccess);
	   }
	switch ( astr[0] )
	   {
	case 'r': case 'w': case 'a':
		break;
	default:
		return_error(e_invalidfileaccess);
	   }
	file_access[0] = astr[0];
	code = parse_file_name(op - 1, &pname);
	if ( code < 0 )
		return code;
	if ( pname.fdev == NULL )
		pname.fdev = fdev_default;
	if ( pname.fname == NULL )		/* just a device */
		code = (*pname.fdev->procs.open_device)(pname.fdev,
					file_access, &s);
	else					/* file */
		code = (*pname.fdev->procs.open_file)(pname.fdev,
					pname.fname, pname.len,
					file_access, &s);
	if ( code < 0 )
		return code;
	make_stream_file(op - 1, s, file_access);
	pop(1);
	return code;
}

/* <file> closefile - */
int
zclosefile(register os_ptr op)
{	stream *s;
	check_file(s, op);
	switch ( sclose(s) )
	{
	case 0:
		pop(1);
		return 0;
	default:
		return_error(e_ioerror);
	}
}

/* ------ Level 2 extensions ------ */

/* <string> deletefile - */
int
zdeletefile(register os_ptr op)
{	parsed_file_name pname;
	int code = parse_real_file_name(op, &pname, "deletefile");
	if ( code < 0 )
		return code;
	code = (*pname.fdev->procs.delete_file)(pname.fdev, pname.fname);
	free_real_file_name(&pname, "deletefile");
	if ( code < 0 )
		return code;
	pop(1);
	return 0;
}

/* <template> <proc> <scratch> filenameforall - */
/****** NOT CONVERTED FOR FILE DEVICES YET ******/
private int file_continue(P1(os_ptr));
private int file_cleanup(P1(os_ptr));
int
zfilenameforall(register os_ptr op)
{	file_enum *pfen;
	int code;
	check_write_type(*op, t_string);
	check_proc(op[-1]);
	check_read_type(op[-2], t_string);
	/* Push a mark, the pattern, the scratch string, the enumerator, */
	/* and the procedure, and invoke the continuation. */
	check_estack(7);
	pfen = gp_enumerate_files_init((char *)op[-2].value.bytes, r_size(op - 2), &alloc_memory_procs);
	if ( pfen == 0 )
		return_error(e_VMerror);
	mark_estack(es_for, file_cleanup);
	*++esp = op[-2];
	*++esp = *op;
	++esp;
	make_tasv(esp, t_string, a_read+a_execute+a_executable, 0,
		  bytes, (byte *)pfen);
	*++esp = op[-1];
	pop(3);  op -= 3;
	code = file_continue(op);
	return (code == o_pop_estack ? o_push_estack : code);
}
/* Continuation operator for enumerating files */
private int
file_continue(register os_ptr op)
{	es_ptr pscratch = esp - 2;
	file_enum *pfen = (file_enum *)esp[-1].value.bytes;
	uint len = r_size(pscratch);
	uint code = gp_enumerate_files_next(pfen, (char *)pscratch->value.bytes, len);
	if ( code == ~(uint)0 )		/* all done */
	   {	esp -= 4;		/* pop proc, pfen, scratch, mark */
		return o_pop_estack;
	   }
	else if ( code > len )		/* overran string */
		return_error(e_rangecheck);
	else
	   {	push(1);
		ref_assign(op, pscratch);
		r_set_size(op, code);
		push_op_estack(file_continue);	/* come again */
		*++esp = pscratch[2];	/* proc */
		return o_push_estack;
	   }
}
/* Cleanup procedure for enumerating files */
private int
file_cleanup(os_ptr op)
{	gp_enumerate_files_close((file_enum *)esp[4].value.bytes);
	return 0;
}

/* <string1> <string2> renamefile - */
int
zrenamefile(register os_ptr op)
{	parsed_file_name pname1, pname2;
	int code = parse_real_file_name(op - 1, &pname1, "renamefile(from)");
	if ( code < 0 )
		return code;
	pname2.fname = 0;
	code = parse_real_file_name(op, &pname2, "renamefile(to)");
	if ( code < 0 || pname1.fdev != pname2.fdev ||
	     (code = (*pname1.fdev->procs.rename_file)(pname1.fdev,
				     pname1.fname, pname2.fname)) < 0
	   )
	{	if ( code >= 0 )
			code = gs_note_error(e_invalidfileaccess);
	}
	free_real_file_name(&pname2, "renamefile(to)");
	free_real_file_name(&pname1, "renamefile(from)");
	if ( code < 0 )
		return code;
	pop(2);
	return 0;
}	

/* <file> status <open_bool> */
/* <string> status <pages> <bytes> <ref_time> <creation_time> true */
/* <string> status false */
int
zstatus(register os_ptr op)
{	switch ( r_type(op) )
	   {
	case t_file:
		make_bool(op, (s_is_valid(fptr(op)) ? 1 : 0));
		return 0;
	case t_string:
	{	parsed_file_name pname;
		file_status fstat;
		int code = parse_real_file_name(op, &pname, "status");
		if ( code < 0 )
			return 0;
		if ( pname.fdev == fdev_default &&
		     gp_file_status(pname.fname, &fstat)
		   )
		{	push(4);
			make_int(op - 4, fstat.size_pages);
			make_int(op - 3, fstat.size_bytes);
			make_int(op - 2, fstat.time_referenced);
			make_int(op - 1, fstat.time_created);
			make_bool(op, 1);
		}
		else
			make_bool(op, 0);
		free_real_file_name(&pname, "status");
	}	return 0;
	default:
		return_error(e_typecheck);
	}
}

/* ------ Ghostscript extensions ------ */

/* <string> findlibfile <found_string> <file> true */
/* <string> findlibfile false */
int
zfindlibfile(register os_ptr op)
{	int code;
#define maxclen 200
	byte cname[maxclen];
	uint clen;
	parsed_file_name pname;
	stream *s;
	check_ostack(2);
	code = parse_file_name(op, &pname);
	if ( code < 0 )
		return code;
	if ( pname.fdev == NULL )
		pname.fdev = fdev_default;
	if ( pname.fdev != fdev_default )
	{	/* Non-OS devices don't have search paths (yet). */
		code = (*pname.fdev->procs.open_file)(pname.fdev,
					pname.fname, pname.len, "r", &s);
		if ( code < 0 )
		{	push(1);
			make_false(op);
			return 0;
		}
		make_stream_file(op + 1, s, "r");
	}
	else
	{	byte *cstr;
		code = lib_file_open(pname.fname, pname.len, cname, maxclen,
				     &clen, op + 1);
		if ( code < 0 )
		{	push(1);
			make_false(op);
			return 0;
		}
		cstr = (byte *)alloc(clen, 1, "findlibfile");
		if ( cstr == 0 )
			return_error(e_VMerror);
		memcpy(cstr, cname, clen);
		make_string(op, a_all, clen, cstr);
	}
	push(2);
	make_true(op);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zfile_op_defs[] = {
	{"1closefile", zclosefile},
	{"1deletefile", zdeletefile},
	{"2file", zfile},
	{"3filenameforall", zfilenameforall},
	{"1findlibfile", zfindlibfile},
	{"2renamefile", zrenamefile},
	{"1status", zstatus},
		/* Internal operators */
	{"0%file_continue", file_continue},
	op_def_end(zfile_init)
};

/* ------ Stream opening ------ */

/* Make a t_file reference to a stream. */
private void
make_stream_file(ref *pfile, stream *s, const char *access)
{	uint attrs =
		(access[1] == '+' ? a_write + a_read + a_execute : 0);
	if ( access[0] == 'r' )
	{	make_file(pfile, attrs | (a_read | a_execute), s->read_id, s);
		s->write_id = 0;
	}
	else
	{	make_file(pfile, attrs | a_write, s->write_id, s);
		s->read_id = 0;
	}
}

/* Open an OS-level file (like fopen), using the search paths if necessary. */
/* Note that it overwrites the file name; the 'const' on bname is a lie. */
private FILE *
lib_file_fopen(file_device *fdev, const char *bname,
  const char *ignore_access)
{	char fmode[3];			/* r, [b], null */
	FILE *file;
	int len = strlen(bname);
	char **ppath;
	strcpy(fmode, "r");
	strcat(fmode, gp_fmode_binary_suffix);
	file = (*fdev->procs.fopen)(fdev, bname, fmode);
	if ( file )
		return file;
	if ( gp_file_name_is_absolute(bname, len) )
		return 0;
	/* Go through the list of search paths */
	for ( ppath = gs_lib_paths; *ppath != 0; ppath++ )
	   {	char *path = *ppath;
		for ( ; ; )
		   {	/* Find the end of the next path */
			char *npath = path;
			uint plen;
			const char *cstr;
			int up, i;
			while ( *npath != 0 && *npath != gp_file_name_list_separator )
				npath++;
			plen = npath - path;
			cstr = gp_file_name_concat_string(path, plen,
							  (const char *)bname,
							  len);
			/* Concatenate the prefix, combiner, and file name. */
			up = plen + strlen(cstr);
#define wbname ((char *)bname)		/* break const */
			for ( i = len + 1; --i >= 0; )
				wbname[i + up] = wbname[i];
			memcpy(wbname, (byte *)path, plen);
			memcpy(wbname + plen, cstr, strlen(cstr));
			file = (*fdev->procs.fopen)(fdev, bname, fmode);
			if ( file )
				return file;
			strcpy(wbname, wbname + up);
#undef wbname
			if ( !*npath ) break;
			path = npath + 1;
		   }
	   }
	return 0;
}
/* The startup code calls this to open @-files. */
FILE *
lib_fopen(const char *bname)
{	return lib_file_fopen(fdev_default, bname, "r");
}

/* Open a file stream on an OS file and create a file object, */
/* using the search paths. */
/* The startup code calls this to open the initialization file gs_init.ps. */
int
lib_file_open(const char *fname, uint len, byte *cname, uint max_clen,
  uint *pclen, ref *pfile)
{	stream *s;
	int code = file_open_stream((const byte *)fname, len, "r",
				    default_buffer_size, &s, lib_file_fopen);
	char *bname;
	uint blen;
	if ( code < 0 ) return code;
	/* Get the name from the stream buffer. */
	bname = (char *)s->cbuf;
	blen = strlen(bname);
	if ( blen > max_clen )
	{	sclose(s);
		return_error(e_limitcheck);
	}
	memcpy(cname, bname, blen);
	*pclen = blen;
	make_stream_file(pfile, s, "r");
	return 0;
}

/* Open a file stream that reads a string. */
/* (This is currently used only by gs_run_string.) */
int
file_read_string(const byte *str, uint len, ref *pfile)
{	register stream *s = file_alloc_stream();
	if ( s == 0 )
		return_error(e_VMerror);
	sread_string(s, str, len);
	s->write_id = 0;
	make_file(pfile, a_readonly, s->read_id, s);
	s->save_close = s->procs.close;
	s->procs.close = file_close_file;
	return 0;
}

/* Open a file stream, optionally on an OS file. */
/* Return 0 if successful, error code if not. */
/* On a successful return, the C file name is in the stream buffer. */
/* If fname==0, set up the file entry, stream, and buffer, */
/* but don't open an OS file or initialize the stream. */
int
file_open_stream(const byte *fname, uint len, const char *file_access,
  uint buffer_size, stream **ps, fdev_proc_fopen_t fopen_proc)
{	byte *buffer;
	register stream *s;
	if ( buffer_size == 0 )
	  buffer_size = default_buffer_size;
	if ( len >= buffer_size )
	  return_error(e_limitcheck);	/* we copy the file name into the buffer */
	/* Allocate the stream first, since it persists */
	/* even after the file has been closed. */
  	s = file_alloc_stream();
	if ( s == 0 )
		return_error(e_VMerror);
	/* Allocate the buffer. */
	buffer = (byte *)alloc(buffer_size, 1, "file_open(buffer)");
	if ( buffer == 0 )
		return_error(e_VMerror);
	if ( fname != 0 )
	   {	/* Copy the name (so we can terminate it with a zero byte.) */
		char *file_name = (char *)buffer;
		char fmode[4];		/* r/w/a, [+], [b], null */
		FILE *file;
		memcpy(file_name, fname, len);
		file_name[len] = 0;		/* terminate string */
		/* Open the file, always in binary mode. */
		strcpy(fmode, file_access);
		strcat(fmode, gp_fmode_binary_suffix);
		/****** fdev_default IS QUESTIONABLE ******/
		file = (*fopen_proc)(fdev_default, file_name, fmode);
		if ( file == 0 )
		   {	alloc_free((char *)buffer, buffer_size, 1,
				   "file_open(buffer)");
			return_error(e_undefinedfilename);
		   }
		/* Set up the stream. */
		switch ( *file_access )
		{
		case 'a':
			sappend_file(s, file, buffer, buffer_size);
			break;
		case 'r':
			sread_file(s, file, buffer, buffer_size);
			break;
		case 'w':
			swrite_file(s, file, buffer, buffer_size);
		}
		s->save_close = s->procs.close;
		s->procs.close = file_close_file;
	   }
	else				/* save the buffer and size */
	   {	s->cbuf = buffer;
		s->bsize = s->cbsize = buffer_size;
	   }
	*ps = s;
	return 0;
}

/* Open a file stream for a filter. */
int
filter_open(const char *file_access, uint buffer_size,
  ref *pfile, const stream_procs _ds *procs, stream **ps)
{	stream *s;
	int code = file_open_stream((byte *)0, 0, file_access,
				    buffer_size, &s, (fdev_proc_fopen_t)0);
	if ( code < 0 )
		return code;
	s_std_init(s, s->cbuf, s->bsize, procs,
		   (*file_access == 'r' ? s_mode_read : s_mode_write));
	s->file = 0;			/* not a file stream */
	make_stream_file(pfile, s, file_access);
	s->save_close = s->procs.close;
	s->procs.close = file_close_file;
	*ps = s;
	return 0;
}

/* ------ Stream closing ------ */

/* Finish closing a file stream by checking whether it is currentfile. */
/* This replaces the close procedure for the std* streams, */
/* which cannot actually be closed. */
/* This is exported for zfiledev.c. */
int
file_close_finish(stream *s)
{	check_close_current_file(s);
	return 0;
}

/* Close a file stream, but don't deallocate the buffer. */
/* This replaces the close procedure for %lineedit and %statementedit. */
/* (This is WRONG: these streams should allocate a new buffer each time */
/* they are opened, but that would overstress the allocator right now.) */
/* This is exported for zfiledev.c. */
int
file_close_disable(stream *s)
{	int code = (*s->save_close)(s);
	if ( code )
		return code;
	/* Increment the IDs to prevent further access. */
	s->read_id = s->write_id = (s->read_id | s->write_id) + 1;
	return file_close_finish(s);
}

/* Close a file stream.  This replaces the close procedure in the stream */
/* for normal (OS) files and for filters. */
private int
file_close_file(stream *s)
{	stream *stemp = (s->strm_is_temp ? s->strm : 0);
	int code = file_close_disable(s);
	if ( code )
		return code;
	alloc_free((char *)s->cbuf, s->cbsize, 1,
		   "file_close(buffer)");
	if ( stemp != 0 )
		alloc_free((char *)stemp, 1, sizeof(stream),
			   "file_close(sub-stream)");
	return 0;
}

/* Close a file object. */
int
file_close(ref *pfile)
{	if ( sclose(fptr(pfile)) )
		return_error(e_ioerror);
	return 0;
}

/* ------ Internal routines ------ */

/* Parse a file name into device and individual name. */
/* The device may be NULL, or the name may be NULL, but not both. */
/* Note that %disk0 has name==NULL, whereas %disk0% just has len==0. */
private int
parse_file_name(os_ptr op, parsed_file_name *pfn)
{	const byte *pname;
	uint len, dlen;
	const byte *pdelim;
	file_device **pftab;

	check_read_type(*op, t_string);
	len = r_size(op);
	pname = op->value.const_bytes;
	if ( len == 0 )
		return_error(e_undefinedfilename);
	if ( pname[0] != '%' )	/* no device */
	  { pfn->fdev = NULL;
	    pfn->fname = (const char *)pname;
	    pfn->len = len;
	    return 0;
	  }
	pname++, len--;
	pdelim = (const byte *)memchr(pname, '%', len);
	if ( pdelim == NULL )	/* device only */
		dlen = len;
	else
	{	dlen = pdelim - pname;
		pdelim++, len--;
	}
	for ( pftab = file_device_table; *pftab != NULL; pftab++ )
	  { const char *dname = (*pftab)->dname;
	    if ( strlen(dname) == dlen && !memcmp(pname, dname, dlen) )
	      { pfn->fdev = *pftab;
		pfn->fname = (const char *)pdelim;
		pfn->len = len - dlen;
		return 0;
	      }
	  }
	return_error(e_undefinedfilename);
}

/* Parse a real (non-device) file name and convert to a C string. */
private int
parse_real_file_name(os_ptr op, parsed_file_name *pfn, const char *cname)
{	int code = parse_file_name(op, pfn);
	const char *fname;
	ref fnref;
	uint len = pfn->len;
	if ( code < 0 )
		return code;
	if ( pfn->fdev == NULL )	 /* no device */
		pfn->fdev = fdev_default;
	if ( len == 0 )
		return_error(e_invalidfileaccess); /* device only */
	fnref.value.const_bytes = (const byte *)pfn->fname;
	r_set_size(&fnref, len);
	fname = ref_to_string(&fnref, cname);
	if ( fname == 0 )
		return_error(e_VMerror);
	pfn->fname = fname;
	pfn->len = len + 1;	/* null terminator */
	return 0;
}

/* Free a file name that was copied to a C string. */
private void
free_real_file_name(parsed_file_name *pfn, const char *cname)
{	if ( pfn->fname != 0 )
		alloc_free((char *)pfn->fname, pfn->len, 1, cname);
}

/* Allocate and return a file stream. */
/* Return 0 if the allocation failed. */
/* The stream is initialized to an invalid state, so the caller need not */
/* worry about cleaning up if a later step in opening the stream fails. */
stream *
file_alloc_stream(void)
{	stream *s;
	/* Look first for a free stream allocated at this level. */
	s = file_list;
	while ( s != 0 )
	{	if ( !s_is_valid(s) && s->read_id != 0 /* i.e. !overflowed */ )
		{	s->strm_is_temp = 0;	/* not a temp stream */
			return s;
		}
		s = s->next;
	}
	s = (stream *)alloc(1, sizeof(stream), "file_open(stream)");
	if ( s == 0 )
		return 0;
	s_init_ids(s);
	s->strm_is_temp = 0;		/* not a temp stream */
	/* Disable the stream now (in case we can't open the file, */
	/* or a filter init procedure fails) so that `restore' won't */
	/* crash when it tries to close open files. */
	s_disable(s);
	/* Add s to the list of files. */
	if ( file_list != 0 )
		file_list->prev = s;
	s->next = file_list;
	s->prev = 0;
	file_list = s;
	return s;
}

/* ------ Memory management ------ */

/* Arrange to save the current file list at a save. */
void
file_save(void)
{	ref_mark_old(&file_list_ref);
	ref_save(&file_list_ref, "file_save");
	make_file(&file_list_ref, 0, 0, 0);
}

/* Close inaccessible files just before a restore. */
void
file_restore(const alloc_save *save)
{	stream *s = file_list;
	while ( s != 0 )
	{	if ( s_is_valid(s) )
			sclose(s);
		s = s->next;
	}
	file_list = 0;
}
