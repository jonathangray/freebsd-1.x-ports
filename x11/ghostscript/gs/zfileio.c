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

/* zfileio.c */
/* File I/O operators for Ghostscript */
#include "ghost.h"
#include "gp.h"
#include "errors.h"
#include "oper.h"
#include "stream.h"
#include "estack.h"
#include "files.h"
#include "iscan.h"
#include "store.h"
#include "gsmatrix.h"			/* for gxdevice.h */
#include "gxdevice.h"
#include "gxdevmem.h"

/* Forward references */
int zreadline_from(P4(byte *, uint, uint *, stream *));
private es_ptr zget_current_file(P0());
private int write_string(P2(os_ptr, stream *));

/* ------ Operators ------ */

/* <file> read <int> true */
/* <file> read false */
int
zread(register os_ptr op)
{	stream *s;
	int ch;
	check_read_file(s, op);
	ch = sgetc(s);
	if ( ch >= 0 )
	   {	make_int(op, ch);
		push(1);
		make_bool(op, 1);
	   }
	else if ( ch == EOFC )
		make_bool(op, 0);
	else
		return_error(e_ioerror);
	return 0;
}

/* <file> <int> write - */
int
zwrite(register os_ptr op)
{	stream *s;
	ulong ch;
	check_write_file(s, op - 1);
	check_type(*op, t_integer);
	ch = op->value.intval;
	if ( ch > 0xff )
		return_error(e_rangecheck);
	sputc(s, (byte)ch);
	pop(2);
	return 0;
}

/* <file> <string> readhexstring <substring> <filled_bool> */
int
zreadhexstring(register os_ptr op)
{	stream *s;
	int code;
	uint nread;
	int odd = -1;
	check_read_file(s, op - 1);
	check_write_type(*op, t_string);
	code = sreadhex(s, op->value.bytes, r_size(op), &nread, &odd, 1);
	switch ( code )
	   {
	case EOFC:
		/* Reached end-of-file before filling the string. */
		/* Return an appropriate substring. */
		r_set_size(op, nread);
		code = 1;
		break;
	case 0:
		/* Filled the string. */
		break;
	default:			/* Error */
		return_error(e_ioerror);
	   }
	ref_assign(op - 1, op);
	make_bool(op, 1 - code);
	return 0;
}

/* <file> <string> writehexstring - */
int
zwritehexstring(register os_ptr op)
{	register stream *s;
	register byte ch;
	register const byte *p;
	register const char _ds *hex_digits = "0123456789abcdef";
	register uint len;
	check_write_file(s, op - 1);
	check_read_type(*op, t_string);
	p = op->value.bytes;
	len = r_size(op);
	while ( len-- )
	   {	ch = *p++;
		sputc(s, hex_digits[ch >> 4]);
		sputc(s, hex_digits[ch & 0xf]);
	   }
	pop(2);
	return 0;
}

/* <file> <string> readstring <substring> <filled_bool> */
int
zreadstring(register os_ptr op)
{	stream *s;
	uint len, rlen;
	check_read_file(s, op - 1);
	check_write_type(*op, t_string);
	len = r_size(op);
	rlen = sgets(s, op->value.bytes, len);
	r_set_size(op, rlen);
	op[-1] = *op;
	make_bool(op, (rlen == len ? 1 : 0));
	return 0;
}

/* <file> <string> writestring - */
int
zwritestring(register os_ptr op)
{	stream *s;
	int code;
	check_write_file(s, op - 1);
	code = write_string(op, s);
	if ( code >= 0 ) pop(2);
	return code;
}

/* <file> <string> readline <substring> <bool> */
int
zreadline(register os_ptr op)
{	stream *s;
	uint count;
	int code;
	check_read_file(s, op - 1);
	check_write_type(*op, t_string);
	code = zreadline_from(op->value.bytes, r_size(op), &count, s);
	if ( code < 0 ) return code;
	r_set_size(op, count);
	op[-1] = *op;
	make_bool(op, code);
	return 0;
}

/* Internal readline routine. */
/* Returns 1 if OK, 0 if end of file, or an error code. */
int
zreadline_from(byte *ptr, uint size, uint *pcount, stream *s)
{	uint count = 0;
	int ch;
	for ( ; ; count++ )
	   {
		/* Most systems define \n as 0xa and \r as 0xd; however, */
		/* OS-9 has \n == \r == 0xd and \l == 0xa.  The following */
		/* code works properly regardless of environment. */

		switch ( ch = sgetc(s) )
		   {
		case '\r':
#if '\n' == '\r'			/* OS-9 or similar */
#  define LF 0xa
			if ( s != gs_stream_stdin )
			{	ch = sgetc(s);
				if ( ch != LF && ch >= 0 )
					sputback(s);
                        }
			/* falls through */
		case LF:
#  undef LF
#else					/* '\n' != '\r' */
			ch = sgetc(s);
			if ( ch != '\n' && ch >= 0 ) sputback(s);
			/* falls through */
		case '\n':
#endif
			*pcount = count;
			return 1;
		case EOFC:
			*pcount = count;
			return 0;
		   }
		if ( count >= size )	/* filled the string */
		   {	sputback(s);
			return_error(e_rangecheck);
		   }
		*ptr++ = ch;
	   }
	/*return 0;*/		/* not reached */
}

/* token - this is called from zstring.c */
int
ztoken_file(register os_ptr op)
{	stream *s;
	int code;
	check_read_file(s, op);
	switch ( code = scan_token(s, 0, (ref *)op) )
	   {
	default:			/* possible error */
		if ( code < 0 ) return code;
					/* read a token */
		push(1);
		make_true(op);
		return 0;
	case scan_EOF:			/* no tokens */
		make_false(op);
		return 0;
	   }
}

/* <file> bytesavailable <int> */
int
zbytesavailable(register os_ptr op)
{	stream *s;
	long avail;
	check_read_file(s, op);
	if ( savailable(s, &avail) < 0 )
		return_error(e_ioerror);
	make_int(op, avail);
	return 0;
}

/* - flush - */
int
zflush(register os_ptr op)
{	sflush(gs_stream_stdout);
	return 0;
}

/* <file> flushfile - */
int
zflushfile(register os_ptr op)
{	stream *s;
	check_file(s, op);
	sflush(s);
	pop(1);
	return 0;
}

/* <file> resetfile - */
int
zresetfile(register os_ptr op)
{	NYI("resetfile");
	pop(1);
	return 0;
}

/* - currentfile <file> */
int
zcurrentfile(register os_ptr op)
{	es_ptr fp;
	push(1);
	/* Check the cache first */
	if ( esfile != 0 )
		ref_assign(op, esfile);
	else if ( (fp = zget_current_file()) == 0 )
	   {	/* Return an invalid file object. */
		/* This doesn't make a lot of sense to me, */
		/* but it's what the PostScript manual specifies. */
		make_file(op, 0, ~0, invalid_file_entry);
	   }
	else
	   {	ref_assign(op, fp);
		/* Load the cache */
		esfile = fp;
	   }
	/* Make the returned value literal. */
	r_clear_attrs(op, a_executable);
	return 0;
}

/* <string> print - */
int
zprint(register os_ptr op)
{	int code = write_string(op, gs_stream_stdout);
	if ( code >= 0 ) pop(1);
	return code;
}

/* <bool> echo - */
int
zecho(register os_ptr op)
{	check_type(*op, t_boolean);
	/****** NOT IMPLEMENTED YET ******/
	pop(1);
	return 0;
}

/* ------ Level 2 extensions ------ */

/* <file> fileposition <int> */
int
zfileposition(register os_ptr op)
{	stream *s;
	check_file(s, op);
	if ( !sseekable(s) )
		return_error(e_ioerror);
	make_int(op, stell(s));
	return 0;
}

/* <file> <int> setfileposition - */
int
zsetfileposition(register os_ptr op)
{	stream *s;
	check_file(s, op - 1);
	check_type(*op, t_integer);
	if ( sseek(s, op->value.intval) < 0 )
		return_error(e_ioerror);
	pop(2);
	return 0;
}

/* ------ Ghostscript extensions ------ */

/* <file> <int> unread - */
int
zunread(register os_ptr op)
{	stream *s;
	ulong ch;
	check_read_file(s, op - 1);
	check_type(*op, t_integer);
	ch = op->value.intval;
	if ( ch > 0xff )
		return_error(e_rangecheck);
	if ( sungetc(s, (byte)ch) < 0 )
		return_error(e_ioerror);
	pop(2);
	return 0;
}

/* <file> <device> writeppmfile - */
int
zwriteppmfile(register os_ptr op)
{	stream *s;
	int code;
	check_write_file(s, op - 1);
	check_type(*op, t_device);
	if ( !gs_device_is_memory(op->value.pdevice) )
		return_error(e_typecheck);
	sflush(s);
	code = gs_writeppmfile((gx_device_memory *)(op->value.pdevice), s->file);
	if ( code >= 0 ) pop(2);
	return code;
}

/* ------ Initialization procedure ------ */

op_def zfileio_op_defs[] = {
	{"1bytesavailable", zbytesavailable},
	{"0currentfile", zcurrentfile},
	{"1echo", zecho},
	{"1fileposition", zfileposition},
	{"0flush", zflush},
	{"1flushfile", zflushfile},
	{"1print", zprint},
	{"1read", zread},
	{"2readhexstring", zreadhexstring},
	{"2readline", zreadline},
	{"2readstring", zreadstring},
	{"1resetfile", zresetfile},
	{"2setfileposition", zsetfileposition},
	{"2unread", zunread},
	{"2write", zwrite},
	{"2writehexstring", zwritehexstring},
	{"2writeppmfile", zwriteppmfile},
	{"2writestring", zwritestring},
	op_def_end(0)
};

/* ------ Non-operator routines ------ */

/* Check a file for reading. */
/* The interpreter calls this to check an executable file. */
int
file_check_read(ref *op, stream **ps)
{	if ( !s_is_reading(*ps = fptr(op)) )
		return_error(e_invalidaccess);
	return 0;
}

/* Get the current file from which the interpreter is reading. */
private es_ptr
zget_current_file(void)
{	register es_ptr ep = esp;
	while ( ep >= esbot )
	{	if ( r_has_type_attrs(ep, t_file, a_executable) )
			return ep;
		ep--;
	}
	return (es_ptr)0;
}

/* When closing a file, check whether it is the current file, */
/* and if so, clear the cache. */
void
check_close_current_file(stream *s)
{	/* If we just closed the file from which the interpreter */
	/* is reading, zap it on the exec stack. */
	es_ptr cfp = zget_current_file();
	if ( cfp != 0 && fptr(cfp) == s )
	{	/* A null would confuse the estack parser.... */
		make_tasv(cfp, t_array, a_executable+a_execute, 0, refs, (ref *)0);
		esfile = 0;		/* clear currentfile cache */
	}
}

/* Switch a file open for read/write access but currently in write mode */
/* to read mode.  Return 1 if OK, 0 if not. */
int
file_switch_to_read(ref *op)
{	stream *s = fptr(op);
	uint modes = s->modes;
	long pos;
	if ( s->write_id != r_size(op) )	/* not valid */
		return 0;
	pos = stell(s);
	if ( sflush(s) < 0 )
		return 0;
	s->read_id = s->write_id;		/* enable reading */
	s->write_id = 0;			/* disable writing */
	sread_file(s, s->file, s->cbuf, s->cbsize);
	fseek(s->file, 0L, SEEK_CUR);		/* pacify C library */
	s->modes |= modes & s_mode_append;	/* don't lose append info */
	s->position = pos;
	return 1;
}

/* Switch a file open for read/write access but currently in read mode */
/* to write mode.  Return 1 if OK, 0 if not. */
int
file_switch_to_write(ref *op)
{	stream *s = fptr(op);
	uint modes = s->modes;
	long pos;
	if ( s->read_id != r_size(op) )		/* not valid */
		return 0;
	pos = stell(s);
	s->write_id = s->read_id;		/* enable writing */
	s->read_id = 0;				/* disable reading */
	fseek(s->file, pos, SEEK_SET);		/* pacify C library */
	if ( modes & s_mode_append )
		sappend_file(s, s->file, s->cbuf, s->cbsize);	/* sets position */
	else
	{	swrite_file(s, s->file, s->cbuf, s->cbsize);
		s->position = pos;
	}
	return 1;
}

/* ------ Internal routines ------ */

/* Write a string on a file.  The file has been checked for validity, */
/* but not the string. */
private int
write_string(os_ptr op, stream *s)
{	uint len;
	check_read_type(*op, t_string);
	len = r_size(op);
	if ( sputs(s, op->value.bytes, len) != len )
		return_error(e_ioerror);
	return 0;
}
