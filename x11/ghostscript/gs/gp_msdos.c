/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gp_msdos.c */
/* Common platform-specific routines for MS-DOS (any compiler) */
#include "stdio_.h"
#include <fcntl.h>
#include "dos_.h"
#include "memory_.h"
#include "string_.h"
#include "gstypes.h"
#include "gp.h"
#include "gsutil.h"

/* ------ Date and time ------ */

/* Read the current date (in days since Jan. 1, 1980) */
/* and time (in milliseconds since midnight). */
void
gp_get_clock(long *pdt)
{	union REGS osdate, ostime;
	long idate;
	static int mstart[12] =
	   { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
	osdate.h.ah = 0x2a;		/* get date */
	intdos(&osdate, &osdate);
#define da_year rshort.cx
#define da_mon h.dh
#define da_day h.dl
	ostime.h.ah = 0x2c;		/* get time */
	intdos(&ostime, &ostime);
#define ti_hour h.ch
#define ti_min h.cl
#define ti_sec h.dh
#define ti_hund h.dl
	idate = (long)osdate.da_year * 365 +
		(osdate.da_year / 4 + 1 +	/* account for leap years */
		 mstart[osdate.da_mon - 1] +	/* month is 1-origin */
		 osdate.da_day - 1);		/* day of month is 1-origin */
	if ( osdate.da_mon <= 2 && osdate.da_year % 4 == 0 )		/* Jan. or Feb. of leap year */
		idate--;
	pdt[0] = idate;
	pdt[1] =
		(ostime.ti_hour * 60 + ostime.ti_min) * 60000L +
		(ostime.ti_sec * 100 + ostime.ti_hund) * 10L;
}

/* ------ Console management ------ */

/* Answer whether a given file is the console (input or output). */
/* This is not a standard gp procedure, */
/* but the MS Windows configuration needs it, */
/* and other MS-DOS configurations might need it someday. */
int
gp_file_is_console(FILE *f)
{	union REGS regs;
	if ( f == NULL )
		return 0;
	regs.h.ah = 0x44;	/* ioctl */
	regs.h.al = 0;		/* get device info */
	regs.rshort.bx = fileno(f);
	intdos(&regs, &regs);
	return ((regs.h.dl & 0x80) != 0 && (regs.h.dl & 3) != 0);
}

/* ------ Printer accessing ------ */

/* Put a printer file (which might be stdout) into binary or text mode. */
/* This is not a standard gp procedure, */
/* but all MS-DOS configurations need it. */
void
gp_set_printer_binary(int prnfno, int binary)
{	union REGS regs;
	regs.h.ah = 0x44;	/* ioctl */
	regs.h.al = 0;		/* get device info */
	regs.rshort.bx = prnfno;
	intdos(&regs, &regs);
	if ( binary )
		regs.h.dl |= 0x20;	/* binary (no ^Z intervention) */
	else
		regs.h.dl &= ~0x20;	/* text */
	regs.h.dh = 0;
	regs.h.ah = 0x44;	/* ioctl */
	regs.h.al = 1;		/* set device info */
	intdos(&regs, &regs);
}

/* ------ File names ------ */

/* Define the character used for separating file names in a list. */
const char gp_file_name_list_separator = ';';

/* Define the default scratch file name prefix. */
const char gp_scratch_file_name_prefix[] = "_temp_";

/* Define the string to be concatenated with the file mode */
/* for opening files without end-of-line conversion. */
const char gp_fmode_binary_suffix[] = "b";
/* Define the file modes for binary reading or writing. */
const char gp_fmode_rb[] = "rb";
const char gp_fmode_wb[] = "wb";

/* Answer whether a file name contains a directory/device specification, */
/* i.e. is absolute (not directory- or device-relative). */
int
gp_file_name_is_absolute(const char *fname, uint len)
{	/* A file name is absolute if it contains a drive specification */
	/* (second character is a :) or if it start with / or \. */
	return ( len >= 1 && (*fname == '/' || *fname == '\\' ||
		(len >= 2 && fname[1] == ':')) );
}

/* Answer the string to be used for combining a directory/device prefix */
/* with a base file name.  The file name is known to not be absolute. */
const char *
gp_file_name_concat_string(const char *prefix, uint plen,
  const char *fname, uint len)
{	if ( plen > 0 )
	  switch ( prefix[plen - 1] )
	   {	case ':': case '/': case '\\': return "";
	   };
	return "\\";
}

/* ------ File enumeration ------ */

struct file_enum_s {
	ff_struct_t ffblk;
	char *pattern;			/* orig pattern + modified pattern */
	int patlen;			/* orig pattern length */
	int pat_size;			/* allocate space for pattern */
	int head_size;			/* pattern length through last */
					/* :, / or \ */
	int first_time;
	const gs_memory_procs *mprocs;
};

/* Initialize an enumeration.  Note that * and ? in a directory */
/* don't work, and \ is taken literally unless a second \ follows. */
file_enum *
gp_enumerate_files_init(const char *pat, uint patlen,
  const gs_memory_procs *mprocs)
{	file_enum *pfen = (file_enum *)(*mprocs->alloc)(1, sizeof(file_enum), "gp_enumerate_files");
	int pat_size = 2 * patlen + 1;
	char *pattern;
	char *p;
	int hsize = 0;
	int i;
	int dot = 0;
	if ( pfen == 0 )
		return 0;
	pattern = (*mprocs->alloc)(pat_size, 1,
				   "gp_enumerate_files(pattern)");
	if ( pattern == 0 )
		return 0;
	memcpy(pattern, pat, patlen);
	p = pattern + patlen;
	for ( i = 0; i < patlen; i++ )
	{	switch ( pat[i] )
		{
		case '*':
			/* Skip to . or end of string so DOS can do it. */
			*p++ = '*';
			while ( i < patlen && pat[i] != '.' ) i++;
			if ( i == patlen && !dot )
			{	/* DOS doesn't interpret * alone as */
				/* matching all files; we need *.*. */
				*p++ = '.';
				*p++ = '*';
			}
			i--;
			continue;
		case '.':
			dot = 1;
			break;
		case '\\':
			if ( i + 1 < patlen && pat[i + 1] == '\\' )
				i++;
			/* falls through */
		case ':':
		case '/':
			hsize = p + 1 - (pattern + patlen);
			dot = 0;
		}
		*p++ = pat[i];
	}
	*p = 0;
	pfen->pattern = pattern;
	pfen->patlen = patlen;
	pfen->pat_size = pat_size;
	pfen->head_size = hsize;
	pfen->mprocs = mprocs;
	pfen->first_time = 1;
	return pfen;
}

/* Enumerate the next file. */
private const string_match_params smp_file = { '*', '?', -1, 1 };
uint
gp_enumerate_files_next(file_enum *pfen, char *ptr, uint maxlen)
{	int code;
	char *p, *q;
	uint len;
	const char *fpat = pfen->pattern + pfen->patlen;
top:	if ( pfen->first_time )
	   {	code = dos_findfirst(fpat, &pfen->ffblk);
		pfen->first_time = 0;
	   }
	else
		code = dos_findnext(&pfen->ffblk);
	if ( code != 0 )
	   {	/* All done, clean up. */
		gp_enumerate_files_close(pfen);
		return ~(uint)0;
	   }
	if ( maxlen < 13 + pfen->head_size ) return maxlen + 1;	/* cop out! */
	memcpy(ptr, fpat, pfen->head_size);
	for ( p = &pfen->ffblk.ff_name[0], q = ptr + pfen->head_size; *p; p++ )
		if ( *p != ' ' ) *q++ = *p;
	len = q - ptr;
	/* Make sure this file really matches the pattern. */
	if ( !string_match(ptr, len, pfen->pattern, pfen->patlen, &smp_file) )
		goto top;
	return len;
}

/* Clean up the file enumeration. */
void
gp_enumerate_files_close(file_enum *pfen)
{	const gs_memory_procs *mprocs = pfen->mprocs;
	(*mprocs->free)(pfen->pattern, pfen->pat_size, 1,
		 "gp_enumerate_files_close(pattern)");
	(*mprocs->free)((char *)pfen, 1, sizeof(file_enum), "gp_enumerate_files_close");
}
