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

/* gsutil.h */
/* Prototypes for procedures in gsutil.c */

/* Note: gsutil.c also implements props_extract, declared in gsprops.h. */

/* ------ String utilities ------ */

/* Compare two strings, returning -1 if the first is less, */
/* 0 if they are equal, and 1 if first is greater. */
/* We can't use memcmp, because we always use unsigned characters. */
extern int bytes_compare(P4(const byte *, uint, const byte *, uint));

/* Test whether a string matches a pattern with wildcards. */
/* If psmp == NULL, use standard parameters: '*' = any substring, */
/* '?' = any character, '\\' quotes next character, don't ignore case. */
typedef struct string_match_params_s {
	int any_substring;		/* '*' */
	int any_char;			/* '?' */
	int quote_next;			/* '\\' */
	int ignore_case;
} string_match_params;
extern int string_match(P5(const byte *str, uint len,
  const byte *pstr, uint plen, const string_match_params *psmp));

/* Compute a hash for a string */
extern uint string_hash(P2(const byte *, uint));
