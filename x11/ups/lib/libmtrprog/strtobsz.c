/* string_to_bufsize.c - parse 10k, 126b etc type buffer size string */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_strings_to_bufsize_sccsid[] = "@(#)strtobsz.c	1.6 26/4/92 (UKC)";

#include <ctype.h>
#include <stdlib.h>
#include <local/ukcprog.h>

#include "utils.h"

int
string_to_bufsize(numstr, p_res)
const char *numstr;
long *p_res;
{
	if (string_to_offset(numstr, p_res) != 0)
		return -1;

	if (*p_res == 0) {
		errf("Illegal zero block size %s", numstr);
		return -1;
	}

	return 0;
}

int
string_to_offset(numstr, p_res)
const char *numstr;
long *p_res;
{
	const char *s;
	long bufsize, scale;

	scale = 1;
	s = numstr;
	bufsize = 0;

	if (*s == '\0') {
		errf("Zero length block size string");
		return -1;
	}

	for (;;) {
		char *ends;
		long n;
		bool negative;

		if (*s == '-') {
			if (!isdigit(s[1]))
				break;
			++s;
			negative = TRUE;
		}
		else
			negative = FALSE;

		n = strtol(s, &ends, 0);
		if (ends == NULL)
			break;
		s = ends;
		if (negative)
			n = -n;
			

		if (*s == 'x') {
			scale *= n;
			++s;
		}
		else {
			bufsize = scale * n;
			break;
		}
	}

	switch (*s++) {
	case 'b':
	case 'B':
		bufsize *= 512;
		break;
	case 'k':
	case 'K':
		bufsize *= 1024;
		break;
	case 'm':
	case 'M':
		bufsize *= 1024 * 1024;
		break;
	case '\0':
	case '+':
	case '-':
		--s;
		break;
	default:
		errf("Illegal size suffix '%c' (use b, k or m)", *s);
		return -1;
	}

	if (*s == '+' || *s == '-') {
		long offset;

		if (string_to_bufsize(s + 1, &offset) != 0)
			return -1;
		bufsize += (*s == '+') ? offset : -offset;
	}
	else if (*s != '\0') {
		errf("Extra characters (\"%s\") after block size %.*s",
							s, s - numstr, numstr);
		return -1;
	}

		
	if (bufsize < 0) {
		errf("Block size %s is not a positive integer", numstr);
		return -1;
	}

	*p_res = bufsize;
	return 0;
}
