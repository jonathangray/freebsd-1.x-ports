/* arg_quote.c - process quoting with ", ' and \ in an argument */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_quote_sccsid[] = "@(#)arg_quote.c	1.10 12/9/92 (UKC)";

#include <ctype.h>
#include <string.h>
#include <local/ukcprog.h>

#include "arg.h"

static int compile_seq PROTO((const char **p_s, char **p_dst));
static int translate_globchar PROTO((const char **p_src, char **p_dst,
							int *p_brace_level));

static int
compile_seq(p_s, p_dst)
const char **p_s;
char **p_dst;
{
	char *buf, *cptr;
	const char *s;
	int not, i;

	buf = *p_dst;
	s = *p_s;
	for (cptr = buf + SEQMAPSIZE; cptr > buf; )
		*--cptr = 0x80;
	if (not = *s == '^')	/* note '=' */
		s++;
	while (*s != ']' && *s != '\0') {
		if (*s == '-' && s[1] != ']') {
			for (i = s[-1] + 1; i <= s[1]; i++)
				SETSEQ(buf, i);
			s += 2;
		}
		else if (*s == '\\' && s[1] != '\0') {
			SETSEQ(buf, s[1]);
			s += 2;
		}
		else {
			SETSEQ(buf, *s);
			s++;
		}
	}
	if (*s++ != ']') {
		errf("unmatched [");
		return -1;
	}
	if (not)
		for (cptr = buf; cptr < buf + SEQMAPSIZE; cptr++)
			*cptr ^= 0x7f;
		
	*p_dst = buf + SEQMAPSIZE;
	*p_s = s;
	return 0;
}

/*  Maximum number of characters that translate_globchar() will produce,
 *  no including the terminating NUL.
 */
#define MAX_TG_LEN	(SEQMAPSIZE + 1)

static int
translate_globchar(p_src, p_dst, p_brace_level)
const char **p_src;
char **p_dst;
int *p_brace_level;
{
	const char *src;
	char *dst;

	src = *p_src;
	dst = *p_dst;
	switch(*src++) {
	case '[':
		*dst++ = PAT_SEQ;
		if (src[1] == ']') {
			errf("empty [] pair");
			return -1;
		}
		if (compile_seq(&src, &dst) != 0)
			return -1;
		break;
	case ']':
		*dst++ = ']';
		break;
	case ',':
		*dst++ = (*p_brace_level > 0) ? PAT_COMMA : ',';
		break;
	case '{':
		*dst++ = PAT_OPENBR;
		++*p_brace_level;
		break;
	case '}':
		*dst++ = PAT_CLOSEBR;
		if (--*p_brace_level < 0) { 
			errf("unmatched }");
			return -1;
		}
		break;
	case '?':
		*dst++ = PAT_QUERY;
		break;
	case '*':
		*dst++ = PAT_STAR;
		break;
	case '~':
		*dst++ = PAT_TWIDDLE;
		break;
	default:
		panic("unknown glob char in tg");
	}
	*p_src = src;
	*p_dst = dst;
	return 0;
}

/*  Convert the string *p_s, with "", '' and \ quoting to a string
 *  without these quotes and with unquoted metacharacters converted
 *  to internal format.
 *
 *  The glob_chars string gives the metacharacters that will be converted.
 *  The full set is "[]~{,}*?".  It is an error to have any characters
 *  other than these in the string.
 */
char *
arg_do_quoting(p_s, glob_chars)
const char **p_s;
const char *glob_chars;
{
	static char *obuf;
	static int obuf_size = 0;
	static char special_chars[] = ";&()|^<>\n \t";
	static char illegal_chars[] = ";&()|^";
	const char *iptr, *olim;
	char *optr;
	bool just_had_backslash;
	int res, quotec, pos, brace_level;

	if (obuf_size == 0) {
		obuf_size = 64;
		obuf = e_malloc(obuf_size);
	}
	olim = obuf + obuf_size - (MAX_TG_LEN + 1);
	quotec = '\0';
	just_had_backslash = FALSE;
	optr = obuf;
	brace_level = 0;
	iptr = *p_s;
	res = 0;
	while (*iptr != '\0') {
		if (optr == olim) {
			pos = optr - obuf;
			obuf_size *= 2;
			obuf = e_realloc(obuf, obuf_size);
			optr = obuf + pos;
			olim = obuf + obuf_size - (MAX_TG_LEN + 1);
		}
		if (just_had_backslash) {
			*optr++ = *iptr++;
			just_had_backslash = FALSE;
		}
		else if (*iptr == '\\') {
			just_had_backslash = TRUE;
			iptr++;
		}
		else if (*iptr == quotec) {
			quotec = '\0';
			iptr++;
		}
		else if (quotec == '\0' && (*iptr == '\'' || *iptr == '"'))
			quotec = *iptr++;
		else if (quotec == '\0' && strchr(special_chars, *iptr) != 0)
			break;
		else if (quotec == '\0' && strchr(glob_chars, *iptr) != 0) {
			if (translate_globchar(&iptr, &optr, &brace_level) != 0) {
				res = -1;
				break;
			}
		}
		else
			*optr++ = *iptr++;
	}
	*optr++ = '\0';

	if (res != 0)
		return NULL;

	if (brace_level > 0)
		errf("unmatched {");
	else if (quotec != '\0')
		errf("unmatched '%c'", quotec);
	else if (just_had_backslash)
		errf("missing character following '\\'");
	else if (*iptr != '\0' && strchr(illegal_chars, *iptr) != 0)
		errf("'%c' illegal in command line", *iptr);
	else {
		*p_s = iptr;
		return obuf;
	}
	return NULL;
}
