/* arg_match.c - match a single pathname component against a pattern */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_match_sccsid[] = "@(#)arg_match.c	1.9 26/4/92 (UKC)";


#include <local/ukcprog.h>

#include "arg.h"

static int rmatch PROTO((const char **p_name, const char **p_pattern));

/*  Recursive regular expression match algorithm.
 *
 *  Recognises the metacharacters '*', '[' and ']' and '?' which have
 *  the same meanings as in csh(1).
 *
 *  It expects the metacharacters to have been translated to PAT_STAR,
 *  PAT_SEQ etc - see arg_quote().
 *
 *  We return TRUE for a match, FALSE for no match.
 *  If (and only if) we get a match, we set *p_name and *p_pattern to
 *  the new values of name and pattern (i.e. move them over the part
 *  we have just matched).
 *
 *  This is called by arg_match() only.
 */
static int
rmatch(p_name, p_pattern)
const char **p_name, **p_pattern;
{
	const char *cptr, *name, *pattern;
	
	name = *p_name;
	pattern = *p_pattern;
	while (*pattern != '/' && *pattern != '\0') {
		switch(*pattern) {
		case PAT_SEQ:
			if (*name == '/' || *name == '\0')
				return FALSE;
			if (!TSTSEQ(pattern + 1, *name))
				return FALSE;
			pattern += SEQMAPSIZE + 1;
			name++;
			break;
		case PAT_QUERY:
			if (*name == '\0' || *name == '/')
				return FALSE;
			pattern++;
			name++;
			break;
		case PAT_STAR:
			pattern++;
			for (cptr = name; *cptr != '\0' && *cptr != '/'; cptr++)
				;
			while (!rmatch(&cptr, &pattern))
				if (--cptr < name)
					return FALSE;
			name = cptr;
			break;
		default:
			if (*name++ != *pattern++)
				return FALSE;
			break;
		}
	}
	if (*name != '/' && *name != '\0')
		return FALSE;
	*p_name = name;
	*p_pattern = pattern;
	return TRUE;
}

/*  Return TRUE if name matches pattern. Pattern is a csh(1) filename
 *  pattern, that has been passed through arg_quote() above.
 *  We deal with files starting with '.' (must be matched explicitly)
 *  and return TRUE immediately of the pattern is "*", otherwise
 *  call rmatch().
 */
int
arg_match(name, pattern)
const char *name, *pattern;
{
	if (*name == '.') {
		if (*pattern == '.') {
			name++;
			pattern++;
		}
		else
			return FALSE;
	}
	if (*pattern == PAT_STAR && (pattern[1] == '/' || pattern[1] == '\0'))
		return TRUE;
	return rmatch(&name, &pattern);
}
