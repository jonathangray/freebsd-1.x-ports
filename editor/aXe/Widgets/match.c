/*
 * match.c is taken from Release 1.4 of Byron Rakitzis' rc command interpreter
 * based on the AT & T Plan 9 shell of the same name. It has only been modified
 * to the extent of reverting to old C, defining some things locally that are
 * defined in rc.h in the rc distribution and extending to accept ! and ^ as
 * additional complementing characters in range checking.
 * rc's COPYRIGHT notice follows.
 */

/*
 * Copyright 1991 Byron Rakitzis.  All rights reserved.
 *
 * This software is not subject to any license of the American Telephone
 * and Telegraph Company or of the Regents of the University of California.
 *
 * Permission is granted to anyone to use this software for any purpose on
 * any computer system, and to alter it and redistribute it freely, subject
 * to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 *    software, no matter how awful, even if they arise from flaws in it.
 *
 * 2. The origin of this software must not be misrepresented, either by
 *    explicit claim or by omission.  Since few users ever read sources,
 *    credits must appear in the documentation.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.  Since few users
 *    ever read sources, credits must appear in the documentation.
 *
 * 4. This notice may not be removed or altered.
 *
 *    [this copyright notice is adapted from Henry Spencer's
 *    "awf" copyright notice.]
 */

/* match.c: pattern matching routines */


static int rangematch();

enum { RANGE_FAIL = -1, RANGE_ERROR = -2 };

typedef enum bool {
        FALSE, TRUE
} bool;

#define streq(x, y) (*(x) == *(y) && strcmp(x, y) == 0)

/* match() matches a single pattern against a single string. */

bool match(p, m, s)
char *p, *m, *s;
{
	int i, j;
	if (m == 0)
		return streq(p, s);
	i = 0;
	while (1) {
		if (p[i] == '\0')
			return *s == '\0';
		else if (m[i]) {
			switch (p[i++]) {
			case '?':
				if (*s++ == '\0')
					return FALSE;
				break;
			case '*':
				while (p[i] == '*' && m[i] == 1)        /* collapse multiple stars */
					i++;
				if (p[i] == '\0') 	/* star at end of pattern? */
					return TRUE;
				while (*s != '\0')
					if (match(p + i, m + i, s++))
						return TRUE;
				return FALSE;
			case '[':
				if (*s == '\0')
					return FALSE;
				switch (j = rangematch(p + i, *s)) {
				default:
					i += j;
					break;
				case RANGE_FAIL:
					return FALSE;
				case RANGE_ERROR:
					if (*s != '[')
						return FALSE;
				}
				s++;
				break;
			default:
			/*	fprintf(stderr,"bad metacharacter in match\n");  */
				/* NOTREACHED */
				return FALSE;
			}
		}  else if (p[i++] != *s++)
			return FALSE;
	}
}

/*
   From the ed(1) man pages (on ranges):

	The `-' is treated as an ordinary character if it occurs first
	(or first after an initial ^) or last in the string.

	The right square bracket does not terminate the enclosed string
	if it is the first character (after an initial `^', if any), in
	the bracketed string.

   rangematch() matches a single character against a class, and returns
   an integer offset to the end of the range on success, or -1 on
   failure.
*/

static int rangematch(p,c) 
char *p, c;
{
	char *orig = p;
	bool neg = (*p == '~' || *p == '!' || *p == '^');
	bool matched = FALSE;
	if (neg)
		p++;
	if (*p == ']') {
		p++;
		matched = (c == ']');
	}
	for (; *p != ']'; p++) {
		if (*p == '\0')
			return RANGE_ERROR;	/* bad syntax */
		if (p[1] == '-' && p[2] != ']') { /* check for [..-..] but ignore [..-] */
			if (c >= *p)
				matched |= (c <= p[2]);
			p += 2;
		} else {
			matched |= (*p == c);
		}
	}
	if (matched ^ neg)
		return p - orig + 1; /* skip the right-bracket */
	else
		return RANGE_FAIL;
}
