/* ssplit.c - split a string into a vector of arguments */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ukcprog_ssplit_sccsid[] = "@(#)ssplit.c	1.5 25/6/92 UKC";

#include <ukcstring.h>
#include <ukcstdlib.h>

#include "ukcprog.h"

/*  Split a string into a NULL terminated vector of words.
 *  The original string is not modified.
 *
 *  Words in the given string are delimited by any character in
 *  the delimiters argument.
 *
 *  By default leading delimiters are ignored and multiple adjacent
 *  delimiters are treated as a single delimiter.  If the delimiters
 *  string starts with a NUL then each occurence of a delimiter
 *  indicates a field and zero length fields are possible.  Thus to
 *  split a password file line, use ssplit(line, "\0:").
 *
 *  The returned vector is malloc'ed storage - when it is finished with
 *  it should be passed to free() to release its storage.
 */
char **
ssplit(line, delimiters)
const char *line, *delimiters;
{
	int ncomp;
	bool want_null_fields;
	const char *cp;
	char **vecp, *buf;
	char *mem;

	if (want_null_fields = *delimiters == '\0')
		++delimiters;

	ncomp = 1;
	if (!want_null_fields) {
		while (*line != '\0' && strchr(delimiters, *line) != NULL)
			++line;
	}
	for (cp = line; *cp != '\0'; cp++)
		if (strchr(delimiters, *cp) != NULL)
			++ncomp;

	/*  We need ncomp+1 char* sized slots for the string pointers
	 *  and terminating NULL, plus space for a copy of the string
	 *  including the terminating NUL.
	 */
	mem = e_malloc((ncomp + 1) * sizeof(char *) + strlen(line) + 1);

	vecp = (char **)mem;
	buf = mem + (ncomp + 1) * sizeof(char *);
	strcpy(buf, line);

	for (;;) {
		if (!want_null_fields) {
			while (*buf != '\0' &&
					strchr(delimiters, *buf) != NULL)
				++buf;
			if (*buf == '\0')
				break;
		}

		*vecp++ = buf;

		if (*buf == '\0')
			break;

		while (*buf != '\0' && strchr(delimiters, *buf) == NULL)
			buf++;

		if (*buf == '\0')
			break;
		*buf++ = '\0';
	}
	*vecp = NULL;

	return (char **)mem;
}
