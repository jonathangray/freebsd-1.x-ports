/* arg_user.c - expand ~ and ~user shorthand */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_user_sccsid[] = "@(#)arg_user.c	1.10 18/9/92 (UKC)";

#include <ctype.h>
#include <sys/types.h>
#include <pwd.h>

#include <string.h>
#include <stdlib.h>
#include <local/ukcprog.h>

#include "arg.h"

static const char *do_twiddle PROTO((const char *s, int len));
static char *de_twiddle PROTO((const char *path, int twiddle_char));

/*  Interpret csh type ~ shorthand.
 *  s is a pointer to the string, which is len bytes long and not
 *  necessarily NUL terminated.
 *
 *  For a zero length string we return the value of the environment variable HOME.
 * 
 *  For a non zero length string, treat the string as a user name and
 *  look up the user's home directory in the password file.
 *  Return NULL and give a message on failure.
 */
static const char *
do_twiddle(s, len)
const char *s;
int len;
{
	static const char *home;
	struct passwd *pw;
	extern int errno;
	
	if (len == 0) {
		if (home == NULL && (home = getenv("HOME")) == NULL) {
			errf("can't expand ~ - $HOME not set");
			return NULL;
		}
		return home;
	}

	/*  ~user
	 */
	errno = 0;
	(void) setpwent();
	if (errno != 0) {
		errf("can't access password file (%m)");
		return NULL;
	}
	while ((pw = getpwent()) != NULL)
		if (strncmp(pw->pw_name, s, len) == 0 && strlen(pw->pw_name) == len)
			return pw->pw_dir;
	errf("unknown user %*s", len, s);
	return NULL;
}

static char *
de_twiddle(readonly_path, twiddle_char)
const char *readonly_path;
int twiddle_char;
{
	char *cptr, *path;

	path = strsave(readonly_path);
	if (twiddle_char == '~' || strchr(path, twiddle_char) == NULL)
		return path;
	for (cptr = path; *cptr != '\0'; cptr++)
		if (*cptr == twiddle_char)
			*cptr = '~';
	return path;
}

/*  Expand csh type ~ and ~user shorthand.
 *
 *  Return path if there is no ~ shorthand.
 *
 *  Return NULL and give an error message if there is an error (e.g. unknown user).
 *
 *  Return a pointer to the expanded path on success.  The returned
 *  string must be free()ed by the caller when it has finished with it.
 */
char *
arg_expand_twiddle(path, twiddle_char)
const char *path;
int twiddle_char;
{
	const char *twidex, *cptr;
	char *buf, *res;
	
	if (*path != twiddle_char)
		return de_twiddle(path, twiddle_char);
	for (cptr = path + 1; *cptr != '/' && *cptr != '\0'; cptr++) {
		if (!isalnum(*cptr) && *cptr != '_')
			return de_twiddle(path, twiddle_char);
	}
	if ((twidex = do_twiddle(path+1, cptr - (path + 1))) == NULL)
		return NULL;
	buf = e_malloc(strlen(twidex) + strlen(cptr) + 1);
	(void) strcpy(buf, twidex);
	(void) strcat(buf, cptr);
	res = de_twiddle(buf, twiddle_char);
	free(buf);
	return res;
}
