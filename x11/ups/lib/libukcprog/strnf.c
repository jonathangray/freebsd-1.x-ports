/* strnf.c -- like sprintf, but with a buffer size argument */

char ukcprog_strnf_sccsid[] = "@(#)strnf.c	1.1 23/6/92 (UKC)";

#include <stdio.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <ukcstdlib.h>
#include "ukcprog.h"


#ifdef __STDC__
void
strnf(char *buf, int bufsize, const char *fmt, ...)
{

#else /* !__STDC__ */
void
strnf(va_alist)
va_dcl
{
	char *buf;
	int bufsize;
	char *fmt;
#endif /* !__STDC__ */
	va_list args;
	char *s;

#ifdef __STDC__
	va_start(args, fmt);
#else
	va_start(args);
	buf = va_arg(args, char *);
	bufsize = va_arg(args, int);
	fmt = va_arg(args, char *);
#endif

	s = formf(buf, bufsize, fmt, args);

	va_end(args);

	/*  If formf had to allocate a buffer then the supplied buf
	 *  was too small.  Copy what will fit and free the formf buffer.
	 */
	if (s != buf) {
		memcpy(buf, s, bufsize - 1);
		buf[bufsize - 1] = '\0';
		free(s);
	}
}
