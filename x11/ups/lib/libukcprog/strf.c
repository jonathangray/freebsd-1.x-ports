/* strf.c -- formatted strings, storage allocated from malloc */

char ukcprog_strf_sccsid[] = "@(#)strf.c	1.5 27/3/92 UKC";


#include <stdio.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <ukcstdlib.h>
#include "ukcprog.h"


#ifdef __STDC__
char *
strf(const char *fmt, ...)
{

#else /* !__STDC__ */
char *
strf(va_alist)
va_dcl
{
	char *fmt;
#endif /* !__STDC__ */
	va_list args;
	char buffer[100];
	char *s;

#ifdef __STDC__
	va_start(args, fmt);
#else
	va_start(args);
	fmt = va_arg(args, char *);
#endif

	s = formf(buffer, sizeof(buffer), fmt, args);

	va_end(args);

	if (s == buffer)	/* not obtained from malloc */
		s = strsave(s);

	return s;
}
