/* errf.c -- formatted error messages */

char ukcprog_errf_sccsid[] = "@(#)errf.c	1.10 26/4/92 UKC";


#include <stdio.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <ukcstdlib.h>
#include <ukcstring.h>

#include "ukcprog.h"

static const char *munge_progname PROTO((const char *));


static errf_ofunc_t User_errf_ofunc = NULL;
static const char *Output_prefix = "";
static char *Progname = NULL;


errf_ofunc_t
errf_set_ofunc(func)
errf_ofunc_t func;
{
	errf_ofunc_t old;

	old = User_errf_ofunc;
	User_errf_ofunc = func;

	return old;
}


/*  errf_set_prefix()
 *  Sets the string that errf() prefixes to messages.
 *  Returns the old one.
 */
const char *
errf_set_prefix(prefix)
const char *prefix;
{
	const char *old;

	old = Output_prefix;
	Output_prefix = prefix;

	return old;
}


/*  errf_get_prefix()
 *  Return the current prefix.
 */
const char *
errf_get_prefix()
{
	return Output_prefix;
}


#if defined(unix) || defined(__unix__)
static const char *
munge_progname(progname)
const char *progname;
{
	const char *name;
	
	if ((name = strrchr(progname, '/')) == NULL)
		name = progname;

	else
		++name;

	if (Progname != NULL)
		free(Progname);

	Progname = strsave(name);

	return "%s: ";
}
#endif /* unix */


#ifdef VMS
static const char *
munge_progname(progname)
const char *progname;
{
	const char *name, *dot;
	char  *s;
	int i;

	if ((name = strrchr(progname, ']')) == NULL)
		name = progname;
	else
		++name;

	if ((dot = strchr(name, '.')) == NULL)
		dot = name + strlen(name);

	s = strf("%.*s, ", dot - name, name);

	for (i = 0; i < dot - name; ++i)
		s[i] = toupper(s[i]);

	if (Progname != NULL)
		free(Progname);

	Progname = s;

	return "%%%s, ";
}
#endif /* VMS */


#ifdef MSDOS
static const char *
munge_progname(progname)
const char *progname;
{
	const char *name;
	
	if ((name = strrchr(progname, '\\')) == NULL)
		name = progname;

	else
		++name;

	if (Progname != NULL)
		free(Progname);

	Progname = strsave(name);

	return "%s: ";
}
#endif /* MSDOS */


/*  errf_set_progname()
 *  Convenience routine to set the errf prefix to include the progname.
 */
void
errf_set_progname(name)
const char *name;
{
	const char *fmt;

	fmt = munge_progname(name);
	errf_set_prefix(strf(fmt, Progname));
}


/*  errf_get_progname()
 */
const char *
errf_get_progname()
{
	return Progname;
}


#ifdef __STDC__
void
errf(const char *fmt, ...)
{

#else /* !__STDC__ */
void
errf(va_alist)
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

	if (User_errf_ofunc != NULL)
		(*User_errf_ofunc)(s);
	else {
		/* default output functionality */
		fprintf(stderr, "%s%s\n", Output_prefix, s);
		fflush(stderr);
	}

	if (s != buffer)	/* must have been obtained from malloc */
		free(s);
}


/*  errf_usage()
 *  Output a usage string prefixed by the program name and exit(1).
 */
void
errf_usage(usage)
const char *usage;
{
	if (User_errf_ofunc != NULL)
		(*User_errf_ofunc)(strf("Usage: %s %s", Progname, usage));
	else {
		/* default output functionality */
		fprintf(stderr, "Usage: %s %s\n", Progname, usage);
	}

	exit(1);
}
