/* $Id: error.c,v 1.1 1994/02/23 14:40:05 jkh Exp $
 *
 * Adapted from 'The UNIX Programming Environment' by Kernighan & Pike
 * and an example from the manualpage for vprintf by
 * Gaute Nessan, University of Tromsoe (gaute@staff.cs.uit.no).
 *
 * Modified by Bjoern Stabell (bjoerns@staff.cs.uit.no).
 */

#include "error.h"

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: error.c,v 1.1 1994/02/23 14:40:05 jkh Exp $";
#endif

#include <stdlib.h>
#include <string.h>



/*
 * This file defines two entry points:
 *
 * init_error()		- Initialize the error routine, accepts program name
 *			  as input.
 * error()		- perror() with printf functionality.
 */



/*
 * Global data.
 */
#define	MAX_PROG_LENGTH	256
static char		progname[MAX_PROG_LENGTH];



/*
 * Functions.
 */
void init_error(char *prog)
{
#ifdef VMS
    char *p = strrchr(prog, ']');
#else
    char *p = strrchr(prog, '/');
#endif

    strncpy(progname, p ? p+1 : prog, MAX_PROG_LENGTH);   /* Beautify arv[0] */
}



#if defined(__STDC__) && !defined(__sun__) || defined(__cplusplus)

/*
 * Ok, let's do it the ANSI C way.
 */
void error(char *fmt, ...)
{
    va_list	 ap;			/* Argument pointer */
    int		 e = errno;		/* Store errno */
#ifdef VMS
    if (e == EVMSERR)
	e = 0/*__gnu_vaxc_errno__*/;
#endif

    va_start(ap, fmt);

    if (progname[0] != '\0')
	fprintf(stderr, "%s: ", progname);

    vfprintf(stderr, fmt, ap);

    if (e != 0)
	fprintf(stderr, " (%s)", strerror(e));

    fprintf(stderr, "\n");

    va_end(ap);
}

#else

/*
 * Hm, we'd better stick to the K&R way.
 */
void
    error(va_alist)
va_dcl		/* Note that the format argument cannot be separately	*
		 * declared because of the definition of varargs.	*/
{
    va_list	 args;
    int		 e = errno;		/* Store errno */
    extern int	 sys_nerr;
    extern char *sys_errlist[];
    char	*fmt;


    va_start(args);

    if (progname[0] != '\0')
	fprintf(stderr, "%s: ", progname);

    fmt = va_arg(args, char *);
    (void) vfprintf(stderr, fmt, args);

    if (e > 0 && e < sys_nerr)
	fprintf(stderr, " (%s)", sys_errlist[e]);

    fprintf(stderr, "\n");

    va_end(args);
}

#endif
