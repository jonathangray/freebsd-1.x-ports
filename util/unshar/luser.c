/*
**  Get user name.  Something for everyone.
*/
/* LINTLIBRARY */
#include "shar.h"
#ifdef	USE_GETPWUID
#include <pwd.h>
#endif	/* USE_GETPWUID */
#ifdef	RCSID
static char RCS[] =
	"$Header: /a/cvs/386BSD/ports/util/unshar/luser.c,v 1.1 1993/09/04 16:45:34 jkh Exp $";
#endif	/* RCSID */


/*
**  Get user name.  Not secure, but who sends nastygrams as shell archives?
*/
char *
User()
{
#ifdef	USE_GETPWUID
#ifndef __386BSD__
    extern struct passwd	*getpwuid();
#endif
    struct passwd		*p;
#endif	/* USE_GETPWUID */
    char			*g;

    if (g = getenv("USER"))
	return(g);
    if (g = getenv("LOGNAME"))
	return(g);
    if (g = getenv("NAME"))
	return(g);
#ifdef	USE_GETPWUID
    if (p = getpwuid(getuid()))
	return(p->pw_name);
#endif	/* USE_GETPWUID */
    return(USERNAME);
}
