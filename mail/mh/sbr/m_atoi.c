/* m_atoi.c - parse a string representation of a message number */
#ifndef	lint
static char ident[] = "@(#)m_atoi.c,v 1.1.1.1 1993/01/30 04:41:25 jtc Exp";
#endif /* lint */

#include "../h/mh.h"


m_atoi (str)
register char *str;
{
    register int    i;
    register char  *cp;

    i = 0;
    cp = str;
#ifdef LOCALE
    while (isdigit(*cp)) {
	i *= 10;
	i += *cp++ - '0';
    }
#else
    while (*cp) {
	if (*cp < '0' || *cp > '9')
	    return 0;
	i *= 10;
	i += *cp++ - '0';
    }
#endif

    return i;
}
