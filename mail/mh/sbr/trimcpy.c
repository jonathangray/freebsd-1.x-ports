/* trimcpy.c - strip [lt]wsp and replace newlines with spaces */
#ifndef       lint
static char ident[] = "@(#)trimcpy.c,v 1.1.1.1 1993/01/30 04:41:30 jtc Exp";
#endif	/*        lint */


#include "../h/mh.h"
#include <ctype.h>
#include <stdio.h>


char *trimcpy (cp)
register char *cp;
{
    register char  *sp;

    while (isspace (*cp))
	cp++;
    for (sp = cp + strlen (cp) - 1; sp >= cp; sp--)
	if (isspace (*sp))
	    *sp = 0;
	else
	    break;
    for (sp = cp; *sp; sp++)
	if (isspace (*sp))
	    *sp = ' ';

    return getcpy (cp);
}
