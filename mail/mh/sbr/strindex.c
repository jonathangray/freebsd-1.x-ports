/* strindex.c - "unsigned" lexical index */
#ifndef	lint
static char ident[] = "@(#)strindex.c,v 1.1.1.1 1993/01/30 04:41:30 jtc Exp";
#endif	/* lint */


int  stringdex (p1, p2)
register char  *p1,
               *p2;
{
    register char  *p;

    if (p1 == 0 || p2 == 0) return(-1);		/* XXX */

    for (p = p2; *p; p++)
	if (uprf (p, p1))
	    return (p - p2);

    return (-1);
}
