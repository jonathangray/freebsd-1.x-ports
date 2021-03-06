/* gans.c - get an answer from the user */
#ifndef	lint
static char ident[] = "@(#)gans.c,v 1.1.1.1 1993/01/30 04:41:25 jtc Exp";
#endif /* lint */

#include "../h/mh.h"
#include <stdio.h>


gans (prompt, ansp)
register char *prompt;
register struct swit *ansp;
{
    register int    i;
    register char  *cp;
    register struct swit   *ap;
    char    ansbuf[BUFSIZ];

    for (;;) {
	printf ("%s", prompt);
	(void) fflush (stdout);
	cp = ansbuf;
	while ((i = getchar ()) != '\n') {
	    if (i == EOF)
		return 0;
	    if (cp < &ansbuf[sizeof ansbuf - 1]) {
#ifdef LOCALE
		i = (isalpha(i) && isupper(i)) ? tolower(i) : i;
#else
		if (i >= 'A' && i <= 'Z')
		    i += 'a' - 'A';
#endif
		*cp++ = i;
	    }
	}
	*cp = 0;
	if (ansbuf[0] == '?' || cp == ansbuf) {
	    printf ("Options are:\n");
	    for (ap = ansp; ap -> sw; ap++)
		printf ("  %s\n", ap -> sw);
	    continue;
	}
	if ((i = smatch (ansbuf, ansp)) < 0) {
	    printf ("%s: %s.\n", ansbuf, i == -1 ? "unknown" : "ambiguous");
	    continue;
	}
	return i;
    }
}
