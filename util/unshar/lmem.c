/*
**  Get some memory or die trying.
*/
/* LINTLIBRARY */
#include "shar.h"
#ifdef	RCSID
static char RCS[] =
	"$Header: /a/cvs/386BSD/ports/util/unshar/lmem.c,v 1.1 1993/09/04 16:45:34 jkh Exp $";
#endif	/* RCSID */


align_t
getmem(i, j)
    unsigned int	 i;
    unsigned int	 j;
{
    extern char		*calloc();
    align_t		 p;

    /* Lint fluff:  "possible pointer alignment problem." */
    if ((p = (align_t)calloc(i, j)) == NULL) {
	/* Print the unsigned values as int's so ridiculous values show up. */
	Fprintf(stderr, "Can't Calloc(%d,%d), %s.\n", i, j, Ermsg(errno));
	exit(1);
    }
    return(p);
}
