/* closefds.c - close-up fd:s */
#ifndef	lint
static char ident[] = "@(#)closefds.c,v 1.1.1.1 1993/01/30 04:41:23 jtc Exp";
#endif	/* lint */

#include "../h/mh.h"
#ifndef	BSD42
#include <stdio.h>
#endif	/* not BSD42 */


void	closefds (i)
register int	i;
{
#ifndef	BSD42
    int     nbits = _NFILE;
#else	/* BSD42 */
    int     nbits = getdtablesize ();
#endif	/* BSD42 */

    for (; i < nbits; i++)
#ifdef	OVERHEAD
	if (i != fd_def && i != fd_ctx)
#endif	/* OVERHEAD */
	    (void) close (i);
}
