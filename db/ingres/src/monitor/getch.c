#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)getch.c	8.1	12/31/84)



/*
**  GET CHARACTER
**
**	This routine is just a getchar, except it allows a pseudo-
**	EOF (i.e. == '\0') marker.
*/

char
getch(void)
{
	register char	c;

	if (GiveEof)
		c = '\0';           /* stands for EOF  */
	else
		c = getc(Input);

        if (c == EOF)               /*  if (c < 0 )   K.Okamoto  */
	    c = '\0';               /*  if really EOF(-1)
                                        This should be rewrite to permit
					to input 8 bit code  K.Okamoto */

	/* deliver EOF if newline in Oneline mode */
	if (c == '\n' && Oneline) {
		ungetc(c, Input);
		c = '\0';
        }
	GiveEof = FALSE;
	return (c);
}
