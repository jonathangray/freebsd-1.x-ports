/* randomize.c 
	vi:se ts=3 sw=3:
 */

/* $Id: randomize.c,v 1.1 1994/02/19 16:03:08 ache Exp $ 
 * $Log: randomize.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:55:13  espie
 * *** empty log message ***
 *
 * Revision 1.3  1994/01/06  19:22:23  Espie
 * *** empty log message ***
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.2  1992/12/03  15:00:50  espie
 * restore stty.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 */

/* input: a series of names (as argv[1:argc - 1])
 * output: the same names, in a random order.
 * with the new database lookup facility, very useful for e.g.,
 * tracker `randomize *` (jukebox)
 */

#include <stdlib.h>
#include <stdio.h>
#include "defs.h"

ID("$Id: randomize.c,v 1.1 1994/02/19 16:03:08 ache Exp $")

/* n = random_range(max): output a number in the range 0:max - 1.
 * For our purpose, we don't have to get a very random number,
 * so the standard generator is alright.
 */
int random_range(max)
int max;
    {
    static init = 0;

        /* initialize the generator to an appropriate seed eventually */
    if (!init)
        {
        srand(time(0));
        init = 1;
        }
    return rand()%max;
    }

/* output(s): output s in a suitable format. Ideally, output() should use
 * the shell quoting conventions for difficult names. Right now, it doesn't
 */
void output(s)
char *s;
    {
    for(; *s; s++)
        switch(*s)
            {
    /*    case ' ':
        case '(':
        case ')':
        case '\\':
            putchar('\\');
            */
        default:
            putchar(*s);
            }
    putchar(' ');
    }

int main(argc, argv)
int argc;
char *argv[];
    {
    int i, k;

        /* set up everything so that our names are in argv[0 : argc - 2] */
    for (i = argc - 1, argv++; i; i--)
        {
            /* invariant: the remaining names are in argv[0: i - 1] */
        k = random_range(i);
        output(argv[k]);
        argv[k] = argv[i - 1];
        }
   exit(0);
    }
