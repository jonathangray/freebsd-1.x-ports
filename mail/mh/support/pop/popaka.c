/* popaka.c - generate POP entries for MMDF-II alias file */
#ifndef	lint
static char ident[] = "@(#)popaka.c,v 1.1.1.1 1993/01/30 04:42:01 jtc Exp";
#endif	/* lint */

#include <stdio.h>
#include "../zotnet/bboards.h"

static process();
/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    struct bboard  *bb;

    if (!setbbinfo (POPUID, POPDB, 1)) {
	fprintf (stderr, "setbbinfo(%s, %s, 1) failed -- %s\n",
		POPUID, POPDB, getbberr ());
	exit (1);
    }

    (void) setbbent (SB_STAY);
    while (bb = getbbent ())
	process (bb);
    (void) endbbent ();

    exit (0);
}

/*  */

static  process (bb)
struct bboard  *bb;
{
    printf ("%s: %s@pop\n", bb -> bb_name, bb -> bb_name);
}
