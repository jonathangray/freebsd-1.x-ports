/* whatnow.c - the MH WhatNow? shell */
#ifndef	lint
static char ident[] = "@(#)whatnow.c,v 1.1.1.1 1993/01/30 04:41:41 jtc Exp";
#endif	/* lint */

#ifdef LOCALE
#include	<locale.h>
#endif

main (argc, argv)
int	argc;
char  **argv;
{
#ifdef LOCALE
    setlocale(LC_ALL, "");
#endif
    WhatNow (argc, argv);
}
