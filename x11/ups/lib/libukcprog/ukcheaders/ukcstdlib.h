/*  ukcstdlib.h - version of ANSI stdlib.h for Classic C.  */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* ukcstdlib.h 1.1 5/16/91 (UKC) */

#ifndef UKCSTDLIB_H_INCLUDED
#define UKCSTDLIB_H_INCLUDED

#ifdef __STDC__
#include <stdlib.h>
#else

double atof(), strtod();
long atol(), strtol();
unsigned long strtoul();
char *calloc(), *malloc(), *realloc(), *getenv();
void srand(), free(), abort(), exit(), qsort();
#endif /* !__STDC__ */

#endif /* !UKCSTDLIB_H_INCLUDED */
