/* fileio.h: routines used by TeX, Metafont, and BibTeX.  */

#ifndef FILEIO_H
#define FILEIO_H

#include "../common/extra.h"

#ifdef BibTeX
/* I don't understand the hack in ../bibtex/convert that makes setjmp
   necessary, but here's the include file, anyway.  */
#include <setjmp.h>
#endif

/* `aopenin' is used both for input files and pool files, so it
   needs to know what path to use.  `aopenout' doesn't use any paths,
   though.  */
#define aopenin(f, p)	open_input (&(f), p)
#define aopenout(f)	open_output (&(f))

/* Closing files is even easier; we don't bother to check the return
   status from fclose(3).  */
#define aclose(f)	if (f) (void) fclose (f)

typedef FILE *alphafile;

#ifdef BibTeX
/* See bibtex.ch for why these are necessary.  */
extern FILE *standardinput;
extern FILE *standardoutput;
#endif

#endif /* not FILEIO_H */
