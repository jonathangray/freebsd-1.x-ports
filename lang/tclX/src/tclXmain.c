/* 
 * tclXmain.c --
 *
 * Main to run the Tcl shell.  This file is a useful template for custom
 * applications that wish to have Tcl as the top level command language.
 * This file can also be compiled with C++.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1993 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tclXmain.c,v 1.1 1994/02/09 01:53:42 jkh Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtend.h"

#ifdef __cplusplus
int
main (int     argc,
      char  **argv)
#else
main(argc, argv)
     int     argc;
     char  **argv;
#endif
{
    TclX_Shell (argc, argv);

    return 0;  /* Never exits here: make the compiler happy */

}

