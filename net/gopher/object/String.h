/********************************************************************
 * lindner
 * 3.3
 * 1993/06/15 06:09:50
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/String.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: String.h
 * Header file that automagically includes string/strings.h
 *********************************************************************
 * Revision History:
 * String.h,v
 * Revision 3.3  1993/06/15  06:09:50  lindner
 * Fixes for multiple includes and prettification
 *
 * Revision 3.2  1993/05/05  18:39:49  lindner
 * Solaris and VMS mods
 *
 * Revision 3.1.1.1  1993/02/11  18:03:04  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#ifndef GSTRINGS_H
#define GSTRINGS_H

/* string.h  == AT&T
   strings.h == BSD

Though your milage may vary... 

*/

#ifdef __convex__
#  include <stdarg.h>
#else
# ifndef VMS
#  include <string.h>
# endif
#endif

#if defined(USG) || defined(hpux) || defined(__svr4__) || defined(VMS)
#include <string.h>
#else
#include <strings.h>
#endif


#endif /* GSTRINGS_H */
