/********************************************************************
 * lindner
 * 3.3
 * 1993/07/20 23:19:41
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/Wait.h,v
 * $Status: $
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: Wait.h
 * Routines that portably define waitpid()
 *********************************************************************
 * Revision History:
 * Wait.h,v
 * Revision 3.3  1993/07/20  23:19:41  lindner
 * Use waitpid, not wait3
 *
 * Revision 3.2  1993/05/05  18:40:16  lindner
 * Solaris mods
 *
 * Revision 3.1.1.1  1993/02/11  18:03:04  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


/*
 * A header file to portably include the stuff for doing waitpid() etal
 */

#include <sys/ioctl.h>
#include <sys/wait.h>
#include <signal.h>

/** These don't know what waitpid() is..  Naughty! **/

#if defined(NeXT)
    typedef union wait Portawait;
#   define waitpid(a,b,c) wait3(b,c,NULL)

#else
    /*** Everything else mostly does.. ***/
    typedef int Portawait;
#endif

