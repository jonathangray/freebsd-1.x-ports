/********************************************************************
 * lindner
 * 3.3
 * 1993/07/20 23:14:13
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/subprocs.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: subprocs.c
 * procedures for dealing with child processes.
 *********************************************************************
 * Revision History:
 * subprocs.c,v
 * Revision 3.3  1993/07/20  23:14:13  lindner
 * Use waitpid, not wait3
 *
 * Revision 3.2  1993/04/15  21:44:50  lindner
 * Fixes for sysv signals
 *
 * Revision 3.1.1.1  1993/02/11  18:02:58  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.2  1992/12/31  05:36:51  lindner
 * Mods for VMS
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#ifdef VMS
void
sig_child()
{ }
setsighandler()
{ }

#else /* not VMS */
 

#include "gopher.h"

#include "Wait.h"

#if defined(SIGTSTP) && !defined(_CRAY)   /* True on a BSD system */
#include <sys/file.h>
#endif

#include <sys/ioctl.h>

/* A little signal handler that handles those damn zombies */


void
sig_child(sig)
  int sig;
{
     /*
      * Use the waitpid() system call with the WNOHANG option
      */

     int pid;

     Portawait status;

     while ( (pid = waitpid(-1, &status, WNOHANG|WUNTRACED)) > 0)
	  ;
}


#ifdef SIGCHLD
#ifndef SIGCLD
#  define SIGCLD SIGCHLD
#endif
#endif


setsighandler() 
{
          
#if defined(SIGTSTP) && !defined(_CRAY)
     signal(SIGCLD, sig_child);  /* BSD */
#else
     signal(SIGCLD, SIG_IGN);
#endif


}

#endif  /* not VMS */
