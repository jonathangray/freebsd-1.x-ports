/********************************************************************
 * lindner
 * 3.4
 * 1993/06/14 22:23:02
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/gopherd.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: gopherd.h
 * Header file for gopher server.
 *********************************************************************
 * Revision History:
 * gopherd.h,v
 * Revision 3.4  1993/06/14  22:23:02  lindner
 * status-->state:
 *
 * Revision 3.3  1993/04/15  21:59:08  lindner
 * Fix for newer hpux systems
 *
 * Revision 3.2  1993/02/19  21:21:32  lindner
 * Fixed problems with signals
 *
 * Revision 3.1.1.1  1993/02/11  18:02:51  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


#include "conf.h"

#include <ctype.h>
#include <stdio.h>


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#if !defined(hpux) || defined(__hpux)
#include <arpa/inet.h>
#endif
#include <pwd.h>
#include <errno.h>

#include <signal.h>

/** For logfile locking... **/
#if !defined(NeXT) && !defined(mips) && !defined(UMAX43) && !defined(sequent) && !defined(sony_news)
#  include <unistd.h>
#  include <fcntl.h>
#else
#  include <fcntl.h>
#  include <sys/file.h>
#  ifndef SEEK_END
#    define SEEK_END L_XTND
#    define SEEK_SET L_SET
#  endif
#endif

extern int errno;

#include "String.h"

#include <sys/stat.h>
#include <time.h>

#include <sys/param.h>
/* This might be in <sys/param.h>, usually 64 */
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN                (64)
#endif

#ifndef NOFILE
#define NOFILE (100)
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN (512)
#endif



#include "GDgopherdir.h"
#include "Dirent.h"

/*
 * Make sure we don't accidentally use a library routine instead of our
 * private restricted version.  Need to avoid the stat in struct stat
 * getting clobbered by the #define for the routine.
 */
typedef struct stat STATSTR;
#include "openers.h"
#define open	barf_ropen
#define fopen	barf_rfopen
#define stat	barf_rstat
#define opendir	barf_ropendir
#define chdir	barf_rchdir

#include "compatible.h"
#include "util.h"
#include "gopherdconf.h"

/*** This one must be last ***/
#include "globals.h"
