/* local.h - fine the -lndir include file */
/* @(#)local.h,v 1.1.1.1 1993/01/30 04:40:23 jtc Exp */

#ifndef	BSD42
#include <sys/types.h>
#else	/* BSD42 */
#include <sys/param.h>
#endif

#ifndef	BSD42
#ifndef NDIR
#ifndef	SYS5DIR
#include <dir.h>		/* last choice */
#else	/* SYS5DIR */
#include <dirent.h>
#endif
#else	/* NDIR */
#include <ndir.h>
#endif
#else	/* BSD42 */
#include <sys/dir.h>
#endif

#include <sys/stat.h>
