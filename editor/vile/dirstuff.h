/*
 *	dirstuff.h
 *
 *	Definitions to interface to unix-like DIRECTORY(3) procedures.
 *	Include this after "estruct.h"
 *
 * $Log: dirstuff.h,v $
 * Revision 1.1  1994/02/01 03:29:15  jkh
 * Initial revision
 *
 * Revision 1.8  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.7  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.6  1993/05/06  12:18:06  pgf
 * oops. typo.
 *
 * Revision 1.5  1993/05/06  11:59:58  pgf
 * added ifdefs for USE_D_NAMLEN, for systems that don't have or don't
 * need it (d_name[] is null-terminated on most systems)
 *
 * Revision 1.4  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.3  1993/04/02  10:57:41  pgf
 * cleanup of ls-based directory enumeration, and support (unused as yet?)
 * for old-style directories
 *
 * Revision 1.2  1993/04/01  15:50:34  pgf
 * for sysV machines, without POSIX or BSD dirent, we now have code
 * that enumerates directories using /bin/ls.
 *
 * Revision 1.1  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 */


#if ! VMS

#define USE_LS_FOR_DIRS 0
#define OLD_STYLE_DIRS 0	/* e.g., pre-SysV.2 14-char names */

#if DIRNAMES_NOT_NULL_TERMINATED
/* rumor has it that some early readdir implementations didn't null-terminate
   the d_name array, and on those you _have_ to use d_namlen to get
   the length.  most modern dirent structs are null-terminated, however. */
#define USE_D_NAMLEN 1
#endif

#if POSIX || TURBO || WATCOM
# if WATCOM
#   include <direct.h>
# else
#   include <dirent.h>
# endif
# define	DIRENT	struct dirent
#else	/* apollo & other old bsd's */
# if BERK
#  include <sys/dir.h>
#  define	DIRENT	struct direct
#  define USE_D_NAMLEN 1
# else
#  undef USE_LS_FOR_DIRS
#  define USE_LS_FOR_DIRS 1
# endif
#endif

#else /* VMS */

#include	<rms.h>
#include	<descrip.h>

#define USE_D_NAMLEN 1

typedef struct	{
	ULONG	d_ino;
	short	d_reclen;
	short	d_namlen;
	char	d_name[NAM$C_MAXRSS];		/* result: SYS$SEARCH */
	} DIRENT;

typedef	struct	{
	DIRENT		dd_ret;
	struct	FAB	dd_fab;
	struct	NAM	dd_nam;
	char		dd_esa[NAM$C_MAXRSS];	/* expanded: SYS$PARSE */
	} DIR;

#endif	/* VMS */

#if USE_LS_FOR_DIRS
#define	DIR	FILE
typedef	struct	{
	char	d_name[NFILEN];
	} DIRENT;
#endif

#if VMS || USE_LS_FOR_DIRS
extern	DIR *	opendir P(( char * ));
extern	DIRENT *readdir P(( DIR * ));
extern	int	closedir P(( DIR * ));
#endif

#if OLD_STYLE_DIRS
	this ifdef is untested
#define USE_D_NAMLEN 1
#define	DIR	FILE
#define	DIRENT	struct direct
#define	opendir(n)	fopen(n,"r")
extern	DIRENT *readdir P(( DIR * ));
#define	closedir(dp)	fclose(dp)
#endif

#ifndef USE_D_NAMLEN
#define USE_D_NAMLEN 0
#endif
