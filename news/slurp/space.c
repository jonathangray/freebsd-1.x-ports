/*
 * space - determine free space on a filesystem
 *
 * Copyright (C) 1992/93 Stephen Hebditch and others.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * This routine determines if there is enough space on the filesystem
 * holding the news spool for a new batch of incoming articles.
 * It is based on space.c in the NNTP reference implementation which
 * credits Stan Barber <sob@bcm.tmc.edu>, Tad Guy <tadguy@cs.odu.edu>,
 * Chris Jepeway <jepeway@utkcs2.cs.utk.edu> and Tom Lane <tgl@cs.cmu.edu>
 * but has been heavily cleaned up and support for SVR4, Linux and
 * BSDI added.
 *
 * $Id: space.c,v 1.1 1993/08/27 02:47:48 alm Exp $
 *
 * $Log: space.c,v $
 * Revision 1.1  1993/08/27 02:47:48  alm
 * Initial revision
 *
 * Revision 1.7  1993/06/14  15:25:42  root
 * Added routine name to error messages.
 * Made close() consistent with usage elsewhere.
 *
 * Revision 1.6  1993/04/22  18:27:21  root
 * Changed NeXT define to __NeXT__.
 *
 * Revision 1.5  1993/03/01  18:08:20  root
 * Completely reworked from the original and heavily tidied up.
 * Support added for SVR4 and Linux.
 *
 * Revision 1.4  1993/02/14  16:12:20  root
 * Added support for BSDI.
 *
 */

#include "slurp.h"

#ifdef MINFREE

#include <sys/types.h>
#include <sys/stat.h>

#define DFREE_OK		0
#define DFREE_INODES	1
#define DFREE_BLOCKS	2
#define DFREE_ERR		3

/*
 * Definitions for use with dfree() for various UNIX families.
 *
 * statfilesys		Routine to call when trying to stat a file system
 *					to get the number of free blocks available.
 * statfs_type		The data type into which statfs() wants to return
 *					useful information.
 * bombed			Boolean expression returning 1 if a call to statfs()
 *					fails.
 * blkavail			Given a statfs_type called fs, return number of free
 *					blocks available to a non-superuser.
 * filavail			Given a statfs_type called fs, return number of free
 *					inodes available to a non-superuser.
 */

  #if defined(SVR4)
	#include <sys/statvfs.h>
	#define statfilesys		statvfs
	typedef struct statvfs 	statfs_type;
	#define bombed(call)	((call) == -1)
	#define blkavail(fs)	((fs).f_bavail)
	#define filavail(fs)	((fs).f_favail)

  #elif defined(sun) || defined(hpux) || defined(pyr) || defined(hp300) || defined(__NeXT__) || defined(linux)
	#include <sys/vfs.h>
	#define statfilesys		statfs
	typedef struct statfs 	statfs_type;
	#define bombed(call)	((call) == -1)
	#define blkavail(fs)	((fs).f_bavail)
	#define filavail(fs)	((fs).f_ffree)

  #elif defined(apollo)
	#include <sys/types.h>
	#include <sys/statfs.h>
	#define statfilesys(a,b) statfs (a, b, sizeof (struct statfs), 0)
	typedef struct statfs 	statfs_type;
	#define bombed(call)	((call) == -1)
	#define blkavail(fs)	((fs).f_bfree)
	#define filavail(fs)	((fs).f_ffree)

  #elif defined(ultrix)
	#include <sys/mount.h>
	typedef struct fs_data	statfs_type;
	#define statfilesys		statfs
	#define bombed(call)	((call) <= 0)
	#define blkavail(fs)	((int)((fs).fd_req.bfreen))
	#define filavail(fs)	((int)((fs).fd_req.gfree))

  #elif defined(__bsdi__) || defined(__386BSD__)
	#include <sys/mount.h>
	typedef struct statfs	statfs_type;
	#define statfilesys		statfs
	#define bombed(call)	((call) < 0)
	#define blkavail(fs)	((int)((fs).f_bfree))
	#define filavail(fs)	((int)((fs).f_ffree))

  #elif defined(SVR3)
	#include <ustat.h>
	typedef struct ustat statfs_type;
		int
	statfilesys (char *dir, statfs_type *fs)
		{
		struct stat file;
		if (stat (dir, &file))
			return (-1);
		if (ustat (file.st_dev, fs))
			return (-2);
		return (0);
		}
	#define bombed(call)	(call != 0)
	#define blkavail(fs)	((fs).f_tfree)
	#define filavail(fs)	((fs).f_tinode)	

  #elif defined(CMU_MACH)
	#include <sys/ioctl.h>
	typedef struct fsparam statfs_type;
		int
	statfilesys (char *dir, statfs_type *fs)
		{
		int fd;
		fd = open (dir, O_RDONLY);
		if (fd < 0)
			return (-1);
		if (ioctl (fd, FIOCFSPARAM, fs) < 0)
			{
			(void) close (fd);
			return(-2);
			}
		(void) close (fd);
		return (0);
		}
	#define bombed (call)	((call) < 0)
	#define blkavail (fs)	((fs).fsp_free-((fs).fsp_size*(fs).fsp_minfree+99)/100)

  #else
	SPACE DEFINITIONS NOT AVAILABLE FOR THIS MACHINE OR NOT SET CORRECTLY
  #endif


/*
 * dfree - Return the free space available on the file system containing
 * the specified directory. Space is measured in kilobytes. A negative
 * value is returned if there is an error.
 */

	static int
dfree (char *location, int free_space)
	{
	statfs_type fsys;

	/* Return error if can't get file system info */
	if (bombed (statfilesys (location, &fsys)))
		return (DFREE_ERR);

	/* If able to test if free inodes then do so */
#if defined(filfree) && defined(MINFILES)
	if (filfree (fsys) < MINFILES )
 		return (DFREE_INODES);
#endif

	/* Test if blocks are available */
	if (blkavail (fsys) < free_space)
		return (DFREE_BLOCKS);

	return (DFREE_OK);
	}


/*
 * space - Returns 1 if there are a sufficient number of free blocks
 * and inodes on the filesystem containing the news spool, or 0 if
 * there are only a small number of blocks / inodes remaining.
 */

	int
space (int min_free)
	{
	switch (dfree (SPOOLDIR, min_free))
		{
		case DFREE_OK:
			return (1);
		case DFREE_ERR:
			log_ret ("space: dfree failed due to system call error");
			break;
		case DFREE_INODES:
			log_msg ("space: no inodes on %s", SPOOLDIR);
			break;
		case DFREE_BLOCKS:
			log_msg ("space: no space on %s", SPOOLDIR);
			break;
		}
	return (0);
	}

#endif /* MINFREE */

/* END-OF-FILE */
