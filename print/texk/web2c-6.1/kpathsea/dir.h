/* dir.h: checked directory operations.

Copyright (C) 1992, 93 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef KPATHSEA_DIR_H
#define KPATHSEA_DIR_H

#include <kpathsea/c-proto.h>
#include <kpathsea/types.h>

/* Use struct dirent instead of struct direct.  */
#if defined (DIRENT) || defined (_POSIX_VERSION)
#include <dirent.h>
#define NLENGTH(dirent) strlen ((dirent)->d_name)
#else /* not (DIRENT or _POSIX_VERSION) */
#define dirent direct
#define NLENGTH(dirent) ((dirent)->d_namlen)

#ifdef SYSNDIR
#include <sys/ndir.h>
#endif

#ifdef NDIR
#include <ndir.h>
#endif

#ifdef SYSDIR
#include <sys/dir.h>
#endif

#endif /* not (DIRENT or _POSIX_VERSION) */

/* Like opendir, closedir, and chdir, but abort if DIRNAME can't be opened.  */
extern DIR *xopendir P1H(string dirname);
extern void xclosedir P1H(DIR *);

/* Returns true if FN is a directory or a symlink to a directory.  */
extern boolean dir_p P1H(const_string fn);

/* If FN is a readable directory, return the number of links it has.
   Otherwise, return -1.  */
extern int dir_links P1H(const_string fn);

#endif /* not KPATHSEA_DIR_H */
