/*
 * hfstools - a Macintosh filesystem access tool
 * (C) Copyright 1993 by Equivalence
 *
 * This file part of hfs.
 *
 * hfs is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * hfs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with hfs; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
 *
 *
 * $Id: mdirent.h,v 1.2 1994/05/15 06:36:18 rich Exp $
 */

#ifndef _MDIRENT_H_
#define	_MDIRENT_H_

/*************************************************

  replacements for the subset of <dirent.h> required by glob

 *************************************************/
 

#define	mac_PATH_MAX		255
#define	mac_NAME_MAX		31


#define mac_MAXPATHLEN 		mac_PATH_MAX

struct mac_dirent {
/*	long		d_ino; */
/*	off_t		d_off; */
/*	unsigned short	d_reclen; */
	char		d_name[mac_NAME_MAX+1];
};

typedef struct {
/*  int dd_fd;	*/		/* file descriptor */
/*  int dd_loc; */		/* offset in buffer */
/*  int dd_size; */		/* # of valid entries in buffer */

  struct mac_dirent *dd_buf; 	/* -> directory buffer */

  int state;			/* 0 = return ".',
                                   1 = return ".."
                                   2 = return first
                                   3 = return next; */

  void * fb;                    /* ptr to the FileBuffer */
} mac_DIR;			/* stream data from opendir() */

extern mac_DIR *mac_opendir (char *name);

extern int mac_closedir (mac_DIR * dirp);

extern struct mac_dirent *mac_readdir (mac_DIR * dirp);

/*************************************************

  replacements for the subset of <sys\stat.h> required by glob

 *************************************************/


struct mac_stat {
/*	dev_t   st_dev;*/
/*	ino_t   st_ino;*/
/*	nlink_t st_nlink;*/
/*	uid_t   st_uid;*/
/*	gid_t   st_gid;*/
/*	dev_t   st_rdev;*/
/*	off_t   st_size;*/
/*	time_t  st_atime;*/
/*	time_t  st_mtime;*/
/*	time_t  st_ctime;*/
	int     st_mode;		/* 1 if a directory, 0 if a file */
};

extern int	mac_stat (char *filename, struct mac_stat *stat_buf);

#endif
