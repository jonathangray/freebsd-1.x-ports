//
// hfstools - a Macintosh filesystem access tool
// (C) Copyright 1993 by Equivalence
//
// This file part of hfs.
//
// hfs is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
// 
// hfs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with hfs; see the file COPYING.  If not, write to
// the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
//
// $Id: mdirent.cxx,v 1.3 1994/05/15 06:36:17 rich Exp $
//

#include <stdio.h>
#include <iostream.h>

#include "mdir.h"
#include "status.h"
#include "fs.h"
#if !defined(__FreeBSD__)
#include "malloc.h"
#endif
#include "cmd.h"

static Filesystem * mdir_fs;

void SetMDirFilesystem (Filesystem & fs)

{
  mdir_fs = &fs;
}

extern "C" {

#include "mdirent.h"

mac_DIR *mac_opendir (char *name)

{
  // resolve the path to a directory ID
  FileBuffer * fb = new FileBuffer();
  if (!mdir_fs->ResolvePath(*fb, name, status_file->GetDir())) {
    delete fb;
    return NULL;
  }

  // create a DIR object using malloc, and then initialise it
  mac_DIR * dir   = (mac_DIR *) malloc(sizeof (mac_DIR));
  dir->state      = 0;
  dir->fb         = (void *)fb;
  dir->dd_buf     = (struct mac_dirent *) malloc(sizeof (struct mac_dirent));

  return dir;
}

int mac_closedir (mac_DIR * dirp)

{
  free ((char *)dirp->dd_buf);
  free (dirp);
  
  return 0;
}

struct mac_dirent *mac_readdir (mac_DIR * dirp)

{
  FileBuffer * fb = (FileBuffer *)dirp->fb;

  switch (dirp->state) {
    case 0:
      strcpy(dirp->dd_buf->d_name, ".");
      dirp->state++;
      break;

    case 1:
      strcpy(dirp->dd_buf->d_name, "..");
      dirp->state++;
      break;

    case 2:
      if (!mdir_fs->FindFirst(*fb, fb->id)) {
        dirp->state = 4;
        return NULL;
      }
      strcpy(dirp->dd_buf->d_name, fb->name);
      dirp->state++;
      break;

    case 3:
      if (!mdir_fs->FindNext(*fb)) {
        dirp->state = 4;
        return NULL;
      }
      strcpy(dirp->dd_buf->d_name, fb->name);
      break;

    default:
    case 4:
      return NULL;
  }

  return dirp->dd_buf;
}


int mac_stat (char *filename, struct mac_stat *stat_buf)

{
  FileBuffer fb;

  if (!mdir_fs->ResolvePath(fb, filename, status_file->GetDir())) 
    return 1;

  stat_buf->st_mode = fb.IsFile() ? 0 : 1;
  return 0;
}

};
