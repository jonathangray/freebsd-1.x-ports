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
//
// $Id: fs.cxx,v 1.3 1994/05/15 06:36:09 rich Exp $
//

#include <iostream.h>
#if !defined(__FreeBSD__)
#include <malloc.h>
#endif
#include <stdlib.h>
#include <string.h>

#include "catalog.h"
#include "extent.h"
#include "error.h"
#include "misc.h"
#include "fs.h"

/////////////////////////////////////////////////////
//
// Filesystem::Mount
//

int Filesystem::Mount (Volume * theVolume)

{
  int err;

  // save the volume
  volume = theVolume;

  // mount the volume
  if ((err = volume->Mount()) != E_OK)
    return err;
    
  // open the catalog and extent files
  catfile = new CatalogFile (*this);
  extfile = new ExtentFile  (*this);

  return 0;
}



/////////////////////////////////////////////////////
//
// Filesystem::Unmount
//

void Filesystem::Unmount ()

{
  delete catfile;
  delete extfile;

  volume->InvalidateCache();
}


/////////////////////////////////////////////////////
//
//
//

int Filesystem::ReadBlock (unsigned long logical_block, void * buffer)

{
  return volume->Read(logical_block + volume->mdb.drAlBlSt, buffer);
}
 

/////////////////////////////////////////////////////
//
//
//

char * Filesystem::GetVolumeName ()

{
  return ptocstr(volume->mdb.drVN);
}


//////////////////////////////////////////////
//
//  convert a pathname to a directory/file buffer
//

BOOL Filesystem::ResolvePath (FileBuffer & fb,
                                    char * pathname,
                                   FileRef current_dir)

{
  FileRef    dir;

  // get the directory start traversing the path at
  dir = (pathname[0] == ':') ? M_CNID_ROOT : current_dir;
  if (!FindDir(fb, dir)) {
    cerr << "internal error: cannot find current directory???\n";
    exit (1);
  }

  // loop through the path, taking each element one at a time
  char * dirname = strtok(pathname, ":");
  while (dirname != NULL) {
    //
    // check if to stay at this directory level
    //
    if (strcmp(dirname, ".") == 0) 
      ;

    //
    // check if to go up one directory level
    //
    else if (strcmp(dirname, "..") == 0) {
      if (dir != M_CNID_ROOT) {
        dir = fb.parid;
        FindDir(fb, dir);
      }
    }

    //
    // else check if to go down a directory level
    //
    else {
      // make sure we are in a directory
      if (fb.IsFile()) {
  cerr << dirname << ": not a directory\n";
  return FALSE;
      }

      // try to change to the directory specified
      if (!FindFile(fb, dir, dirname)) {
  cerr << dirname << ": no such file or directory\n";
  return FALSE;
      }
       
      // get the ID of the new file/directory
      dir = fb.id;
    }

    // extract the next path element
    dirname = strtok(NULL, ":");
  }

  return TRUE;
}

