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
// $Id: fs.h,v 1.3 1994/05/15 06:36:10 rich Exp $
//

#ifndef _FS_H
#define _FS_H

#include "misc.h"
#include "volume.h"
#include "error.h"
#include "catalog.h"
#include "extent.h"
#include "file.h"

//////////////////////////////////////////////
//
//  Filesystem
//  A Macintosh filesystem 
//

class Filesystem {

  friend class BTreeFile;
  friend class CatalogFile;
  friend class File;

  public:
    //
    //  User functions
    //

    // Mount a filesystem on the specified volume
    int           Mount         (Volume * volume);

    // Unmount a filesystem
    void          Unmount       ();

    // Return the volume name
    char *        GetVolumeName ();

    // Return number of bytes free on the volume
    inline ULONG         GetVolFreeSpace() { return (INT)(GetMDB()->drFreeBks)*(ULONG)GetMDB()->drAlBlkSiz; }

    // return TRUE if the specified file/directory can be found
    inline BOOL FindFile (FileBuffer & fb, FileRef parent_dir, char * name)
      { return catfile->FindFile(fb, parent_dir, name); }

    // return pathname if the specified directory exists
    inline BOOL FindDir (FileBuffer & fb, FileRef dirid, char **Path = NULL)
      { return catfile->FindDir(fb, dirid, Path); }

    // find the first file in the specified directory
    inline BOOL FindFirst (FileBuffer & fb, FileRef dirid)
      { return catfile->FindFirst(fb, dirid); }

    // used to find subsequent files in the same directory
    inline BOOL FindNext (FileBuffer & fb)
      { return catfile->FindNext(fb); }

    // resolve a pathname to a filebuffer
    BOOL ResolvePath (FileBuffer & fb, char * pathname, FileRef current_dir);

    //
    // internal functions
    //

    // find an extent record for a file
    inline BOOL FindExtent (AllBlkNum * extent, AllBlkNum * start_block, FileRef id, AllBlkNum block, BOOL IsData)
      { return extfile->FindExtent(extent, start_block, id, block, IsData); }

    // return ptr to the MDB for this volume
    inline MDB * GetMDB () { return &(volume->mdb); }

    // read a logical block
    int Filesystem::ReadBlock (LogBlkNum logical_block, void * buffer);
    

  protected:
    Volume      * volume;    // volume driver
    CatalogFile * catfile;   // catalog file
    ExtentFile  * extfile;   // extents file
};

#endif

