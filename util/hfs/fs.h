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
// $Id: fs.h,v 1.2 1994/05/15 06:19:24 rich Exp $
// $Log: fs.h,v $
// Revision 1.2  1994/05/15 06:19:24  rich
// hfs for FreeBSD.
//
// hfs allows files to be copied off a Macintosh HFS disk on a
// non-Macintosh computer.  Written by Craig Southeren,
// geoffw@extro.ucc.su.oz.au.
//
// Revision 1.1.1.1  1994/05/15  05:44:53  rich
// hfs 0.3 from sunsite
//
// Revision 1.8  1994/01/11  00:43:00  craigs
// Added Unmount call
//
// Revision 1.7  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.6  1993/12/30  07:55:14  craigs
// Changed GetFreeSpace to GetVolFreeSpace to avoid conflict
// with an obsolete NT function of the same name
//
// Revision 1.5  1993/12/23  22:41:30  craigs
// Changed for changes to catfile->FindDir
//
// Revision 1.4  1993/12/23  15:20:04  craigs
// Rehashed to copy with ResolvePath and extents functions
//
// Revision 1.3  1993/12/16  20:16:53  craigs
// Added extents file
//
// Revision 1.2  1993/11/23  20:28:25  craigs
// Removed reference to std.h
//
// Revision 1.1  1993/11/22  22:27:51  craigs
// Initial revision
//
//
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

