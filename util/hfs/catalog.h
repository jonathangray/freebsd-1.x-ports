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
// $Id: catalog.h,v 1.1 1994/05/15 05:44:51 rich Exp $
// $Log: catalog.h,v $
// Revision 1.1  1994/05/15 05:44:51  rich
// Initial revision
//
// Revision 1.8  1994/01/11  00:40:40  craigs
// General cleanups
//
// Revision 1.7  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.6  1993/12/30  07:48:24  craigs
// Removed redundant dirname attribute from FileBuffer
//
// Revision 1.5  1993/12/23  22:39:12  craigs
// Changed FindDir to be bit more intelligent
//
// Revision 1.4  1993/12/23  15:15:27  craigs
// Added copy constructor for FileBuffer
// Added extra functions for FileBuffer
//
// Revision 1.3  1993/11/23  22:20:20  craigs
// Btree is now a descendant of MacFile, so extents now work!
//
// Revision 1.2  1993/11/23  20:29:37  craigs
// Changed unsigned long to ULONG, and short to BOOL
//
// Revision 1.1  1993/11/22  22:27:51  craigs
// Initial revision
//
//
//

#ifndef _CATALOG_H
#define _CATALOG_H

#include "btree.h"

class FileBuffer {
  private:
    void Assign(FileBuffer & fb);

  public:
    inline FileBuffer () 
      { name = NULL; rsrcsize = datasize = 0; }

    inline FileBuffer(FileBuffer & fb) 
      { name = NULL; Assign(fb); }

    inline FileBuffer & operator = (FileBuffer & fb)
      { Assign(fb); return *this; }

    inline ~FileBuffer ()
      { if (name != NULL)    delete name; };

    inline BOOL IsFile() 
      { return isfile; }

    inline BOOL IsInvisible () { return isfile && (flags & FInfo::fInvisible); }

    char * name;  // name of the file as a "C" string
    FileRef id;   // the ID
    FileRef parid;  // parent directory ID
    ULONG   date; // time of creation
    BOOL   isfile;  // TRUE if this is a file
    ULONG      rsrcsize;      // size of resource fork
    ULONG      datasize;  // size of data fork

    // remainder are only valid if item is a file
    char       creator[4];  // creator
    char       filetype[4]; // file type
    ExtDataRec dataext;         // first extent of data fork
    ExtDataRec rsrcext;         // first extent of resource fork
    UINT       flags;           // Finder flags
};

//////////////////////////////////////////////
//
//  CatalogFile
//

class CatalogFile : public BTreeFile {
  public:
    CatalogFile  (Filesystem & theFs);

    BOOL FindFirst (FileBuffer & fb, FileRef dirID);
    BOOL FindNext  (FileBuffer & fb);
    BOOL FindFile  (FileBuffer & fb, FileRef dirID, char * name);
    BOOL FindDir   (FileBuffer & fb, FileRef dirID, char **Path = NULL);
};

#endif 

