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
// $Id: extent.cxx,v 1.1 1994/05/15 05:44:52 rich Exp $
// $Log: extent.cxx,v $
// Revision 1.1  1994/05/15 05:44:52  rich
// Initial revision
//
// Revision 1.2  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.1  1993/12/16  20:16:12  craigs
// Initial revision
//
//

#include <iostream.h>

#include "catalog.h"
#include "fs.h"
#include <string.h>

typedef MUINT ExtDataRec[6];

///////////////////////////////////////////////////////
//
// FindExtentKey
//   This class is a Btree search key used to find an extent
//   for a file

class FindExtentKey : public BTreeSearchKey {

  friend class ExtentFile;

  public:
    FindExtentKey(AllBlkNum * theExtent,
                  AllBlkNum * theExtentStart,
                      FileRef theId,
                    AllBlkNum theStartBlock,
                         BOOL IsData);

  protected:
    virtual Comparison Compare (BTreeKey & key, BOOL IsLeaf);
    virtual BOOL OnFound       (BTreeKey & key, BTreeLeaf & leaf);

    FileRef   id;
    AllBlkNum start_block;
    int       data_fork;
    AllBlkNum *extent;
    AllBlkNum *extent_start;
};


FindExtentKey::FindExtentKey(AllBlkNum * theExtent,
                             AllBlkNum * theExtentStart,
                             FileRef theId,
                             AllBlkNum theStartBlock,
                             BOOL IsData)

{
  id          = theId;
  start_block = theStartBlock;
  data_fork   = IsData ? 0x00 : 0xff;
  extent      = theExtent;
  extent_start = theExtentStart;
}

BTreeSearchKey::Comparison FindExtentKey::Compare (BTreeKey & key, BOOL IsLeaf)

{
  ExtKeyRec & extkey = (ExtKeyRec &)key;

#if 0
cerr << "Comparing key to "
     << extkey.GetID() << "(" << id << ")"
     << ", fork = "
     << extkey.GetForkType() << "(" << data_fork << ")"
     << ", block = "
     << extkey.GetFABN() << "(" << start_block << ")"
     << ", (" << (IsLeaf ? "leaf" : "index") << ")\n";
#endif

  if (extkey.GetForkType() != data_fork) {
    if (data_fork == 0x00)
      return LessThan;
    else
      return GreaterThan;
  }

  if (extkey.GetID() != id) {
    if (extkey.GetID() < id)
      return LessThan;
    else
      return GreaterThan;
  }

  if (extkey.GetFABN() < start_block)
    return LessThan;
  else if (extkey.GetFABN() > start_block)
    return GreaterThan;

  return Equals;
};


BOOL FindExtentKey::OnFound (BTreeKey & key, BTreeLeaf & leaf)

{
  ExtKeyRec  & extkey  = (ExtKeyRec &)key;
  ExtDataRec & extdata = (ExtDataRec &)leaf;

  for (int i = 0; i < 6; i++)
    extent[i] = (AllBlkNum)extdata[i];

  *extent_start = extkey.GetFABN();

  return TRUE;
}



//////////////////////////////////////
//
// ExtentFile::ExtentFile
//

ExtentFile::ExtentFile (Filesystem & theFs)
    : BTreeFile (theFs,
                 theFs.GetMDB()->drXtExtRec, 
                 M_CNID_EXTENTS,
                 theFs.GetMDB()->drXtFlSize)

{
#if 0
  cerr << "\n\nExtent extent record:\n";
  dump (theFs.GetMDB()->drXtExtRec, 12);
#endif
}


//////////////////////////////////////
//
// ExtentFile::FindExtent
//

BOOL ExtentFile::FindExtent (AllBlkNum * theExtent, AllBlkNum * start_block, FileRef theId, AllBlkNum theBlockNum, BOOL IsData)

{
  FindExtentKey key(theExtent, start_block, theId, theBlockNum, IsData);
  return Search (key);
}
