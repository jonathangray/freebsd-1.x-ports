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
// $Id: btree.h,v 1.1 1994/05/15 05:44:51 rich Exp $
// $Log: btree.h,v $
// Revision 1.1  1994/05/15 05:44:51  rich
// Initial revision
//
// Revision 1.8  1994/01/11  00:37:39  craigs
// Removed declaration of ResumeSearch
// Added BTreeFile destructor
//
// Revision 1.7  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.6  1994/01/01  16:03:31  craigs
// Removed ResumeSearch
//
// Revision 1.5  1993/12/16  20:14:19  craigs
// Added ResumeSearch feature - not sure if it will or should be used,
// but it's there if I need it!
//
// Revision 1.4  1993/11/24  21:36:05  craigs
// Various changes remove warnings under MSDOS/NT
//     by robertj
//
// Revision 1.3  1993/11/23  22:19:37  craigs
// Btree is now a descendant of MacFile - so extents now work!
//
// Revision 1.2  1993/11/23  20:29:37  craigs
// Changed unsigned long to ULONG, and short to BOOL
//
// Revision 1.1  1993/11/22  22:27:51  craigs
// Initial revision
//
//
//

#ifndef _BTREE_H
#define _BTREE_H

#include "ostream.h"
#include "misc.h"
#include "mac.h"
#include "file.h"

class Filesystem;

/////////////////////////////////////////////////
//
// BTree key record
//   This is not the full structure - just the first byte is always
//   the length of the key
//

class BTreeKey {
  private:
    BYTE  keylen;
  public:
    int GetKeyLen()  { return keylen; }
};


/////////////////////////////////////////////////
//
// BTree leaf record
//    This is not the full structure - just a type to differentiate
//    it from the key record
//

class BTreeLeaf {
};

/////////////////////////////////////////////////
//
// BTree search key
//

class BTreeSearchKey {
  friend class BTreeFile;
  public:
    enum Comparison {
      Deleted,
      LessThan,
      Equals,
      GreaterThan,
    };
    virtual Comparison Compare    (BTreeKey & key, BOOL IsLeaf) = 0;
    inline virtual BOOL OnFound   (BTreeKey & , BTreeLeaf & ) { return TRUE; };
};

//////////////////////////////////////////////
//
//  BtreeFile
//

class BTreeFile : public MacFile {
  public:
    BTreeFile                (Filesystem & theFs, ExtDataRec theExt, FileRef fileId, ULONG eof);
    inline ~BTreeFile        ()   { Close(); }
    inline BOOL Search       (BTreeSearchKey & key) { return SearchNode (header_node.GetRoot(), key); }

  protected:
    Filesystem      * fs;     // file system we are using
    ExtDataRec      * ext;    // ptr to extent record 
    BTreeHeaderNode header_node;  // node header block

  private:
    BOOL SearchNode       (LogBlkNum node_number, BTreeSearchKey & key);
    BOOL SearchLeafNode   (BTreeNode * Node, BTreeSearchKey & key, int firstRecord = 0);
    BOOL SearchIndexNode  (BTreeNode * Node, BTreeSearchKey & key);
};

#endif // _BTREE_H

