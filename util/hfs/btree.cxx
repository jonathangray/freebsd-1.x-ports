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
// $Id: btree.cxx,v 1.2 1994/05/15 06:19:11 rich Exp $
// $Log: btree.cxx,v $
// Revision 1.2  1994/05/15 06:19:11  rich
// hfs for FreeBSD.
//
// hfs allows files to be copied off a Macintosh HFS disk on a
// non-Macintosh computer.  Written by Craig Southeren,
// geoffw@extro.ucc.su.oz.au.
//
// Revision 1.1.1.1  1994/05/15  05:44:52  rich
// hfs 0.3 from sunsite
//
// Revision 1.10  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.9  1994/01/01  16:02:13  craigs
// Removed ResumeSearch
// Corrected problem with searching index nodes starting with
// the search key
//
// Revision 1.8  1993/12/23  22:50:51  craigs
// Removed warning about uninitialised variable
//
// Revision 1.7  1993/12/23  15:13:48  craigs
// Fixed warning regarding duplicate default initialiser under DOS/NT
//
// Revision 1.6  1993/12/16  20:14:19  craigs
// Added ResumeSearch feature - not sure if it will or should be used,
// but it's there if I need it!
//
// Revision 1.5  1993/11/25  22:01:34  craigs
// Fixed problems with not traversing link lists of index nodes, removed
// requirement for IsLeaf argument to OnFound function of search keys (
// (although kept it because it's useful for debugging!) and fixed handling
// for rightmost nodes of each tree level
//
// Revision 1.4  1993/11/24  21:31:58  craigs
// Various changes for clean compile under MSDOS/NT
//
// Revision 1.3  1993/11/23  22:19:37  craigs
// Btree is now a descendant of MacFile - so extents now work!
//
// Revision 1.2  1993/11/23  20:29:37  craigs
// Changed unsigned long to ULONG, and short to BOOL
//
// Revision 1.1  1993/11/22  22:25:55  craigs
// Initial revision
//
//
//

#include <iostream.h>
#include <stdlib.h>

#include "btree.h"
#include "fs.h"
#include "misc.h"

//////////////////////////////////////
//
// BTreeFile::BTreeFile
//

BTreeFile::BTreeFile (Filesystem & theFs,
                      ExtDataRec   theExt,
                      FileRef      fileId,
                      ULONG        eof)
  : MacFile(theFs)

{
  //
  // open ourself as a file
  //
  Open(fileId, theExt, eof);

  //
  // read the header node
  //
  Read (0, (BYTE *)&header_node);

  //
  // check the type of the header node
  //
  if (header_node.GetType() != BTreeNode::HdrNode) {
    cout << "internal error: header record has incorrect node type\n";
    exit (0);
  }
}


//////////////////////////////////////
//
// BTreeFile::SearchNode
//

BOOL BTreeFile::SearchNode (LogBlkNum node_number,
                     BTreeSearchKey & searchKey)

{
  BTreeNode node;

  //
  // read in the node
  //
  Read (node_number, (BYTE *)&node);

  //
  // depending upon the node type, do something
  //
  switch (node.GetType()) {
    case BTreeNode::IndxNode:
      return SearchIndexNode (&node, searchKey);

    case BTreeNode::LeafNode:
      return SearchLeafNode (&node, searchKey);

    default:
      break;
  }

  cout << "error: logical block " 
       << node_number
       << " contains unknown node type "
       << (int)node.GetType()
       << "\n";
  dump(&node, 512);
  exit(1);
  return FALSE;
}


//////////////////////////////////////
//
// BTreeFile::SearchIndexNode
//

BOOL BTreeFile::SearchIndexNode (BTreeNode * node, BTreeSearchKey & searchKey)

{
  int i;
  BTreeKey *key, *last_key = NULL;
  ULONG child_node;
  ULONG last_child_node = 0;
  ULONG next_node;

#ifdef DUMP_INDEX_NODES
cout << "\n\nIndex Node:\n";
dump (node, 512);
#endif

  //
  // step through the index records
  //

  for (;;) {
    //
    // step through the index records
    //
    for (i = 0; i < node->GetNRecs(); i++) {
      key = (BTreeKey *)node->GetRecord(i);
      if (key->GetKeyLen() > 0) {
        child_node = (ULONG)*(MULONG *)(node->GetRecord(i) + key->GetKeyLen() + 1);

        switch (searchKey.Compare(*key, FALSE)) {

          case BTreeSearchKey::Equals:
            return SearchNode(child_node, searchKey);

          case BTreeSearchKey::GreaterThan:
            if (last_key != NULL) 
              return SearchNode(last_child_node, searchKey);
            else
              return FALSE;

          case BTreeSearchKey::LessThan:
            last_key = key;
            last_child_node = child_node;
            break;

          default:
            break;
        }
      }
    }

    //
    // see if another index record. If not, move the last node
    //
    if ((next_node = node->GetFLink()) == 0) {
      if (last_key != NULL) 
        return SearchNode(last_child_node, searchKey);
      else
        return FALSE;
    }

    //
    // read the node in
    //
    Read (next_node, (BYTE *)node);

    //
    // if the new node is not an index node, this is an error
    //
    if (node->GetType() != BTreeNode::IndxNode) {
      cerr << "error: found unexpected node type ("
           << (int)node->GetType()
           << " on forward link from index node\n";
      return FALSE;
    } 
  } 
  return FALSE;
}


//////////////////////////////////////
//
// BTreeFile::SearchLeafNode
//

BOOL BTreeFile::SearchLeafNode (BTreeNode * node,
                           BTreeSearchKey & searchKey,
                                        int firstRecord)

{
  int i;
  BTreeKey * key;
  BTreeLeaf * leaf;
  ULONG next_node;

#ifdef DUMP_LEAF_NODES
cout << "\n\nIndex Node:\n";
dump (node, 512);
#endif

  //
  // step through the leaf records
  //

  for (;;) {
    //
    // step through the key records
    //
    for (i = firstRecord; i < node->GetNRecs(); i++) {
      key  = (BTreeKey *)  node->GetRecord(i);
      if (key->GetKeyLen() > 0) {
        switch (searchKey.Compare(*key, TRUE)) {
          case BTreeSearchKey::Equals:
            leaf = (BTreeLeaf *) node->GetRecord(i, key->GetKeyLen() + 1, 2);
            if (searchKey.OnFound(*key, *leaf)) 
              return TRUE;
            break;

          case BTreeSearchKey::GreaterThan:
            return FALSE;

          default:
            break;
        }
      }
    }

    //
    // see if another leaf record
    //
    if ((next_node = node->GetFLink()) == 0)
      return FALSE;

    //
    // read the node in
    //
    Read (next_node, (BYTE *)node);

    //
    // if the new node is not a leaf node, this is an error
    //
    if (node->GetType() != BTreeNode::LeafNode) {
      cerr << "error: found unexpected node type ("
           << (int)node->GetType()
           << " on forward link from leaf node\n";
      return FALSE;
    } 
  } 
}
