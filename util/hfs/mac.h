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
// $Id: mac.h,v 1.3 1994/05/15 06:36:14 rich Exp $
//

#ifndef _MAC_H
#define _MAC_H

#include <memory.h>

#include "endian.h"

////////////////////////////////////////////////////////
//
// define a couple of types to aid in error checking
//
////////////////////////////////////////////////////////

typedef UINT  AllBlkNum;    // at most 655536 allocation blocks
          // in a volume

typedef ULONG LogBlkNum;    // can be lots of logical blocks
          // in a volume

typedef ULONG FileRef;    // can be lots of allocation blocks
          // in a volume

/////////////////////////////////////////////////
//
// size of a logical block, and how many bits to shift
// a block number to get a byte offset
//

#define M_LBLOCK_SIZE   512
#define M_LBLOCK_SIZE_SHIFT 9


/////////////////////////////////////////////////
//
// logical block number of Master Directory Block
//

#define M_MASTER_MDB_LBLOCK 2


/////////////////////////////////////////////////
//
// signature values for MFS and HFS volumes
//

#define M_HFS_SIG   0x4244
#define M_MFS_SIG   0xd2d7


/////////////////////////////////////////////////
//
// catalog node IDs
//

#define M_CNID_ROOT_PARENT  1
#define M_CNID_ROOT   2
#define M_CNID_EXTENTS    3
#define M_CNID_CATALOG    4
#define M_CNID_BAD    5

/////////////////////////////////////////////////
//
// Extent record
//

typedef MUINT ExtDataRec[6];


/////////////////////////////////////////////////
//
// Master Directory Block
//

class MDB {
  public:
  MUINT       drSigWord;  // volume signature 
  MULONG      drCrDate;   // volume creation date 
  MULONG      drLsMod;    // date and time of last modification 
  MUINT       drAtrb;   // volume attributes 
  MUINT       drNmFls;    // number of files in root directory 
  MUINT       drVBMSt;    // first block of volume bitmap 
  MUINT       drAllocPtr; // start of next allocation search 
  MUINT       drNmAlBlks; // number of allocation blocks in volume 
  MULONG      drAlBlkSiz; // size of allocation blocks in bytes 
  MULONG      drClpSiz;   // default clump size 
  MUINT       drAlBlSt;   // first allocation block in volume 
  MULONG      drNxtCNID;  // next unused catalog node id 
  MUINT       drFreeBks;  // number of free blocks 
  BYTE        drVN[28];   // volume name 
  MULONG      drVolBkUp;        // time and date of last backup 
  MUINT       drVSeqNum;        // volume backup sequence number 
  MULONG      drWrCnt;          // volume write count 
  MULONG      drXTClpSiz; // clump size for extents overflow file 
  MULONG      drCTClpSiz; // clump size for catalog file 
  MUINT       drNmRtDirs;       // number of directories in root directory 
  MULONG      drFilCnt;         // number of files in volume 
  MULONG      drDirCnt;         // number of directories in volume 
  MULONG      drFndrInfo[8];    // finder information 
  MUINT       drVCSize;         // size (in blocks) of volume cache 
  MUINT       drVBMCSize;       // size (in blocks) of volume bitmap cache 
  MUINT       drCtlCSize;       // size (in blocks) of common volume cache 
  MULONG      drXtFlSize;       // size of extents overflow file 
  ExtDataRec  drXtExtRec;       // extent record for extents overflow file 
  MULONG      drCtFlSize;       // size of catalog file 
  ExtDataRec  drCtExtRec;       // extent record for catalog file 
  BYTE        pad [512-162];    // pad this structure out to 512 bytes
};


/////////////////////////////////////////////////
//
// Node Buffer
//

#define ALIGN(n,a)  (((n)+(a)-1)&(~(a-1)))

class BTreeNode {

  public:
    enum NodeType {
      IndxNode  = 0x00, // index node
      HdrNode = 0x01, // header node
      MapNode = 0x02, // map node
      LeafNode  = 0xff  // leaf node
    };

    inline NodeType  GetType()        { return (NodeType)node.descriptor.ndType; }
    inline int       GetNRecs()       { return node.descriptor.ndNRecs; }
    inline LogBlkNum GetFLink ()      { return node.descriptor.ndFLink; }
    inline int       GetOffset(int n) { return (int)node.offsets[255-n]; }
    inline BYTE *    GetRecord(int n) { return (BYTE *)(node.data + (int)node.offsets[255-n]); }
    inline BYTE *    GetRecord(int n, int o, int align) { return (BYTE *)(node.data + ALIGN((int)node.offsets[255-n] + o, align)); }

  protected:
    union {
      //
      // node descriptor
      //
      struct {
        MULONG  ndFLink;  // forward link 
        MULONG  ndBLink;  // backward link 
        BYTE  ndType;   // node type 
        SBYTE ndNHeight;  // node level 
        MUINT ndNRecs;  // number of nodes in record 
        MUINT ndResv2;  // reserved 
      } descriptor;

      //
      // BTree header node
      struct {
        BYTE    pad[0xe];
        MUINT bthDepth; // current depth of tree
        MULONG  bthRoot;  // number of root node 
        MULONG  bthNRecs; // number of leaf nodes in tree 
        MULONG  bthFNode; // number of first leaf node 
        MULONG  bthLNode; // number of last leaf node
        MUINT bthNodeSize;  // size of a node 
        MUINT bthKeyLen;  // maximum length of a key 
        MULONG  bthNNodes;  // total number of nodes in the tree 
        MULONG  bthFree;  // total number of free nodes 
        BYTE    bthResv[76];  // reserved
      } header;

      //
      // raw data
      //
      BYTE data [512];    // raw data

      //
      // easy way to get at offsets data
      //
      MUINT     offsets [256];  // integer values, used from end of node

    } node;
};

class BTreeHeaderNode : public BTreeNode {
  public:
    inline LogBlkNum GetRoot() { return node.header.bthRoot; }
};

/////////////////////////////////////////////////
//
// Catalog key
//

class CatKeyRec {
  private:
    BYTE  ckrKeyLen;  // key length
    BYTE  ckrResrv1;  // reserved
    MULONG  ckrParID; // parent directory ID
    BYTE  ckrCName[32]; // catalog node name
    MULONG      ckrNode;  // node number

  public:
    inline FileRef   GetParID  () { return ckrParID; }
    inline BYTE *    GetCName  () { return ckrCName; }
    inline LogBlkNum GetNode   () { return ckrNode; }
    inline int       GetKeyLen () { return ckrKeyLen; }
};

//////////////////////////////////////////
//
// Finder data types
//

class Point {
  MUINT x;
  MUINT y;
};

class Rect {
  Point  pos;
  Point  size;
};

class DInfo {
  public:
  Rect     frRect;    // folder's rectangle
  MUINT    frFlags;   // flags
  Point    frLocation;          // folder's location
  MUINT    frView;              // folder's view
};

class FInfo {
  public:
  BYTE        fdType[4];        // file'type
  BYTE        fdCreator[4];     // file's creator
  MUINT       fdFlags;          // flags
  Point       fdLocation;       // file's location
  MUINT       fdFldr;           // file's window

  enum { fInvisible = 16384 };
};

class FXInfo {
  public:
  MUINT       fdIconID;   // icon ID
  MUINT       fdUnused[4];      // reserved
  MUINT       fdComment;        // comment ID
  MULONG      fdPutAway;        // home directory ID
};

class DXInfo {
  public:
  Point       frScroll;         // scroll position
  MULONG      frOpenChain;      // directory ID chain of open folders
  MUINT       frUnused;         // reserved
  MUINT       frComment;        // commend ID
  MULONG      frPutAway;        // directory ID
};

/////////////////////////////////////////////////
//
// Catalog Data types
//

enum {
  DirRec   = 1,   /* directory record */
  FilRec   = 2,   /* file record */
  ThdRec   = 3,   /* directory thread record */
  FThdRec  = 4    /* file thread record */
};


/////////////////////////////////////////////////
//
// Catalog Data record
//

class CatDataRec {
  private:

  BYTE    cdrType;  /* record type */
  BYTE    cdrResrv2;  /* reserved */

  union {
    struct {
      MUINT dirFlags; /* directory flags */
      MUINT dirVal;   /* directory valence */
      MULONG  dirDirID; /* directory ID */
      MULONG  dirCrDat; /* date and time of creation */
      MULONG  dirMdDat; /* date and time of last modification */
      MULONG  dirBkDat; /* date and time of last backup */
      DInfo dirUsrInfo; /* Finder information */
      DXInfo  dirFndrInfo;  /* additional finder information */
      MULONG  dirResrv[4];  /* reserved */
    } cdrDirRec;

    struct {
      BYTE  filFlags; /* file flags */
      BYTE  filTyp;   /* file type */
      FInfo filUsrWds;  /* Finder information */
      MULONG  filFlNum; /* file number */
      MUINT filStBlk; /* first allocation block of data fork */
      MULONG    filLgLen; /* logical EOF of data fork */
      MULONG  filPyLen; /* physical EOF of data fork */
      MUINT filRStBlk;  /* first allocation block of resource fork */
      MULONG    filRLgLen;  /* logical EOF of resource fork */
      MULONG  filRPyLen;  /* physical EOF of resource fork */
      MULONG  filCrDat; /* date and time of creation */
      MULONG  filMdDat; /* date and time of last modification */
      MULONG  filBkDat; /* date and time of last backup */
      FXInfo    filFndrInfo;    /* additional Finder information */
      MUINT     fileClpSize;  /* file clump size */
      ExtDataRec filExtRec;     /* first data fork extent record */
      ExtDataRec filRExtRec;    /* first resource fork extent record */
      MULONG  filResrv; /* reserved */
    } cdrFilRec;

    struct {
      MULONG  thdResrv[2];  /* reserved */
      MULONG  thdParID; /* parent ID  for this directory */
      BYTE  thdCName[32]; /* name of this directory */
    } cdrThdRec;

    struct {
      MULONG  fthdResrv[2]; /* reserved */
      MULONG  fthdParID;  /* parent ID for this file */
      BYTE  fthdCName[32];  /* name of this file */
    } cdrFThdRec;
  } rec;

  public:
    inline int GetType()   { return cdrType; }

    inline  BOOL IsFile ()       { return cdrType == FilRec; }
    inline unsigned long GetId () { return IsFile() ?
          (unsigned long)rec.cdrFilRec.filFlNum : (unsigned long)rec.cdrDirRec.dirDirID; }

    inline unsigned long GetDate ()
      { return IsFile() ?
          (-2082844800L + (unsigned long)rec.cdrFilRec.filCrDat) :
          (-2082844800L + (unsigned long)rec.cdrDirRec.dirCrDat);
      }

    void GetFileData (char * creator,
                      char * filetype,
                      unsigned long * dsize,
                      ExtDataRec dext,
                      unsigned long * rsize,
                      ExtDataRec rext,
		      UINT *flags);

    inline  FileRef  GetThdParID ()  { return rec.cdrFThdRec.fthdParID; }
    inline  BYTE *   GetThdCName ()  { return rec.cdrFThdRec.fthdCName; }

    inline int GetSize();
      
};


/////////////////////////////////////////////////
//
// Extent key
//

class ExtKeyRec {
  private:
    BYTE  xkrKeyLen;  // key length
    BYTE  xkrFkType;  // fork type
    MULONG  xkrFNum;  // file number
    MUINT xkrFABN;        // starting file allocation block

  public:
    inline int        GetKeyLen   () { return xkrKeyLen; }
    inline int        GetForkType () { return xkrFkType; }
    inline FileRef    GetID       () { return xkrFNum; }
    inline AllBlkNum  GetFABN     () { return xkrFABN; }
};


#endif
