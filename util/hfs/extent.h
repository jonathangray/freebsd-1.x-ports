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
// $Id: extent.h,v 1.3 1994/05/15 06:36:07 rich Exp $
//

#ifndef _EXTENT_H
#define _EXTENT_H

#include "btree.h"

//////////////////////////////////////////////
//
//  ExtentFile
//

class ExtentFile : public BTreeFile {
  public:
    ExtentFile (Filesystem & theFs);

    BOOL FindExtent (UINT * extent,
                AllBlkNum * start_block,
                    FileRef id,
                  AllBlkNum block_num,
                       BOOL IsData);
};

#endif 

