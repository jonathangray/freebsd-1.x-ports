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
// $Id: file.h,v 1.3 1994/05/15 06:36:09 rich Exp $
//

#ifndef _FILE_H
#define _FILE_H

#include <stdio.h>

class Filesystem;
class FileBuffer;

class MacFile {
  public:
    enum {
      HexDump         = 0x0001,
      ConvertNewLines = 0x0002,
      ResourceFork    = 0x0004
    };

    MacFile (Filesystem & filesystem);

    int Open(unsigned long theId,
		 ExtDataRec theExtent,
		 unsigned long phys_eof,
                     BOOL data_fork = TRUE);

    int Open (FileBuffer & fb, UINT mode = 0);

    int Read (LogBlkNum block_number, BYTE * buffer);

    inline int Close() { return 0; }

    void CopyToOS(FILE * output_file);

  private:
    Filesystem * fs;
    
    FileRef id;                 // ID of file
    AllBlkNum  first_extent[6]; // copy of first extent for file
    ULONG eof;                  // logical size of file

    AllBlkNum extent[6];        // copy of last extent record used
    AllBlkNum extent_pos;       // first block of this extent
    BOOL data_fork;             // true if data fork
    UINT mode;                  // various mode flags
};

#endif
