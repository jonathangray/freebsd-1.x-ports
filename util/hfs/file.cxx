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
// $Id: file.cxx,v 1.2 1994/05/15 06:36:08 rich Exp $
//

#include <stdio.h>
#include <stdlib.h>

#include <iomanip.h>

#include "misc.h"
#include "fs.h"
#include "file.h"

#define	CR		0x0d
#define	LF		0x0a

#define	MAC_NEWLINE	CR
#define	UNIX_NEWLINE	LF

////////////////////////////////////////////////
//
//  MacFile::MacFile
//      Constructor for MacFile. The only information needed
//      at this point is some way to get to the base file system
//

MacFile::MacFile (Filesystem & theFs)

{
  fs = &theFs;
}

////////////////////////////////////////////////
//
//  MacFile::Open
//      This is the low level open, which requires the first extent
//      record, the physical eof and the file ID. This function will
//      normally only be called by the higher level open call, but it
//      is needed to open the catalog and extent file
//      
//

int MacFile::Open(ULONG theId,
               ExtDataRec theExtent,
               ULONG phys_eof,
                BOOL isDataFork)

{
  id = theId;
  for (int i = 0; i < 6; i++) 
    extent[i] = first_extent[i] = theExtent[i];
  extent_pos = 0;
  eof = phys_eof;
  data_fork = isDataFork;
  mode = 0;
  return 0;
}


////////////////////////////////////////////////
//
//  MacFile::Open
//      This is the high level data fork open, which takes a FileBuffer
//      as used by the catalog functions. 
//

int MacFile::Open (FileBuffer & fb, UINT flags)

{
  if ((flags & ResourceFork) == 0)
    Open(fb.id, fb.dataext, fb.datasize, TRUE);
  else
    Open(fb.id, fb.rsrcext, fb.rsrcsize, FALSE);
  mode = flags;

  return 0;
}

////////////////////////////////////////////////
//
//  CheckExtent
//      Used to see if the extent record contains the required physical
//      block number
//

static BOOL CheckExtent(AllBlkNum & phys_block,
                        AllBlkNum *extent,
                        AllBlkNum start_block)


{
  for (int i = 0; i < 6; i+= 2) {

    // if both the start and length of the extent is zero, 
    // then this is the end of the file
    if ((extent[i] | extent[i+1]) == 0)
      break;

    // check to see if the desired block is in this extent
    // if it is, calculate the physical block number and return TRUE
    if (start_block < (extent[i+1]))  {
      phys_block = extent[i] + start_block;
      return TRUE;
    }

    // if not, adjust the start_block and try the next one
    start_block -= extent[i+1];
  }

  return FALSE;
}


////////////////////////////////////////////////
//
//  MacFile::Read
//      Used to perform a read from a specified logical block number
//      within a file. Before searching the extents file, check to
//      see if the block we require is in the currently bufferered
//      extent record
//

int MacFile::Read (LogBlkNum block_number, BYTE * buffer)

{
  AllBlkNum phys_block;

  //////////////////////////////////////////////
  //
  // WARNING: this code assumes the allocation and logical
  // block size are the same. This is true for floppy disks
  // but cannot be true for disks over 32M in size
  //

  //
  // if the currently buffered extent record doesn't have the block
  // number we want, then start checking elsewhere
  //
  if ((block_number < extent_pos) || !CheckExtent(phys_block, extent, (AllBlkNum)(block_number-extent_pos))) {
    if (!fs->FindExtent(extent, &extent_pos, id, block_number, data_fork)) {
      cerr << "warning: seek to block "
           << phys_block
           << " for file "
           << id
           << "is past end of file\n";
      return FALSE;
    }
    // this extent should contain the block we want
    if (!CheckExtent(phys_block, extent, (AllBlkNum)(block_number-extent_pos))) {
      cerr << "error: extent does not contain block???\n";
      exit (1);
    }
  }

  // read the physical block
  fs->ReadBlock(phys_block, buffer);

  return 0;
}

////////////////////////////////////////////////
//
//  NewLinesToOS
//      Convert the Macintosh newlines in the specified buffer to the current OS newlines
//

static UINT NewLinesToOS (BYTE * out, BYTE * in, size_t size)

{
  BYTE * p = out;

  for (size_t i = 0; i < size; i++) {
    if (*in == MAC_NEWLINE) {
#if defined(_MSDOS) || defined(WIN32)
      *p++ = CR;
      *p++ = LF;
#else
      *p++ = UNIX_NEWLINE;
#endif
      in++;
    } else 
      *p++ = *in++;
  }

  return p - out;
}


////////////////////////////////////////////////
//
//  MacFile::CopyToOS
//      Copy the HFS file to the OS file specified
//

void MacFile::CopyToOS (FILE * output_file)

{
  BYTE buffer [512];
  BYTE nl_buffer [1024];
  UINT nl_len;
  ULONG remaining;

  // read the correct number of blocks, and truncate the last one correctly
  remaining = eof;
  for (LogBlkNum i = 0; remaining > 0 ; i++) {
    Read (i, buffer);
    size_t size = (size_t)MIN(remaining, 512);
    if (mode & HexDump)
      dump(buffer, size);
    else if (mode & ConvertNewLines) {
      nl_len = NewLinesToOS(nl_buffer, buffer, size);
      fwrite(nl_buffer, nl_len, 1, output_file);
    } else
      fwrite(buffer, size, 1, output_file);
    remaining -= size;
  }
}
