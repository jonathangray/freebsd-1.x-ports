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
// $Id: volume.h,v 1.2 1994/05/15 06:19:36 rich Exp $
// $Log: volume.h,v $
// Revision 1.2  1994/05/15 06:19:36  rich
// hfs for FreeBSD.
//
// hfs allows files to be copied off a Macintosh HFS disk on a
// non-Macintosh computer.  Written by Craig Southeren,
// geoffw@extro.ucc.su.oz.au.
//
// Revision 1.1.1.1  1994/05/15  05:44:54  rich
// hfs 0.3 from sunsite
//
// Revision 1.5  1994/01/11  00:45:48  craigs
// Added InvalidateCache call so Filesystem::Unmount will work properly
//
// Revision 1.4  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.3  1993/11/24  21:36:05  craigs
// Various changes remove warnings under MSDOS/NT
//     by robertj
//
// Revision 1.2  1993/11/23  20:18:16  craigs
// Added MSDOS/Windows compatibility
//     by robertj
//
// Revision 1.1  1993/11/22  22:27:51  craigs
// Initial revision
//
//
//

#ifndef _VOLUME_H
#define _VOLUME_H

#if defined(WIN32)
#include <stdarg.h>
#include <windef.h>
#include <winbase.h>
#include <winioctl.h>
#endif

#include "mac.h"

//////////////////////////////////////////////
//
//  Volume
//  A volume is a single disk/file/partition that contains a 
//      Macintosh filesystem 
//

class Volume {
  public:
    enum { LogicalBlockShift = 9 };
      // number of bits to shift a logical block number right
      // to make a byte offset

    enum { LogicalBlockSize = 512 };
      // size of a logical block in bytes

    int Mount();
      // mount the volume

    virtual int Read (ULONG logical_block, void * buffer) = 0;
      // read a logical block

    virtual int Write (ULONG logical_block, void * buffer) = 0;
      // write a logical block

    virtual inline void InvalidateCache()  { }
      // invalidate the cache - used when the disk is refreshed

    MDB mdb;       
      // copy of MDB for disk
};


//////////////////////////////////////////////
//
//  FileVolume
//  A Macintosh filesystem mounted inside a host system file
//

class FileVolume : public Volume {
  public:
    FileVolume::FileVolume(const char * device_name);

    int Read  (ULONG logical_block, void * buffer);

    int Write (ULONG logical_block, void * buffer);

  private:
    int fd;
};


//////////////////////////////////////////////
//
//  DeviceVolume
//  A Macintosh filesystem mounted on a device
//

class DeviceVolume : public Volume {
  public:
    DeviceVolume::DeviceVolume(const char * device_name);

    BOOL Read  (ULONG logical_block, void * buffer);

    BOOL Write (ULONG logical_block, void * buffer);

    void InvalidateCache();

    static BOOL IsDeviceName(const char * devname);

  private:
    enum { CacheSize = 20 };
    struct Cache {
      ULONG block;
      ULONG lastUsed;
      char  buffer[LogicalBlockSize];
    } cache[CacheSize];
    ULONG countLRU;
    
#if defined(_MSDOS)
    UINT driveNum, numHeads, numTracks, numSectors;
#elif defined(WIN32)
    HANDLE hDrive;
#else
    int fd;
#endif
};

#endif 

