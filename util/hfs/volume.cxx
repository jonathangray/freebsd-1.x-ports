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
// $Id: volume.cxx,v 1.1 1994/05/15 05:44:53 rich Exp $
// $Log: volume.cxx,v $
// Revision 1.1  1994/05/15 05:44:53  rich
// Initial revision
//
// Revision 1.7  1994/01/11  02:39:55  craigs
// Added newline to end of cannot open file message
//
// Revision 1.6  1994/01/11  00:45:48  craigs
// Added InvalidateCache call so Filesystem::Unmount will work properly
//
// Revision 1.5  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.4  1993/12/23  15:19:30  craigs
// Changed to accept A: and B: as device names under Unix
//
// Revision 1.3  1993/11/24  21:36:05  craigs
// Various changes remove warnings under MSDOS/NT
//     by robertj
//
// Revision 1.2  1993/11/23  20:18:51  craigs
// Added MSDOS/Windows compatibility
//     by robertj
//
// Revision 1.1  1993/11/22  22:25:55  craigs
// Initial revision
//
//
//

#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>
#if defined(_MSDOS) || defined(WIN32)
#include <io.h>
#else
#include <unistd.h>
#include <sys/stat.h>
#endif

#include "volume.h"
#include "error.h"
#include "misc.h"


//////////////////////////////////////
//
// Volume::Mount
//

int Volume::Mount ()

{
  // read the MDB
  if (!Read(M_MASTER_MDB_LBLOCK, &mdb))
    return E_FAULTYMDB;

  // check the signature
  if (mdb.drSigWord != M_HFS_SIG) 
    return E_NOTHFS;

  // all is OK
  return E_OK;
}


//////////////////////////////////////
//
// FileVolume::FileVolume
//

FileVolume::FileVolume (const char * devname)

{
  if ((fd = open (devname, O_RDWR)) < 0) {
    cerr << "error: cannot open the file \""
         << devname
         << "\" for use as a Macintosh volume\n";
    exit (1);
  }
}

//////////////////////////////////////
//
// FileVolume::Read
//

int FileVolume::Read (ULONG logical_block, void * buffer) 

{
  if (lseek (fd, logical_block << LogicalBlockShift, 0) < 0)
    return 0;
  return read (fd, buffer, LogicalBlockSize) == LogicalBlockSize;
}

//////////////////////////////////////
//
// FileVolume::Write
//

int FileVolume::Write (ULONG , void * )

{
//  if (lseek (fd, logical_block << LogicalBlockShift, 0) < 0)
//    return 0;
  return 0;
}


//////////////////////////////////////
//
// DeviceVolume::DeviceVolume
//

DeviceVolume::DeviceVolume (const char * devname)

{
  memset(cache, 0, sizeof(cache));
  countLRU = 1;

#if defined(_MSDOS)

  BYTE drive = (BYTE)(devname[0] - 'A');
  BYTE maxHead;
  BYTE maxSector;
  short maxTrack;
  
  _asm {
    mov  ah,8           // Code to get the disk parameters
    mov  dl,drive
    int  0x13
    jc   invalid_drive  // Disk does not exist
    mov  maxHead,dh     // Get the disk parameters to where C can get at them
    mov  ah,cl
    and  cl,0x3f
    mov  maxSector,cl
    mov  al,5
    shr  ah,cl
    mov  al,ch
    mov  maxTrack,ax
  }
  driveNum = drive;       // Put disk parameters into the object variables
  numHeads = maxHead + 1;
  numTracks = maxTrack + 1;
  numSectors = maxSector;
  return;

invalid_drive:

#elif defined(WIN32)

  char drivename[20];
  if (isalpha(devname[0])) {
    sprintf(drivename, "\\\\.\\%s", devname);
    devname = drivename;
  }
  hDrive = CreateFile(devname,
                  GENERIC_READ|GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (hDrive != INVALID_HANDLE_VALUE) {
    DWORD c;
    if (DeviceIoControl(hDrive, FSCTL_LOCK_VOLUME, NULL, 0, NULL, 0, &c, NULL))
      return;
  }
#else

  if ((fd = open ( (tolower(devname[0]) == 'a') ? "/dev/fd0H1440" : "/dev/fd1H1440", O_RDWR)) > 0)
    return;

#endif

  cerr << "error: cannot open the device \""
       << devname
       << "\" for use as a Macintosh volume\n";
  exit (1);
}

//////////////////////////////////////
//
// DeviceVolume::Read
//

BOOL DeviceVolume::Read (ULONG logical_block, void * buffer) 

{
  Cache * c = cache;
  for (int i = 0; i < CacheSize; i++, c++) {
    if (c->lastUsed != 0 && c->block == logical_block) {
      memcpy(buffer, c->buffer, LogicalBlockSize);
      c->lastUsed = countLRU++;
      return TRUE;
    }
  }

#if defined(_MSDOS)

  BYTE drive = (BYTE)driveNum;
  BYTE sector = (BYTE)(logical_block%numSectors + 1);
  BYTE head = (BYTE)((logical_block/numSectors)%numHeads);
  UINT track = (UINT)(logical_block/numSectors/numHeads);
  if (track >= numTracks) {
    cerr << "error: logical block number too large\n";
    return FALSE;
  }
  void __far * _buffer = buffer;

  _asm {
    mov  ah,2         // function code
    mov  al,1         // count of sectors
    les  bx,_buffer   // address to read into
    mov  dx,track     // track number
    mov  ch,dl
    mov  cl,5
    shl  dh,5
    mov  cl,sector    // sector number
    or   cl,dh
    mov  dh,head      // head number
    mov  dl,drive     // drive number
    int  0x13
    jnc  read_ok
  }
  return FALSE;

read_ok:

#elif defined(WIN32)

  if (SetFilePointer(hDrive,
           logical_block << LogicalBlockShift, NULL, FILE_BEGIN) == 0xffffffff)
      return FALSE;
  DWORD bytesRead;
  if (!ReadFile(hDrive, buffer, LogicalBlockSize, &bytesRead, NULL))
    return FALSE;

#else

  if (lseek (fd, logical_block << LogicalBlockShift, 0) < 0)
    return FALSE;
  if (read (fd, buffer, LogicalBlockSize) != LogicalBlockSize)
    return FALSE;

#endif

  Cache * cLRU = c = cache;
  for (i = 1; i < CacheSize; i++) {
    c++;
    if (cLRU->lastUsed > c->lastUsed)
      cLRU = c;
  }
  cLRU->block = logical_block;
  cLRU->lastUsed = countLRU++;
  memcpy(cLRU->buffer, buffer, LogicalBlockSize);

  return TRUE;
}

      
//////////////////////////////////////
//
// DeviceVolume::Write
//

int DeviceVolume::Write (ULONG , void * )

{
  return 0;
}



//////////////////////////////////////
//
// DeviceVolume::InvalidateCache
//

void DeviceVolume::InvalidateCache()

{
  memset(cache, 0, sizeof(cache));
  countLRU = 1;
}


//////////////////////////////////////
//
// DeviceVolume::IsDeviceName
//

BOOL DeviceVolume::IsDeviceName(const char * devname)
{
#if defined(_MSDOS)
  return isalpha(devname[0]) && devname[1] == ':' && devname[2] == '\0';
#elif defined(WIN32)
  if (devname[0] == '\\' && devname[1] == '\\') {
    if ((devname = strchr(devname+2, '\\')) == NULL)
      return FALSE;
    devname++;
  }
  return isalpha(devname[0]) && devname[1] == ':' && devname[2] == '\0';
#else
  return devname[1] == ':' && devname[2] == '\0' && strchr("ab", tolower(devname[0])) != NULL;
#endif

//  struct stat s;
//  return stat(devname, &s) == 0 && S_ISBLK(s.st_mode);
}
