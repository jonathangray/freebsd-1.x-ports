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
// $Id: config.h,v 1.3 1994/05/15 06:36:01 rich Exp $
//

#ifndef _CONFIG_H
#define _CONFIG_H

//
// environment variable used to set different disk device
//
#define DISK_DEVICE_ENV   "HFS_DEVICE"

//
// default disk device if not set in Makefile
//
#ifndef DEFAULT_DISK_DEVICE
#define DEFAULT_DISK_DEVICE "/dev/rfd0a"
#endif

//
// name of file stored in user's home directory which stores
// the volume name and current directory
//
#ifndef STAT_FILE
#define STAT_FILE   ".macdir"
#endif

#if defined(_MSDOS) || defined(WIN32)
#define STRNICMP strnicmp
#else
#define STRNICMP strncasecmp
#endif



#endif
