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
// $Id: status.cxx,v 1.1 1994/05/15 05:44:53 rich Exp $
// $Log: status.cxx,v $
// Revision 1.1  1994/05/15 05:44:53  rich
// Initial revision
//
// Revision 1.5  1994/01/11  00:45:01  craigs
// Changed status filename parsing to use DIR_SEP macro
//
// Revision 1.4  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.3  1993/11/24  21:36:05  craigs
// Various changes remove warnings under MSDOS/NT
//     by robertj
//
// Revision 1.2  1993/11/23  20:16:56  craigs
// Added MSDOS/Windows compatibility
//    by robertj
//
// Revision 1.1  1993/11/22  22:25:55  craigs
// Initial revision
//
//
//

#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if !defined(_MSDOS) && !defined(WIN32)
#include <unistd.h>
#include <pwd.h>
#endif

#include "misc.h"
#include "status.h"
#include "config.h"


//////////////////////////////////////////////
//
//  get the pathname to the user's status file
//

static const char * GetPath (const char * defaultPath)

{
#if !defined(_MSDOS) && !defined(WIN32)
  int uid;
  struct passwd *pw = NULL;
#endif
  const char *ptr;

  //
  // following steps used to find the user's home directory:
  //   1. look for explicit HOME environment variable
  //   2. get user name from USER environment variable or using uid
  //      and use password file to find home directory
  //   3. if all of the above fails, use the current directory
  //
  if ((ptr = getenv ("STAT_FILE")) != NULL)
    return ptr;

#if defined(_MSDOS) || defined(WIN32)
  if ((ptr = getenv ("TEMP")) == NULL)
    ptr = defaultPath;
#else
  if ((ptr = getenv ("HOME")) == NULL) {
    if ((ptr = getenv ("USER")) != NULL) {
      pw = getpwnam (ptr);
    } else {
      uid = getuid ();
      pw = getpwuid (uid);
    }
    if (pw == NULL) {
      ptr = ".";
    }
  }
#endif

  //
  // copy the filename to local storage, so we can play with it
  //
  char * stat_file = new char[strlen(ptr) + 1 + strlen(STAT_FILE) + 1];
  strcpy (stat_file, ptr);

  //
  // append directory separator
  //
  char * last_char = &stat_file[strlen(stat_file)-1];
  if (strlen(stat_file) > 0 && *last_char != DIR_SEP) {
    *++last_char = DIR_SEP;
    *++last_char = '\0';
  }

  //
  // append name of status file
  //
  strcat(stat_file, STAT_FILE);

  return stat_file;
}


//////////////////////////////////////////////
//
//  create the statusfile object
//

StatusFile::StatusFile (const char * defaultPath)

{
  filename = GetPath(defaultPath);
  volume_name = NULL;
  dir = 0;
  dirty = FALSE;
}


//////////////////////////////////////////////
//
//  read the status file from the user's home directory
//

void StatusFile::Read ()

{
  FILE * file;
  char line[1024];
  int l;

  //
  // open the status file
  //
  if ((file = fopen (filename, "r")) == NULL)
    return;

  //
  // first line gives volume name
  //
  if (fgets (line, 1023, file) != NULL) {
    l = strlen(line);
    if ((l > 0) && (line[l-1] == '\n'))
      line[l-1] = '\0';
    volume_name = new char[strlen(line)+1];
    strcpy(volume_name, line);

    //
    // second line gives current directory number
    //
    if (fgets (line, 1023, file) != NULL) {
      l = strlen(line);
      if ((l > 0) && (line[l-1] == '\n'))
        line[l-1] = '\0';
      if ((dir = atoi(line)) <= 0)
        dir = 0;
    }
  }

  // 
  // close the file
  //
  fclose(file);
}


void StatusFile::Write (BOOL force)

{
  FILE * file;

  if (!force && !dirty)
    return;

  if ((file = fopen(filename, "w")) == NULL) 
    return;

  fprintf (file, "%s\n%li", volume_name, dir);

  fclose(file);
}

