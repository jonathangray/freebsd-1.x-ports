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
// $Id: copy.cxx,v 1.3 1994/05/15 06:36:02 rich Exp $
//

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#if defined(__FreeBSD__)
#include <stdio.h>
#include <sys/types.h>
#endif
#include <sys/stat.h>

#if defined(_MSDOS) || defined(WIN32)
#include <io.h>
#else
#include <unistd.h>
#endif

extern "C" {
#include "glob.h"
};
#include "mdir.h"

#include "cmd.h"

static char * MangleName (char * mac_name)

{
  char * new_name = new char [strlen(mac_name) + 1];
  char * p = new_name;

  // remove illegal characters
  while (*mac_name != '\0') {
#if defined(_MSDOS) || defined(WIN32)
    if (strchr("/\\' \"*+,:<>=?()|", *mac_name) != NULL)
      *p = '_';
#else
    if (*mac_name == '/')
      *p = ':';
#endif
    else if (*mac_name <= 0x20 || *mac_name == 0x7f)
      *p = '_';
    else 
      *p = *mac_name;
    mac_name++;
    p++;
  }

  *p = '\0';

  // chop filename to 8.3 if MSDOS or NT
  // Not all NT filesystems need this, but we do it anyway!
  #if defined(_MSDOS) || defined(WIN32)
  if (new_name[0] == '.')
    new_name[0] = '_';

  char * dot_pos = strchr(new_name, '.');
  if (dot_pos == NULL) {
    if (strlen(new_name) > 8)
      new_name[8] = '\0';
  } else {
    if (dot_pos - new_name > 8) {
      new_name[8] = '.';
      for (int i = 0; *(dot_pos+1+i) != '\0'; i++) {
        new_name [9+i] = *(dot_pos+1+i);
        if (new_name[9+i] == '.')
          new_name[9+i] = '_';
      }
      new_name[9+i] = '\0';
      if (strlen(new_name) > 12)
        new_name [12] = '\0';
    }
  }
  #endif

  return new_name;
}

static void CopyFromMacDisk (char * from, char * to, Filesystem & fs, UINT flags, BOOL mangle)

{
  FileBuffer fb;
  char * os_dir;
  char * os_to = NULL;
  BOOL copy = TRUE;

  // find the source file
  if (!fs.ResolvePath(fb, from, status_file->GetDir())) 
    return;

  // if the source file is really a directory, don't copy it
  if (!fb.IsFile()) {
    cout << "omitting directory " << from << "\n";
    return;
  }

  // if there is a destination specified, and it exists, it must be a directory 
  os_dir = ".";
  if (to != NULL)  {
    if (access(to, 0) == 0)
      os_dir = to;
    else {
      os_to = new char [strlen(to) + 1];
      strcpy(os_to, to);
    }
  } 

  // if os_to is NULL at this point, then use os_dir to construct a filename
  if (os_to == NULL) {
    char * to_base;
    int l;
    if (mangle)
      to_base = MangleName(fb.name);
    else
      to_base = strdup(fb.name);
    os_to = new char [(l = strlen(os_dir)) + 1 + strlen(to_base) + 1];
    strcpy(os_to, os_dir);
    if ((l > 0) && (os_to[l-1] != DIR_SEP)) {
      os_to[l]     = DIR_SEP;
      os_to[l+1]   = '\0';
    }
    strcat(os_to, to_base);
  }

  // see if the file we are about to copy to exists
  if (access (os_to, 0) == 0) {
    char buff[3];
    cout << "Overwrite "
         << os_to
         << "(y/n) ? ";
    fgets (buff, 3, stdin);
    if (tolower(buff[0]) != 'y') {
      cout << "Skipping "
           << os_to
           << "\n";
      copy = FALSE;
    }
    if (unlink(os_to)) {
      perror(os_to);
      exit(1);
    }
  } 

  cout << "Copying " << fb.name << " to " << os_to << "\n";
  if (copy) {
    FILE * osfile;
    MacFile macfile(fs);
    if ((osfile = fopen(os_to, "w")) == NULL) {
      perror(os_to);
      exit(1);
    }
    
    macfile.Open(fb, flags);

    macfile.CopyToOS(osfile);

    macfile.Close();

    if (fclose(osfile)) {
      perror(os_to);
      exit(1);
    }
  }

  delete os_to;
}


int GetFunction (ArgList & args, Filesystem & fs)

{
  UINT flags = 0;
  BOOL mangle;

  // if less than one argument, print usage
  if (args.argc() < 1) {
    cerr << "usage: hread source dest\n";
    exit (1);
  }

  // if t option specified, convert newlines
  if (args.OptionSpecified('t'))
    flags |= MacFile::ConvertNewLines;

  // if R option specified, open resource fork
  if (args.OptionSpecified('R'))
    flags |= MacFile::ResourceFork;

  // if M option specified, don't mange filenames
  mangle = !args.OptionSpecified('M');

  glob_t g;
  char * dest = NULL;

  // tell the globber where the filesystem is
  SetMDirFilesystem(fs);

  // glob the first argument
  g.gl_offs = 0;
  glob(args[0], GLOB_DOOFFS, NULL, &g);

  // if more than one argument, glob everything except the last one
  if (args.argc() > 1) {
    dest = args[args.argc()-1];
    for (int i = 1; i < args.argc()-1; i++)
      glob(args[i], GLOB_DOOFFS | GLOB_APPEND, NULL, &g);
  }

  // if copying multiple files, last parameter must be a directory or NULL
  if (dest != NULL && g.gl_pathc > 1) {
    struct stat sb;
    if (stat (dest, &sb) ||
        ((sb.st_mode & S_IFMT) != S_IFDIR)) {
      cerr << "error: when copying multiple files, last argument must be a directory\n";
      exit(1);
    } 
  }

  // copy the files
  for (int i = 0; i < g.gl_pathc; i++) 
    CopyFromMacDisk(g.gl_pathv[i], dest, fs, flags, mangle);

  return 0;
}
