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
// $Id: main.cxx,v 1.3 1994/05/15 06:36:15 rich Exp $
//

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>

#include "args.h"
#include "btree.h"
#include "error.h"
#include "catalog.h"
#include "fs.h"
#include "config.h"
#include "paranoid.h"
#include "status.h"
#include "file.h"
#include "cmd.h"
#include "ls.h"
#include "copy.h"

#define VERSION_MSG     "0.3"

struct FunctionTableElement {
  char       * name;
  CommandPtr function;
};

StatusFile * status_file;

//////////////////////////////////////////////
//
//  output a file to stdout
//
 
int CatFunction (ArgList & args, Filesystem & fs)

{
  int flags;

  //
  // there can be only only argument
  //
  if (args.argc() != 1) {
    cout << "usage: hcat file\n";
    return 0;
  }

  flags = 0;
  if (args.OptionSpecified('x'))
    flags |= MacFile::HexDump;
  if (args.OptionSpecified('t'))
    flags |= MacFile::ConvertNewLines;
  if (args.OptionSpecified('R'))
    flags |= MacFile::ResourceFork;

  //
  // find the file
  //
  FileBuffer fb;
  if (!fs.FindFile(fb, status_file->GetDir(), args[0])) {
    cerr << args[0] << ": file not found\n";
    exit (1);
  }

  //
  // dump the file
  //
  MacFile file(fs);

  file.Open(fb, flags);
  file.CopyToOS(stdout);
  file.Close();

  return 0;
}

//////////////////////////////////////////////
//
//  change directory
//

int CdFunction (ArgList & args, Filesystem & fs)

{
  //
  // there can be only only argument
  //
  if (args.argc() != 1) {
    cout << "usage: hcd dir\n";
    return 0;
  }

  // resolve the argument to a directory
  FileBuffer fb;
  if (!fs.ResolvePath(fb, args[0], status_file->GetDir())) 
    exit (1);

  // set the current directory
  status_file->SetDir(fb.id);
  
  return 0;
}


//////////////////////////////////////////////
//
//  change directory
//

int PwdFunction (ArgList & args, Filesystem & fs)

{
  char * path;

  //
  // no arguments
  //
  if (args.argc() != 0) {
    cout << "usage: hpwd\n";
    return 0;
  }

  //
  // get the current directory name, and print it
  //
  FileBuffer fb;
  fs.FindDir(fb, status_file->GetDir(), &path);
  cout << path << "\n";

  return 0;
}


//////////////////////////////////////////////
//
//  table of possible commands vs procedures. This table must include
//  synonyms for argv[0] as well as argv[1] type commands
//

static FunctionTableElement function_table[] = {
  { "cd",       CdFunction  },
  { "read",     GetFunction },
  { "ls",       LsFunction  },
  { "cat",      CatFunction },
  { "dir",      DirFunction },
  { "pwd",      PwdFunction },

  { "hcd",      CdFunction  },
  { "hread",    GetFunction },
  { "hls",      LsFunction  },
  { "hcat",     CatFunction },
  { "hdir",     DirFunction },
  { "hpwd",     PwdFunction },
  { NULL,       NULL }
};

//////////////////////////////////////////////
//
//  main entry point
//

int main (int argc, char *argv[])

{
  ArgList args (argv[0]);
  Volume * volume;
  Filesystem fs;
  int err;
  char * disk_device;
  char * funcName;
  CommandPtr function = NULL;

  //
  // if the program is invoked by it's full name, do the correct thing
  //
  if (strncmp(args.GetProgname(), "hfs", 3) == 0) {
    if (argc == 1) {
      cout << "HFSTools v"
           << VERSION_MSG
     << "\n"
     << "(C) Copyright 1993 by Equivalence\n";
      exit (0);
    } else {
      funcName = argv[1]; 
      argc -= 2;
      argv += 2;
    }
  } else {
    funcName = args.GetProgname();
    argc--;
    argv++;
  }
  args.Parse(argc, argv, "altxMR");

  //
  // get ptr to the function to handle the command
  //
  for (int i = 0; function_table[i].function != NULL; i++) 
    if (strcmp(funcName, function_table[i].name) == 0) {
      function = function_table[i].function;
      break;
    }

  //
  // give error message if no command found
  //
  if (function == NULL) {
    cerr << "error: unknown function \""
         << funcName
         << "\"\n";
    return 0;
  }

  //
  // get name of disk device from environment, or use default
  //
  if ((disk_device = getenv(DISK_DEVICE_ENV)) == NULL)
    disk_device = DEFAULT_DISK_DEVICE;

  //
  // open the device we are about to mount
  //
  if (DeviceVolume::IsDeviceName(disk_device))
    volume = new DeviceVolume(disk_device);
  else
    volume = new FileVolume(disk_device);

  //
  // mount the filesystem 
  //
  if ((err = fs.Mount (volume)) != E_OK) {
    cerr << "error: mount failed because ";
    switch(err) {
      case E_FAULTYMDB:
  cerr << "the Master Directory Block is corrupted\n";
  break;

      case E_NOTHFS:
  cerr << "the disk is not an HFS disk\n";
  break;
    }
    exit(1);
  }

  //
  // read status file (if we have one) and get the 
  // current directory name
  //
  status_file = new StatusFile(args.GetProgPath());
  status_file->Read();

  //
  // if the volume name has changed, then reset the current directory
  //
  {
    char * volume_name = fs.GetVolumeName();
    if ((status_file->GetVolumeName() == NULL) ||
  (strcmp(status_file->GetVolumeName(), volume_name) != 0)) {
      cerr << "Disk change detected - resetting current directory\n";
      status_file->SetVolumeName(volume_name);
      status_file->SetDir(M_CNID_ROOT);
    } else
      delete volume_name;
  }

  //
  // ensure the current directory exists
  //
  {
    FileBuffer fb;
    if (!fs.FindDir(fb, status_file->GetDir())) {
      cerr << "Cannot change to current directory - resetting to root directory\n";
      status_file->SetDir(M_CNID_ROOT);
    } 
  }

  //
  // dispatch the function
  //
  int retval = (*function)(args, fs);
  status_file->Write();

  return retval;
}

