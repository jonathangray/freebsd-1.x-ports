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
// $Id: ls.cxx,v 1.3 1994/05/15 06:36:11 rich Exp $
//

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <time.h>

#include "cmd.h"

////////////////////////////////////////////////////
//
// define a descendant of FileBuffer that knows how to
// print itself, and can also be chained into lists
//

class LsFileBuffer : public FileBuffer {
  public:
    // new constructors
    inline LsFileBuffer ()
      : FileBuffer() { next = NULL; }

    inline LsFileBuffer (FileBuffer & lfb)
      : FileBuffer(lfb) { next = NULL; }

    inline LsFileBuffer & operator = (FileBuffer & fb)
      { *this = fb; this->next = NULL; return *this; }

    // define output operators
    friend ostream & operator<<(ostream & out, LsFileBuffer & fb);
    ostream & PrintOn (ostream & out);

    // define list operators
    inline LsFileBuffer * GetNext()              { return next; }
    inline void SetNext (LsFileBuffer * theNext) { next = theNext; }


  private:
    LsFileBuffer * next;
};

class FileList {
  public:
    enum {
      ExcludeInvisible = 1,
      LongListing      = 2,
      Recursive        = 4
    };

    FileList(UINT flags, Filesystem & theFs, FileBuffer * fb = NULL);
    ~FileList();

    inline int GetMaxNameLen ()      { return max_name_len; }
    inline LsFileBuffer * GetHead () { return head; }
    inline LsFileBuffer * GetName () { return list_name; }
    inline ULONG GetFileCount ()     { return file_count; }
    inline ULONG GetDataTotal ()     { return total_data; }
    inline ULONG GetRsrcTotal ()     { return total_resource; }

    friend void DisplayFileList(FileList & filelist, char * dir_prefix, Filesystem & fs);

  private:
    int            max_name_len;
    ULONG           total_data;
    ULONG           total_resource;
    ULONG           file_count;

    UINT           flags;
    LsFileBuffer * list_name;

    LsFileBuffer * head;
    LsFileBuffer * tail;
};

ostream & LsFileBuffer::PrintOn (ostream & out) 
{
  char buff[30];
  time_t tm;
  if (IsFile()) {
    out << creator[0] << creator[1] << creator[2] << creator[3]
  << "   "
  << filetype[0] << filetype[1] << filetype[2] << filetype[3]
  << "   "
  << setw(8) << datasize << setw(0)
  << "  "
  << setw(8) << rsrcsize << setw(0);
  } else
    out << "                                ";
  tm = (LONG)date;

  if (difftime(time(NULL), tm) > (60L*60L*24L*365L))
    strftime(buff, 20, "%b %d  %Y", localtime(&tm));
  else
    strftime(buff, 20, "%b %d %H:%M", localtime(&tm));
  out << "  " << buff << " ";
  return out << "  " << name << "\n";
}

inline ostream & operator<<(ostream & out, LsFileBuffer & fb)
  { return fb.PrintOn(out); }

//////////////////////////////////////////////
//
//  construct a list of the files in a directory
//

FileList::FileList (UINT theFlags,
              Filesystem & fs,
              FileBuffer * fb /* = NULL */)

{
  LsFileBuffer *t;

  // initialise attributes
  head = tail = NULL; 
  total_data = total_resource = file_count = 0;
  max_name_len = 0;
  flags = theFlags;
  list_name = new LsFileBuffer(*fb);

  // if the fb we got was really a file, just return it
  if (fb->IsFile()) {
    total_data = fb->datasize;
    total_resource = fb->rsrcsize;
    file_count = 1;
    max_name_len = strlen(fb->name);
    return;
  }

  // if no files in the directory, do nothing
  if (!fs.FindFirst (*fb, fb->id)) 
    return;

  // make the list of files
  do {
    if ((flags & ExcludeInvisible) && fb->IsInvisible())
      ;
    else {
      file_count++;
      total_data += fb->datasize;
      total_resource += fb->rsrcsize;

      max_name_len = MAX(strlen(fb->name), max_name_len);
      t = new LsFileBuffer(*fb);
      if (head == NULL) 
        head = tail = t;
      else {
        tail->SetNext(t);
        tail = t;
      }
    }
  } while (fs.FindNext(*fb));
}

//////////////////////////////////////////////
//
//  list file(s) with all attributes
//
 
FileList::~FileList()

{
  LsFileBuffer * p;

  delete list_name;

  while (head != NULL) {
    p = head->GetNext();
    delete head;
    head = p;
  }
}

//////////////////////////////////////////////
//
//  list file(s) with all attributes
//
 
int DirFunction (ArgList & args, Filesystem & fs)

{
  FileBuffer     fb;
  FileRef        dir;
  LsFileBuffer * head;
  char         * path = NULL;

  // if no directory specified, just use the current directory
  // otherwise resolve arg[0] to a directory
  if (args.argc() != 0) {
    if (!fs.ResolvePath(fb, args[0], status_file->GetDir())) 
      exit (1);
    dir = fb.id;
    if (fb.IsFile()) {
      FileBuffer nfb;
      fs.FindDir (nfb, fb.parid, &path);
    } else
      fs.FindDir (fb, dir, &path);
  } else {
    dir = status_file->GetDir();
    fs.FindDir (fb, dir, &path);
  }


  // display volume information
  cout << "\n"
       << "Volume is "
       << fs.GetVolumeName()
       << "\nDirectory of "
       << path
       << "\n\n";
  delete path;

  // get the list of files
  FileList filelist(0, fs, &fb);

  // output the list of files
  if ((head = filelist.GetHead()) == NULL) 
    cout << *(filelist.GetName());
  else
    for (head = filelist.GetHead(); head != NULL; head = head->GetNext()) 
      cout << *head;

  cout << "\n"
       << setw(12) << filelist.GetFileCount() << setw(0)
       << " file(s)"
       << setw(12) << filelist.GetDataTotal() << setw(0)
       << " bytes (data)\n"
       << setw(32) << filelist.GetRsrcTotal() << setw(0)
       << " bytes (resource)\n"
       << setw(32) << fs.GetVolFreeSpace() << setw(0)
       << " bytes free\n";
       

  return 0;
}

//////////////////////////////////////////////
//
//  display a list of files
//

static void DisplayFileList(FileList & filelist, char * dir_prefix, Filesystem & fs)

{
  LsFileBuffer * head;

  // display the directory name if required
  if (dir_prefix != NULL) 
    cout << "\n" << dir_prefix << ":" << filelist.list_name->name << "\n";

  // display the list of files in verbose mode
  if (filelist.flags & FileList::LongListing) {
    for (head = filelist.GetHead(); head != NULL; head = head->GetNext()) 
      cout << *head;
  } else {
    int files_per_line = 80 / (filelist.GetMaxNameLen() + 2);
    int col = 0;
    for (head = filelist.GetHead(); head != NULL; head = head->GetNext()) {
      cout << head->name;
      if (++col >= files_per_line) {
        cout << "\n";
        col = 0;
      } else {
        for (int i = filelist.GetMaxNameLen() - strlen(head->name); i > 0; i--)
          cout << " ";
        cout << "  ";
      }
    }
    if (col != 0)
      cout << "\n";
  }

  // if we are recursing, output all of the subdirectories
  if (filelist.flags & FileList::Recursive)
    for (head = filelist.GetHead(); head != NULL; head = head->GetNext()) 
      if (!head->IsFile()) {
        FileList nfilelist(filelist.flags, fs, head);
        char *nname;
        if (dir_prefix != NULL) {
          nname = new char [strlen(dir_prefix) + 1 + strlen(filelist.GetName()->name) + 1];
          strcpy(nname, dir_prefix);
          strcat(nname, ":");
          strcat(nname, filelist.GetName()->name);
        } else {
          nname = new char [strlen(filelist.GetName()->name) + 1];
          strcpy(nname, filelist.GetName()->name);
        }
        DisplayFileList(nfilelist, nname, fs);
        delete nname;
      }
}


//////////////////////////////////////////////
//
//  list file(s) sort of like the ls command
//
 
int LsFunction (ArgList & args, Filesystem & fs)

{
  FileBuffer     fb;
  FileRef        dir;
  UINT           flags = FileList::ExcludeInvisible;

  // if specified -a option, don't exclude invisible files
  if (args.OptionSpecified('a'))
    flags &= ~FileList::ExcludeInvisible;

  // if specified -l option, do long listing
  if (args.OptionSpecified('l'))
    flags |= FileList::LongListing;

  // if specified -R option, do recursive descent of directory hierarchy
  if (args.OptionSpecified('R'))
    flags |= FileList::Recursive;

  // if no directory specified, just use the current directory
  // otherwise resolve arg[0] to a directory
  if (args.argc() == 0) {
    fs.FindDir (fb, dir = status_file->GetDir());
  } else if (!fs.ResolvePath(fb, args[0], status_file->GetDir())) 
    exit (1);

  // get the list of files
  FileList filelist(flags, fs, &fb);
  if (filelist.GetHead() == NULL) {
    cout << *(filelist.GetName());
    return 0;
  }

  // display the files
  DisplayFileList(filelist, NULL, fs);

  return 0;
}
