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
// $Id: catalog.cxx,v 1.1 1994/05/15 05:44:51 rich Exp $
// $Log: catalog.cxx,v $
// Revision 1.1  1994/05/15 05:44:51  rich
// Initial revision
//
// Revision 1.11  1994/01/11  00:38:27  craigs
// Fixed problem with halting catalog searches on finding filename that
// matched a prefix of the required one
// Added STRNICMP macro to deal with portability of strncasecmp
// General cleaning up declarations to reduce compiler warning under MSDOS/NT
//
// Revision 1.10  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.9  1994/01/01  16:03:44  craigs
// Added completely optimised searching for FindNext, FindFile and FindDir
//
// Revision 1.8  1993/12/30  07:46:30  craigs
// Removed redundant dirname attribute from FileBuffer
// Removed while(1) construct to avoid MSVC warning
//
// Revision 1.7  1993/12/23  22:38:39  craigs
// Fixed FindDir to act intelligently when not constructing pathnames
//
// Revision 1.6  1993/12/23  15:14:47  craigs
// Added copy constructor for FileBuffer
// Initial stab at optimised FindNextKey
//
// Revision 1.5  1993/11/25  22:03:27  craigs
// Added extra debugging, simplified FirstFirstKey::Compare, fixed
// swapped return values from FindDirKey::OnFound
//
// Revision 1.4  1993/11/24  21:36:05  craigs
// Various changes remove warnings under MSDOS/NT
//     by robertj
//
// Revision 1.3  1993/11/23  22:20:20  craigs
// Btree is now a descendant of MacFile, so extents now work!
//
// Revision 1.2  1993/11/23  20:29:37  craigs
// Changed unsigned long to ULONG, and short to BOOL
//
// Revision 1.1  1993/11/22  22:25:55  craigs
// Initial revision
//
//
//

#include <iostream.h>

#include "catalog.h"
#include "fs.h"
#include "config.h"
#include <string.h>
#include <malloc.h>


///////////////////////////////////////////////////////
//
// CatSearchKey
//

class CatSearchKey : public BTreeSearchKey {
  public:
    inline  CatSearchKey       (FileBuffer * theFb)  { fb = theFb; }
    virtual BOOL OnFound       (BTreeKey & key, BTreeLeaf & leaf);

  protected:
    FileBuffer * fb;
};

BOOL CatSearchKey::OnFound   (BTreeKey & key, BTreeLeaf & leaf)

{
  CatKeyRec  & catkey  = (CatKeyRec &)key;
  CatDataRec & catdata = (CatDataRec &)leaf;

  // ignore directory thread records
  if (catdata.GetType() == ThdRec) 
    return FALSE;

  // copy common information
  if (fb->name != NULL)
    free(fb->name);
  fb->name   = ptocstr(catkey.GetCName());
  fb->parid  = catkey.GetParID();
  fb->id     = catdata.GetId();
  fb->isfile = catdata.IsFile();
  fb->date   = catdata.GetDate();

  catdata.GetFileData(fb->creator,
                      fb->filetype,
                      &fb->datasize,
                      fb->dataext,
                      &fb->rsrcsize,
                      fb->rsrcext,
                      &fb->flags);
  return TRUE;
}

///////////////////////////////////////////////////////
//
// FindFirstKey
//   This class is a Btree search key used to find the
//   first child file in a directory

class FindFirstKey : public CatSearchKey {

  friend class CatalogFile;

  public:
    FindFirstKey(FileBuffer & theFb, FileRef theDir);

  protected:
    virtual Comparison Compare (BTreeKey & key, BOOL IsLeaf);
    virtual BOOL OnFound       (BTreeKey & key, BTreeLeaf & leaf);

    FileRef id;
    BOOL    trigger;
};


FindFirstKey::FindFirstKey(FileBuffer & theFb, FileRef ID)
  : CatSearchKey(&theFb)

{
#if 0
  cout << "FindFirst: Looking for first file in directory " << ID << "\n";
#endif
  id = ID;
  trigger = FALSE;
}

BTreeSearchKey::Comparison FindFirstKey::Compare (BTreeKey & key, BOOL IsLeaf)

{
  CatKeyRec & catkey = (CatKeyRec &)key;

#if 0
cout << "FindFirst: Comparing key to " << catkey.GetParID() << " ";
pstring((char *)catkey.GetCName());
cout << " (" << (IsLeaf ? "leaf" : "index") << ")\n";
#endif

  if (catkey.GetParID() < id)
    return LessThan;
  if (catkey.GetParID() > id) 
    return GreaterThan;

  if (*(catkey.GetCName()) != 0) 
    return trigger ? Equals : GreaterThan;

#if 0
  cout << "   Compare found!\n";
#endif

  return Equals;
};

BOOL FindFirstKey::OnFound (BTreeKey & key, BTreeLeaf & leaf)

{
  CatKeyRec & catkey = (CatKeyRec &)key;

  if (*(catkey.GetCName()) == 0) {
    trigger = TRUE;
    return FALSE;
  }

  return CatSearchKey::OnFound(key, leaf);
}

///////////////////////////////////////////////////////
//
// FindNextKey
//   This class is a Btree search key used to find the 
//   child file after using FindFirstKey

class FindNextKey : public CatSearchKey {

  friend class CatalogFile;

  public:
    FindNextKey(FileBuffer & fb);

  private:
    virtual Comparison Compare (BTreeKey & key, BOOL IsLeaf);
    virtual BOOL OnFound (BTreeKey & key, BTreeLeaf & leaf);

    FileRef last_parid;
    FileRef last_id;
    char *  last_name;
    BOOL trigger;
};


FindNextKey::FindNextKey(FileBuffer & theFb)
  : CatSearchKey(&theFb)

{
#if 0
  cout << "FindNext: Looking for file after " << theFb.name << "(" << theFb.parid << ")\n";
#endif
  last_parid = theFb.parid;
  last_name  = theFb.name;
  last_id    = theFb.id;

  trigger = FALSE;
}

BTreeSearchKey::Comparison FindNextKey::Compare (BTreeKey & key, BOOL IsLeaf)

{
  CatKeyRec & catkey = (CatKeyRec &)key;

#if 0
cout << "FindNext: Comparing key to " << catkey.GetParID() << " ";
pstring((char *)catkey.GetCName());
cout << " (" << (IsLeaf ? "leaf" : "index") << ")\n";
#endif

  if (catkey.GetParID() < last_parid)
    return LessThan;
  else if (catkey.GetParID() > last_parid) 
    return GreaterThan;

  if (!trigger) {
    char * name = (char *)catkey.GetCName();

    if (*name == 0)
      return LessThan;

    int c = STRNICMP(name+1, last_name, MIN(*name, strlen(last_name)));
    if (c < 0)
      return LessThan;
    else if (c > 0)
      return GreaterThan;

    c = *name - strlen(last_name);
    if (c < 0)
      return LessThan;
    else if (c > 0)
      return GreaterThan;
  }

#if 0
  cout << "   Compare found!\n";
#endif
  return Equals;
};



BOOL FindNextKey::OnFound (BTreeKey & key, BTreeLeaf & leaf)

{
  CatKeyRec  & catkey  = (CatKeyRec &)key;
  CatDataRec & catdata = (CatDataRec &)leaf;

  if (trigger)  {
    CatSearchKey::OnFound (key, leaf);
    return TRUE;
  }

  if ((last_parid == catkey.GetParID()) &&
      (last_id    == catdata.GetId())) {
#if 0
    cerr << "trigger set\n";
#endif
    trigger = TRUE;
  }

  return FALSE;
}


///////////////////////////////////////////////////////
//
// FindDirKey
//   This class is a Btree search key used to find the
//   name of a directory

class FindDirKey : public CatSearchKey {

  friend class CatalogFile;

  public:
    FindDirKey(FileBuffer & theFb, ULONG theDir);

    virtual Comparison Compare (BTreeKey & key, BOOL IsLeaf);

  protected:
    virtual BOOL OnFound      (BTreeKey & key, BTreeLeaf & leaf);
    FileRef  dirid;
};

FindDirKey::FindDirKey(FileBuffer & theFb, ULONG theDir)
  : CatSearchKey (&theFb)

{
#if 0
  cout << "FindDir: Looking for directory " << theDir << "\n";
#endif
  dirid = theDir;
}

BTreeSearchKey::Comparison FindDirKey::Compare (BTreeKey & key, BOOL IsLeaf)

{
  CatKeyRec & catkey = (CatKeyRec &)key;

#if 0
cout << "FindDir: Comparing key to " << catkey.GetParID() << " ";
pstring((char *)catkey.GetCName());
cout << " (" << (IsLeaf ? "leaf" : "index") << ")\n";
#endif

  if (catkey.GetParID() < dirid)
    return LessThan;
  if (catkey.GetParID() > dirid) 
    return GreaterThan;

  if (*(catkey.GetCName()) != 0)
    return GreaterThan;

  return Equals;
};


BOOL FindDirKey::OnFound (BTreeKey & /*key*/, BTreeLeaf & leaf)

{
  CatDataRec & catdata = (CatDataRec &)leaf;

  //
  // finished when we get the directory thread record
  //
  if (catdata.GetType() == ThdRec) {
#if 0
cout << "FindDir: thread record found\n";
#endif
    fb->id     = dirid;
    if (dirid == M_CNID_ROOT) {
      fb->name   = new char[1];
      *(fb->name) = '\0';
    } else
      fb->name   = ptocstr(catdata.GetThdCName());
    fb->parid  = catdata.GetThdParID();
    fb->isfile = FALSE;
    return TRUE;
  }
  return FALSE;
}



///////////////////////////////////////////////////////
//
// FindFileKey
//   This class is a Btree search key used to find a file
//   

class FindFileKey : public CatSearchKey {

  friend class CatalogFile;

  public:
    FindFileKey(FileBuffer & fb, FileRef dir_id, char * name);

  private:
    virtual Comparison Compare (BTreeKey & key, BOOL IsLeaf);

    char *  find_name;
    FileRef dir_id;
};


FindFileKey::FindFileKey(FileBuffer & fb, FileRef theDir, char * theName)
  : CatSearchKey(&fb)

{
  find_name  = theName;
  dir_id     = theDir;
}

BTreeSearchKey::Comparison FindFileKey::Compare (BTreeKey & key, BOOL IsLeaf)

{
  CatKeyRec & catkey = (CatKeyRec &)key;

#if 0
cout << "Comparing key to " << catkey.GetParID() << " ";
pstring((char *)catkey.GetCName());
cout << " (" << (IsLeaf ? "leaf" : "index") << ")\n";
#endif

  if (catkey.GetParID() < dir_id)
    return LessThan;
  else if (catkey.GetParID() > dir_id) 
    return GreaterThan;

  char * name = (char *)catkey.GetCName();

  if (*name == 0)
    return LessThan;

  int c = STRNICMP(name+1, find_name, MIN(*name, strlen(find_name)));
  if (c < 0)
    return LessThan;
  else if (c > 0)
    return GreaterThan;

  c = *name - strlen(find_name);
  if (c < 0)
    return LessThan;
  else if (c > 0)
    return GreaterThan;

  return Equals;
};


//////////////////////////////////////
//
// CatalogFile::CatalogFile
//

CatalogFile::CatalogFile (Filesystem & theFs)
    : BTreeFile (theFs,
                 theFs.GetMDB()->drCtExtRec, 
     M_CNID_CATALOG,
                 theFs.GetMDB()->drCtFlSize)

{
#if 0
  cout << "\n\nCatalog extent record:\n";
  dump (theFs.GetMDB()->drCtExtRec, 12);
#endif
}


//////////////////////////////////////
//
// CatalogFile::FindFirst
//

BOOL CatalogFile::FindFirst (FileBuffer & fb, ULONG dirID)

{
  FindFirstKey key(fb, dirID);
  return Search (key);
}


//////////////////////////////////////
//
// CatalogFile::FindNext
//

BOOL CatalogFile::FindNext (FileBuffer & fb)

{
  FindNextKey key(fb);
  return Search (key);
}

//////////////////////////////////////
//
// CatalogFile::FindFile
//

BOOL CatalogFile::FindFile (FileBuffer & fb,
                            FileRef dirID,
                            char * name)

{
  FindFileKey key(fb, dirID, name);
  return Search (key);
}


//////////////////////////////////////
//
// CatalogFile::FindDir
//

BOOL CatalogFile::FindDir (FileBuffer & fb, ULONG dirID, char ** Path)

{
  FindDirKey key(fb, dirID);
  if (!Search(key)) 
    return FALSE;

  if (Path == NULL)
    return TRUE;

  *Path = new char [1];
  (*Path)[0] = '\0';
  FileBuffer nfb = fb;

  for (;;) {
    if (nfb.id == M_CNID_ROOT)
      break;
    *Path = strprepend(nfb.name, *Path);
    *Path = strprepend(":", *Path);
    FindDirKey key(nfb, nfb.parid);
    if (!Search(key)) {
      delete *Path;
      return FALSE;
    }
  }
  if ((*Path)[0] != ':')
    *Path = strprepend(":", *Path);
  return TRUE;
}


//////////////////////////////////////
//
// FileBuffer::Assign
//

void FileBuffer::Assign(FileBuffer & fb)

{
  if (name != NULL)
    free(name);
  name     = strdup(fb.name);
  id       = fb.id;
  parid    = fb.parid;
  date     = fb.date;
  isfile   = fb.isfile;
  datasize  = fb.datasize;
  rsrcsize  = fb.rsrcsize;

   // remainder are only valid if item is a file
  if (isfile) {
    for (int i = 0; i < 4;i++)
      creator[i]   = fb.creator[i];
    for (i = 0; i < 4;i++)
      filetype[i]  = fb.filetype[i];
    for (i = 0; i < 6;i++)
      dataext[i]   = fb.dataext[i];
    for (i = 0; i < 6;i++)
      rsrcext[i]   = fb.rsrcext[i];
    flags     = fb.flags;
  }
}


