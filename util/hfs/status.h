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
// $Id: status.h,v 1.3 1994/05/15 06:36:24 rich Exp $
//

#ifndef _STATUS_H
#define _STATUS_H

#include "mac.h"

class StatusFile {
  public:
    StatusFile(const char * defaultPath);
    void Read();
    void Write(BOOL force = FALSE);

    inline FileRef       GetDir()        { return dir; }
    inline char *        GetVolumeName() { return volume_name; }

    inline void          SetDir (FileRef Dir)
      { dir = Dir; dirty = TRUE; }

    inline void          SetVolumeName (char * Vname)
      { volume_name = Vname; dirty = TRUE; }

  private:
    FileRef dir;
    const char * filename;
    char * volume_name;
    BOOL dirty;
};

#endif

