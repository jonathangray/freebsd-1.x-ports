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
// $Id: mac.cxx,v 1.1 1994/05/15 05:44:52 rich Exp $
// $Log: mac.cxx,v $
// Revision 1.1  1994/05/15 05:44:52  rich
// Initial revision
//
// Revision 1.3  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.2  1993/12/23  22:42:22  craigs
// Forced rsrcsize and datasize to be set for all FileBuffers
// Ensured that creator and type strings are always printable
//
// Revision 1.1  1993/12/23  14:59:45  craigs
// Initial revision
//
//

#include "mac.h"

#include <stdio.h>
#include <ctype.h>

#define	MAKE_PRINTABLE(c)	(isprint(c)?(c):' ')

void CatDataRec::GetFileData (char * creator,
                              char * filetype,
                              unsigned long * dsize,
                              ExtDataRec dext,
                              unsigned long * rsize,
                              ExtDataRec rext,
                              UINT       * flags)
{
  int i;

  if (!IsFile()) {
    if (rsize    != NULL)
      *rsize = 0;
    if (dsize    != NULL)
      *dsize = 0;
    return;
  }

  if (creator  != NULL)
    for (i = 0; i < 4; i++)
      creator[i] = MAKE_PRINTABLE(rec.cdrFilRec.filUsrWds.fdCreator[i]);

  if (filetype != NULL)
    for (i = 0; i < 4; i++)
      filetype[i] = MAKE_PRINTABLE(rec.cdrFilRec.filUsrWds.fdType[i]);

  if (dsize    != NULL)
    *dsize = (unsigned long)rec.cdrFilRec.filLgLen;

  if (dext     != NULL)
    for (i = 0; i < 6; i++) dext[i] = rec.cdrFilRec.filExtRec[i];

  if (rsize    != NULL)
    *rsize = (unsigned long)rec.cdrFilRec.filRLgLen;

  if (rext     != NULL)
    for (i = 0; i < 6; i++) rext[i] = rec.cdrFilRec.filRExtRec[i];

  if (flags    != NULL)
    *flags = rec.cdrFilRec.filUsrWds.fdFlags;
}
  

int CatDataRec::GetSize()

{
  switch (cdrType) {
    case DirRec:
      return sizeof(rec.cdrDirRec);
    case FilRec:
      return sizeof(rec.cdrFilRec);
    case ThdRec:
      return sizeof(rec.cdrThdRec);
    case FThdRec:
      return sizeof(rec.cdrFThdRec);
    default:
      return 0;
  }
}
      
