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
// $Id: paranoid.cxx,v 1.1 1994/05/15 05:44:53 rich Exp $
// $Log: paranoid.cxx,v $
// Revision 1.1  1994/05/15 05:44:53  rich
// Initial revision
//
// Revision 1.2  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.1  1993/11/22  22:25:55  craigs
// Initial revision
//
//
//

#include <iostream.h>
#include "endian.h"

static char paranoia_string1[] = "paranoia check failed - type ";
static char paranoia_string2[] = " should be of size ";
static char paranoia_string3[] = ", but it is of size";

#define CHECK_TYPE(type,str,siz) if (sizeof (type) != siz) \
    cerr << paranoia_string1 << str \
         << paranoia_string2 << siz \
         << paranoia_string3 << sizeof (type) \
         << "\n"; 

void paranoid_check ()

{
  CHECK_TYPE (MLONG,  "MLONG", 4)
  CHECK_TYPE (MULONG, "MULONG",4)
  CHECK_TYPE (MINT,   "MINT",  2)
  CHECK_TYPE (MUINT,  "MUINT", 2)
  CHECK_TYPE (MBYTE,  "MBYTE",  1)
  CHECK_TYPE (MSBYTE, "MSBYTE", 1)
}
