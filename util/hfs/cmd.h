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
// $Id: cmd.h,v 1.2 1994/05/15 06:19:15 rich Exp $
// $Log: cmd.h,v $
// Revision 1.2  1994/05/15 06:19:15  rich
// hfs for FreeBSD.
//
// hfs allows files to be copied off a Macintosh HFS disk on a
// non-Macintosh computer.  Written by Craig Southeren,
// geoffw@extro.ucc.su.oz.au.
//
// Revision 1.1.1.1  1994/05/15  05:44:52  rich
// hfs 0.3 from sunsite
//
// Revision 1.2  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.1  1993/12/23  15:11:55  craigs
// Initial revision
//
//

#ifndef _CMD_H
#define _CMD_H

#include "endian.h"
#include "misc.h"
#include "args.h"
#include "status.h"
#include "fs.h"

typedef int (*CommandPtr)(ArgList & args,
			  Filesystem & fs);

extern StatusFile * status_file;

#endif


