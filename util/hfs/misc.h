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
// $Id: misc.h,v 1.2 1994/05/15 06:19:32 rich Exp $
// $Log: misc.h,v $
// Revision 1.2  1994/05/15 06:19:32  rich
// hfs for FreeBSD.
//
// hfs allows files to be copied off a Macintosh HFS disk on a
// non-Macintosh computer.  Written by Craig Southeren,
// geoffw@extro.ucc.su.oz.au.
//
// Revision 1.1.1.1  1994/05/15  05:44:54  rich
// hfs 0.3 from sunsite
//
// Revision 1.5  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.4  1993/12/23  22:44:23  craigs
// Added strprepend function
//
// Revision 1.3  1993/12/23  15:18:47  craigs
// Added MIN and MAX functions
//
// Revision 1.2  1993/11/24  21:36:05  craigs
// Various changes remove warnings under MSDOS/NT
//     by robertj
//
// Revision 1.1  1993/11/22  22:27:51  craigs
// Initial revision
//
//
//

#ifndef _MISC_H_
#define	_MISC_H_

#ifdef _MSC_VER
#pragma warning(disable:4710) // inlines not expanded warning
#endif

#include "endian.h"

void dump (void * vbuffer, int count);

char * ptocstr (BYTE * pstring);

void pstring (char * str);

char * strprepend(char * s1, char * s2);

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef MAX
#define	MAX(x,y)	((x)<(y)?(y):(x))
#endif

#ifndef	MIN
#define	MIN(x,y)	((x)<(y)?(x):(y))
#endif

#endif
