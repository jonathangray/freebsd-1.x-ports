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
// $Id: misc.h,v 1.3 1994/05/15 06:36:20 rich Exp $
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
