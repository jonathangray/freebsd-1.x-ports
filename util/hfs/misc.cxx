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
// $Id: misc.cxx,v 1.1 1994/05/15 05:44:52 rich Exp $
// $Log: misc.cxx,v $
// Revision 1.1  1994/05/15 05:44:52  rich
// Initial revision
//
// Revision 1.4  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.3  1993/12/23  22:44:23  craigs
// Added strprepend function
//
// Revision 1.2  1993/11/23  20:28:49  craigs
// Removed reference to std.h
//
// Revision 1.1  1993/11/22  22:25:55  craigs
// Initial revision
//
//
//

#include <stdio.h>
#include <iostream.h>
#include <string.h>

#include "misc.h"

char * ptocstr (BYTE * pstring)

{
  // get size of the string
  int len = pstring[0];

  // allocate storage
  char * cstring = new char[len + 1];

  // copy the name in
  strncpy (cstring, (const char *)pstring+1, len);

  // and terminate it
  cstring [len] = '\0';

  return cstring;
}

void dump (void * vbuffer, int count)

{
  unsigned char * buffer = (unsigned char *)vbuffer;
  int i, b;
  for (b = 0; b < count; b += 16) {
    printf ("%03x   ", b);
    for (i = 0; i < 16; i++) 
      printf ("%02x ", buffer [i+b]);
    printf ("    ");
    for (i = 0; i < 16; i++) 
      printf ("%c", (0x20 <= buffer [i+b]) && (buffer[i+b] < 0x7f) ? buffer[i+b] : '.');
    printf ("\n");
  }
}

void pstring (char * str)

{
  int k;
  cout << "\"";
  for (k = 1; k <= str[0]; k++)
    cout << str[k];
  cout << "\"";
}


char * strprepend (char * s1, char * s2)

{
  char *z;

  if (s2 == NULL) {
    z = new char [strlen(s1) + 1];
    strcpy(z,s1);
  } else {
    z = new char [strlen(s1) + strlen(s2) + 1]; 
    strcpy(z,s1);
    strcat(z,s2);
    delete s2;
  }
  return z;
}

