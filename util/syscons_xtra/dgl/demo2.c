/*
 *  Copyright (C) 1991 By DeepCore Technologies
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *      DeepCore Technologies
 *	Att: Søren Schmidt 	Email:	sos@kmd-ac.dk
 *	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
 *	DK9210 Aalborg SO	Phone:  +45 9814 8076
 */

#include <sys/types.h>
#include "dgl.h"

#define	WPUT(i, col)	{DGLSetSegment((i)>>15); vptr[(i)&0x7fff]=col;}

main(argc, argv)
int argc; 
char **argv;
{
int x, y, i;
unsigned short *vptr;

  if (argc!=2) {
    printf("usage: demo <mode>\n");
    exit(1);
  }
  DGLInitMode(atoi(argv[1]));
  sleep(1);
  DGLDisplay->Xsize = 1600;
  DGLClear(DGLDisplay, 255);
  DGLSetSegment(0);
  vptr=SvgaMem;
  for (i=0; i<256; i++) WPUT(i, (i>>3)<<10 | (i>>3)<<5 | (i>>3));
  for (i=0; i<256; i++) WPUT(400+i, (i>>3)<<10 | (i>>3)<<5 | (i>>3));
  for (i=0; i<256; i++) WPUT(800+i, (i>>3)<<10 | (i>>3)<<5 | (i>>3));
  for (i=0; i<256; i++) WPUT(1200+i, (i>>3)<<10);
  for (i=0; i<256; i++) WPUT(1600+i, (i>>3)<<10);
  for (i=0; i<256; i++) WPUT(2000+i, (i>>3)<<10);
  for (i=0; i<256; i++) WPUT(2400+i, (i>>3)<<5);
  for (i=0; i<256; i++) WPUT(2800+i, (i>>3)<<5);
  for (i=0; i<256; i++) WPUT(3200+i, (i>>3)<<5);
  for (i=0; i<256; i++) WPUT(3600+i, (i>>3));
  for (i=0; i<256; i++) WPUT(4000+i, (i>>3));
  for (i=0; i<256; i++) WPUT(4400+i, (i>>3));
  while(1);
  DGLEnd();
}

