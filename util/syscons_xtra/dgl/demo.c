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

main(argc, argv)
int argc; 
char **argv;
{
int x, y, i;
DGLBitmap tmp;

  if (argc!=2) {
    printf("usage: demo <mode>\n");
    exit(1);
  }
  DGLInitMode(atoi(argv[1]));
  sleep(1);
  DGLClear(DGLDisplay, 0);
  tmp.Type = MEMBUF; tmp.Bitmap = (char*)malloc(256*256);
  tmp.Xsize = 256; tmp.Ysize = 256;
  for (y=0; y<DGLYsize; y++) 
    DGLLine(DGLDisplay, 0, y, DGLXsize-1, y, y % 256);
  DGLLine(DGLDisplay, 0, 0, DGLXsize-1, DGLYsize-1, 63);
  DGLLine(DGLDisplay, 0, DGLYsize-1, DGLXsize-1, 0, 63);
  DGLLine(DGLDisplay, 0, 0, 0, DGLYsize-1, 63);
  DGLLine(DGLDisplay, DGLXsize-1, 0, DGLXsize-1, DGLYsize-1, 63);
  DGLEllipse(DGLDisplay, 256, 0, 256, 256, 63);
  DGLEllipse(DGLDisplay, 0, 256, 256, 256, 0);
  DGLBitmapString(DGLDisplay, 100,100,
    "This is text ‘›†’", 63, 0, 0, 0);
  DGLBitmapString(DGLDisplay, 100,100,
    "This is text ‘›†’", 63, 0, 0, 1);
  DGLBitmapString(DGLDisplay, 100,100,
    "This is text ‘›†’", 63, 0, 0, 2);
  DGLBitmapString(DGLDisplay, 100,100,
    "This is text ‘›†’", 63, 0, 0, 3);
  DGLBitmapString(DGLDisplay, 100,100,
    "This is text ‘›†’", 63, 0, 0, 4);
  DGLBitmapCopy(DGLDisplay, 256, 100, DGLDisplay, 0, 0, 256, 256, 0);
  sleep(2);
  memset(tmp.Bitmap, 0xff, 256*256);
  DGLBitmapCopy(&tmp, 4, 4, DGLDisplay, 0, 0, 128, 128, 0);
  for (i=0; i < 100; i+=5) {
    DGLBitmapCopy(DGLDisplay, 320+i, 200+i, &tmp, 0, 0, 136, 136, 0);
  }
  sleep(2);
  while (++i) {
    DGLLine(DGLDisplay,  rand()%DGLXsize, rand()%DGLYsize, 
            rand()%DGLXsize, rand()%DGLYsize, rand()%256);
    DGLEllipse(DGLDisplay, rand()%DGLXsize, rand()%DGLYsize,
   	       rand()%DGLXsize/2, rand()%DGLYsize/2, rand()%256);
    DGLBitmapCopy(DGLDisplay, rand()%DGLXsize, rand()%DGLYsize,
  		  DGLDisplay, rand()%DGLXsize, rand()%DGLYsize,
  		  rand()%DGLXsize, rand()%DGLYsize, 0);
    if (i > 256) break;
  }
  DGLEnd();
}

