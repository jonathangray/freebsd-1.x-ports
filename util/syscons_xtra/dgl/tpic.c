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
  if (argc!=2) {
    printf("usage: tpic <mode>\n");
    exit(1);
  }
  DGLInitMode(atoi(argv[1]));
  DGLClear(DGLDisplay, 0);

  /* draw box around screen */
  DGLBox(DGLDisplay, 0, 0, DGLXsize-1, DGLYsize-1, 63);

  Mesh();
  CornerCircles(0);
  ColorBars();
  GrayScales();
  CenterEllipsesClear();
  CenterEllipses(63);
  while (1) {
    CornerCircles(rand()%256);
    sleep(1);
    CenterEllipses(rand()%256);
    sleep(1);
  }
  DGLEnd();
}

Mesh()
{
int i;
  for (i=0; i<DGLXsize; i+=DGLXsize/64) 
    DGLLine(DGLDisplay, i, 0, i, DGLYsize-1, 63);
  for (i=0; i<DGLYsize; i+=DGLXsize/64) 
    DGLLine(DGLDisplay, 0, i, DGLXsize-1, i, 63);
}    

CornerCircles(color)
int color;
{
  DGLEllipse(DGLDisplay, 
    DGLXsize/16, DGLXsize/16, DGLXsize/16, DGLXsize/16, 63);
  DGLFilledEllipse(DGLDisplay, 
    DGLXsize/16, DGLXsize/16, (DGLXsize/16)-1, (DGLXsize/16)-1, color);
  DGLBitmapString(DGLDisplay, (DGLXsize/16)-16, (DGLXsize/16)-8,
    "TesT", 63, 0, 0, 0);

  DGLEllipse(DGLDisplay, 
    DGLXsize-1-(DGLXsize/16), DGLXsize/16, DGLXsize/16, DGLXsize/16, 63);
  DGLFilledEllipse(DGLDisplay, 
    DGLXsize-1-(DGLXsize/16), DGLXsize/16, 
      (DGLXsize/16)-1, (DGLXsize/16)-1, color);
  DGLBitmapString(DGLDisplay, DGLXsize-(DGLXsize/16)-16, (DGLXsize/16)-8,
    "TesT", 63, 0, 0, 0);

  DGLEllipse(DGLDisplay, 
    DGLXsize-1-(DGLXsize/16), DGLYsize-1-(DGLXsize/16), 
      DGLXsize/16, DGLXsize/16, 63);
  DGLFilledEllipse(DGLDisplay, 
    DGLXsize-1-(DGLXsize/16), DGLYsize-1-(DGLXsize/16),
      (DGLXsize/16)-1, (DGLXsize/16)-1, color);
  DGLBitmapString(DGLDisplay, DGLXsize-(DGLXsize/16)-16, DGLYsize-8-DGLXsize/16,
    "TesT", 63, 0, 0, 0);

  DGLEllipse(DGLDisplay, 
    DGLXsize/16, DGLYsize-1-(DGLXsize/16), DGLXsize/16, DGLXsize/16, 63);
  DGLFilledEllipse(DGLDisplay, 
    DGLXsize/16, DGLYsize-1-(DGLXsize/16), 
      (DGLXsize/16)-1, (DGLXsize/16)-1, color);
  DGLBitmapString(DGLDisplay, (DGLXsize/16)-16, DGLYsize-8-DGLXsize/16,
    "TesT", 63, 0, 0, 0);
}

GrayScales()
{
int i;
  for (i=0; i<DGLXsize/2; i++) {
    DGLLine(DGLDisplay, 
      DGLXsize/4+i, 1+DGLXsize/16*2,
        DGLXsize/4+i, DGLXsize/16*3, i/(DGLXsize/128));
    DGLLine(DGLDisplay, 
      DGLXsize/4*3, 1+i+DGLXsize/16*2,
        (DGLXsize/16*13)-1, 1+i+DGLXsize/16*2, i/(DGLXsize/128));
    DGLLine(DGLDisplay, 
      (DGLXsize/4*3)-i, DGLXsize/16*9,
        (DGLXsize/4*3)-i, (DGLXsize/16*10)-1, i/(DGLXsize/128));
    DGLLine(DGLDisplay, 
      DGLXsize/4, (DGLXsize/16*10)-i-1,
        1+DGLXsize/16*3, (DGLXsize/16*10)-i-1, i/(DGLXsize/128));
  }
}

CenterEllipsesClear()
{
int i;
  DGLFilledEllipse(DGLDisplay, DGLXsize/2, DGLYsize/2, 
    (DGLYsize/4)-1, (DGLYsize/4)-1, 0);
}

CenterEllipses(color)
int color;
{
int i;
  for (i=(DGLYsize/4)-1; i>=0; i-=5) {
    DGLEllipse(DGLDisplay, 
      DGLXsize/2, DGLYsize/2, i, (DGLYsize/4)-1, color);
    DGLEllipse(DGLDisplay, 
      DGLXsize/2, DGLYsize/2, (DGLYsize/4)-1, i, color);
  }
}

ColorBars()
{
int i;
  for (i=0; i<256; i++) {
    DGLLine(DGLDisplay, 
      (DGLXsize/16)+1, i-128+DGLYsize/2,
        (DGLXsize/8)-1, i-128+DGLYsize/2, i);
    DGLLine(DGLDisplay, 
      DGLXsize-1-(DGLXsize/16), 128-i+DGLYsize/2,
        DGLXsize+1-(DGLXsize/8), 128-i+DGLYsize/2, i);
  }
}


