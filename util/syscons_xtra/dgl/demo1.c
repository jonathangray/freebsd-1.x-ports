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
int i, j=0, x, y, line, stop=0, xp=100, yp=100;
char str[16];
DGLWidget *Widget0;
DGLWidget *Widget1;
DGLWidget *Widget2;
DGLWidget *Widget3;
DGLWidget *Widget4;

  if (argc!=2) {
      printf("usage: demo1 <mode>\n");
      exit(1);
  }
  DGLInitMode(atoi(argv[1]));
  sleep(1);
  DGLClear(DGLDisplay, 64);

  Widget0=(DGLWidget*)DGLWidgetCreate(32,32);
  Widget0->Xpos=100; Widget0->Ypos=100;
  memset (Widget0->Mask->Bitmap, 0x3f, 32*32);
  DGLFilledEllipse(Widget0->Mask, 16, 16, 12, 12, 0);
  memset (Widget0->Object->Bitmap, 0xb0, 32*32);

  Widget1=(DGLWidget*)DGLWidgetCreate(32,32);
  Widget1->Xpos=120;
  Widget1->Ypos=120;
  memset (Widget1->Mask->Bitmap, 0x00, 32*32);
  memset (Widget1->Object->Bitmap, 0x80, 32*32);
  DGLFilledEllipse(Widget1->Mask, 16, 16, 12, 12, 65);

  Widget2=(DGLWidget*)DGLWidgetCreate(32,32);
  Widget2->Xpos=140;
  Widget2->Ypos=140;
  memset (Widget2->Mask->Bitmap, 0x3f, 32*32);
  memset (Widget2->Object->Bitmap, 0xc0, 32*32);

  Widget3=(DGLWidget*)DGLWidgetCreate(32,32);
  Widget3->Xpos=200;
  Widget3->Ypos=100;
  memset (Widget3->Mask->Bitmap, 0x3f, 32*32);
  memset (Widget3->Object->Bitmap, 0xa0, 32*32);

  Widget4=(DGLWidget*)DGLWidgetCreate(32,32);
  Widget4->Xpos=130;
  Widget4->Ypos=130;
  memset (Widget4->Mask->Bitmap, 0x3f, 32*32);
  memset (Widget4->Object->Bitmap, 0x60, 32*32);
  DGLEllipse(Widget4->Object, 16, 16, 12, 12, 65);

  DGLWidgetAdd(Widget0);
  DGLWidgetShow(Widget0);

  DGLWidgetAdd(Widget1);
  DGLWidgetShow(Widget1);

  DGLWidgetAdd(Widget4);
  DGLWidgetShow(Widget4);

  DGLWidgetAdd(Widget2);
  DGLWidgetShow(Widget2);

  DGLWidgetAdd(Widget3);
  DGLWidgetShow(Widget3);

  while(1) {
      movearound(Widget3);
      movearound1(Widget0);
  }
}

movearound(Widget) 
DGLWidget *Widget;
{
static int xp=100, yp=100;

  if (random()&1)
    xp+=3; 
  else 
    xp-=3;
  if (xp > DGLDisplay->Xsize)
    xp = DGLDisplay->Xsize;
  if (xp < 0)
    xp = 0;
  if (random()&1) 
    yp+=3; 
  else 
    yp-=3;
  if (yp > DGLDisplay->Ysize) 
    yp = DGLDisplay->Ysize;
  if (yp < 0) 
    yp = 0;
  DGLWidgetMoveTo(Widget, xp, yp);
}
movearound1(Widget) 
DGLWidget *Widget;
{
static int xp=100, yp=100;
static int xa=4, ya=4;

  if (xp > DGLDisplay->Xsize-32)
    xa = -4;
  if (xp < 0)
    xa = 4;

  if (yp > DGLDisplay->Ysize-32) 
    ya = -4;
  if (yp < 0) 
    ya = 4;

  xp+=xa; yp+=ya;

  DGLWidgetMoveTo(Widget, xp, yp);
}
