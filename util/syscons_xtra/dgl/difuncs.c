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

#include "dgl.h"

/*
 * some handy defines 
 */
#define ABS(a)		(((a)<0) ? -(a) : (a))
#define SGN(a)		(((a)<0) ? -1 : 1)

/*
 * local variables
 */
static char *DGLIFontArray;

char DGLGetXY(object, x, y) 
DGLBitmap *object;
int x, y;
{ 
int offset=((y*object->Xsize)+x); 

  if (DGLSwitchPending) 
    DGLSwitchAllow();
  if (object->Type == VIDBUF) {
    DGLSetSegment((offset>>12)); 
    return object->Bitmap[(offset)&0xffff]; 
  }
  else 
    return object->Bitmap[offset];
}

DGLLine(object, x1, y1, x2, y2, color)
DGLBitmap *object;
int x1, y1, x2, y2;
char color;
{
int d, x, y, ax, ay, sx, sy, dx, dy;

  dx = x2-x1; ax = ABS(dx)<<1; sx = SGN(dx); x = x1;
  dy = y2-y1; ay = ABS(dy)<<1; sy = SGN(dy); y = y1;

#if 0
  if (dy == 0 && dx > 8) {			/* (long) horizontal line */
    int val = (unsigned char) color;
    int ypos = object->Xsize*y;
    val |= val<<8; val |= val<<16;
    /* plot first pixel up til 32 bit boundary */
    for ( ; x%(sizeof(int)) && x < x2; x++)	
      DGLSetPixel(object, x+ypos, color);
    /* plot 4 pixels at a time until 3 or less pixels left */
    for ( ; x < x2; x+=4) 
      DGLSet4Pixel(object, ((x+ypos)>>2), val);
    /* plot remaining pixels (if any) */
    for ( ; x <= x2; x++)
      DGLSetPixel(object, x+ypos, color);
  }
  else 
#endif
  
  if (ax>ay) {					/* x dominant */
    d = ay-(ax>>1);
    for (;;) {
      DGLSetXY(object, x, y, color);
      if (x==x2)
	return;
      if (d>=0) {
	y += sy; d -= ax;
      }
      x += sx; d += ay;
    }
  }
  else {					/* y dominant */
    d = ax-(ay>>1);
    for (;;) {
      DGLSetXY(object, x, y, color);
      if (y==y2) 
	return;
      if (d>=0) {
	x += sx; d -= ay;
      }
      y += sy; d += ax;
    }
  }
}

DGLBox(object, x1, y1, x2, y2, color)
DGLBitmap *object;
int x1, y1, x2, y2;
char color;
{
  DGLLine(object, x1, y1, x2, y1, color);
  DGLLine(object, x2, y1, x2, y2, color);
  DGLLine(object, x2, y2, x1, y2, color);
  DGLLine(object, x1, y2, x1, y1, color);
}

DGLFilledBox(object, x1, y1, x2, y2, color)
DGLBitmap *object;
int x1, y1, x2, y2;
char color;
{
int y;

  for (y=y1; y<=y2; y++) DGLLine(object, x1, y, x2, y, color);
}

inline set4pixels(object, x, y, xc, yc, color) 
DGLBitmap *object;
int x, y, xc, yc;
char color;
{
  if (x!=0) { 
    DGLSetXY(object, xc+x, yc+y, color); 
    DGLSetXY(object, xc-x, yc+y, color); 
    if (y!=0) { 
      DGLSetXY(object, xc+x, yc-y, color); 
      DGLSetXY(object, xc-x, yc-y, color); 
    } 
  } 
  else { 
    DGLSetXY(object, xc, yc+y, color); 
    if (y!=0) 
      DGLSetXY(object, xc, yc-y, color); 
  } 
}

DGLEllipse(object, xc, yc, a, b, color)
DGLBitmap *object;
int xc, yc, a, b;
char color;
{
int x    = 0, 
    y    = b, 
    asq  = a*a, 
    asq2 = a*a*2, 
    bsq  = b*b, 
    bsq2 = b*b*2,
    d    = bsq-asq*b+asq/4,
    dx   = 0,
    dy   = asq2*b;

  while (dx<dy) {
    set4pixels(object, x, y, xc, yc, color);
    if (d>0) {
      y--; dy-=asq2; d-=dy;
    }
    x++; dx+=bsq2; d+=bsq+dx;
  }
  d+=(3*(asq-bsq)/2-(dx+dy))/2;
  while (y>=0) {
    set4pixels(object, x, y, xc, yc, color);
    if (d<0) {
      x++; dx+=bsq2; d+=dx;
    }
    y--; dy-=asq2; d+=asq-dy;
  }
}

inline set2lines(object, x, y, xc, yc, color) 
DGLBitmap *object;
int x, y, xc, yc;
char color;
{
  if (x!=0) { 
    DGLLine(object, xc+x, yc+y, xc-x, yc+y, color); 
    if (y!=0) 
      DGLLine(object, xc+x, yc-y, xc-x, yc-y, color); 
  } 
  else { 
    DGLLine(object, xc, yc+y, xc, yc-y, color); 
  } 
}

DGLFilledEllipse(object, xc, yc, a, b, color)
DGLBitmap *object;
int xc, yc, a, b;
char color;
{
int x    = 0, 
    y    = b, 
    asq  = a*a, 
    asq2 = a*a*2, 
    bsq  = b*b, 
    bsq2 = b*b*2,
    d    = bsq-asq*b+asq/4,
    dx   = 0,
    dy   = asq2*b;

  while (dx<dy) {
    set2lines(object, x, y, xc, yc, color);
    if (d>0) {
      y--; dy-=asq2; d-=dy;
    }
    x++; dx+=bsq2; d+=bsq+dx;
  }
  d+=(3*(asq-bsq)/2-(dx+dy))/2;
  while (y>=0) {
    set2lines(object, x, y, xc, yc, color);
    if (d<0) {
      x++; dx+=bsq2; d+=dx;
    }
    y--; dy-=asq2; d+=asq-dy;
  }
}

