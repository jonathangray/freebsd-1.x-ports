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
 *  some handy defines
 */

#define DOWN	0x00
#define UP	0x01
#define RIGHT	0x02
#define LEFT	0x04
#define USEMASK 0x08
#define SRCMEM  0x10
#define SRCVID	0x20
#define DESMEM  0x40
#define DESVID	0x80

#define YINC	for (y=srcy*src->Xsize, desy=desy*des->Xsize; \
	             y <= srcy2*src->Xsize; \
	             y+=src->Xsize, desy+=des->Xsize) 

#define YDEC    for (y=srcy2*src->Xsize, desy=desy2*des->Xsize; \
	             y >= srcy*src->Xsize; \
	             y-=src->Xsize, desy-=des->Xsize) 

#define XINC    for (x=srcx+y,tmpx=desx+desy;x<=srcx2+y;x++,tmpx++) 

#define XDEC    for (x=srcx2+y,tmpx=desx2+desy;x>=srcx+y;x--,tmpx--) 

#define MM	des->Bitmap[tmpx] = src->Bitmap[x];

#define MV	DGLSetSegment((x>>12)&0xf0);\
		des->Bitmap[tmpx] = src->Bitmap[x&0xffff]; 

#define VM      DGLSetSegment(tmpx>>16);\
	        des->Bitmap[tmpx&0xffff] = src->Bitmap[x];

#define VV      DGLSetSegment(tmpx>>16|((x>>12)&0xf0));\
                des->Bitmap[tmpx&0xffff]=src->Bitmap[x&0xffff];

#define MMM     if (mask->Bitmap[x]) { MM }
#define MMV     if (mask->Bitmap[x]) { MV }
#define MVM     if (mask->Bitmap[x]) { VM }
#define MVV     if (mask->Bitmap[x]) { VV }
		

DGLBitmapCopy(des, desx, desy, src, srcx, srcy, width, height, mask)
DGLBitmap *des;
int desx, desy;
DGLBitmap *src;
int srcx, srcy;
int width, height;
DGLBitmap *mask;
{
int x, y, srcx2, srcy2, desx2, desy2, tmpx, mode;

  /*
   *  Check for coordinates out of bounds 
   */

  if (srcx>src->Xsize||srcy>src->Ysize||desy>des->Xsize||desy>des->Ysize)
    return -1;
  if (srcx < 0) {
    width=width+srcx; desx-=srcx; srcx=0;
  }
  if (srcy < 0) {
    height=height+srcy; desy-=srcy; srcy=0;
  }
  if (desx < 0) {
    width=width+desx; srcx-=desx; desx=0;
  }
  if (desy < 0) {
    height=height+desy; srcy-=desy; desy=0;
  }

  /*
   *  Check and adjust too big areas
   */
  if (srcx+width > src->Xsize) width=src->Xsize-srcx;
  if (srcy+height > src->Ysize) height=src->Ysize-srcy;
  if (desx+width > des->Xsize) width=des->Xsize-desx;
  if (desy+height > des->Ysize) height=des->Ysize-desy;

  width -= 1; height -= 1; 
  srcx2 = srcx+width; srcy2 = srcy+height;
  desx2 = desx+width; desy2 = desy+height;

  mode = 0;

  if (DGLSwitchPending) DGLSwitchAllow();

  if (src->Bitmap == des->Bitmap) {
    if (desy > srcy) mode |= UP; else mode |= DOWN; 
    if (desx > srcx) mode |= LEFT; else mode |= RIGHT;
  }
  else
    mode |= RIGHT | DOWN;

  if (src->Type == VIDBUF) mode |= SRCVID; else mode |= SRCMEM;

  if (des->Type == VIDBUF) mode |= DESVID; else mode |= DESMEM;

  if (mask) mode |= USEMASK;

  switch (mode) {
    case (RIGHT | DOWN | DESMEM | SRCMEM):
      YINC { XINC { MM } } break;
    case (RIGHT | DOWN | DESMEM | SRCVID):
      YINC { XINC { MV } } break;
    case (RIGHT | DOWN | DESVID | SRCMEM):
      YINC { XINC { VM } } break;
    case (RIGHT | DOWN | DESVID | SRCVID):
      YINC { XINC { VV } } break;
    case (RIGHT | DOWN | DESMEM | SRCMEM | USEMASK):
      YINC { XINC { MMM } } break;
    case (RIGHT | DOWN | DESMEM | SRCVID | USEMASK):
      YINC { XINC { MMV } } break;
    case (RIGHT | DOWN | DESVID | SRCMEM | USEMASK):
      YINC { XINC { MVM } } break;
    case (RIGHT | DOWN | DESVID | SRCVID | USEMASK):
      YINC { XINC { MVV } } break;
    
    case (RIGHT | UP | DESMEM | SRCMEM):
      YDEC { XINC { MM } } break;
    case (RIGHT | UP | DESMEM | SRCVID):
      YDEC { XINC { MV } } break;
    case (RIGHT | UP | DESVID | SRCMEM):
      YDEC { XINC { VM } } break;
    case (RIGHT | UP | DESVID | SRCVID):
      YDEC { XINC { VV } } break;
    case (RIGHT | UP | DESMEM | SRCMEM | USEMASK):
      YDEC { XINC { MMM } } break;
    case (RIGHT | UP | DESMEM | SRCVID | USEMASK):
      YDEC { XINC { MMV } } break;
    case (RIGHT | UP | DESVID | SRCMEM | USEMASK):
      YDEC { XINC { MVM } } break;
    case (RIGHT | UP | DESVID | SRCVID | USEMASK):
      YDEC { XINC { MVV } } break;

    case (LEFT | DOWN | DESMEM | SRCMEM):
      YINC { XDEC { MM } } break;
    case (LEFT | DOWN | DESMEM | SRCVID):
      YINC { XDEC { MV } } break;
    case (LEFT | DOWN | DESVID | SRCMEM):
      YINC { XDEC { VM } } break;
    case (LEFT | DOWN | DESVID | SRCVID):
      YINC { XDEC { VV } } break;
    case (LEFT | DOWN | DESMEM | SRCMEM | USEMASK):
      YINC { XDEC { MMM } } break;
    case (LEFT | DOWN | DESMEM | SRCVID | USEMASK):
      YINC { XDEC { MMV } } break;
    case (LEFT | DOWN | DESVID | SRCMEM | USEMASK):
      YINC { XDEC { MVM } } break;
    case (LEFT | DOWN | DESVID | SRCVID | USEMASK):
      YINC { XDEC { MVV } } break;

    case (LEFT | UP | DESMEM | SRCMEM):
      YDEC { XDEC { MM } } break;
    case (LEFT | UP | DESMEM | SRCVID):
      YDEC { XDEC { MV } } break;
    case (LEFT | UP | DESVID | SRCMEM):
      YDEC { XDEC { VM } } break;
    case (LEFT | UP | DESVID | SRCVID):
      YDEC { XDEC { VV } } break;
    case (LEFT | UP | DESMEM | SRCMEM | USEMASK):
      YDEC { XDEC { MMM } } break;
    case (LEFT | UP | DESMEM | SRCVID | USEMASK):
      YDEC { XDEC { MMV } } break;
    case (LEFT | UP | DESVID | SRCMEM | USEMASK):
      YDEC { XDEC { MVM } } break;
    case (LEFT | UP | DESVID | SRCVID | USEMASK):
      YDEC { XDEC { MVV } } break;
  }
  return 0;
}

DGLBitmapClear(des, desx, desy, src, srcx, srcy, width, height, mask, color)
DGLBitmap *des;
int desx, desy;
DGLBitmap *src;
int srcx, srcy;
int width, height;
DGLBitmap *mask;
char color;
{
int x, y, srcx2, srcy2, desx2, desy2, tmpx, mode;

  /*
   *  Check for coordinates out of bounds 
   */
  if (srcx>src->Xsize||srcy>src->Ysize||desy>des->Xsize||desy>des->Ysize)
    return -1;
  if (srcx < 0) {
    width=width+srcx; desx-=srcx; srcx=0;
  }
  if (srcy < 0) {
    height=height+srcy; desy-=srcy; srcy=0;
  }
  if (desx < 0) {
    width=width+desx; srcx-=desx; desx=0;
  }
  if (desy < 0) {
    height=height+desy; srcy-=desy; desy=0;
  }

  /*
   *  Check and adjust too big areas
   */
  if (srcx+width > src->Xsize) width=src->Xsize-srcx;
  if (srcy+height > src->Ysize) height=src->Ysize-srcy;
  if (desx+width > des->Xsize) width=des->Xsize-desx;
  if (desy+height > des->Ysize) height=des->Ysize-desy;

  width -= 1; height -= 1; 

  srcx2 = srcx+width; srcy2 = srcy+height;
  desx2 = desx+width; desy2 = desy+height;


  if (DGLSwitchPending) DGLSwitchAllow();

  if (des->Type == VIDBUF) mode = DESVID;
  else mode = DESMEM;

  if (mask) mode |= USEMASK;

  switch (mode) {
    case (DESMEM):
      YINC { XINC {
		    des->Bitmap[tmpx] = color; 
		  }
	   } break;
    case (DESMEM|USEMASK):
      YINC { XINC { 
		    if (mask->Bitmap[x])
		      des->Bitmap[tmpx] = color;
		  } 
	   } break;
    case (DESVID):
      YINC { XINC {
                    DGLSetSegment(tmpx>>16);
                    des->Bitmap[tmpx&0xffff] = color;
                  }
	   } break;
    case (DESVID|USEMASK):
      YINC { XINC {
                    if (mask->Bitmap[x]) {
                      DGLSetSegment(tmpx>>16);
                      des->Bitmap[tmpx&0xffff] = color;
                    }
		  }
	   } break;
  }
}

DGLBitmap *DGLBitmapCreate(Xdim, Ydim)
int Xdim, Ydim;
{
DGLBitmap *B;

  if ((B=(DGLBitmap*)malloc(sizeof(DGLBitmap))) == (DGLBitmap*)0)
    return (DGLBitmap*)0;
  if ((B->Bitmap=(char*)malloc(Xdim*Ydim)) == (char*)0) {
    free(B);
    return (DGLBitmap*)0;
  }
  memset(B->Bitmap, 0x00, Xdim*Ydim);
  B->Xsize = Xdim;
  B->Ysize = Ydim;
  B->Type = MEMBUF;
  return B;
}

DGLBitmapDestroy(Bitmap)
DGLBitmap *Bitmap;
{
 if (Bitmap->Type == MEMBUF) 
   free(Bitmap->Bitmap);
 free(Bitmap);
}

