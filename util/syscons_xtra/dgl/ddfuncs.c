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
#include <sys/ioctl.h>
#include <sys/console.h>
#include "dgl.h"
#include "driver.h"

/*
 * local variables
 */
static char *DGLIFontArray;


DGLDisplayOffset(offset)
int offset;
{
  if (DGLSwitchPending) 
    DGLSwitchAllow();

  offset>>=2;
  SvgaGraphicsMode->CRTC[0x0C] = (offset&0x0FF00)>>8;
  SvgaGraphicsMode->CRTC[0x0D] = (offset&0x000FF);
  SvgaGraphicsMode->CRTC[0x19] = (offset&0x30000)>>16;
  outb(0x3BF, 0x03); outb(0x3D8, 0xA0);	/* unlock ET4000 ext. regs */
  outb(0x3D4, 0x0C); outb(0x3D5, SvgaGraphicsMode->CRTC[0x0C]);
  outb(0x3D4, 0x0D); outb(0x3D5, SvgaGraphicsMode->CRTC[0x0D]);
  outb(0x3D4, 0x33); outb(0x3D5, SvgaGraphicsMode->CRTC[0x19]);
  outb(0x3BF, 0x01); outb(0x3D8, 0xA0);	/* lock ET4000 ext. regs */
}

DGLSetPalette(red, green, blue)
char *red, *green, *blue;
{
int i ;
  
  for (i=0x00; i<0x100; i++) {		/* save in graphics_mode as well */
    SvgaGraphicsMode->LUT[i*3+0] = red[i];
    SvgaGraphicsMode->LUT[i*3+1] = green[i];
    SvgaGraphicsMode->LUT[i*3+2] = blue[i];
  }

  if (DGLSwitchPending) 
    DGLSwitchAllow();

  outb(0x3C6, 0xFF);			/* no pixel mask */
  outb(0x3C8, 0x00);
  for (i=0x00; i<0x300; i++) {		/* setup Palette registers */
    outb(0x3C9, SvgaGraphicsMode->LUT[i]);
  }
  inb(0x3DA); 				/* reset flip-flop */
  outb(0x3C0, 0x20);			/* enable Palette */
}

DGLSetPaletteIndex(color, red, green, blue)
char color, red, green, blue;
{
  SvgaGraphicsMode->LUT[color*3+0] = red;
  SvgaGraphicsMode->LUT[color*3+1] = green;
  SvgaGraphicsMode->LUT[color*3+2] = blue;

  if (DGLSwitchPending) 
    DGLSwitchAllow();

  outb(0x3C6, 0xFF);			/* no pixel mask */
  outb(0x3C8, color); 
  outb(0x3C9, red); outb(0x3C9, green); outb(0x3C9, blue);
  inb(0x3DA); 				/* reset flip-flop */
  outb(0x3C0, 0x20);			/* enable Palette */
}

DGLSetBorder(color)
char color;
{
  SvgaGraphicsMode->ATC[0x11] = color;

  if (DGLSwitchPending) 
    DGLSwitchAllow();

  inb(0x3DA); 				/* reset flip-flop */
  outb(0x3C0,0x11); outb(0x3C0, color); 
  inb(0x3DA); 				/* reset flip-flop */
  outb(0x3C0, 0x20);			/* enable Palette */
}

DGLBlankDisplay(on)
int on;
{
unsigned char val;

  if (DGLSwitchPending) 
    DGLSwitchAllow();

  outb(0x3C4, 0x01); val = inb(0x3C5); outb(0x3C4, 0x01);
  outb(0x3C5, ((on) ? (val |= 0x20) : (val &= 0xDF)));
}

DGLClear(object, color)
DGLBitmap *object;
char color;
{
int i, *ptr, value=color|color<<8; value|=value<<16;

  if (DGLSwitchPending)
    DGLSwitchAllow();

  if (object->Type == VIDBUF) {
    ptr=(int*)object->Bitmap;
      for (i=0; i<((object->Xsize*object->Ysize)>>2); i++) {
        DGLSetSegment(i>>14);
        ptr[i&0x3fff] = value;
      }
  } 
  else {
    for (i=0; i<(object->Xsize*object->Ysize); i++) {
      object->Bitmap[i] = color;
    }
  }
}

DGLBeep(duration, tone)
{
  ioctl(0, KIOCSOUND, tone);
  usleep(duration);
  ioctl(0, KIOCSOUND, 0);
}

