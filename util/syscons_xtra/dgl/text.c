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

#include <stdio.h>
#include "dgl.h"

/*
 *  Local variables
 */
static DGLText		*DGLTextFont = 0;


DGLTextSetFontFile(filename)
char *filename;
{
FILE *fd;

  if (DGLTextFont) {
    if (DGLTextFont->BitmapArray) 
      free (DGLTextFont->BitmapArray);
    free(DGLTextFont);
  }

  if ((DGLTextFont=(DGLText*)malloc(sizeof(DGLText))) == (DGLText*)0)
	return 1;

  if (filename==NULL) {
    if ((fd=fopen("/usr/lib/vidi/font8x16", "r"))==(FILE*)0) 
      return 1;
    DGLTextFont->Type = 0;
    DGLTextFont->Width = 8;
    DGLTextFont->Height = 16;
  }
  else {
    if ((fd=fopen(filename, "r"))==(FILE*)0)
      return 1;
    fread(&DGLTextFont->Type, 1 , 1, fd);
    fread(&DGLTextFont->Width, 1 , 1, fd);
    fread(&DGLTextFont->Height, 1 , 1, fd);
  }

  DGLTextFont->BitmapArray = 
    (char*)malloc(256*DGLTextFont->Width*DGLTextFont->Height);

  fread(DGLTextFont->BitmapArray, 1, 
    (256*DGLTextFont->Width* DGLTextFont->Height), fd);

  fclose(fd);
  return 0;
}

DGLBitmapPutChar(Object, x, y, ch, fgcol, bgcol, fill, dir)
DGLBitmap *Object;
int x, y;
char ch, fgcol, bgcol;
int fill, dir;
{
int lin, bit, biton;

  for(lin = 0; lin < DGLTextFont->Height; lin++) {
    for(bit = 0; bit < DGLTextFont->Width; bit++) {
      if (DGLTextFont->BitmapArray[(((unsigned char)ch<<4)+lin)]&(1<<bit))
        switch (dir) {
          case 0:
            DGLSetXY(Object, (x+7-bit), (y+lin), fgcol);
            break;
 
          case 1:
            DGLSetXY(Object, (x+lin), (y-7+bit), fgcol);
            break;
    
          case 2:
            DGLSetXY(Object, (x-7+bit), (y-lin), fgcol);
            break;
   
          case 3:
            DGLSetXY(Object, (x-lin), (y+7-bit), fgcol);
            break;
  
          case 4:
            DGLSetXY(Object, (x+lin+7-bit), (y+lin+bit), fgcol);
            break;
        }
      else if (fill)
        switch (dir) {
          case 0:
            DGLSetXY(Object, (x+7-bit), (y+lin), bgcol);
            break;
  
          case 1:
            DGLSetXY(Object, (x+lin), (y-7+bit), bgcol);
    	    break;
    
          case 2:
            DGLSetXY(Object, (x-7+bit), (y-lin), bgcol);
            break;
   
          case 3:
            DGLSetXY(Object, (x-lin), (y+7-bit), bgcol);
            break;
   
          case 4:
            DGLSetXY(Object, (x+lin+7-bit), (y+lin+bit), bgcol);
            break;
        }
    }
  }
}

DGLBitmapString(Object, x, y, str, fgcol, bgcol, fill, dir)
DGLBitmap *Object;
int x, y;
char *str, fgcol, bgcol;
int fill, dir;
{
int pos;

  for (pos=0; pos<strlen(str); pos++) {
    switch (dir) {

      case 0:
        DGLBitmapPutChar(Object, x+(pos*DGLTextFont->Width), y, 
                         str[pos], fgcol, bgcol, fill, dir);
	break;

      case 1:
        DGLBitmapPutChar(Object, x, y-(pos*DGLTextFont->Width), 
		         str[pos], fgcol, bgcol, fill, dir);
	break;

      case 2:
        DGLBitmapPutChar(Object, x-(pos*DGLTextFont->Width), y, 
		         str[pos], fgcol, bgcol, fill, dir);
	break;

      case 3:
        DGLBitmapPutChar(Object, x, y+(pos*DGLTextFont->Width),  
		         str[pos], fgcol, bgcol, fill, dir);
	break;

      case 4:
        DGLBitmapPutChar(Object, x+(pos*DGLTextFont->Width),
                         y-(pos*DGLTextFont->Width), 
		         str[pos], fgcol, bgcol, fill, dir);
	break;
    }

  }
}

DGLWidgetString(Widget, x, y, str, fgcol, bgcol, fill, dir)
DGLWidget *Widget;
int x, y;
char *str, fgcol, bgcol;
int fill, dir;
{
  DGLBitmapString(Widget->Object, x, y, str, fgcol, bgcol, fill, dir);
  DGLWidgetUpdate(Widget);
}
