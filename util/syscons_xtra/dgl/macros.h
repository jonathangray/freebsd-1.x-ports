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

extern  int DGLSwitchPending;

#define DGLSetSegment(segm) { \
	outb(0x3cd, SvgaSaveSeg = segm); \
}

#define DGLSetReadSegment(read) { \
	outb(0x3cd, SvgaSaveSeg = \
	(((read)&0x0f)<<4 | (SvgaSaveSeg&0x0f))); \
}

#define DGLSetWriteSegment(write) { \
	outb(0x3cd, SvgaSaveSeg = \
	((SvgaSaveSeg&0xf0) | ((write)&0x0f))); \
}

#define DGLSetPixel(object, offset, color) { \
	int xxi=(offset); \
	if (DGLSwitchPending) DGLSwitchAllow(); \
	if (object->Type == VIDBUF) { \
      	    DGLSetSegment((xxi>>16)); \
	    object->Bitmap[(xxi)&0xffff]=(color); \
	} else \
	    object->Bitmap[xxi]=(color); \
}

#define DGLSet4Pixel(object, offset, color) { \
	int *xxptr=(int*) object->Bitmap, xxi=(offset); \
	if (DGLSwitchPending) DGLSwitchAllow(); \
	if (object->Type == VIDBUF) { \
      	    DGLSetSegment((xxi>>14)); \
	    xxptr[(xxi)&0x3fff]=(color); \
	} else \
	    xxptr[xxi>>2]=(color); \
}

#define DGLSetXYNoClip(object, x, y, color) { \
	int offset=(((y)*object->Xsize)+(x); \
	if (DGLSwitchPending) DGLSwitchAllow(); \
	if (object->Type == VIDBUF) { \
      	    DGLSetSegment((offset>>16)); \
	    object->Bitmap[(offset)&0xffff]=(color); \
	} else \
	    object->Bitmap[offset]=(color); \
}

#define DGLSetXY(object, x, y, color) { \
	int offset; \
	if ((x)>=0 && (x)<object->Xsize && (y)>=0 && (y)<object->Ysize) { \
	    offset=(((y)*object->Xsize)+(x)); \
	    if (DGLSwitchPending) DGLSwitchAllow(); \
	    if (object->Type == VIDBUF) { \
      	        DGLSetSegment((offset>>16)); \
	        object->Bitmap[(offset)&0xffff]=(color); \
	    } else \
	        object->Bitmap[offset]=(color); \
	} \
}
