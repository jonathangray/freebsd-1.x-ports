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

#include "compiler.h"
#include "macros.h"

typedef struct {
    char 		Type;
    int  		Xsize;
    int 		Ysize;
    char 		*Bitmap;
} DGLBitmap;

/*
 * Defined Type's
 */
#define MEMBUF		0
#define VIDBUF		1
#define NOBUF		255

typedef struct DGLWidget {
    int	  	 	Status;
    int	  	 	Xpos, Ypos;
    int	  	 	Xhot, Yhot;
    DGLBitmap 	 	*Object;
    DGLBitmap 	 	*Mask;
    DGLBitmap 	 	*SaveUnder;
    int		 	(*CallBackFunction)();
    struct DGLWidget 	*Over;
    struct DGLWidget 	*Under;
} DGLWidget;

/*
 * Defined Status's
 */
#define	ACTIVE		0x0001
#define WORKING		0x0002
#define DIRTY		0x0004

#define XPOS(widget)	(widget->Xpos - widget->Xhot)
#define YPOS(widget)	(widget->Ypos - widget->Yhot)
#define XSIZ(widget)	(widget->Object->Xsize)
#define YSIZ(widget)	(widget->Object->Ysize)

typedef struct DGLText {
    char		Type;
    char		Width;
    char		Height;
    char		*BitmapArray;
} DGLText;

extern	char	 	*SvgaMem;
extern 	char		*SvgaBuf;
extern	int	 	SvgaSaveSeg;
extern	int 	 	DGLXsize, DGLYsize;
extern 	DGLBitmap 	*DGLDisplay;
extern	DGLWidget	*HeadWidget;
extern	DGLWidget	*TailWidget;

/*
 *  Video modes
 */
#define	CO256_320x200	0
#define	CO256_640x400	1
#define CO256_640x480	2
#define CO256_800x600	3
#define CO256_1024x768	4
#define CO256_1024x768a	5
#define CO256_1152x900	6
#define	HICOL_320x200	10
#define	HICOL_640x400	11
#define HICOL_640x480	12
#define HICOL_800x600	13
#define HICOL_1024x768	14
#define HICOL_1024x768a	15
#define HICOL_1152x900	16
#define	BW_320x200	20
#define	BW_640x400	21
#define BW_640x480	22
#define BW_800x600	23
#define BW_1024x768	24
#define BW_1024x768a	25
#define BW_1152x900	26

