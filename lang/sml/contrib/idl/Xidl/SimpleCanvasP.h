/*
	SimpleCanvas widget for X/idl library

	@(#)SimpleCanvasP.h	2.1 93/03/07 00:58:10

*/
#ifndef _SimpleCanvasP_h
#define _SimpleCanvasP_h

#include "SimpleCanvas.h"
/* include superclass private header file */
#include <X11/CoreP.h>
#include <X11/IntrinsicP.h>

typedef unsigned char byte;

/*
	Things  which can appear on a canvas
	(feel free to extend this ad-lib)
*/

typedef struct  ThingLine
	{       Position        startx, starty;
		Position        endx,   endy;
	}       ThingLine;

typedef struct  ThingBox
	{       Position        startx, starty;
		Dimension       width,  height;
	}       ThingBox;

typedef struct  ThingString
	{       Position        startx, starty;
		Dimension       width,  length,
				ascent, descent;
		String          string;
		byte            fontnumber;
	}       ThingString;

typedef enum    {StringType, LineType, BoxType} ThingType;

typedef union   ThingAny
	{       ThingString    string;
		ThingLine      line;
		ThingBox       box;
	}       ThingAny;

typedef struct  Thing
	{       ThingType      type;
		byte           class;   /* index into gc[] */
		ThingAny       obj;
		struct Thing   *next;
	}       Thing;

/*
	instance fields
*/
typedef struct
{       /* Resources    */
	Pixel                   foreground;
	XFontStruct             *fontinfo[256];
	XtCallbackList          expose_callback;
	XtCallbackList          offset_callback;
	XtCallbackList          extent_callback;
	/* Non-resources */
	Thing                   *everything;
	Position                offsetx,
				offsety;
	GC                      gc[256];

	/* Item selection */
	GC                      selectedgc;
	Thing                   *selected[256];  /* one item per class */

	/* Dragging       */
	Position                dragx, dragy;
	Bool                    dragging;

	/* Extent of the drawing       */
	Position                topleftx,  toplefty;
	Dimension               botrightx, botrighty;

	/* Character selection */

}       SimpleCanvasPart;


typedef struct
{
	int                     empty;
}       SimpleCanvasClassPart;

typedef struct _SimpleCanvasClassRec
{
	CoreClassPart           core_class;
	SimpleCanvasClassPart   simplecanvas_class;
}       SimpleCanvasClassRec;

extern SimpleCanvasClassRec simplecanvasClassRec;


typedef struct _SimpleCanvasRec
{
	CorePart                core;
	SimpleCanvasPart        simplecanvas;
}       SimpleCanvasRec;

#endif /* _SimpleCanvasP_h */
