
/*
 * bltGrMisc.c --
 *
 *	This module implements a graph widget for the
 *	Tk toolkit.
 *
 * Copyright 1991-1994 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

#include "blt.h"
#include "bltGraph.h"
#include <X11/Xutil.h>

extern int strcasecmp _ANSI_ARGS_((CONST char *s1, CONST char *s2));

static int ParsePosition _ANSI_ARGS_((ClientData, Tcl_Interp *, Tk_Window,
	char *, char *, int));
static char *PrintPosition _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltPositionOption =
{
    ParsePosition, PrintPosition, (ClientData)0
};

static int ParseAxisFlags _ANSI_ARGS_((ClientData, Tcl_Interp *, Tk_Window,
	char *, char *, int));
static char *PrintAxisFlags _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltXAxisFlagsOption =
{
    ParseAxisFlags, PrintAxisFlags, (ClientData)ANY_X_MASK
};

Tk_CustomOption bltYAxisFlagsOption =
{
    ParseAxisFlags, PrintAxisFlags, (ClientData)ANY_Y_MASK
};


/* ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */

int
Blt_GetPosition(interp, string, pointPtr)
    Tcl_Interp *interp;
    char *string;
    XPoint *pointPtr;
{
    char *strX, *strY;
    int x, y;

    if ((string == NULL) || (*string == '\0')) {
	pointPtr->y = pointPtr->x = DEF_POSITION;
	return TCL_OK;		/* Position marked as default */
    }
    if (*string != '@') {
	Tcl_AppendResult(interp, "bad position \"", string,
	    "\": must start with an '@'", (char *)NULL);
	return TCL_ERROR;
    }
    strX = string + 1;
    strY = strchr(strX, ',');
    if (strY == NULL) {
	Tcl_AppendResult(interp, "bad position \"", string,
	    "\": should be \"@x,y\"", (char *)NULL);
	return TCL_ERROR;
    }
    *strY++ = '\0';
    if ((Tcl_GetInt(interp, strX, &x) != TCL_OK) ||
	(Tcl_GetInt(interp, strY, &y) != TCL_OK)) {
	return TCL_ERROR;
    }
    pointPtr->x = x, pointPtr->y = y;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ParsePosition --
 *
 *	Convert the string representation of a legend XY position into
 *	window coordinates.  The form of the string must be "@x,y" or
 *	none.
 *
 * Results:
 *	The return value is a standard Tcl result.  The symbol type is
 *	written into the widget record.
 *
 * Side Effects:
 *	If no legend position is given, the right margin of the graph
 *	will be automatically increased to hold the legend.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParsePosition(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* New legend position string */
    char *widgRec;		/* Widget record */
    int offset;			/* offset to XPoint structure */
{
    XPoint *pointPtr = (XPoint *)(widgRec + offset);
    return (Blt_GetPosition(interp, value, pointPtr));
}

/*
 *----------------------------------------------------------------------
 *
 * PrintPosition --
 *
 *	Convert the window coordinates into a string.
 *
 * Results:
 *	The string representing the coordinate position is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintPosition(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of XPoint in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    char *result;
    XPoint *pointPtr = (XPoint *)(widgRec + offset);

    result = "";
    if (pointPtr->x != DEF_POSITION) {
	char string[200];

	sprintf(string, "@%d,%d", pointPtr->x, pointPtr->y);
	result = strdup(string);
	if (result == NULL) {
	    return (strerror(errno));
	}
	*freeProcPtr = TCL_DYNAMIC;
    }
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * ParseAxisFlags --
 *
 *	Convert the string indicating which axis flags to use.
 *
 * Results:
 *	The return value is a standard Tcl result.  The axis flags are
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseAxisFlags(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* Axis type */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of AxisType field */
{
    int *flagsPtr = (int *)(widgRec + offset);
    int mask = (int)clientData;
    int flags;

    flags = 0;
    if (mask == ANY_X_MASK) {
	if (strcmp(value, "x2") == 0) {
	    flags = X2_AXIS_MASK;
	} else if (strcmp(value, "x1") == 0) {
	    flags = X1_AXIS_MASK;
	} else if (strcmp(value, "x") == 0) {
	    flags = X1_AXIS_MASK;
	} else if (strcmp(value, "both") == 0) {
	    flags = (X1_AXIS_MASK | X2_AXIS_MASK);
	} else {
	    Tcl_AppendResult(interp, "bad x-axis type \"", value,
		"\": should be x or x2", (char *)NULL);
	    return TCL_ERROR;
	}
    } else {
	if (strcmp(value, "y2") == 0) {
	    flags = Y2_AXIS_MASK;
	} else if (strcmp(value, "y1") == 0) {
	    flags = Y1_AXIS_MASK;
	} else if (strcmp(value, "y") == 0) {
	    flags = Y1_AXIS_MASK;
	} else if (strcmp(value, "both") == 0) {
	    flags = (Y1_AXIS_MASK | Y2_AXIS_MASK);
	} else {
	    Tcl_AppendResult(interp, "bad y-axis type \"", value,
		"\": should be y or y2", (char *)NULL);
	    return TCL_ERROR;
	}
    }
    *flagsPtr &= ~mask;
    *flagsPtr |= flags;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintAxisFlags --
 *
 *	Convert the window coordinates into a string.
 *
 * Results:
 *	The string representing the coordinate position is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintAxisFlags(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of AxisType field */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    char *result;
    int flags = *(int *)(widgRec + offset);
    int mask = (int)clientData;

    flags &= mask;
    result = "";
    if ((flags == ANY_X_MASK) || (flags == ANY_Y_MASK)) {
	result = "both";
    } else if (flags & X1_AXIS_MASK) {
	result = "x";
    } else if (flags & X2_AXIS_MASK) {
	result = "x2";
    } else if (flags & Y1_AXIS_MASK) {
	result = "y";
    } else if (flags & Y2_AXIS_MASK) {
	result = "y2";
    }
    return (result);
}

/*-----------------------------------------------------------------
 * X-related drawing routines
 * -----------------------------------------------------------------
 */

/*
 * -----------------------------------------------------------------
 *
 * Blt_TextStringWidth
 *
 *	Returns the width in pixels of a text string.
 *
 * -----------------------------------------------------------------
 */
unsigned int
Blt_TextStringWidth(fontPtr, text)
    XFontStruct *fontPtr;	/* Font information */
    char *text;			/* Text string */
{
    XCharStruct bbox;		/* Bounding box for text string */
    int dummy;			/* not used */

    /* Get the width and height of the text string to be created */

    XTextExtents(fontPtr, text, strlen(text), &dummy, &dummy, &dummy, &bbox);
    return (bbox.rbearing + bbox.lbearing);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_TranslateTextCoords --
 *
 * 	Translate the coordinates of a given text string based upon
 *	the anchor specified.  The anchor indicates where the given
 *	xy position are in relation to the text bounding box.
 *
 *  		nw --- n --- ne
 *  		|            |
 *  		w   center   e
 *  		|            |
 *  		sw --- s --- se
 *
 * 	The coordinates returned are translated to the baseline
 * 	origin of the text bounding box (suitable for giving to
 * 	XDrawString, XDrawText, etc).
 *
 * Results:
 *	The translated text coordinates are returned.
 *
 * -----------------------------------------------------------------
 */
XPoint
Blt_TranslateTextCoords(fontPtr, text, x, y, anchor)
    XFontStruct *fontPtr;	/* Font information */
    char *text;			/* Text string */
    int x, y;			/* Position of anchor */
    Tk_Anchor anchor;		/* Direction of the anchor */
{
    int width;
    XPoint newPt;

    /*
     * Get the width the text string to be created. Height is assumed to be
     * the sum of font ascent and descent.
     */

    width = Blt_TextStringWidth(fontPtr, text);

    newPt.x = x, newPt.y = y;
    switch (anchor) {
    case TK_ANCHOR_NW:
	newPt.y += fontPtr->ascent;
	break;
    case TK_ANCHOR_W:
	newPt.y += (fontPtr->ascent - fontPtr->descent) / 2;
	break;
    case TK_ANCHOR_SW:
	newPt.y -= fontPtr->descent;
	break;
    case TK_ANCHOR_NE:
	newPt.x -= width;
	newPt.y += fontPtr->ascent;
	break;
    case TK_ANCHOR_E:
	newPt.x -= width;
	newPt.y += (fontPtr->ascent - fontPtr->descent) / 2;
	break;
    case TK_ANCHOR_SE:
	newPt.x -= width;
	newPt.y -= fontPtr->descent;
	break;
    case TK_ANCHOR_N:
	newPt.y += fontPtr->ascent;
	newPt.x -= width / 2;
	break;
    case TK_ANCHOR_S:
	newPt.y -= fontPtr->descent;
	newPt.x -= width / 2;
	break;
    case TK_ANCHOR_CENTER:
	newPt.x -= width / 2;
	newPt.y += (fontPtr->ascent - fontPtr->descent) / 2;
	break;
    }
    return (newPt);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_GetBoundingBox
 *
 * 	Computes the size the bounding box of a rotated rectangle, given
 *	by the width, height, and rotation.  The rectangle corners are
 *	simply rotated and the min and max x,y coordinates are determined.
 *      The rectangle is centered at 0,0.  Point 0 is at -w/2, -h/2.
 *	Point 1 is at w/2, -h/2, etc.
 *
 *  		0 ------- 1
 *  		|         |
 *  		|    x    |
 *  		|         |
 *  		3 ------- 2
 *
 * Results:
 *	The width and height of the bounding box containing the
 *	rotated rectangle are returned.
 *
 * -----------------------------------------------------------------
 */
void
Blt_GetBoundingBox(width, height, theta, rotWPtr, rotHPtr, pointArr)
    unsigned int width, height;	/* Unrotated region */
    double theta;		/* Rotation of box */
    unsigned int *rotWPtr, *rotHPtr;	/* Rotated bounding box region */
    XPoint *pointArr;		/* Points of the rotated box */
{
    register int i;
    double sinTheta, cosTheta;
    double maxX, maxY;
    register double x, y;
    struct Coord {
	double x, y;
    } corner[4];

    /*
     * Set the four corners of the rectangle whose center is the origin
     */

    corner[1].x = corner[2].x = width * 0.5;
    corner[0].x = corner[3].x = -corner[1].x;
    corner[2].y = corner[3].y = height * 0.5;
    corner[0].y = corner[1].y = -corner[2].y;

    theta = (-theta / 180.0) * M_PI;
    sinTheta = sin(theta), cosTheta = cos(theta);
    maxX = maxY = 0.0;

    /*
     * Rotate the four corners and find the maximum X and Y coordinates
     */

    for (i = 0; i < 4; i++) {
	x = (corner[i].x * cosTheta) - (corner[i].y * sinTheta);
	y = (corner[i].x * sinTheta) + (corner[i].y * cosTheta);
	if (x > maxX) {
	    maxX = x;
	}
	if (y > maxY) {
	    maxY = y;
	}
	if (pointArr != NULL) {
	    pointArr[i].x = BLT_RND(x);
	    pointArr[i].y = BLT_RND(y);
	}
    }

    /*
     * By symmetry, the width and height of the bounding box are
     * twice the maximum x and y coordinates.
     */

    *rotWPtr = (int)((maxX + maxX) + 0.5);
    *rotHPtr = (int)((maxY + maxY) + 0.5);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_TranslateBoxCoords --
 *
 * 	Translate the coordinates of a given bounding box based
 *	upon the anchor specified.  The anchor indicates where
 *	the given xy position is in relation to the bounding box.
 *
 *  		nw --- n --- ne
 *  		|            |
 *  		w   center   e
 *  		|            |
 *  		sw --- s --- se
 *
 * 	The coordinates returned are translated to the origin of the
 * 	bounding box (suitable for giving to XCopyArea, etc.)
 *
 * Results:
 *	The translated coordinates of the bounding box are returned.
 *
 * -----------------------------------------------------------------
 */
XPoint
Blt_TranslateBoxCoords(x, y, width, height, anchor)
    int x, y;			/* Window coordinates of anchor */
    unsigned int width, height;	/* Extents of the bounding box */
    Tk_Anchor anchor;		/* Direction of the anchor */
{
    XPoint newPt;

    newPt.x = x, newPt.y = y;
    switch (anchor) {
    case TK_ANCHOR_NW:		/* Upper left corner */
	break;
    case TK_ANCHOR_W:		/* Left center */
	newPt.y -= (height / 2);
	break;
    case TK_ANCHOR_SW:		/* Lower left corner */
	newPt.y -= height;
	break;
    case TK_ANCHOR_N:		/* Top center */
	newPt.x -= (width / 2);
	break;
    case TK_ANCHOR_CENTER:	/* Centered */
	newPt.x -= (width / 2);
	newPt.y -= (height / 2);
	break;
    case TK_ANCHOR_S:		/* Bottom center */
	newPt.x -= (width / 2);
	newPt.y -= height;
	break;
    case TK_ANCHOR_NE:		/* Upper right corner */
	newPt.x -= width;
	break;
    case TK_ANCHOR_E:		/* Right center */
	newPt.x -= width;
	newPt.y -= (height / 2);
	break;
    case TK_ANCHOR_SE:		/* Lower right corner */
	newPt.x -= width;
	newPt.y -= height;
	break;
    }
    return (newPt);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_RotateBitmap --
 *
 *	Creates a new bitmap containing the rotated image of the given
 *	bitmap.  We also need a special GC of depth 1, so that we do
 *	not need to rotate more than one plane of the bitmap.
 *
 * Results:
 *	Returns a new bitmap containing the rotated image.
 *
 * -----------------------------------------------------------------
 */
Pixmap
Blt_RotateBitmap(display, draw, bitmapGC, srcBitmap, srcWidth, srcHeight,
    theta, rotWPtr, rotHPtr)
    Display *display;		/* X display */
    Drawable draw;		/* Root window drawable */
    GC bitmapGC;		/* GC created from bitmap where fg=1,bg=0 */
    Pixmap srcBitmap;		/* Source bitmap to be rotated */
    unsigned int srcWidth;	/* Width and height of the source bitmap */
    unsigned int srcHeight;
    double theta;		/* Right angle rotation to perform */
    unsigned int *rotWPtr, *rotHPtr;
{
    XImage *src, *dest;
    Pixmap destBitmap;
    unsigned int destWidth, destHeight;
    register int dx, dy;	/* Destination bitmap coordinates */
    register int sx, sy;	/* Source bitmap coordinates */
    double transX, transY;
    double rotX, rotY;
    double radians, sinTheta, cosTheta;
    unsigned long pixel;
    double srcX, srcY;		/* Center of source rectangle */
    double destX, destY;	/* Center of destination rectangle */

    /*
     * Create a bitmap and image big enough to contain the rotated text
     */

    Blt_GetBoundingBox(srcWidth, srcHeight, theta, &destWidth, &destHeight,
	(XPoint *)NULL);
    destBitmap = XCreatePixmap(display, draw, destWidth, destHeight, 1);
    XSetForeground(display, bitmapGC, 0x0);
    XFillRectangle(display, destBitmap, bitmapGC, 0, 0, destWidth, destHeight);
    src = XGetImage(display, srcBitmap, 0, 0, srcWidth, srcHeight, 1, ZPixmap);
    dest = XGetImage(display, destBitmap, 0, 0, destWidth, destHeight, 1,
	ZPixmap);
    radians = (theta / 180.0) * M_PI;
    sinTheta = sin(radians), cosTheta = cos(radians);
    /*
     * Coordinates of the centers of the source and destination rectangles
     */
    srcX = srcWidth * 0.5;
    srcY = srcHeight * 0.5;
    destX = destWidth * 0.5;
    destY = destHeight * 0.5;
    /*
     * Rotate each pixel of dest image, placing results in source image
     */
    for (dx = 0; dx < destWidth; dx++) {
	for (dy = 0; dy < destHeight; dy++) {
	    if (theta == 270.0) {
		sx = dy, sy = destWidth - dx - 1;
	    } else if (theta == 180.0) {
		sx = destWidth - dx - 1, sy = destHeight - dy - 1;
	    } else if (theta == 90.0) {
		sx = destHeight - dy - 1, sy = dx;
	    } else if (theta == 0.0) {
		sx = dx, sy = dy;
	    } else {

		/* Translate origin to center of destination image */

		transX = dx - destX;
		transY = dy - destY;

		/* Rotate the coordinates about the origin */

		rotX = (transX * cosTheta) - (transY * sinTheta);
		rotY = (transX * sinTheta) + (transY * cosTheta);

		/* Translate back to the center of the source image */
		rotX += srcX;
		rotY += srcY;

		sx = BLT_RND(rotX);
		sy = BLT_RND(rotY);

		/*
		 * Verify the coordinates, since the destination image can be
		 * bigger than the source
		 */

		if ((sx >= srcWidth) || (sx < 0) || (sy >= srcHeight) ||
		    (sy < 0)) {
		    continue;
		}
	    }
	    pixel = XGetPixel(src, sx, sy);
	    if (pixel) {
		XPutPixel(dest, dx, dy, pixel);
	    }
	}
    }

    /* Write the rotated image into the destination bitmap */

    XPutImage(display, destBitmap, bitmapGC, dest, 0, 0, 0, 0, destWidth,
	destHeight);

    /* Clean up temporary resources used */

    XDestroyImage(src), XDestroyImage(dest);
    *rotWPtr = destWidth;
    *rotHPtr = destHeight;
    return (destBitmap);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_CreateTextBitmap --
 *
 *	Draw a bitmap, using the the given window coordinates
 *	as an anchor for the text bounding box.
 *
 * Results:
 *	Returns the bitmap representing the text string.
 *
 * Side Effects:
 *	Bitmap is drawn using the given font and GC on the graph
 *	window at the given coordinates, anchor, and rotation.
 *
 * -----------------------------------------------------------------
 */
Pixmap
Blt_CreateTextBitmap(display, draw, fontPtr, text, theta, widthPtr, heightPtr)
    Display *display;
    Drawable draw;
    XFontStruct *fontPtr;	/* Font to draw bitmap */
    char *text;			/* Text string to draw */
    double theta;		/* Desired rotation of bitmap */
    unsigned int *widthPtr, *heightPtr;	/* Height of text bitmap */
{
    unsigned int width, height;	/* Width and height of text bounding box */
    Pixmap bitmap;
    XGCValues gcValues;
    unsigned long gcMask;
    GC bitmapGC;
    int numChar;		/* Size of text string */

    numChar = strlen(text);

    /*
     * Determine the width and height of the text bitmap to be created
     */

    width = Blt_TextStringWidth(fontPtr, text);
    height = (unsigned int)TEXTHEIGHT(fontPtr);

    /*
     * Create a temporary bitmap and draw the text string into it
     */

    bitmap = XCreatePixmap(display, draw, width, height, 1);
    gcValues.font = fontPtr->fid;
    gcValues.foreground = gcValues.background = 0;
    gcMask = (GCFont | GCForeground | GCBackground);
    bitmapGC = XCreateGC(display, bitmap, gcMask, &gcValues);

    /*
     * Clear the bitmap before drawing the text string into it.
     * (XDrawImageString is unreliable to draw all bits)
     */

    XFillRectangle(display, bitmap, bitmapGC, 0, 0, width, height);
    XSetForeground(display, bitmapGC, 1);
    XDrawString(display, bitmap, bitmapGC, 0, fontPtr->ascent, text, numChar);
    if (theta == 0.0) {
	*widthPtr = width, *heightPtr = height;
    } else {
	Pixmap rotBitmap;

	rotBitmap = Blt_RotateBitmap(display, draw, bitmapGC, bitmap, width,
	    height, theta, widthPtr, heightPtr);
	XFreePixmap(display, bitmap);
	bitmap = rotBitmap;
    }
    XFreeGC(display, bitmapGC);
    return (bitmap);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_StencilBitmap --
 *
 *	Stencil (draw foreground only) a bitmap at the given window
 *      coordinates.  This routine should be used sparingly since it
 *      is very slow on most X servers.
 *
 *      Please note:  This routine assumes that the GC is NOT shared,
 * 	since it will change the FillStyle and TSOrigin fields of the GC.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Bitmap is stenciled on the graph window at the given coordinates.
 * -----------------------------------------------------------------
 */
void
Blt_StencilBitmap(display, draw, gc, bitmap, x, y, width, height)
    Display *display;
    Drawable draw;
    GC gc;			/* Graphic context to use */
    Pixmap bitmap;		/* Bitmap to be displayed */
    int x, y;			/* x, y position of bitmap */
    unsigned int width, height;
{
    XSetStipple(display, gc, bitmap);
    XSetTSOrigin(display, gc, x, y);
    XSetFillStyle(display, gc, FillStippled);
    XFillRectangle(display, draw, gc, x, y, width, height);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_DrawText --
 *
 *	Draw a text string, possibly rotated,  using the the given
 *	window coordinates as an anchor for the text bounding box.
 *
 *      Please note:  This routine assumes that for rotated (not
 *	right angles) text the GC is NOT shared, since Blt_StencilBitmap
 *	will change the FillStyle and TSOrigin fields of the GC.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Text string is drawn using the given font and GC on the
 *	graph window at the given coordinates, anchor, and rotation
 * -----------------------------------------------------------------
 */
void
Blt_DrawText(display, draw, text, attrPtr, x, y)
    Display *display;
    Drawable draw;
    char *text;
    TextAttributes *attrPtr;	/* Text attribute information */
    int x;			/* Window x coordinate */
    int y;			/* Window y coordinate */
{
    XPoint win;

    if ((text == NULL) || (*text == '\0')) {	/* Empty string, do nothing */
	return;
    }
    if (attrPtr->theta == 0.0) {/* No rotation. Handle simple case */
	int textLength = strlen(text);

	win = Blt_TranslateTextCoords(attrPtr->fontPtr, text, x, y,
	    attrPtr->anchor);
	XDrawString(display, draw, attrPtr->gc, win.x, win.y,
	    text, textLength);
    } else {
	Pixmap bitmap;
	unsigned int bmWidth, bmHeight;

	/* Create rotated bitmap of text string */

	bitmap = Blt_CreateTextBitmap(display, draw, attrPtr->fontPtr, text,
	    attrPtr->theta, &bmWidth, &bmHeight);
	win = Blt_TranslateBoxCoords(x, y, bmWidth, bmHeight,
	    attrPtr->anchor);
	if ((attrPtr->theta == 90.0) || (attrPtr->theta == 180.0) ||
	    (attrPtr->theta == 270.0)) {
	    XCopyPlane(display, bitmap, draw, attrPtr->gc, 0, 0, bmWidth,
		bmHeight, win.x, win.y, 1);
	} else {
	    Blt_StencilBitmap(display, draw, attrPtr->gc, bitmap,
		win.x, win.y, bmWidth, bmHeight);
	}
	XFreePixmap(display, bitmap);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_FindToken --
 *
 *	Linearly search through a list of strings.  Returns the
 *      the index of *key*.
 *
 * Results:
 *	Returns index of *key* if found, -1 otherwise.
 *
 * -----------------------------------------------------------------
 */

int
Blt_GetTokenIndex(tokenList, key, ignoreCase)
    char **tokenList;
    char *key;
    int ignoreCase;
{
    int length;
    register int count;
    register char **p;
    int found;
    char c;

    length = strlen(key);
    count = 0;
    c = *key;
    for (p = tokenList; *p != NULL; p++) {
	if (ignoreCase) {
	    found = (strncasecmp(key, *p, length) == 0);
	} else {
	    found = ((c == **p) && (strncmp(key, *p, length) == 0));
	}
	if (found) {
	    return (count);
	}
	count++;
    }
    return -1;
}
