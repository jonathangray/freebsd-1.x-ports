/*
 * bltGrBar.c --
 *
 *	This module implements a elements in the graph widget
 *	for the Tk toolkit.
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
#include <ctype.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>


/*
 * Sun's bundled and unbundled C compilers can't grok static function
 * typedefs (it can handle extern) like
 *
 * 	static Tk_OptionParseProc parseProc;
 *  	static Tk_OptionPrintProc printProc;
 *
 * Provide forward declarations here:
*/

static int ParseColorList _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *value, char *widgRec,
	int offset));
static char *PrintColorList _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));
static int ParseBitmapList _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *value, char *widgRec,
	int offset));
static char *PrintBitmapList _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

#include "bltGrElem.h"

extern Tk_CustomOption bltXVectorOption;
extern Tk_CustomOption bltYVectorOption;
extern Tk_CustomOption bltTwinOption;
extern Tk_CustomOption bltXAxisFlagsOption;
extern Tk_CustomOption bltYAxisFlagsOption;

static Tk_CustomOption ColorListOption =
{
    ParseColorList, PrintColorList, (ClientData)0
};
static Tk_CustomOption BitmapListOption =
{
    ParseBitmapList, PrintBitmapList, (ClientData)0
};

#define DEF_SEG_SIZE	8	/* Default size of the static array.
				 * Each element represents a segment of
				 * the bar. */

typedef struct {
    Tcl_Interp *interp;		/* Interpreter of the graph widget */
    ElementClassType type;	/* Type of element is BAR_ELEMENT */
    unsigned int flags;
    Tk_Uid id;			/* Identifier to refer the element.
				 * Used in the "insert", "delete", or
				 * "show", commands. */
    int mapped;			/* If non-zero, element is currently
				 * visible.*/
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    char *label;		/* Label displayed in legend */
    SymbolType symbol;		/* Element symbol type */
    double symbolScale;		/* Size of symbol as a percentage of
				 * the drawing area. */
    unsigned int symbolSize;	/* Computed size of symbol in pixels. */
    Vector x, y;		/* Contains array of numeric values */
    unsigned int axisFlags;	/* Indicates which axes to map element's
				 * coordinates onto */
    int *activeArr;		/* Array of indices of active data
				 * points (malloc-ed). Initially points
				 * to static storage "staticArr". */
    int staticArr[DEF_ACTIVE_SIZE];
    int numActivePoints;	/* Number of active data points. If
				 * zero and active bit is set in
				 * "flags", then all data points are
				 * active. */

    ElemConfigProc *configProc;
    ElemDestroyProc *destroyProc;
    ElemDisplayProc *displayProc;
    ElemLimitsProc *limitsProc;
    ElemDistanceProc *closestProc;
    ElemLayoutProc *layoutProc;
    ElemPrintProc *printProc;
    ElemDrawSymbolsProc *drawSymbolsProc;
    ElemPrintSymbolsProc *printSymbolsProc;

    /*
     * Bar specific attributes
     */
    Tk_3DBorder border;		/* 3D border and background color */
    int borderWidth;		/* 3D border width of bar */
    int relief;			/* Relief of bar */

    Tk_3DBorder activeBorder;
    XColor *activeFg;
    GC activeGC;

    Pixmap *bitmapArr;		/* Array of bitmaps for each segment of
				 * the bar (malloc-ed). */
    int numBitmaps;		/* # of bitmaps in the array */
    XColor **colorArr;		/* Array of normal foreground colors
				 * for each segment of the bar
				 * (malloc-ed). */
    int numColors;		/* # of normal foreground colors in
				 * the array */
    GC *gcArr;			/* Array of GC pointers for each
				 * segment of the bar (malloc-ed). */
    int stacked;		/* If non-zero, the bar y-values
				 * represent a set of stacked bar
				 * segments */
    int padX;			/* Spacing on either side of bar */
    XRectangle *segments;	/* Array of rectangle coordinates
				 * composing the dimension of each
				 * segments of the bar. By default, is
				 * set to "segArr". */
    XRectangle segArr[DEF_SEG_SIZE];
    int numSegments;		/* Number of segments in the bar */

} Bar;

#define DEF_BAR_ACTIVE_BG_COLOR "red"
#define DEF_BAR_ACTIVE_BG_MONO	WHITE
#define DEF_BAR_ACTIVE_FG_COLOR "pink"
#define DEF_BAR_ACTIVE_FG_MONO 	BLACK
#define DEF_BAR_BG_COLOR	"navyblue"
#define DEF_BAR_BG_MONO		BLACK
#define DEF_BAR_BORDERWIDTH	"2"
#define DEF_BAR_DATA		(char *)NULL
#define DEF_BAR_FG_COLOR     	"blue"
#define DEF_BAR_FG_MONO		WHITE
#define DEF_BAR_LABEL		(char *)NULL
#define DEF_BAR_RELIEF		"raised"
#define DEF_BAR_STACKED		"0"
#define DEF_BAR_STIPPLE		""
#define DEF_BAR_X_AXIS		"x"
#define DEF_BAR_X_DATA		(char *)NULL
#define DEF_BAR_Y_AXIS		"y"
#define DEF_BAR_Y_DATA		(char *)NULL

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BORDER, "-activebackground",
	"elemActiveBackground", "Background",
	DEF_BAR_ACTIVE_BG_COLOR, Tk_Offset(Bar, activeBorder),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-activebackground",
	"elemActiveBackground", "Background",
	DEF_BAR_ACTIVE_BG_MONO, Tk_Offset(Bar, activeBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground",
	"elemActiveForeground", "Foreground",
	DEF_BAR_ACTIVE_FG_COLOR, Tk_Offset(Bar, activeFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground",
	"elemActiveForeground", "Foreground",
	DEF_BAR_ACTIVE_FG_MONO, Tk_Offset(Bar, activeFg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-background", "elemBackground", "Background",
	DEF_BAR_BG_COLOR, Tk_Offset(Bar, border),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-background", "elemBackground", "Background",
	DEF_BAR_BG_COLOR, Tk_Offset(Bar, border),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_SYNONYM, "-bd", "elemBorderwidth", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-bg", "elemBackground", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_PIXELS, "-borderwidth", "elemBorderwidth", "Borderwidth",
	DEF_BAR_BORDERWIDTH, Tk_Offset(Bar, borderWidth), 0},
    {TK_CONFIG_SYNONYM, "-fg", "elemForeground", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-data", "elemData", "Data",
	(char *)NULL, 0, 0, &bltTwinOption},
    {TK_CONFIG_CUSTOM, "-foreground", "elemForeground", "Foreground",
	DEF_BAR_FG_COLOR, Tk_Offset(Bar, colorArr),
	TK_CONFIG_COLOR_ONLY, &ColorListOption},
    {TK_CONFIG_CUSTOM, "-foreground", "elemForeground", "Foreground",
	DEF_BAR_FG_COLOR, Tk_Offset(Bar, colorArr),
	TK_CONFIG_MONO_ONLY, &ColorListOption},
    {TK_CONFIG_STRING, "-label", "elemLabel", "Label",
	DEF_BAR_LABEL, Tk_Offset(Bar, label), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-mapx", "elemMapX", "MapX",
	DEF_BAR_X_AXIS, Tk_Offset(Bar, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltXAxisFlagsOption},
    {TK_CONFIG_CUSTOM, "-mapy", "elemMapY", "MapY",
	DEF_BAR_Y_AXIS, Tk_Offset(Bar, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltYAxisFlagsOption},
    {TK_CONFIG_RELIEF, "-relief", "elemRelief", "Relief",
	DEF_BAR_RELIEF, Tk_Offset(Bar, relief), 0},
    {TK_CONFIG_BOOLEAN, "-stacked", "elemStacked", "Stacked",
	DEF_BAR_STACKED, Tk_Offset(Bar, stacked), 0},
    {TK_CONFIG_CUSTOM, "-stipple", "elemStipple", "Stipple",
	DEF_BAR_STIPPLE, Tk_Offset(Bar, bitmapArr), 0, &BitmapListOption},
    {TK_CONFIG_CUSTOM, "-xdata", "elemXdata", "Xdata",
	DEF_BAR_X_DATA, 0, 0, &bltXVectorOption},
    {TK_CONFIG_CUSTOM, "-ydata", "elemYdata", "Ydata",
	DEF_BAR_Y_DATA, 0, 0, &bltYVectorOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};


/* ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 *
 * ParseColorList --
 *
 *	Convert a list of color names in to an array of XColor pointers.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	An array of color pointers in allocated and filled.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseColorList(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* unused */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/*  */
    char *value;		/* List of foreground color names */
    char *widgRec;		/* Bar element record */
    int offset;			/* unused */
{
    Bar *barPtr = (Bar *)widgRec;
    register int i;
    int numColors;
    XColor **colorArr, **colorPtrPtr;
    char **colorNames;

    colorNames = NULL;
    if (Tcl_SplitList(interp, value, &numColors, &colorNames) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numColors < 1) {
	interp->result = "no colors specified";
	goto error;
    }
    colorArr = (XColor **)malloc(sizeof(XColor *) * (numColors + 1));
    if (colorArr == NULL) {
	goto error;
    }
    for (i = 0; i < numColors; i++) {
	colorArr[i] = Tk_GetColor(interp, tkwin, Tk_Colormap(tkwin),
	    Tk_GetUid(colorNames[i]));
	if (colorArr[i] == NULL) {
	    goto error;
	}
    }
    colorArr[i] = NULL;
    free((char *)colorNames);
    if (barPtr->colorArr != NULL) {
	for (colorPtrPtr = barPtr->colorArr; *colorPtrPtr != NULL;
	    colorPtrPtr++) {
	    Tk_FreeColor(*colorPtrPtr);
	}
	free((char *)barPtr->colorArr);
    }
    barPtr->colorArr = colorArr;
    barPtr->numColors = numColors;
    return TCL_OK;

  error:
    if (colorArr != NULL) {
	for (colorPtrPtr = colorArr; *colorPtrPtr != NULL; colorPtrPtr++) {
	    Tk_FreeColor(*colorPtrPtr);
	}
	free((char *)colorArr);
    }
    if (colorNames != NULL) {
	free((char *)colorNames);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintColorList --
 *
 *	Convert the array of color pointers into a list of color names.
 *
 * Results:
 *	The string representing the list of color names is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintColorList(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Type of axis vector to print */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Bar element record */
    int offset;			/* not used */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Bar *barPtr = (Bar *)(widgRec);
    Tcl_DString buffer;
    char *result;
    register XColor **colorPtrPtr;

    Tcl_DStringInit(&buffer);
    for (colorPtrPtr = barPtr->colorArr; *colorPtrPtr != NULL; colorPtrPtr++) {
	Tcl_DStringAppendElement(&buffer, Tk_NameOfColor(*colorPtrPtr));
    }
    result = Tcl_DStringValue(&buffer);
    result = strdup(result);
    *freeProcPtr = TCL_DYNAMIC;
    Tcl_DStringFree(&buffer);
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * ParseBitmapList --
 *
 *	Convert a list of bitmap names in to an array of XBitmap pointers.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	An array of bitmap pointers in allocated and filled.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseBitmapList(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* unused */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/*  */
    char *value;		/* List of bitmap names */
    char *widgRec;		/* Bar element record */
    int offset;			/* unused */
{
    Bar *barPtr = (Bar *)widgRec;
    register int i;
    int numBitmaps;
    Pixmap *bitmapArr, bitmap;
    char **bitmapNames;

    bitmapNames = NULL;
    if (Tcl_SplitList(interp, value, &numBitmaps, &bitmapNames) != TCL_OK) {
	return TCL_ERROR;
    }
    bitmapArr = (Pixmap *) calloc(sizeof(Pixmap), numBitmaps + 1);
    if (bitmapArr == NULL) {
	goto error;
    }
    for (i = 0; i < numBitmaps; i++) {
	if (*bitmapNames[i] == '\0') {
	    bitmap = None;
	} else {
	    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(bitmapNames[i]));
	    if (bitmap == None) {
		goto error;
	    }
	}
	bitmapArr[i] = bitmap;
    }
    free((char *)bitmapNames);
    if (barPtr->bitmapArr != NULL) {
	for (i = 0; i < barPtr->numBitmaps; i++) {
	    bitmap = barPtr->bitmapArr[i];
	    if (bitmap != None) {
		Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
	    }
	}
	free((char *)barPtr->bitmapArr);
    }
    barPtr->bitmapArr = bitmapArr;
    if (numBitmaps == 0) {
	numBitmaps = 1;
    }
    barPtr->numBitmaps = numBitmaps;
    return TCL_OK;

  error:
    if (bitmapArr != NULL) {
	for (i = 0; i < numBitmaps; i++) {
	    if (bitmapArr[i] != None) {
		Tk_FreeBitmap(Tk_Display(tkwin), bitmapArr[i]);
	    }
	}
	free((char *)bitmapArr);
    }
    if (bitmapNames != NULL) {
	free((char *)bitmapNames);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintBitmapList --
 *
 *	Convert the array of bitmap pointers into a list of bitmap names.
 *
 * Results:
 *	The string representing the list of bitmap names is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintBitmapList(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Type of axis vector to print */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Bar element record */
    int offset;			/* not used */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Bar *barPtr = (Bar *)(widgRec);
    Tcl_DString buffer;
    char *result;
    register int i;
    register char *name;

    Tcl_DStringInit(&buffer);
    for (i = 0; i < barPtr->numBitmaps; i++) {
	name = "";
	if (barPtr->bitmapArr[i] != None) {
	    name = Tk_NameOfBitmap(Tk_Display(tkwin), barPtr->bitmapArr[i]);
	}
	Tcl_DStringAppendElement(&buffer, name);
    }
    result = Tcl_DStringValue(&buffer);
    result = strdup(result);
    *freeProcPtr = TCL_DYNAMIC;
    Tcl_DStringFree(&buffer);
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureBar --
 *
 *	Sets up the appropriate configuration parameters in the GC.
 *      It is assumed the parameters have been previously set by
 *	a call to Tk_ConfigureWidget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information such as bar foreground/background
 *	color and stipple etc. get set in a new GC.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigureBar(graphPtr, elemPtr)
    Graph *graphPtr;
    register Element *elemPtr;
{
    XGCValues gcValues;
    unsigned long gcMask;
    Bar *barPtr = (Bar *)elemPtr;
    register int i;
    GC *newArr, newGC;
    XColor *colorPtr;
    Pixmap stipple;
    int numSegments;

    barPtr->symbol = SQUARE_SYMBOL;	/* Use square symbol in legend */

    gcValues.foreground = barPtr->activeFg->pixel;
    gcValues.background = (Tk_3DBorderColor(barPtr->activeBorder))->pixel;
    gcMask = GCForeground | GCBackground;
    stipple = barPtr->bitmapArr[0];	/* Always will have at least one entry */
    if (stipple != None) {
	gcValues.stipple = stipple;
	gcValues.fill_style = FillOpaqueStippled;
	gcMask |= (GCStipple | GCFillStyle);
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (barPtr->activeGC != NULL) {
	Tk_FreeGC(graphPtr->display, barPtr->activeGC);
    }
    barPtr->activeGC = newGC;

    gcValues.background = (Tk_3DBorderColor(barPtr->border))->pixel;
    numSegments = barPtr->y.length;
    newArr = (GC *)malloc(sizeof(GC) * (numSegments + 1));
    for (i = 0; i < numSegments; i++) {
	colorPtr = barPtr->colorArr[i % barPtr->numColors];
	gcValues.foreground = colorPtr->pixel;
	stipple = barPtr->bitmapArr[i % barPtr->numBitmaps];
	if (stipple != None) {
	    gcValues.stipple = stipple;
	    gcValues.fill_style = FillOpaqueStippled;
	    gcMask |= (GCStipple | GCFillStyle);
	} else {
	    gcMask &= ~(GCStipple | GCFillStyle);
	}
	newArr[i] = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    }
    newArr[i] = NULL;
    if (barPtr->gcArr != NULL) {
	register GC *gcPtr;

	for (gcPtr = barPtr->gcArr; *gcPtr != NULL; gcPtr++) {
	    Tk_FreeGC(graphPtr->display, *gcPtr);
	}
	free((char *)barPtr->gcArr);
    }
    barPtr->gcArr = newArr;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ComputeBarLimits --
 *
 *	Returns the limits of the bar data
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ComputeBarLimits(graphPtr, elemPtr, axisPtr, minPtr, maxPtr)
    Graph *graphPtr;
    Element *elemPtr;
    GraphAxis *axisPtr;		/* Axis information */
    double *minPtr, *maxPtr;
{
    Bar *barPtr = (Bar *)elemPtr;
    Vector *vecPtr;

    *minPtr = Blt_posInfinity, *maxPtr = Blt_negInfinity;
    if ((barPtr->x.length < 1) || (barPtr->y.length < 1)) {
	return 0;
    }
    if (!(elemPtr->axisFlags & AXIS_MASK(axisPtr))) {
	return 0;
    }
    if (X_AXIS(axisPtr)) {
	vecPtr = &(barPtr->x);
	*minPtr = (axisPtr->logScale) ? vecPtr->logMin : vecPtr->min;
	*minPtr -= 0.5;
	*maxPtr = vecPtr->max + 0.5;
    } else {
	vecPtr = &(barPtr->y);
	*maxPtr = vecPtr->max;
	*minPtr = (axisPtr->logScale) ? vecPtr->logMin : vecPtr->min;

	/* Stacked vector may either change the max or min */
	if (barPtr->stacked) {
	    register int i;
	    double sum = 0.0;

	    for (i = 0; i < vecPtr->length; i++) {
		sum += vecPtr->data[i];
	    }
	    if (sum > *maxPtr) {
		*maxPtr = sum;
	    } else if (sum < *minPtr) {
		*minPtr = sum;
	    }
	}
    }
    return (vecPtr->length);
}

/*
 *----------------------------------------------------------------------
 *
 * ClosestBar --
 *
 *	Find the bar segment closest to the window coordinates	point
 *	specified.
 *
 * Results:
 *	Returns 1 if the point is width any bar segment, otherwise 0.
 *	Does not currently use graphPtr->halo.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ClosestBar(graphPtr, elemPtr, winX, winY, closePtr)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;		/* Bar element */
    int winX, winY;		/* Window coordinates of point on screen */
    ClosestPoint *closePtr;	/* Index of closest point in element */
{
    Bar *barPtr = (Bar *)elemPtr;
    register int i, n;
    double sum;
    int delta[3];
    int barDist, maxDist;
    register XRectangle *rectPtr;

    if (barPtr->numSegments == 0) {
	return 0;
    }
    sum = 0.0;
    barDist = (graphPtr->halo + 1);
    for (i = 0; i < barPtr->numSegments; i++) {
	rectPtr = &(barPtr->segments[i]);
	if (barPtr->stacked) {
	    sum += barPtr->y.data[i];
	}
	maxDist = (rectPtr->x - winX);
	delta[0] = (winX - (rectPtr->x + rectPtr->width));
	delta[1] = (rectPtr->y - winY);
	delta[2] = (winY - (rectPtr->y + rectPtr->height));
	for (n = 0; n < 3; n++) {
	    if (delta[n] > maxDist) {
		maxDist = delta[n];
	    }
	}
	/* If it's in the bounding box, then this is the segment */
	if (maxDist <= 0) {
	    closePtr->elemPtr = elemPtr;
	    closePtr->index = i;
	    closePtr->x = barPtr->x.data[0];
	    closePtr->y = (barPtr->stacked) ? sum : barPtr->y.data[i];
	    closePtr->dist = 0.0;
	    return 1;
	}
	/*
	 * If it's in the extended bounding box, save the distance and
	 * the index of the segement closest to the edge
	 */
	if (maxDist < barDist) {
	    barDist = maxDist;
	    closePtr->index = i;
	    closePtr->y = (barPtr->stacked) ? sum : barPtr->y.data[i];
	}
    }
    if (barDist <= graphPtr->halo) {
	closePtr->elemPtr = elemPtr;
	closePtr->x = barPtr->x.data[0];
	closePtr->dist = barDist;
	return 1;
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutBar --
 *
 *	Calculates the actual window coordinates of the bar element.
 *	The window coordinates are saved in the bar element structure.
 *
 * Results:
 *	None.
 *
 * Notes:
 *	A bar can have multiple y values.  In this case, the bar can be
 * 	represented as either a set of contiguous bars (no spacing) or a
 *      single multi-segmented (stacked) bar.
 *
 *	The X axis layout for a barchart may be presented in one of two ways.
 *	If x values are used, the bars are placed at those coordinates.
 *	Otherwise, the range will represent the number of values.
 *
 *----------------------------------------------------------------------
 */
static void
LayoutBar(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;
    double sum;			/* Sum of bar y values */
    double avgSize;
    int curX, curY;		/* Current and last Y positions */
    int lastY, delta;		/* Last y-coordinate used */
    XRectangle *segArr;
    int avgWidth;		/* Width of each bar in set */
    GraphAxis *X, *Y;
    int x, y, width, height;
    register int i;
    int numSegments;

    if ((barPtr->x.length < 1) || (barPtr->y.length < 1)) {
	return;			/* No bars */
    }
    /* Determine which axes the bar element is mapped to */
    X = (barPtr->axisFlags & X1_AXIS_MASK)
	? graphPtr->axisArr[X1_AXIS] : graphPtr->axisArr[X2_AXIS];
    Y = (barPtr->axisFlags & Y1_AXIS_MASK)
	? graphPtr->axisArr[Y1_AXIS] : graphPtr->axisArr[Y2_AXIS];

    segArr = barPtr->segArr;
    numSegments = barPtr->y.length;
    if (numSegments > DEF_SEG_SIZE) {
	segArr = (XRectangle *)malloc(barPtr->y.length * sizeof(XRectangle));
	if (segArr == NULL) {
	    return;
	}
    }
    curY = lastY = Blt_Transform(Y, 0.0);	/* Baseline of bars */

    /* Watch out for limiting of values */
    avgWidth = Blt_TransformDist(X, graphPtr->barWidth);
    if (avgWidth < 1) {
	avgWidth = 1;
    }
    /* Use only the first data value in the X array for positioning */
    curX = Blt_Transform(X, barPtr->x.data[0]) - avgWidth / 2;
    sum = 0.0;
    if (!barPtr->stacked) {
	avgWidth /= barPtr->y.length;
    }
    width = avgWidth - (barPtr->padX * 2);
    for (i = 0; i < barPtr->y.length; i++) {
	x = curX;
	if (barPtr->stacked) {
	    /* Next point is the current sum the y values */
	    sum += barPtr->y.data[i];
	    lastY = curY;
	    curY = Blt_Transform(Y, sum);
	} else {
	    curY = Blt_Transform(Y, barPtr->y.data[i]);
	    curX += avgWidth;	/* Adjust x-coordinate by next bar width */
	}
	if (barPtr->y.max < 0.0) {
	    y = lastY;		/* Origin is last y-coordinate */
	    delta = curY - lastY;
	} else {
	    y = curY;		/* Origin is current y-coordinate */
	    delta = lastY - curY;
	}
	height = BLT_ABS(delta) + 1;
	if (graphPtr->inverted) {
	    segArr[i].y = x;
	    segArr[i].x = y - height;
	    segArr[i].width = height;
	    segArr[i].height = width;
	} else {
	    segArr[i].x = x;
	    segArr[i].y = y;
	    segArr[i].width = width;
	    segArr[i].height = height;
	}
    }
    avgSize = graphPtr->avgSymSize * barPtr->symbolScale;
    barPtr->symbolSize = BLT_RND(avgSize);
    if (barPtr->segments != barPtr->segArr) {
	free((char *)barPtr->segments);
    }
    barPtr->numSegments = numSegments;
    barPtr->segments = segArr;
}

/*
 * -----------------------------------------------------------------
 *
 * DrawSymbols --
 *
 * 	Draw a symbol centered at the given x,y window coordinate
 *	based upon the element symbol type and size.
 *
 * Results:
 *	None.
 *
 * Problems:
 *	Most notable is the round-off errors generated when
 *	calculating the centered position of the symbol.
 * -----------------------------------------------------------------
 */
/*ARGSUSED*/
static void
DrawSymbols(graphPtr, elemPtr, size, pointArr, numPoints, active)
    Graph *graphPtr;
    Element *elemPtr;
    int size;
    XPoint *pointArr;
    int numPoints;
    int active;			/* unused */
{
    Bar *barPtr = (Bar *)elemPtr;
    register int i;
    int x, y, radius;

    radius = (size / 2);
    size--;
    for (i = 0; i < numPoints; i++) {
	x = pointArr[i].x - radius, y = pointArr[i].y - radius;
	XFillRectangle(graphPtr->display, graphPtr->canvas, barPtr->gcArr[0],
	    x, y, size, size);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DisplayBar --
 *
 *	Draws the rectangle representing the bar element.
 *	If the relief option is set to "raised" or "sunken" and the
 *	bar borderwidth is set (borderwidth > 0), a 3D border is
 *	drawn around the bar.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	X drawing commands are output.
 *
 *----------------------------------------------------------------------
 */
static void
DisplayBar(graphPtr, elemPtr, active)
    Graph *graphPtr;
    Element *elemPtr;
    int active;
{
    Bar *barPtr = (Bar *)elemPtr;
    register int i;
    Tk_3DBorder border;
    GC gc;
    unsigned int numRects;
    unsigned int index, maxIndex;
    int useIndex;

    if (active) {
	gc = barPtr->activeGC;
	border = barPtr->activeBorder;
    } else {
	border = barPtr->border;
    }

    numRects = barPtr->numSegments;
    useIndex = ((active) && (barPtr->numActivePoints > 0));
    if (useIndex) {
	maxIndex = numRects - 1;
	numRects = barPtr->numActivePoints;
    }
    for (i = 0; i < numRects; i++) {
	index = i;
	if (useIndex) {
	    index = barPtr->activeArr[i];
	    if (index > maxIndex) {
		continue;
	    }
	}
	if (!active) {
	    gc = barPtr->gcArr[index];
	}
	XFillRectangle(graphPtr->display, graphPtr->canvas, gc,
	    barPtr->segments[index].x, barPtr->segments[index].y,
	    barPtr->segments[index].width, barPtr->segments[index].height);
	if ((barPtr->borderWidth > 0) && (barPtr->relief != TK_RELIEF_FLAT)) {
	    Tk_Draw3DRectangle(graphPtr->display, graphPtr->canvas,
		border, barPtr->segments[index].x, barPtr->segments[index].y,
		barPtr->segments[index].width, barPtr->segments[index].height,
		barPtr->borderWidth, barPtr->relief);
	}
    }
}

/*
 * -----------------------------------------------------------------
 *
 * PrintSymbols --
 *
 * 	Draw a symbol centered at the given x,y window coordinate
 *	based upon the element symbol type and size.
 *
 * Results:
 *	None.
 *
 * Problems:
 *	Most notable is the round-off errors generated when
 *	calculating the centered position of the symbol.
 * -----------------------------------------------------------------
 */
/*ARGSUSED*/
static void
PrintSymbols(graphPtr, elemPtr, size, pointArr, numPoints, active)
    Graph *graphPtr;
    Element *elemPtr;
    int size;
    XPoint *pointArr;
    int numPoints;
    int active;
{
    Bar *barPtr = (Bar *)elemPtr;
    register int i;
    XColor *normalFg;
    Pixmap stipple;

    stipple = barPtr->bitmapArr[0];
    if (stipple != None) {
	unsigned int width, height;

	Blt_BackgroundToPostScript(graphPtr, Tk_3DBorderColor(barPtr->border));
	Tcl_AppendResult(graphPtr->interp, "/StippleProc {\ngsave\n",
	    (char *)NULL);
	Tk_SizeOfBitmap(graphPtr->display, stipple, &width, &height);
	Blt_StippleToPostScript(graphPtr, stipple, width, height, 1);
	Tcl_AppendResult(graphPtr->interp, "} def\n", (char *)NULL);
    }
    for (i = 0; i < numPoints; i++) {
	normalFg = barPtr->colorArr[(i % barPtr->numColors)];
	if (stipple == None) {
	    Blt_ForegroundToPostScript(graphPtr, normalFg);
	}
	sprintf(graphPtr->scratchPtr, "%d %d %d Sq\n", pointArr[i].x,
	    pointArr[i].y, size);
	Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
	if (stipple != None) {
	    Tcl_AppendResult(graphPtr->interp, "gsave\n", (char *)NULL);
	    Blt_ForegroundToPostScript(graphPtr, normalFg);
	    Tcl_AppendResult(graphPtr->interp, "/StippleProc cvx exec\n",
		"grestore\n", (char *)NULL);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PrintBar --
 *
 *	Similar to the DrawBar procedure, prints PostScript related
 *	commands to form rectangle representing the bar element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript pen width, dashes, and color settings are changed.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
PrintBar(graphPtr, elemPtr, active)
    Graph *graphPtr;
    Element *elemPtr;
    int active;			/* Unused */
{
    Bar *barPtr = (Bar *)elemPtr;
    register int i;
    XColor *normalFg;
    register Pixmap stipple;

    for (i = 0; i < barPtr->numSegments; i++) {
	normalFg = barPtr->colorArr[(i % barPtr->numColors)];
	stipple = barPtr->bitmapArr[(i % barPtr->numBitmaps)];
	if (stipple != None) {
	    unsigned int width, height;

	    Blt_BackgroundToPostScript(graphPtr,
		Tk_3DBorderColor(barPtr->border));
	    Blt_RectangleToPostScript(graphPtr, barPtr->segments[i].x,
		barPtr->segments[i].y, barPtr->segments[i].width,
		barPtr->segments[i].height);
	    Tk_SizeOfBitmap(graphPtr->display, stipple, &width, &height);
	    Blt_ForegroundToPostScript(graphPtr, normalFg);
	    Blt_StippleToPostScript(graphPtr, stipple, width, height, True);
	} else {
	    Blt_ForegroundToPostScript(graphPtr, normalFg);
	    Blt_RectangleToPostScript(graphPtr, barPtr->segments[i].x,
		barPtr->segments[i].y, barPtr->segments[i].width,
		barPtr->segments[i].height);
	}
	if ((barPtr->borderWidth > 0) &&
	    (barPtr->relief != TK_RELIEF_FLAT)) {
	    Blt_Print3DRectangle(graphPtr, barPtr->border,
		barPtr->segments[i].x, barPtr->segments[i].y,
		(unsigned int)barPtr->segments[i].width,
		barPtr->segments[i].height, barPtr->borderWidth,
		barPtr->relief);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyBar --
 *
 *	Release memory and resources allocated for the bar element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the bar element is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyBar(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;

    Tk_FreeOptions(barPtr->configSpecs, (char *)barPtr, graphPtr->display, 0);

    if (barPtr->gcArr != NULL) {
	register GC *gcPtr;

	for (gcPtr = barPtr->gcArr; *gcPtr != NULL; gcPtr++) {
	    Tk_FreeGC(graphPtr->display, *gcPtr);
	}
	free((char *)barPtr->gcArr);
    }
    if (barPtr->activeGC != NULL) {
	Tk_FreeGC(graphPtr->display, barPtr->activeGC);
    }
    if (barPtr->colorArr != NULL) {
	register XColor **colorPtrPtr;

	for (colorPtrPtr = barPtr->colorArr; *colorPtrPtr != NULL;
	    colorPtrPtr++) {
	    Tk_FreeColor(*colorPtrPtr);
	}
	free((char *)barPtr->colorArr);
    }
    if (barPtr->bitmapArr != NULL) {
	register int i;

	for (i = 0; i < barPtr->numBitmaps; i++) {
	    if (barPtr->bitmapArr[i] != None) {
		Tk_FreeBitmap(graphPtr->display, barPtr->bitmapArr[i]);
	    }
	}
	free((char *)barPtr->bitmapArr);
    }
    if (barPtr->segments != barPtr->segArr) {
	free((char *)barPtr->segments);
    }
    if (barPtr->x.data != NULL) {
	free((char *)barPtr->x.data);
    }
    if (barPtr->y.data != NULL) {
	free((char *)barPtr->y.data);
    }
    if (barPtr->activeArr != barPtr->staticArr) {
	free((char *)barPtr->activeArr);
    }
    free((char *)barPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateBar --
 *
 *	Allocate memory and initialize methods for the new bar element.
 *
 * Results:
 *	The pointer to the newly allocated element structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the bar element structure.
 *
 *----------------------------------------------------------------------
 */
Element *
Blt_BarElement()
{
    register Bar *barPtr;

    barPtr = (Bar *)calloc(1, sizeof(Bar));
    if (barPtr == NULL) {
	return NULL;
    }
    barPtr->configSpecs = configSpecs;
    barPtr->configProc = ConfigureBar;
    barPtr->destroyProc = DestroyBar;
    barPtr->displayProc = DisplayBar;
    barPtr->limitsProc = ComputeBarLimits;
    barPtr->closestProc = ClosestBar;
    barPtr->layoutProc = LayoutBar;
    barPtr->printProc = PrintBar;
    barPtr->drawSymbolsProc = DrawSymbols;
    barPtr->printSymbolsProc = PrintSymbols;

    barPtr->type = BAR_ELEM_TYPE;
    barPtr->relief = TK_RELIEF_RAISED;
    barPtr->symbolScale = 1.0;
    barPtr->borderWidth = 2;
    barPtr->stacked = 0;
    barPtr->numBitmaps = barPtr->numSegments = barPtr->numColors = 0;
    barPtr->segments = barPtr->segArr;
    return ((Element *)barPtr);
}
