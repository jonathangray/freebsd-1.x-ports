
/*
 * bltGrLine.c --
 *
 *	This module implements line elements in the graph widget
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
static int ParseSymbolType _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *value, char *widgRec,
	int offset));
static char *PrintSymbolType _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

static int ParseTrace _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *value, char *widgRec,
	int offset));
static char *PrintTrace _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

#include "bltGrElem.h"

/*
 * Element attributes: either symbol types or line styles
*/
static char *symbolTokens[] =
{
    "Line", "Square", "Circle",
    "Diamond", "Plus", "Cross",
    "Splus", "Scross", (char *)NULL,
};

#define TRACE_BOTH 		0
#define TRACE_INCREASING	1
#define TRACE_DECREASING	2
#define BROKEN_TRACE(dir,x,last) \
    ((((dir) != TRACE_DECREASING) && ((x) < (last))) || \
     (((dir) != TRACE_INCREASING) && ((x) > (last))))

/*
 * Trace option values
 */
static char *traceTokens[] =
{
    "both", "increasing", "decreasing", (char *)NULL,
};

typedef struct {
    SymbolType type;
    Display *display;		/* Display on which to draw symbol */
    Drawable canvas;		/* Window to draw symbol */
    GC lineGC;
    GC borderGC;
    GC fillGC;
    int radius;
    int size;
    int needBorder;
    XPoint offset[13];

} SymbolInfo;


typedef struct {
    XPoint *pointPtr;		/* Array of points defining the segment */
    int numPoints;		/* Number of points in segment */
} CurveSegment;

typedef struct {
    Tcl_Interp *interp;		/* Interpreter of the graph widget.
				 * This is only needed for
				 * Tcl_PrintDouble calls in the custom
				 * configure print routines. */
    ElementClassType type;	/* Type of element is LINE_ELEMENT */
    unsigned int flags;
    Tk_Uid id;			/* Identifier to refer the element.
				 * Used in the "insert", "delete", or
				 * "show", commands. */
    int mapped;			/* If non-zero, element is currently
				 * visible.*/
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    char *label;		/* Label displayed in legend */
    SymbolType symbol;		/* Element symbol type */
    double symbolScale;		/* Scale factor when computing symbol
				 * size */
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
     * Line specific configurable attributes
     */
    XColor *normalFg;		/* normal color of lines/borders*/
    XColor *normalBg;		/* normal color of dashed lines, fills */
    XColor *activeFg;		/* active color of lines/borders */
    XColor *activeBg;		/* active color of dashed lines, fills */

    int lineWidth;		/* Width of the line connecting the
				 * symbols together. If less than 0,
				 * no line is drawn. */
    int activeLineWidth;	/* Width of the active line */
    int borderWidth;		/* Width of the border around the
				   symbol */
    int dashes;			/* Dash on-off list value */
    int trace;			/* Indicates if to break connected line
				 * segments where x-coordinate values
				 * are not monotonically increasing or
				 * decreasing. */
    GC normalBorderGC;		/* Symbol border graphics context */
    GC normalFillGC;		/* Symbol fill graphics context */
    GC normalLineGC;		/* Line graphics context */

    GC activeLineGC;
    GC activeFillGC;
    GC activeBorderGC;
    /*
     * Drawing related structures
     */
    XPoint *pointArr;		/* Array of window coordinates
				 * representing the points of the line
				 * (malloc'ed) */
    int numPoints;		/* Number of points in point array */
    CurveSegment *segArr;	/* Array of line segment information
				 * (malloc'ed) */
    int numSegments;		/* Number of line segments in point
				 * array */

} Line;

extern Tk_CustomOption bltXVectorOption;
extern Tk_CustomOption bltYVectorOption;
extern Tk_CustomOption bltTwinOption;
extern Tk_CustomOption bltXAxisFlagsOption;
extern Tk_CustomOption bltYAxisFlagsOption;

static Tk_CustomOption SymbolOption =
{
    ParseSymbolType, PrintSymbolType, (ClientData)0
};

static Tk_CustomOption TraceOption =
{
    ParseTrace, PrintTrace, (ClientData)0
};

#define DEF_LINE_ACTIVE_BG_COLOR "red"
#define DEF_LINE_ACTIVE_BG_MONO	WHITE
#define DEF_LINE_ACTIVE_FG_COLOR "pink"
#define DEF_LINE_ACTIVE_FG_MONO BLACK
#define DEF_LINE_ACTIVE_LINE_WIDTH "1"
#define DEF_LINE_BG_COLOR	"navyblue"
#define DEF_LINE_BG_MONO	BLACK
#define DEF_LINE_DASHES		"0"
#define DEF_LINE_DATA		(char *)NULL
#define DEF_LINE_FG_COLOR     	"blue"
#define DEF_LINE_FG_MONO	WHITE
#define DEF_LINE_LABEL		(char *)NULL
#define DEF_LINE_SYMBOL_BW 	"1"
#define DEF_LINE_SYMBOL_SCALE	"1.0"
#define DEF_LINE_TRACE		"both"
#define DEF_LINE_WIDTH 		"1"
#define DEF_LINE_X_AXIS		"x"
#define DEF_LINE_X_DATA		(char *)NULL
#define DEF_LINE_Y_AXIS		"y"
#define DEF_LINE_Y_DATA		(char *)NULL
#define DEF_LINE_SYMBOL		"line"

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_COLOR, "-activebackground",
	"elemActiveBackground", "Background",
	DEF_LINE_ACTIVE_BG_COLOR, Tk_Offset(Line, activeBg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activebackground",
	"elemActiveBackground", "Background",
	DEF_LINE_ACTIVE_BG_MONO, Tk_Offset(Line, activeBg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground",
	"elemActiveForeground", "Foreground",
	DEF_LINE_ACTIVE_FG_COLOR, Tk_Offset(Line, activeFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground",
	"elemActiveForeground", "Foreground",
	DEF_LINE_ACTIVE_FG_MONO, Tk_Offset(Line, activeFg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_PIXELS, "-activelinewidth", "elemActiveLineWidth", "LineWidth",
	DEF_LINE_ACTIVE_LINE_WIDTH, Tk_Offset(Line, activeLineWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-background", "elemBackground", "Background",
	DEF_LINE_BG_COLOR, Tk_Offset(Line, normalBg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-background", "elemBackground", "Background",
	DEF_LINE_BG_MONO, Tk_Offset(Line, normalBg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_SYNONYM, "-bg", "elemBackground", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_PIXELS, "-borderwidth", "elemBorderWidth", "BorderWidth",
	DEF_LINE_SYMBOL_BW, Tk_Offset(Line, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_INT, "-dashes", "elemDashes", "Dashes",
	DEF_LINE_DASHES, Tk_Offset(Line, dashes),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-data", "elemData", "Data",
	DEF_LINE_DATA, 0, 0, &bltTwinOption},
    {TK_CONFIG_SYNONYM, "-fg", "elemForeground", (char *)NULL,
	(char *)NULL, 0, TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_SYNONYM, "-fg", "elemForeground", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_COLOR, "-foreground", "elemForeground", "Foreground",
	DEF_LINE_FG_COLOR, Tk_Offset(Line, normalFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "elemForeground", "Foreground",
	DEF_LINE_FG_MONO, Tk_Offset(Line, normalFg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_STRING, "-label", "elemLabel", "Label",
	DEF_LINE_LABEL, Tk_Offset(Line, label), TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-linewidth", "elemLineWidth", "LineWidth",
	DEF_LINE_WIDTH, Tk_Offset(Line, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "elemMapX", "MapX",
	DEF_LINE_X_AXIS, Tk_Offset(Line, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltXAxisFlagsOption},
    {TK_CONFIG_CUSTOM, "-mapy", "elemMapY", "MapY",
	DEF_LINE_Y_AXIS, Tk_Offset(Line, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltYAxisFlagsOption},
    {TK_CONFIG_BOOLEAN, "-noretrace", "elemNoretrace", "Noretrace",
	(char *)NULL, Tk_Offset(Line, trace), 0},
    {TK_CONFIG_DOUBLE, "-scale", "elemScale", "Scale",
	DEF_LINE_SYMBOL_SCALE, Tk_Offset(Line, symbolScale),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-symbol", "elemSymbol", "Symbol",
	DEF_LINE_SYMBOL, Tk_Offset(Line, symbol), 0, &SymbolOption},
    {TK_CONFIG_CUSTOM, "-trace", "elemTrace", "Trace",
	DEF_LINE_TRACE, Tk_Offset(Line, trace),
	TK_CONFIG_DONT_SET_DEFAULT, &TraceOption},
    {TK_CONFIG_CUSTOM, "-xdata", "elemXdata", "Xdata",
	DEF_LINE_X_DATA, 0, 0, &bltXVectorOption},
    {TK_CONFIG_CUSTOM, "-ydata", "elemYdata", "Ydata",
	DEF_LINE_Y_DATA, 0, 0, &bltYVectorOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};



/* ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 *
 * ParseSymbolType --
 *
 *	Convert the string representation of a line style or symbol name
 *	into its numeric form.
 *
 * Results:
 *	The return value is a standard Tcl result.  The symbol type is
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseSymbolType(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* String representing symbol type */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
{
    int *symbolPtr = (int *)(widgRec + offset);
    int index;

    index = Blt_GetTokenIndex(symbolTokens, value, 1);
    if (index < 0) {
	Tcl_AppendResult(interp, "bad symbol name \"", value, "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    *symbolPtr = index;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintSymbolType --
 *
 *	Convert the symbol value into a string.
 *
 * Results:
 *	The string representing the symbol type or line style is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintSymbolType(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    int symbol = *(int *)(widgRec + offset);

    return (symbolTokens[symbol]);
}

/*
 *----------------------------------------------------------------------
 *
 * ParseTrace --
 *
 *	Convert the string representation of a line style or symbol name
 *	into its numeric form.
 *
 * Results:
 *	The return value is a standard Tcl result.  The symbol type is
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseTrace(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* String representing symbol type */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
{
    int *valuePtr = (int *)(widgRec + offset);
    int index;

    index = Blt_GetTokenIndex(traceTokens, value, 0);
    if (index < 0) {
	Tcl_AppendResult(interp, "bad trace value \"", value,
	    "\" : should be \"increasing\", \"decreasing\", or \"both\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    *valuePtr = index;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintTrace --
 *
 *	Convert the symbol value into a string.
 *
 * Results:
 *	The string representing the symbol type or line style is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintTrace(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    int value = *(int *)(widgRec + offset);

    return (traceTokens[value]);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureLine --
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
 *	Configuration information such as line width, line style, color
 *	etc. get set in a new GC.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigureLine(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Line *linePtr = (Line *)elemPtr;
    unsigned long gcMask;
    GC newGC;
    XGCValues gcValues;

    /* Borders of symbols: need foreground only */

    gcMask = (GCLineWidth | GCForeground);
    gcValues.foreground = linePtr->normalBg->pixel;
    gcValues.line_width = linePtr->borderWidth;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (linePtr->normalBorderGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->normalBorderGC);
    }
    linePtr->normalBorderGC = newGC;

    gcValues.foreground = linePtr->activeBg->pixel;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (linePtr->activeBorderGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->activeBorderGC);
    }
    linePtr->activeBorderGC = newGC;

    /* Fills for symbols: need reversed foreground and background */

    gcMask |= GCBackground;
    gcValues.background = linePtr->normalBg->pixel;
    gcValues.foreground = linePtr->normalFg->pixel;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (linePtr->normalFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->normalFillGC);
    }
    linePtr->normalFillGC = newGC;

    gcValues.background = linePtr->activeBg->pixel;
    gcValues.foreground = linePtr->activeFg->pixel;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (linePtr->activeFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->activeFillGC);
    }
    linePtr->activeFillGC = newGC;

    /* Lines */

    gcMask |= (GCLineStyle | GCCapStyle | GCJoinStyle);
    gcValues.cap_style = CapButt;
    gcValues.join_style = JoinRound;
    gcValues.line_style = LineSolid;
    gcValues.line_width = linePtr->lineWidth;
    gcValues.background = linePtr->normalBg->pixel;
    gcValues.foreground = linePtr->normalFg->pixel;
    if (linePtr->dashes > 0) {
	gcValues.dash_offset = 0;
	gcValues.line_style = LineDoubleDash;
	gcValues.dashes = linePtr->dashes;
	gcMask |= (GCDashList | GCDashOffset);
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (linePtr->normalLineGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->normalLineGC);
    }
    linePtr->normalLineGC = newGC;

    gcValues.background = linePtr->activeBg->pixel;
    gcValues.foreground = linePtr->activeFg->pixel;
    gcValues.line_width = linePtr->activeLineWidth;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (linePtr->activeLineGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->activeLineGC);
    }
    linePtr->activeLineGC = newGC;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetLineLimits --
 *
 *	Returns the limits of the line element
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
GetLineLimits(graphPtr, elemPtr, axisPtr, minPtr, maxPtr)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;
    GraphAxis *axisPtr;		/* Axis information */
    double *minPtr, *maxPtr;
{
    Line *linePtr = (Line *)elemPtr;
    int numPoints;

    *minPtr = Blt_posInfinity, *maxPtr = Blt_negInfinity;
    numPoints = BLT_MIN(linePtr->x.length, linePtr->y.length);

    /* Verify the element uses this axis */
    if (!(elemPtr->axisFlags & AXIS_MASK(axisPtr))) {
	return 0;
    }
    if (numPoints > 0) {
	if (X_AXIS(axisPtr)) {
	    *minPtr = (axisPtr->logScale) ? linePtr->x.logMin : linePtr->x.min;
	    *maxPtr = linePtr->x.max;
	} else {
	    *minPtr = (axisPtr->logScale) ? linePtr->y.logMin : linePtr->y.min;
	    *maxPtr = linePtr->y.max;
	}
    }
    return (numPoints);
}

/*
 *----------------------------------------------------------------------
 *
 * ClosestLine --
 *
 *	Find the distance of the data point in the element closest to
 *	the window coordinates point specified.
 *
 * Results:
 *	Returns the distance of the closest point.  In addition,
 *	indexPtr is set to position of coordinate and the list of data
 *	points.  Index is -1 if the point does not fail within the
 *	prescribed range.
 *
 *----------------------------------------------------------------------
 */
static int
ClosestLine(graphPtr, elemPtr, winX, winY, closePtr)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;		/* Element to examine */
    int winX, winY;		/* Window coordinates of point on screen */
    ClosestPoint *closePtr;	/* Info about closest point in element */
{
    int length;
    Line *linePtr = (Line *)elemPtr;
    unsigned int dist, maxDist, minDist;
    int delta;
    unsigned int deltaX, deltaY;
    double realDist;
    register int i;
    int index;

    length = linePtr->numPoints;
    maxDist = graphPtr->halo * graphPtr->halo;
    minDist = maxDist + 1;
    index = -1;
    for (i = 0; i < length; i++) {
	delta = winX - linePtr->pointArr[i].x;
	deltaX = BLT_ABS(delta);
	delta = winY - linePtr->pointArr[i].y;
	deltaY = BLT_ABS(delta);
	if ((deltaX > graphPtr->halo) || (deltaY > graphPtr->halo)) {
	    continue;
	}
	dist = deltaX * deltaX + deltaY * deltaY;
	if (dist < minDist) {
	    index = i;
	    minDist = dist;
	}
    }
    if (minDist > maxDist) {
	return 0;
    }
    realDist = sqrt((double)minDist);
    if (realDist > (double)graphPtr->halo) {
	return 0;
    }
    closePtr->elemPtr = elemPtr;
    closePtr->index = index;
    closePtr->x = linePtr->x.data[index];
    closePtr->y = linePtr->y.data[index];
    closePtr->dist = realDist;
    return (realDist);
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutLine --
 *
 *	Calculates the actual window coordinates of the line element.
 *	The window coordinates are saved in an allocated point array.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is (re)allocated for the point array.
 *
 *----------------------------------------------------------------------
 */
static void
LayoutLine(graphPtr, elemPtr)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;		/* Element component record */
{
    Line *linePtr = (Line *)elemPtr;
    XPoint *pointArr;
    unsigned int numPoints;
    int x, lastX;
    int total, numSegments;
    double avgSize;
    register int i;
    CurveSegment *segArr;
    int needSegments;

    numPoints = BLT_MIN(linePtr->x.length, linePtr->y.length);
    if (numPoints < 1) {
	linePtr->numPoints = linePtr->numSegments = 0;
	return;
    }
    pointArr = (XPoint *)malloc(numPoints * sizeof(XPoint));
    if (pointArr == NULL) {
	return;			/* Can't allocate point array */
    }
    /*
     * We need to determine is line segments exist only if
     *
     * 1) retracing is not allowed, and
     *
     * 2) we need to draw element lines (either because the symbol
     *    type is "line" or the line width is greater than zero,
     *	  which means to add a line in addition to the symbol).
     */
    needSegments = ((linePtr->trace != TRACE_BOTH) &&
	((linePtr->lineWidth > 0) || (linePtr->symbol == LINE_SYMBOL)));
    /*
     * Transform and store the coordinates into an array window
     * points.  In addition, make a quick count of line segments.
     */
    lastX = 0;			/* suppress compiler warning */
    total = numSegments = 0;
    for (i = 0; i < numPoints; i++) {
	pointArr[i] = Blt_TransformPt(graphPtr, linePtr->x.data[i],
	    linePtr->y.data[i], linePtr->axisFlags);
	x = pointArr[i].x;
	if ((needSegments) && (total > 0) &&
	    BROKEN_TRACE(linePtr->trace, x, lastX)) {
	    numSegments++;
	    total = 0;
	}
	total++;
	lastX = x;
    }
    if (total > 0) {
	numSegments++;
    }
    /* Create an array of line segment information  */
    segArr = (CurveSegment *) malloc(sizeof(CurveSegment) * numSegments);
    if (segArr == NULL) {
	return;
    }
    if (!needSegments) {
	segArr[0].pointPtr = pointArr;
	segArr[0].numPoints = numPoints;
    } else {
	int count;

	lastX = 0;		/* suppress compiler warning */
	total = count = 0;
	for (i = 0; i < numPoints; i++) {
	    x = pointArr[i].x;
	    if ((total > 0) && (BROKEN_TRACE(linePtr->trace, x, lastX))) {
		segArr[count].pointPtr = pointArr + (i - total);
		segArr[count].numPoints = total;
		count++;
		total = 0;
	    }
	    total++;
	    lastX = x;
	}
	if (total > 0) {
	    segArr[count].pointPtr = pointArr + (i - total);
	    segArr[count].numPoints = total;
	    count++;
	}
    }
    if (linePtr->pointArr != NULL) {
	free((char *)linePtr->pointArr);
    }
    linePtr->pointArr = pointArr;
    linePtr->numPoints = numPoints;
    if (linePtr->segArr != NULL) {
	free((char *)linePtr->segArr);
    }
    linePtr->segArr = segArr;
    linePtr->numSegments = numSegments;

    /* Make the symbol size odd so that its center is a single pixel. */
    linePtr->symbolSize |= 0x01;
    avgSize = graphPtr->avgSymSize * linePtr->symbolScale;
    linePtr->symbolSize = BLT_RND(avgSize);
}

/*
 * -----------------------------------------------------------------
 *
 * SymbolDrawInfo --
 *
 * 	Initialize a structure containing information about how to
 *	draw the a symbol at a particular size.  For example, we
 *	precalculate the offsets draw the symbol, rather than
 *	computing them at each data point. Some information is
 *	stored in this structure to save passing it in to each call
 *	of DrawLineSymbol.
 *
 * Results:
 *	Returns a pointer to a static symbol information structure.
 *
 * Problems:
 *	Most notable is the round-off errors generated when
 *	calculating the centered position of the symbol.
 *
 * -----------------------------------------------------------------
 */
static SymbolInfo *
SymbolDrawInfo(graphPtr, linePtr, size, active)
    Graph *graphPtr;		/* Graph widget record */
    Line *linePtr;		/* Line element information */
    int size;			/* Size of element */
    int active;			/* If non-zero, set to draw active element */
{
    static SymbolInfo s;
    int delta;

    s.display = graphPtr->display;
    s.canvas = graphPtr->canvas;
    s.radius = (size / 2);
    s.size = size;
    s.type = linePtr->symbol;
    s.needBorder = (linePtr->borderWidth > 0);

    switch (s.type) {
    case CROSS_SYMBOL:
    case PLUS_SYMBOL:

	delta = (size / 6);

	/*
	 *
	 *          2   3	The plus/cross symbol is a closed polygon
	 *			of 12 points. The diagram to the left
	 *    0,12  1   4    5	represents the positions of the points
	 *           x,y	which are computed below. The extra
	 *     11  10   7    6	(thirteenth) point connects the first and
	 *			last points.
	 *          9   8
	 */

	s.offset[0].x = s.offset[11].x = s.offset[12].x = -s.radius;
	s.offset[2].x = s.offset[1].x = s.offset[10].x = s.offset[9].x =
	    -delta;
	s.offset[3].x = s.offset[4].x = s.offset[7].x = s.offset[8].x =
	    delta;
	s.offset[5].x = s.offset[6].x = s.radius;
	s.offset[2].y = s.offset[3].y = -s.radius;
	s.offset[0].y = s.offset[1].y = s.offset[4].y = s.offset[5].y =
	    s.offset[12].y = -delta;
	s.offset[11].y = s.offset[10].y = s.offset[7].y = s.offset[6].y =
	    delta;
	s.offset[9].y = s.offset[8].y = s.radius;
	if (linePtr->symbol == CROSS_SYMBOL) {
	    register int i;
	    register double dx, dy, temp;

	    /* For the cross symbol, rotate the points by 45 degrees. */
	    for (i = 0; i < 12; i++) {
		dx = (double)(s.offset[i].x) * M_SQRT1_2;
		dy = (double)(s.offset[i].y) * M_SQRT1_2;
		temp = dx - dy;
		s.offset[i].x = BLT_RND(temp);
		temp = dx + dy;
		s.offset[i].y = BLT_RND(temp);
	    }
	    s.offset[12] = s.offset[0];
	}
	break;

    case DIAMOND_SYMBOL:

	/*
	 *
	 *             		The plus symbol is a closed polygon
	 *	      1		of 4 points. The diagram to the left
	 *                  	represents the positions of the points
	 *       0,4 x,y  2	which are computed below. The extra
	 *     			(fifth) point connects the first and
	 *	      3		last points.
	 *
	 */

	s.offset[0].y = s.offset[4].y = s.offset[2].y = s.offset[1].x =
	    s.offset[3].x = 0;
	s.offset[1].y = s.offset[4].x = s.offset[0].x = -s.radius;
	s.offset[3].y = s.offset[2].x = s.radius;
	break;

    case SPLUS_SYMBOL:
    case SCROSS_SYMBOL:
	s.radius = size / 2;
	s.offset[0].y = s.offset[1].y = s.offset[2].x = s.offset[3].x = 0;
	s.offset[0].x = s.offset[2].y = -s.radius;
	s.offset[1].x = s.offset[3].y = s.radius;
	/* Rotate the 4 points 45 degrees for a cross */
	if (s.type == SCROSS_SYMBOL) {
	    register int i;
	    register double dx, dy, temp;

	    for (i = 0; i < 4; i++) {
		dx = (double)(s.offset[i].x) * M_SQRT1_2;
		dy = (double)(s.offset[i].y) * M_SQRT1_2;
		temp = dx - dy;
		s.offset[i].x = BLT_RND(temp);
		temp = dx + dy;
		s.offset[i].y = BLT_RND(temp);
	    }
	}
	break;

    case SQUARE_SYMBOL:
	s.size = (int)ceil(size * M_SQRT1_2);
	s.radius = size / 2;
	break;

    case CIRCLE_SYMBOL:
	s.size--;
	break;

    case LINE_SYMBOL:
	break;
    }

    /* Set the correct GCs, depending if the element is active  */
    if (active) {
	s.borderGC = linePtr->activeBorderGC;
	s.lineGC = linePtr->activeLineGC;
	s.fillGC = linePtr->activeFillGC;
    } else {
	s.borderGC = linePtr->normalBorderGC;
	s.lineGC = linePtr->normalLineGC;
	s.fillGC = linePtr->normalFillGC;
    }
    return (&s);
}

/*
 * -----------------------------------------------------------------
 *
 * DrawSymbol --
 *
 * 	Draws the symbol (as indicated by the symbol information
 *	structure) centered at the given x,y coordinate.
 *
 * Results:
 *	None.
 *
 * -----------------------------------------------------------------
 */
static void
DrawSymbol(x, y, infoPtr)
    int x, y;			/* x,y coordinates of center of symbol */
    SymbolInfo *infoPtr;	/* Information how to draw the symbol */
{
    XPoint points[13];
    XSegment segments[2];
    register int n;

    switch (infoPtr->type) {
    case LINE_SYMBOL:
	/*
	 * Draw an extra line offset by one pixel from the previous to
	 * give a thicker appearance
	 */
	XDrawLine(infoPtr->display, infoPtr->canvas, infoPtr->lineGC,
	    x - infoPtr->radius, y, x + infoPtr->radius, y);
	XDrawLine(infoPtr->display, infoPtr->canvas, infoPtr->lineGC,
	    x - infoPtr->radius, y + 1, x + infoPtr->radius, y + 1);
	break;

    case PLUS_SYMBOL:
    case CROSS_SYMBOL:
	for (n = 0; n < 13; n++) {
	    points[n].x = infoPtr->offset[n].x + x;
	    points[n].y = infoPtr->offset[n].y + y;
	}
	XFillPolygon(infoPtr->display, infoPtr->canvas, infoPtr->fillGC,
	    points, 13, Complex, CoordModeOrigin);
	if (infoPtr->needBorder) {
	    XDrawLines(infoPtr->display, infoPtr->canvas, infoPtr->borderGC,
		points, 13, CoordModeOrigin);
	}
	break;

    case SPLUS_SYMBOL:
    case SCROSS_SYMBOL:
	segments[0].x1 = infoPtr->offset[0].x + x;
	segments[0].y1 = infoPtr->offset[0].y + y;
	segments[0].x2 = infoPtr->offset[1].x + x;
	segments[0].y2 = infoPtr->offset[1].y + y;
	segments[1].x1 = infoPtr->offset[2].x + x;
	segments[1].y1 = infoPtr->offset[2].y + y;
	segments[1].x2 = infoPtr->offset[3].x + x;
	segments[1].y2 = infoPtr->offset[3].y + y;
	/* I'm not sure what attributes this symbol type should have.
	 * Right now I using the border GC instead of line or fill GC */
	XDrawSegments(infoPtr->display, infoPtr->canvas, infoPtr->borderGC,
	    segments, 2);
	break;

    case SQUARE_SYMBOL:
	x -= infoPtr->radius, y -= infoPtr->radius;
	XFillRectangle(infoPtr->display, infoPtr->canvas, infoPtr->fillGC,
	    x, y, infoPtr->size, infoPtr->size);
	if (infoPtr->needBorder) {
	    XDrawRectangle(infoPtr->display, infoPtr->canvas,
		infoPtr->borderGC, x, y, infoPtr->size, infoPtr->size);
	}
	break;

    case CIRCLE_SYMBOL:
	x -= infoPtr->radius, y -= infoPtr->radius;
	XFillArc(infoPtr->display, infoPtr->canvas, infoPtr->fillGC, x, y,
	    infoPtr->size, infoPtr->size, 0, 23040);
	if (infoPtr->needBorder) {
	    XDrawArc(infoPtr->display, infoPtr->canvas, infoPtr->borderGC,
		x, y, infoPtr->size, infoPtr->size, 0, 23040);
	}
	break;

    case DIAMOND_SYMBOL:
	for (n = 0; n < 5; n++) {
	    points[n].x = infoPtr->offset[n].x + x;
	    points[n].y = infoPtr->offset[n].y + y;
	}
	XFillPolygon(infoPtr->display, infoPtr->canvas, infoPtr->fillGC,
	    points, 5, Convex, CoordModeOrigin);
	if (infoPtr->needBorder) {
	    XDrawLines(infoPtr->display, infoPtr->canvas, infoPtr->borderGC,
		points, 5, CoordModeOrigin);
	}
	break;
    }
}

/*
 * -----------------------------------------------------------------
 *
 * DrawSymbols --
 *
 * 	Draw a symbol centered at each point given in the array of
 *	window coordinates (coordArr) based upon the element symbol
 *	type and size.
 *
 * Results:
 *	None.
 *
 * Problems:
 *	Most notable is the round-off errors generated when
 *	calculating the centered position of the symbol.
 *
 * -----------------------------------------------------------------
 */
static void
DrawSymbols(graphPtr, elemPtr, size, pointArr, numPoints, active)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;		/* Line element information */
    int size;			/* Size of element */
    XPoint *pointArr;		/* Array of x,y coordinates for line */
    int numPoints;		/* Number of coordinates in array */
    int active;
{
    Line *linePtr = (Line *)elemPtr;
    SymbolInfo *infoPtr;
    register unsigned int i, index;
    unsigned int maxIndex;
    int useIndex;

    infoPtr = SymbolDrawInfo(graphPtr, linePtr, size, active);
    useIndex = ((active) && (linePtr->numActivePoints > 0));
    if (useIndex) {
	maxIndex = numPoints - 1;
	numPoints = linePtr->numActivePoints;
    }
    for (i = 0; i < numPoints; i++) {
	index = i;
	if (useIndex) {
	    index = linePtr->activeArr[i];
	    if (index > maxIndex) {
		continue;
	    }
	}
	DrawSymbol(pointArr[index].x, pointArr[index].y, infoPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DisplayLine --
 *
 *	Draws the connected line(s) representing the element. If the
 *	line is made up of non-line symbols and the line width parameter
 *	has been set (linewidth > 0), the element will also be drawn as
 *	a line (with the linewidth requested).  The line may consist of
 *	separate line segments.
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
DisplayLine(graphPtr, elemPtr, active)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;		/* Element to be drawn */
    int active;			/* Determines if line is drawn with normal
				 * or active foreground/background colors */
{
    Line *linePtr = (Line *)elemPtr;
    int needLine;
    GC lineGC;

    if (linePtr->numPoints < 1) {
	return;
    }
    if (active) {
	needLine = ((linePtr->numActivePoints == 0) &&
	    ((linePtr->activeLineWidth > 0) ||
		(linePtr->symbol == LINE_SYMBOL)));
	lineGC = linePtr->activeLineGC;
    } else {
	/*
	 * Draw lines if symbol is "line" or linewidth is greater than 0
	 */
	needLine = ((linePtr->lineWidth > 0) ||
	    (linePtr->symbol == LINE_SYMBOL));
	lineGC = linePtr->normalLineGC;
    }
    if (needLine) {
	register int i;

	for (i = 0; i < linePtr->numSegments; i++) {
	    XDrawLines(graphPtr->display, graphPtr->canvas, lineGC,
		linePtr->segArr[i].pointPtr, linePtr->segArr[i].numPoints,
		CoordModeOrigin);
	}
    }
    /* Draw non-line symbols */
    if (linePtr->symbol != LINE_SYMBOL) {
	DrawSymbols(graphPtr, (Element *)linePtr, linePtr->symbolSize,
	    linePtr->pointArr, linePtr->numPoints, active);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * SymbolPrintInfo --
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
static void
SymbolPrintInfo(graphPtr, linePtr, active)
    Graph *graphPtr;
    Line *linePtr;
    int active;
{
    XColor *bgColorPtr, *fgColorPtr;

    /* Set line and foreground attributes */
    if (active) {
	fgColorPtr = linePtr->activeFg;
	bgColorPtr = linePtr->activeBg;
    } else {
	fgColorPtr = linePtr->normalFg;
	bgColorPtr = linePtr->normalBg;
    }
    if ((linePtr->symbol == SPLUS_SYMBOL) ||
	(linePtr->symbol == SCROSS_SYMBOL)) {
	Blt_ForegroundToPostScript(graphPtr, bgColorPtr);
    } else {
	Blt_ForegroundToPostScript(graphPtr, fgColorPtr);
    }
    if (linePtr->symbol == LINE_SYMBOL) {
	Blt_LineWidthToPostScript(graphPtr, linePtr->lineWidth + 2);
	Blt_LineDashesToPostScript(graphPtr, linePtr->dashes);
    } else {
	Blt_LineWidthToPostScript(graphPtr, linePtr->lineWidth);
	Blt_LineDashesToPostScript(graphPtr, 0);
    }
    Tcl_AppendResult(graphPtr->interp, "/BgColorProc {\n   ", (char *)NULL);
    Blt_BackgroundToPostScript(graphPtr, bgColorPtr);
    Tcl_AppendResult(graphPtr->interp, "} def\n", (char *)NULL);

    if ((linePtr->symbol != LINE_SYMBOL) &&
	(linePtr->borderWidth > 0)) {
	Tcl_AppendResult(graphPtr->interp, "/BorderProc {\n  gsave\n",
	    (char *)NULL);
	Blt_BackgroundToPostScript(graphPtr, bgColorPtr);
	Blt_LineWidthToPostScript(graphPtr, linePtr->borderWidth);
	Tcl_AppendResult(graphPtr->interp,
	    "    stroke grestore } def\n", (char *)NULL);
    } else {
	Tcl_AppendResult(graphPtr->interp, "/BorderProc {} def\n",
	    (char *)NULL);
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
static void
PrintSymbols(graphPtr, elemPtr, size, pointArr, numPoints, active)
    Graph *graphPtr;
    Element *elemPtr;
    int size;
    XPoint *pointArr;
    int numPoints;
    int active;
{
    Line *linePtr = (Line *)elemPtr;
    double symbolSize;
    register int i;
    int useIndex;
    unsigned int index, maxIndex;

    SymbolPrintInfo(graphPtr, linePtr, active);
    symbolSize = (double)size;
    if ((elemPtr->symbol == SQUARE_SYMBOL) ||
	(elemPtr->symbol == DIAMOND_SYMBOL)) {
	/* Adjust square and diamond symbol sizes */
	symbolSize = ((double)size * M_SQRT1_2) - 1;
    }
    useIndex = ((active) && (linePtr->numActivePoints > 0));
    if (useIndex) {
	maxIndex = numPoints - 1;
	numPoints = linePtr->numActivePoints;
    }
    for (i = 0; i < numPoints; i++) {
	index = i;
	if (useIndex) {
	    index = linePtr->activeArr[i];
	    if (index > maxIndex) {
		continue;
	    }
	}
	sprintf(graphPtr->scratchPtr, "%d %d %g %.2s\n",
	    pointArr[index].x, pointArr[index].y, symbolSize,
	    symbolTokens[linePtr->symbol]);
	Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PrintLine --
 *
 *	Similar to the DrawLine procedure, prints PostScript related
 *	commands to form the connected line(s) representing the element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript pen width, dashes, and color settings are changed.
 *
 *----------------------------------------------------------------------
 */
static void
PrintLine(graphPtr, elemPtr, active)
    Graph *graphPtr;
    Element *elemPtr;
    int active;
{
    Line *linePtr = (Line *)elemPtr;
    XColor *bgColorPtr, *fgColorPtr;
    int needLine;
    int lineWidth;

    if (linePtr->numPoints < 1) {
	return;
    }
    if (active) {
	needLine = ((linePtr->numActivePoints == 0) &&
	    ((linePtr->activeLineWidth > 0) ||
		(linePtr->symbol == LINE_SYMBOL)));
	fgColorPtr = linePtr->activeFg;
	bgColorPtr = linePtr->activeBg;
	lineWidth = linePtr->activeLineWidth;
    } else {
	needLine =
	    ((linePtr->lineWidth > 0) || (linePtr->symbol == LINE_SYMBOL));
	fgColorPtr = linePtr->normalFg;
	bgColorPtr = linePtr->normalBg;
	lineWidth = linePtr->lineWidth;
    }

    /* Set line and foreground attributes */
    Blt_ForegroundToPostScript(graphPtr, fgColorPtr);

    if (linePtr->dashes > 0) {
	Blt_LineDashesToPostScript(graphPtr, linePtr->dashes);
	Tcl_AppendResult(graphPtr->interp, "/DashesProc {\ngsave\n",
	    (char *)NULL);
	Blt_BackgroundToPostScript(graphPtr, bgColorPtr);
	Blt_LineDashesToPostScript(graphPtr, 0);
	Tcl_AppendResult(graphPtr->interp, "stroke grestore\n} def\n",
	    (char *)NULL);
    } else {
	Tcl_AppendResult(graphPtr->interp, "/DashesProc {} def\n",
	    (char *)NULL);
    }
    Blt_LineWidthToPostScript(graphPtr, lineWidth);
    if (needLine) {
	register int i;

	for (i = 0; i < linePtr->numSegments; i++) {
	    Blt_PrintLine(graphPtr, linePtr->segArr[i].pointPtr,
		linePtr->segArr[i].numPoints);
	}
    }
    /* Draw non-line symbols */

    if (linePtr->symbol != LINE_SYMBOL) {
	(*linePtr->printSymbolsProc) (graphPtr, (Element *)linePtr,
	    linePtr->symbolSize, linePtr->pointArr, linePtr->numPoints,
	    active);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyLine --
 *
 *	Release memory and resources allocated for the line element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the line element is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyLine(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Line *linePtr = (Line *)elemPtr;

    Tk_FreeOptions(linePtr->configSpecs, (char *)linePtr,
	graphPtr->display, 0);

    if (linePtr->normalBorderGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->normalBorderGC);
    }
    if (linePtr->normalFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->normalFillGC);
    }
    if (linePtr->normalLineGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->normalLineGC);
    }
    if (linePtr->activeBorderGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->activeBorderGC);
    }
    if (linePtr->activeFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->activeFillGC);
    }
    if (linePtr->activeLineGC != NULL) {
	Tk_FreeGC(graphPtr->display, linePtr->activeLineGC);
    }
    if (linePtr->pointArr != NULL) {
	free((char *)linePtr->pointArr);
    }
    if (linePtr->segArr != NULL) {
	free((char *)linePtr->segArr);
    }
    if (linePtr->x.data != NULL) {
	free((char *)linePtr->x.data);
    }
    if (linePtr->y.data != NULL) {
	free((char *)linePtr->y.data);
    }
    if (linePtr->activeArr != linePtr->staticArr) {
	free((char *)linePtr->activeArr);
    }
    free((char *)linePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LineElement --
 *
 *	Allocate memory and initialize methods for the new line element.
 *
 * Results:
 *	The pointer to the newly allocated element structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the line element structure.
 *
 *----------------------------------------------------------------------
 */
Element *
Blt_LineElement()
{
    register Line *linePtr;

    linePtr = (Line *)calloc(1, sizeof(Line));
    if (linePtr == NULL) {
	return NULL;
    }
    linePtr->configSpecs = configSpecs;
    linePtr->configProc = ConfigureLine;
    linePtr->destroyProc = DestroyLine;
    linePtr->displayProc = DisplayLine;
    linePtr->limitsProc = GetLineLimits;
    linePtr->closestProc = ClosestLine;
    linePtr->layoutProc = LayoutLine;
    linePtr->printProc = PrintLine;
    linePtr->drawSymbolsProc = DrawSymbols;
    linePtr->printSymbolsProc = PrintSymbols;
    linePtr->borderWidth = 1;
    linePtr->activeLineWidth = linePtr->lineWidth = 1;
    linePtr->symbolScale = 1.0;
    linePtr->trace = TRACE_BOTH;
    linePtr->type = LINE_ELEM_TYPE;
    return ((Element *)linePtr);
}
