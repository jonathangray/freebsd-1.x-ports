
/*
 * bltGrLegd.c --
 *
 *	This module implements legends for the graph widget for
 *	the Tk toolkit.
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
#include "bltGrElem.h"

#define DEF_LEGEND_ACTIVE_BG_COLOR 	BISQUE2
#define DEF_LEGEND_ACTIVE_BG_MONO	WHITE
#define DEF_LEGEND_ACTIVE_BORDER_WIDTH  "2"
#define DEF_LEGEND_ACTIVE_FG_COLOR	BLACK
#define DEF_LEGEND_ACTIVE_FG_MONO	BLACK
#define DEF_LEGEND_ACTIVE_RELIEF	"flat"
#define DEF_LEGEND_ANCHOR	   	"nw"
#define DEF_LEGEND_BG_COLOR	   	BISQUE1
#define DEF_LEGEND_BG_MONO		WHITE
#define DEF_LEGEND_BORDER_WIDTH 	"2"
#define DEF_LEGEND_FG_COLOR		BLACK
#define DEF_LEGEND_FG_MONO		BLACK
#define DEF_LEGEND_FONT			"*-Helvetica-Bold-R-Normal-*-120-*"
#define DEF_LEGEND_IPAD_X		"1"
#define DEF_LEGEND_IPAD_Y		"1"
#define DEF_LEGEND_MAPPED       	"1"
#define DEF_LEGEND_PAD_X		"4"
#define DEF_LEGEND_PAD_Y		"0"
#define DEF_LEGEND_POSITION		(char *)NULL
#define DEF_LEGEND_RELIEF		"sunken"

extern Tk_CustomOption bltPositionOption;

/*
 * -------------------------------------------------------------------
 *
 * Legend --
 *
 * 	Contains information specific to how the legend will be
 *	displayed.
 *
 * -------------------------------------------------------------------
 */


typedef struct {
    int mapped;			/* Requested state of the legend, If non-zero,
				 * legend is displayed */
    unsigned int width, height;	/* Dimensions of the legend */
    XPoint anchorPos;		/* Window coordinates of legend positioning
				 * point. Used in conjunction with the anchor
				 * to determine the location of the legend. If
				 * x or y are DEF_POSITION the legend is set
				 * in the right margin */
    int useDefault;		/* Use the default legend position */

    LegendDisplayProc *displayProc;
    LegendPrintProc *printProc;
    LegendDestroyProc *destroyProc;
    LegendGeometryProc *geomProc;

    int ipadX, ipadY;		/* # of pixels padding around legend entries */
    int padX, padY;		/* # of pixels padding to exterior of legend */
    unsigned int numLabels;	/* Number of labels (and symbols) to display */
    unsigned int numCols;	/* Number of columns in legend */
    unsigned int numRows;	/* Number of rows in legend */
    unsigned int entryWidth;
    unsigned int entryHeight;

    int maxSymSize;		/* Size of largest symbol to be displayed.
				 * Used to calculate size of legend */
    Tk_Anchor anchor;		/* Anchor of legend. Used to interpret the
				 * positioning point of the legend in the
				 * graph*/
    XFontStruct *fontPtr;	/* Font for legend text */

    Tk_3DBorder border;		/* 3-D border and background color legend. */
    int borderWidth;		/* Width of legend 3-D border */
    int relief;			/* 3-d effect of border around the legend:
				 * TK_RELIEF_RAISED etc. */
    XColor *normalFg;		/* Foreground color for legend
				 * text. Symbols retain the color
				 * specified for the element*/
    GC normalGC;		/* Normal legend entry graphics context */

    Tk_3DBorder activeBorder;	/* Background color for active legend
				 * entries. */
    int activeBW;		/* Width of legend 3-D border */
    int activeRelief;		/* 3-d effect: TK_RELIEF_RAISED etc. */
    XColor *activeFg;		/* Foreground color for active legend
				 * entries. Symbols retain the color
				 * specified for the element*/
    GC activeGC;		/* Active legend entry graphics context */

} Legend;

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BORDER, "-activebackground", "legendActiveBackground",
	"ActiveBackground", DEF_LEGEND_ACTIVE_BG_COLOR,
	Tk_Offset(Legend, activeBorder), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-activebackground", "legendActiveBackground",
	"ActiveBackground", DEF_LEGEND_ACTIVE_BG_MONO,
	Tk_Offset(Legend, activeBorder), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_INT, "-activeborderwidth", "legendActiveBorderWidth",
	"BorderWidth", DEF_LEGEND_BORDER_WIDTH, Tk_Offset(Legend, activeBW),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-activeforeground", "legendActiveForeground",
	"ActiveForeground", DEF_LEGEND_ACTIVE_FG_COLOR,
	Tk_Offset(Legend, activeFg), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground", "legendActiveForeground",
	"ActiveForeground", DEF_LEGEND_ACTIVE_FG_MONO,
	Tk_Offset(Legend, activeFg), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_RELIEF, "-activerelief", "legendActiveRelief", "Relief",
	DEF_LEGEND_ACTIVE_RELIEF, Tk_Offset(Legend, activeRelief),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_ANCHOR, "-anchor", "legendAnchor", "Anchor",
	DEF_LEGEND_ANCHOR, Tk_Offset(Legend, anchor),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_SYNONYM, "-bg", "legendBackground", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_BORDER, "-background", "legendBackground", "Background",
	DEF_LEGEND_BG_MONO, Tk_Offset(Legend, border),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-background", "legendBackground", "Background",
	DEF_LEGEND_BG_COLOR, Tk_Offset(Legend, border),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_INT, "-borderwidth", "legendBorderWidth", "BorderWidth",
	DEF_LEGEND_BORDER_WIDTH, Tk_Offset(Legend, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_SYNONYM, "-bd", "legendBorderWidth", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_FONT, "-font", "legendFont", "Font",
	DEF_LEGEND_FONT, Tk_Offset(Legend, fontPtr), 0},
    {TK_CONFIG_SYNONYM, "-fg", "legendForeground", (char *)NULL, (char *)NULL,
	0, 0},
    {TK_CONFIG_COLOR, "-foreground", "legendForeground", "Foreground",
	DEF_LEGEND_FG_COLOR, Tk_Offset(Legend, normalFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "legendForeground", "Foreground",
	DEF_LEGEND_FG_MONO, Tk_Offset(Legend, normalFg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_PIXELS, "-ipadx", "legendIPadX", "Pad",
	DEF_LEGEND_IPAD_X, Tk_Offset(Legend, ipadX),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-ipady", "legendIPadY", "Pad",
	DEF_LEGEND_IPAD_Y, Tk_Offset(Legend, ipadY),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-mapped", "legendMapped", "Mapped",
	DEF_LEGEND_MAPPED, Tk_Offset(Legend, mapped),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-padx", "legendPadX", "Pad",
	DEF_LEGEND_PAD_X, Tk_Offset(Legend, padX), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-pady", "legendPadY", "Pad",
	DEF_LEGEND_PAD_Y, Tk_Offset(Legend, padY), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-position", "legendPosition", "Position",
	DEF_LEGEND_POSITION, Tk_Offset(Legend, anchorPos),
	TK_CONFIG_NULL_OK, &bltPositionOption},
    {TK_CONFIG_RELIEF, "-relief", "legendRelief", "Relief",
	DEF_LEGEND_RELIEF, Tk_Offset(Legend, relief),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

extern int Blt_GetPosition _ANSI_ARGS_((Tcl_Interp *interp, char *string,
	XPoint *pointPtr));

static XPoint
GetOrigin(graphPtr, legendPtr)
    Graph *graphPtr;
    Legend *legendPtr;
{
    int x, y;
    Tk_Anchor anchor;
    XPoint origin;

    x = (graphPtr->width - graphPtr->borderWidth);
    y = (graphPtr->extreme.y - graphPtr->plotBW);
    anchor = TK_ANCHOR_NE;
    if (!legendPtr->useDefault) {
	x = legendPtr->anchorPos.x;
	y = legendPtr->anchorPos.y;
	anchor = legendPtr->anchor;
    }
    origin = Blt_TranslateBoxCoords(x, y, legendPtr->width, legendPtr->height,
	anchor);
    origin.x += legendPtr->padX;
    origin.y += legendPtr->padY;
    return (origin);
}



static int
GetIndex(legendPtr, originPtr, pointPtr)
    Legend *legendPtr;
    XPoint *originPtr, *pointPtr;
{
    int x, y;
    unsigned int width, height;
    unsigned int row, column;
    unsigned int index;

    x = originPtr->x + legendPtr->borderWidth;
    y = originPtr->y + legendPtr->borderWidth;
    width = legendPtr->width - 2 * (legendPtr->padX + legendPtr->borderWidth);
    height = legendPtr->height - 2 * (legendPtr->padY + legendPtr->borderWidth);

    if ((pointPtr->x < x) || (pointPtr->x > (x + width)) ||
	(pointPtr->y < y) || (pointPtr->y > (y + height))) {
	return -1;
    }
    /* It's in the box. Compute the label index */

    row = (pointPtr->y - y) / legendPtr->entryHeight;
    column = (pointPtr->x - x) / legendPtr->entryWidth;
    index = (column * legendPtr->numRows) + row;
    if (index >= legendPtr->numLabels) {
	return -1;
    }
    return index;
}



static Element *
LocateElement(graphPtr, legendPtr, elemId)
    Graph *graphPtr;
    Legend *legendPtr;
    char *elemId;
{
    Blt_ListEntry *entryPtr;
    Element *elemPtr;

    elemPtr = NULL;
    if (elemId[0] == '@') {
	XPoint origin, point;
	int count, index;

	if (Blt_GetPosition(graphPtr->interp, elemId, &point) != TCL_OK) {
	    return NULL;
	}
	origin = GetOrigin(graphPtr, legendPtr);
	index = GetIndex(legendPtr, &origin, &point);
	if (index < 0) {
	    return NULL;
	}
	count = 0;
	for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	    entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	    elemPtr = (Element *)Blt_GetListValue(entryPtr);
	    if ((elemPtr->x.length < 1) || (elemPtr->y.length < 1) ||
		(elemPtr->label == NULL)) {
		continue;
	    }
	    if (count == index) {
		break;
	    }
	    count++;
	}
    }
    return (elemPtr);
}

/*
 * -----------------------------------------------------------------
 *
 * ComputeLegendGeometry --
 *
 * 	Calculates the dimensions (width and height) needed for
 *	the legend.  Also determines the number of rows and columns
 *	necessary to list all the valid element labels.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *   	The following fields of the legend are calculated and set.
 *
 * 	numLabels   - number of valid labels of elements in the
 *		      display list.
 * 	numRows	    - number of rows of entries
 * 	numCols	    - number of columns of entries
 * 	entryHeight - height of each entry
 * 	entryWidth  - width of each entry
 * 	height	    - width of legend (includes borders and padding)
 * 	width	    - height of legend (includes borders and padding)
 *
 * -----------------------------------------------------------------
 */
static void
ComputeLegendGeometry(graphPtr, maxHeight)
    Graph *graphPtr;
    int maxHeight;
{
    Legend *legendPtr = (Legend *)graphPtr->legendPtr;
    unsigned int width, height, twiceBW;
    unsigned int numLabels, numRows, numCols;

    numCols = numRows = numLabels = 0;
    height = width = 0;
    if (legendPtr->mapped) {
	register Blt_ListEntry *entryPtr;
	Element *elemPtr;
	unsigned int textHeight, entryHeight, entryWidth;
	unsigned int w;

	for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	    entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	    elemPtr = (Element *)Blt_GetListValue(entryPtr);
	    if ((elemPtr->x.length < 1) || (elemPtr->y.length < 1) ||
		(elemPtr->label == NULL)) {
		continue;	/* Ignore elements with incomplete data */
	    }
	    w = Blt_TextStringWidth(legendPtr->fontPtr, elemPtr->label);
	    if (w > width) {
		width = w;
	    }
	    numLabels++;
	}
	textHeight = TEXTHEIGHT(legendPtr->fontPtr);
	twiceBW = (2 * legendPtr->activeBW);
	entryHeight = textHeight + twiceBW + (2 * legendPtr->ipadY);
	entryWidth = width + textHeight + twiceBW + (3 * legendPtr->ipadX);
	maxHeight -= 2 * (legendPtr->borderWidth + legendPtr->padY);
	numRows = maxHeight / entryHeight;
	if (numRows > 0) {
	    numCols = ((numLabels - 1) / numRows) + 1;
	    if (numRows > numLabels) {
		numRows = numLabels;
	    }
	    height = (2 * (legendPtr->borderWidth + legendPtr->padY)) +
		(numRows * entryHeight);
	    width = (2 * (legendPtr->borderWidth + legendPtr->padX)) +
		(numCols * entryWidth);
	}
	legendPtr->entryWidth = entryWidth;
	legendPtr->entryHeight = entryHeight;
	legendPtr->numRows = numRows;
	legendPtr->numCols = numCols;
    }
    legendPtr->numLabels = numLabels;
    legendPtr->height = height;
    legendPtr->width = width;
}

/*
 * -----------------------------------------------------------------
 *
 * DisplayLegend --
 *
 * -----------------------------------------------------------------
 */
static void
DisplayLegend(graphPtr)
    Graph *graphPtr;
{
    Legend *legendPtr = (Legend *)graphPtr->legendPtr;
    int x, y, startY;
    register Element *elemPtr;
    unsigned int width, height;
    unsigned int labelX, symbolX, symbolY;
    unsigned int counter;
    Blt_ListEntry *entryPtr;
    XPoint origPos, curPos;
    unsigned int symSize, midPoint;
    TextAttributes textAttr;
    int redraw;

    graphPtr->flags &= ~LEGEND_ONLY;
    if ((!legendPtr->mapped) || (legendPtr->numLabels == 0) ||
	(legendPtr->numRows == 0) || (legendPtr->numCols == 0)) {
	return;
    }
    width = legendPtr->width - (2 * legendPtr->padX);
    height = legendPtr->height - (2 * legendPtr->padY);
    symSize = legendPtr->fontPtr->ascent;
    midPoint = (symSize / 2) + 1 + legendPtr->activeBW;

    labelX = TEXTHEIGHT(legendPtr->fontPtr) + legendPtr->activeBW +
	(2 * legendPtr->ipadX);
    symbolY = midPoint + legendPtr->ipadY;
    symbolX = midPoint + legendPtr->ipadX;

    origPos = GetOrigin(graphPtr, legendPtr);
    x = origPos.x, y = origPos.y, redraw = 0;

    if (graphPtr->canvas == None) {
	/*
	 * If there's no pixmap already to draw into, then this
	 * routine was called from Tk_DoWhenIdle instead of
	 * DisplayGraph.  Create a temporary pixmap and reset the x,y
	 * coordinates to do a quick redisplay of just the legend.
	 */
	graphPtr->canvas = XCreatePixmap(graphPtr->display,
	    Tk_WindowId(graphPtr->tkwin), width, height,
	    Tk_Depth(graphPtr->tkwin));
	x = y = 0;
	redraw = 1;
    }
    Tk_Fill3DRectangle(graphPtr->display, graphPtr->canvas,
	legendPtr->border, x, y, width, height, legendPtr->borderWidth,
	legendPtr->relief);

    x += legendPtr->borderWidth;
    y += legendPtr->borderWidth;

    textAttr.fontPtr = legendPtr->fontPtr;
    textAttr.anchor = TK_ANCHOR_W;
    textAttr.theta = 0.0;
    textAttr.bgColorPtr = (XColor *)NULL;
    textAttr.gc = legendPtr->normalGC;

    counter = 0;
    startY = y;
    for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	elemPtr = (Element *)Blt_GetListValue(entryPtr);

	/*
	 * Draw each symbol and label.  Check that the label isn't NULL and
	 * that the element has at least one data point.
	 */

	if ((elemPtr->x.length < 1) || (elemPtr->y.length < 1) ||
	    (elemPtr->label == NULL)) {
	    continue;
	}
	textAttr.gc = legendPtr->normalGC;

	if (elemPtr->flags & LABEL_ACTIVE) {
	    Tk_Fill3DRectangle(graphPtr->display, graphPtr->canvas,
		legendPtr->activeBorder, x, y, legendPtr->entryWidth,
		legendPtr->entryHeight, legendPtr->activeBW,
		legendPtr->activeRelief);
	    textAttr.gc = legendPtr->activeGC;
	}
	curPos.x = x + symbolX;
	curPos.y = y + symbolY;
	(*elemPtr->drawSymbolsProc) (graphPtr, elemPtr, symSize, &curPos, 1,
	    ELEM_NORMAL);
	curPos.x = x + labelX;
	Blt_DrawText(graphPtr->display, graphPtr->canvas, elemPtr->label,
	    &textAttr, curPos.x, curPos.y);
	counter++;

	/* Check when to move to the next column */
	if ((counter % legendPtr->numRows) > 0) {
	    y += legendPtr->entryHeight;
	} else {
	    x += legendPtr->entryWidth;
	    y = startY;
	}
    }
    if (redraw) {
	(*graphPtr->crosshairs->toggleProc) (graphPtr);
	XCopyArea(graphPtr->display, graphPtr->canvas,
	    Tk_WindowId(graphPtr->tkwin), graphPtr->marginGC, 0, 0,
	    width, height, origPos.x, origPos.y);
	(*graphPtr->crosshairs->toggleProc) (graphPtr);
	XFreePixmap(graphPtr->display, graphPtr->canvas);
	graphPtr->canvas = None;
    }
}

/*
 * -----------------------------------------------------------------
 *
 * PrintLegend --
 *
 * -----------------------------------------------------------------
 */
static void
PrintLegend(graphPtr)
    Graph *graphPtr;
{
    Legend *legendPtr = (Legend *)graphPtr->legendPtr;
    int x, y, startY;
    register Element *elemPtr;
    unsigned int labelX, symbolX, symbolY;
    unsigned int counter;
    Blt_ListEntry *entryPtr;
    XPoint origPos, curPos;
    Tk_Anchor anchor;		/* Anchor of legend */
    unsigned int symSize, midPoint;
    int width, height;
    TextAttributes textAttr;

    if ((!legendPtr->mapped) || (legendPtr->numLabels == 0) ||
	(legendPtr->numRows == 0) || (legendPtr->numCols == 0)) {
	return;
    }
    /*
     * First calculate the upper left coordinate of the legend.
     * Consider the anchor.
     */

    x = graphPtr->width - graphPtr->borderWidth;
    y = graphPtr->extreme.y - graphPtr->plotBW;
    anchor = TK_ANCHOR_NE;

    if (!legendPtr->useDefault) {
	double scale, coord;
	/*
	 * Legend position was given in window coordinates so we have to
	 * scale using the current page coordinates.
	 */
	scale = (double)graphPtr->width / Tk_Width(graphPtr->tkwin);
	coord = (legendPtr->anchorPos.x * scale);
	x = BLT_RND(coord);
	scale = (double)graphPtr->height / Tk_Height(graphPtr->tkwin);
	coord = (legendPtr->anchorPos.y * scale);
	y = BLT_RND(coord);
	anchor = legendPtr->anchor;
    }
    origPos = Blt_TranslateBoxCoords(x, y, legendPtr->width,
	legendPtr->height, anchor);

    x = origPos.x + legendPtr->padX;
    y = origPos.y + legendPtr->padY;

    width = legendPtr->width - (2 * legendPtr->padX);
    height = legendPtr->height - (2 * legendPtr->padY);
    Blt_3DRectangleToPostScript(graphPtr, legendPtr->border, x, y, width,
	height, legendPtr->borderWidth, legendPtr->relief);

    x += legendPtr->borderWidth;
    y += legendPtr->borderWidth;

    symSize = legendPtr->fontPtr->ascent;
    midPoint = (symSize / 2) + 1 + legendPtr->activeBW;

    labelX = TEXTHEIGHT(legendPtr->fontPtr) + legendPtr->activeBW +
	(2 * legendPtr->ipadX);
    symbolY = midPoint + legendPtr->ipadY;
    symbolX = midPoint + legendPtr->ipadX;

    textAttr.fontPtr = legendPtr->fontPtr;
    textAttr.anchor = TK_ANCHOR_W;
    textAttr.theta = 0.0;
    textAttr.fgColorPtr = graphPtr->marginFg;
    textAttr.bgColorPtr = (XColor *)NULL;

    counter = 0;
    startY = y;
    for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	elemPtr = (Element *)Blt_GetListValue(entryPtr);

	/*
	 * Print each symbol and label.  Check that the label isn't NULL and
	 * that the element has at least one data point.
	 */
	if ((elemPtr->x.length < 1) || (elemPtr->y.length < 1) ||
	    (elemPtr->label == NULL)) {
	    continue;
	}
	textAttr.fgColorPtr = legendPtr->normalFg;
	if (elemPtr->flags & LABEL_ACTIVE) {
	    Blt_3DRectangleToPostScript(graphPtr, legendPtr->activeBorder,
		x, y, legendPtr->entryWidth, legendPtr->entryHeight,
		legendPtr->activeBW, legendPtr->activeRelief);
	    textAttr.fgColorPtr = legendPtr->activeFg;
	}
	curPos.x = x + symbolX;
	curPos.y = y + symbolY;
	(*elemPtr->printSymbolsProc) (graphPtr, elemPtr, symSize, &curPos, 1,
	    ELEM_NORMAL);
	curPos.x = x + labelX;
	Blt_TextToPostScript(graphPtr, elemPtr->label, &textAttr, curPos.x,
	    curPos.y);
	counter++;
	if ((counter % legendPtr->numRows) > 0) {
	    y += legendPtr->entryHeight;
	} else {
	    x += legendPtr->entryWidth;
	    y = startY;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureLegend --
 *
 * 	Routine to configure the legend.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureLegend(graphPtr, legendPtr, argc, argv, flags)
    Graph *graphPtr;
    Legend *legendPtr;
    int argc;
    char *argv[];
    int flags;
{
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;
    int newFlag;

    if (Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin,
	    configSpecs, argc, argv, (char *)legendPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    gcMask = GCForeground | GCFont;
    gcValues.font = legendPtr->fontPtr->fid;
    gcValues.foreground = legendPtr->normalFg->pixel;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (legendPtr->normalGC != NULL) {
	Tk_FreeGC(graphPtr->display, legendPtr->normalGC);
    }
    legendPtr->normalGC = newGC;

    gcMask |= GCBackground;
    gcValues.foreground = legendPtr->activeFg->pixel;
    gcValues.background = Tk_3DBorderColor(legendPtr->activeBorder)->pixel;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (legendPtr->activeGC != NULL) {
	Tk_FreeGC(graphPtr->display, legendPtr->activeGC);
    }
    legendPtr->activeGC = newGC;
    newFlag = (legendPtr->anchorPos.x == DEF_POSITION);

    /*
     *
     *  Update the layout of the graph (and redraw the elements) if
     *  any of the following legend options which affect the size of
     *	the legend changed.
     *
     *		-activeborderwidth, -borderwidth
     *		-font
     *		-mapped
     *		-ipadx, -ipady, -padx, -pady
     *
     *  If the position of the legend changed to/from the default
     *  position, also indicate that a new layout is needed.
     *
     */
    if ((legendPtr->useDefault != newFlag) || ((legendPtr->useDefault) &&
	    (Blt_OptionChanged(configSpecs, "-*borderwidth", "-*pad?",
		    "-mapped", "-font", (char *)NULL)))) {
	graphPtr->flags |= (DIRTY | LAYOUT_ALL);
	legendPtr->useDefault = newFlag;
    }
    graphPtr->flags |= REFRESH;
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyLegend --
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with the legend are freed.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyLegend(graphPtr)
    Graph *graphPtr;
{
    Legend *legendPtr = (Legend *)graphPtr->legendPtr;

    Tk_FreeOptions(configSpecs, (char *)legendPtr, graphPtr->display, 0);
    if (legendPtr->normalGC != NULL) {
	Tk_FreeGC(graphPtr->display, legendPtr->normalGC);
    }
    if (legendPtr->activeGC != NULL) {
	Tk_FreeGC(graphPtr->display, legendPtr->activeGC);
    }
    free((char *)legendPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateLegend --
 *
 * 	Creates and initializes a legend structure with default settings
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
int
Blt_CreateLegend(graphPtr)
    Graph *graphPtr;
{
    Legend *legendPtr;

    legendPtr = (Legend *)calloc(1, sizeof(Legend));
    if (legendPtr == NULL) {
	graphPtr->interp->result = "can't allocate legend structure";
	return TCL_ERROR;
    }
    legendPtr->mapped = TRUE;
    legendPtr->anchorPos.x = legendPtr->anchorPos.y = DEF_POSITION;
    legendPtr->useDefault = 1;
    legendPtr->relief = TK_RELIEF_SUNKEN;
    legendPtr->activeRelief = TK_RELIEF_FLAT;
    legendPtr->activeBW = legendPtr->borderWidth = 2;
    legendPtr->ipadX = legendPtr->ipadY = 1;
    legendPtr->padX = 4;
    legendPtr->padY = 0;
    legendPtr->anchor = TK_ANCHOR_NW;
    legendPtr->displayProc = DisplayLegend;
    legendPtr->printProc = PrintLegend;
    legendPtr->destroyProc = DestroyLegend;
    legendPtr->geomProc = ComputeLegendGeometry;
    graphPtr->legendPtr = (GraphLegend *)legendPtr;
    return (ConfigureLegend(graphPtr, legendPtr, 0, (char **)NULL, 0));
}

/*
 *----------------------------------------------------------------------
 *
 * GetEntry --
 *
 * 	Find the legend entry from the given argument.  The argument
 *	can be either a screen position "@x,y" or the name of an
 *	element.
 *
 *	I don't know how useful it is to test with the name of an
 *	element.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
static int
GetEntry(graphPtr, legendPtr, argc, argv)
    Graph *graphPtr;
    Legend *legendPtr;
    int argc;
    char *argv[];
{
    register Element *elemPtr;

    if ((!legendPtr->mapped) || (legendPtr->numLabels == 0)) {
	return TCL_OK;
    }
    if (argc != 4) {
	Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
	    argv[0], " legend search index\"", NULL);
	return TCL_ERROR;
    }
    elemPtr = LocateElement(graphPtr, legendPtr, argv[3]);
    if (elemPtr != NULL) {
	Tcl_SetResult(graphPtr->interp, elemPtr->id, TCL_STATIC);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ActivateEntry --
 *
 * 	Routine to configure the legend.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
static int
ActivateEntry(graphPtr, legendPtr, argc, argv)
    Graph *graphPtr;
    Legend *legendPtr;
    int argc;
    char *argv[];
{
    Element *elemPtr;
    Tcl_HashEntry *entryPtr;
    int active;
    int redrawNeeded;
    register int i;

    if (argc < 4) {
	Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
	    argv[0], " legend ", argv[2], " name...\"", NULL);
	return TCL_ERROR;
    }
    active = (argv[2][0] == 'a') ? LABEL_ACTIVE : 0;
    redrawNeeded = 0;
    for (i = 3; i < argc; i++) {
	entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[i]);
	if (entryPtr == NULL) {
	    Tcl_AppendResult(graphPtr->interp, "can't find an element \"",
		argv[i], "\"", (char *)NULL);
	    return TCL_ERROR;
	}
	elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
	if (active != (elemPtr->flags & LABEL_ACTIVE)) {
	    elemPtr->flags ^= LABEL_ACTIVE;
	    if (elemPtr->label != NULL) {
		redrawNeeded++;
	    }
	}
    }
    if ((redrawNeeded) && (legendPtr->mapped)) {
	/*
	 * We need to redraw the legend. If there isn't a redraw
	 * already pending for the whole graph, we can redraw just the
	 * legend, calling the legend's display routine rather than
	 * the graph's.  The window must be mapped though.
	 */
	if (graphPtr->flags & REDRAW_PENDING) {
	    graphPtr->flags |= REFRESH;
	} else if (!(graphPtr->flags & LEGEND_ONLY)) {
	    if ((graphPtr->tkwin != NULL) && (Tk_IsMapped(graphPtr->tkwin))) {
		Tk_DoWhenIdle((Tk_IdleProc *) DisplayLegend,
		    (ClientData)graphPtr);
		graphPtr->flags |= LEGEND_ONLY;
	    }
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LegendCmd --
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Legend is possibly redrawn.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
int
Blt_LegendCmd(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char **argv;
{
    char c;
    int length;
    Tcl_Interp *interp = graphPtr->interp;
    Legend *legendPtr = (Legend *)graphPtr->legendPtr;

    /* Initialize the crosshairs on first call */

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " legend option ?args?\"", NULL);
	return TCL_ERROR;
    }
    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 'c') && (strncmp(argv[2], "configure", length) == 0)) {
	int flags = TK_CONFIG_ARGV_ONLY;

	if (argc == 3) {
	    return (Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin,
		    configSpecs, (char *)legendPtr, (char *)NULL, flags));
	} else if (argc == 4) {
	    return (Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin,
		    configSpecs, (char *)legendPtr, argv[3], flags));
	}
	if (ConfigureLegend(graphPtr, legendPtr, argc - 3,
		argv + 3, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else if ((c == 'g') && (strncmp(argv[2], "get", length) == 0)) {
	return (GetEntry(graphPtr, legendPtr, argc, argv));
    } else if ((c == 'a') && (strncmp(argv[2], "activate", length) == 0)) {
	return (ActivateEntry(graphPtr, legendPtr, argc, argv));
    } else if ((c == 'd') && (strncmp(argv[2], "deactivate", length) == 0)) {
	return (ActivateEntry(graphPtr, legendPtr, argc, argv));
    } else {
	Tcl_AppendResult(interp, "bad legend option \"", argv[2],
	    "\": should be activate, configure, deacitvate, or get",
	    (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}
