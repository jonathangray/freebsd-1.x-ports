/*
 * bltGraph.c --
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
 * Graph widget created by Sani Nassif and George Howlett.
 */

/*
 * To do:
 *
 * 1) Fix log manual scale for log axes.
 *
 * 2) Update manual pages.
 *
 * 3) Update comments.
 *
 * 5) Contour and flow graphs
 *
 * 6) Account for roundoff error when calculating bar widths
 *
 * 7) Arrows for line tags
 *
 * 9) Make sure reasonable defaults show for configuration items
 *
 * 10) Rubberbox for zooming???
 */

#include "blt.h"
#include "bltGraph.h"
#include "bltGrTag.h"
#include "bltGrElem.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#ifndef GRAPH_VERSION
#define GRAPH_VERSION "4.0"
#endif

static char *classNames[] =
{
    "Blt_graph", "Blt_barchart"
};

static unsigned int configFlags[] =
{
    XYGRAPH_MASK, BARCHART_MASK
};

#define DEF_GRAPH_BAR_WIDTH	"0.95"
#define DEF_GRAPH_BG_COLOR	BISQUE1
#define DEF_GRAPH_BG_MONO	WHITE
#define DEF_GRAPH_BORDER_WIDTH	"0"
#define DEF_GRAPH_BORDER_WIDTH  "0"
#define DEF_GRAPH_BUFFERED	"1"
#define DEF_GRAPH_CURSOR  	"crosshair"
#define DEF_GRAPH_FG_COLOR	BLACK
#define DEF_GRAPH_FG_MONO	BLACK
#define DEF_GRAPH_FONT		"*-Helvetica-Bold-R-Normal-*-120-*"
#define DEF_GRAPH_HALO		"0.5i"
#define DEF_GRAPH_HALO_BAR	"0.1i"
#define DEF_GRAPH_HEIGHT	"400"
#define DEF_GRAPH_INVERT_XY	"0"
#define DEF_GRAPH_MARGIN	"0"
#define DEF_GRAPH_PLOT_BG_COLOR	ANTIQUEWHITE1
#define DEF_GRAPH_PLOT_BG_MONO	WHITE
#define DEF_GRAPH_PLOT_BW_COLOR "2"
#define DEF_GRAPH_PLOT_BW_MONO  "0"
#define DEF_GRAPH_PLOT_RELIEF	"sunken"
#define DEF_GRAPH_RELIEF	"flat"
#define DEF_GRAPH_TITLE		(char *)NULL
#define DEF_GRAPH_WIDTH		"400"

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_GRAPH_BG_COLOR, Tk_Offset(Graph, border),
	TK_CONFIG_COLOR_ONLY | ALL_MASK},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_GRAPH_BG_MONO, Tk_Offset(Graph, border),
	TK_CONFIG_MONO_ONLY | ALL_MASK},
    {TK_CONFIG_DOUBLE, "-barwidth", "barWidth", "BarWidth",
	DEF_GRAPH_BAR_WIDTH, Tk_Offset(Graph, barWidth), BARCHART_MASK},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL, (char *)NULL, 0,
	ALL_MASK},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0,
	ALL_MASK},
    {TK_CONFIG_PIXELS, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_GRAPH_BORDER_WIDTH, Tk_Offset(Graph, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_PIXELS, "-bottommargin", "bottomMargin", "Margin",
	DEF_GRAPH_MARGIN, Tk_Offset(Graph, bottomMargin), ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-bufferelements", "bufferElements", "BufferElements",
	DEF_GRAPH_BUFFERED, Tk_Offset(Graph, buffered),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
	DEF_GRAPH_CURSOR, Tk_Offset(Graph, cursor),
	ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_GRAPH_FONT, Tk_Offset(Graph, fontPtr), ALL_MASK},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL, (char *)NULL, 0,
	ALL_MASK},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_GRAPH_FG_COLOR, Tk_Offset(Graph, marginFg),
	TK_CONFIG_COLOR_ONLY | ALL_MASK},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_GRAPH_FG_MONO, Tk_Offset(Graph, marginFg),
	TK_CONFIG_MONO_ONLY | ALL_MASK},
    {TK_CONFIG_PIXELS, "-halo", "halo", "Halo",
	DEF_GRAPH_HALO_BAR, Tk_Offset(Graph, halo), BARCHART_MASK},
    {TK_CONFIG_PIXELS, "-halo", "halo", "Halo",
	DEF_GRAPH_HALO, Tk_Offset(Graph, halo), XYGRAPH_MASK},
    {TK_CONFIG_PIXELS, "-height", "height", "Height",
	DEF_GRAPH_HEIGHT, Tk_Offset(Graph, reqHeight),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-invertxy", "invertXY", "InvertXY",
	DEF_GRAPH_INVERT_XY, Tk_Offset(Graph, inverted),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_PIXELS, "-leftmargin", "leftMargin", "Margin",
	DEF_GRAPH_MARGIN, Tk_Offset(Graph, leftMargin),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_COLOR, "-plotbackground", "plotBackground", "Background",
	DEF_GRAPH_PLOT_BG_MONO, Tk_Offset(Graph, plotBg),
	TK_CONFIG_MONO_ONLY | ALL_MASK},
    {TK_CONFIG_COLOR, "-plotbackground", "plotBackground", "Background",
	DEF_GRAPH_PLOT_BG_COLOR, Tk_Offset(Graph, plotBg),
	TK_CONFIG_COLOR_ONLY | ALL_MASK},
    {TK_CONFIG_INT, "-plotborderwidth", "plotBorderWidth", "BorderWidth",
	DEF_GRAPH_PLOT_BW_COLOR, Tk_Offset(Graph, plotBW),
	ALL_MASK | TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_INT, "-plotborderwidth", "plotBorderWidth", "BorderWidth",
	DEF_GRAPH_PLOT_BW_MONO, Tk_Offset(Graph, plotBW),
	ALL_MASK | TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_RELIEF, "-plotrelief", "plotRelief", "Relief",
	DEF_GRAPH_PLOT_RELIEF, Tk_Offset(Graph, plotRelief),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_GRAPH_RELIEF, Tk_Offset(Graph, relief),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_PIXELS, "-rightmargin", "rightMargin", "Margin",
	DEF_GRAPH_MARGIN, Tk_Offset(Graph, rightMargin),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_STRING, "-title", "title", "Title",
	DEF_GRAPH_TITLE, Tk_Offset(Graph, title),
	ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-topmargin", "topMargin", "Margin",
	DEF_GRAPH_MARGIN, Tk_Offset(Graph, topMargin),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_PIXELS, "-width", "width", "Width",
	DEF_GRAPH_WIDTH, Tk_Offset(Graph, reqWidth),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

double Blt_negInfinity, Blt_posInfinity;

extern int Blt_CreateAxis _ANSI_ARGS_((Graph *graphPtr, AxisType type, int));
extern int Blt_AxisCmd _ANSI_ARGS_((Graph *graphPtr, GraphAxis *axisPtr,
	int argc, char **argv, int flags));
extern void Blt_ComputeAxes _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_UpdateAxisBackgrounds _ANSI_ARGS_((Graph *graphPtr,
	XColor *bgColorPtr));
extern int Blt_ComputeLayout _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_LegendCmd _ANSI_ARGS_((Graph *graphPtr, int argc, char **argv));
extern int Blt_CreateLegend _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_CreatePostScript _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_CreateCrosshairs _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_CrosshairsCmd _ANSI_ARGS_((Graph *graphPtr, int argc,
	char **argv));
static void DisplayGraph _ANSI_ARGS_((ClientData clientData));
static void DestroyGraph _ANSI_ARGS_((ClientData clientData));
static int GraphWidgetCmd _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, int argc, char **argv));
extern int Blt_TagCmd _ANSI_ARGS_((Graph *graphPtr, int argc, char **argv));
extern int Blt_ElementCmd _ANSI_ARGS_((Graph *graphPtr, int argc,
	char **argv));

/*
 *--------------------------------------------------------------
 *
 * Blt_RedrawGraph --
 *
 *	Tell the Tk dispatcher to call the graph display routine at
 *	the next idle point.  This request is made only if the window
 *	is mapped and no other redraw request is pending.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The window is eventually redisplayed.
 *
 *--------------------------------------------------------------
 */
void
Blt_RedrawGraph(graphPtr)
    Graph *graphPtr;		/* Graph widget record */
{
    if (Tk_IsMapped(graphPtr->tkwin) && !(graphPtr->flags & REDRAW_PENDING)) {
	Tk_DoWhenIdle(DisplayGraph, (ClientData)graphPtr);
	graphPtr->flags |= REDRAW_PENDING;
    }
}

/*
 *--------------------------------------------------------------
 *
 * GraphEventProc --
 *
 *	This procedure is invoked by the Tk dispatcher for various
 *	events on graphs.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get cleaned
 *	up.  When it gets exposed, the graph is eventually redisplayed.
 *
 *--------------------------------------------------------------
 */
static void
GraphEventProc(clientData, eventPtr)
    ClientData clientData;	/* Graph widget record */
    register XEvent *eventPtr;	/* Event which triggered call to routine */
{
    register Graph *graphPtr = (Graph *)clientData;

    if (eventPtr->type == Expose) {
	if (eventPtr->xexpose.count == 0) {
	    graphPtr->flags |= REFRESH;
	    Blt_RedrawGraph(graphPtr);
	}
    } else if (eventPtr->type == DestroyNotify) {
	Tcl_DeleteCommand(graphPtr->interp, Tk_PathName(graphPtr->tkwin));
	graphPtr->tkwin = NULL;
	if (graphPtr->flags & REDRAW_PENDING) {
	    Tk_CancelIdleCall(DisplayGraph, (ClientData)graphPtr);
	}
	Tk_EventuallyFree((ClientData)graphPtr, DestroyGraph);
    } else if (eventPtr->type == ConfigureNotify) {
	graphPtr->flags |= (LAYOUT_ALL | REFRESH);
	Blt_RedrawGraph(graphPtr);
    }
}

/*
 *--------------------------------------------------------------
 *
 * GraphCoords --
 *
 *	This procedure returns a list of the graph coordinate
 *	values corresponding with the given window X and Y
 *	coordinate positions.
 *
 * Results:
 *	Returns a standard Tcl result.  The interp->result field is
 *	a Tcl list of the corresponding graph X and Y coordinates.
 *	If an error occurred while parsing the window positions,
 *	TCL_ERROR is returned, and interp->result will contain
 *	the error message.
 *
 *--------------------------------------------------------------
 */
static int
GraphCoords(graphPtr, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    int argc;
    char **argv;
{
    int winX, winY;		/* Integer window coordinates representation */
    char string[TCL_DOUBLE_SPACE + 1];
    double x, y;

    if (argc != 4) {
	Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
	    argv[0], " invtransform winX winY\"", NULL);
	return TCL_ERROR;
    }
    if (Tcl_GetInt(graphPtr->interp, argv[2], &winX) != TCL_OK ||
	Tcl_GetInt(graphPtr->interp, argv[3], &winY) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Perform the reverse transformation from window coordinates to
     * data coordinates
     */
    x = Blt_InvTransform(graphPtr->axisArr[X1_AXIS], winX);
    y = Blt_InvTransform(graphPtr->axisArr[Y1_AXIS], winY);

    if (graphPtr->inverted) {
	Tcl_PrintDouble(graphPtr->interp, y, string);
	Tcl_AppendElement(graphPtr->interp, string);
	Tcl_PrintDouble(graphPtr->interp, x, string);
	Tcl_AppendElement(graphPtr->interp, string);
    } else {
	Tcl_PrintDouble(graphPtr->interp, x, string);
	Tcl_AppendElement(graphPtr->interp, string);
	Tcl_PrintDouble(graphPtr->interp, y, string);
	Tcl_AppendElement(graphPtr->interp, string);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * WindowCoords --
 *
 *	This procedure returns a list of the window coordinates
 *	corresponding with the given graph x and y coordinates.
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the list of the graph coordinates. If an error occurred
 *	while parsing the window positions, TCL_ERROR is returned,
 *	then interp->result will contain an error message.
 *
 *--------------------------------------------------------------
 */
static int
WindowCoords(graphPtr, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    int argc;
    char **argv;
{
    double x, y;
    char string[TCL_DOUBLE_SPACE + 1];

    if (argc != 4) {
	Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
	    argv[0], " transform x y\"", NULL);
	return TCL_ERROR;
    }
    if (Tcl_ExprDouble(graphPtr->interp, argv[2], &x) != TCL_OK ||
	Tcl_ExprDouble(graphPtr->interp, argv[3], &y) != TCL_OK) {
	return TCL_ERROR;
    }
    /* Perform the transformation from window coordinates to data */
    if (graphPtr->inverted) {
	sprintf(string, "%d", Blt_Transform(graphPtr->axisArr[Y1_AXIS], y));
	Tcl_AppendElement(graphPtr->interp, string);
	sprintf(string, "%d", Blt_Transform(graphPtr->axisArr[X1_AXIS], x));
	Tcl_AppendElement(graphPtr->interp, string);
    } else {
	sprintf(string, "%d", Blt_Transform(graphPtr->axisArr[X1_AXIS], x));
	Tcl_AppendElement(graphPtr->interp, string);
	sprintf(string, "%d", Blt_Transform(graphPtr->axisArr[Y1_AXIS], y));
	Tcl_AppendElement(graphPtr->interp, string);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * AdjustAxisPointers --
 *
 *	Sets the axis pointers according to whether the axis is
 *	inverted on not.  The axis locations are also reset.
 *
 * Results:
 *	None.
 *
 *--------------------------------------------------------------
 */
static void
AdjustAxisPointers(graphPtr)
    Graph *graphPtr;		/* Graph widget record */
{
    if (graphPtr->inverted) {
	graphPtr->bottomAxis = graphPtr->axisArr[Y1_AXIS];
	graphPtr->leftAxis = graphPtr->axisArr[X1_AXIS];
	graphPtr->topAxis = graphPtr->axisArr[Y2_AXIS];
	graphPtr->rightAxis = graphPtr->axisArr[X2_AXIS];
    } else {
	graphPtr->bottomAxis = graphPtr->axisArr[X1_AXIS];
	graphPtr->leftAxis = graphPtr->axisArr[Y1_AXIS];
	graphPtr->topAxis = graphPtr->axisArr[X2_AXIS];
	graphPtr->rightAxis = graphPtr->axisArr[Y2_AXIS];
    }
    graphPtr->bottomAxis->location = BOTTOM_AXIS;
    graphPtr->leftAxis->location = LEFT_AXIS;
    graphPtr->topAxis->location = TOP_AXIS;
    graphPtr->rightAxis->location = RIGHT_AXIS;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyGraph --
 *
 *	This procedure is invoked by Tk_EventuallyFree or Tk_Release
 *	to clean up the internal structure of a graph at a safe time
 *	(when no-one is using it anymore).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the widget is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyGraph(clientData)
    ClientData clientData;
{
    register Graph *graphPtr = (Graph *)clientData;
    Tcl_HashEntry *entryPtr;
    Tcl_HashSearch cursor;
    Element *elemPtr;
    Tag *tagPtr;
    register int i;
    GraphAxis *axisPtr;

    /*
     * Destroy the individual components of the graph: elements, tags,
     * X and Y axes, legend, display lists etc.
     */
    for (entryPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&cursor)) {
	elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
	(*elemPtr->destroyProc) (graphPtr, elemPtr);
    }
    Tcl_DeleteHashTable(&(graphPtr->elemTable));
    Blt_ClearList(&(graphPtr->elemList));
    for (entryPtr = Tcl_FirstHashEntry(&(graphPtr->tagTable), &cursor);
	entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&cursor)) {
	tagPtr = (Tag *)Tcl_GetHashValue(entryPtr);
	(*tagPtr->destroyProc) (graphPtr, tagPtr);
    }
    Tcl_DeleteHashTable(&(graphPtr->tagTable));
    Blt_ClearList(&(graphPtr->tagList));

    for (i = 0; i < 4; i++) {
	axisPtr = graphPtr->axisArr[i];
	(*axisPtr->destroyProc) (graphPtr, axisPtr);
    }
    (*graphPtr->legendPtr->destroyProc) (graphPtr);
    (*graphPtr->postscript->destroyProc) (graphPtr);
    (*graphPtr->crosshairs->destroyProc) (graphPtr);

    /* Release allocated X resources and memory. */
    if (graphPtr->marginGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->marginGC);
    }
    if (graphPtr->marginFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->marginFillGC);
    }
    if (graphPtr->elemMap != None) {
	XFreePixmap(graphPtr->display, graphPtr->elemMap);
    }
    Tk_FreeOptions(configSpecs, (char *)graphPtr, graphPtr->display,
	configFlags[graphPtr->type]);
    free((char *)graphPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateGraph --
 *
 *	This procedure creates and initializes a new widget.
 *
 * Results:
 *	The return value is a pointer to a structure describing
 *	the new widget.  If an error occurred, then the return
 *	value is NULL and an error message is left in interp->result.
 *
 * Side effects:
 *	Memory is allocated, a Tk_Window is created, etc.
 *
 *----------------------------------------------------------------------
 */

static Graph *
CreateGraph(interp, parent, pathName, type)
    Tcl_Interp *interp;
    Tk_Window parent;
    char *pathName;
    GraphClassType type;
{
    Graph *graphPtr;
    Tk_Window tkwin;
    unsigned int flags;
    register int i;

    tkwin = Tk_CreateWindowFromPath(interp, parent, pathName, (char *)NULL);
    if (tkwin == (Tk_Window)NULL) {
	return (Graph *) NULL;
    }
    graphPtr = (Graph *)calloc(1, sizeof(Graph));
    if (graphPtr == (Graph *)NULL) {
	interp->result = "can't allocate graph structure";
	return (Graph *) NULL;
    }
    Tk_SetClass(tkwin, classNames[type]);

    /* Initialize the data structure for the graph. */

    graphPtr->tkwin = tkwin;
    graphPtr->pathName = Tk_PathName(tkwin);
    graphPtr->display = Tk_Display(tkwin);
    graphPtr->interp = interp;
    graphPtr->type = type;
    graphPtr->width = graphPtr->height = 400;
    graphPtr->reqWidth = graphPtr->reqHeight = 400;
    graphPtr->buffered = 1;
    graphPtr->plotRelief = TK_RELIEF_SUNKEN;
    graphPtr->relief = TK_RELIEF_FLAT;
    graphPtr->flags |= (DIRTY | LAYOUT_ALL);

    Tcl_InitHashTable(&(graphPtr->elemTable), TCL_STRING_KEYS);
    Blt_InitLinkedList(&(graphPtr->elemList), TCL_STRING_KEYS);
    Tcl_InitHashTable(&(graphPtr->tagTable), TCL_STRING_KEYS);
    Blt_InitLinkedList(&(graphPtr->tagList), TCL_STRING_KEYS);

    graphPtr->nextTagId = 1;
    /*
     * Axes and tags (text, bitmap) need private GCs, so create the
     * window now.
     */
    Tk_MakeWindowExist(tkwin);

    flags = configFlags[type];
    for (i = 0; i < 4; i++) {
	if (Blt_CreateAxis(graphPtr, (AxisType)i, flags) != TCL_OK) {
	    goto error;
	}
    }
    AdjustAxisPointers(graphPtr);

    if (Blt_CreatePostScript(graphPtr) != TCL_OK) {
	goto error;
    }
    if (Blt_CreateCrosshairs(graphPtr) != TCL_OK) {
	goto error;
    }
    if (Blt_CreateLegend(graphPtr) != TCL_OK) {
	goto error;
    }
    /* Compute the initial axis ticks and labels */

    Blt_ComputeAxes(graphPtr);
    Tk_CreateEventHandler(graphPtr->tkwin, ExposureMask | StructureNotifyMask,
	GraphEventProc, (ClientData)graphPtr);
    Tcl_CreateCommand(interp, Tk_PathName(graphPtr->tkwin), GraphWidgetCmd,
	(ClientData)graphPtr, (Tcl_CmdDeleteProc *)NULL);
    return (graphPtr);

  error:
    if (tkwin != (Tk_Window)NULL) {
	Tk_DestroyWindow(tkwin);
    }
    return (Graph *) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureGraph --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or
 *	reconfigure) a graph widget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for graphPtr;  old resources get freed, if there
 *	were any.  The graph is redisplayed.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureGraph(graphPtr, argc, argv, flags)
    register Graph *graphPtr;	/* Graph widget record */
    int argc;			/* Number of configuration arguments */
    char **argv;		/* Configuration arguments */
    unsigned int flags;		/* Configuration flags */
{
    XColor *colorPtr;
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;

    if (Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin, configSpecs,
	    argc, argv, (char *)graphPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((graphPtr->reqWidth < 1) || (graphPtr->reqHeight < 1)) {
	Tcl_AppendResult(graphPtr->interp,
	    "impossible width/height specifications for \"",
	    Tk_PathName(graphPtr->tkwin), "\"", (char *)NULL);
	return TCL_ERROR;
    }
    Tk_GeometryRequest(graphPtr->tkwin, graphPtr->reqWidth,
	graphPtr->reqHeight);
    Tk_SetInternalBorder(graphPtr->tkwin, graphPtr->borderWidth);
    Tk_SetBackgroundFromBorder(graphPtr->tkwin, graphPtr->border);

    colorPtr = Tk_3DBorderColor(graphPtr->border);

    /* Update background color for axis text GCs */
    Blt_UpdateAxisBackgrounds(graphPtr, colorPtr);

    /*
     * Create GCs for interior and exterior regions, and a background
     * GC for clearing the margins with XFillRectangle
     */
    gcValues.foreground = graphPtr->marginFg->pixel;
    gcValues.background = colorPtr->pixel;
    gcValues.font = graphPtr->fontPtr->fid;
    gcMask = (GCForeground | GCBackground | GCFont);
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (graphPtr->marginGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->marginGC);
    }
    graphPtr->marginGC = newGC;
    gcValues.foreground = colorPtr->pixel;
    gcValues.background = graphPtr->marginFg->pixel;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (graphPtr->marginFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->marginFillGC);
    }
    graphPtr->marginFillGC = newGC;
    gcValues.foreground = graphPtr->plotBg->pixel;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (graphPtr->plotFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->plotFillGC);
    }
    graphPtr->plotFillGC = newGC;
    /*
     * If the -inverted option changed, we need to readjust the pointers
     * to the axes and recompute the their scales.
     */
    if (Blt_OptionChanged(configSpecs, "-invertxy", (char *)NULL)) {
	AdjustAxisPointers(graphPtr);
	Blt_ComputeAxes(graphPtr);
    }
    /*
     * Free the pixmap if we're not buffering the display of elements
     * anymore.
     */
    if ((!graphPtr->buffered) && (graphPtr->elemMap != None)) {
	XFreePixmap(graphPtr->display, graphPtr->elemMap);
	graphPtr->elemMap = None;
    }
    /*
     * Reconfigure the crosshairs in case the plotting area's
     * background color changed
     */
    (*graphPtr->crosshairs->configProc) (graphPtr);

    /*
     *  Update the layout of the graph (and redraw the elements) if
     *  any of the following graph options which affect the size of
     *	the plotting area has changed.
     *
     *      -borderwidth, -plotborderwidth
     *	    -font, -title
     *	    -width, -height
     *	    -invertxy
     *	    -bottommargin, -leftmargin, -rightmargin, -topmargin
     *
     */
    if (Blt_OptionChanged(configSpecs, "-invertxy", "-title", "-font",
	    "-*margin", "-*width", "-height", (char *)NULL)) {
	graphPtr->flags |= (DIRTY | LAYOUT_ALL);
    }
    graphPtr->flags |= REFRESH;
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * GraphCmd --
 *
 *	Creates a new window and Tcl command representing an
 *	instance of a graph widget.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
static int
GraphCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Tk_Window tkwin = (Tk_Window)clientData;
    register Graph *graphPtr;
    GraphClassType type;
    unsigned int flags;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"", argv[0],
	    " pathName ?options?\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (strcmp(argv[0], "blt_graph") == 0) {
	type = GRAPH;
    } else if (strcmp(argv[0], "blt_barchart") == 0) {
	type = BARCHART;
    } else if (strcmp(argv[0], "blt_contour") == 0) {
	type = CONTOUR;
    } else {
	Tcl_AppendResult(interp, "unknown graph-creation command \"", argv[0],
	    "\"", (char *)NULL);
	return TCL_ERROR;
    }
    Blt_negInfinity = (double)MAX_NEG_VAL;
    Blt_posInfinity = (double)MAX_POS_VAL;
    graphPtr = CreateGraph(interp, tkwin, argv[1], type);
    if (graphPtr == NULL) {
	return TCL_ERROR;
    }
    flags = configFlags[type];
    if (ConfigureGraph(graphPtr, argc - 2, argv + 2, flags) != TCL_OK) {
	Tk_DestroyWindow(graphPtr->tkwin);
	return TCL_ERROR;
    }
    interp->result = graphPtr->pathName;
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * GraphWidgetCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
static int
GraphWidgetCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register Graph *graphPtr = (Graph *)clientData;
    int result = TCL_ERROR;
    Tk_Window tkwin = graphPtr->tkwin;
    char c;
    unsigned int length;
    unsigned int flags;
    GraphClassType type;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " option ?arg arg ...?\"", (char *)NULL);
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)graphPtr);

    c = argv[1][0];
    length = strlen(argv[1]);
    type = graphPtr->type;
    flags = (configFlags[type] | TK_CONFIG_ARGV_ONLY);

    if ((c == 'c') && (length > 2) &&
	(strncmp(argv[1], "configure", length) == 0)) {
	if (argc == 2)
	    result = Tk_ConfigureInfo(interp, tkwin, configSpecs,
		(char *)graphPtr, (char *)NULL, flags);
	else if (argc == 3)
	    result = Tk_ConfigureInfo(interp, tkwin, configSpecs,
		(char *)graphPtr, argv[2], flags);
	else {
	    result = ConfigureGraph(graphPtr, argc - 2, argv + 2, flags);
	}
    } else if ((c == 'c') && (length > 1) &&
	(strncmp(argv[1], "crosshairs", length) == 0)) {
	result = Blt_CrosshairsCmd(graphPtr, argc, argv);
    } else if ((c == 'e') && (strncmp(argv[1], "element", length) == 0)) {
	result = Blt_ElementCmd(graphPtr, argc, argv);
    } else if ((c == 't') && (strncmp(argv[1], "tag", length) == 0)) {
	result = Blt_TagCmd(graphPtr, argc, argv);
    } else if ((c == 'x') && (length > 1) &&
	(strncmp(argv[1], "xaxis", length) == 0)) {
	result = Blt_AxisCmd(graphPtr, graphPtr->axisArr[X1_AXIS], argc, argv,
	    flags);
    } else if ((c == 'x') && (length > 1) &&
	(strncmp(argv[1], "x2axis", length) == 0)) {
	result = Blt_AxisCmd(graphPtr, graphPtr->axisArr[X2_AXIS], argc, argv,
	    flags);
    } else if ((c == 'y') && (length > 1) &&
	(strncmp(argv[1], "yaxis", length) == 0)) {
	result = Blt_AxisCmd(graphPtr, graphPtr->axisArr[Y1_AXIS], argc, argv,
	    flags);
    } else if ((c == 'y') && (length > 1) &&
	(strncmp(argv[1], "y2axis", length) == 0)) {
	result = Blt_AxisCmd(graphPtr, graphPtr->axisArr[Y2_AXIS], argc, argv,
	    flags);
    } else if ((c == 'l') && (length > 1) &&
	(strncmp(argv[1], "legend", length) == 0)) {
	result = Blt_LegendCmd(graphPtr, argc, argv);
    } else if ((c == 'l') && (length > 1) &&
	(strncmp(argv[1], "locate", length) == 0)) {
	/* Obsolete syntax "locate" */
	result = GraphCoords(graphPtr, argc, argv);
    } else if ((c == 'i') && (length > 1) &&
	(strncmp(argv[1], "invtransform", length) == 0)) {
	result = GraphCoords(graphPtr, argc, argv);
    } else if ((c == 't') && (length > 1) &&
	(strncmp(argv[1], "transform", length) == 0)) {
	result = WindowCoords(graphPtr, argc, argv);
    } else if ((c == 'p') && (strncmp(argv[1], "postscript", length) == 0)) {
	result = (*graphPtr->postscript->printProc) (graphPtr, argc, argv);
    } else if ((c == 'p') && (length > 1) &&
	(strncmp(argv[1], "psconfigure", length) == 0)) {
	result = (*graphPtr->postscript->configProc) (graphPtr, argc, argv);
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be\
 configure, crosshairs, element, invtransform, legend, postscript,\
 psconfigure, tag, transform, xaxis, yaxis, or y2axis", (char *)NULL);
    }
    Tk_Release((ClientData)graphPtr);
    return result;
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_LayoutGraph --
 *
 *	Based upon the computes sizing of the plotting area, calls
 *	routines to calculate the positions for the graph components.
 *	This routine is called when the layout changes.  It is called
 *	from DisplayGraph and PrintPostScript.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Old storage is freed and new memory is allocated for holding
 *	component positions.  These positions will eventually affect
 *	where the components are drawn in the graph window.
 *
 * -----------------------------------------------------------------
 */
void
Blt_LayoutGraph(graphPtr)
    Graph *graphPtr;
{
    Blt_ListEntry *entryPtr;
    Element *elemPtr;
    Tag *tagPtr;

    /* Layout axes */
    if (graphPtr->flags & LAYOUT_ALL) {
	GraphAxis *axisPtr;
	register int i;

	for (i = 0; i < 4; i++) {
	    axisPtr = graphPtr->axisArr[i];
	    (*axisPtr->layoutProc) (graphPtr, axisPtr);
	}
    }
    /* Layout elements */
    for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	elemPtr = (Element *)Blt_GetListValue(entryPtr);
	if ((graphPtr->flags & LAYOUT_DIRTY) ||
	    (elemPtr->flags & LAYOUT_NEEDED)) {
	    (*elemPtr->layoutProc) (graphPtr, elemPtr);
	    elemPtr->flags &= ~LAYOUT_NEEDED;
	}
    }

    /* Layout tags */
    for (entryPtr = Blt_FirstListEntry(&(graphPtr->tagList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	tagPtr = (Tag *)Blt_GetListValue(entryPtr);
	if ((graphPtr->flags & LAYOUT_DIRTY) ||
	    (tagPtr->flags & LAYOUT_NEEDED)) {
	    (*tagPtr->layoutProc) (graphPtr, tagPtr);
	    tagPtr->flags &= ~LAYOUT_NEEDED;
	}
    }
    graphPtr->flags &= ~LAYOUT_ALL;
}

/*
 * -----------------------------------------------------------------
 *
 * DisplayElements --
 *
 *	Calls the individual element drawing routines for each
 *	element.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Elements are drawn into the drawable (pixmap) which will
 *	eventually be displayed in the graph window.
 *
 * -----------------------------------------------------------------
 */
static void
DisplayElements(graphPtr)
    Graph *graphPtr;
{
    Blt_ListEntry *entryPtr;
    register Element *elemPtr;

    for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	elemPtr = (Element *)Blt_GetListValue(entryPtr);
	(*elemPtr->displayProc) (graphPtr, elemPtr, ELEM_NORMAL);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * DisplayActiveElements --
 *
 *	Calls the individual element drawing routines to display
 *	the active colors for each element.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Elements are drawn into the drawable (pixmap) which will
 *	eventually be displayed in the graph window.
 *
 * -----------------------------------------------------------------
 */
static void
DisplayActiveElements(graphPtr)
    Graph *graphPtr;
{
    Blt_ListEntry *entryPtr;
    register Element *elemPtr;

    for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	elemPtr = (Element *)Blt_GetListValue(entryPtr);
	if (elemPtr->flags & ACTIVE) {
	    (*elemPtr->displayProc) (graphPtr, elemPtr, ELEM_ACTIVE);
	}
    }
}

/*
 * -----------------------------------------------------------------
 *
 * DisplayTags --
 *
 *	Calls the individual tag drawing routines for each tag.
 *	A tag will not be drawn if there is an element associated
 *	with the tag (elemId is non-NULL) and that element is
 *	not in the element display list.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Tags are drawn into the drawable (pixmap) which will eventually
 *	be displayed in the graph window.
 *
 * -----------------------------------------------------------------
 */
static void
DisplayTags(graphPtr)
    Graph *graphPtr;
{
    Blt_ListEntry *listPtr;
    Tag *tagPtr;
    int mapped;

    for (listPtr = Blt_FirstListEntry(&(graphPtr->tagList)); listPtr != NULL;
	listPtr = Blt_NextListEntry(listPtr)) {
	tagPtr = (Tag *)Blt_GetListValue(listPtr);
	mapped = 1;

	if (tagPtr->elemId != NULL) {
	    Tcl_HashEntry *entryPtr;
	    Element *elemPtr;

	    entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable),
		(char *)tagPtr->elemId);
	    if (entryPtr == NULL) {
		continue;
	    }
	    elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
	    mapped = elemPtr->mapped;
	}
	if (mapped) {
	    (*tagPtr->displayProc) (graphPtr, tagPtr);
	}
    }
}

/*
 * -----------------------------------------------------------------
 *
 * DisplayExterior --
 *
 * 	Draws the exterior region of the graph (axes, ticks, titles, etc)
 *	onto a pixmap. The interior region is defined by the given
 *	rectangle structure.
 *
 *		X coordinate axis
 *		Y coordinate axis
 *		legend
 *		interior border
 *		exterior border
 *		titles (X and Y axis, graph)
 *
 * Returns:
 *	None.
 *
 * Side Effects:
 *	Exterior of graph is displayed in its window.
 *
 * -----------------------------------------------------------------
 */
static void
DisplayExterior(graphPtr, rectPtr)
    Graph *graphPtr;
    XRectangle *rectPtr;	/* Interior plotting region */
{
    int x, y;
    XRectangle rectArr[4];
    GraphAxis *axisPtr;
    TextAttributes textAttr;
    register int i;

    /*
     * Draw the four outer rectangles which encompass the plotting
     * surface. This clears the surrounding area and clips the plot.
     */

    rectArr[0].x = rectArr[0].y = rectArr[3].x = rectArr[1].x = 0;
    rectArr[0].width = rectArr[3].width = graphPtr->width;
    rectArr[0].height = rectPtr->y;
    rectArr[3].y = rectPtr->y + rectPtr->height;
    rectArr[3].height = graphPtr->height - rectArr[3].y;
    rectArr[2].y = rectArr[1].y = rectPtr->y;
    rectArr[1].width = rectPtr->x;
    rectArr[2].height = rectArr[1].height = rectPtr->height;
    rectArr[2].x = rectPtr->x + rectPtr->width;
    rectArr[2].width = graphPtr->width - rectArr[2].x;
    XFillRectangles(graphPtr->display, graphPtr->canvas,
	graphPtr->marginFillGC, rectArr, 4);

    /* Interior 3D border */

    if ((graphPtr->plotRelief != TK_RELIEF_FLAT) &&
	(graphPtr->plotBW > 0)) {
	Tk_Draw3DRectangle(graphPtr->display, graphPtr->canvas,
	    graphPtr->border, rectPtr->x, rectPtr->y, rectPtr->width,
	    rectPtr->height, graphPtr->plotBW, graphPtr->plotRelief);
    }
    /* Legend */
    if (graphPtr->legendPtr->useDefault) {
	(*graphPtr->legendPtr->displayProc) (graphPtr);
    }
    /* Initialize text attributes for graph and axis titles */

    textAttr.theta = 0.0;
    textAttr.anchor = TK_ANCHOR_CENTER;
    textAttr.gc = graphPtr->marginGC;
    textAttr.bgColorPtr = Tk_3DBorderColor(graphPtr->border);
    textAttr.fontPtr = graphPtr->fontPtr;

    if (graphPtr->title != NULL) {
	x = (graphPtr->extreme.x + graphPtr->origin.x) / 2;
	y = graphPtr->borderWidth + TEXTHEIGHT(graphPtr->fontPtr);
	Blt_DrawText(graphPtr->display, graphPtr->canvas, graphPtr->title,
	    &textAttr, x, y);
    }
    /* Draw axes */
    for (i = 0; i < 4; i++) {
	axisPtr = graphPtr->axisArr[i];
	if (axisPtr->mapped) {
	    (*axisPtr->displayProc) (graphPtr, axisPtr, &textAttr);
	}
    }
    /* Exterior 3D border */
    if ((graphPtr->relief != TK_RELIEF_FLAT) &&
	(graphPtr->borderWidth > 0)) {
	Tk_Draw3DRectangle(graphPtr->display, graphPtr->canvas,
	    graphPtr->border, 0, 0, graphPtr->width, graphPtr->height,
	    graphPtr->borderWidth, graphPtr->relief);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DisplayGraph --
 *
 *	This procedure is invoked to display a graph widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Commands are output to X to display the graph in its
 *	current mode.
 *
 *----------------------------------------------------------------------
 */
static void
DisplayGraph(clientData)
    ClientData clientData;
{
    register Graph *graphPtr = (Graph *)clientData;
    XRectangle plotArea;
    int twiceBW;
    GraphLegend *legendPtr;

    /*
     * If the window has been deleted or is no longer mapped, do
     * nothing
     */
    graphPtr->flags &= ~REDRAW_PENDING;
    if ((graphPtr->tkwin == NULL) || (!Tk_IsMapped(graphPtr->tkwin))) {
	return;
    }
#ifdef notdef
    fprintf(stderr, "Calling DisplayGraph\n");
#endif

    graphPtr->width = Tk_Width(graphPtr->tkwin);
    graphPtr->height = Tk_Height(graphPtr->tkwin);

    if (graphPtr->flags & LAYOUT_NEEDED) {
	if (graphPtr->flags & LAYOUT_DIRTY) {
	    if (Blt_ComputeLayout(graphPtr) != TCL_OK) {
		return;		/* Not enough room to plot graph */
	    }
	}
	Blt_LayoutGraph(graphPtr);
    }
    /*
     * Determine the plotting surface region of the graph window
     */
    twiceBW = (graphPtr->plotBW * 2);
    plotArea.x = graphPtr->origin.x - graphPtr->plotBW;
    plotArea.y = graphPtr->extreme.y - graphPtr->plotBW;
    plotArea.width = (graphPtr->extreme.x - graphPtr->origin.x) + twiceBW;
    plotArea.height = (graphPtr->origin.y - graphPtr->extreme.y) + twiceBW;

    /*
     * Create a pixmap (for double buffering) the size of the window
     */
    graphPtr->canvas = XCreatePixmap(graphPtr->display,
	Tk_WindowId(graphPtr->tkwin), graphPtr->width, graphPtr->height,
	Tk_Depth(graphPtr->tkwin));

    if (graphPtr->buffered) {
	if ((graphPtr->elemWidth != graphPtr->width) ||
	    (graphPtr->elemHeight != graphPtr->height) ||
	    (graphPtr->elemMap == None)) {
	    /*
	     * Create a pixmap to save elements if one doesn't already
	     * exist or the size of the window has changed.
	     */
	    graphPtr->elemWidth = graphPtr->width;
	    graphPtr->elemHeight = graphPtr->height;
	    if (graphPtr->elemMap != None) {
		XFreePixmap(graphPtr->display, graphPtr->elemMap);
	    }
	    graphPtr->elemMap = XCreatePixmap(graphPtr->display,
		Tk_WindowId(graphPtr->tkwin), graphPtr->elemWidth,
		graphPtr->elemHeight, Tk_Depth(graphPtr->tkwin));
	    graphPtr->flags |= DIRTY;
	}
	if (graphPtr->flags & DIRTY) {
	    Pixmap save;
	    /*
	     * Clear the background and draw the elements into the pixmap.
	     */
	    XFillRectangles(graphPtr->display, graphPtr->elemMap,
		graphPtr->plotFillGC, &plotArea, 1);
	    save = graphPtr->canvas;
	    graphPtr->canvas = graphPtr->elemMap;
	    DisplayElements(graphPtr);
	    graphPtr->canvas = save;
	    graphPtr->flags &= ~DIRTY;
	}
	XCopyArea(graphPtr->display, graphPtr->elemMap, graphPtr->canvas,
	    graphPtr->marginGC, plotArea.x, plotArea.y,
	    (unsigned int)plotArea.width, (unsigned int)plotArea.height,
	    plotArea.x, plotArea.y);
    } else {
	XFillRectangles(graphPtr->display, graphPtr->canvas,
	    graphPtr->plotFillGC, &plotArea, 1);
	DisplayElements(graphPtr);
    }
    DisplayTags(graphPtr);

    legendPtr = graphPtr->legendPtr;
    if (!legendPtr->useDefault) {
	(*legendPtr->displayProc) (graphPtr);
    }
    DisplayActiveElements(graphPtr);

    /* Disable crosshairs and update lengths before drawing to the display */
    (*graphPtr->crosshairs->toggleProc) (graphPtr);
    (*graphPtr->crosshairs->updateProc) (graphPtr);

    if (graphPtr->flags & REFRESH) {
	DisplayExterior(graphPtr, &plotArea);
	XCopyArea(graphPtr->display, graphPtr->canvas,
	    Tk_WindowId(graphPtr->tkwin), graphPtr->marginGC, 0, 0,
	    graphPtr->width, graphPtr->height, 0, 0);
    } else {
	/* Draw just the interior plotting region */

	plotArea.x += graphPtr->plotBW;
	plotArea.y += graphPtr->plotBW;
	plotArea.width -= (graphPtr->plotBW * 2);
	plotArea.height -= (graphPtr->plotBW * 2);
	XCopyArea(graphPtr->display, graphPtr->canvas,
	    Tk_WindowId(graphPtr->tkwin), graphPtr->marginGC, plotArea.x,
	    plotArea.y, (unsigned int)plotArea.width,
	    (unsigned int)plotArea.height, plotArea.x, plotArea.y);
    }
    (*graphPtr->crosshairs->toggleProc) (graphPtr);

    XFreePixmap(graphPtr->display, graphPtr->canvas);
    graphPtr->canvas = None;
    graphPtr->flags &= ~REFRESH;
}

int
Blt_GraphInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, "blt_graph", (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_graph\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_graph\" requires Tk", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", "blt_graph", GRAPH_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, "blt_graph", GraphCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}

int
Blt_BarchartInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, "blt_barchart", (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_barchart\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_barchart\" requires Tk",
	    (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", "blt_barchart", GRAPH_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, "blt_barchart", GraphCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}
