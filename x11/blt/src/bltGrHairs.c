
/*
 * bltGrHairs.c --
 *
 *	This module implements a crosshairs for the graph widget.
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

#include "blt.h"
#include "bltGraph.h"


extern Tk_CustomOption bltPositionOption;

/*
 * -------------------------------------------------------------------
 *
 * Crosshairs
 *
 *	Contains the line segments positions and graphics context used
 *	to simulate crosshairs (by XORing) on the graph.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    CrosshairsToggleProc *toggleProc;
    CrosshairsUpdateProc *updateProc;
    CrosshairsConfigProc *configProc;
    CrosshairsDestroyProc *destroyProc;

    XPoint anchorPos;		/* Hot spot for crosshairs */
    int state;			/* Internal state of crosshairs. If non-zero,
				 * crosshairs are displayed. */
    int mapped;			/* Requested state of crosshairs (on/off). This
				 * is not necessarily consistent with the
				 * internal state variable.  This is true when
				 * the hot spot is off the graph.  */
    int dashes;			/* Dashstyle of the crosshairs. This represents
				 * the number of alternatingly drawn pixels. If
				 * zero, the hairs are drawn as a solid line */
    int lineWidth;		/* Width of the simulated crosshair lines */
    XSegment segArr[2];		/* Positions of line segments representing the
				 * simulated crosshairs. */
    XColor *colorPtr;		/* Foreground color of crosshairs */
    GC gc;			/* Graphics context for crosshairs. Set to
				 * GXxor to not require redraws of graph */
} Crosshairs;

#define DEF_HAIRS_DASHES	"0"
#define DEF_HAIRS_FG_COLOR	BLACK
#define DEF_HAIRS_FG_MONO	BLACK
#define DEF_HAIRS_LINE_WIDTH	"0"
#define DEF_HAIRS_MAPPED	"0"
#define DEF_HAIRS_POSITION	(char *)NULL

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_HAIRS_FG_COLOR, Tk_Offset(Crosshairs, colorPtr),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_INT, "-dashes", "xhairsDashes", "Dashes",
	DEF_HAIRS_DASHES, Tk_Offset(Crosshairs, dashes), 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_HAIRS_FG_MONO, Tk_Offset(Crosshairs, colorPtr),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_INT, "-linewidth", "xhairsLineWidth", "XhairsLinewidth",
	DEF_HAIRS_LINE_WIDTH, Tk_Offset(Crosshairs, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-mapped", "xhairsMapped", "XhairsMapped",
	DEF_HAIRS_MAPPED, Tk_Offset(Crosshairs, mapped),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-position", "xhairsPosition", "XhairsPosition",
	DEF_HAIRS_POSITION, Tk_Offset(Crosshairs, anchorPos),
	0, &bltPositionOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

extern int Blt_PointOnGraph _ANSI_ARGS_((Graph *, XPoint *));

/*
 *----------------------------------------------------------------------
 *
 * TurnOffHairs --
 *
 *	XOR's the existing line segments (representing the crosshairs),
 *	thereby erasing them.  The internal state of the crosshairs is
 *	tracked.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Crosshairs are erased.
 *
 *----------------------------------------------------------------------
 */
static void
TurnOffHairs(tkwin, hairsPtr)
    Tk_Window tkwin;
    Crosshairs *hairsPtr;
{
    if (Tk_IsMapped(tkwin) && (hairsPtr->state)) {
	XDrawSegments(Tk_Display(tkwin), Tk_WindowId(tkwin), hairsPtr->gc,
	    hairsPtr->segArr, 2);
	hairsPtr->state = 0;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TurnOnHairs --
 *
 *	Draws (by XORing) new line segments, creating the effect of
 *	crosshairs. The internal state of the crosshairs is tracked.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Crosshairs are displayed.
 *
 *----------------------------------------------------------------------
 */
static void
TurnOnHairs(tkwin, hairsPtr)
    Tk_Window tkwin;
    Crosshairs *hairsPtr;
{
    if (Tk_IsMapped(tkwin) && (!hairsPtr->state)) {
	XDrawSegments(Tk_Display(tkwin), Tk_WindowId(tkwin), hairsPtr->gc,
	    hairsPtr->segArr, 2);
	hairsPtr->state = 1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureCrosshairs --
 *
 *	Configures attributes of the crosshairs such as line width,
 *	dashes, and position.  The crosshairs are first turned off
 *	before any of the attributes changes.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Crosshair GC is allocated.
 *
 *----------------------------------------------------------------------
 */
static void
ConfigureCrosshairs(graphPtr)
    Graph *graphPtr;
{
    XGCValues gcValues;
    unsigned long gcMask;
    GC newGC;
    long colorValue;
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    /*
     * Turn off the crosshairs temporarily. This is in case the new
     * configuration changes the size, style, or position of the lines.
     */
    TurnOffHairs(graphPtr->tkwin, hairsPtr);

    gcValues.function = GXxor;

    /* The graph's color option may not have been set yet */
    if (graphPtr->plotBg == NULL) {
	colorValue = WhitePixelOfScreen(Tk_Screen(graphPtr->tkwin));
    } else {
	colorValue = graphPtr->plotBg->pixel;
    }
    gcValues.background = colorValue;
    gcValues.foreground = (colorValue ^ hairsPtr->colorPtr->pixel);

    if (hairsPtr->lineWidth < 0) {
	hairsPtr->lineWidth = 0;
    }
    gcValues.line_width = hairsPtr->lineWidth;
    gcMask = (GCForeground | GCBackground | GCFunction | GCLineWidth);
    if (hairsPtr->dashes > 0) {
	gcValues.dashes = hairsPtr->dashes;
	gcValues.dash_offset = 0;
	gcValues.line_style = LineOnOffDash;
	gcMask |= (GCDashOffset | GCDashList | GCLineStyle);
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (hairsPtr->gc != NULL) {
	Tk_FreeGC(graphPtr->display, hairsPtr->gc);
    }
    hairsPtr->gc = newGC;

    /*
     * Are the new coordinates on the graph?
     */
    if (!Blt_PointOnGraph(graphPtr, &(hairsPtr->anchorPos))) {
	return;			/* Coordinates are off the graph */
    }
    hairsPtr->segArr[0].x2 = hairsPtr->segArr[0].x1 = hairsPtr->anchorPos.x;
    hairsPtr->segArr[0].y1 = graphPtr->origin.y;
    hairsPtr->segArr[0].y2 = graphPtr->extreme.y;
    hairsPtr->segArr[1].y2 = hairsPtr->segArr[1].y1 = hairsPtr->anchorPos.y;
    hairsPtr->segArr[1].x1 = graphPtr->origin.x;
    hairsPtr->segArr[1].x2 = graphPtr->extreme.x;

    if (hairsPtr->mapped) {
	TurnOnHairs(graphPtr->tkwin, hairsPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ToggleCrosshairs --
 *
 *	Turn on or off crosshair simulation if it has been requested
 *	and the current crosshair position is in the plotting area.
 *	This routine is used to erase the crosshairs before the
 *	graph is redrawn (invalidating the XOR'ed old crosshairs).
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Crosshairs may be drawn or erased from the plotting area.
 *
 *----------------------------------------------------------------------
 */
static void
ToggleCrosshairs(graphPtr)
    Graph *graphPtr;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    if ((hairsPtr->mapped) && (hairsPtr->state)) {
	XDrawSegments(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	    hairsPtr->gc, hairsPtr->segArr, 2);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * UpdateCrosshairs --
 *
 *	Update the length of the hairs (not the hot spot).
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
UpdateCrosshairs(graphPtr)
    Graph *graphPtr;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    hairsPtr->segArr[0].y1 = graphPtr->origin.y;
    hairsPtr->segArr[0].y2 = graphPtr->extreme.y;
    hairsPtr->segArr[1].x1 = graphPtr->origin.x;
    hairsPtr->segArr[1].x2 = graphPtr->extreme.x;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyCrosshairs --
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Crosshair GC is allocated.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyCrosshairs(graphPtr)
    Graph *graphPtr;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    Tk_FreeOptions(configSpecs, (char *)hairsPtr, graphPtr->display, 0);
    if (hairsPtr->gc != NULL) {
	Tk_FreeGC(graphPtr->display, hairsPtr->gc);
    }
    free((char *)hairsPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateCrosshairs --
 *
 *	Creates and initializes a new crosshair structure.
 *
 * Results:
 *	Returns TCL_ERROR if the crosshair structure cannot be created,
 *	otherwise TCL_OK.
 *
 * Side Effects:
 *	Crosshair GC is allocated.
 *
 *----------------------------------------------------------------------
 */
int
Blt_CreateCrosshairs(graphPtr)
    Graph *graphPtr;
{
    Crosshairs *hairsPtr;

    hairsPtr = (Crosshairs *)calloc(1, sizeof(Crosshairs));
    if (hairsPtr == NULL) {
	graphPtr->interp->result = "can't allocate crosshairs structure";
	return TCL_ERROR;
    }
    hairsPtr->mapped = hairsPtr->state = 0;
    hairsPtr->anchorPos.x = hairsPtr->anchorPos.y = DEF_POSITION;
    hairsPtr->destroyProc = DestroyCrosshairs;
    hairsPtr->toggleProc = ToggleCrosshairs;
    hairsPtr->updateProc = UpdateCrosshairs;
    hairsPtr->configProc = ConfigureCrosshairs;
    graphPtr->crosshairs = (GraphCrosshairs *)hairsPtr;
    if (Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin, configSpecs,
	    0, (char **)NULL, (char *)hairsPtr, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CrosshairsCmd --
 *
 *	User routine to configure crosshair simulation.  Crosshairs
 *	are simulated by drawing line segments parallel to both axes
 *	using the XOR drawing function. The allows the lines to be
 *	erased (by drawing them again) without redrawing the entire
 *	graph.  Care must be taken to erase crosshairs before redrawing
 *	the graph and redraw them after the graph is redraw.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Crosshairs may be drawn in the plotting area.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
int
Blt_CrosshairsCmd(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char **argv;
{
    char c;
    int length;
    int boolean;
    Tcl_Interp *interp = graphPtr->interp;
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " crosshairs option ?args?\"", NULL);
	return TCL_ERROR;
    }
    c = argv[2][0];
    length = strlen(argv[2]);

    if ((c == 'c') && (strncmp(argv[2], "configure", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" crosshairs configure \"", NULL);
	    return TCL_ERROR;
	}
	if (argc == 3) {
	    return (Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
		    (char *)hairsPtr, (char *)NULL, 0));
	} else if (argc == 4) {
	    return (Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
		    (char *)hairsPtr, argv[3], 0));
	}
	if (Tk_ConfigureWidget(interp, graphPtr->tkwin, configSpecs, argc - 3,
		argv + 3, (char *)hairsPtr, TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	    return TCL_ERROR;
	}
	ConfigureCrosshairs(graphPtr);
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[2], "toggle", length) == 0)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" crosshairs toggle\"", NULL);
	    return TCL_ERROR;
	}
	boolean = (!hairsPtr->mapped);
    } else if ((c == 's') && (strncmp(argv[2], "set", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" crosshairs set boolean\"", NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetBoolean(graphPtr->interp, argv[3], &boolean) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else {
	Tcl_AppendResult(interp, "bad crosshairs option \"", argv[2],
	    "\": should be configure, set, or toggle", (char *)NULL);
	return TCL_ERROR;
    }
    if (hairsPtr->mapped == boolean) {
	return TCL_OK;
    }
    hairsPtr->mapped = boolean;

    if (hairsPtr->mapped) {
	TurnOnHairs(graphPtr->tkwin, hairsPtr);
    } else {
	TurnOffHairs(graphPtr->tkwin, hairsPtr);
    }
    return TCL_OK;
}
