/*
 * bltGraph.h --
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

#ifndef _GRAPH_H
#define _GRAPH_H

#include <math.h>
#ifdef HAVE_FLOAT_H
#include <float.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

/*
 * ----------------------------------------------------------------------
 *
 * MAX_DBL_VALUE and MIN_DBL_VALUE are the largest and small values
 * representable on the graph or barchart.  They are used for the
 * following:
 *
 * 1. when searching for smallest or largest value of a set,
 *    initialize current min/max to this.
 *
 * 2. represent tag coordinates "Inf" and "-Inf".
 * ----------------------------------------------------------------------
 */
#ifdef HAVE_FLOAT_H
#define MAX_DBL_VALUE 	DBL_MAX
#define MIN_DBL_VALUE	DBL_MIN
#else
/* Don't want to include __infinity (definition of HUGE_VAL (SC1.x)) */
#ifdef sun
#define MAX_DBL_VALUE	1.7976931348623157E+308
#define MIN_DBL_VALUE 	2.2250738585072014E-308
#else
/* Start to go into punt formation. */
#ifdef HUGE_VAL
#define MAX_DBL_VALUE   HUGE_VAL
#define MIN_DBL_VALUE   (1/HUGE_VAL)
#ifdef HUGE
#define MAX_DBL_VALUE	HUGE
#define MIN_DBL_VALUE	(1/HUGE)
#else
/* Punt: Assume something simple and relatively small */
#define MAX_DBL_VALUE	3.40282347E+38
#define MIN_DBL_VALUE 	1.17549435E-38
#endif /*HUGE*/
#endif /*HUGE_VAL*/
#endif /*sun*/
#endif /*HAVE_FLOAT_H*/

#define MAX_POS_VAL	MAX_DBL_VALUE
#define MAX_NEG_VAL	(-MAX_DBL_VALUE)

#undef BLT_MIN
#define BLT_MIN(a,b)	(((a)<(b))?(a):(b))

#undef BLT_MAX
#define BLT_MAX(a,b)	(((a)>(b))?(a):(b))

/*
 * ------------------------------------------------------------------------
 *
 * Definitions of macros replacing math library functions, fabs, abs,
 * rint, and exp10.
 *
 * Although many of these routines may be in your math library, they
 * aren't used in libtcl.a or libtk.a.  This makes it impossible to
 * load BLT library as a shared object without requiring that the math
 * library be a shared object too.  We avoid the problem by replacing
 * the "exotic" math routines with macros.
 *
 * ------------------------------------------------------------------------
 */
#undef BLT_FABS
#define BLT_FABS(x) 	(((x)<0.0)?(-(x)):(x))

#undef BLT_ABS
#define BLT_ABS(x) 	(((x)<0)?(-(x)):(x))

#undef BLT_RND
#define BLT_RND(x) 	((int)((x) + (((x)<0.0) ? -0.5 : 0.5)))

#undef BLT_EXP10
#define BLT_EXP10(x)	(pow(10.0,(x)))

#ifndef M_PI
#define M_PI    	3.14159265358979323846
#endif /* M_PI */

#ifndef M_SQRT2
#define M_SQRT2		1.41421356237309504880
#endif /* M_SQRT1_2 */

#ifndef M_SQRT1_2
#define M_SQRT1_2	0.70710678118654752440
#endif /* M_SQRT1_2 */

#ifndef SHRT_MAX
#define SHRT_MAX	0x7FFF
#endif /* SHRT_MAX */

#define TRUE 1
#define FALSE 0

/*
 * Mask values used to selectively enable GRAPH or BARCHART entries in
 * the various configuration specs.
 */
#define XYGRAPH_MASK	TK_CONFIG_USER_BIT
#define BARCHART_MASK	TK_CONFIG_USER_BIT << 1
#define ALL_MASK	(XYGRAPH_MASK | BARCHART_MASK)
#define CONTOUR_MASK	TK_CONFIG_USER_BIT << 2

#define PADX		2	/* Padding between labels/titles */
#define PADY    	2	/* Padding between labels */
#define TEXTHEIGHT(f) 	((f)->ascent + (f)->descent)
#define DEF_POSITION 	-SHRT_MAX	/* Indicates that no position was
				     * specified */

/*
 * -------------------------------------------------------------------
 *
 * 	Graph component structure definitions
 *
 * -------------------------------------------------------------------
 */
typedef struct Graph Graph;
typedef struct GraphAxis GraphAxis;
typedef struct GraphLegend GraphLegend;
typedef struct GraphPostScript GraphPostScript;
typedef struct GraphCrosshairs GraphCrosshairs;

/*
 * -------------------------------------------------------------------
 *
 * GraphClassType --
 *
 *	Enumerates the different types of graphs this program
 *	produces. Currently, only one this of graph is allowed
 *	on a single axis.
 *
 * -------------------------------------------------------------------
 */
typedef enum {
    GRAPH, BARCHART, CONTOUR
} GraphClassType;

/*
 * -------------------------------------------------------------------
 *
 * TextAttributes --
 *
 * 	Represents a convenience structure to hold text attributes
 *	which determine how a text string is to be displayed on the
 *	window, or drawn with PostScript commands.  The alternative
 *	is to have drawing and printing routines with more parameters.
 * 	This seems like a more efficient and less cumbersome way.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    char *text;			/* Text string to be displayed  */
    XFontStruct *fontPtr;	/* Font to use for string */
    XColor *bgColorPtr;		/* Background color of string */
    XColor *fgColorPtr;		/* Foreground color of string */
    Tk_Anchor anchor;		/* Anchor type: used to position the
				 * string */
    double theta;		/* Rotation of text in degrees. */
    GC gc;			/* Graphics context to use when displaying
				 * the string on the window */
} TextAttributes;

/*
 * -------------------------------------------------------------------
 *
 * AxisTypes --
 *
 *	Enumerated type representing the types of axes
 *
 * -------------------------------------------------------------------
 */
typedef enum AxisTypes {
    X1_AXIS, Y1_AXIS, X2_AXIS, Y2_AXIS
} AxisType;

#define AXIS_MASK(a)	(1<<((a)->type))
#define X_AXIS(a)    	(!((a)->type&1))
#define Y_AXIS(a)	((a)->type&1)

/* AxisFlags used by elements and tags */
#define X1_AXIS_MASK	(1<<X1_AXIS)
#define Y1_AXIS_MASK	(1<<Y1_AXIS)
#define X2_AXIS_MASK	(1<<X2_AXIS)
#define Y2_AXIS_MASK	(1<<Y2_AXIS)

#define STD_AXES_MASK	(X1_AXIS_MASK|Y1_AXIS_MASK)
#define ANY_X_MASK	(X1_AXIS_MASK|X2_AXIS_MASK)
#define ANY_Y_MASK	(Y1_AXIS_MASK|Y2_AXIS_MASK)

typedef enum AxisLocations {
    BOTTOM_AXIS, LEFT_AXIS, TOP_AXIS, RIGHT_AXIS
} AxisLocation;

/*
 * -------------------------------------------------------------------
 *
 * GraphAxis --
 *
 * 	Structure contains options controlling how the axis will be
 * 	displayed.
 *
 * -------------------------------------------------------------------
 */

typedef void (AxisDisplayProc) _ANSI_ARGS_((Graph *graphPtr,
	GraphAxis *axisPtr, TextAttributes * attrPtr));
typedef void (AxisPrintProc) _ANSI_ARGS_((Graph *graphPtr, GraphAxis *axisPtr,
	TextAttributes * attrPtr));
typedef void (AxisLayoutProc) _ANSI_ARGS_((Graph *graphPtr,
	GraphAxis *axisPtr));
typedef void (AxisDestroyProc) _ANSI_ARGS_((Graph *graphPtr,
	GraphAxis *axisPtr));

struct GraphAxis {
    enum AxisTypes type;
    enum AxisLocations location;/* Location of the axis: right, left, etc. */
    int logScale;		/* If non-zero, scale values
				 * logarithmically */
    int mapped;			/* If non-zero, display the axis */

    AxisDisplayProc *displayProc;
    AxisPrintProc *printProc;
    AxisLayoutProc *layoutProc;
    AxisDestroyProc *destroyProc;
};

/*
 * -------------------------------------------------------------------
 *
 * GraphLegend --
 *
 * 	Contains information specific to how the legend will be
 *	displayed.
 *
 * -------------------------------------------------------------------
 */

typedef void (LegendDisplayProc) _ANSI_ARGS_((Graph *graphPtr));
typedef void (LegendPrintProc) _ANSI_ARGS_((Graph *graphPtr));
typedef void (LegendDestroyProc) _ANSI_ARGS_((Graph *graphPtr));
typedef void (LegendGeometryProc) _ANSI_ARGS_((Graph *graphPtr, int height));

struct GraphLegend {
    int mapped;			/* Requested state of the legend, If
				 * non-zero, legend is displayed */
    int width, height;		/* Dimensions of the legend */
    XPoint anchorPos;		/* Window coordinates of legend
				 * positioning point. Used in conjunction
				 * with the anchor to determine the
				 * location of the legend. */
    int useDefault;		/* Draw the legend in the default location */

    LegendDisplayProc *displayProc;
    LegendPrintProc *printProc;
    LegendDestroyProc *destroyProc;
    LegendGeometryProc *geomProc;
};

/*
 * -------------------------------------------------------------------
 *
 * GraphPostScript --
 *
 * 	Structure contains information specific to the outputting of
 *	PostScript commands to print the graph.
 *
 * -------------------------------------------------------------------
 */

typedef int (PostScriptConfigureProc) _ANSI_ARGS_((Graph *graphPtr, int argc,
	char **argv));
typedef int (PostScriptPrintProc) _ANSI_ARGS_((Graph *graphPtr, int argc,
	char **argv));
typedef void (PostScriptDestroyProc) _ANSI_ARGS_((Graph *graphPtr));

struct GraphPostScript {
    PostScriptConfigureProc *configProc;
    PostScriptPrintProc *printProc;
    PostScriptDestroyProc *destroyProc;
};

/*
 * -------------------------------------------------------------------
 *
 * GraphCrosshairs --
 *
 *	Contains the line segments positions and graphics context used
 *	to simulate crosshairs (by XORing) on the graph.
 *
 * -------------------------------------------------------------------
 */
typedef void (CrosshairsToggleProc) _ANSI_ARGS_((Graph *graphPtr));
typedef void (CrosshairsUpdateProc) _ANSI_ARGS_((Graph *graphPtr));
typedef void (CrosshairsConfigProc) _ANSI_ARGS_((Graph *graphPtr));
typedef void (CrosshairsDestroyProc) _ANSI_ARGS_((Graph *graphPtr));

struct GraphCrosshairs {
    CrosshairsToggleProc *toggleProc;	/* Toggle visiblity of crosshairs */
    CrosshairsUpdateProc *updateProc;	/* Update lengths of hairs */
    CrosshairsConfigProc *configProc;	/* Configure GC */
    CrosshairsDestroyProc *destroyProc;	/* Release X resources */
};

/*
 * -------------------------------------------------------------------
 *
 * Graph --
 *
 *	Top level structure containing everything pertaining to
 *	the graph.
 *
 * -------------------------------------------------------------------
 */
struct Graph {
    Tk_Window tkwin;		/* Window that embodies the graph.  NULL
				 * means that the window has been
				 * destroyed but the data structures
				 * haven't yet been cleaned up. */
    Pixmap canvas;		/* Pixmap for double buffering output */
    Display *display;		/* Display containing widget; needed,
				 * among other things, to release
				 * resources after tkwin has already gone
				 * away. */
    char *pathName;		/* Pathname of the widget. Is saved here
				 * in case the widget is destroyed and
				 * tkwin become unavailable for querying
				 * */
    Tcl_Interp *interp;		/* Interpreter associated with graph */
    GraphClassType type;	/* Type: either GRAPH or BARCHART */
    unsigned int flags;		/* Flags;  see below for definitions. */

    GraphPostScript *postscript;/* PostScript options: see bltGrPS.c */
    GraphLegend *legendPtr;	/* Legend information: see bltGrLegd.c */
    GraphAxis *axisArr[4];	/* Coordinate axis info: see bltGrAxis.c */
    GraphAxis *bottomAxis, *topAxis, *leftAxis, *rightAxis;
    GraphCrosshairs *crosshairs;/* Crosshairs information: see
				   bltGrHairs.c */
    Tcl_HashTable elemTable;	/* Hash table containing all elements
				 * created, not just those currently
				 * displayed. */
    Blt_LinkedList elemList;	/* Display list of elements */
    Tcl_HashTable tagTable;	/* Hash table of tags */
    Blt_LinkedList tagList;	/* Display list of tags */
    unsigned int nextTagId;	/* Tracks next tag identifier available */

    int reqWidth, reqHeight;	/* Requested size of graph window */
    int halo;			/* Maximum distance allowed between points
				 * when searching for a point */
    unsigned int buffered;	/* If non-zero, cache elements by drawing
				 * them into a pixmap */
    unsigned int inverted;	/* If non-zero, indicates the x and y axis
				 * positions should be inverted. */
    double barWidth;		/* Scale factor for the width of bar */

    int leftMargin;		/* Requested sizes for margins surrounding */
    int rightMargin;		/* the plotting area. If non-zero, the */
    int topMargin;		/* requested margin is used, otherwise the */
    int bottomMargin;		/* computed margin sizes are used. */

    char *title;		/* Graph title */

    XFontStruct *fontPtr;	/* Font for graph and axis titles */
    Cursor cursor;

    int borderWidth;		/* Width the exterior border */
    int relief;			/* Relief of the exterior border */
    Tk_3DBorder border;		/* 3-D border used to delineate the plot
				 * surface and outer edge of window */
    XColor *marginFg;		/* Foreground color of title and axis
				 * labels */
    int plotBW;			/* Width of interior 3-D border. */
    int plotRelief;		/* 3-d effect: TK_RELIEF_RAISED etc. */
    XColor *plotBg;		/* Color of plotting surface */

    GC plotFillGC;		/* GC to fill the plotting area with a
				 * solid background color. The fill color
				 * is stored in "plotBg". */
    GC marginGC;		/* Graphics context for margin. Includes
				 * margin background */
    GC marginFillGC;		/* GC to fill the background of the
				 * margins, clipping the plotting
				 * area. The fill color is obtained
				 * from "border". */

    double avgSymSize;		/* Average size of a symbol */

    int width, height;		/* Size of graph window or PostScript
				 * page */
    XPoint origin;		/* Origin of the graph */
    XPoint extreme;		/* Outer limit of graph */

    Pixmap elemMap;		/* For double buffering the display of
				 * graph elements */
    unsigned int elemWidth;	/* Dimensions of element buffer pixmap */
    unsigned int elemHeight;
    char *scratchPtr;		/* Utility space for building strings.
				 * Points to buffer of BUFSIZ bytes on
				 * the stack.  Currently used to
				 * create PostScript output for the
				 * "postscript" command. */


};

/*
 * Flag bits for graphs:
 */

#define	LAYOUT_NEEDED 	(1<<0)	/* Indicates that one of the many
				 * graph configurations has changed
				 * (element, tag, axis, legend, etc)
				 * and the layout of the graph
				 * (position of the graph in the
				 * window) needs to be recalculated. */

#define	LAYOUT_DIRTY	(1<<1)	/* Non-zero indicates that the layout
				 * of the axes and all elements and
				 * tags and the graph need to be
				 * recalculated. Otherwise, the layout
				 * of only those tags and elements that
				 * have changed will be reset. */

#define	LAYOUT_ALL 	(LAYOUT_NEEDED|LAYOUT_DIRTY)

#define REDRAW_PENDING 	(1<<3)	/* Non-zero means a DoWhenIdle
				 * handler has already been queued to
				 * redraw this window. */

#define REFRESH		(1<<4)	/* Non-zero means that exterior
				 * region of the graph, in addition to
				 * the interior (plotting surface)
				 * needs to be redrawn. The possible
				 * reasons are:
				 *
				 * 1) an axis configuration changed
				 *
				 * 2) an axis limit changed
				 *
				 * 3) titles have changed
				 *
				 * 4) window was resized.
				 */

#define LEGEND_ONLY	(1<<5)	/* Non-zero means that only the legend
				 * need to be redrawn. In this case, the
				 * legend display routine is called instead
				 * of the graph display routine. */


#define	DIRTY		(1<<6)	/* If set, redraw all elements into the
				 * pixmap used for buffering elements. */

/*
 * ---------------------- Forward declarations ------------------------
 */
extern Pixmap Blt_CreateTextBitmap _ANSI_ARGS_((Display * display,
	Drawable draw, XFontStruct *fontPtr, char *textStr, double theta,
	unsigned int *bmWPtr, unsigned int *bmHPtr));
extern void Blt_DrawText _ANSI_ARGS_((Display * display, Drawable draw,
	char *text, TextAttributes * attrPtr, int x, int y));
extern void Blt_RedrawGraph _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_GetBoundingBox _ANSI_ARGS_((unsigned int width,
	unsigned int height, double theta, unsigned int *widthPtr,
	unsigned int *heightPtr, XPoint *pointArr));
extern Pixmap Blt_RotateBitmap _ANSI_ARGS_((Display * display, Drawable draw,
	GC gc, Pixmap bitmap, unsigned int width, unsigned int height,
	double theta, unsigned int *rotWPtr, unsigned int *rotHPtr));
extern void Blt_ComputeAxes _ANSI_ARGS_((Graph *graphPtr));

extern int Blt_Transform _ANSI_ARGS_((GraphAxis *axis, double value));
extern double Blt_InvTransform _ANSI_ARGS_((GraphAxis *axis, int coord));
extern XPoint Blt_TransformPt _ANSI_ARGS_((Graph *graphPtr, double x, double y,
	unsigned int axisFlags));
extern int Blt_TransformDist _ANSI_ARGS_((GraphAxis *axis, double value));

extern void Blt_StencilBitmap _ANSI_ARGS_((Display * display, Drawable draw,
	GC gc, Pixmap bitmap, int x, int y, unsigned int width,
	unsigned int height));
extern unsigned int Blt_TextStringWidth _ANSI_ARGS_((XFontStruct *fontPtr,
	char *text));
extern XPoint Blt_TranslateBoxCoords _ANSI_ARGS_((int x, int y,
	unsigned int width, unsigned int height, Tk_Anchor anchor));
extern XPoint Blt_TranslateTextCoords _ANSI_ARGS_((XFontStruct *fontPtr,
	char *text, int x, int y, Tk_Anchor anchor));

extern void Blt_PrintLine _ANSI_ARGS_((Graph *graphPtr, XPoint *pointArr,
	int numPoints));
extern void Blt_PrintBitmap _ANSI_ARGS_((Graph *graphPtr, Pixmap bitmap, int x,
	int y, unsigned int width, unsigned int height));
extern void Blt_SetLineAttributes _ANSI_ARGS_((Graph *graphPtr,
	XColor *colorPtr, int lineWidth, int lineDashes));
extern void Blt_3DRectangleToPostScript _ANSI_ARGS_((Graph *graphPtr,
	Tk_3DBorder border, int x, int y, unsigned int width,
	unsigned int height, int borderWidth, int relief));
extern void Blt_BackgroundToPostScript _ANSI_ARGS_((Graph *graphPtr,
	XColor *colorPtr));
extern void Blt_BitmapToPostScript _ANSI_ARGS_((Graph *graphPtr, Pixmap bitmap,
	int x, int y, unsigned int width, unsigned int height, double theta,
	XColor *bgColorPtr));
extern void Blt_FontToPostScript _ANSI_ARGS_((Graph *graphPtr,
	XFontStruct *fontPtr));
extern void Blt_ForegroundToPostScript _ANSI_ARGS_((Graph *graphPtr,
	XColor *colorPtr));
extern void Blt_LineDashesToPostScript _ANSI_ARGS_((Graph *graphPtr,
	int lineDashes));
extern void Blt_LineWidthToPostScript _ANSI_ARGS_((Graph *graphPtr,
	int lineWidth));
extern void Blt_LinesToPostScript _ANSI_ARGS_((Graph *graphPtr,
	XPoint *pointArr, int numPoints));
extern void Blt_PolygonToPostScript _ANSI_ARGS_((Graph *graphPtr,
	XPoint *pointArr, int numPoints));
extern void Blt_Print3DRectangle _ANSI_ARGS_((Graph *graphPtr,
	Tk_3DBorder border, int x, int y, unsigned int width,
	unsigned int height, int borderWidth, int relief));
extern void Blt_RectangleToPostScript _ANSI_ARGS_((Graph *graphPtr,
	int x, int y, unsigned int width, unsigned int height));
extern void Blt_RectanglesToPostScript _ANSI_ARGS_((Graph *graphPtr,
	XRectangle *rectArr, int numRects));
extern void Blt_SegmentsToPostScript _ANSI_ARGS_((Graph *graphPtr,
	XSegment *segArr, int numSegs));
extern void Blt_StippleToPostScript _ANSI_ARGS_((Graph *graphPtr,
	Pixmap bitmap, unsigned int width, unsigned int height, int fgOrBg));
extern void Blt_TextToPostScript _ANSI_ARGS_((Graph *graphPtr, char *text,
	TextAttributes * attrPtr, int x, int y));
extern int Blt_GetTokenIndex _ANSI_ARGS_((char **list, char *key, int flag));
extern int Blt_OptionChanged _ANSI_ARGS_(VARARGS);

/*
 * ---------------------- Global declarations ------------------------
 */
extern double Blt_negInfinity, Blt_posInfinity;

#endif /* _GRAPH_H */
