/*
 * bltGrAxis.c --
 *
 *	This module implements coordinate axes for a graph
 *	widget in the Tk toolkit.
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

#include <assert.h>
#include "blt.h"
#include "bltGraph.h"
#include "bltGrElem.h"
#include <ctype.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define NTICK 5

#ifndef SHRT_MIN
#define SHRT_MIN                -0x8000
#endif /* SHRT_MIN */
#ifndef SHRT_MAX
#define SHRT_MAX                 0x7FFF
#endif /* SHRT_MAX */

/*
 * Round x in terms of units
 */
#define UROUND(x,u)	(BLT_RND((x)/(u))*(u))
#define UCEIL(x,u)	(ceil((x)/(u))*(u))
#define UFLOOR(x,u)	(floor((x)/(u))*(u))

#define DEF_MAJOR_TICK 	0.030	/* Length of a major tick */
#define DEF_MINOR_TICK 	0.015	/* Length of a minor (sub)tick */
#define DEF_LABEL_TICK 	0.040	/* Distance from graph to start of label */

#define NUMDIGITS	15	/* Specifies the number of digits of
				 * accuracy used when outputting axis
				 * tick labels. */
enum PositionIndices {
    MAJOR_TICK, MINOR_TICK, TICK_LABEL, AXIS_LINE
};

#define AVG_TICK_NUM_CHARS	16	/* Assumed average tick label size */

enum LimitIndices {
    LMIN, LMAX
};

/* Note: The following array is ordered according to enum AxisTypes */
static char *axisNames[] =
{
    "x", "y", "x2", "y2"
};

/* Map normalized coordinates to window coordinates */
#define MAPX(X,x)    	(BLT_RND((x)*(X)->scale)+(X)->offset)
#define MAPY(Y,y)    	((Y)->offset-BLT_RND((y)*(Y)->scale))

/* Map graph coordinates to normalized coordinates [0..1] */
#define NORM(a,x) 	(((x) - (a)->min) / (a)->range)

/*
 * Sun's bundled and unbundled C compilers choke on static function
 * typedefs (while it can handle extern declarations) like
 *
 * 	static Tk_OptionParseProc parseProc;
 *  	static Tk_OptionPrintProc printProc;
 *
 * As a workaround, provide forward declarations here:
 */
static int ParseAxisLimit _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp, Tk_Window tkwin, char *value, char *widgRec, int offset));
static char *PrintAxisLimit _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin, char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption MinLimitOption =
{
    ParseAxisLimit, PrintAxisLimit, (ClientData)LMIN,
};
static Tk_CustomOption MaxLimitOption =
{
    ParseAxisLimit, PrintAxisLimit, (ClientData)LMAX,
};

/*
 * ----------------------------------------------------------------------
 *
 * Label --
 *
 * 	Structure containing the formatted label and the screen
 * 	coordinates position of the tick label (anchored at its
 * 	center).
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    char *text;			/* Label for tick on axis */
    short int x, y;		/* Window position of tick on graph */
} Label;

/*
 * ----------------------------------------------------------------------
 *
 * Axis --
 *
 * 	Structure contains options controlling how the axis will be
 * 	displayed.
 *
 * ----------------------------------------------------------------------
 */

typedef struct {
    enum AxisTypes type;	/* Type of axis: X1, Y1, X2, or Y2 */
    enum AxisLocations location;/* Location of the axis relative to plotting
				 * surface: right, left, etc. */
    int logScale;		/* If non-zero, scale values logarithmically */
    int mapped;			/* If non-zero, display the axis */

    AxisDisplayProc *displayProc;
    AxisPrintProc *printProc;
    AxisLayoutProc *layoutProc;
    AxisDestroyProc *destroyProc;

    /* User-definable fields */
    char *title;		/* Axis title */
    int showTicks;		/* If non-zero, display (both major
				 * and minor) ticks on the axis */
    int loose;			/* If non-zero, autoscale limits loosely */
    int descending;		/* In non-zero, axis values are descending
				 * instead of monotonically increasing. */
    double limits[2];		/* Limits for scaling of axis */
    double prevMin, prevMax;	/* Last limits for scaling of axis */
    double reqStep;		/* Manually selected step size for
				 * major ticks: If zero or less,
				 * automatically calculate a "best"
				 * step size based on range of
				 * values. */
    int reqSubTicks;		/* Manually selected # of subticks:
				 * The default value is 2. */
    XFontStruct *fontPtr;	/* Font used to draw tick labels. */
    XColor *fgColorPtr;		/* Foreground color for ticks, labels,
				 * and axis */
    int lineWidth;		/* Line thickness of axis and ticks */
    double theta;		/* Rotation of tick labels in degrees. */
    char *formatCmd;		/* If non-NULL, indicates a Tcl proc
				 * to call when formatting tick
				 * labels. See the manual for its
				 * usage. */
    double subStep;		/* Step interval between minor ticks */
    int subTicks;		/* # of minor ticks between major
				 * ticks */
    double step;		/* Step interval between major ticks */
    int numTicks;		/* # of major ticks possible on axis:
				 * Calculated by tick layout routines. */

    /* Calculated values */
    unsigned int flags;

    Tk_Anchor anchor;		/* Anchor of tick labels */
    int posArr[4];		/* Screen location of axis, major tick,
				 * minor tick, and tick label */
    XPoint titlePos;		/* Coordinates of axis title */

    Tcl_Interp *interp;
    unsigned int width, height;	/* Bounding box of axis */

    double tickMin, tickMax;	/* Smallest and largest possible major
				 * ticks on the plot */
    int tickLength;		/* Legend of major tick on axis. */
    double min, max;		/* Actual (including padding) axis limits */
    double range;		/* Range of values (max-min) */
    double scale;		/* Scale factor to convert values to
				 * pixels */
    int offset;			/* Offset of plotting region from window
				 * origin */

    GC lineGC;			/* Graph context for axis lines and ticks */
    GC textGC;			/* Graphic context for tick labels:
				 * Must be a private GC (can't use
				 * Tk_GetGC) because the FillStyle,
				 * TSOrigin, and Stipple fields may
				 * change when the graph is layout is
				 * calculated, to accommodate rotation
				 * of text. Also, note that the
				 * background color is also reset when
				 * the background color of the graph
				 * changes. */
    int numSegments;		/* Size of the above segment array */
    XSegment *segArr;		/* Array of computed tick line
				 * segments. Also includes the axis
				 * line */
    int numLabels;		/* Size of the above label array */
    Label *labelArr;		/* Array of computed tick labels: See the
				 * description of the Label structure. */

} Axis;

/* Axis flags: */
#define AXIS_CONFIG_DIRTY	(1<<8)
#define AXIS_CONFIG_USER_BIT	(1<<9)
#define AXIS_CONFIG_MIN_MASK	(AXIS_CONFIG_USER_BIT << LMIN)
#define AXIS_CONFIG_MAX_MASK	(AXIS_CONFIG_USER_BIT << LMAX)
#define AXIS_CONFIG_MIN_SET(a) 	((a)->flags & AXIS_CONFIG_MIN_MASK)
#define AXIS_CONFIG_MAX_SET(a) 	((a)->flags & AXIS_CONFIG_MAX_MASK)

#define VERTICAL_AXIS(a)	((a)->location&1)
#define HORIZONTAL_AXIS(a)    	(!((a)->location&1))

#define DEF_AXIS_ALT_MAPPED 	"false"
#define DEF_AXIS_COMMAND	(char *)NULL
#define DEF_AXIS_DESCENDING 	"0"
#define DEF_AXIS_FG_COLOR	BLACK
#define DEF_AXIS_FG_MONO	BLACK
#define DEF_AXIS_FONT	 	"*-Courier-Bold-R-Normal-*-100-*"
#define DEF_AXIS_TICK_LENGTH	"0.1i"
#define DEF_AXIS_LINE_WIDTH	"0"
#define DEF_AXIS_LOG_SCALE 	"0"
#define DEF_AXIS_LOOSE 		"0"
#define DEF_AXIS_MAX		(char *)NULL
#define DEF_AXIS_MIN		(char *)NULL
#define DEF_AXIS_ROTATE		"0.0"
#define DEF_AXIS_STD_MAPPED 	"1"
#define DEF_AXIS_STEPSIZE	"0.0"
#define DEF_AXIS_SUBTICKS	"2"
#define DEF_AXIS_TICKS		"1"
#define DEF_AXIS_X_STEPSIZE_BARCHART	"1.0"
#define DEF_AXIS_X_SUBTICKS_BARCHART	"0"
#define DEF_AXIS_TITLE		(char *)NULL

static Tk_ConfigSpec xAxisConfigSpecs[] =
{
    {TK_CONFIG_COLOR, "-color", "xColor", "AxisColor",
	DEF_AXIS_FG_COLOR, Tk_Offset(Axis, fgColorPtr),
	TK_CONFIG_COLOR_ONLY | ALL_MASK},
    {TK_CONFIG_COLOR, "-color", "xColor", "AxisColor",
	DEF_AXIS_FG_MONO, Tk_Offset(Axis, fgColorPtr),
	TK_CONFIG_MONO_ONLY | ALL_MASK},
    {TK_CONFIG_STRING, "-command", "xCommand", "AxisCommand",
	DEF_AXIS_COMMAND, Tk_Offset(Axis, formatCmd),
	ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-descending", "xDescending", "AxisDescending",
	DEF_AXIS_DESCENDING, Tk_Offset(Axis, descending),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_FONT, "-font", "xFont", "AxisFont",
	DEF_AXIS_FONT, Tk_Offset(Axis, fontPtr), ALL_MASK},
    {TK_CONFIG_PIXELS, "-linewidth", "xLinewidth", "AxisLinewidth",
	DEF_AXIS_LINE_WIDTH, Tk_Offset(Axis, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-logscale", "xLogscale", "AxisLogscale",
	DEF_AXIS_LOG_SCALE, Tk_Offset(Axis, logScale),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-loose", "xLoose", "AxisLoose",
	DEF_AXIS_LOOSE, Tk_Offset(Axis, loose),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-mapped", "xMapped", "AxisMapped",
	DEF_AXIS_STD_MAPPED, Tk_Offset(Axis, mapped),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_CUSTOM, "-max", "xMax", "AxisMax",
	DEF_AXIS_MAX, 0, ALL_MASK | TK_CONFIG_NULL_OK, &MaxLimitOption},
    {TK_CONFIG_CUSTOM, "-min", "xMin", "AxisMin",
	DEF_AXIS_MIN, 0, ALL_MASK | TK_CONFIG_NULL_OK, &MinLimitOption},
    {TK_CONFIG_DOUBLE, "-rotate", "xRotate", "AxisRotate",
	DEF_AXIS_ROTATE, Tk_Offset(Axis, theta),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-showticks", "xShowticks", "AxisShowticks",
	DEF_AXIS_TICKS, Tk_Offset(Axis, showTicks),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_DOUBLE, "-stepsize", "xStepsize", "AxisStepsize",
	DEF_AXIS_STEPSIZE, Tk_Offset(Axis, reqStep), XYGRAPH_MASK},
    {TK_CONFIG_DOUBLE, "-stepsize", "xStepsize", "AxisStepsize",
	DEF_AXIS_X_STEPSIZE_BARCHART, Tk_Offset(Axis, reqStep),
	BARCHART_MASK},
    {TK_CONFIG_INT, "-subticks", "xSubticks", "AxisSubticks",
	DEF_AXIS_SUBTICKS, Tk_Offset(Axis, reqSubTicks), XYGRAPH_MASK},
    {TK_CONFIG_INT, "-subticks", "xSubticks", "AxisSubticks",
	DEF_AXIS_X_SUBTICKS_BARCHART, Tk_Offset(Axis, reqSubTicks),
	BARCHART_MASK},
    {TK_CONFIG_PIXELS, "-ticklength", "xTickLength", "AxisTickLength",
	DEF_AXIS_TICK_LENGTH, Tk_Offset(Axis, tickLength), ALL_MASK},
    {TK_CONFIG_STRING, "-title", "xTitle", "AxisTitle",
	"X", Tk_Offset(Axis, title), ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};


static Tk_ConfigSpec x2AxisConfigSpecs[] =
{
    {TK_CONFIG_COLOR, "-color", "x2Color", "AxisColor",
	DEF_AXIS_FG_COLOR, Tk_Offset(Axis, fgColorPtr),
	TK_CONFIG_COLOR_ONLY | ALL_MASK},
    {TK_CONFIG_COLOR, "-color", "x2Color", "AxisColor",
	DEF_AXIS_FG_MONO, Tk_Offset(Axis, fgColorPtr),
	TK_CONFIG_MONO_ONLY | ALL_MASK},
    {TK_CONFIG_STRING, "-command", "x2Command", "AxisCommand",
	DEF_AXIS_COMMAND, Tk_Offset(Axis, formatCmd),
	ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-descending", "x2Descending", "AxisDescending",
	DEF_AXIS_DESCENDING, Tk_Offset(Axis, descending),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_FONT, "-font", "x2Font", "AxisFont",
	DEF_AXIS_FONT, Tk_Offset(Axis, fontPtr), ALL_MASK},
    {TK_CONFIG_PIXELS, "-linewidth", "x2Linewidth", "AxisLinewidth",
	DEF_AXIS_LINE_WIDTH, Tk_Offset(Axis, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-logscale", "x2Logscale", "AxisLogscale",
	DEF_AXIS_LOG_SCALE, Tk_Offset(Axis, logScale),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-loose", "x2Loose", "AxisLoose",
	DEF_AXIS_LOOSE, Tk_Offset(Axis, loose),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-mapped", "x2Mapped", "AxisMapped",
	DEF_AXIS_ALT_MAPPED, Tk_Offset(Axis, mapped),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_CUSTOM, "-max", "x2Max", "AxisMax",
	DEF_AXIS_MAX, 0, ALL_MASK | TK_CONFIG_NULL_OK, &MaxLimitOption},
    {TK_CONFIG_CUSTOM, "-min", "x2Min", "AxisMin",
	DEF_AXIS_MIN, 0, ALL_MASK | TK_CONFIG_NULL_OK, &MinLimitOption},
    {TK_CONFIG_DOUBLE, "-rotate", "x2Rotate", "AxisRotate",
	DEF_AXIS_ROTATE, Tk_Offset(Axis, theta),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-showticks", "x2Showticks", "AxisShowticks",
	DEF_AXIS_TICKS, Tk_Offset(Axis, showTicks), ALL_MASK},
    {TK_CONFIG_DOUBLE, "-stepsize", "x2Stepsize", "AxisStepsize",
	DEF_AXIS_STEPSIZE, Tk_Offset(Axis, reqStep),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_INT, "-subticks", "x2Subticks", "AxisSubticks",
	DEF_AXIS_SUBTICKS, Tk_Offset(Axis, reqSubTicks),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_PIXELS, "-ticklength", "x2TickLength", "AxisTickLength",
	DEF_AXIS_TICK_LENGTH, Tk_Offset(Axis, tickLength), ALL_MASK},
    {TK_CONFIG_STRING, "-title", "x2Title", "AxisTitle",
	DEF_AXIS_TITLE, Tk_Offset(Axis, title), ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

static Tk_ConfigSpec yAxisConfigSpecs[] =
{
    {TK_CONFIG_COLOR, "-color", "yColor", "AxisColor",
	DEF_AXIS_FG_COLOR, Tk_Offset(Axis, fgColorPtr),
	TK_CONFIG_COLOR_ONLY | ALL_MASK},
    {TK_CONFIG_COLOR, "-color", "yColor", "AxisColor",
	DEF_AXIS_FG_MONO, Tk_Offset(Axis, fgColorPtr),
	TK_CONFIG_MONO_ONLY | ALL_MASK},
    {TK_CONFIG_STRING, "-command", "yCommand", "AxisCommand",
	(char *)NULL, Tk_Offset(Axis, formatCmd),
	ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-descending", "yDescending", "AxisDescending",
	DEF_AXIS_DESCENDING, Tk_Offset(Axis, descending),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_FONT, "-font", "yFont", "AxisFont",
	DEF_AXIS_FONT, Tk_Offset(Axis, fontPtr), ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-loose", "yLoose", "AxisLoose",
	DEF_AXIS_LOOSE, Tk_Offset(Axis, loose),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_PIXELS, "-linewidth", "yLinewidth", "AxisLinewidth",
	DEF_AXIS_LINE_WIDTH, Tk_Offset(Axis, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-logscale", "yLogscale", "AxisLogscale",
	DEF_AXIS_LOG_SCALE, Tk_Offset(Axis, logScale),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-mapped", "yMapped", "AxisMapped",
	DEF_AXIS_STD_MAPPED, Tk_Offset(Axis, mapped),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_CUSTOM, "-max", "yMax", "AxisMax",
	DEF_AXIS_MAX, 0, ALL_MASK | TK_CONFIG_NULL_OK, &MaxLimitOption},
    {TK_CONFIG_CUSTOM, "-min", "yMin", "AxisMin",
	DEF_AXIS_MIN, 0, ALL_MASK | TK_CONFIG_NULL_OK, &MinLimitOption},
    {TK_CONFIG_DOUBLE, "-rotate", "yRotate", "AxisRotate",
	DEF_AXIS_ROTATE, Tk_Offset(Axis, theta),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-showticks", "yShowticks", "AxisShowticks",
	DEF_AXIS_TICKS, Tk_Offset(Axis, showTicks),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_DOUBLE, "-stepsize", "yStepsize", "AxisStepsize",
	DEF_AXIS_STEPSIZE, Tk_Offset(Axis, reqStep),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_INT, "-subticks", "ySubticks", "AxisSubticks",
	DEF_AXIS_SUBTICKS, Tk_Offset(Axis, reqSubTicks), ALL_MASK},
    {TK_CONFIG_PIXELS, "-ticklength", "yTickLength", "AxisTickLength",
	DEF_AXIS_TICK_LENGTH, Tk_Offset(Axis, tickLength), ALL_MASK},
    {TK_CONFIG_STRING, "-title", "yTitle", "AxisTitle",
	"Y", Tk_Offset(Axis, title), ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

static Tk_ConfigSpec y2AxisConfigSpecs[] =
{
    {TK_CONFIG_COLOR, "-color", "y2Color", "AxisColor",
	DEF_AXIS_FG_COLOR, Tk_Offset(Axis, fgColorPtr),
	TK_CONFIG_COLOR_ONLY | ALL_MASK},
    {TK_CONFIG_COLOR, "-color", "y2Color", "AxisColor",
	DEF_AXIS_FG_MONO, Tk_Offset(Axis, fgColorPtr),
	TK_CONFIG_MONO_ONLY | ALL_MASK},
    {TK_CONFIG_STRING, "-command", "y2Command", "AxisCommand",
	DEF_AXIS_COMMAND, Tk_Offset(Axis, formatCmd),
	ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-descending", "y2Descending", "AxisDescending",
	DEF_AXIS_DESCENDING, Tk_Offset(Axis, descending),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_FONT, "-font", "y2Font", "AxisFont",
	DEF_AXIS_FONT, Tk_Offset(Axis, fontPtr), ALL_MASK},
    {TK_CONFIG_PIXELS, "-linewidth", "y2Linewidth", "AxisLinewidth",
	DEF_AXIS_LINE_WIDTH, Tk_Offset(Axis, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-loose", "y2Loose", "AxisLoose",
	DEF_AXIS_LOOSE, Tk_Offset(Axis, loose),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-logscale", "y2Logscale", "AxisLogscale",
	DEF_AXIS_LOG_SCALE, Tk_Offset(Axis, logScale),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-mapped", "y2Mapped", "AxisMapped",
	DEF_AXIS_ALT_MAPPED, Tk_Offset(Axis, mapped),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_CUSTOM, "-max", "y2Max", "AxisMax",
	DEF_AXIS_MAX, 0, ALL_MASK | TK_CONFIG_NULL_OK, &MaxLimitOption},
    {TK_CONFIG_CUSTOM, "-min", "y2Min", "AxisMin",
	DEF_AXIS_MIN, 0, ALL_MASK | TK_CONFIG_NULL_OK, &MinLimitOption},
    {TK_CONFIG_DOUBLE, "-rotate", "y2Rotate", "AxisRotate",
	DEF_AXIS_ROTATE, Tk_Offset(Axis, theta),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_BOOLEAN, "-showticks", "y2Showticks", "AxisShowticks",
	DEF_AXIS_TICKS, Tk_Offset(Axis, showTicks),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_DOUBLE, "-stepsize", "y2Stepsize", "AxisStepsize",
	DEF_AXIS_STEPSIZE, Tk_Offset(Axis, reqStep),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_MASK},
    {TK_CONFIG_INT, "-subticks", "y2Subticks", "AxisSubticks",
	DEF_AXIS_SUBTICKS, Tk_Offset(Axis, reqSubTicks), ALL_MASK},
    {TK_CONFIG_PIXELS, "-ticklength", "y2TickLength", "AxisTickLength",
	DEF_AXIS_TICK_LENGTH, Tk_Offset(Axis, tickLength), ALL_MASK},
    {TK_CONFIG_STRING, "-title", "y2Title", "AxisTitle",
	DEF_AXIS_TITLE, Tk_Offset(Axis, title), ALL_MASK | TK_CONFIG_NULL_OK},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

static Tk_ConfigSpec *axisConfigSpecs[4] =
{
    xAxisConfigSpecs, yAxisConfigSpecs,	/* Normal x-axis and y-axis */
    x2AxisConfigSpecs, y2AxisConfigSpecs	/* Alternate x-axis and y-axis */
};

/* ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */
/*
 * ----------------------------------------------------------------------
 *
 * ParseAxisLimit --
 *
 *	Convert the string representation of an axis limit into its
 *	numeric form.
 *
 * Results:
 *	The return value is a standard Tcl result.  The symbol type is
 *	written into the widget record.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseAxisLimit(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* Either LMIN or LMAX */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* */
    char *widgRec;		/* Axis structure */
    int offset;			/* Offset of limit */
{
    Axis *axisPtr = (Axis *)(widgRec);
    int whichLimit = (int)clientData;
    unsigned int mask;

    mask = (AXIS_CONFIG_USER_BIT << whichLimit);
    if ((value == NULL) || (*value == '\0')) {
	axisPtr->flags &= ~mask;
    } else {
	double newLimit;

	if (Tcl_ExprDouble(interp, value, &newLimit) != TCL_OK) {
	    return TCL_ERROR;
	}
	axisPtr->limits[whichLimit] = newLimit;
	axisPtr->flags |= mask;
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintAxisLimit --
 *
 *	Convert the floating point axis limit into a string.
 *
 * Results:
 *	The string representation of the limit is returned.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintAxisLimit(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Either LMIN or LMAX */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* */
    int offset;
    Tcl_FreeProc **freeProcPtr;
{
    Axis *axisPtr = (Axis *)(widgRec);
    int whichLimit = (int)clientData;
    unsigned int mask;
    char *result;

    result = "";
    mask = (AXIS_CONFIG_USER_BIT << whichLimit);
    if (axisPtr->flags & mask) {
	char string[TCL_DOUBLE_SPACE + 1];

	Tcl_PrintDouble(axisPtr->interp, axisPtr->limits[whichLimit],
	    string);
	result = strdup(string);
	if (result == NULL) {
	    return "";
	}
	*freeProcPtr = TCL_DYNAMIC;
    }
    return result;
}

/*
 * ----------------------------------------------------------------------
 *
 * MakeLabel --
 *
 *	Converts a floating point tick value to a string representation.
 *
 * Results:
 *	Returns a formatted label in the string buffer.
 *
 * Side Effects:
 *	Formatted tick label will be displayed on the graph.
 *
 * ----------------------------------------------------------------------
 */
static int
MakeLabel(graphPtr, axisPtr, value, string)
    Graph *graphPtr;		/* Graph widget */
    Axis *axisPtr;		/* Axis structure */
    double value;		/* */
    char string[];		/* string (length is always
				 * TCL_DOUBLE_SPACE+1) containing the
				 * formatted label */
{
    if (axisPtr->logScale) {
	sprintf(string, "1E%d", BLT_RND(value));
    } else {
	if (axisPtr->formatCmd == NULL) {
	    sprintf(string, "%.*g", NUMDIGITS, value);
	} else {
	    Tcl_PrintDouble(axisPtr->interp, value, string);
	}
    }
    if (axisPtr->formatCmd != NULL) {
	Tcl_ResetResult(axisPtr->interp);
	if (Tcl_VarEval(axisPtr->interp, axisPtr->formatCmd, " ",
		Tk_PathName(graphPtr->tkwin), " ",
		string, (char *)NULL) != TCL_OK) {
	    Tk_BackgroundError(axisPtr->interp);
	} else {
	    char *result;

	    result = axisPtr->interp->result;
	    if (*result != '\0') {
		strncpy(string, result, TCL_DOUBLE_SPACE);
		string[TCL_DOUBLE_SPACE] = 0;
		Tcl_ResetResult(axisPtr->interp);
	    }
	}
    }
    return TCL_OK;
}

/* Map graph coordinate to normalized coordinates (consider log scale) */
static double
Scale(axisPtr, x)
    Axis *axisPtr;
    double x;
{
    if (x == Blt_posInfinity) {
	return (1.0);
    } else if (x == Blt_negInfinity) {
	return (0.0);
    }
    if (axisPtr->logScale) {
	if (x > 0.0) {
	    x = log10(x);
	} else if (x < 0.0) {
	    x = 0.0;
	}
    }
    return (NORM(axisPtr, x));
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_InvTransform --
 *
 *	Maps the given window y-coordinate back to a graph coordinate
 *	value. Called by the graph locater routine.
 *
 * Results:
 *	Returns the graph coordinate value at the given window
 *	y-coordinate.
 *
 * ----------------------------------------------------------------------
 */
double
Blt_InvTransform(axis, coord)
    GraphAxis *axis;
    int coord;
{
    double norm, value;
    Axis *axisPtr = (Axis *)axis;

    if (HORIZONTAL_AXIS(axisPtr)) {
	coord = (coord - axisPtr->offset);
    } else {
	coord = (axisPtr->offset - coord);
    }
    norm = ((double)coord / axisPtr->scale);
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    value = (norm * axisPtr->range) + axisPtr->min;
    if (axisPtr->logScale) {
	value = BLT_EXP10(value);
    }
    return (value);
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_Transform --
 *
 *	Map the given graph coordinate value to its axis, returning a
 *	window position.
 *
 * Results:
 *	Returns the window coordinate position on the given axis.
 *
 * Note:
 *	Since line and polygon clipping is performed by the X server,
 *	we must be careful about coordinates which are outside of the
 *      range of a signed short int.
 *
 * ----------------------------------------------------------------------
 */
int
Blt_Transform(axis, value)
    GraphAxis *axis;
    double value;
{
    Axis *axisPtr = (Axis *)axis;
    double norm;
    int coord;

    norm = Scale(axisPtr, value);
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    coord = HORIZONTAL_AXIS(axisPtr)
	? MAPX(axisPtr, norm) : MAPY(axisPtr, norm);

    /* Should really figure out a good offset value and test for that
     * because we could still generate bogus numbers */
    if (coord >= SHRT_MAX) {
	coord = SHRT_MAX - 1000;
    } else if (coord <= SHRT_MIN) {
	coord = SHRT_MIN + 1000;
    }
    return (coord);
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_TransformPt --
 *
 *	Maps the given graph x,y coordinate values to a window position.
 *
 * Results:
 *	Returns a XPoint structure containing the window coordinates of
 *	the given graph x,y coordinate.
 *
 * ----------------------------------------------------------------------
 */
XPoint
Blt_TransformPt(graphPtr, x, y, axisFlags)
    Graph *graphPtr;
    double x, y;
    unsigned int axisFlags;	/* Specifies which axes to use */
{
    XPoint winPos;
    enum AxisTypes axisType;

    axisType = (axisFlags & X1_AXIS_MASK) ? X1_AXIS : X1_AXIS;
    winPos.x = Blt_Transform(graphPtr->axisArr[axisType], x);
    axisType = (axisFlags & Y1_AXIS_MASK) ? Y1_AXIS : Y2_AXIS;
    winPos.y = Blt_Transform(graphPtr->axisArr[axisType], y);
    if (graphPtr->inverted) {	/* Swap x and y coordinates */
	int coord;

	coord = winPos.y;
	winPos.y = winPos.x;
	winPos.x = coord;
    }
    return (winPos);
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_TransformDist --
 *
 *	Map the given graph x-coordinate value to a window position.
 *
 * Results:
 *	Returns the window coordinate position at the given graph
 *	x-coordinate.
 *
 * Note:
 *	Since line and polygon clipping is performed by the X server,
 *	we must be careful about coordinates which are outside of the
 *      range of a signed short int.
 *
 * ----------------------------------------------------------------------
 */
int
Blt_TransformDist(axis, value)
    GraphAxis *axis;
    double value;
{
    Axis *axisPtr = (Axis *)axis;
    double norm;
    int zero, coord;

    norm = Scale(axisPtr, 0.0);
    zero = HORIZONTAL_AXIS(axisPtr)
	? MAPX(axisPtr, norm) : MAPY(axisPtr, norm);
    norm = Scale(axisPtr, value);
    coord = HORIZONTAL_AXIS(axisPtr)
	? MAPX(axisPtr, norm) : MAPY(axisPtr, norm);
    coord -= zero;
    return (BLT_ABS(coord));
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_PointOnGraph --
 *
 *	Determines if the window coordinates given represent a point
 *	on the graph (within the bounds of the graph axes).
 *
 * Results:
 *	Returns 1 is the point is within the bounds of the graph,
 *	0 otherwise.
 *
 * ----------------------------------------------------------------------
 */
int
Blt_PointOnGraph(graphPtr, pointPtr)
    Graph *graphPtr;
    XPoint *pointPtr;
{
    double norm;
    Axis *axisPtr;

    /*
     * It doesn't make a difference which x-axis or y-axis we use
     * (the point is referenced by window coordinates). We're just
     * checking if the point's within the range of axis' normalized
     * coordinates [0..1].
     */
    axisPtr = (Axis *)graphPtr->bottomAxis;
    if (axisPtr->scale == 0.0) {
	return 0;		/* Axis layout hasn't been calculated yet */
    }
    norm = (pointPtr->x - axisPtr->offset) / axisPtr->scale;
    if ((norm < 0.0) || (norm > 1.0)) {
	return 0;		/* x-coordinates are off the graph */
    }
    axisPtr = (Axis *)graphPtr->leftAxis;
    norm = (axisPtr->offset - pointPtr->y) / axisPtr->scale;
    return ((norm >= 0.0) && (norm <= 1.0));
}

/*
 * ----------------------------------------------------------------------
 *
 * UpdateLimits --
 *
 *	Updates the min and max values for each axis as determined by
 *	the data elements currently to be displayed.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Minimum, maximum data limit fields for both the X and Y axes
 *	in the graph widget record are updated.
 *
 * ----------------------------------------------------------------------
 */
static void
UpdateLimits(graphPtr, axisPtr)
    Graph *graphPtr;
    Axis *axisPtr;
{
    int result;

    if ((!AXIS_CONFIG_MAX_SET(axisPtr)) || (!AXIS_CONFIG_MIN_SET(axisPtr))) {
	register Element *elemPtr;
	Blt_ListEntry *entryPtr;
	register double min, max;
	double elemMin, elemMax;
	double value;

	/* Find the minimum and maximum values for all the elements
	   displayed */
	min = Blt_posInfinity, max = Blt_negInfinity;
	for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	    entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	    elemPtr = (Element *)Blt_GetListValue(entryPtr);
	    if ((*elemPtr->limitsProc) (graphPtr, elemPtr,
		    (GraphAxis *)axisPtr, &elemMin, &elemMax) > 0) {
		if (min > elemMin) {
		    min = elemMin;
		}
		if (max < elemMax) {
		    max = elemMax;
		}
	    }
	}
	/*
	 * When auto-scaling, the axis limits are the bounds of the
	 * element data.  If no data exists, set arbitrary limits (wrt
	 * to log/linear scale).
	 */
	if (min == Blt_posInfinity) {
	    min = (axisPtr->logScale) ? 0.001 : -10.0;
	}
	if (max == Blt_negInfinity) {
	    max = 10.0;
	}
	/*
	 * Handle situations where only one limit is set.
	 */
	value = min;
	if (AXIS_CONFIG_MIN_SET(axisPtr)) {
	    min = value = axisPtr->limits[LMIN];
	} else if (AXIS_CONFIG_MAX_SET(axisPtr)) {
	    max = value = axisPtr->limits[LMAX];
	}
	/* If there's no range of data (min >= max), manufacture one */
	if (min >= max) {
	    if (value == 0.0) {
		min = -0.1, max = 0.1;
	    } else {
		double x;

		x = BLT_FABS(value) * 0.1;
		min = value - x;
		max = value + x;
	    }
	}
	if (!AXIS_CONFIG_MIN_SET(axisPtr)) {
	    axisPtr->limits[LMIN] = min;
	}
	if (!AXIS_CONFIG_MAX_SET(axisPtr)) {
	    axisPtr->limits[LMAX] = max;
	}
    }
    /* Indicate if the axis limits have changed */
    result = (axisPtr->limits[LMAX] != axisPtr->prevMax) ||
	(axisPtr->limits[LMIN] != axisPtr->prevMin);
    /* and save the previous minimum and maximum values */
    axisPtr->prevMin = axisPtr->limits[LMIN];
    axisPtr->prevMax = axisPtr->limits[LMAX];
    if (result) {
	axisPtr->flags |= AXIS_CONFIG_DIRTY;
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * NiceNum --
 *
 * 	Taken from Paul Heckbert's "Nice Numbers for Graph Labels" in
 *	Graphics Gems (pp 61-63).  Finds a "nice" number approximately
 *	equal to x.  Round the number if round=1, take ceiling if round=0.
 *
 * ----------------------------------------------------------------------
 */
static double
NiceNum(x, round)
    double x;
    int round;
{
    double exponX;		/* exponent of x */
    double fractX;		/* fractional part of x */
    double nf;			/* nice, rounded fraction */

    exponX = floor(log10(x));
    fractX = x / BLT_EXP10(exponX);	/* between 1 and 10 */
    if (round) {
	if (fractX < 1.5) {
	    nf = 1.;
	} else if (fractX < 3.0) {
	    nf = 2.;
	} else if (fractX < 7.0) {
	    nf = 5.;
	} else {
	    nf = 10.;
	}
    } else if (fractX <= 1.0) {
	nf = 1.;
    } else if (fractX <= 2.0) {
	nf = 2.;
    } else if (fractX <= 5.0) {
	nf = 5.0;
    } else {
	nf = 10.0;
    }
    return (nf * BLT_EXP10(exponX));
}

/*
 * ----------------------------------------------------------------------
 *
 * LogAxis --
 *
 * 	Determine the range and units of a log scaled axis.
 *
 * 	Unless the axis limits are specified, the axis is scaled
 * 	automatically, where the smallest and largest major ticks
 * 	encompass the range of actual data values.  When an axis
 * 	limit is specified, that value represents the
 * 	smallest(min)/largest(max) value in the displayed range of
 * 	values.
 *
 * 	Both manual and automatic scaling are affected by the
 * 	step used.  By default, the step is the largest
 * 	power of ten to divide the range in more than one piece.
 *
 * 	Automatic scaling:
 *   	Find the smallest number of units which contain the range of
 *   	values.  The minimum and maximum major tick values will be
 *   	represent the range of values for the axis. This greatest
 *   	number of major ticks possible is 10.
 *
 * 	Manual scaling:
 *   	Make the minimum and maximum data values the represent the
 *   	range of the values for the axis.  The minimum and maximum
 *   	major ticks will be inclusive of this range.  This provides
 *   	the largest area for plotting and the expected results when
 *   	the axis min and max values have be set by the user (.e.g zooming).
 *   	The maximum number of major ticks is 20.
 *
 *   	For log scale, there is always the possibility that the minimum
 *   	and maximum data values are the same magnitude.  To represent
 *   	the points properly, at least one full decade should be shown.
 *   	However, if you zoom a log scale plot, the results should be
 *   	predictable. Therefore, in that case, show only minor ticks.
 *   	Lastly, there should be an appropriate way to handle numbers <=0.
 *
 *          maxY
 *            |    units = magnitude (of least significant digit)
 *            |    high  = largest unit tick < max axis value
 *      high _|    low   = smallest unit tick > min axis value
 *            |
 *            |    range = high - low
 *            |    # ticks = greatest factor of range/units
 *           _|
 *        U   |
 *        n   |
 *        i   |
 *        t  _|
 *            |
 *            |
 *            |
 *       low _|
 *            |
 *            |_minX________________maxX__
 *            |   |       |      |       |
 *     minY  low                        high
 *           minY
 *
 *
 * 	numTicks = Number of ticks
 * 	min = Minimum value of axis
 * 	max = Maximum value of axis
 * 	range    = Range of values (max - min)
 *
 * 	If the number of decades is greater than ten, it is assumed
 *	that the full set of log-style ticks can't be drawn properly.
 *
 * Results:
 *	None
 *
 * ----------------------------------------------------------------------
 */
static void
LogAxis(axisPtr)
    Axis *axisPtr;
{
    double range;
    double min, max;

    min = axisPtr->limits[LMIN];
    max = axisPtr->limits[LMAX];

    if (min > 0.0) {
	min = floor(log10(min));
    } else {
	min = 0.0;
    }
    if (max > 0.0) {
	max = ceil(log10(max));
    } else {
	max = 1.0;
    }
    range = max - min;
    if (range > 10) {
	range = NiceNum(range, 0);
	axisPtr->step = NiceNum(range / (NTICK - 1), 1);

	/* Find the outer limits in terms of the step. */
	min = UFLOOR(min, axisPtr->step);
	max = UCEIL(max, axisPtr->step);
	axisPtr->numTicks = (int)((max - min) / axisPtr->step) + 1;
	axisPtr->subStep = BLT_EXP10(floor(log10(axisPtr->step)));

	if (axisPtr->step == axisPtr->subStep) {
	    axisPtr->subTicks = 5;
	    axisPtr->subStep = axisPtr->step * 0.2;
	} else {
	    axisPtr->subTicks = BLT_RND(axisPtr->step / axisPtr->subStep);
	}
    } else {
	if (min == max) {
	    max++;
	}
	axisPtr->numTicks = (int)((max - min) + 1);
	axisPtr->step = 1.0;
	axisPtr->subTicks = 10;
    }
    axisPtr->min = axisPtr->tickMin = min;
    axisPtr->max = axisPtr->tickMax = max;
    axisPtr->range = (max - min);
#ifdef notdef
    fprintf(stderr, "Major: %s\nRegion min=%g,max=%g\nTick min=%g,max=%g\n\
numTicks=%d, range=%g, step=%.15g\n", axisNames[axisPtr->type], min, max,
	axisPtr->tickMin, axisPtr->tickMax, axisPtr->numTicks,
	axisPtr->range, axisPtr->step);
    fprintf(stderr, "Minor numTicks=%d, step=%.15g\n\n",
	axisPtr->subTicks, axisPtr->subStep);
#endif
}

/*
 * ----------------------------------------------------------------------
 *
 * LinearAxis --
 *
 * 	Determine the units of a linear scaled axis.
 *
 * 	Unless the axis limits are specified, the axis is scaled
 * 	automatically, where the smallest and largest major ticks
 * 	encompass the range of actual data values.  When an axis
 * 	limit is specified, that value represents the
 * 	smallest(min)/largest(max) value in the displayed range of
 * 	values.
 *
 * 	Both manual and automatic scaling are affected by the
 * 	step used.  By default, the step is the largest
 * 	power of ten to divide the range in more than one piece.
 *
 * 	Automatic scaling:
 *   	Find the smallest number of units which contain the range of
 *   	values.  The minimum and maximum major tick values will be
 *   	represent the range of values for the axis. This greatest
 *   	number of major ticks possible is 10.
 *
 * 	Manual scaling:
 *   	Make the minimum and maximum data values the represent the
 *   	range of the values for the axis.  The minimum and maximum
 *   	major ticks will be inclusive of this range.  This provides
 *   	the largest area for plotting and the expected results when
 *   	the axis min and max values have be set by the user (.e.g zooming).
 *   	The maximum number of major ticks is 20.
 *
 *   	For log scale, there is always the possibility that the minimum
 *   	and maximum data values are the same magnitude.  To represent
 *   	the points properly, at least one full decade should be shown.
 *   	However, if you zoom a log scale plot, the results should be
 *   	predictable. Therefore, in that case, show only minor ticks.
 *   	Lastly, there should be an appropriate way to handle numbers <=0.
 *
 *          maxY
 *            |    units = magnitude (of least significant digit)
 *            |    high  = largest unit tick < max axis value
 *      high _|    low   = smallest unit tick > min axis value
 *            |
 *            |    range = high - low
 *            |    # ticks = greatest factor of range/units
 *           _|
 *        U   |
 *        n   |
 *        i   |
 *        t  _|
 *            |
 *            |
 *            |
 *       low _|
 *            |
 *            |_minX________________maxX__
 *            |   |       |      |       |
 *     minY  low                        high
 *           minY
 *
 *
 * 	numTicks = Number of ticks
 * 	min = Minimum value of axis
 * 	max = Maximum value of axis
 * 	range    = Range of values (max - min)
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
LinearAxis(axisPtr)
    Axis *axisPtr;
{
    double range, unit, pad;
    double min, max;

    min = axisPtr->limits[LMIN];
    max = axisPtr->limits[LMAX];

    /*
     * Calculate the major step.
     */
    range = max - min;
    if ((axisPtr->reqStep > 0.0) && (axisPtr->reqStep < range)) {
	axisPtr->step = axisPtr->reqStep;
    } else {
	range = NiceNum(range, 0);
	axisPtr->step = NiceNum(range / (NTICK - 1), 1);
    }

    /*
     * Find the outer tick values in terms of the major step interval.
     * Add +0.0 to preclude the possibility of an IEEE -0.0.
     */

    axisPtr->tickMin = UFLOOR(min, axisPtr->step) + 0.0;
    axisPtr->tickMax = UCEIL(max, axisPtr->step) + 0.0;
    range = axisPtr->tickMax - axisPtr->tickMin;
    unit = range / axisPtr->step;
    axisPtr->numTicks = BLT_RND(unit) + 1;

    /*
     * If the axis is "loose", the range is between the two outermost
     * ticks. Otherwise if it's "tight", the range is between the data
     * min and max.
     */
    if (axisPtr->loose) {
	axisPtr->min = axisPtr->tickMin, axisPtr->max = axisPtr->tickMax;
    } else {
	axisPtr->min = min, axisPtr->max = max;
    }

    /*
     * If is a limit is auto-scaled, add some padding so that the
     * symbols representing data points at the extremes aren't clipped
     * in half by the edge of the plot.  Two percent is an arbitrary
     * guess.
     */

    pad = (axisPtr->max - axisPtr->min) * 0.02;
    if (!AXIS_CONFIG_MIN_SET(axisPtr)) {
	axisPtr->min -= pad;
    }
    if (!AXIS_CONFIG_MAX_SET(axisPtr)) {
	axisPtr->max += pad;
    }
    axisPtr->range = axisPtr->max - axisPtr->min;

#ifdef notdef
    fprintf(stderr, "Major: %s\nRegion min=%g,max=%g\nTick min=%g,max=%g\n\
numTicks=%d, range=%g, step=%.15g\n", axisNames[axisPtr->type], min, max,
	axisPtr->tickMin, axisPtr->tickMax, axisPtr->numTicks,
	axisPtr->range, axisPtr->step);
#endif

    /* Now calculate the minor tick step and number. */
    axisPtr->subTicks = axisPtr->reqSubTicks;
    if (axisPtr->subTicks < 0) {
	axisPtr->subTicks = 0;
    }
    if (axisPtr->subTicks > 0) {
	axisPtr->subStep = axisPtr->step / axisPtr->subTicks;
    } else {
	axisPtr->subStep = axisPtr->step * 0.2;	/* Need this for layout */
    }
#ifdef notdef
    fprintf(stderr, "Minor numTicks=%d, step=%.15g\n\n",
	axisPtr->subTicks, axisPtr->subStep);
#endif
}

/*
 * -----------------------------------------------------------------
 *
 * SetAxisLimits  --
 *
 * -----------------------------------------------------------------
 */
static void
SetAxisLimits(graphPtr, axisPtr)
    Graph *graphPtr;
    Axis *axisPtr;
{
    UpdateLimits(graphPtr, axisPtr);

    /*
     * For barcharts, adjust the min or max values to include 0.0
     * if the bars are drawn along this axis.
     */
    if ((graphPtr->type == BARCHART) && (Y_AXIS(axisPtr))) {
	if (!AXIS_CONFIG_MIN_SET(axisPtr) && (axisPtr->limits[LMIN] > 0.0)) {
	    axisPtr->limits[LMIN] = 0.0;
	}
	if (!AXIS_CONFIG_MAX_SET(axisPtr) && (axisPtr->limits[LMAX] < 0.0)) {
	    axisPtr->limits[LMAX] = 0.0;
	}
    }
    if (axisPtr->flags & AXIS_CONFIG_DIRTY) {
	/* Calculate min/max tick (major/minor) layouts */
	if (axisPtr->logScale) {
	    LogAxis(axisPtr);
	} else {
	    LinearAxis(axisPtr);
	}
	axisPtr->flags &= ~AXIS_CONFIG_DIRTY;

	/* When any axis changes, we need to layout the entire graph. */
	graphPtr->flags |= (LAYOUT_ALL | DIRTY | REFRESH);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_ComputeAxes --
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
void
Blt_ComputeAxes(graphPtr)
    Graph *graphPtr;
{
    register int i;

    for (i = 0; i < 4; i++) {
	SetAxisLimits(graphPtr, (Axis *)graphPtr->axisArr[i]);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureAxis --
 *
 *	Configures axis attributes (font, line width, label, etc) and
 *	allocates a new (possibly shared) graphics context.  Line cap
 *	style is projecting.  This is for the problem of when a tick
 *	sits directly at the end point of the axis.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Axis resources are allocated (GC, font). Axis layout is
 *	deferred until the height and width of the window are known.
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureAxis(graphPtr, axisPtr, argc, argv, flags)
    Graph *graphPtr;
    Axis *axisPtr;
    int argc;
    char *argv[];
    int flags;
{
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;
    Tk_ConfigSpec *configSpecs;

    configSpecs = axisConfigSpecs[axisPtr->type];
    if (flags & TK_CONFIG_ARGV_ONLY) {
	if (argc == 0) {
	    return (Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin,
		    configSpecs, (char *)axisPtr, (char *)NULL, flags));
	} else if (argc == 1) {
	    return (Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin,
		    configSpecs, (char *)axisPtr, argv[0], flags));
	}
    }
    if (Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin, configSpecs,
	    argc, argv, (char *)axisPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Check requested X and Y axis limits. Can't allow min to be
     * greater than max, or have undefined log scale limits.
     */
    if ((AXIS_CONFIG_MIN_SET(axisPtr)) && (AXIS_CONFIG_MAX_SET(axisPtr)) &&
	(axisPtr->limits[LMIN] >= axisPtr->limits[LMAX])) {
	sprintf(graphPtr->interp->result,
	    "impossible %s-axis limits (min %g >= max %g)",
	    axisNames[axisPtr->type], axisPtr->limits[LMIN],
	    axisPtr->limits[LMAX]);
	return TCL_ERROR;
    }
    if ((axisPtr->logScale) && (AXIS_CONFIG_MIN_SET(axisPtr)) &&
	(axisPtr->limits[LMIN] <= 0.0)) {
	sprintf(graphPtr->interp->result,
	    "invalid %s-axis limits (min=%g,max=%g) for log scale",
	    axisNames[axisPtr->type], axisPtr->limits[LMIN],
	    axisPtr->limits[LMAX]);
	return TCL_ERROR;
    }
    /*
     * Reset bogus line widths to zero. Can't allow bad line widths
     * because the layout routines compute axis and tick positions
     * with them.
     */
    if (axisPtr->lineWidth < 1) {
	axisPtr->lineWidth = 0;
    }
    /*
     * Create an unshared GC for the tick labels. The GC is private
     * because the labels may be rotated, requiring the GCStipple and
     * GCTSOffset fields to change.
     */
    gcMask = GCForeground | GCFont;
    gcValues.font = axisPtr->fontPtr->fid;
    gcValues.foreground = axisPtr->fgColorPtr->pixel;
    if (graphPtr->border != NULL) {
	gcValues.background = Tk_3DBorderColor(graphPtr->border)->pixel;
	gcMask |= GCBackground;
    }
    newGC = XCreateGC(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	gcMask, &gcValues);
    if (axisPtr->textGC != NULL) {
	XFreeGC(graphPtr->display, axisPtr->textGC);
    }
    axisPtr->textGC = newGC;

    /* Create GC for axis line and ticks. */

    gcMask = GCForeground | GCLineWidth | GCCapStyle;
    gcValues.line_width = axisPtr->lineWidth;
    gcValues.cap_style = CapProjecting;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (axisPtr->lineGC != NULL) {
	Tk_FreeGC(graphPtr->display, axisPtr->lineGC);
    }
    axisPtr->lineGC = newGC;

    /*
     * Don't bother to check what options have changed.  Almost every
     * axis configuration option changes the size of the plotting area
     * (except for -foreground).
     */
    graphPtr->flags |= LAYOUT_ALL;
    axisPtr->flags |= AXIS_CONFIG_DIRTY;
    SetAxisLimits(graphPtr, axisPtr);
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyAxis --
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources (font, color, gc, labels, etc.) associated with the
 *	axis are deallocated.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyAxis(graphPtr, axis)
    Graph *graphPtr;
    GraphAxis *axis;
{
    Axis *axisPtr = (Axis *)axis;

    Tk_FreeOptions(axisConfigSpecs[axisPtr->type], (char *)axisPtr,
	graphPtr->display, 0);

    if (axisPtr->lineGC != NULL) {
	Tk_FreeGC(graphPtr->display, axisPtr->lineGC);
    }
    if (axisPtr->textGC != NULL) {
	XFreeGC(graphPtr->display, axisPtr->textGC);
    }
    if (axisPtr->labelArr != NULL) {
	free((char *)axisPtr->labelArr);
    }
    if (axisPtr->segArr != NULL) {
	free((char *)axisPtr->segArr);
    }
    free((char *)axisPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * CalculateOffsets --
 *
 *	Determines the placements of the axis, major and minor ticks,
 *	and title of the axis.
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
CalculateOffsets(graphPtr, axisPtr)
    Graph *graphPtr;
    Axis *axisPtr;
{
    int pad;			/* Offset of axis from interior region. This
				 * includes a possible border and the axis
				 * line width. */
    unsigned int textHeight;
    int innerPos, outerPos;
    int majorOffset, minorOffset, labelOffset, titleOffset;

    textHeight = TEXTHEIGHT(graphPtr->fontPtr);

    titleOffset = graphPtr->borderWidth + textHeight;
    majorOffset = BLT_ABS(axisPtr->tickLength);
    minorOffset = BLT_RND(majorOffset * 0.5);
    labelOffset = BLT_RND(majorOffset * 1.4) + axisPtr->lineWidth / 2;

    /* Adjust offset for the interior border width and the line width */
    pad = graphPtr->plotBW + axisPtr->lineWidth + 2;
    if (graphPtr->plotBW > 0) {
	pad++;
    }
    if ((axisPtr->location == LEFT_AXIS) || (axisPtr->location == TOP_AXIS)) {
	majorOffset = -majorOffset;
	minorOffset = -minorOffset;
	labelOffset = -labelOffset;
    }
    /*
     * Pre-calculate the x-coordinate positions of the axis,
     * tick labels, and the individual major and minor ticks.
     */
    switch (axisPtr->location) {
    case BOTTOM_AXIS:
	innerPos = graphPtr->origin.y + pad;
	axisPtr->titlePos.x = (graphPtr->extreme.x + graphPtr->origin.x) / 2;
	axisPtr->titlePos.y = graphPtr->height - titleOffset;
	axisPtr->anchor = TK_ANCHOR_N;
	break;

    case LEFT_AXIS:
	innerPos = graphPtr->origin.x - pad;
	axisPtr->titlePos.x = titleOffset;
	axisPtr->titlePos.y = (graphPtr->origin.y + graphPtr->extreme.y) / 2;
	axisPtr->anchor = TK_ANCHOR_E;
	break;

    case TOP_AXIS:
	innerPos = graphPtr->extreme.y - pad;
	axisPtr->titlePos.x = (graphPtr->extreme.x + graphPtr->origin.x) / 2;
	axisPtr->titlePos.y = titleOffset;
	if (graphPtr->title != NULL) {
	    axisPtr->titlePos.y += (2 * textHeight);
	} else {
	    axisPtr->titlePos.y += (textHeight / 2);
	}
	axisPtr->anchor = TK_ANCHOR_S;
	break;

    case RIGHT_AXIS:
	innerPos = graphPtr->extreme.x + pad;
	axisPtr->titlePos.x = graphPtr->width -
	    (graphPtr->legendPtr->width + titleOffset);
	axisPtr->titlePos.y = (graphPtr->origin.y + graphPtr->extreme.y) / 2;
	axisPtr->anchor = TK_ANCHOR_W;
	break;

    }
    outerPos = innerPos + majorOffset;
    axisPtr->posArr[MAJOR_TICK] = outerPos;
    axisPtr->posArr[AXIS_LINE] = innerPos;
    axisPtr->posArr[MINOR_TICK] = innerPos + minorOffset;
    axisPtr->posArr[TICK_LABEL] = innerPos + labelOffset;
    if (axisPtr->tickLength < 0) {
	axisPtr->posArr[MAJOR_TICK] = innerPos;
	axisPtr->posArr[AXIS_LINE] = outerPos;
    }
}

static XSegment
AxisLine(axisPtr, min, max)
    Axis *axisPtr;		/* Axis information */
    double min, max;		/* Limits of axis in graph coordinates */
{
    double normMin, normMax;
    XSegment segment;

    normMax = NORM(axisPtr, min);
    if (axisPtr->descending) {
	normMax = 1.0 - normMax;
    }
    normMin = NORM(axisPtr, max);
    if (axisPtr->descending) {
	normMin = 1.0 - normMin;
    }
    if (HORIZONTAL_AXIS(axisPtr)) {
	segment.y1 = segment.y2 = axisPtr->posArr[AXIS_LINE];
	segment.x1 = MAPX(axisPtr, normMin);
	segment.x2 = MAPX(axisPtr, normMax);
    } else {
	segment.x1 = segment.x2 = axisPtr->posArr[AXIS_LINE];
	segment.y1 = MAPY(axisPtr, normMin);
	segment.y2 = MAPY(axisPtr, normMax);
    }
    return (segment);
}


static XSegment
Tick(axisPtr, value, whichTick)
    Axis *axisPtr;
    double value;
    int whichTick;		/* If non-zero, create minor tick instead */
{
    double norm;
    XSegment segment;
    int tick;

    norm = NORM(axisPtr, value);
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    tick = axisPtr->posArr[whichTick];
    if (HORIZONTAL_AXIS(axisPtr)) {
	segment.y1 = axisPtr->posArr[AXIS_LINE];
	segment.y2 = tick;
	segment.x1 = segment.x2 = MAPX(axisPtr, norm);
    } else {
	segment.x1 = axisPtr->posArr[AXIS_LINE];
	segment.x2 = tick;
	segment.y1 = segment.y2 = MAPY(axisPtr, norm);
    }
    return (segment);
}

/*
 * -----------------------------------------------------------------
 *
 * LayoutAxis --
 *
 *	Pre-calculate the x-coordinate positions of the axis, ticks
 *	and labels to be used later when displaying the X axis.  Ticks
 *	(minor and major) will be saved in an array of XSegments so
 *	that they can be drawn in one XDrawSegments call. The strings
 *	representing the tick labels and the corresponding window
 *	positions are saved in an array of Label's.
 *
 *      Calculates the values for each major and minor tick and checks to
 *	see if they are in range (the outer ticks may be outside of the
 *	range of plotted values).
 *
 * Results:
 *	None.
 *
 * SideEffects:
 *	Line segments and tick labels saved will be used to draw
 *	the X axis.
 * ----------------------------------------------------------------- */
static void
LayoutAxis(graphPtr, axis)
    Graph *graphPtr;
    GraphAxis *axis;
{
    Axis *axisPtr = (Axis *)axis;
    XSegment *segArr;
    unsigned int arraySize;
    double min, max;
    double value, subValue;
    register int i, j;
    register int sgmts, labels;
    double epsilon;
    static float logTable[] =	/* Precomputed log10 values [1..10] */
    {
	0.0, 0.301, 0.477, 0.602, 0.699, 0.778, 0.845, 0.903, 0.954, 1.0
    };

    CalculateOffsets(graphPtr, axisPtr);

    /* Save all line coordinates in an array of line segments. */

    arraySize = (1 + (axisPtr->numTicks * (axisPtr->subTicks + 1)));
    segArr = (XSegment *)malloc(arraySize * sizeof(XSegment));
    if (segArr == NULL) {
	return;			/* Can't allocate array of segments */
    }
    if ((axisPtr->logScale) || (axisPtr->loose) ||
	(axisPtr->limits[LMIN] == axisPtr->limits[LMAX])) {
	min = axisPtr->tickMin, max = axisPtr->tickMax;
    } else {
	min = axisPtr->limits[LMIN];
	max = axisPtr->limits[LMAX];
    }

    /* Axis baseline */
    segArr[0] = AxisLine(axisPtr, min, max);

    sgmts = 1, labels = 0;
    if (!axisPtr->showTicks) {
	goto done;		/* Only display axis line */
    }
    /* Use numbers just beyond the limits when testing for equality */
    epsilon = 2 * MIN_DBL_VALUE;
    min -= epsilon, max += epsilon;

    value = axisPtr->tickMin;	/* Start from smallest axis tick */
    for (i = 0; i < axisPtr->numTicks; i++) {
	subValue = value = UROUND(value, axisPtr->step);

	/* Minor ticks */
	for (j = 1; j < axisPtr->subTicks; j++) {
	    if ((axisPtr->logScale) && (axisPtr->step == 1.0)) {
		subValue = value + (double)logTable[j];
	    } else {
		subValue += axisPtr->subStep;
	    }
	    if ((subValue >= min) && (subValue <= max)) {
		segArr[sgmts] = Tick(axisPtr, subValue, MINOR_TICK);
		sgmts++;
	    }
	}

	/* Major tick and label */
	if ((value >= min) && (value <= max)) {
	    short int labelPos;

	    segArr[sgmts] = Tick(axisPtr, value, MAJOR_TICK);
	    labelPos = (short int)axisPtr->posArr[TICK_LABEL];

	    /* Save tick label position */

	    if (HORIZONTAL_AXIS(axisPtr)) {
		axisPtr->labelArr[labels].x = segArr[sgmts].x1;
		axisPtr->labelArr[labels].y = labelPos;
	    } else {
		axisPtr->labelArr[labels].x = labelPos;
		axisPtr->labelArr[labels].y = segArr[sgmts].y1;
	    }
	    sgmts++, labels++;
	}
	value += axisPtr->step;
    }

  done:

    assert(sgmts <= arraySize);
    assert(labels <= axisPtr->numLabels);

    if (axisPtr->segArr != NULL) {
	free((char *)axisPtr->segArr);
    }
    axisPtr->segArr = segArr;
    axisPtr->numSegments = sgmts;
}

/*
 * -----------------------------------------------------------------
 *
 * DisplayAxis --
 *
 *	Draws the axis, ticks, and labels onto the canvas.
 *
 *	Initializes and passes text attribute information through
 *	TextAttributes structure.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Axis gets drawn on window.
 *
 * -----------------------------------------------------------------
 */

static float titleRot[4] =	/* Rotation for each axis title */
{
    0.0, 90.0, 0.0, 270.0
};

static void
DisplayAxis(graphPtr, axis, attrPtr)
    Graph *graphPtr;
    GraphAxis *axis;
    TextAttributes *attrPtr;
{
    Axis *axisPtr = (Axis *)axis;

    if (axisPtr->title != NULL) {
	attrPtr->theta = (double)titleRot[axisPtr->location];
	Blt_DrawText(graphPtr->display, graphPtr->canvas, axisPtr->title,
	    attrPtr, axisPtr->titlePos.x, axisPtr->titlePos.y);
    }
    if (axisPtr->showTicks) {
	register int i;
	TextAttributes textAttr;

	/* Setup static text attribute information */

	textAttr.theta = axisPtr->theta;
	textAttr.anchor = axisPtr->anchor;
	textAttr.fontPtr = axisPtr->fontPtr;
	textAttr.fgColorPtr = axisPtr->fgColorPtr;
	textAttr.bgColorPtr = Tk_3DBorderColor(graphPtr->border);
	textAttr.gc = axisPtr->textGC;

	/* Draw the ticks labels and then the ticks and axis */
	for (i = 0; i < axisPtr->numLabels; i++) {
	    Blt_DrawText(graphPtr->display, graphPtr->canvas,
		axisPtr->labelArr[i].text, &textAttr,
		axisPtr->labelArr[i].x, axisPtr->labelArr[i].y);
	}
    }
    if (axisPtr->numSegments > 0) {
	XDrawSegments(graphPtr->display, graphPtr->canvas, axisPtr->lineGC,
	    axisPtr->segArr, axisPtr->numSegments);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * PrintAxis --
 *
 *	Generates PostScript output to draw the axis, ticks, and
 *	labels.
 *
 *	Initializes and passes text attribute information through
 *	TextAttributes structure.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	PostScript output is left in graphPtr->interp->result;
 *
 * -----------------------------------------------------------------
 */
static void
PrintAxis(graphPtr, axis, attrPtr)
    Graph *graphPtr;
    GraphAxis *axis;
    TextAttributes *attrPtr;
{
    Axis *axisPtr = (Axis *)axis;

    if (axisPtr->title != NULL) {
	attrPtr->theta = (double)titleRot[axisPtr->location];
	Blt_TextToPostScript(graphPtr, axisPtr->title, attrPtr,
	    axisPtr->titlePos.x, axisPtr->titlePos.y);
    }
    if (axisPtr->showTicks) {
	TextAttributes textAttr;
	register int i;

	/* Setup static text attribute information */

	textAttr.theta = axisPtr->theta;
	textAttr.anchor = axisPtr->anchor;
	textAttr.fontPtr = axisPtr->fontPtr;
	textAttr.fgColorPtr = axisPtr->fgColorPtr;
	textAttr.bgColorPtr = (XColor *)NULL;

	for (i = 0; i < axisPtr->numLabels; i++) {
	    Blt_TextToPostScript(graphPtr, axisPtr->labelArr[i].text,
		&textAttr, axisPtr->labelArr[i].x, axisPtr->labelArr[i].y);
	}
    }
    if (axisPtr->numSegments > 0) {
	Blt_SetLineAttributes(graphPtr, axisPtr->fgColorPtr,
	    axisPtr->lineWidth, 0);
	Blt_SegmentsToPostScript(graphPtr, axisPtr->segArr,
	    axisPtr->numSegments);
    }
}

static void
GetAxisGeometry(graphPtr, axisPtr)
    Graph *graphPtr;
    Axis *axisPtr;
{
    register int i;
    register int count;
    char label[TCL_DOUBLE_SPACE];
    unsigned int length;
    unsigned int arraySize, poolSize, used;
    char *pool;
    unsigned int textWidth, textHeight;
    unsigned int bbWidth, bbHeight;
    int maxWidth, maxHeight;
    double value;
    double epsilon, minAxis, maxAxis;
    Label *labelArr;
    int pad;

    if ((axisPtr->logScale) || (axisPtr->loose) ||
	(axisPtr->limits[LMIN] == axisPtr->limits[LMAX])) {
	minAxis = axisPtr->tickMin, maxAxis = axisPtr->tickMax;
    } else {
	minAxis = axisPtr->limits[LMIN];
	maxAxis = axisPtr->limits[LMAX];
    }

    /* Use numbers just beyond the limits when testing for equality */

    epsilon = 2 * MIN_DBL_VALUE;
    minAxis -= epsilon, maxAxis += epsilon;

    /* Create an array of labels with an attached of characters strings */
    arraySize = axisPtr->numTicks * sizeof(Label);
    poolSize = axisPtr->numTicks * AVG_TICK_NUM_CHARS * sizeof(char);
    labelArr = (Label *)malloc(arraySize + poolSize);
    pool = (char *)labelArr + arraySize;

    textHeight = TEXTHEIGHT(axisPtr->fontPtr);

    maxHeight = maxWidth = 0;
    used = count = 0;
    value = axisPtr->tickMin;
    for (i = 0; i < axisPtr->numTicks; i++, value += axisPtr->step) {
	value = UROUND(value, axisPtr->step);
	if ((value < minAxis) || (value > maxAxis)) {
	    continue;		/* Out of range */
	}
	MakeLabel(graphPtr, axisPtr, value, label);
	length = strlen(label);

	/*  Resize the label array if we overflow its string pool */
	if (poolSize <= (used + length)) {
	    int newSize;
	    Label *newArr;

	    newSize = poolSize + poolSize;
	    while (newSize <= (used + length)) {
		newSize += newSize;
	    }
	    newArr = (Label *)malloc(arraySize + newSize);
	    memcpy((char *)newArr, (char *)labelArr, arraySize + poolSize);
	    pool = (char *)newArr + arraySize;
	    free((char *)labelArr);
	    poolSize = newSize;
	    labelArr = newArr;
	}
	textWidth = Blt_TextStringWidth(axisPtr->fontPtr, label);
	labelArr[count].text = (pool + used);
	strcpy(labelArr[count].text, label);
	used += (length + 1);
	count++;

	if (axisPtr->theta == 0.0) {
	    bbWidth = textWidth, bbHeight = textHeight;
	} else {
	    Blt_GetBoundingBox(textWidth, textHeight, axisPtr->theta,
		&bbWidth, &bbHeight, (XPoint *)NULL);
	}
	if (bbWidth > maxWidth) {
	    maxWidth = bbWidth;
	}
	if (bbHeight > maxHeight) {
	    maxHeight = bbHeight;
	}
    }
    if (axisPtr->labelArr != NULL) {
	free((char *)axisPtr->labelArr);
    }
    axisPtr->labelArr = labelArr;
    axisPtr->numLabels = count;
    assert(axisPtr->numLabels <= axisPtr->numTicks);

    /*
     * Because the axis cap style is "CapProjecting", there's an extra
     * 1.5 linewidth to be accounted for.
     */
    pad = ((axisPtr->lineWidth * 15) / 10) + 2;
    axisPtr->width = maxWidth + pad;
    axisPtr->height = maxHeight + pad;

    value = BLT_ABS(axisPtr->tickLength) * 1.4;
    pad = BLT_RND(value) + graphPtr->plotBW + 1;
    if (graphPtr->plotBW > 0) {
	pad++;
    }
    if (HORIZONTAL_AXIS(axisPtr)) {
	axisPtr->height += pad;
    } else {
	axisPtr->width += pad;
    }
}

void
Blt_UpdateAxisBackgrounds(graphPtr, colorPtr)
    Graph *graphPtr;
    XColor *colorPtr;		/* Background color of graph margin area */
{
    Axis *axisPtr;
    register int i;

    for (i = 0; i < 4; i++) {
	axisPtr = (Axis *)graphPtr->axisArr[i];
	XSetBackground(Tk_Display(graphPtr->tkwin), axisPtr->textGC,
	    colorPtr->pixel);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_ComputeLayout --
 *
 * 	Calculate the layout of the graph.  Based upon the data,
 *	axis limits, X and Y titles, and title height, determine
 *	the cavity left which is the plotting surface.  The first
 *	step get the data and axis limits for calculating the space
 *	needed for the top, bottom, left, and right margins.
 *
 * 	1) The LEFT margin is the area from the left border to the
 *	   Y axis (not including ticks). It composes the border
 *	   width, the width an optional Y axis label and its padding,
 *	   and the tick numeric labels. The Y axis label is rotated
 *	   90 degrees so that the width is the font height.
 *
 * 	2) The RIGHT margin is the area from the end of the graph
 *	   to the right window border. It composes the border width,
 *	   some padding, the font height (this may be dubious. It
 *	   appears to provide a more even border), the max of the
 *	   legend width and 1/2 max X tick number. This last part is
 *	   so that the last tick label is not clipped.
 *
 *           Area Width
 *      ___________________________________________________________
 *      |          |                               |               |
 *      |          |   TOP  height of title        |               |
 *      |          |                               |               |
 *      |          |           x2 title            |               |
 *      |          |                               |               |
 *      |          |        height of x2-axis      |               |
 *      |__________|_______________________________|_______________|  A
 *      |          |                        extreme|               |  r
 *      |          |                               |               |  e
 *      |   LEFT   |                               |     RIGHT     |  a
 *      |          |                               |               |
 *      | y        |     Free area = 104%          |      y2       |  H
 *      |          |     Plotting surface = 100%   |               |  e
 *      | t        |     Tick length = 2 + 2%      |      t        |  i
 *      | i        |                               |      i        |  g
 *      | t        |                               |      t  legend|  h
 *      | l        |                               |      l   width|  t
 *      | e        |                               |      e        |
 *      |    height|                               |height         |
 *      |       of |                               | of            |
 *      |    y-axis|                               |y2-axis        |
 *      |          |                               |               |
 *      |          |origin                         |               |
 *      |__________|_______________________________|_______________|
 *      |          | (xoffset, yoffset)            |               |
 *      |          |                               |               |
 *      |          |       height of x-axis        |               |
 *      |          |                               |               |
 *      |          |   BOTTOM   x title            |               |
 *      |__________|_______________________________|_______________|
 *
 * 3) The TOP margin is the area from the top window border to the top
 *    of the graph. It composes the border width, twice the height of
 *    the title font (if one is given) and some padding between the
 *    title.
 *
 * 4) The BOTTOM margin is area from the bottom window border to the
 *    X axis (not including ticks). It composes the border width, the height
 *    an optional X axis label and its padding, the height of the font
 *    of the tick labels.
 *
 * The plotting area is between the margins which includes the X and Y axes
 * including the ticks but not the tick numeric labels. The length of
 * the ticks and its padding is 5% of the entire plotting area.  Hence the
 * entire plotting area is scaled as 105% of the width and height of the
 * area.
 *
 * The axis labels, ticks labels, title, and legend may or may not be
 * displayed which must be taken into account.
 *
 *
 * -----------------------------------------------------------------
 */
int
Blt_ComputeLayout(graphPtr)
    Graph *graphPtr;
{
    int left, right, top, bottom;
    int maxTickWidth;
    int height;
    int leftOver;
    unsigned int borderWidths;
    unsigned int lineHeight = TEXTHEIGHT(graphPtr->fontPtr);
    Axis *x1, *x2, *y1, *y2;
    unsigned int twiceHeight = (2 * lineHeight);
    unsigned int halfHeight = (lineHeight / 2);
    double value;

    x1 = (Axis *)graphPtr->bottomAxis;
    x2 = (Axis *)graphPtr->topAxis;
    y1 = (Axis *)graphPtr->leftAxis;
    y2 = (Axis *)graphPtr->rightAxis;

    top = (graphPtr->title != NULL) ? twiceHeight : halfHeight;
    left = ((y1->mapped) && (y1->title != NULL)) ? twiceHeight : halfHeight;
    bottom = ((x1->mapped) && (x1->title != NULL)) ? twiceHeight : halfHeight;
    right = ((y2->mapped) && (y2->title != NULL)) ? twiceHeight : 0;

    if ((x2->mapped) && (x2->title != NULL)) {
	top += twiceHeight;
    }
    GetAxisGeometry(graphPtr, x1);
    maxTickWidth = 0;
    if ((x1->mapped) && (x1->showTicks)) {
	bottom += x1->height;
    }
    GetAxisGeometry(graphPtr, x2);
    if ((x2->mapped) && (x2->showTicks)) {
	top += x2->height;
    }
    GetAxisGeometry(graphPtr, y1);
    if ((y1->mapped) && (y1->showTicks)) {
	left += y1->width + PADX;
    }
    GetAxisGeometry(graphPtr, y2);
    if ((y2->mapped) && (y2->showTicks)) {
	right += y2->width + PADX;
    }
    /* Override calculated values if user specified margins */

    if (graphPtr->leftMargin > 0) {
	left = graphPtr->leftMargin;
    }
    if (graphPtr->topMargin > 0) {
	top = graphPtr->topMargin;
    }
    if (graphPtr->bottomMargin > 0) {
	bottom = graphPtr->bottomMargin;
    }
    borderWidths = graphPtr->borderWidth + graphPtr->plotBW;
    height = graphPtr->height - ((2 * borderWidths) + top + bottom);
    (*graphPtr->legendPtr->geomProc) (graphPtr, height);
    if ((graphPtr->legendPtr->mapped) && (graphPtr->legendPtr->useDefault)) {
	right += graphPtr->legendPtr->width;
    } else {
	right += halfHeight;
    }
    maxTickWidth = BLT_MAX(x1->width, x2->width) / 2;
    if (right < maxTickWidth) {
	right = maxTickWidth;
    }
    if (graphPtr->rightMargin > 0) {
	right = graphPtr->rightMargin;
    }
    top += borderWidths;
    left += borderWidths;
    right += borderWidths;
    bottom += borderWidths;

    /* Based upon the margins, calculate the space left for the graph. */

    x1->offset = left;
    y1->offset = graphPtr->height - bottom;
    leftOver = graphPtr->width - (left + right);
    if (leftOver < 0) {
	return TCL_ERROR;
    }
    x1->scale = (double)leftOver;	/* Pixels per X unit */
    leftOver = graphPtr->height - (top + bottom);
    if (leftOver < 0) {
	return TCL_ERROR;
    }
    y1->scale = (double)leftOver;	/* Pixels per Y unit */
    x2->scale = x1->scale;
    x2->offset = x1->offset;
    y2->scale = y1->scale;
    y2->offset = y1->offset;

    /* Calculate the average symbol (formula is arbitrary) */

    value = log(((double)x1->scale) * y1->scale) * 0.8;
    graphPtr->avgSymSize = BLT_RND(value);

    graphPtr->origin.x = MAPX(x1, 0.0);
    graphPtr->origin.y = MAPY(y1, 0.0);
    graphPtr->extreme.x = MAPX(x1, 1.0);
    graphPtr->extreme.y = MAPY(y1, 1.0);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * GetAxisLimits --
 *
 *	This procedure returns a string representing the axis limits
 *	of the graph.  The format of the string is { xmin ymin xmax ymax}.
 *
 * Results:
 *	Always returns TCL_OK.  The interp->result field is
 *	a list of the graph axis limits.
 *
 *--------------------------------------------------------------
 */
static int
GetAxisLimits(axisPtr, argc, argv)
    Axis *axisPtr;
    int argc;
    char **argv;

{
    char string[TCL_DOUBLE_SPACE + 1];
    double min, max;

    if (argc != 3) {
	Tcl_AppendResult(axisPtr->interp, "wrong # args: should be \"",
	    argv[0], " ", axisNames[axisPtr->type], "axis limits\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    min = axisPtr->min, max = axisPtr->max;
    if (axisPtr->logScale) {
	min = BLT_EXP10(min);
	max = BLT_EXP10(max);
    }
    Tcl_PrintDouble(axisPtr->interp, min, string);
    Tcl_AppendElement(axisPtr->interp, string);
    Tcl_PrintDouble(axisPtr->interp, max, string);
    Tcl_AppendElement(axisPtr->interp, string);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * InvTransformCoord --
 *
 *	Maps the given window coordinate into an axis-value.
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the axis value. If an error occurred, TCL_ERROR is returned
 *	and interp->result will contain an error message.
 *
 * ----------------------------------------------------------------------
 */
static int
InvTransformCoord(axisPtr, argc, argv)
    Axis *axisPtr;
    int argc;
    char **argv;
{
    int coord;			/* Integer window coordinate*/
    char string[TCL_DOUBLE_SPACE + 1];
    double value;

    if (argc != 4) {
	Tcl_AppendResult(axisPtr->interp, "wrong # args: should be \"",
	    argv[0], " ", axisNames[axisPtr->type],
	    "axis invtransform winPos\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (Tcl_GetInt(axisPtr->interp, argv[2], &coord) != TCL_OK) {
	return TCL_ERROR;
    }
    value = Blt_InvTransform((GraphAxis *)axisPtr, coord);
    Tcl_PrintDouble(axisPtr->interp, value, string);
    Tcl_AppendElement(axisPtr->interp, string);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TransformCoord --
 *
 *	Maps the given axis-value to a window coordinate.
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the window coordinate. If an error occurred, TCL_ERROR
 *	is returned and interp->result will contain an error
 *	message.
 *
 * ----------------------------------------------------------------------
 */
static int
TransformCoord(axisPtr, argc, argv)
    Axis *axisPtr;		/* Axis */
    int argc;
    char **argv;
{
    double value;
    int coord;

    if (argc != 4) {
	Tcl_AppendResult(axisPtr->interp, "wrong # args: should be \"",
	    argv[0], " ", axisNames[axisPtr->type], "axis transform value\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if (Tcl_ExprDouble(axisPtr->interp, argv[2], &value) != TCL_OK) {
	return TCL_ERROR;
    }
    coord = Blt_Transform((GraphAxis *)axisPtr, value);
    sprintf(axisPtr->interp->result, "%d", coord);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_CreateAxis --
 *
 *	Create and initialize a structure containing information to
 * 	display a graph axis.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * ----------------------------------------------------------------------
 */
int
Blt_CreateAxis(graphPtr, type, flags)
    Graph *graphPtr;
    enum AxisTypes type;
    int flags;			/* Configuration flags */
{
    Axis *axisPtr;
    enum AxisLocations location;

    axisPtr = (Axis *)calloc(1, sizeof(Axis));
    if (axisPtr == NULL) {
	graphPtr->interp->result = "can't allocate axis structure";
	return TCL_ERROR;
    }
    location = (AxisLocation) type;	/* For now, there's a 1-1
					 * correspondence between axis
					 * types and locations */
    axisPtr->type = type;
    axisPtr->location = location;
    axisPtr->interp = graphPtr->interp;	/* Needed for Tcl_PrintDouble */
    axisPtr->showTicks = 1;
    axisPtr->reqSubTicks = 2;
    axisPtr->destroyProc = DestroyAxis;
    axisPtr->displayProc = DisplayAxis;
    axisPtr->layoutProc = LayoutAxis;
    axisPtr->printProc = PrintAxis;

    /*
     * The actual axis min and max can't be the same, so initializing
     * the previous limits to zero is Ok.
     */
    axisPtr->prevMin = axisPtr->prevMax = 0.0;
    axisPtr->mapped = ((location == BOTTOM_AXIS) || (location == LEFT_AXIS));

    graphPtr->axisArr[type] = (GraphAxis *)axisPtr;
    if (ConfigureAxis(graphPtr, axisPtr, 0, (char **)NULL, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

int
Blt_AxisCmd(graphPtr, axis, argc, argv, flags)
    Graph *graphPtr;
    GraphAxis *axis;
    int argc;
    char **argv;
    int flags;
{
    int result = TCL_ERROR;
    Axis *axisPtr = (Axis *)axis;
    Tcl_Interp *interp = graphPtr->interp;
    char c;
    int length;
    char *which;

    which = axisNames[axisPtr->type];
    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " ", which, "axis option ?args?\"", NULL);
	return TCL_ERROR;
    }
    c = argv[2][0];
    length = strlen(argv[2]);

    if ((c == 'c') && (strncmp(argv[2], "configure", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" ", which, "axis configure ?args?\"", NULL);
	    return TCL_ERROR;
	}
	result = ConfigureAxis(graphPtr, axisPtr, argc - 3, argv + 3, flags);
    } else if ((c == 'l') && (strncmp(argv[2], "limits", length) == 0)) {
	result = GetAxisLimits(axisPtr, argc, argv);
    } else if ((c == 'i') && (strncmp(argv[2], "invtransform", length) == 0)) {
	result = InvTransformCoord(axisPtr, argc, argv);
    } else if ((c == 't') && (strncmp(argv[2], "transform", length) == 0)) {
	result = TransformCoord(axisPtr, argc, argv);
    } else {
	Tcl_AppendResult(interp, "bad ", which, "axis option \"", argv[2],
	    "\":  should be configure or limits", (char *)NULL);
	return TCL_ERROR;
    }
    return result;
}
