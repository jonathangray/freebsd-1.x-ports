
/*
 * bltGrTag.c --
 *
 *	This module implements tags in a graph widget for
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
#include "bltGrTag.h"
#include <ctype.h>

extern Tk_CustomOption bltXAxisFlagsOption;
extern Tk_CustomOption bltYAxisFlagsOption;

#define DEF_TAG_ANCHOR		"center"
#define DEF_TAG_BG_COLOR	WHITE
#define DEF_TAG_BG_MONO		WHITE
#define DEF_TAG_BITMAP		(char *)NULL
#define DEF_TAG_DASHES		"0"
#define DEF_TAG_ELEMENT		(char *)NULL
#define DEF_TAG_FG_COLOR	BLACK
#define DEF_TAG_FG_MONO		BLACK
#define DEF_TAG_FONT		"*-Helvetica-Bold-R-Normal-*-120-*"
#define DEF_TAG_HEIGHT		"0"
#define DEF_TAG_LINE_WIDTH	"0"
#define DEF_TAG_MAP_X		"x"
#define DEF_TAG_MAP_Y		"y"
#define DEF_TAG_ROTATE		"0.0"
#define DEF_TAG_STIPPLE		(char *)NULL
#define DEF_TAG_TEXT		(char *)NULL
#define DEF_TAG_WIDTH		"0"
#define DEF_TAG_WINDOW		(char *)NULL
#define DEF_TAG_X_OFFSET	"0"
#define DEF_TAG_Y_OFFSET	"0"

static char *tagNames[] =
{
    "text", "line", "polygon", "bitmap", "window", (char *)NULL,
};

typedef struct {
    TagClassType type;		/* Type of tag */
    int flags;
    Tk_Uid id;			/* Identifier for tag */
    double *coordArr;		/* Coordinate array to position tag */
    unsigned int numCoords;	/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with tag */
    int axisFlags;		/* Indicates which axes to element's map
				 * coordinates onto */

    DisplayTagProc *displayProc;
    DestroyTagProc *destroyProc;
    ConfigTagProc *configProc;
    LayoutTagProc *layoutProc;
    PrintTagProc *printProc;
    TypeOfTagProc *typeProc;

    /*
     * Text specific fields and attributes
     */
    char *text;			/* Text to display in graph (malloc'ed) or
				 * NULL. */
#ifdef notdef
    int textLength;		/* # of characters in text. */
    char *textVarName;		/* Name of variable (malloc'ed) or NULL. If
				 * non-NULL, graph displays the contents of
				 * this variable. */
#endif
    XFontStruct *fontPtr;	/* font */
    XColor *normalFg;		/* foreground */
    XColor *normalBg;		/* background */
    int xOffset, yOffset;	/* pixel offset from anchor */
    Tk_Anchor anchor;		/* Anchor type */
    double theta;		/* rotation */

    /*
     * Private, calculated items
     */
    GC gc;			/* Private graphic context */
    GC bgGC;			/* Shared graphic context: used to draw
				 * background area of text */
    int x, y;			/* Window x, y position of tag */
    Pixmap bitmap;		/* Bitmap containing rotated image of text
				 * string. Set to None if no rotation */
    unsigned int width;		/* Extents of rotated text bitmap */
    unsigned int height;

} TextTag;

static Tk_ConfigSpec textConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", "textTagAnchor", "TagAnchor",
	DEF_TAG_ANCHOR, Tk_Offset(TextTag, anchor), 0},
    {TK_CONFIG_COLOR, "-background", "textTagBackground", "TagBackground",
	DEF_TAG_BG_COLOR, Tk_Offset(TextTag, normalBg),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-background", "textTagBackground", "TagBackground",
	DEF_TAG_BG_MONO, Tk_Offset(TextTag, normalBg),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-bg", "textTagBackground", "TagBackground",
	(char *)NULL, 0, 0},
    {TK_CONFIG_UID, "-element", "textTagElement", "TagElement",
	DEF_TAG_ELEMENT, Tk_Offset(TextTag, elemId), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-fg", "textTagForeground", "TagForeground",
	(char *)NULL, 0, 0},
    {TK_CONFIG_FONT, "-font", "textTagFont", "TagFont",
	DEF_TAG_FONT, Tk_Offset(TextTag, fontPtr), 0},
    {TK_CONFIG_COLOR, "-foreground", "textTagForeground", "TagForeground",
	DEF_TAG_FG_COLOR, Tk_Offset(TextTag, normalFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "textTagForeground", "TagForeground",
	DEF_TAG_FG_MONO, Tk_Offset(TextTag, normalFg), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-mapx", "textTagMapX", "TagMapX",
	DEF_TAG_MAP_X, Tk_Offset(TextTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltXAxisFlagsOption},
    {TK_CONFIG_CUSTOM, "-mapy", "textTagMapY", "TagMapY",
	DEF_TAG_MAP_Y, Tk_Offset(TextTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltYAxisFlagsOption},
    {TK_CONFIG_DOUBLE, "-rotate", "textTagRotate", "TagRotate",
	DEF_TAG_ROTATE, Tk_Offset(TextTag, theta),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_STRING, "-text", "textTagText", "TagText",
	DEF_TAG_TEXT, Tk_Offset(TextTag, text), TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-xoffset", "textTagXOffset", "TagXOffset",
	DEF_TAG_X_OFFSET, Tk_Offset(TextTag, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "textTagYOffset", "TagYOffset",
	DEF_TAG_Y_OFFSET, Tk_Offset(TextTag, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

typedef struct {
    TagClassType type;		/* Type of tag */
    int flags;
    Tk_Uid id;			/* Identifier for tag */
    double *coordArr;		/* Coordinate array to position tag */
    unsigned int numCoords;	/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with tag */
    int axisFlags;		/* Indicates which axis to element's map
				 * y-coordinates onto */

    DisplayTagProc *displayProc;
    DestroyTagProc *destroyProc;
    ConfigTagProc *configProc;
    LayoutTagProc *layoutProc;
    PrintTagProc *printProc;
    TypeOfTagProc *typeProc;

    /*
     * Window specific attributes
     */
    char *pathName;		/* Name of child window to be displayed */
    Tk_Window child;		/* Window to display */
    int reqWidth, reqHeight;	/* Requested window extents */
    unsigned int width, height;	/* Actual window extents */
    int xOffset, yOffset;	/* Pixel offset from anchor */
    Tk_Anchor anchor;		/* Anchor */
    int x, y;

} WindowTag;

static Tk_ConfigSpec windowConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", "winTagAnchor", "TagAnchor",
	DEF_TAG_ANCHOR, Tk_Offset(WindowTag, anchor), 0},
    {TK_CONFIG_UID, "-element", "winTagElement", "TagElement",
	DEF_TAG_ELEMENT, Tk_Offset(WindowTag, elemId), TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-height", "winTagHeight", "TagHeight",
	DEF_TAG_HEIGHT, Tk_Offset(WindowTag, reqHeight),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "winTagMapX", "TagMapX",
	DEF_TAG_MAP_X, Tk_Offset(WindowTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltXAxisFlagsOption},
    {TK_CONFIG_CUSTOM, "-mapy", "winTagMapY", "TagMapY",
	DEF_TAG_MAP_Y, Tk_Offset(WindowTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltYAxisFlagsOption},
    {TK_CONFIG_STRING, "-pathname", "winTagPathName", "TagPathName",
	DEF_TAG_WINDOW, Tk_Offset(WindowTag, pathName), 0},
    {TK_CONFIG_PIXELS, "-width", "winTagWidth", "TagWidth",
	DEF_TAG_WIDTH, Tk_Offset(WindowTag, reqWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-xoffset", "winTagXOffset", "TagXOffset",
	DEF_TAG_X_OFFSET, Tk_Offset(WindowTag, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "winTagYOffset", "TagYOffset",
	DEF_TAG_Y_OFFSET, Tk_Offset(WindowTag, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

typedef struct {
    TagClassType type;		/* Type of tag */
    unsigned int flags;
    Tk_Uid id;			/* Identifier for tag */
    double *coordArr;		/* Coordinate array to position tag */
    unsigned int numCoords;	/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with tag */
    int axisFlags;		/* Indicates which axes to element's map
				 * coordinates onto */

    DisplayTagProc *displayProc;
    DestroyTagProc *destroyProc;
    ConfigTagProc *configProc;
    LayoutTagProc *layoutProc;
    PrintTagProc *printProc;
    TypeOfTagProc *typeProc;

    /*
     * Bitmap specific attributes
     */
    Pixmap bitmap;		/* Bitmap to be displayed */
    double theta;		/* Degrees to rotate bitmap */
    XColor *normalFg;		/* foreground color */
    XColor *normalBg;		/* background color */
    Tk_Anchor anchor;		/* anchor */
    int xOffset, yOffset;	/* Pixel offset from anchor */

    GC gc;			/* Private graphic context */
    GC bgGC;			/* Shared graphic context */
    int x, y;			/* Window x,y position of the bitmap */
    Pixmap rotBitmap;		/* Rotated bitmap */
    unsigned int width;		/* Extents of rotated bitmap */
    unsigned int height;

} BitmapTag;

static Tk_ConfigSpec bitmapConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", "bmTagAnchor", "TagAnchor",
	DEF_TAG_ANCHOR, Tk_Offset(BitmapTag, anchor),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-background", "bmTagBackground", "TagBackground",
	DEF_TAG_BG_COLOR, Tk_Offset(BitmapTag, normalBg),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-background", "bmTagBackground", "TagBackground",
	DEF_TAG_BG_MONO, Tk_Offset(BitmapTag, normalBg),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-bg", "bmTagBackground", (char *)NULL, (char *)NULL,
	0, 0},
    {TK_CONFIG_BITMAP, "-bitmap", "bmTagBitmap", "TagBitmap",
	DEF_TAG_BITMAP, Tk_Offset(BitmapTag, bitmap), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-fg", "bmTagForeground", (char *)NULL, (char *)NULL,
	0, 0},
    {TK_CONFIG_COLOR, "-foreground", "bmTagForeground", "TagForeground",
	DEF_TAG_FG_COLOR, Tk_Offset(BitmapTag, normalFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "bmTagForeground", "TagForeground",
	DEF_TAG_FG_MONO, Tk_Offset(BitmapTag, normalFg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_UID, "-element", "bmTagElement", "TagElement",
	DEF_TAG_ELEMENT, Tk_Offset(BitmapTag, elemId), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-mapx", "bmTagMapX", "TagMapX",
	DEF_TAG_MAP_X, Tk_Offset(BitmapTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltXAxisFlagsOption},
    {TK_CONFIG_CUSTOM, "-mapy", "bmTagMapY", "TagMapY",
	DEF_TAG_MAP_Y, Tk_Offset(BitmapTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltYAxisFlagsOption},
    {TK_CONFIG_DOUBLE, "-rotate", "bmTagRotate", "TagRotate",
	DEF_TAG_ROTATE, Tk_Offset(BitmapTag, theta),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-xoffset", "bmTagXOffset", "TagXOffset",
	DEF_TAG_X_OFFSET, Tk_Offset(BitmapTag, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "bmTagYOffset", "TagYOffset",
	DEF_TAG_Y_OFFSET, Tk_Offset(BitmapTag, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

typedef struct {
    TagClassType type;		/* Type is LINE_TAG_TYPE */
    unsigned int flags;
    Tk_Uid id;			/* Identifier for tag */
    double *coordArr;		/* Coordinate array to position tag */
    unsigned int numCoords;	/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with tag */
    int axisFlags;		/* Indicates which axes to element's map
				 * coordinates onto */

    DisplayTagProc *displayProc;
    DestroyTagProc *destroyProc;
    ConfigTagProc *configProc;
    LayoutTagProc *layoutProc;
    PrintTagProc *printProc;
    TypeOfTagProc *typeProc;

    /*
     * Line specific attributes
     */
    XColor *normalFg;		/* Foreground */
    XColor *normalBg;		/* Background color */
    int lineWidth;		/* line width */
    int dashes;			/* Dash list value */
#ifdef notdef
    int arrow;			/* Indicates whether or not to draw
				 * arrowheads: "none", "first", "last", or
				 * "both". */
    float arrowShapeA;		/* Distance from tip of arrowhead to center. */
    float arrowShapeB;		/* Distance from tip of arrowhead to trailing
				 * point, measured along shaft. */
    float arrowShapeC;		/* Distance of trailing points from outside
				 * edge of shaft. */
    double *firstArrowPtr;	/* Points to array of 5 points describing
				 * polygon for arrowhead at first point in
				 * line.  First point of arrowhead is tip.
				 * Malloc'ed.  NULL means no arrowhead at
				 * first point. */
    double *lastArrowPtr;	/* Points to polygon for arrowhead at last
				 * point in line (5 points, first of which is
				 * tip).  Malloc'ed.  NULL means no arrowhead
				 * at last point. */
#endif
    int xOffset, yOffset;	/* Pixel offset */

    GC gc;			/* Private graphic context */
    XPoint *pointArr;
    int numPoints;
} LineTag;

static Tk_ConfigSpec lineConfigSpecs[] =
{
#ifdef notdef
    {TK_CONFIG_STRING, "-arrow", "lineTagArrow", "TagArrow",
	"none", Tk_Offset(LineTag, arrow), TK_CONFIG_DONT_SET_DEFAULT},
#endif
    {TK_CONFIG_COLOR, "-background", "lineTagBackground", "TagBackground",
	DEF_TAG_BG_COLOR, Tk_Offset(LineTag, normalBg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-background", "lineTagBackground", "TagBackground",
	DEF_TAG_BG_MONO, Tk_Offset(LineTag, normalBg), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_SYNONYM, "-bg", "lineTagBackground", (char *)NULL, (char *)NULL,
	0, 0},
    {TK_CONFIG_INT, "-dashes", "lineTagDashes", "TagDashes",
	DEF_TAG_DASHES, Tk_Offset(LineTag, dashes),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_UID, "-element", "lineTagElement", "TagElement",
	DEF_TAG_ELEMENT, Tk_Offset(LineTag, elemId), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-fg", "lineTagForeground", (char *)NULL, (char *)NULL,
	0, 0},
    {TK_CONFIG_COLOR, "-foreground", "lineTagForeground", "TagForeground",
	DEF_TAG_FG_COLOR, Tk_Offset(LineTag, normalFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "lineTagForeground", "TagForeground",
	DEF_TAG_FG_MONO, Tk_Offset(LineTag, normalFg), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_PIXELS, "-linewidth", "lineTagLineWidth", "TagLineWidth",
	DEF_TAG_LINE_WIDTH, Tk_Offset(LineTag, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "lineTagMapX", "TagMapX",
	DEF_TAG_MAP_X, Tk_Offset(LineTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltXAxisFlagsOption},
    {TK_CONFIG_CUSTOM, "-mapy", "lineTagMapY", "TagMapY",
	DEF_TAG_MAP_Y, Tk_Offset(LineTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltYAxisFlagsOption},
    {TK_CONFIG_PIXELS, "-xoffset", "lineTagXOffset", "TagXOffset",
	DEF_TAG_X_OFFSET, Tk_Offset(LineTag, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "lineTagYOffset", "TagYOffset",
	DEF_TAG_Y_OFFSET, Tk_Offset(LineTag, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

typedef struct {
    TagClassType type;		/* Type of tag */
    int flags;
    Tk_Uid id;			/* Identifier for tag */
    double *coordArr;		/* Coordinate array to position tag */
    unsigned int numCoords;	/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with tag */
    int axisFlags;		/* Indicates which axis to element's map
				 * y-coordinates onto */

    DisplayTagProc *displayProc;
    DestroyTagProc *destroyProc;
    ConfigTagProc *configProc;
    LayoutTagProc *layoutProc;
    PrintTagProc *printProc;
    TypeOfTagProc *typeProc;

    /*
     * Polygon specific attributes and fields
     */
    XColor *normalFg;		/* foreground */
    XColor *normalBg;		/* background */
    Pixmap stipple;		/* stipple pattern */
    int lineWidth;		/* line width */
    int dashes;			/* dash list value */
    int xOffset, yOffset;	/* Pixel offset */

    GC lineGC;			/* Private graphic context */
    GC fillGC;			/* Private graphic context */
    XPoint *pointArr;		/* Points needed to draw polygon */
    int numPoints;		/* Number of points in above array */
} PolygonTag;

static Tk_ConfigSpec polygonConfigSpecs[] =
{
    {TK_CONFIG_COLOR, "-background", "polyTagBackground", "TagBackground",
	DEF_TAG_BG_COLOR, Tk_Offset(PolygonTag, normalBg),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-background", "polyTagBackground", "TagBackground",
	DEF_TAG_BG_MONO, Tk_Offset(PolygonTag, normalBg),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-bg", "polyTagBackground", (char *)NULL, (char *)NULL,
	0, 0},
    {TK_CONFIG_INT, "-dashes", "polyTagDashes", "TagDashes",
	DEF_TAG_DASHES, Tk_Offset(PolygonTag, dashes),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_UID, "-element", "polyTagElement", "TagElement",
	DEF_TAG_ELEMENT, Tk_Offset(PolygonTag, elemId), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-fg", "polyTagForeground", (char *)NULL, (char *)NULL,
	0, 0},
    {TK_CONFIG_COLOR, "-foreground", "polyTagForeground", "TagForeground",
	DEF_TAG_FG_COLOR, Tk_Offset(PolygonTag, normalFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "polyTagForeground", "TagForeground",
	DEF_TAG_FG_MONO, Tk_Offset(PolygonTag, normalFg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_PIXELS, "-linewidth", "polyTagLineWidth", "TagLineWidth",
	DEF_TAG_LINE_WIDTH, Tk_Offset(PolygonTag, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "polyTagMapX", "TagMapX",
	DEF_TAG_MAP_X, Tk_Offset(PolygonTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltXAxisFlagsOption},
    {TK_CONFIG_CUSTOM, "-mapy", "polyTagMapY", "TagMapY",
	DEF_TAG_MAP_Y, Tk_Offset(PolygonTag, axisFlags),
	TK_CONFIG_DONT_SET_DEFAULT, &bltYAxisFlagsOption},
    {TK_CONFIG_BITMAP, "-stipple", "polyTagStipple", "TagStipple",
	DEF_TAG_STIPPLE, Tk_Offset(PolygonTag, stipple), TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-xoffset", "polyTagXOffset", "TagXOffset",
	DEF_TAG_X_OFFSET, Tk_Offset(PolygonTag, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "polyTagYOffset", "TagYOffset",
	DEF_TAG_Y_OFFSET, Tk_Offset(PolygonTag, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

extern int strncasecmp _ANSI_ARGS_((CONST char *s1, CONST char *s2, size_t n));

/*
 * ----------------------------------------------------------------------
 *
 * GetExprValue --
 *
 * 	Convert the expression string into a floating point value. The
 *	only reason we use this routine instead of Tcl_ExprDouble is to
 *	handle "elastic" bounds.  That is, convert the strings "-Inf",
 *	"Inf" into MAX_NEG_VAL and MAX_POS_VAL respectively.
 *
 * Results:
 *	The return value is a standard Tcl result.  The value of the
 * 	expression is passed back via valuePtr.
 *
 * ----------------------------------------------------------------------
 */
static int
GetExprValue(interp, expr, valuePtr)
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    char *expr;			/* Numeric expression string to parse */
    double *valuePtr;		/* Real-valued result of expression */
{
    int length;

    length = strlen(expr);
    if ((expr[0] == 'I') && (strcmp(expr, "Inf") == 0)) {
	*valuePtr = Blt_posInfinity;	/* Elastic upper bound */
    } else if ((expr[0] == '-') && (expr[1] == 'I') &&
	(strcmp(expr, "-Inf") == 0)) {
	*valuePtr = Blt_negInfinity;	/* Elastic lower bound */
    } else if ((expr[0] == 'm') && (length > 1) &&
	(strncasecmp(expr, "minAxisValue", length) == 0)) {
	*valuePtr = Blt_negInfinity;	/* Elastic lower bound */
    } else if ((expr[0] == 'm') && (length > 1) &&
	(strncasecmp(expr, "maxAxisValue", length) == 0)) {
	*valuePtr = Blt_posInfinity;	/* Elastic upper bound */
    } else if (Tcl_ExprDouble(interp, expr, valuePtr) != TCL_OK) {
	Tcl_AppendResult(interp, "bad expression \"", expr, "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

 /*
  * ----------------------------------------------------------------------
 *
 * GetExprString --
 *
 * 	Convert the double precision value into its string representation.
 *	The only reason this routine is used in instead of sprintf, is to
 *	handle the "elastic" bounds.  That is, convert the values
 *	MAX_POS_VAL and MAX_NEG_VAL into "maxvalue" and "minvalue"
 *	respectively.
 *
 * Results:
 *	The return value is a standard Tcl result.  The string of the
 * 	expression is passed back via string.
 *
 * ----------------------------------------------------------------------
 */
static int
GetExprString(interp, x, string)
    Tcl_Interp *interp;
    double x;			/* Numeric value */
    char *string;		/* String representation of value */
{
    if (x == Blt_posInfinity) {
	strcpy(string, "maxAxisValue");
    } else if (x == Blt_negInfinity) {
	strcpy(string, "minAxisValue");
    } else {
	Tcl_PrintDouble(interp, x, string);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureBitmap --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or
 *	reconfigure) a bitmap tag.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as bitmap pixmap, colors, rotation,
 *	etc. get set for tagPtr;  old resources get freed, if there
 *	were any.  The tag is eventually redisplayed.
 *
 *----------------------------------------------------------------------
 */
 /* ARGSUSED */
static int
ConfigureBitmap(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    BitmapTag *bmTagPtr = (BitmapTag *)tagPtr;

    if (bmTagPtr->bitmap != None) {
	GC newGC;
	XGCValues gcValues;
	unsigned long gcMask;

	gcValues.foreground = bmTagPtr->normalFg->pixel;
	gcMask = GCForeground | GCFillStyle;
	if (bmTagPtr->normalBg != NULL) {
	    gcValues.background = bmTagPtr->normalBg->pixel;
	    gcValues.fill_style = FillSolid;
	    gcMask |= GCBackground;
	} else {
	    gcValues.stipple = bmTagPtr->bitmap;
	    gcValues.fill_style = FillStippled;
	    gcMask |= GCStipple;
	}

	/*
	 * Note that this is *not* a shared GC.  GCTileStipXOrigin and
	 * GCTileStipYOrigin may change when displayed.
	 */
	newGC = XCreateGC(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	    gcMask, &gcValues);
	if (bmTagPtr->gc != NULL) {
	    XFreeGC(graphPtr->display, bmTagPtr->gc);
	}
	bmTagPtr->gc = newGC;
	/* Create background GC color */
	if (bmTagPtr->normalBg != NULL) {
	    gcValues.foreground = bmTagPtr->normalBg->pixel;
	    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
	    if (bmTagPtr->bgGC != NULL) {
		Tk_FreeGC(graphPtr->display, bmTagPtr->bgGC);
	    }
	    bmTagPtr->bgGC = newGC;
	}
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * LayoutBitmap --
 *
 * 	This procedure gets called each time the layout of the graph
 *	changes.  The x, y window coordinates of the bitmap tag are
 *	saved in the tag structure.
 *
 *	Additionly, if no background color was specified, the
 *	GCTileStipXOrigin and GCTileStipYOrigin attributes are set in
 *	the private GC.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Window coordinates are saved and if no background color was
 * 	set, the GC stipple origins are changed to calculated window
 *	coordinates.
 *
 * ----------------------------------------------------------------------
 */
static void
LayoutBitmap(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    BitmapTag *bmTagPtr = (BitmapTag *)tagPtr;
    unsigned int width, height;
    XPoint win, bmPos;

    if (bmTagPtr->bitmap == None) {
	return;
    }
    Tk_SizeOfBitmap(graphPtr->display, bmTagPtr->bitmap, &width, &height);
    win = Blt_TransformPt(graphPtr, bmTagPtr->coordArr[0],
	bmTagPtr->coordArr[1], bmTagPtr->axisFlags);
    bmPos = Blt_TranslateBoxCoords(win.x, win.y, width, height,
	bmTagPtr->anchor);
    bmTagPtr->x = (bmPos.x + bmTagPtr->xOffset);
    bmTagPtr->y = (bmPos.y + bmTagPtr->yOffset);

    if ((bmTagPtr->rotBitmap != None) &&
	(bmTagPtr->rotBitmap != bmTagPtr->bitmap)) {
	XFreePixmap(graphPtr->display, bmTagPtr->rotBitmap);
	bmTagPtr->rotBitmap = None;
    }
    if (bmTagPtr->theta == 0.0) {
	bmTagPtr->width = width;
	bmTagPtr->height = height;
	bmTagPtr->rotBitmap = bmTagPtr->bitmap;
    } else {
	GC bitmapGC;
	unsigned long gcMask;
	XGCValues gcValues;

	gcValues.foreground = 1, gcValues.background = 0;
	gcMask = (GCForeground | GCBackground);
	bitmapGC = XCreateGC(graphPtr->display, bmTagPtr->bitmap, gcMask,
	    &gcValues);
	bmTagPtr->rotBitmap = Blt_RotateBitmap(graphPtr->display,
	    Tk_WindowId(graphPtr->tkwin), bitmapGC, bmTagPtr->bitmap,
	    width, height, bmTagPtr->theta, &(bmTagPtr->width),
	    &(bmTagPtr->height));
	XFreeGC(graphPtr->display, bitmapGC);
	bmPos = Blt_TranslateBoxCoords(win.x, win.y, bmTagPtr->width,
	    bmTagPtr->height, bmTagPtr->anchor);
	bmTagPtr->x = bmPos.x + bmTagPtr->xOffset;
	bmTagPtr->y = bmPos.y + bmTagPtr->yOffset;
    }
    if (bmTagPtr->normalBg == NULL) {
	XSetTSOrigin(graphPtr->display, bmTagPtr->gc, bmTagPtr->x,
	    bmTagPtr->y);
	XSetStipple(graphPtr->display, bmTagPtr->gc, bmTagPtr->rotBitmap);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DisplayBitmap --
 *
 *	This procedure is invoked to display a bitmap tag.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	GC stipple origins are changed to current window coordinates.
 *	Commands are output to X to display the tag in its current mode.
 *
 *----------------------------------------------------------------------
 */
static void
DisplayBitmap(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    BitmapTag *bmTagPtr = (BitmapTag *)tagPtr;

    if (bmTagPtr->bitmap == None) {
	return;
    }
    if (bmTagPtr->normalBg != NULL) {
	if ((bmTagPtr->theta == 0.0) || (bmTagPtr->theta == 90.0) ||
	    (bmTagPtr->theta == 180.0) || (bmTagPtr->theta == 270.0)) {
	    /*
	     * Right angle rotations: Bounding box matches background area
	     */
	    XCopyPlane(graphPtr->display, bmTagPtr->rotBitmap,
		graphPtr->canvas, bmTagPtr->gc, 0, 0, bmTagPtr->width,
		bmTagPtr->height, bmTagPtr->x, bmTagPtr->y, 1);
	    return;
	} else {
	    XPoint pointArr[4];
	    register int i;
	    unsigned int width, height;

	    Tk_SizeOfBitmap(graphPtr->display, bmTagPtr->bitmap, &width,
		&height);
	    Blt_GetBoundingBox(width, height, bmTagPtr->theta, &width, &height,
		pointArr);
	    for (i = 0; i < 4; i++) {
		pointArr[i].x += bmTagPtr->x + (bmTagPtr->width / 2);
		pointArr[i].y += bmTagPtr->y + (bmTagPtr->height / 2);
	    }
	    XFillPolygon(graphPtr->display, graphPtr->canvas, bmTagPtr->bgGC,
		pointArr, 4, Convex, CoordModeOrigin);
	}
    }
    Blt_StencilBitmap(graphPtr->display, graphPtr->canvas, bmTagPtr->gc,
	bmTagPtr->rotBitmap, bmTagPtr->x, bmTagPtr->y, bmTagPtr->width,
	bmTagPtr->height);
}


static void
PrintBitmap(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;		/* Tag to be printed */
{
    BitmapTag *bmTagPtr = (BitmapTag *)tagPtr;

    if (bmTagPtr->bitmap != None) {
	unsigned int width, height;
	int centerX, centerY;

	Tk_SizeOfBitmap(graphPtr->display, bmTagPtr->bitmap, &width, &height);

	/* Find the center of the bounding box */
	centerX = bmTagPtr->x + (bmTagPtr->width / 2);
	centerY = bmTagPtr->y + (bmTagPtr->height / 2);

	Blt_ForegroundToPostScript(graphPtr, bmTagPtr->normalFg);
	Blt_BitmapToPostScript(graphPtr, bmTagPtr->bitmap, centerX, centerY,
	    width, height, bmTagPtr->theta, bmTagPtr->normalBg);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyBitmap --
 *
 *	Destroys the structure containing the attributes of the bitmap
 * 	tag.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Bitmap attributes (GCs, colors, bitmap, etc) get destroyed.
 *	Memory is released, X resources are freed, and the graph is
 *	redrawn.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyBitmap(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    BitmapTag *bmTagPtr = (BitmapTag *)tagPtr;

    if (bmTagPtr->gc != NULL) {
	XFreeGC(graphPtr->display, bmTagPtr->gc);
    }
    if (bmTagPtr->bgGC != NULL) {
	Tk_FreeGC(graphPtr->display, bmTagPtr->bgGC);
    }
    if ((bmTagPtr->rotBitmap != None) &&
	(bmTagPtr->rotBitmap != bmTagPtr->bitmap)) {
	XFreePixmap(graphPtr->display, bmTagPtr->rotBitmap);
    }
    if (bmTagPtr->bitmap != None) {
	Tk_FreeBitmap(graphPtr->display, bmTagPtr->bitmap);
    }
    if (bmTagPtr->normalFg != NULL) {
	Tk_FreeColor(bmTagPtr->normalFg);
    }
    if (bmTagPtr->normalBg != NULL) {
	Tk_FreeColor(bmTagPtr->normalBg);
    }
    if (bmTagPtr->coordArr != NULL) {
	free((char *)bmTagPtr->coordArr);
    }
    free((char *)bmTagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateBitmap --
 *
 *	Allocate memory and initialize methods for the new bitmap tag.
 *
 * Results:
 *	The pointer to the newly allocated tag structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the bitmap tag structure.
 *
 *----------------------------------------------------------------------
 */
static Tag *
CreateBitmap()
{
    register Tag *tagPtr;

    tagPtr = (Tag *)calloc(1, sizeof(BitmapTag));

    if (tagPtr != NULL) {
	tagPtr->configSpecs = bitmapConfigSpecs;
	tagPtr->configProc = ConfigureBitmap;
	tagPtr->destroyProc = DestroyBitmap;
	tagPtr->displayProc = DisplayBitmap;
	tagPtr->layoutProc = LayoutBitmap;
	tagPtr->printProc = PrintBitmap;
	tagPtr->type = BITMAP_TAG_TYPE;
    }
    return (tagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureText --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or
 *	reconfigure) a text tag.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for tagPtr;  old resources get freed, if there
 *	were any.  The tag is eventually redisplayed.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigureText(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    TextTag *textTagPtr = (TextTag *)tagPtr;

    if (textTagPtr->text != NULL) {
	GC newGC;
	XGCValues gcValues;
	unsigned long gcMask;

	gcValues.foreground = textTagPtr->normalFg->pixel;
	gcValues.font = textTagPtr->fontPtr->fid;
	gcValues.fill_style = FillSolid;
	gcMask = (GCFont | GCForeground | GCFillStyle);
	if (textTagPtr->normalBg != NULL) {
	    gcValues.background = textTagPtr->normalBg->pixel;
	    gcMask |= GCBackground;
	} else if (textTagPtr->theta != 0.0) {
	    gcValues.fill_style = FillStippled;
	}
	/*
	 * Note that this is *not* a shared GC.  If text is rotated, the
	 * GCTileStipXOrigin, GCTileStipYOrigin, GCStipple values will be
	 * changed when the layout is recalculated.
	 */
	newGC = XCreateGC(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	    gcMask, &gcValues);
	if (textTagPtr->gc != NULL) {
	    XFreeGC(graphPtr->display, textTagPtr->gc);
	}
	textTagPtr->gc = newGC;
	if (textTagPtr->normalBg != NULL) {
	    gcValues.foreground = textTagPtr->normalBg->pixel;
	    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
	    if (textTagPtr->bgGC != NULL) {
		Tk_FreeGC(graphPtr->display, textTagPtr->bgGC);
	    }
	    textTagPtr->bgGC = newGC;
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutText --
 *
 *	Calculate the layout position for a text tag.  Positional
 *	information is saved in the tag.  If the text is rotated,
 *	a bitmap containing the text is created.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	If no background color has been specified, the GC stipple
 *	origins are changed to current window coordinates. For both
 *	rotated and non-rotated text, if any old bitmap is leftover,
 *	it is freed.
 *
 *----------------------------------------------------------------------
 */
static void
LayoutText(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    TextTag *textTagPtr = (TextTag *)tagPtr;
    XPoint win, anchorPos;

    if (textTagPtr->text == NULL) {
	return;
    }
    win = Blt_TransformPt(graphPtr, textTagPtr->coordArr[0],
	textTagPtr->coordArr[1], textTagPtr->axisFlags);
    anchorPos = Blt_TranslateTextCoords(textTagPtr->fontPtr, textTagPtr->text,
	win.x, win.y, textTagPtr->anchor);
    /* Save page coordinates */
    textTagPtr->x = (anchorPos.x + textTagPtr->xOffset);
    textTagPtr->y = (anchorPos.y + textTagPtr->yOffset);

    if (textTagPtr->bitmap != None) {
	XFreePixmap(graphPtr->display, textTagPtr->bitmap);
	textTagPtr->bitmap = None;
    }
    if (textTagPtr->theta == 0.0) {	/* No rotation; Handle simple case */
	textTagPtr->width = Blt_TextStringWidth(textTagPtr->fontPtr,
	    textTagPtr->text);
	textTagPtr->height = TEXTHEIGHT(textTagPtr->fontPtr);
    } else {
	textTagPtr->bitmap = Blt_CreateTextBitmap(graphPtr->display,
	    Tk_WindowId(graphPtr->tkwin), textTagPtr->fontPtr,
	    textTagPtr->text, textTagPtr->theta, &(textTagPtr->width),
	    &(textTagPtr->height));
	anchorPos = Blt_TranslateBoxCoords(win.x, win.y, textTagPtr->width,
	    textTagPtr->height, textTagPtr->anchor);
	textTagPtr->x = anchorPos.x + textTagPtr->xOffset;
	textTagPtr->y = anchorPos.y + textTagPtr->yOffset;
	XSetStipple(graphPtr->display, textTagPtr->gc, textTagPtr->bitmap);
	XSetTSOrigin(graphPtr->display, textTagPtr->gc, textTagPtr->x,
	    textTagPtr->y);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DisplayText --
 *
 *	Draw the text tag on the drawable given. If the text is not
 *	rotated, simply use the X text drawing routines. However, if
 *	the text has been rotated, stencil the bitmap representing
 *	the text. Since stencilling is very expensive, we try to
 *	draw right angle rotations with XCopyArea.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Commands are output to X to display the tag in its current mode.
 *
 *----------------------------------------------------------------------
 */
static void
DisplayText(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    TextTag *textTagPtr = (TextTag *)tagPtr;

    if (textTagPtr->text == NULL) {
	return;
    }
    if (textTagPtr->theta == 0.0) {

	/*
	 * No rotation; Handle simple case with text drawing routines
	 */

	if (textTagPtr->normalBg == (XColor *)NULL) {
	    XDrawString(graphPtr->display, graphPtr->canvas, textTagPtr->gc,
		textTagPtr->x, textTagPtr->y, textTagPtr->text,
		strlen(textTagPtr->text));
	} else {
	    XDrawImageString(graphPtr->display, graphPtr->canvas,
		textTagPtr->gc, textTagPtr->x, textTagPtr->y,
		textTagPtr->text, strlen(textTagPtr->text));
	}
	return;
    }
    if (textTagPtr->normalBg != NULL) {
	if ((textTagPtr->theta == 90.0) || (textTagPtr->theta == 180.0) ||
	    (textTagPtr->theta == 270.0)) {

	    /*
	     * Right angle rotations: The bounding box of the text matches
	     * background area so we can simply draw the bitmap.
	     */

	    XCopyPlane(graphPtr->display, textTagPtr->bitmap,
		graphPtr->canvas, textTagPtr->gc, 0, 0, textTagPtr->width,
		textTagPtr->height, textTagPtr->x, textTagPtr->y, 1);
	    return;
	} else {
	    XPoint pointArr[4];
	    register int i;
	    unsigned int width, height;

	    /*
	     * Non-orthogonal rotations: Get the coordinates of the rotated
	     * background area and draw a filled polygon before stencilling.
	     */

	    height = TEXTHEIGHT(textTagPtr->fontPtr);
	    width = Blt_TextStringWidth(textTagPtr->fontPtr,
		textTagPtr->text);
	    Blt_GetBoundingBox(width, height, textTagPtr->theta, &width,
		&height, pointArr);
	    for (i = 0; i < 4; i++) {
		pointArr[i].x += textTagPtr->x + (width / 2);
		pointArr[i].y += textTagPtr->y + (height / 2);
	    }
	    XFillPolygon(graphPtr->display, graphPtr->canvas,
		textTagPtr->bgGC, pointArr, 4, Convex, CoordModeOrigin);
	}
    }
    /* Stencil the bitmap (a very expensive drawing operation) */
    Blt_StencilBitmap(graphPtr->display, graphPtr->canvas, textTagPtr->gc,
	textTagPtr->bitmap, textTagPtr->x, textTagPtr->y, textTagPtr->width,
	textTagPtr->height);
}

/*
 *----------------------------------------------------------------------
 *
 * PrintText --
 *
 *	Outputs PostScript commands to draw a text tag at a given
 *	x,y coordinate, rotation, anchor, and font.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript font and color settings are changed.
 *
 *----------------------------------------------------------------------
 */
static void
PrintText(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    TextTag *textTagPtr = (TextTag *)tagPtr;
    int width, height;		/* Width and height of unrotated text */
    double centerX, centerY;

    if (textTagPtr->text == NULL) {
	return;
    }
    Blt_ForegroundToPostScript(graphPtr, textTagPtr->normalFg);
    Blt_FontToPostScript(graphPtr, textTagPtr->fontPtr);

    width = Blt_TextStringWidth(textTagPtr->fontPtr, textTagPtr->text);
    height = TEXTHEIGHT(textTagPtr->fontPtr);

    /*
     * Find the center of the bounding box for PostScript rotation
     */

    centerX = textTagPtr->x + (textTagPtr->width * 0.5);
    centerY = textTagPtr->y + (textTagPtr->height * 0.5);

    if (textTagPtr->normalBg == (XColor *)NULL) {
	Tcl_AppendResult(graphPtr->interp, "false ", (char *)NULL);
    } else {
	Tcl_AppendResult(graphPtr->interp, "{ ", (char *)NULL);
	Blt_BackgroundToPostScript(graphPtr, textTagPtr->normalBg);
	Tcl_AppendResult(graphPtr->interp, "} true ", (char *)NULL);
    }
    sprintf(graphPtr->scratchPtr, "%g %g %d %d %d %g (%s) DrawText\n",
	centerX, centerY, width, height, textTagPtr->fontPtr->ascent,
	textTagPtr->theta, textTagPtr->text);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyText --
 *
 *	Destroys the structure containing the attributes of the text
 * 	tag.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Text attributes (GCs, colors, stipple, font, etc) get destroyed.
 *	Memory is released, X resources are freed, and the graph is
 *	redrawn.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyText(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    TextTag *textTagPtr = (TextTag *)tagPtr;

    if (textTagPtr->gc != NULL) {
	XFreeGC(graphPtr->display, textTagPtr->gc);
    }
    if (textTagPtr->bgGC != NULL) {
	Tk_FreeGC(graphPtr->display, textTagPtr->bgGC);
    }
    if (textTagPtr->text != NULL) {
	free((char *)textTagPtr->text);
    }
    if (textTagPtr->normalFg != NULL) {
	Tk_FreeColor(textTagPtr->normalFg);
    }
    if (textTagPtr->normalBg != NULL) {
	Tk_FreeColor(textTagPtr->normalBg);
    }
    if (textTagPtr->fontPtr != NULL) {
	Tk_FreeFontStruct(textTagPtr->fontPtr);
    }
    if (textTagPtr->coordArr != NULL) {
	free((char *)textTagPtr->coordArr);
    }
    if (textTagPtr->bitmap != None) {
	XFreePixmap(graphPtr->display, textTagPtr->bitmap);
    }
    free((char *)textTagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateText --
 *
 *	Allocate memory and initialize methods for the new text tag.
 *
 * Results:
 *	The pointer to the newly allocated tag structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the text tag structure.
 *
 *----------------------------------------------------------------------
 */
static Tag *
CreateText()
{
    register Tag *tagPtr;

    tagPtr = (Tag *)calloc(1, sizeof(TextTag));
    if (tagPtr != NULL) {
	tagPtr->configSpecs = textConfigSpecs;
	tagPtr->configProc = ConfigureText;
	tagPtr->destroyProc = DestroyText;
	tagPtr->displayProc = DisplayText;
	tagPtr->layoutProc = LayoutText;
	tagPtr->printProc = PrintText;
	tagPtr->type = TEXT_TAG_TYPE;
    }
    return (tagPtr);
}

/*
 *--------------------------------------------------------------
 *
 * WindowStructureProc --
 *
 *	This procedure is invoked whenever StructureNotify events
 *	occur for a window that's managed as part of a canvas window
 *	item. This procedure's only purpose is to clean up when
 *	windows are deleted.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The window is disassociated from the window item when it is
 *	deleted.
 *
 *--------------------------------------------------------------
 */
static void
WindowStructureProc(clientData, eventPtr)
    ClientData clientData;	/* Pointer to record describing window item. */
    XEvent *eventPtr;		/* Describes what just happened. */
{
    register WindowTag *windowPtr = (WindowTag *)clientData;

    if (eventPtr->type == DestroyNotify) {
	windowPtr->child = NULL;
    }
}

/*
 *--------------------------------------------------------------
 *
 * WindowRequestProc --
 *
 *	This procedure is invoked whenever a window that's associated
 *	with a window item changes its requested dimensions.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The size and location on the window of the window may change,
 *	depending on the options specified for the window item.
 *
 *--------------------------------------------------------------
 */
/* ARGSUSED */
static void
WindowRequestProc(clientData, tkwin)
    ClientData clientData;	/* Pointer to record for window item. */
    Tk_Window tkwin;		/* Window that changed its desired size. */
{
    WindowTag *winTagPtr = (WindowTag *)clientData;

    if (winTagPtr->reqWidth == 0)
	winTagPtr->width = Tk_ReqWidth(tkwin);
    if (winTagPtr->reqHeight == 0)
	winTagPtr->height = Tk_ReqHeight(tkwin);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureWindow --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or
 *	reconfigure) a window tag.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as window pathname, placement,
 *	etc. get set for tagPtr;  old resources get freed, if there
 *	were any.  The tag is eventually redisplayed.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureWindow(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    WindowTag *winTagPtr = (WindowTag *)tagPtr;

    if (winTagPtr->pathName != NULL) {
	Tk_Window child;

	child = Tk_NameToWindow(graphPtr->interp, winTagPtr->pathName,
	    graphPtr->tkwin);
	if (child == NULL) {
	    return TCL_ERROR;
	}
	if (Tk_Parent(child) != graphPtr->tkwin) {
	    Tcl_AppendResult(graphPtr->interp, "\"", winTagPtr->pathName,
		"\" is not a child of ", Tk_PathName(graphPtr->tkwin),
		(char *)NULL);
	    return TCL_ERROR;
	}
	if (child != winTagPtr->child) {
	    if (winTagPtr->child != NULL) {
		Tk_DeleteEventHandler(winTagPtr->child, StructureNotifyMask,
		    WindowStructureProc, (ClientData)winTagPtr);
		Tk_ManageGeometry(winTagPtr->child, (Tk_GeometryProc *) NULL,
		    (ClientData)NULL);
		Tk_UnmapWindow(winTagPtr->child);
	    }
	    Tk_CreateEventHandler(child, StructureNotifyMask,
		WindowStructureProc, (ClientData)winTagPtr);
	    Tk_ManageGeometry(child, WindowRequestProc, (ClientData)winTagPtr);
	}
	Tk_MapWindow(child);
	winTagPtr->child = child;
	winTagPtr->width = Tk_ReqWidth(child);
	winTagPtr->height = Tk_ReqHeight(child);
	if (winTagPtr->reqWidth > 0)
	    winTagPtr->width = winTagPtr->reqWidth;
	if (winTagPtr->reqHeight > 0)
	    winTagPtr->height = winTagPtr->reqHeight;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutWindow --
 *
 *	Calculate the layout position for a window tag.  Positional
 *	information is saved in the tag.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
LayoutWindow(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    WindowTag *winTagPtr = (WindowTag *)tagPtr;

    if (winTagPtr->child != (Tk_Window)NULL) {
	XPoint anchorPos;	/* Calculated window coordinates of window */

	anchorPos = Blt_TransformPt(graphPtr, winTagPtr->coordArr[0],
	    winTagPtr->coordArr[1], winTagPtr->axisFlags);
	anchorPos = Blt_TranslateBoxCoords(anchorPos.x, anchorPos.y,
	    winTagPtr->width, winTagPtr->height, winTagPtr->anchor);
	winTagPtr->x = anchorPos.x + winTagPtr->xOffset;
	winTagPtr->y = anchorPos.y + winTagPtr->yOffset;
    }
}

/*ARGSUSED*/
static void
DisplayWindow(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    WindowTag *winTagPtr = (WindowTag *)tagPtr;

    if (winTagPtr->child != (Tk_Window)NULL) {
	if ((winTagPtr->height != Tk_Height(winTagPtr->child)) ||
	    (winTagPtr->width != Tk_Width(winTagPtr->child)) ||
	    (winTagPtr->x != Tk_X(winTagPtr->child)) ||
	    (winTagPtr->y != Tk_Y(winTagPtr->child))) {
	    Tk_MoveResizeWindow(winTagPtr->child, winTagPtr->x, winTagPtr->y,
		winTagPtr->width, winTagPtr->height);
	}
	if (!Tk_IsMapped(winTagPtr->child)) {
	    Tk_MapWindow(winTagPtr->child);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyWindow --
 *
 *	Destroys the structure containing the attributes of the window
 *      tag.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Window is unmapped.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
DestroyWindow(graphPtr, tagPtr)
    Graph *graphPtr;		/* Not used. */
    Tag *tagPtr;
{
    WindowTag *winTagPtr = (WindowTag *)tagPtr;

    if (winTagPtr->child != NULL) {
	Tk_DeleteEventHandler(winTagPtr->child, StructureNotifyMask,
	    WindowStructureProc, (ClientData)winTagPtr);
	Tk_ManageGeometry(winTagPtr->child, (Tk_GeometryProc *) NULL,
	    (ClientData)NULL);
	Tk_DestroyWindow(winTagPtr->child);
    }
    if (winTagPtr->coordArr != NULL) {
	free((char *)winTagPtr->coordArr);
    }
    if (winTagPtr->pathName != NULL) {
	free(winTagPtr->pathName);
    }
    free((char *)winTagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateWindow --
 *
 *	Allocate memory and initialize methods for the new window tag.
 *
 * Results:
 *	The pointer to the newly allocated tag structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the window tag structure.
 *
 *----------------------------------------------------------------------
 */
static Tag *
CreateWindow()
{
    register Tag *tagPtr;

    tagPtr = (Tag *)calloc(1, sizeof(WindowTag));
    if (tagPtr != NULL) {
	tagPtr->configSpecs = windowConfigSpecs;
	tagPtr->configProc = ConfigureWindow;
	tagPtr->destroyProc = DestroyWindow;
	tagPtr->displayProc = DisplayWindow;
	tagPtr->layoutProc = LayoutWindow;
	tagPtr->printProc = NULL;
	tagPtr->type = WINDOW_TAG_TYPE;
    }
    return (tagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureLine --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or
 *	reconfigure) a line tag.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as line width, colors, dashes,
 *	etc. get set for tagPtr;  old resources get freed, if there
 *	were any.  The tag is eventually redisplayed.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigureLine(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    LineTag *lineTagPtr = (LineTag *)tagPtr;
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;

    gcMask = (GCForeground | GCBackground | GCLineWidth | GCLineStyle |
	GCCapStyle | GCJoinStyle);
    gcValues.foreground = lineTagPtr->normalFg->pixel;
    gcValues.background = lineTagPtr->normalBg->pixel;
    gcValues.cap_style = CapRound;
    gcValues.join_style = JoinRound;
    gcValues.line_style = LineSolid;
    gcValues.dash_offset = 0;
    gcValues.line_width = lineTagPtr->lineWidth;
    if (lineTagPtr->dashes > 0) {
	gcValues.line_style = LineOnOffDash;
	gcValues.dashes = lineTagPtr->dashes;
	gcMask |= (GCDashList | GCDashOffset);
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (lineTagPtr->gc != NULL) {
	Tk_FreeGC(graphPtr->display, lineTagPtr->gc);
    }
    lineTagPtr->gc = newGC;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutLine --
 *
 *	Calculate the layout position for a line tag.  Positional
 *	information is saved in the tag.  The line positions are
 *	stored in an array of points (malloc'ed).
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
LayoutLine(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    LineTag *lineTagPtr = (LineTag *)tagPtr;
    XPoint *pointArr;
    register int numPoints;
    register int i;

    if (lineTagPtr->numCoords < 2) {
	return;			/* Too few points */
    }
    pointArr = (XPoint *)malloc((lineTagPtr->numCoords / 2) * sizeof(XPoint));
    if (pointArr == NULL) {
	return;			/* Can't allocate new point array */
    }
    numPoints = 0;
    for (i = 0; i < lineTagPtr->numCoords; i += 2) {
	pointArr[numPoints] = Blt_TransformPt(graphPtr,
	    lineTagPtr->coordArr[i], lineTagPtr->coordArr[i + 1],
	    lineTagPtr->axisFlags);
	pointArr[numPoints].x += lineTagPtr->xOffset;
	pointArr[numPoints].y += lineTagPtr->yOffset;
	numPoints++;
    }
    if (lineTagPtr->pointArr != NULL) {
	free((char *)lineTagPtr->pointArr);
    }
    lineTagPtr->pointArr = pointArr;
    lineTagPtr->numPoints = numPoints;
}

static void
DisplayLine(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    LineTag *lineTagPtr = (LineTag *)tagPtr;

    if (lineTagPtr->numPoints > 0) {
	XDrawLines(graphPtr->display, graphPtr->canvas, lineTagPtr->gc,
	    lineTagPtr->pointArr, lineTagPtr->numPoints, CoordModeOrigin);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PrintLine --
 *
 *	Prints postscript commands to display the connect line.
 *	Dashed lines need to be handled specially, especially if a
 *	a background color is designated.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript output commands are saved in the interpreter
 *	(infoPtr->interp) result field.
 *
 *----------------------------------------------------------------------
 */
static void
PrintLine(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    LineTag *lineTagPtr = (LineTag *)tagPtr;

    if (lineTagPtr->numPoints > 1) {
	Blt_LineWidthToPostScript(graphPtr, lineTagPtr->lineWidth);
	Blt_ForegroundToPostScript(graphPtr, lineTagPtr->normalFg);
	if (lineTagPtr->dashes > 0) {
	    Blt_LineDashesToPostScript(graphPtr, lineTagPtr->dashes);
	    Tcl_AppendResult(graphPtr->interp, "/DashesProc {\ngsave\n",
		(char *)NULL);
	    Blt_BackgroundToPostScript(graphPtr, lineTagPtr->normalBg);
	    Blt_LineDashesToPostScript(graphPtr, 0);
	    Tcl_AppendResult(graphPtr->interp, "stroke grestore\n} def\n",
		(char *)NULL);
	} else {
	    Tcl_AppendResult(graphPtr->interp, "/DashesProc {} def\n",
		(char *)NULL);
	}
	Blt_PrintLine(graphPtr, lineTagPtr->pointArr, lineTagPtr->numPoints);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyLine --
 *
 *	Destroys the structure and attributes of a line tag.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Line attributes (GCs, colors, stipple, etc) get released.
 *	Memory is deallocated, X resources are freed.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyLine(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    LineTag *lineTagPtr = (LineTag *)tagPtr;

    if (lineTagPtr->gc != NULL) {
	Tk_FreeGC(graphPtr->display, lineTagPtr->gc);
    }
    if (lineTagPtr->normalFg != NULL) {
	Tk_FreeColor(lineTagPtr->normalFg);
    }
    if (lineTagPtr->normalBg != NULL) {
	Tk_FreeColor(lineTagPtr->normalBg);
    }
    if (lineTagPtr->coordArr != NULL) {
	free((char *)lineTagPtr->coordArr);
    }
    if (lineTagPtr->pointArr != NULL) {
	free((char *)lineTagPtr->pointArr);
    }
    free((char *)lineTagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateLine --
 *
 *	Allocate memory and initialize methods for a new line tag.
 *
 * Results:
 *	The pointer to the newly allocated tag structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the line tag structure.
 *
 *----------------------------------------------------------------------
 */
static Tag *
CreateLine()
{
    register Tag *tagPtr;

    tagPtr = (Tag *)calloc(1, sizeof(LineTag));
    if (tagPtr != NULL) {
	tagPtr->configSpecs = lineConfigSpecs;
	tagPtr->configProc = ConfigureLine;
	tagPtr->destroyProc = DestroyLine;
	tagPtr->displayProc = DisplayLine;
	tagPtr->layoutProc = LayoutLine;
	tagPtr->printProc = PrintLine;
	tagPtr->type = LINE_TAG_TYPE;
    }
    return (tagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigurePolygon --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or
 *	reconfigure) a polygon tag.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as polygon color, dashes, fillstyle,
 *	etc. get set for tagPtr;  old resources get freed, if there
 *	were any.  The tag is eventually redisplayed.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigurePolygon(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    PolygonTag *polyTagPtr = (PolygonTag *)tagPtr;
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;

    gcMask = (GCForeground | GCLineWidth | GCLineStyle |
	GCCapStyle | GCJoinStyle);
    gcValues.foreground = polyTagPtr->normalFg->pixel;
    if (polyTagPtr->normalBg != NULL) {
	gcValues.background = polyTagPtr->normalBg->pixel;
	gcMask |= GCBackground;
    }
    gcValues.cap_style = CapRound;
    gcValues.join_style = JoinRound;
    gcValues.line_style = LineSolid;
    gcValues.dash_offset = 0;
    gcValues.line_width = polyTagPtr->lineWidth;
    if (polyTagPtr->dashes > 0) {
	gcValues.line_style = LineOnOffDash;
	gcValues.dashes = polyTagPtr->dashes;
	gcMask |= (GCDashList | GCDashOffset);
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (polyTagPtr->lineGC != NULL) {
	Tk_FreeGC(graphPtr->display, polyTagPtr->lineGC);
    }
    polyTagPtr->lineGC = newGC;

    if (polyTagPtr->stipple != None) {
	gcValues.stipple = polyTagPtr->stipple;
	if (polyTagPtr->normalBg != NULL) {
	    gcValues.fill_style = FillOpaqueStippled;
	} else {
	    gcValues.fill_style = FillStippled;
	}
	gcMask |= (GCStipple | GCFillStyle);
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (polyTagPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, polyTagPtr->fillGC);
    }
    polyTagPtr->fillGC = newGC;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutPolygon --
 *
 *	Calculate the layout position for a polygon tag.  Positional
 *	information is saved in the polygon in an array of points
 *	(malloc'ed).
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
LayoutPolygon(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    PolygonTag *polyTagPtr = (PolygonTag *)tagPtr;
    XPoint *pointArr;
    register int numPoints;
    register int i;

    if (polyTagPtr->numCoords < 3) {
	return;			/* Too few points */
    }
    numPoints = (polyTagPtr->numCoords / 2) + 1;
    pointArr = (XPoint *)malloc(numPoints * sizeof(XPoint));
    if (pointArr == NULL) {
	return;			/* Can't allocate point array */
    }
    numPoints = 0;
    for (i = 0; i < polyTagPtr->numCoords; i += 2) {
	pointArr[numPoints] = Blt_TransformPt(graphPtr,
	    polyTagPtr->coordArr[i], polyTagPtr->coordArr[i + 1],
	    polyTagPtr->axisFlags);
	pointArr[numPoints].x += polyTagPtr->xOffset;
	pointArr[numPoints].y += polyTagPtr->yOffset;
	numPoints++;
    }

    /*
     * Make certain the polygon is closed
     */
    pointArr[numPoints++] = Blt_TransformPt(graphPtr, polyTagPtr->coordArr[0],
	polyTagPtr->coordArr[1], polyTagPtr->axisFlags);
    if (polyTagPtr->pointArr != NULL) {
	free((char *)polyTagPtr->pointArr);
    }
    polyTagPtr->pointArr = pointArr;
    polyTagPtr->numPoints = numPoints;
}


static void
DisplayPolygon(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    PolygonTag *polyTagPtr = (PolygonTag *)tagPtr;

    if (polyTagPtr->numPoints > 0) {
	XFillPolygon(graphPtr->display, graphPtr->canvas, polyTagPtr->fillGC,
	    polyTagPtr->pointArr, polyTagPtr->numPoints, Complex,
	    CoordModeOrigin);
    }
    if (polyTagPtr->lineWidth > 0) {
	XDrawLines(graphPtr->display, graphPtr->canvas, polyTagPtr->lineGC,
	    polyTagPtr->pointArr, polyTagPtr->numPoints, CoordModeOrigin);
    }
}


static void
PrintPolygon(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    PolygonTag *polyTagPtr = (PolygonTag *)tagPtr;

    if (polyTagPtr->numPoints < 3) {
	return;
    }
    /*
     * If a background color was specified, draw the polygon filled
     * with the background color.
     */
    if (polyTagPtr->normalBg != NULL) {
	Blt_BackgroundToPostScript(graphPtr, polyTagPtr->normalBg);
	Blt_PolygonToPostScript(graphPtr, polyTagPtr->pointArr,
	    polyTagPtr->numPoints);
    }
    /*
     * Draw the outline and/or stipple in the foreground color.
     */
    if ((polyTagPtr->lineWidth > 0) || (polyTagPtr->stipple != None)) {
	char *command;

	Blt_ForegroundToPostScript(graphPtr, polyTagPtr->normalFg);
	Blt_LinesToPostScript(graphPtr, polyTagPtr->pointArr,
	    polyTagPtr->numPoints);
	command = (polyTagPtr->lineWidth > 0)
	    ? "Stroke closepath\n" : "closepath\n";
	Tcl_AppendResult(graphPtr->interp, command, (char *)NULL);

	if (polyTagPtr->stipple != None) {
	    unsigned int width, height;

	    Tk_SizeOfBitmap(graphPtr->display, polyTagPtr->stipple, &width,
		&height);
	    Blt_StippleToPostScript(graphPtr, polyTagPtr->stipple, width,
		height, True);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyPolygon --
 *
 *	Release memory and resources allocated for the polygon element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the polygon element is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyPolygon(graphPtr, tagPtr)
    Graph *graphPtr;
    Tag *tagPtr;
{
    PolygonTag *polyTagPtr = (PolygonTag *)tagPtr;

    if (polyTagPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, polyTagPtr->fillGC);
    }
    if (polyTagPtr->lineGC != NULL) {
	Tk_FreeGC(graphPtr->display, polyTagPtr->lineGC);
    }
    if (polyTagPtr->normalBg != NULL) {
	Tk_FreeColor(polyTagPtr->normalBg);
    }
    if (polyTagPtr->normalFg != NULL) {
	Tk_FreeColor(polyTagPtr->normalFg);
    }
    if (polyTagPtr->stipple != None) {
	Tk_FreeBitmap(graphPtr->display, polyTagPtr->stipple);
    }
    if (polyTagPtr->coordArr != NULL) {
	free((char *)polyTagPtr->coordArr);
    }
    if (polyTagPtr->pointArr != NULL) {
	free((char *)polyTagPtr->pointArr);
    }
    free((char *)polyTagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreatePolygon --
 *
 *	Allocate memory and initialize methods for the new polygon tag.
 *
 * Results:
 *	The pointer to the newly allocated tag structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the polygon tag structure.
 *
 *----------------------------------------------------------------------
 */
static Tag *
CreatePolygon()
{
    register Tag *tagPtr;

    tagPtr = (Tag *)calloc(1, sizeof(PolygonTag));
    if (tagPtr != NULL) {
	tagPtr->configSpecs = polygonConfigSpecs;
	tagPtr->configProc = ConfigurePolygon;
	tagPtr->destroyProc = DestroyPolygon;
	tagPtr->displayProc = DisplayPolygon;
	tagPtr->layoutProc = LayoutPolygon;
	tagPtr->printProc = PrintPolygon;
	tagPtr->type = POLYGON_TAG_TYPE;
    }
    return (tagPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * GetTagClassType --
 *
 *	Convert the tag type string value into a numeric value.
 *
 * Results:
 *	The value representing the tag type is returned.
 *
 *----------------------------------------------------------------------
 */
static int
GetTagClassType(interp, name, typePtr)
    Tcl_Interp *interp;
    char *name;
    TagClassType *typePtr;
{
    int index;

    index = Blt_GetTokenIndex(tagNames, name, 0);
    if (index < 0) {
	Tcl_AppendResult(interp, "unknown tag type \"", name, "\": should be",
	    " text, line, polygon, bitmap, or window", (char *)NULL);
	return TCL_ERROR;
    }
    *typePtr = (TagClassType) index;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetTagCoords --
 *
 *	If no coordinates are specified (coordList is NULL), this
 *	routine returns the coordinates of a given tag. Otherwise,
 *	the Tcl coordinate list is converted to their floating point
 *	values. It will then replace the current tag coordinates.
 *
 *	Since different tag types require different number of
 *	coordinates this must be checked here.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side effects:
 *	If the tag coordinates are reset, the graph is eventually redrawn
 *	with at the new tag coordinates.
 *
 *----------------------------------------------------------------------
 */

static int
GetTagCoords(interp, tagPtr, coordList)
    Tcl_Interp *interp;
    Tag *tagPtr;
    char coordList[];
{
    register int i;
    register double *coordArr, *coordPtr;
    int minArgs, maxArgs;
    int result = TCL_ERROR;
    int numExprs;
    char **exprArr;
    double value;

    if (coordList == NULL) {
	char string[TCL_DOUBLE_SPACE];

	for (i = 0; i < tagPtr->numCoords; i++) {
	    GetExprString(interp, tagPtr->coordArr[i], string);
	    Tcl_AppendElement(interp, string);
	}
	return TCL_OK;
    }
    /* Split the list of coordinates and check the values */

    if (Tcl_SplitList(interp, coordList, &numExprs, &exprArr) != TCL_OK) {
	return TCL_ERROR;
    }
    coordArr = NULL;
    if (numExprs > 0) {
	if (numExprs & 1) {
	    interp->result = "odd number of tag coordinates specified";
	    goto error;
	}
	switch (tagPtr->type) {
	case LINE_TAG_TYPE:
	    minArgs = 4, maxArgs = 0;
	    break;
	case POLYGON_TAG_TYPE:
	    minArgs = 6, maxArgs = 0;
	    break;
	case WINDOW_TAG_TYPE:
	case BITMAP_TAG_TYPE:
	case TEXT_TAG_TYPE:
	    minArgs = 2, maxArgs = 2;
	    break;
	default:
	    interp->result = "unknown tag type";
	    goto error;
	}
	if (numExprs < minArgs) {
	    interp->result = "too few tag coordinates specified";
	    goto error;
	}
	if ((maxArgs > 0) && (numExprs > maxArgs)) {
	    interp->result = "too many tag coordinates specified";
	    goto error;
	}
	coordArr = (double *)malloc((unsigned int)numExprs * sizeof(double));
	if (coordArr == NULL) {
	    interp->result = "can't allocate coordinate array";
	    goto error;
	}
	coordPtr = coordArr;
	for (i = 0; i < numExprs; i++) {
	    if (GetExprValue(interp, exprArr[i], &value) != TCL_OK) {
		goto error;
	    }
	    *coordPtr++ = value;
	}
    }
    if (tagPtr->coordArr != NULL) {
	free((char *)tagPtr->coordArr);
    }
    tagPtr->coordArr = coordArr;
    tagPtr->numCoords = numExprs;
    tagPtr->flags |= LAYOUT_NEEDED;
    result = TCL_OK;
  error:
    free((char *)exprArr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * TagCoords --
 *
 *	Return coordinates of a given tag
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side effects:
 *	The graph is eventually redrawn with the new tag.
 *
 *----------------------------------------------------------------------
 */
static int
TagCoords(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char **argv;
{
    Tag *tagPtr;
    Tcl_HashEntry *entryPtr;

    entryPtr = Tcl_FindHashEntry(&(graphPtr->tagTable), argv[0]);
    if (entryPtr == NULL) {
	Tcl_AppendResult(graphPtr->interp, "can't find tag \"", argv[0], "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tagPtr = (Tag *)Tcl_GetHashValue(entryPtr);
    if (GetTagCoords(graphPtr->interp, tagPtr, argv[1]) != TCL_OK) {
	return TCL_ERROR;
    }
    if (argc > 1) {
	graphPtr->flags |= LAYOUT_NEEDED;
	Blt_RedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TagIds --
 *
 *	Returns a list of tag identifiers in interp->result;
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static int
TagIds(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char **argv;
{
    Tag *tagPtr;
    Blt_ListEntry *entryPtr;

    for (entryPtr = Blt_FirstListEntry(&(graphPtr->tagList)); entryPtr != NULL;
	entryPtr = Blt_NextListEntry(entryPtr)) {
	tagPtr = (Tag *)Blt_GetListValue(entryPtr);
	if ((argc == 0) || (Tcl_StringMatch(tagPtr->id, argv[0]))) {
	    Tcl_AppendElement(graphPtr->interp, tagPtr->id);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureTag --
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureTag(graphPtr, tagId, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    char *tagId;		/* Tag identifier */
    int argc;			/* Number of options */
    char *argv[];		/* List of tag options */
{
    Tcl_HashEntry *entryPtr;
    Tag *tagPtr;
    int result;
    int flags = TK_CONFIG_ARGV_ONLY;

    entryPtr = Tcl_FindHashEntry(&(graphPtr->tagTable), tagId);
    if (entryPtr == NULL) {
	Tcl_AppendResult(graphPtr->interp, "can't find tag \"", tagId, "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tagPtr = (Tag *)Tcl_GetHashValue(entryPtr);
    if (argc == 0) {
	return (Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin,
		tagPtr->configSpecs, (char *)tagPtr,
		(char *)NULL, flags));
    } else if (argc == 1) {
	return (Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin,
		tagPtr->configSpecs, (char *)tagPtr,
		argv[0], flags));
    }
    if (Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin,
	    tagPtr->configSpecs, argc, argv, (char *)tagPtr,
	    flags) != TCL_OK) {
	return TCL_ERROR;
    }
    result = (*tagPtr->configProc) (graphPtr, tagPtr);
    tagPtr->flags |= LAYOUT_NEEDED;
    graphPtr->flags |= LAYOUT_NEEDED;
    Blt_RedrawGraph(graphPtr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * TypeOfTag --
 *
 *	Convert the tag type value into a string.
 *
 * Results:
 *	The string representing the tag type is returned.
 *
 *----------------------------------------------------------------------
 */
static char *
TypeOfTag(tagPtr)
    Tag *tagPtr;
{
    return tagNames[tagPtr->type];
}

/*
 *----------------------------------------------------------------------
 *
 * CreateTag --
 *
 *	This procedure creates and initializes a new tag.
 *
 * Results:
 *	The return value is a pointer to a structure describing
 *	the new element.  If an error occurred, then the return
 *	value is NULL and an error message is left in interp->result.
 *
 * Side effects:
 *	Memory is allocated, etc.
 *
 *----------------------------------------------------------------------
 */
static int
CreateTag(graphPtr, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    int argc;
    char *argv[];
{
    Tag *tagPtr;
    char tagId[80];
    Blt_ListEntry *listPtr;
    Tcl_HashEntry *hashPtr;
    int dummy;
    TagClassType type;

    if (GetTagClassType(graphPtr->interp, argv[0], &type) != TCL_OK) {
	return TCL_ERROR;
    }
    tagPtr = NULL;
    switch (type) {
    case LINE_TAG_TYPE:
	tagPtr = CreateLine();
	break;
    case WINDOW_TAG_TYPE:
	tagPtr = CreateWindow();
	break;
    case BITMAP_TAG_TYPE:
	tagPtr = CreateBitmap();
	break;
    case POLYGON_TAG_TYPE:
	tagPtr = CreatePolygon();
	break;
    case TEXT_TAG_TYPE:
	tagPtr = CreateText();
	break;
    default:
	graphPtr->interp->result = "unknown tag type";
	return TCL_ERROR;
    }
    if (tagPtr == NULL) {
	graphPtr->interp->result = "can't allocate new tag";
	return TCL_ERROR;
    }
    tagPtr->type = (TagClassType) type;
    tagPtr->axisFlags = STD_AXES_MASK;
    tagPtr->typeProc = TypeOfTag;
    sprintf(tagId, "TAG%d", graphPtr->nextTagId++);
    tagPtr->id = Tk_GetUid(tagId);
    listPtr = Blt_CreateListEntry(tagPtr->id);
    Blt_SetListValue(listPtr, tagPtr);
    Blt_LinkListAfter(&(graphPtr->tagList), listPtr,
	(Blt_ListEntry *)NULL);

    hashPtr = Tcl_CreateHashEntry(&(graphPtr->tagTable), tagPtr->id,
	&dummy);
    Tcl_SetHashValue(hashPtr, (ClientData)tagPtr);
    if (GetTagCoords(graphPtr->interp, tagPtr, argv[1]) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin,
	    tagPtr->configSpecs, argc - 2, argv + 2,
	    (char *)tagPtr, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((*tagPtr->configProc) (graphPtr, tagPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    graphPtr->interp->result = tagPtr->id;
    tagPtr->flags |= LAYOUT_NEEDED;
    graphPtr->flags |= LAYOUT_NEEDED;
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteTag --
 *
 *	Deletes the tag given by tagId.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new display list.
 *
 *----------------------------------------------------------------------
 */
static int
DeleteTag(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char *argv[];
{
    Tcl_HashEntry *entryPtr;
    Tag *tagPtr;
    register int i;

    for (i = 0; i < argc; i++) {
	entryPtr = Tcl_FindHashEntry(&(graphPtr->tagTable), argv[i]);
	if (entryPtr == NULL) {
	    Tcl_AppendResult(graphPtr->interp, "can't find tag \"", argv[i],
		"\"", (char *)NULL);
	    return TCL_ERROR;
	}
	tagPtr = (Tag *)Tcl_GetHashValue(entryPtr);
	Tcl_DeleteHashEntry(entryPtr);
	Blt_DeleteListEntry(&(graphPtr->tagList),
	    Blt_FindListEntry(&(graphPtr->tagList), argv[i]));
	(*tagPtr->destroyProc) (graphPtr, tagPtr);
    }
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TagAfter --
 *
 *	Reorders the tag (given by the first tagId) after the another
 *      tag (given by the second tagId) in the tag display list.  If
 *	no second tagId is given, the tag is placed at the end of list.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new display list.
 *
 *----------------------------------------------------------------------
 */
static int
TagAfter(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char *argv[];
{
    Blt_ListEntry *entryPtr, *afterPtr;

    entryPtr = Blt_FindListEntry(&(graphPtr->tagList), argv[0]);
    if (entryPtr == NULL) {
	Tcl_AppendResult(graphPtr->interp, "can't find tag \"", argv[0], "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    afterPtr = NULL;
    if (argc == 2) {
	afterPtr = Blt_FindListEntry(&(graphPtr->tagList), argv[1]);
	if (afterPtr == NULL) {
	    Tcl_AppendResult(graphPtr->interp, "can't find after tag \"",
		argv[1], "\"", (char *)NULL);
	    return TCL_ERROR;
	}
    }
    Blt_UnlinkListEntry(&(graphPtr->tagList), entryPtr);
    Blt_LinkListAfter(&(graphPtr->tagList), entryPtr, afterPtr);
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TagBefore --
 *
 *	Reorders the tag (given by the first tagId) before the another
 *      tag (given by the second tagId) in the tag display list.  If
 *	no second tagId is given, the tag is placed at the beginning of
 *	the list.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new display list.
 *
 *----------------------------------------------------------------------
 */
static int
TagBefore(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char *argv[];
{
    Blt_ListEntry *entryPtr, *beforePtr;

    entryPtr = Blt_FindListEntry(&(graphPtr->tagList), argv[0]);
    if (entryPtr == NULL) {
	Tcl_AppendResult(graphPtr->interp, "can't find tag \"", argv[0], "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    beforePtr = NULL;
    if (argc == 2) {
	beforePtr = Blt_FindListEntry(&(graphPtr->tagList), argv[1]);
	if (beforePtr == NULL) {
	    Tcl_AppendResult(graphPtr->interp, "can't find before tag \"",
		argv[1], "\"", (char *)NULL);
	    return TCL_ERROR;
	}
    }
    Blt_UnlinkListEntry(&(graphPtr->tagList), entryPtr);
    Blt_LinkListBefore(&(graphPtr->tagList), entryPtr, beforePtr);
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TagType --
 *
 *	Returns a symbolic name for the type of the tag whose ID is
 *	given.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *	interp->result contains the symbolic type of the tag.
 *
 *----------------------------------------------------------------------
 */
static int
TagType(graphPtr, tagId)
    Graph *graphPtr;
    char *tagId;
{
    Tag *tagPtr;
    Tcl_HashEntry *entryPtr;

    entryPtr = Tcl_FindHashEntry(&(graphPtr->tagTable), tagId);
    if (entryPtr != NULL) {
	tagPtr = (Tag *)Tcl_GetHashValue(entryPtr);
	graphPtr->interp->result = (*tagPtr->typeProc) (tagPtr);
    }
    return TCL_OK;
}

/* Public routines */

/*
 *--------------------------------------------------------------
 *
 * Blt_TagCmd --
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
int
Blt_TagCmd(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char **argv;
{
    int result = TCL_ERROR;
    char c;
    int length;

    if (argc < 3) {
	Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
	    argv[0], " tag option name ?args?\"", NULL);
	return TCL_ERROR;
    }
    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 'c') && (length > 1) &&
	(strncmp(argv[2], "create", length) == 0)) {
	if (argc < 4) {
	    Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
		argv[0], " tag create type coords ?options?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	result = CreateTag(graphPtr, argc - 3, argv + 3);
    } else if ((c == 'c') && (length > 2) &&
	(strncmp(argv[2], "configure", length) == 0)) {
	if (argc < 4) {
	    Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
		argv[0], " tag configure tagId ?options?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	result = ConfigureTag(graphPtr, argv[3], argc - 4, argv + 4);
    } else if ((c == 'd') && (strncmp(argv[2], "delete", length) == 0)) {
	if (argc < 4) {
	    Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
		argv[0], " tag delete tagId ?tagId..?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	result = DeleteTag(graphPtr, argc - 3, argv + 3);
    } else if ((c == 't') && (length > 1) &&
	(strncmp(argv[2], "type", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
		argv[0], " tag type tagId\"", (char *)NULL);
	    return TCL_ERROR;
	}
	result = TagType(graphPtr, argv[2]);
    } else if ((c == 'a') && (strncmp(argv[2], "after", length) == 0)) {
	if (argc < 4 || argc > 5) {
	    Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
		argv[0], " tag after tagId ?afterTagId?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	result = TagAfter(graphPtr, argc - 3, argv + 3);
    } else if ((c == 'b') && (strncmp(argv[2], "before", length) == 0)) {
	if (argc < 4 || argc > 5) {
	    Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
		argv[0], " tag before tagId ?beforeTagId?\"", (char *)NULL);
	}
	result = TagBefore(graphPtr, argc - 3, argv + 3);
    } else if ((c == 'c') && (length > 2) &&
	(strncmp(argv[2], "coords", length) == 0)) {
	if (argc < 4) {
	    Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
		argv[0], " tag coords tagId ?coords?\"", (char *)NULL);
	}
	result = TagCoords(graphPtr, argc - 3, argv + 3);
    } else if ((c == 'i') && (strncmp(argv[2], "ids", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
		argv[0], " tag tagids ?pattern?\"", (char *)NULL);
	}
	result = TagIds(graphPtr, argc - 3, argv + 3);
    } else {
	Tcl_AppendResult(graphPtr->interp, "bad tag option \"", argv[2], "\":\
 should be after, before, configure, create, coords, delete, ids, or type",
	    (char *)NULL);
	return TCL_ERROR;
    }
    return (result);
}
