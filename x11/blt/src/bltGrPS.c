
/*
 * bltGrPS.c --
 *
 *      This module implements a graph widget for
 *      the Tk toolkit.
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
/*
 * -----------------------------------------------------------------
 *
 * PostScript routines to print the graph
 *
 * -----------------------------------------------------------------
 */

#include "blt.h"
#include "bltGraph.h"
#include "bltGrElem.h"
#include "bltGrTag.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define RED(c)          ((double)(c)->red/65536.0)
#define GREEN(c)        ((double)(c)->green/65536.0)
#define BLUE(c)         ((double)(c)->blue/65536.0)

#define PSMAXPATH	1450	/* Max length of a PS path */

#define SET_BG_COLOR 	0
#define SET_FG_COLOR	1

typedef enum ColorLevels {
    MONOCHROME_LEVEL, GRAYSCALE_LEVEL, FULLCOLOR_LEVEL
} ColorLevel;

static char *modeTokens[] =
{
    "monochrome", "grayscale", "color", (char *)NULL,
};

/*
 * Sun's bundled and unbundled C compilers choke on static function
 * typedefs (but they can handle "extern") such as
 *
 * 	static Tk_OptionParseProc parseProc;
 *  	static Tk_OptionPrintProc printProc;
 *
 * Workaround: provide forward declarations here.
 */
static int ParseColorMode _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *value, char *widgRec,
	int offset));
static char *PrintColorMode _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption ColorModeOption =
{
    ParseColorMode, PrintColorMode, (ClientData)0,
};

/*
 * -------------------------------------------------------------------
 *
 * PostScript --
 *
 * 	Structure contains information specific to the outputing of
 *	PostScript commands to print the graph.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    PostScriptConfigureProc *configProc;
    PostScriptPrintProc *printProc;
    PostScriptDestroyProc *destroyProc;

    int pageWidth, pageHeight;	/* Requested page extents */
    int xOffset, yOffset;	/* Requested page anchor */
    Tk_Anchor anchor;		/* Anchor type for positioning page */
    ColorLevel colorLevel;	/* If zero, use BW color scheme */
    char *colorVarName;		/* If non-NULL, name of color map variable
				 * containing PostScript color translations */
    char *fontVarName;		/* If non-NULL, name of font map varible */
    int landscape;		/* If non-zero, rotation page layout 90
				 * degrees */

    int x, y;			/* Actual coordinates of anchor in pica */
    int width, height;		/* Actual extents of window in pica */

    double pointScale;		/* Scale factor to convert window coordinates
				 * to printer points (pica) */
} PostScript;

#define DEF_PS_COLOR_MAP	(char *)NULL
#define DEF_PS_COLOR_MODE	"color"
#define DEF_PS_FONT_MAP		(char *)NULL
#define DEF_PS_LANDSCAPE	"0"
#define DEF_PS_PAGE_ANCHOR	"nw"
#define DEF_PS_PAGE_HEIGHT	"0"
#define DEF_PS_PAGE_WIDTH	"0"
#define DEF_PS_PAGE_X_OFFSET	"1i"
#define DEF_PS_PAGE_Y_OFFSET	"1i"

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_CUSTOM, "-colormode", "colorMode", (char *)NULL,
	DEF_PS_COLOR_MODE, Tk_Offset(PostScript, colorLevel),
	TK_CONFIG_DONT_SET_DEFAULT, &ColorModeOption},
    {TK_CONFIG_STRING, "-colormap", "colorMap", (char *)NULL,
	DEF_PS_COLOR_MAP, Tk_Offset(PostScript, colorVarName), 0},
    {TK_CONFIG_STRING, "-fontmap", "fontMap", (char *)NULL,
	DEF_PS_FONT_MAP, Tk_Offset(PostScript, fontVarName), 0},
    {TK_CONFIG_BOOLEAN, "-landscape", "landscape", (char *)NULL,
	DEF_PS_LANDSCAPE, Tk_Offset(PostScript, landscape),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_ANCHOR, "-pageanchor", "pageAnchor", (char *)NULL,
	DEF_PS_PAGE_ANCHOR, Tk_Offset(PostScript, anchor),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-pageheight", "pageHeight", (char *)NULL,
	DEF_PS_PAGE_HEIGHT, Tk_Offset(PostScript, pageHeight),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-pagewidth", "pageWidth", (char *)NULL,
	DEF_PS_PAGE_WIDTH, Tk_Offset(PostScript, pageWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-pagex", "pageX", (char *)NULL,
	DEF_PS_PAGE_X_OFFSET, Tk_Offset(PostScript, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-pagey", "pageY", (char *)NULL,
	DEF_PS_PAGE_Y_OFFSET, Tk_Offset(PostScript, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

#ifndef NO_INLINE_PROLOG
#include "bltGrPS.h"
#endif /*NO_INLINE_PROLOG*/

extern int Blt_ComputeLayout _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_LayoutGraph _ANSI_ARGS_((Graph *graphPtr));

/*
 *----------------------------------------------------------------------
 *
 * ParseColorMode --
 *
 *	Convert the string representation of a PostScript color mode
 * 	into the enumerated type representing the color level:
 *
 *		2 - Full color,
 *		1 - Grey scale, and
 *		0 - Monochrome.
 *
 * Results:
 *	The return value is a standard Tcl result.  The color level is
 *	written into the page layout information structure.
 *
 * Side Effects:
 *	Future invocations of the "postscript" widget command will use
 *	this variable to determine how color information will be displayed
 *	in the PostScript output it produces.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseColorMode(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* New legend position string */
    char *widgRec;		/* Graph widget record */
    int offset;			/* Offset of colorLevel field in record */
{
    ColorLevel *colorLevel = (ColorLevel *)(widgRec + offset);
    int index;

    if (value == NULL || *value == '\0') {
	*colorLevel = FULLCOLOR_LEVEL;
	return TCL_OK;
    }
    index = Blt_GetTokenIndex(modeTokens, value, 0);
    if (index < 0) {
	Tcl_AppendResult(interp, "bad color mode \"", value,
	    "\": should be ", "\"color\", \"grayscale\", or \"monochrome\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    *colorLevel = (ColorLevel)index;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintColorMode --
 *
 *	Convert the current color level into the string representing a
 *	valid color mode.
 *
 * Results:
 *	The string representing the color mode is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintColorMode(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* PostScript structure record */
    int offset;			/* field of colorLevel in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    ColorLevel level = *(ColorLevel *)(widgRec + offset);

    return (modeTokens[level]);
}

/*
 *----------------------------------------------------------------------
 *
 * XColorToPostScript --
 *
 *	Convert the a XColor (from its RGB values) to a PostScript
 *	command.  If a Tcl color map variable exists, it will be
 *	consulted for a PostScript translation based upon the color name.
 *
 * Results:
 *	The string representing the color mode is returned.
 *
 *----------------------------------------------------------------------
 */
static void
XColorToPostScript(graphPtr, colorPtr, fgOrBg)
    Graph *graphPtr;
    XColor *colorPtr;		/* Color value to be converted */
    int fgOrBg;			/* If non-zero, this represents a foreground
				 * color, otherwise a background color */
{
    PostScript *psPtr = (PostScript *)graphPtr->postscript;

    if (psPtr->colorVarName != NULL) {
	char *colorDesc;

	colorDesc = Tcl_GetVar2(graphPtr->interp, psPtr->colorVarName,
	    Tk_NameOfColor(colorPtr), 0);
	if (colorDesc != NULL) {
	    Tcl_AppendResult(graphPtr->interp, colorDesc, " ", (char *)NULL);
	    return;
	}
    }
    sprintf(graphPtr->scratchPtr, "%g %g %g %s  ",
	RED(colorPtr), GREEN(colorPtr), BLUE(colorPtr),
	(fgOrBg) ? "SetFgColor" : "SetBgColor");
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}

/*
 *----------------------------------------------------------------------
 *
 * ReverseBits --
 *
 *	Convert a byte from a X image into PostScript image order.
 *	This requires the nybbles to be reversed but also their bit values.
 *
 * Results:
 *	The converted byte is returned.
 *
 *----------------------------------------------------------------------
 */
static unsigned char
ReverseBits(byte)		/* Reverse bits */
    register unsigned char byte;
{
    byte = ((byte >> 1) & 0x55) | ((byte << 1) & 0xaa);
    byte = ((byte >> 2) & 0x33) | ((byte << 2) & 0xcc);
    byte = ((byte >> 4) & 0x0f) | ((byte << 4) & 0xf0);
    return (byte);
}

/*
 * -----------------------------------------------------------------
 *
 * XBitmapToPostScript --
 *
 *      Output a PostScript image string of the given bitmap image.
 *      It is assumed the image is one bit deep and a zero value
 *      indicates an off-pixel.  To convert to PostScript, the
 *      bits need to be reversed from the X11 image order.
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      A PostScript image string is output to the file pointer in
 *      the page layout structure.
 *
 * -----------------------------------------------------------------
 */
static void
XBitmapToPostScript(graphPtr, bitmap, width, height)
    Graph *graphPtr;
    Pixmap bitmap;
    unsigned int width, height;
{
    register unsigned int byte = 0;
    register int x, y, bitPos;
    unsigned long pixel;
    XImage *imagePtr;
    int byteCount = 0;

    imagePtr = XGetImage(graphPtr->display, bitmap, 0, 0, width, height,
	1, ZPixmap);
    Tcl_AppendResult(graphPtr->interp, "<", (char *)NULL);
    bitPos = 0;			/* Inhibit compiler warning */
    for (y = 0; y < height; y++) {
	byte = 0;
	for (x = 0; x < width; x++) {
	    pixel = XGetPixel(imagePtr, x, y);
	    bitPos = x % 8;
	    byte |= (unsigned char)(pixel << bitPos);
	    if (bitPos == 7) {
		sprintf(graphPtr->scratchPtr, "%02x", ReverseBits(byte));
		Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr,
		    (char *)NULL);
		byteCount++;
		byte = 0;
	    }
	    if (byteCount >= 30) {
		Tcl_AppendResult(graphPtr->interp, "\n", (char *)NULL);
		byteCount = 0;
	    }
	}
	if (bitPos != 7) {
	    sprintf(graphPtr->scratchPtr, "%02x", ReverseBits(byte));
	    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr,
		(char *)NULL);
	    byteCount++;
	}
    }
    Tcl_AppendResult(graphPtr->interp, "> ", (char *)NULL);
    XDestroyImage(imagePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * NameOfAtom --
 *
 *	Convenience routine to wrap around Tk_GetAtomName.  Returns
 *	NULL instead of "?bad atom?" is the atom cannot be found.
 *
 * Results:
 *	The name of the atom is returned if found. Otherwise NULL.
 *
 *----------------------------------------------------------------------
 */
static char *
NameOfAtom(tkwin, atom)
    Tk_Window tkwin;
    Atom atom;
{
    char *result;

    result = Tk_GetAtomName(tkwin, atom);
    if (strcmp(result, "?bad atom?") == 0) {
	return NULL;
    }
    return (result);
}

/*
 * -----------------------------------------------------------------
 *
 * XFontStructToPostScript --
 *
 *      Map X11 font to a PostScript font. Currently, only fonts
 *	whose FOUNDRY property are "Adobe" are converted. Simply
 *	gets the XA_FULL_NAME and XA_FAMILY properties and pieces
 *	together a PostScript fontname.
 *
 * Results:
 *      Returns the mapped PostScript font name is one is possible.
 *	Otherwise NULL.
 *
 * -----------------------------------------------------------------
 */
static char *
XFontStructToPostScript(tkwin, fontPtr)
    Tk_Window tkwin;		/* Window to query for atoms */
    XFontStruct *fontPtr;	/* Font structure to map to name */
{
    Atom atom;
    char *fullName, *family, *foundry;
    register char *src, *dest;
    char *start;
    static char string[200];	/* What size? */

    if (XGetFontProperty(fontPtr, XA_FULL_NAME, &atom) == False) {
	return NULL;
    }
    fullName = NameOfAtom(tkwin, atom);
    if (fullName == NULL) {
	return NULL;
    }
    family = foundry = NULL;
    if (XGetFontProperty(fontPtr, Tk_InternAtom(tkwin, "FOUNDRY"), &atom)) {
	foundry = NameOfAtom(tkwin, atom);
    }
    if (XGetFontProperty(fontPtr, XA_FAMILY_NAME, &atom)) {
	family = NameOfAtom(tkwin, atom);
    }
    /*
     * Try to map font only if foundry is Adobe
     */
    if ((foundry == NULL) || (strcmp(foundry, "Adobe") != 0) ||
	(family == NULL)) {
#ifdef notdef
	fprintf(stderr, "huh? Full name (%s) for non-PS font\n", fullName);
#endif
	return NULL;
    }
    src = fullName + strlen(family);
    /*
     * Special case: Fix mapping of "New Century Schoolbook"
     */
    if ((*family == 'N') && (strcmp(family, "New Century Schoolbook") == 0)) {
	family = "NewCenturySchlbk";
    }
    /*
     * PostScript font name is in the form <family>-<type face>
     */
    sprintf(string, "%s-", family);
    dest = start = string + strlen(string);
    /*
     * Append the type face (part of the full name trailing the family name)
     * to the the PostScript font name, removing any spaces
     *
     * ex. " Bold Italic" ==> "BoldItalic"
     */
    while (*src != '\0') {
	if (*src != ' ') {
	    *dest++ = *src;
	}
	src++;
    }
    if (dest == start) {
	--dest;			/* Remove '-' to leave just the family name */
    }
    *dest = '\0';		/* Make a valid string */
    return (string);
}

/*
 * -------------------------------------------------------------------
 * Routines to convert X drawing functions to PostScript commands.
 * -------------------------------------------------------------------
 */
void
Blt_BackgroundToPostScript(graphPtr, colorPtr)
    Graph *graphPtr;
    XColor *colorPtr;
{
    XColorToPostScript(graphPtr, colorPtr, SET_BG_COLOR);
    Tcl_AppendResult(graphPtr->interp, "\n", (char *)NULL);
}

void
Blt_ForegroundToPostScript(graphPtr, colorPtr)
    Graph *graphPtr;
    XColor *colorPtr;
{
    XColorToPostScript(graphPtr, colorPtr, SET_FG_COLOR);
    Tcl_AppendResult(graphPtr->interp, "\n", (char *)NULL);
}

void
Blt_LineWidthToPostScript(graphPtr, lineWidth)
    Graph *graphPtr;
    int lineWidth;
{
    if (lineWidth < 1) {
	lineWidth = 1;
    }
    sprintf(graphPtr->scratchPtr, "%d setlinewidth\n", lineWidth);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}

void
Blt_LineDashesToPostScript(graphPtr, dashes)
    Graph *graphPtr;
    int dashes;
{
    if (dashes < 0) {
	dashes = 0;
    }
    sprintf(graphPtr->scratchPtr, "%d SetDashes\n", dashes);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}

void
Blt_SetLineAttributes(graphPtr, colorPtr, lineWidth, dashes)
    Graph *graphPtr;
    XColor *colorPtr;
    int lineWidth;
    int dashes;
{
    Blt_ForegroundToPostScript(graphPtr, colorPtr);
    Blt_LineWidthToPostScript(graphPtr, lineWidth);
    Blt_LineDashesToPostScript(graphPtr, dashes);
}

void
Blt_RectangleToPostScript(graphPtr, x, y, width, height)
    Graph *graphPtr;
    int x, y;
    unsigned int width, height;
{
    sprintf(graphPtr->scratchPtr, "%d %d %d %d Box Fill\n", x, y, width,
	height);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}

void
Blt_LinesToPostScript(graphPtr, pointArr, numPoints)
    Graph *graphPtr;
    XPoint *pointArr;
    int numPoints;
{
    register int i;

    sprintf(graphPtr->scratchPtr, "newpath %d %d moveto\n", pointArr[0].x,
	pointArr[0].y);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    for (i = 1; i < numPoints; i++) {
	sprintf(graphPtr->scratchPtr, "%d %d lineto\n", pointArr[i].x,
	    pointArr[i].y);
	Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    }
}

void
Blt_PolygonToPostScript(graphPtr, pointArr, numPoints)
    Graph *graphPtr;
    XPoint *pointArr;
    int numPoints;
{
    Blt_LinesToPostScript(graphPtr, pointArr, numPoints);
    sprintf(graphPtr->scratchPtr, "%d %d lineto closepath Fill\n",
	pointArr[0].x, pointArr[0].y);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}

void
Blt_SegmentsToPostScript(graphPtr, segArr, numSegments)
    Graph *graphPtr;
    XSegment *segArr;
    int numSegments;
{
    register int i;

    for (i = 0; i < numSegments; i++) {
	sprintf(graphPtr->scratchPtr, "%d %d %d %d DrawSegment\n",
	    segArr[i].x1, segArr[i].y1, segArr[i].x2, segArr[i].y2);
	Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    }
}

void
Blt_RectanglesToPostScript(graphPtr, rectArr, numRects)
    Graph *graphPtr;
    XRectangle rectArr[];
    int numRects;
{
    register int i;

    for (i = 0; i < numRects; i++) {
	Blt_RectangleToPostScript(graphPtr, rectArr[i].x, rectArr[i].y,
	    rectArr[i].width, rectArr[i].height);
    }
}

/*
 * The Border structure used internally by the Tk_3D* routines.
 * The following is a copy of it from tk3d.c.
 */
typedef struct {
    Display *display;		/* Display for which the resources below are
                                 * allocated. */
    int refCount;		/* Number of different users of this border.*/
    XColor *bgColorPtr;		/* Background color (intensity between
                                 * lightColorPtr and darkColorPtr). */
    XColor *lightColorPtr;	/* Color used for lighter areas of border
                                 * (must free this when deleting structure).*/
    XColor *darkColorPtr;	/* Color for darker areas (must free when
                                 * deleting structure). */
    Pixmap shadow;		/* Stipple pattern to use for drawing
                                 * lighter-shadow-ed areas.  Only used on
                                 * monochrome displays;  on color displays
                                 * this is None. */
    GC lightGC;			/* Used to draw lighter parts of the border.*/
    GC darkGC;			/* Used to draw darker parts of the the
                                 * border. */
    GC bgGC;			/* Used (if necessary) to draw areas in the
                                 * background color. */
    Tcl_HashEntry *hashPtr;	/* Entry in borderTable (needed in order to
                                 * delete structure). */
} Border;


void
Blt_Print3DRectangle(graphPtr, border, x, y, width, height, borderWidth,
    relief)
    Graph *graphPtr;		/* File pointer to write PS output. */
    Tk_3DBorder border;		/* Token for border to draw. */
    int x, y;			/* Coordinates of rectangle */
    unsigned int width, height;	/* Region to be drawn. */
    int borderWidth;		/* Desired width for border, in pixels. */
    int relief;			/* Should be either TK_RELIEF_RAISED or
                                 * TK_RELIEF_SUNKEN;  indicates position of
                                 * interior of window relative to exterior. */
{
    Border *borderPtr = (Border *) border;
    XColor lightColor, darkColor;
    XColor *lightColorPtr, *darkColorPtr;
    XColor *top, *bottom;
    XPoint points[7];
    Tk_Window tkwin = graphPtr->tkwin;
    int twiceWidth = (borderWidth * 2);

    if ((width < twiceWidth) || (height < twiceWidth)) {
	return;
    }
    if ((borderPtr->lightColorPtr == NULL) ||
	(borderPtr->darkColorPtr == NULL)) {
	Screen *screenPtr;
	Colormap colorMap;

	lightColor.pixel = borderPtr->bgColorPtr->pixel;
	screenPtr = Tk_Screen(graphPtr->tkwin);
	if (lightColor.pixel == WhitePixelOfScreen(screenPtr)) {
	    darkColor.pixel = BlackPixelOfScreen(screenPtr);
	} else {
	    darkColor.pixel = WhitePixelOfScreen(screenPtr);
	}
	colorMap = Tk_Colormap(graphPtr->tkwin);
	XQueryColor(graphPtr->display, colorMap, &lightColor);
	lightColorPtr = &lightColor;
	XQueryColor(Tk_Display(tkwin), colorMap, &darkColor);
	darkColorPtr = &darkColor;
    } else {
	lightColorPtr = borderPtr->lightColorPtr;
	darkColorPtr = borderPtr->darkColorPtr;
    }

    /*
     * Handle grooves and ridges with recursive calls.
     */

    if ((relief == TK_RELIEF_GROOVE) || (relief == TK_RELIEF_RIDGE)) {
	int halfWidth, insideOffset;

	halfWidth = borderWidth / 2;
	insideOffset = borderWidth - halfWidth;
	Blt_Print3DRectangle(graphPtr, border, x, y, width, height, halfWidth,
	    (relief == TK_RELIEF_GROOVE) ? TK_RELIEF_SUNKEN :
	    TK_RELIEF_RAISED);
	Blt_Print3DRectangle(graphPtr, border, x + insideOffset,
	    y + insideOffset, width - insideOffset * 2,
	    height - insideOffset * 2, halfWidth,
	    (relief == TK_RELIEF_GROOVE) ? TK_RELIEF_RAISED :
	    TK_RELIEF_SUNKEN);
	return;
    }
    if (relief == TK_RELIEF_RAISED) {
	top = lightColorPtr;
	bottom = darkColorPtr;
    } else if (relief == TK_RELIEF_SUNKEN) {
	top = darkColorPtr;
	bottom = lightColorPtr;
    } else {
	top = bottom = borderPtr->bgColorPtr;
    }
    Blt_BackgroundToPostScript(graphPtr, bottom);
    Blt_RectangleToPostScript(graphPtr, x, y + height - borderWidth,
	width, (unsigned int)borderWidth);
    Blt_RectangleToPostScript(graphPtr, x + width - borderWidth, y,
	(unsigned int)borderWidth, (unsigned int)height);
    points[0].x = points[1].x = points[6].x = x;
    points[0].y = points[6].y = y + height;
    points[1].y = points[2].y = y;
    points[2].x = x + width;
    points[3].x = x + width - borderWidth;
    points[3].y = points[4].y = y + borderWidth;
    points[4].x = points[5].x = x + borderWidth;
    points[5].y = y + height - borderWidth;
    if (relief != TK_RELIEF_FLAT) {
	Blt_BackgroundToPostScript(graphPtr, top);
    }
    Blt_PolygonToPostScript(graphPtr, points, 7);
}

void
Blt_3DRectangleToPostScript(graphPtr, border, x, y, width, height,
    borderWidth, relief)
    Graph *graphPtr;
    Tk_3DBorder border;		/* Token for border to draw. */
    int x, y;			/* Coordinates of top-left of border area */
    unsigned int width, height;	/* Dimension of border to be drawn. */
    int borderWidth;		/* Desired width for border, in pixels. */
    int relief;			/* Should be either TK_RELIEF_RAISED or
                                 * TK_RELIEF_SUNKEN;  indicates position of
                                 * interior of window relative to exterior. */
{
    Border *borderPtr = (Border *) border;

    /*
     * I'm assuming that the rectangle is to be drawn as a background.
     * Setting the pen color as a foreground or background, only affects
     * the plot when the colormode option is "monochrome".
     */
    Blt_BackgroundToPostScript(graphPtr, borderPtr->bgColorPtr);
    Blt_RectangleToPostScript(graphPtr, x, y, width, height);
    Blt_Print3DRectangle(graphPtr, border, x, y, width, height, borderWidth,
	relief);
}

void
Blt_StippleToPostScript(graphPtr, bitmap, width, height, fillOrStroke)
    Graph *graphPtr;
    Pixmap bitmap;
    unsigned int width, height;
    int fillOrStroke;
{
    sprintf(graphPtr->scratchPtr, "%d %d\n", width, height);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    XBitmapToPostScript(graphPtr, bitmap, width, height);
    Tcl_AppendResult(graphPtr->interp, (fillOrStroke) ? "true" : "false",
	" StippleFill\n", (char *)NULL);
}

void
Blt_BitmapToPostScript(graphPtr, bitmap, centerX, centerY,
    width, height, theta, bgColorPtr)
    Graph *graphPtr;
    Pixmap bitmap;		/* Bitmap to be converted to PostScript */
    int centerX, centerY;	/* Bitmap's center coordinates */
    unsigned int width, height;	/* Extents of bitmap */
    double theta;		/* Degrees to rotate bitmap */
    XColor *bgColorPtr;		/* Background color of bitmap: NULL indicates
				 * no background color */
{
    if (bgColorPtr != NULL) {
	Tcl_AppendResult(graphPtr->interp, "{ ", (char *)NULL);
	XColorToPostScript(graphPtr, bgColorPtr, SET_BG_COLOR);
	Tcl_AppendResult(graphPtr->interp, "} true ", (char *)NULL);
    } else {
	Tcl_AppendResult(graphPtr->interp, "false ", (char *)NULL);
    }
    sprintf(graphPtr->scratchPtr, "%d %d %d %d %g\n", centerX, centerY,
	width, height, theta);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    XBitmapToPostScript(graphPtr, bitmap, width, height);
    Tcl_AppendResult(graphPtr->interp, " DrawBitmap\n", (char *)NULL);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_FontToPostScript --
 *
 *      Map the X font to a PostScript font and point size.
 *
 *	If a Tcl array variable was specified, each element should
 *	be indexed by the X11 font name and contain a list of 1-2
 *	elements; the PostScript font name and the desired point
 *	size.  The point size may be omitted and the X font point
 *	size will be used.
 *
 *	Otherwise, if the foundry is "Adobe", we try to do a pausible
 *	mapping looking at the full name of the font and building
 *	a string in the form of "Family-TypeFace".
 *
 * Returns:
 *      None.
 *
 * Side Effects:
 *      PostScript commands are output to change the type and the
 *      point size of the current font.
 *
 * -----------------------------------------------------------------
 */
void
Blt_FontToPostScript(graphPtr, fontPtr)
    Graph *graphPtr;
    XFontStruct *fontPtr;	/* X font to query about */
{
    PostScript *psPtr = (PostScript *)graphPtr->postscript;
    unsigned long fontProp;
    char *fontName;
    int pointSize;

    fontName = "Helvetica-Bold";/* Default font */
    pointSize = 120;		/* Default point size */
    if (XGetFontProperty(fontPtr, XA_POINT_SIZE, &fontProp) != False) {
	pointSize = (int)fontProp;
    }
    if (psPtr->fontVarName != NULL) {
	char *fontInfo;

	fontInfo = Tcl_GetVar2(graphPtr->interp, psPtr->fontVarName,
	    Tk_NameOfFontStruct(fontPtr), 0);
	if (fontInfo != NULL) {
	    int numProps;
	    char **propArr = NULL;

	    if (Tcl_SplitList(graphPtr->interp, fontInfo, &numProps,
		    &propArr) == TCL_OK) {
		fontName = propArr[0];
		if (numProps == 2) {
		    Tcl_GetInt(graphPtr->interp, propArr[1], &pointSize);
		}
	    }
	    sprintf(graphPtr->scratchPtr, "%g /%s SetFont\n",
		(double)pointSize, fontName);
	    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr,
		(char *)NULL);
	    if (propArr != (char **)NULL) {
		free((char *)propArr);
	    }
	    return;
	}
    }
    /*
     * Otherwise, try to map by querying the X font for its properties
     */
    fontName = XFontStructToPostScript(graphPtr->tkwin, fontPtr);
    if (fontName == NULL) {
	fontName = "Helvetica-Bold";	/* Default font */
    }
    sprintf(graphPtr->scratchPtr, "%g /%s SetFont\n",
	(double)pointSize / 10.0, fontName);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_TextToPostScript --
 *
 *      Output PostScript commands to print a text string. The string
 *	may be rotated at any arbitrary angle, and placed according
 *	the anchor type given. The anchor indicates how to interpret
 *	the window coordinates as an anchor for the text bounding box.
 *	If a background color is specified (i.e. bgColorPtr != NULL),
 *	output a filled rectangle the size of the bounding box in the
 *	given background color.
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      Text string is drawn using the given font and GC on the graph
 *	window at the given coordinates, anchor, and rotation
 * -----------------------------------------------------------------
 */
void
Blt_TextToPostScript(graphPtr, text, attrPtr, x, y)
    Graph *graphPtr;
    char *text;			/* Text string to print */
    TextAttributes *attrPtr;	/* Text attribute information: rotation,
				 * font, background color, anchor */
    int x, y;			/* Window coordinates of anchor */
{
    unsigned int width, height;	/* Width and height of unrotated text */
    XPoint upperLeft;		/* Point in upperleft corner of rotated box */
    double centerX, centerY;
    unsigned int rotWidth, rotHeight;

    if ((text == NULL) || (*text == '\0')) {
	return;
    }
    width = Blt_TextStringWidth(attrPtr->fontPtr, text);
    height = attrPtr->fontPtr->ascent + attrPtr->fontPtr->descent;
    Blt_GetBoundingBox(width, height, attrPtr->theta, &rotWidth, &rotHeight,
	(XPoint *)NULL);
    /*
     * Find the center of the bounding box
     */
    upperLeft = Blt_TranslateBoxCoords(x, y, rotWidth, rotHeight,
	attrPtr->anchor);
    centerX = upperLeft.x + (rotWidth * 0.5);
    centerY = upperLeft.y + (rotHeight * 0.5);

    Blt_FontToPostScript(graphPtr, attrPtr->fontPtr);
    Blt_ForegroundToPostScript(graphPtr, attrPtr->fgColorPtr);

    if (attrPtr->bgColorPtr == (XColor *)NULL) {
	Tcl_AppendResult(graphPtr->interp, "false ", (char *)NULL);
    } else {
	Tcl_AppendResult(graphPtr->interp, "{ ", (char *)NULL);
	XColorToPostScript(graphPtr, attrPtr->bgColorPtr, SET_BG_COLOR);
	Tcl_AppendResult(graphPtr->interp, "} true ", (char *)NULL);
    }
    sprintf(graphPtr->scratchPtr, "%g %g %d %d %d %g (%s) DrawText\n",
	centerX, centerY, width, height, attrPtr->fontPtr->ascent,
	attrPtr->theta, text);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_PrintLine --
 *
 *      Outputs PostScript commands to print a multi-segmented line.
 *	It assumes a procedure DashesProc was previously defined.
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      Line is printed.

 * -----------------------------------------------------------------
 */

void
Blt_PrintLine(graphPtr, pointArr, numPoints)
    Graph *graphPtr;
    XPoint *pointArr;
    int numPoints;
{
    register int i;

    if (numPoints <= 0) {
	return;
    }
    sprintf(graphPtr->scratchPtr, "newpath %d %d moveto\n", pointArr[0].x,
	pointArr[0].y);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    for (i = 1; i < (numPoints - 1); i++) {
	if (i % PSMAXPATH) {
	    sprintf(graphPtr->scratchPtr, "%d %d lineto\n",
		pointArr[i].x, pointArr[i].y);
	} else {
	    sprintf(graphPtr->scratchPtr,
		"%d %d lineto\nDashesProc stroke\n\newpath %d %d moveto\n",
		pointArr[i].x, pointArr[i].y, pointArr[i].x, pointArr[i].y);
	}
	Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    }
    /*
     * Note: It's assumed that i is numPoints - 1 after finishing loop
     */
    sprintf(graphPtr->scratchPtr, "%d %d lineto\nDashesProc stroke\n",
	pointArr[i].x, pointArr[i].y);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
}



static void
DestroyPostScript(graphPtr)
    Graph *graphPtr;
{
    Tk_FreeOptions(configSpecs, (char *)graphPtr->postscript,
	graphPtr->display, 0);
    free((char *)graphPtr->postscript);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigurePostScript --
 *
 *      This procedure is invoked to print the graph in a file.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      A new PostScript file is created.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigurePostScript(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;			/* Number of options in argv vector */
    char **argv;		/* Option vector */
{
    int flags = TK_CONFIG_ARGV_ONLY;

    if (argc == 2) {
	return (Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin,
		configSpecs, (char *)graphPtr->postscript, (char *)NULL, flags));
    } else if (argc == 3) {
	return (Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin,
		configSpecs, (char *)graphPtr->postscript, argv[2], flags));
    }
    if (Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin, configSpecs,
	    argc - 2, argv + 2, (char *)graphPtr->postscript, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * --------------------------------------------------------------------------
 *
 * PrintPreamble
 *
 *    	The PostScript preamble calculates the needed translation and scaling
 *    	to make X11 coordinates compatible with PostScript.
 *
 *     +-----------------------+
 *     |        1" = 72pica    |   1" left, right, top, and bottom margin
 *     |  ------------------ur |   leaving a 6.5" x 9" area.
 *     |  |  O--->          |  |
 *     |  |  |              |  |
 *     |  |  | 6.5" = 468 pica |
 *     |  |  v              |  |
 *     |  |                 |  |                     468 pica
 *     |  |  9" = 648 pica  |  |   scaleX =  ---------------------------
 *     |  |                 |  |                  Width of window
 * 11" |  |                 |  |
 *     | ll------------------  |
 *     |     bounding box      |                     648 pica
 *     |                       |   scaleY =  ---------------------------
 *     |                       |                  Height of window
 *     |                       |
 *     |                       |  To retain the aspect ratio, we use only
 *     |                       |  the smaller of the two scales.  The Y scale
 *     |                       |  is negative since the X11 Y origin is at
 *     +-----------------------+  the top instead of the bottom.
 *              8.5"
 *
 * 	Landscape:
 *		Rotate the picture by 90 degrees
 * ---------------------------------------------------------------------
 */

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif /* HAVE_SYS_TIME_H */
#endif /* TIME_WITH_SYS_TIME */

static int
PrintPreamble(graphPtr, fileName)
    Graph *graphPtr;
    char *fileName;
{
    PostScript *psPtr = (PostScript *)graphPtr->postscript;
    long date;
    char *version;

#ifdef NO_INLINE_PROLOG
#define STRING_LENGTH 400
    char string[STRING_LENGTH + 1];
    char *libDir;
    FILE *f;

#endif /* NO_INLINE_PROLOG */
    XPoint origin;

    version = Tcl_GetVar(graphPtr->interp, "tk_version", TCL_GLOBAL_ONLY);
    if (version == NULL) {
	version = "???";
    }
    if (fileName == NULL) {
	fileName = Tk_PathName(graphPtr->tkwin);
    }
    Tcl_AppendResult(graphPtr->interp,
	"%!PS-Adobe-3.0 EPSF-3.0\n%%Pages: 1\n",
	"%%Title: (", fileName, ")\n",
	"%%DocumentNeededResources: font Helvetica Courier\n",
	(char *)NULL);

    /* Find upper left coordinate of bounding box */
    origin = Blt_TranslateBoxCoords(psPtr->x, psPtr->y,
	(unsigned int)psPtr->width, (unsigned int)psPtr->height,
	psPtr->anchor);
    sprintf(graphPtr->scratchPtr, "%%%%BoundingBox:  %d %d %d %d\n",
	origin.x, 792 - (psPtr->height + origin.y),	/* Lower left */
	origin.x + psPtr->width, (792 - origin.y));
    date = time((time_t *) NULL);
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr,
	"%%Creator: ", Tk_Class(graphPtr->tkwin),
	" (Tk version ", version, ")\n",
	"%%CreationDate: ", ctime(&date),
	"%%EndComments\n", (char *)NULL);
#ifndef NO_INLINE_PROLOG
    Tcl_AppendResult(graphPtr->interp, postScriptDefinitions, (char *)NULL);
#else
    /*
     * Read a standard prolog file from disk and insert it into
     * the PostScript.
     */
    libDir = Tcl_GetVar(graphPtr->interp, "blt_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	Tcl_ResetResult(graphPtr->interp);
	Tcl_AppendResult(graphPtr->interp,
	    "couldn't find BLT library directory:",
	    " blt_library variable doesn't exist", (char *)NULL);
	return TCL_ERROR;
    }
    sprintf(string, "%.350s/bltGraph.pro", libDir);
    f = fopen(string, "r");
    if (f == NULL) {
	Tcl_ResetResult(graphPtr->interp);
	Tcl_AppendResult(graphPtr->interp, "couldn't open prolog file \"",
	    string, "\": ", Tcl_PosixError(graphPtr->interp),
	    (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(graphPtr->interp, "\n% including file \"", string,
	"\"\n\n", (char *)NULL);
    while (fgets(graphPtr->scratchPtr, STRING_LENGTH, f) != NULL) {
	Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    }
    if (ferror(f)) {
	fclose(f);
	Tcl_ResetResult(graphPtr->interp);
	Tcl_AppendResult(graphPtr->interp, "error reading prolog file \"",
	    string, "\": ", Tcl_PosixError(graphPtr->interp),
	    (char *)NULL);
	return TCL_ERROR;
    }
    fclose(f);
#endif /* NO_INLINE_PROLOG */
    if (psPtr->landscape) {
	sprintf(graphPtr->scratchPtr,
	    "/CL %d def\n%d %d translate\n-90 rotate\n%%%%EndSetup\n\n",
	    psPtr->colorLevel, origin.x, origin.y + psPtr->height);
    } else {
	sprintf(graphPtr->scratchPtr,
	    "/CL %d def\n%d %d translate\n%%%%EndSetup\n\n",
	    psPtr->colorLevel, origin.x, origin.y);
    }
    Tcl_AppendResult(graphPtr->interp, graphPtr->scratchPtr, (char *)NULL);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------
 *
 * PrintTags --
 *
 * -----------------------------------------------------------------
 */
static void
PrintTags(graphPtr)
    Graph *graphPtr;
{
    Blt_ListEntry *entryPtr;
    register Tag *tagPtr;

    for (entryPtr = Blt_FirstListEntry(&(graphPtr->tagList)); entryPtr != NULL;
	entryPtr = Blt_NextListEntry(entryPtr)) {
	tagPtr = (Tag *)Blt_GetListValue(entryPtr);
	if ((tagPtr->printProc != NULL) &&
	    ((tagPtr->elemId == (Tk_Uid) NULL) ||
		Blt_FindListEntry(&(graphPtr->elemList), tagPtr->elemId))) {
	    Tcl_AppendResult(graphPtr->interp, "\n% ",
		(*tagPtr->typeProc) (tagPtr), " Tag\n\n",
		(char *)NULL);
	    (*tagPtr->printProc) (graphPtr, tagPtr);
	}
    }
}

/*
 * -----------------------------------------------------------------
 *
 * PrintElements --
 *
 * -----------------------------------------------------------------
 */
static void
PrintElements(graphPtr)
    Graph *graphPtr;
{
    Blt_ListEntry *entryPtr;
    register Element *elemPtr;

    for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	elemPtr = (Element *)Blt_GetListValue(entryPtr);
	Tcl_AppendResult(graphPtr->interp, "\n% Element \"", elemPtr->id,
	    "\"\n\n", (char *)NULL);
	(*elemPtr->printProc) (graphPtr, elemPtr, ELEM_NORMAL);
    }
}

static void
PrintActiveElements(graphPtr)
    Graph *graphPtr;
{
    Blt_ListEntry *entryPtr;
    register Element *elemPtr;

    for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	elemPtr = (Element *)Blt_GetListValue(entryPtr);
	if (elemPtr->flags & ACTIVE) {
	    Tcl_AppendResult(graphPtr->interp, "\n% Active Element \"",
		elemPtr->id, "\"\n\n", (char *)NULL);
	    (*elemPtr->printProc) (graphPtr, elemPtr, ELEM_ACTIVE);
	}
    }
}


static void
PrintExterior(graphPtr, regionPtr)
    Graph *graphPtr;
    XRectangle *regionPtr;	/* Interior plotting region */
{
    register int i;
    int x, y;
    XRectangle rectArr[4];
    TextAttributes textAttr;
    GraphAxis *axisPtr;

    rectArr[0].x = rectArr[0].y = rectArr[3].x = rectArr[1].x = 0;
    rectArr[0].width = rectArr[3].width = graphPtr->width;
    rectArr[0].height = regionPtr->y;
    rectArr[3].y = regionPtr->y + regionPtr->height;
    rectArr[3].height = graphPtr->height - rectArr[3].y;
    rectArr[2].y = rectArr[1].y = regionPtr->y;
    rectArr[1].width = regionPtr->x;
    rectArr[2].height = rectArr[1].height = regionPtr->height;
    rectArr[2].x = regionPtr->x + regionPtr->width;
    rectArr[2].width = graphPtr->width - rectArr[2].x;

    /* Clear the surrounding margins and clip the plotting surface */
    Blt_BackgroundToPostScript(graphPtr, Tk_3DBorderColor(graphPtr->border));
    for (i = 0; i < 4; i++) {
	Blt_RectangleToPostScript(graphPtr, rectArr[i].x,
	    rectArr[i].y, rectArr[i].width, rectArr[i].height);
    }
    /* Interior 3D border */
    if ((graphPtr->plotRelief != TK_RELIEF_FLAT) && (graphPtr->plotBW > 0)) {
	Blt_Print3DRectangle(graphPtr, graphPtr->border, regionPtr->x,
	    regionPtr->y, regionPtr->width, regionPtr->height,
	    graphPtr->plotBW, graphPtr->plotRelief);
    }
    /* Print legend if using default location */
    if (graphPtr->legendPtr->useDefault) {
	(*graphPtr->legendPtr->printProc) (graphPtr);
    }
    textAttr.theta = 0.0;
    textAttr.anchor = TK_ANCHOR_CENTER;
    textAttr.fgColorPtr = graphPtr->marginFg;
    textAttr.bgColorPtr = (XColor *)NULL;
    textAttr.fontPtr = graphPtr->fontPtr;

    if (graphPtr->title != NULL) {
	x = (graphPtr->extreme.x + graphPtr->origin.x) / 2;
	y = graphPtr->borderWidth + TEXTHEIGHT(graphPtr->fontPtr);
	Blt_TextToPostScript(graphPtr, graphPtr->title, &textAttr, x, y);
    }
    for (i = 0; i < 4; i++) {	/* Print axes */
	axisPtr = graphPtr->axisArr[i];
	if (axisPtr->mapped) {
	    (*axisPtr->printProc) (graphPtr, axisPtr, &textAttr);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PrintPostScript --
 *
 *      This procedure is invoked to print the graph in a file.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      A new PostScript file is created.
 *
 *----------------------------------------------------------------------
 */
static int
PrintPostScript(graphPtr, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    int argc;			/* Number of options in argv vector */
    char **argv;		/* Option vector */
{
    PostScript *psPtr = (PostScript *)graphPtr->postscript;
    GraphLegend *legendPtr = graphPtr->legendPtr;
    XRectangle clipRect;
    int result = TCL_ERROR;
    char scratchSpace[BUFSIZ * 2];
    FILE *f = NULL;
    int twiceBW;
    char *fileName;		/* Name of file to write PostScript output
                                 * If NULL, output is returned via
                                 * interp->result. */

    fileName = NULL;
    if ((argc > 2) && (argv[2][0] != '-')) {
	f = fopen(argv[2], "w");
	if (f == NULL) {
	    Tcl_AppendResult(graphPtr->interp, "can't create \"", argv[2],
		"\": ", Tcl_PosixError(graphPtr->interp), (char *)NULL);
	    return TCL_ERROR;
	}
	fileName = argv[2];
	argv++, argc--;
    }
    graphPtr->scratchPtr = scratchSpace;
    if ((argc > 0) &&
	(Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin, configSpecs,
		argc - 2, argv + 2, (char *)psPtr,
		TK_CONFIG_ARGV_ONLY) != TCL_OK)) {
	goto error;
    }
    /* Scale coordinates into printer points (pica) */
    psPtr->x = psPtr->xOffset * psPtr->pointScale;
    psPtr->y = psPtr->yOffset * psPtr->pointScale;
    psPtr->width = graphPtr->width;
    if (psPtr->pageWidth > 0) {
	psPtr->width = psPtr->pageWidth * psPtr->pointScale;
    }
    psPtr->height = graphPtr->height;
    if (psPtr->pageHeight > 0) {
	psPtr->height = psPtr->pageHeight * psPtr->pointScale;
    }
    if (psPtr->landscape) {
	graphPtr->width = psPtr->height;
	graphPtr->height = psPtr->width;
    } else {
	graphPtr->height = psPtr->height;
	graphPtr->width = psPtr->width;
    }
    if (Blt_ComputeLayout(graphPtr) != TCL_OK) {
	graphPtr->interp->result = "not enough space to print graph";
	goto error;
    }
    graphPtr->flags |= LAYOUT_ALL;
    Blt_LayoutGraph(graphPtr);
    Tcl_ResetResult(graphPtr->interp);
    if (PrintPreamble(graphPtr, fileName) != TCL_OK) {
	goto error;
    }
    twiceBW = (graphPtr->plotBW + graphPtr->plotBW);
    /*
     * Determine rectangle of the plotting surface the graph window
     */
    clipRect.x = graphPtr->origin.x - graphPtr->plotBW;
    clipRect.y = graphPtr->extreme.y - graphPtr->plotBW;
    clipRect.width = (graphPtr->extreme.x - graphPtr->origin.x) + twiceBW;
    clipRect.height = (graphPtr->origin.y - graphPtr->extreme.y) + twiceBW;

    Blt_FontToPostScript(graphPtr, graphPtr->fontPtr);
    Blt_BackgroundToPostScript(graphPtr, graphPtr->plotBg);
    Blt_RectangleToPostScript(graphPtr, clipRect.x, clipRect.y, clipRect.width,
	clipRect.height);
    Tcl_AppendResult(graphPtr->interp, "gsave clip\n\n", (char *)NULL);
    /*
     * Draw the elements and tags on the interior of the plotting surface
     */
    PrintElements(graphPtr);
    PrintTags(graphPtr);
    PrintActiveElements(graphPtr);
    if (!legendPtr->useDefault) {
	(*legendPtr->printProc) (graphPtr);
    }
    Tcl_AppendResult(graphPtr->interp, "\n% Unset clipping\ngrestore\n\n",
	(char *)NULL);
    PrintExterior(graphPtr, &clipRect);
    Tcl_AppendResult(graphPtr->interp,
	"showpage\n%Trailer\ngrestore\nend\n%EOF\n", (char *)NULL);
    /*
     * If a file name was given, write the results to that file
     */
    if (f != NULL) {
	fputs(graphPtr->interp->result, f);
	Tcl_ResetResult(graphPtr->interp);
	if (ferror(f)) {
	    Tcl_AppendResult(graphPtr->interp, "error writing file \"",
		fileName, "\": ", Tcl_PosixError(graphPtr->interp),
		(char *)NULL);
	    goto error;
	}
    }
    result = TCL_OK;

  error:
    if (f != NULL) {
	fclose(f);
    }
    /* Reset height and width of graph window */
    graphPtr->width = Tk_Width(graphPtr->tkwin);
    graphPtr->height = Tk_Height(graphPtr->tkwin);
    graphPtr->flags = LAYOUT_ALL;
    /*
     * Redraw the graph in order to re-calculate the layout as soon
     * as possible. This is in the case the crosshairs are active.
     */
    Blt_RedrawGraph(graphPtr);
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreatePostScript --
 *
 *      Creates a postscript structure.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      A new PostScript structure is created.
 *
 *----------------------------------------------------------------------
 */
int
Blt_CreatePostScript(graphPtr)
    Graph *graphPtr;
{
    double pixelsPerInch;
    int screenNum;
    int screenWidth, screenWidthMM;
    PostScript *psPtr;

    psPtr = (PostScript *)calloc(1, sizeof(PostScript));
    if (psPtr == NULL) {
	graphPtr->interp->result = "can't allocate postscript structure";
	return TCL_ERROR;
    }
    psPtr->colorLevel = FULLCOLOR_LEVEL;
    psPtr->anchor = TK_ANCHOR_NW;	/* Upperleft anchor origin */
    psPtr->destroyProc = DestroyPostScript;
    psPtr->configProc = ConfigurePostScript;
    psPtr->printProc = PrintPostScript;
    screenNum = Tk_ScreenNumber(graphPtr->tkwin);
    screenWidth = DisplayWidth(graphPtr->display, screenNum);
    screenWidthMM = DisplayWidthMM(graphPtr->display, screenNum);
#define MM_PER_INCH 25.4
    pixelsPerInch = (MM_PER_INCH * (double)screenWidth / screenWidthMM);
#define PICA_PER_INCH 72.0
    /*
     * Set margins to one inch
     */
    psPtr->pointScale = PICA_PER_INCH / pixelsPerInch;
    psPtr->xOffset = psPtr->yOffset = (int)(pixelsPerInch + 0.5);
    graphPtr->postscript = (GraphPostScript *)psPtr;
    return TCL_OK;
}
