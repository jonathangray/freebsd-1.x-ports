/*
 * bltHtext.c --
 *
 *	This module implements a hypertext widget for the
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
 * tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Hypertext widget created by George Howlett.
 */

/*
 * To do:
 *
 * 1) Fix scroll unit round off errors.
 *
 * 2) Bug in reporting errors in Tcl evaluations.
 *
 * 3) Selections of text. (characters, word, line)
 *
 * 4) Tabstops for easier placement of text and child widgets.
 *    Use variable "tabstops" to set/reset tabstops.
 *
 * 5) Better error checking.
 *
 */

#include "blt.h"
#include <X11/Xutil.h>

#ifndef HTEXT_VERSION
#define HTEXT_VERSION "2.3"
#endif

#define HTEXT_CMDNAME 	"blt_htext"

#define LINES_ALLOC_CHUNK 512

#define BLT_MIN(a,b)	(((a)<(b))?(a):(b))
#define BLT_MAX(a,b)	(((a)>(b))?(a):(b))

/*
 * Flags passed to TkMeasureChars: taken from tkInt.h
 */
#define TK_WHOLE_WORDS          1
#define TK_AT_LEAST_ONE         2
#define TK_PARTIAL_OK           4

/*
 * The following enumerated values are used as bit flags.
 */
typedef enum {
    FILL_NONE, FILL_X, FILL_Y, FILL_BOTH
} FillFlags;

static int ParseFill _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *value, char *widgRec, int offset));
static char *PrintFill _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption FillOption =
{
    ParseFill, PrintFill, (ClientData)0
};

/*
 * Justify option values
 */
typedef enum {
    JUSTIFY_CENTER, JUSTIFY_TOP, JUSTIFY_BOTTOM
} Justify;

static int ParseJustify _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp, Tk_Window tkwin, char *value, char *widgRec, int offset));
static char *PrintJustify _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin, char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption JustifyOption =
{
    ParseJustify, PrintJustify, (ClientData)0
};

static char *fillStrings[] =
{
    "none", "x", "y", "both"
};
static char *justifyStrings[] =
{
    "center", "top", "bottom"
};

typedef struct Child {
    struct Htext *textPtr;	/* Pointer to parent's Htext structure */
    Tk_Window tkwin;		/* Widget window */
    int flags;

    int x, y;			/* Origin of child subwindow in text */

    /* Dimensions of the cavity surrounding the child window */

    unsigned int cavityWidth, cavityHeight;

    /*
     * Dimensions of the child window.  Saved for later comparisons to
     * check for resizing.
     */
    unsigned int windowWidth, windowHeight;

    int precedingTextEnd;	/* Number of characters of text */
    int precedingTextWidth;	/* Width of normal text preceding child */

    Tk_Anchor anchor;
    Justify justify;		/* Justification of region wrt to line */

    int reqWidth, reqHeight;	/* Requested dimension of cavity */
    double relWidth, relHeight;	/* Relative dimensions of cavity.
				 * Sizes are calculated with respect
				 * the size of the viewport */
    int padX, padY;		/* Extra padding to frame around */
    int ipadX, ipadY;		/* Internal padding for window */

    FillFlags fill;		/* Fill style flag */

} Child;

/*
 * Flag bits children subwindows:
 */
#define VISIBLE		4	/* Subwindow is visible in the viewport. */

/*
 * Information used for parsing configuration specs:
 */

/*
 * Defaults for children:
 */

#define DEF_CHILD_ANCHOR        "center"
#define DEF_CHILD_FILL		"none"
#define DEF_CHILD_HEIGHT	"0"
#define DEF_CHILD_JUSTIFY	"center"
#define DEF_CHILD_PAD_X		"0"
#define DEF_CHILD_PAD_Y		"0"
#define DEF_CHILD_REL_HEIGHT	"0.0"
#define DEF_CHILD_REL_WIDTH  	"0.0"
#define DEF_CHILD_WIDTH  	"0"

static Tk_ConfigSpec childConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", (char *)NULL, (char *)NULL,
	DEF_CHILD_ANCHOR, Tk_Offset(Child, anchor), 
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-fill", (char *)NULL, (char *)NULL,
	DEF_CHILD_FILL, Tk_Offset(Child, fill),
	TK_CONFIG_DONT_SET_DEFAULT, &FillOption},
    {TK_CONFIG_PIXELS, "-height", (char *)NULL, (char *)NULL,
	DEF_CHILD_HEIGHT, Tk_Offset(Child, reqHeight), 
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-justify", (char *)NULL, (char *)NULL,
	DEF_CHILD_JUSTIFY, Tk_Offset(Child, justify),
	TK_CONFIG_DONT_SET_DEFAULT, &JustifyOption},
    {TK_CONFIG_PIXELS, "-padx", (char *)NULL, (char *)NULL,
	DEF_CHILD_PAD_X, Tk_Offset(Child, padX), 
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-pady", (char *)NULL, (char *)NULL,
	DEF_CHILD_PAD_Y, Tk_Offset(Child, padY),
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_DOUBLE, "-relheight", (char *)NULL, (char *)NULL,
	DEF_CHILD_REL_HEIGHT, Tk_Offset(Child, relHeight),
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_DOUBLE, "-relwidth", (char *)NULL, (char *)NULL,
	DEF_CHILD_REL_WIDTH, Tk_Offset(Child, relWidth),
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-width", (char *)NULL, (char *)NULL,
	DEF_CHILD_WIDTH, Tk_Offset(Child, reqWidth),
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/*
 * Structure to contain the contents of a single line of text and the
 * children on that line.
 *
 * Individual lines are not configurable, although changes to the size
 * of children do effect its values.
*/

typedef struct {
    int offset;			/* Pixel offset from world coordinate 0,0 */

    short int height;		/* Height of line */
    short int width;		/* Width of line */
    short int baseline;		/* Baseline of text */
    short int numChars;		/* Number of characters in normal text */
    char *text;			/* The plain text on the line */

    Blt_LinkedList windowList;	/* List of subwindows on the line of text */

} Line;


typedef struct {
    Tk_3DBorder border;		/* Border and background for selected
			         * characters. */
    int borderWidth;		/* Width of border around selection. */
    XColor *activeFg;		/* Foreground color for selected text. */
    GC gc;			/* For drawing selected text. */

    int first;			/* Position of first selected character
				 * (-1 means nothing is selected). */
    int last;			/* Position of last selected character.
				 * (-1  means nothing is selected). */
    int anchor;			/* Fixed end of selection
			         * (i.e. "select to" operation will
			         * use this as one end of the selection).*/
} Selection;

typedef struct {
    long int x, y;
    long int width, height;
} ViewPort;

/*
 * Hypertext widget.
 */
typedef struct Htext {
    Tk_Window tkwin;		/* Window that embodies the widget.
                                 * NULL means that the window has been
                                 * destroyed but the data structures
                                 * haven't yet been cleaned up.*/
    Display *display;		/* Display containing widget; needed,
                                 * among other things, to release
                                 * resources after tkwin has already
                                 * gone away. */
    Tcl_Interp *interp;		/* Interpreter associated with widget. */
    unsigned int flags;

    /* User-configurable fields */

    Tk_3DBorder border;		/* Don't current use borders (only color) */
    XColor *normalFg;
    int reqLineNum;		/* Line requested by "gotoline" command */
    int reqWidth, reqHeight;	/* Requested dimensions of the text window */
    int maxWidth, maxHeight;
    Cursor cursor;		/* X Cursor for child */

    char *yScrollCmd;		/* Name of vertical scrollbar to invoke */
    int scrollY;		/* # of pixels per vert scroll */
    char *xScrollCmd;		/* Name of horizontal scroll bar to invoke */
    int scrollX;		/* # of pixels per horiz scroll */
    int lineSpacing;		/* # of pixels between lines */
    int specChar;		/* Special character designating a TCL
			         * command block in a hypertext file. */
    XFontStruct *fontPtr;	/* Font for normal text */
    char *fileName;		/* Name of hypertext file  */
    char *text;			/* Text */


    /*
     * The view port is the width and height of the window and the
     * origin of the viewport (upper left corner) in world coordinates.
     */
    ViewPort vPort;		/* Position and size of viewport in
				 * virtual coordinates */
    int pendingX, pendingY;	/* New upper-left corner (origin) of
				 * the viewport (not yet posted) */

    int first, last;		/* Range of lines displayed */

    Tcl_HashTable subwindows;	/* Table of child sub-windows */
    GC gc;			/* Graphics context for normal text */

#ifdef notdef
    Selection selection;
#endif
    /*
     * Scanning information:
     */
    XPoint scanMark;		/* Anchor position of scan */
    XPoint scanPt;		/* x,y position where the scan started. */

    unsigned int width, height;	/* Size of the window: saved to recognize
				 * when the viewport is resized. */
    char *intBuffer;		/* Internal text buffer */
    Line **linePtrPtr;		/* Array of text lines */
    unsigned int numLines;	/* # of line entered into array. */
    unsigned int arraySize;	/* Size of array allocated. */

} Htext;

/*
 * Bit flags for the hypertext widget:
 */
#define REDRAW_PENDING	1	/* A DoWhenIdle handler has already
				 * been queued to redraw the window */
#define IGNORE_EXPOSURES (1<<1)	/* Ignore exposure events in the text
				 * window.  Potentially many expose
				 * events can occur while rearranging
				 * subwindows during a single call to
				 * the DisplayText.  */
#define VIEW_RESIZED	(1<<2)	/* Size of viewport (window) has
				   changed */
#define VIEW_MOVED 	(1<<3)	/* Position of viewport has moved.
				 * This occurs when scrolling or
				 * goto-ing a new line */
#define REQUEST_LAYOUT 	(1<<4)	/* Something has happened which
				 * requires the layout of text and
				 * child window positions to be
				 * recalculated.  The following
				 * actions may cause this:
				 *
				 * 1) the contents of the hypertext
				 *    has changed by either the -file or
				 *    -text options.
				 *
				 * 2) a text attribute has changed
				 *    (line spacing, font, etc)
				 *
				 * 3) a subwindow has been resized or
				 *    moved.
				 *
				 * 4) a child configuration option has
				 *    changed.
				 */
#define LAYOUT_CHANGED 	(1<<5)	/* The layout was recalculated and the
				 * size of the world (text layout) has
				 * changed. */
#define REQUEST_GOTO 	(1<<6)	/* Indicates the starting text line
				 * number has changed. To be reflected
				 * the next time the widget is
				 * redrawn. */

#define DEF_HTEXT_BG_COLOR	BISQUE1
#define DEF_HTEXT_BG_MONO	WHITE
#define DEF_HTEXT_CURSOR	"pencil"
#define DEF_HTEXT_FG_COLOR	BLACK
#define DEF_HTEXT_FG_MONO	BLACK
#define DEF_HTEXT_FILE_NAME	(char *)NULL
#define DEF_HTEXT_FONT		"*-Helvetica-Bold-R-Normal-*-14-*"
#define DEF_HTEXT_HEIGHT	"0"
#define DEF_HTEXT_LINE_SPACING	"1"
#define DEF_HTEXT_MAX_HEIGHT	"9.5i"
#define DEF_HTEXT_MAX_WIDTH 	"6.5i"
#define DEF_HTEXT_SCROLL_UNITS	"10"
#define DEF_HTEXT_SPEC_CHAR	"0x25"
#define DEF_HTEXT_TEXT		(char *)NULL
#define DEF_HTEXT_WIDTH		"0"

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_HTEXT_BG_COLOR, Tk_Offset(Htext, border),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_HTEXT_BG_MONO, Tk_Offset(Htext, border), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
	DEF_HTEXT_CURSOR, Tk_Offset(Htext, cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_STRING, "-filename", "fileName", "FileName",
	DEF_HTEXT_FILE_NAME, Tk_Offset(Htext, fileName), TK_CONFIG_NULL_OK},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_HTEXT_FONT, Tk_Offset(Htext, fontPtr), 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_HTEXT_FG_COLOR, Tk_Offset(Htext, normalFg), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_HTEXT_FG_MONO, Tk_Offset(Htext, normalFg), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_PIXELS, "-height", "height", "Height",
	DEF_HTEXT_HEIGHT, Tk_Offset(Htext, reqHeight), 
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-linespacing", "lineSpacing", "LineSpacing",
	DEF_HTEXT_LINE_SPACING, Tk_Offset(Htext, lineSpacing), 
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-maxheight", "maxHeight", "MaxHeight",
	DEF_HTEXT_MAX_HEIGHT, Tk_Offset(Htext, maxHeight), 0},
    {TK_CONFIG_PIXELS, "-maxwidth", "maxWidth", "MaxWidth",
	DEF_HTEXT_MAX_WIDTH, Tk_Offset(Htext, maxWidth), 0},
    {TK_CONFIG_INT, "-specialchar", "specialChar", "SpecialChar",
	DEF_HTEXT_SPEC_CHAR, Tk_Offset(Htext, specChar), 0},
    {TK_CONFIG_STRING, "-text", "text", "Text",
	DEF_HTEXT_TEXT, Tk_Offset(Htext, text), TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-width", "width", "Width",
	DEF_HTEXT_WIDTH, Tk_Offset(Htext, reqWidth), 
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_STRING, "-xscrollcommand", "xScrollCommand", "ScrollCommand",
	(char *)NULL, Tk_Offset(Htext, xScrollCmd), TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-xscrollunits", "xScrollUnits", "ScrollUnits",
	DEF_HTEXT_SCROLL_UNITS, Tk_Offset(Htext, scrollX), 
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_STRING, "-yscrollcommand", "yScrollCommand", "ScrollCommand",
	(char *)NULL, Tk_Offset(Htext, yScrollCmd), TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-yscrollunits", "yScrollUnits", "yScrollUnits",
	DEF_HTEXT_SCROLL_UNITS, Tk_Offset(Htext, scrollY), 
        TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/* Forward Declarations */

static int TextWidgetCmd _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, int argc, char **argv));
static void DestroyText _ANSI_ARGS_((ClientData clientdata));
static int ConfigureHtext _ANSI_ARGS_((Tcl_Interp *interp,
	Htext *textPtr, int argc, char **argv, int flags));
static void TextEventProc _ANSI_ARGS_((ClientData clientdata,
	XEvent *eventPtr));
static void EventuallyRedraw _ANSI_ARGS_((Htext *textPtr));
static int IncludeText _ANSI_ARGS_((Tcl_Interp *, Htext *, char *));
static int ParseInput _ANSI_ARGS_((Tcl_Interp *, Htext *, char *));
static char *ReadFile _ANSI_ARGS_((Tcl_Interp *interp, char *fileName));
static int AppendChild _ANSI_ARGS_((Htext *, Tcl_Interp *, int, char **));
static Child *CreateChild _ANSI_ARGS_((Htext *textPtr, char *pathName));
static void DestroyChild _ANSI_ARGS_((Child *childPtr));
static void ChildStructureProc _ANSI_ARGS_((ClientData clientdata,
	XEvent *eventPtr));
static Child *FindChild _ANSI_ARGS_((Htext *textPtr, char *childName));
static char *CollectCommand _ANSI_ARGS_((Htext *textPtr, char *inputPtr,
	char *command));
static Line *NewLine _ANSI_ARGS_((Htext *textPtr));
static void DestroyLine _ANSI_ARGS_((Line *linePtr));
static Line *GetLastLine _ANSI_ARGS_((Htext *textPtr));
static void SetTextInfo _ANSI_ARGS_((Line *linePtr, char *line, int size));
static void GetTextInfo _ANSI_ARGS_((Line *linePtr, char *line, int *size));
static void ComputeLayout _ANSI_ARGS_((Htext *textPtr));
static void LayoutLine _ANSI_ARGS_((Htext *textPtr, Line *linePtr));
static void FreeAllLines _ANSI_ARGS_((Htext *textPtr));
static void AdjustLinesAllocated _ANSI_ARGS_((Htext *textPtr));
static void DisplayText _ANSI_ARGS_((ClientData clientData));
static void DrawPage _ANSI_ARGS_((Htext *textPtr, int deltaY));
static void MoveChild _ANSI_ARGS_((Child *childPtr, int offset));
static void UpdateScrollbar _ANSI_ARGS_((Tcl_Interp *interp, char *cmd,
	int total, int window, int first, int units));
static int GetVisibleLines _ANSI_ARGS_((Htext *textPtr));
static int LineSearch _ANSI_ARGS_((Htext *textPtr, int position,
	int low, int high));
static int ResizeArray _ANSI_ARGS_((char **memPtr, unsigned int elemSize,
	unsigned int oldCount, unsigned int newCount));
static void CreateTraces _ANSI_ARGS_((Htext *textPtr));
static void DeleteTraces _ANSI_ARGS_((Htext *textPtr));
static void ChildGeometryProc _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin));
static void SendBogusEvent _ANSI_ARGS_((Tk_Window tkwin));

extern void TkDisplayChars _ANSI_ARGS_((Display * display, Drawable drawable,
	GC gc, XFontStruct *fontStructPtr, char *string, int numChars,
	int x, int y, int flags));
extern int TkMeasureChars _ANSI_ARGS_((XFontStruct *fontStructPtr,
	char *source, int maxChars, int startX, int maxX, int flags,
	int *nextXPtr));

extern int Blt_OptionChanged _ANSI_ARGS_(VARARGS);

/* end of Forward Declarations */

/*
 *----------------------------------------------------------------------
 *
 * ParseFill --
 *
 *	Converts the fill style string into its numeric representation.
 *	This configuration option affects how the slave window is expanded
 *	if there is extra space in the cubicle in which it sits.
 *
 *	Valid style strings are:
 *
 *	    "none"   Don't expand the window to fill the cubicle.
 * 	    "x"	     Expand only the window's width.
 *	    "y"	     Expand only the window's height.
 *	    "both"   Expand both the window's height and width.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseFill(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* Fill style string */
    char *widgRec;		/* Cubicle structure record */
    int offset;			/* Offset of style in record */
{
    FillFlags *fillPtr = (FillFlags *)(widgRec + offset);
    int length;
    char c;

    c = value[0];
    length = strlen(value);
    if ((c == 'n') && (strncmp(value, "none", length) == 0)) {
	*fillPtr = FILL_NONE;
    } else if ((c == 'x') && (strncmp(value, "x", length) == 0)) {
	*fillPtr = FILL_X;
    } else if ((c == 'y') && (strncmp(value, "y", length) == 0)) {
	*fillPtr = FILL_Y;
    } else if ((c == 'b') && (strncmp(value, "both", length) == 0)) {
	*fillPtr = FILL_BOTH;
    } else {
	Tcl_AppendResult(interp, "bad fill argument \"", value,
	    "\": should be none, x, y, or both", (char *)NULL);
	return TCL_ERROR;
    }
    return (TCL_OK);
}

/*
 *----------------------------------------------------------------------
 *
 * PrintFill --
 *
 *	Returns the fill style string based upon the fill flags.
 *
 * Results:
 *	The fill style string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintFill(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Row/column structure record */
    int offset;			/* Offset of fill in Partition record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    FillFlags fill = *(FillFlags *)(widgRec + offset);

    return (fillStrings[(int)fill]);
}

/*
 *----------------------------------------------------------------------
 *
 * ParseJustify --
 *
 * 	Converts the justification string into its numeric
 * 	representation. This configuration option affects how the
 *	slave window is positioned with respect to the line on which
 *	it sits.
 *
 *	Valid style strings are:
 *
 *	"top"      Uppermost point of region is top of the line's
 *		   text
 * 	"center"   Center point of region is line's baseline.
 *	"bottom"   Lowermost point of region is bottom of the
 *		   line's text
 *
 * Returns:
 *	A standard Tcl result.  If the value was not valid
 *
 *---------------------------------------------------------------------- */
/*ARGSUSED*/
static int
ParseJustify(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* Justification string */
    char *widgRec;		/* Structure record */
    int offset;			/* Offset of justify in record */
{
    Justify *justPtr = (Justify *)(widgRec + offset);
    int length;
    char c;

    c = value[0];
    length = strlen(value);
    if ((c == 'c') && (strncmp(value, "center", length) == 0)) {
	*justPtr = JUSTIFY_CENTER;
    } else if ((c == 't') && (strncmp(value, "top", length) == 0)) {
	*justPtr = JUSTIFY_TOP;
    } else if ((c == 'b') && (strncmp(value, "bottom", length) == 0)) {
	*justPtr = JUSTIFY_BOTTOM;
    } else {
	Tcl_AppendResult(interp, "bad justification argument \"", value,
	    "\": should be center, top, or bottom", (char *)NULL);
	return TCL_ERROR;
    }
    return (TCL_OK);
}

/*
 *----------------------------------------------------------------------
 *
 * PrintJustify --
 *
 *	Returns the justification style string based upon the value.
 *
 * Results:
 *	The justification style string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintJustify(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Structure record */
    int offset;			/* Offset of justify record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    Justify justify = *(Justify *)(widgRec + offset);

    return (justifyStrings[(int)justify]);
}

/*
 *----------------------------------------------------------------------
 *
 * EventuallyRedraw --
 *
 *	Queues a request to redraw the text window at the next idle
 *	point.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Information gets redisplayed.  Right now we don't do selective
 *	redisplays:  the whole window will be redrawn.  This doesn't
 *	seem to hurt performance noticeably, but if it does then this
 *	could be changed.
 *
 *----------------------------------------------------------------------
 */
static void
EventuallyRedraw(textPtr)
    register Htext *textPtr;	/* Information about widget. */
{
    if ((textPtr->tkwin != NULL) && !(textPtr->flags & REDRAW_PENDING)) {
	textPtr->flags |= REDRAW_PENDING;
	Tk_DoWhenIdle(DisplayText, (ClientData)textPtr);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyText --
 *
 * 	This procedure is invoked by Tk_EventuallyFree or Tk_Release
 *	to clean up the internal structure of a Htext at a safe time
 *	(when no-one is using it anymore).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the widget is freed up.
 *
 * ----------------------------------------------------------------------
 */

static void
DestroyText(clientData)
    ClientData clientData;	/* Info about hypertext widget. */
{
    register Htext *textPtr = (Htext *)clientData;

    if (textPtr->gc != NULL) {
	Tk_FreeGC(textPtr->display, textPtr->gc);
    }
    Tk_FreeOptions(configSpecs, (char *)textPtr, textPtr->display, 0);
    FreeAllLines(textPtr);
    Tcl_DeleteHashTable(&(textPtr->subwindows));
    free((char *)textPtr);
}

/*
 * --------------------------------------------------------------
 *
 * TextEventProc --
 *
 * 	This procedure is invoked by the Tk dispatcher for various
 * 	events on hypertext widgets.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets exposed, it is redisplayed.
 *
 * --------------------------------------------------------------
 */

static void
TextEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    Htext *textPtr = (Htext *)clientData;

    if (eventPtr->type == ConfigureNotify) {
	if ((textPtr->width != Tk_Width(textPtr->tkwin)) ||
	    (textPtr->height != Tk_Height(textPtr->tkwin))) {
	    textPtr->flags |= (REQUEST_LAYOUT | VIEW_RESIZED);
	    EventuallyRedraw(textPtr);
	}
    } else if (eventPtr->type == Expose) {

	/*
	 * If the Expose event was synthetic (i.e. we manufactured it
	 * ourselves during a redraw operation), toggle the bit flag
	 * which controls redraws.
	 */

	if (eventPtr->xexpose.send_event) {
	    textPtr->flags ^= IGNORE_EXPOSURES;
	    return;
	}
	if ((eventPtr->xexpose.count == 0) &&
	    !(textPtr->flags & IGNORE_EXPOSURES)) {
	    EventuallyRedraw(textPtr);
	}
    } else if (eventPtr->type == DestroyNotify) {
	Tcl_DeleteCommand(textPtr->interp, Tk_PathName(textPtr->tkwin));
	textPtr->tkwin = NULL;
	if (textPtr->flags & REDRAW_PENDING) {
	    Tk_CancelIdleCall(DisplayText, (ClientData)textPtr);
	}
	Tk_EventuallyFree((ClientData)textPtr, DestroyText);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureHtext --
 *
 * 	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	a hypertext widget.
 *
 * 	The layout of the text must be calculated (by ComputeLayout)
 *	whenever particular options change; -font, -filename, -linespacing
 *	and -text options. If the user has changes one of these options,
 *	it must be detected so that the layout can be recomputed. Since the
 *	coordinates of the layout are virtual, there is no need to adjust
 *	them if physical window attributes (window size, etc.)
 *	change.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 * 	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 * 	etc. get set for textPtr;  old resources get freed, if there were any.
 * 	The hypertext is redisplayed.
 *
 * ----------------------------------------------------------------------
 */


static int
ConfigureHtext(interp, textPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Htext *textPtr;		/* Information about widget; may or may not
			         * already have values for some fields. */
    int argc;			/* Number of valid entries in argv. */
    char **argv;		/* Arguments. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    XGCValues gcValues;
    unsigned long valueMask;
    GC newGC;

    if (Tk_ConfigureWidget(interp, textPtr->tkwin, configSpecs, argc, argv,
	    (char *)textPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    Tk_SetBackgroundFromBorder(textPtr->tkwin, textPtr->border);
    if (Blt_OptionChanged(configSpecs, "-font", "-linespacing", (char *)NULL)) {
	textPtr->flags |= REQUEST_LAYOUT;
    }
    gcValues.font = textPtr->fontPtr->fid;
    gcValues.foreground = textPtr->normalFg->pixel;
    valueMask = GCForeground | GCFont;
    newGC = Tk_GetGC(textPtr->tkwin, valueMask, &gcValues);
    if (textPtr->gc != NULL) {
	Tk_FreeGC(textPtr->display, textPtr->gc);
    }
    textPtr->gc = newGC;

    /*
     * If the either the -text or -file option changed, read in the
     * new text.  The -text option supersedes any -file option.
     */
    if (Blt_OptionChanged(configSpecs, "-filename", "-text", (char *)NULL)) {
	int result;

	FreeAllLines(textPtr);	/* Delete any previous lines */
	CreateTraces(textPtr);	/* Create variable traces */

	result = IncludeText(interp, textPtr, textPtr->fileName);

	DeleteTraces(textPtr);
	if (result == TCL_ERROR) {
	    FreeAllLines(textPtr);
	    return TCL_ERROR;
	}
	AdjustLinesAllocated(textPtr);
	textPtr->flags |= REQUEST_LAYOUT;	/* Mark for layout update */
    }
    EventuallyRedraw(textPtr);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * NewLine --
 *
 * 	This procedure creates and initializes a new line of text.
 *
 * Results:
 *	The return value is a pointer to a structure describing the new
 * 	line of text.  If an error occurred, then the return value is NULL
 *	and an error message is left in interp->result.
 *
 * Side effects:
 *	Memory is allocated.
 *
 * ----------------------------------------------------------------------
 */
static Line *
NewLine(textPtr)
    Htext *textPtr;
{
    Line *linePtr;

    if (textPtr->numLines >= textPtr->arraySize) {
	if (textPtr->arraySize == 0) {
	    textPtr->arraySize = LINES_ALLOC_CHUNK;
	} else {
	    textPtr->arraySize += textPtr->arraySize;
	}
	ResizeArray((char **)&(textPtr->linePtrPtr), sizeof(Line *),
	    textPtr->arraySize, textPtr->numLines);
    }
    /* Create new line entry and add to table */
    linePtr = (Line *)calloc(1, sizeof(Line));
    if (linePtr == NULL) {
	textPtr->interp->result = "can't allocate line structure";
	return NULL;
    }
    Blt_InitLinkedList(&(linePtr->windowList), TCL_ONE_WORD_KEYS);
    textPtr->linePtrPtr[textPtr->numLines++] = linePtr;
    return (linePtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyLine --
 *
 * 	This procedure is invoked by FreeAllLines to clean up the
 * 	internal structure of a line.
 *
 * Results: None.
 *
 * Side effects:
 *	Everything associated with the line (text and children) is
 *	freed up.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyLine(linePtr)
    register Line *linePtr;
{
    Blt_ListEntry *entryPtr;
    Child *childPtr;

    /* Free the list of child structures */

    for (entryPtr = Blt_FirstListEntry(&(linePtr->windowList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	childPtr = (Child *)Blt_GetListValue(entryPtr);
	DestroyChild(childPtr);
    }
    Blt_ClearList(&(linePtr->windowList));
    if (linePtr->text != NULL) {
	free(linePtr->text);
    }
    free((char *)linePtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * AppendChild --
 *
 * 	This procedure creates and initializes a new hyper text child.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side effects:
 *	Memory is allocated.  Child gets configured.
 *
 * ----------------------------------------------------------------------
 */
static int
AppendChild(textPtr, interp, argc, argv)
    Htext *textPtr;		/* Hypertext widget */
    Tcl_Interp *interp;		/* Interpreter associated with widget */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Line *linePtr;
    Child *childPtr;
    Blt_ListEntry *entryPtr;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " append pathName ?options?\"", (char *)NULL);
	return TCL_ERROR;
    }
    childPtr = CreateChild(textPtr, argv[2]);
    if (childPtr == NULL) {
	return TCL_ERROR;
    }
    if (Tk_ConfigureWidget(interp, textPtr->tkwin, childConfigSpecs,
	    argc - 3, argv + 3, (char *)childPtr, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Append child to list of subwindows of the last line.
     */
    linePtr = GetLastLine(textPtr);
    entryPtr = Blt_CreateListEntry((char *)childPtr->tkwin);
    Blt_LinkListAfter(&(linePtr->windowList), entryPtr,
	(Blt_ListEntry *)NULL);
    Blt_SetListValue(entryPtr, childPtr);
    linePtr->width += childPtr->cavityWidth;
    childPtr->precedingTextEnd = linePtr->numChars;

    textPtr->flags |= REQUEST_LAYOUT;
    EventuallyRedraw(textPtr);
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------
 *
 * TranslateAnchor --
 *
 * 	Translate the coordinates of a given bounding box based
 *	upon the anchor specified.  The anchor indicates where
 *	the given xy position is in relation to the bounding box.
 *
 *  		nw --- n --- ne
 *  		|            |     x,y ---+
 *  		w   center   e      |     |
 *  		|            |      +-----+
 *  		sw --- s --- se
 *
 * Results:
 *	The translated coordinates of the bounding box are returned.
 *
 * -----------------------------------------------------------------
 */
static XPoint
TranslateAnchor(deltaX, deltaY, anchor)
    int deltaX, deltaY;		/* Difference between outer and inner regions
				 */
    Tk_Anchor anchor;		/* Direction of the anchor */
{
    XPoint anchorPos;

    anchorPos.x = anchorPos.y = 0;
    switch (anchor) {
    case TK_ANCHOR_NW:		/* Upper left corner */
	break;
    case TK_ANCHOR_W:		/* Left center */
	anchorPos.y = (deltaY / 2);
	break;
    case TK_ANCHOR_SW:		/* Lower left corner */
	anchorPos.y = deltaY;
	break;
    case TK_ANCHOR_N:		/* Top center */
	anchorPos.x = (deltaX / 2);
	break;
    case TK_ANCHOR_CENTER:	/* Centered */
	anchorPos.x = (deltaX / 2);
	anchorPos.y = (deltaY / 2);
	break;
    case TK_ANCHOR_S:		/* Bottom center */
	anchorPos.x = (deltaX / 2);
	anchorPos.y = deltaY;
	break;
    case TK_ANCHOR_NE:		/* Upper right corner */
	anchorPos.x = deltaX;
	break;
    case TK_ANCHOR_E:		/* Right center */
	anchorPos.x = deltaX;
	anchorPos.y = (deltaY / 2);
	break;
    case TK_ANCHOR_SE:		/* Lower right corner */
	anchorPos.x = deltaX;
	anchorPos.y = deltaY;
	break;
    }
    return (anchorPos);
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateChild --
 *
 * 	This procedure creates and initializes a new child subwindow
 *	in the hyper text widget.
 *
 * Results:
 *	The return value is a pointer to a structure describing the
 *	new child.  If an error occurred, then the return value is
 *      NULL and an error message is left in interp->result.
 *
 * Side effects:
 *	Memory is allocated. Child window is mapped. Callbacks are set
 *	up for subwindow resizes and geometry requests.
 *
 * ----------------------------------------------------------------------
 */
static Child *
CreateChild(textPtr, name)
    Htext *textPtr;		/* Hypertext widget */
    char *name;			/* Name of child window */
{
    register Child *childPtr;
    Tk_Window tkwin;
    char buf[BUFSIZ];
    Tcl_HashEntry *entryPtr;
    int dummy;

    if (name[0] != '.') {	/* Relative path, make absolute */
	sprintf(buf, "%s.%s", Tk_PathName(textPtr->tkwin), name);
	name = buf;
    }
    /* Get the Tk window and parent Tk window associated with the child */
    tkwin = Tk_NameToWindow(textPtr->interp, name, textPtr->tkwin);
    if (tkwin == NULL) {
	return NULL;
    }
    if (FindChild(textPtr, name) != NULL) {
	Tcl_AppendResult(textPtr->interp, "\"", name,
	    "\" is already appended to ", Tk_PathName(textPtr->tkwin),
	    (char *)NULL);
	return NULL;
    }
    if (textPtr->tkwin != Tk_Parent(tkwin)) {
	Tcl_AppendResult(textPtr->interp, "\"", name,
	    "\" is not a child of ", Tk_PathName(textPtr->tkwin),
	    (char *)NULL);
	return NULL;
    }
    childPtr = (Child *)calloc(1, sizeof(Child));
    if (childPtr == NULL) {
	textPtr->interp->result = "can't create child structure";
	return NULL;
    }
    childPtr->tkwin = tkwin;
    childPtr->textPtr = textPtr;
    childPtr->x = childPtr->y = 0;
    childPtr->fill = FILL_NONE;
    childPtr->justify = JUSTIFY_CENTER;
    childPtr->anchor = TK_ANCHOR_CENTER;
    entryPtr = Tcl_CreateHashEntry(&(textPtr->subwindows), (char *)tkwin,
	&dummy);
    Tcl_SetHashValue(entryPtr, (ClientData)childPtr);

    Tk_ManageGeometry(tkwin, ChildGeometryProc, (ClientData)childPtr);
    Tk_CreateEventHandler(tkwin, StructureNotifyMask, ChildStructureProc,
	(ClientData)childPtr);
    return (childPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyChild --
 *
 * 	This procedure is invoked by DestroyLine to clean up the
 * 	internal structure of a child.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the widget is freed up.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyChild(childPtr)
    register Child *childPtr;
{
    /* Destroy the child window if it still exists */
    if (childPtr->tkwin != NULL) {
	Tcl_HashEntry *entryPtr;

	Tk_DeleteEventHandler(childPtr->tkwin, StructureNotifyMask,
	    ChildStructureProc, (ClientData)childPtr);
	entryPtr = Tcl_FindHashEntry(&(childPtr->textPtr->subwindows),
	    (char *)childPtr->tkwin);
	Tcl_DeleteHashEntry(entryPtr);
	Tk_DestroyWindow(childPtr->tkwin);
    }
    free((char *)childPtr);
}

/*
 * --------------------------------------------------------------
 *
 * TextEventProc --
 *
 * 	This procedure is invoked by the Tk dispatcher for various
 * 	events on hypertext widgets.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets exposed, it is redisplayed.
 *
 * --------------------------------------------------------------
 */
static void
ChildStructureProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    register Child *childPtr = (Child *)clientData;
    Htext *textPtr;

    if ((childPtr == NULL) || (childPtr->tkwin == NULL)) {
	return;
    }
    textPtr = childPtr->textPtr;

    if (eventPtr->type == DestroyNotify) {
	/*
	 * Mark the child as deleted by dereferencing the Tk window
	 * pointer. Zero out the height and width to collapse the area
	 * used by the child.  Redraw the window only if the child is
	 * currently visible and mapped.
	 */
	childPtr->textPtr->flags |= REQUEST_LAYOUT;
	if (Tk_IsMapped(childPtr->tkwin) && (childPtr->flags & VISIBLE)) {
	    EventuallyRedraw(textPtr);
	}
	Tk_DeleteEventHandler(childPtr->tkwin, StructureNotifyMask,
	    ChildStructureProc, (ClientData)childPtr);
	Tcl_DeleteHashEntry(Tcl_FindHashEntry(&(childPtr->textPtr->subwindows),
		(char *)childPtr->tkwin));
	childPtr->tkwin = NULL;
    } else if (eventPtr->type == ConfigureNotify) {

	/*
	 * Children can't request new positions by themselves, so worry
	 * only about resizing.
	 */

	if (childPtr->windowWidth != Tk_Width(childPtr->tkwin) ||
	    childPtr->windowHeight != Tk_Height(childPtr->tkwin)) {
	    EventuallyRedraw(textPtr);
	    textPtr->flags |= REQUEST_LAYOUT;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ComputeLayout --
 *
 *	This procedure computes the total width and height needed
 *      to contain the text and children from all the lines of text.
 *      It merely sums the heights and finds the maximum width of
 *	all the lines.  The width and height are needed for scrolling.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
ComputeLayout(textPtr)
    Htext *textPtr;
{
    register int count;
    register Line **linePtrPtr;
    register int height, width;

    width = height = 0;
    linePtrPtr = textPtr->linePtrPtr;
    for (count = 0; count < textPtr->numLines; count++) {
	(*linePtrPtr)->offset = height;
	LayoutLine(textPtr, *linePtrPtr);
	height += (*linePtrPtr)->height;
	if ((*linePtrPtr)->width > width) {
	    width = (*linePtrPtr)->width;
	}
	linePtrPtr++;
    }
    /*
     * Set changed flag if new layout changed size of virtual text.
     */
    if ((height != textPtr->vPort.height) ||
	(width != textPtr->vPort.width)) {
	textPtr->flags |= LAYOUT_CHANGED;
	textPtr->vPort.height = height;
	textPtr->vPort.width = width;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GetReqWidth --
 *
 *	Returns the width requested by the child window. The requested
 *	space also includes any internal padding which has been designated
 *	for this window.
 *
 * Results:
 *	Returns the requested width of the child window.
 *
 *----------------------------------------------------------------------
 */

static int
GetReqWidth(childPtr)
    Child *childPtr;
{
    int width;

    if (childPtr->reqWidth > 0) {
	width = childPtr->reqWidth;
    } else if (childPtr->relWidth > 0.0) {
	width = (int)((double)Tk_Width(childPtr->textPtr->tkwin) *
	    childPtr->relWidth + 0.5);
    } else {
	width = Tk_ReqWidth(childPtr->tkwin) + 2 * (childPtr->ipadX);
    }
    return (width);
}

/*
 *----------------------------------------------------------------------
 *
 * GetReqHeight --
 *
 *	Returns the height requested by the child window. The requested
 *	space also includes any internal padding which has been designated
 *	for this window.
 *
 * Results:
 *	Returns the requested height of the child window.
 *
 *----------------------------------------------------------------------
 */
static int
GetReqHeight(childPtr)
    Child *childPtr;
{
    int height;

    if (childPtr->reqHeight > 0) {
	height = childPtr->reqHeight;
    } else if (childPtr->relHeight > 0.0) {
	height = (int)((double)Tk_Height(childPtr->textPtr->tkwin) *
	    childPtr->relHeight + 0.5);
    } else {
	height = Tk_ReqHeight(childPtr->tkwin) + 2 * (childPtr->ipadY);
    }
    return (height);
}

/*
 *----------------------------------------------------------------------
 *
 * GetCavityWidth --
 *
 *	Returns the width of the cavity based upon the requested size
 *	of the child window.  The requested space also includes any
 *	external padding which has been designated for this window.
 *
 * Results:
 *	Returns the requested width of the cavity.
 *
 *----------------------------------------------------------------------
 */

static int
GetCavityWidth(childPtr)
    Child *childPtr;
{
    int width;

    width = GetReqWidth(childPtr) +
	(2 * (Tk_Changes(childPtr->tkwin)->border_width + childPtr->padX));
    return (width);
}

/*
 *----------------------------------------------------------------------
 *
 * GetCavityHeight --
 *
 *	Returns the height of the cavity based upon the requested size
 *	of the child window.  The requested space also includes any
 *	external padding which has been designated for this window.
 *
 * Results:
 *	Returns the requested height of the cavity.
 *
 *----------------------------------------------------------------------
 */

static int
GetCavityHeight(childPtr)
    Child *childPtr;
{
    int height;

    height = GetReqHeight(childPtr) +
	(2 * (Tk_Changes(childPtr->tkwin)->border_width + childPtr->padY));
    return (height);
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutLine --
 *
 *	This procedure computes the total width and height needed
 *      to contain the text and children for a particular line.
 *      It also calculates the baseline of the text on the line with
 *	respect to the other children on the line.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
LayoutLine(textPtr, linePtr)
    Htext *textPtr;
    Line *linePtr;
{
    register Child *childPtr;
    int numChars;
    int maxAscent, maxDescent, maxHeight;
    int ascent, descent;
    register int curPos = 0;
    int median;			/* Difference of font ascent/descent values */
    Blt_ListEntry *entryPtr;
    register int x, y;
    int newX;

    /*
     * Pass 1: Determine the maximum ascent (baseline) and descent
     * needed for the line.  We'll need this for figuring the top,
     * bottom, and center anchors.
     */
    /* Initialize line defaults */
    maxAscent = textPtr->fontPtr->ascent;
    maxDescent = textPtr->fontPtr->descent;
    median = textPtr->fontPtr->ascent - textPtr->fontPtr->descent;
    ascent = descent = 0;	/* Suppress compiler warnings */
    for (entryPtr = Blt_FirstListEntry(&(linePtr->windowList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	childPtr = (Child *)Blt_GetListValue(entryPtr);
	if (childPtr->tkwin == NULL) {
	    continue;
	}
	childPtr->cavityHeight = GetCavityHeight(childPtr);
	childPtr->cavityWidth = GetCavityWidth(childPtr);
	switch (childPtr->justify) {
	case JUSTIFY_TOP:
	    ascent = textPtr->fontPtr->ascent + childPtr->padY;
	    descent = childPtr->cavityHeight - textPtr->fontPtr->ascent;
	    break;
	case JUSTIFY_CENTER:
	    ascent = (childPtr->cavityHeight + median) / 2;
	    descent = (childPtr->cavityHeight - median) / 2;
	    break;
	case JUSTIFY_BOTTOM:
	    ascent = childPtr->cavityHeight - textPtr->fontPtr->descent;
	    descent = textPtr->fontPtr->descent;
	    break;
	}
	if (descent > maxDescent) {
	    maxDescent = descent;
	}
	if (ascent > maxAscent) {
	    maxAscent = ascent;
	}
    }

    maxHeight = maxAscent + maxDescent + textPtr->lineSpacing;
    x = 0;			/* Always starts from x=0 */
    y = 0;			/* Suppress compiler warning */
    curPos = 0;
    /*
     * Pass 2: Find the placements of the text and children along each
     * line.
     */
    for (entryPtr = Blt_FirstListEntry(&(linePtr->windowList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	childPtr = (Child *)Blt_GetListValue(entryPtr);
	if (childPtr->tkwin == NULL) {
	    continue;
	}
	/* Get the width of the text leading to the child */
	numChars = (childPtr->precedingTextEnd - curPos);
	if ((numChars > 0) && (linePtr->text != NULL)) {
	    TkMeasureChars(textPtr->fontPtr, linePtr->text + curPos,
		numChars, x, 10000, TK_PARTIAL_OK | TK_AT_LEAST_ONE, &newX);
	    childPtr->precedingTextWidth = newX - x;
	    x = newX;
	}
	switch (childPtr->justify) {
	case JUSTIFY_TOP:
	    y = maxAscent + textPtr->fontPtr->descent -
		childPtr->cavityHeight;
	    break;
	case JUSTIFY_CENTER:
	    y = maxAscent - (childPtr->cavityHeight + median) / 2;
	    break;
	case JUSTIFY_BOTTOM:
	    y = maxAscent + textPtr->fontPtr->descent -
		childPtr->cavityHeight;
	    break;
	}
	childPtr->x = x;
	childPtr->y = y;
	curPos = childPtr->precedingTextEnd;
	x += childPtr->cavityWidth;
    }

    /*
     * This may be piece of line after last child and will also pick
     * up the entire line if no children occurred on it
     */
    numChars = (linePtr->numChars - curPos);
    if ((numChars > 0) && (linePtr->text != NULL)) {
	TkMeasureChars(textPtr->fontPtr, linePtr->text + curPos, numChars,
	    x, 10000, TK_PARTIAL_OK | TK_AT_LEAST_ONE, &newX);
	x = newX;
    }
    /* Update line parameters */
    linePtr->width = (unsigned int)x;
    linePtr->height = maxHeight;
    linePtr->baseline = maxAscent;
}

/*
 * ----------------------------------------------------------------------
 *
 * MoveChild --
 *
 * 	Move a child subwindow to a new location in the hypertext
 *	parent window.  If the window has no geometry (i.e. width,
 *	or height is 0), simply unmap to window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Each subwindow is moved to its new location, generating
 *      Expose events in the parent for each child window moved.
 *
 * ----------------------------------------------------------------------
 */
static void
MoveChild(childPtr, offset)
    register Child *childPtr;
    int offset;
{
    int winWidth, winHeight;
    int width, height;
    int deltaX, deltaY;
    int x, y;
    int padX, padY;

    if (childPtr->tkwin == NULL) {
	return;
    }
    winWidth = GetReqWidth(childPtr);
    winHeight = GetReqHeight(childPtr);
    if ((winWidth < 1) || (winHeight < 1)) {
	if (Tk_IsMapped(childPtr->tkwin)) {
	    Tk_UnmapWindow(childPtr->tkwin);
	}
	return;
    }
    padX = (Tk_Changes(childPtr->tkwin)->border_width + childPtr->padX);
    padY = (Tk_Changes(childPtr->tkwin)->border_width + childPtr->padY);
    x = padX + childPtr->x - childPtr->textPtr->vPort.x;
    y = padY + offset + childPtr->y - childPtr->textPtr->vPort.y;
    width = childPtr->cavityWidth - (2 * padX);
    height = childPtr->cavityHeight - (2 * padY);

    if ((width < winWidth) || (childPtr->fill & FILL_X)) {
	winWidth = width;
    }
    if ((height < winHeight) || (childPtr->fill & FILL_Y)) {
	winHeight = height;
    }
    deltaX = deltaY = 0;
    if (width > winWidth) {
	deltaX = width - winWidth;
    }
    if (height > winHeight) {
	deltaY = height - winHeight;
    }
    if ((deltaX > 0) || (deltaY > 0)) {
	XPoint anchorPos;

	anchorPos = TranslateAnchor(deltaX, deltaY, childPtr->anchor);
	x += anchorPos.x, y += anchorPos.y;
    }
    childPtr->windowWidth = winWidth;
    childPtr->windowHeight = winHeight;
    if ((x != Tk_X(childPtr->tkwin)) || (y != Tk_Y(childPtr->tkwin)) ||
	(winWidth != Tk_Width(childPtr->tkwin)) ||
	(winHeight != Tk_Height(childPtr->tkwin))) {
	Tk_MoveResizeWindow(childPtr->tkwin, x, y, (unsigned int)winWidth,
	    (unsigned int)winHeight);
	if (!Tk_IsMapped(childPtr->tkwin)) {
	    Tk_MapWindow(childPtr->tkwin);
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DrawPage --
 *
 * 	This procedure displays the lines of text and moves the child
 *      windows to their new positions.  It draws lines with regard to
 *	the direction of the scrolling.  The idea here is to make the
 *	text and buttons appear to move together. Otherwise you will
 *	get a "jiggling" effect where the windows appear to bump into
 *	the next line before that line is moved.  In the worst case, where
 *	every line has at least one widget, you can get an aquarium effect
 *      (lines appear to ripple up).
 *
 * 	The text area may start between line boundaries (to accommodate
 *	both variable height lines and constant scrolling). Subtract the
 *	difference of the page offset and the line offset from the starting
 *	coordinates. For horizontal scrolling, simply subtract the offset
 *	of the viewport. The window will clip the top of the first line,
 *	the bottom of the last line, whatever text extends to the left
 *	or right of the viewport on any line.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Commands are output to X to display the line in its current
 * 	mode.
 *
 * ----------------------------------------------------------------------
 */
static void
DrawPage(textPtr, deltaY)
    Htext *textPtr;
    int deltaY;			/* Change from previous Y coordinate */
{
    Line *linePtr;
    Child *childPtr;
    Tk_Window tkwin = textPtr->tkwin;
    int numChars;
    int curPos;
    int baseline;
    Pixmap pixMap;
    int forceCopy = 0;
    register int i;
    int curLine, lastY;
    register int x, y;
    Blt_ListEntry *entryPtr;

    /* Setup: Clear the display */
    /* Create an off-screen pixmap for semi-smooth scrolling. */
    pixMap = XCreatePixmap(textPtr->display, Tk_WindowId(tkwin),
	Tk_Width(tkwin), Tk_Height(tkwin),
	DefaultDepthOfScreen(Tk_Screen(tkwin)));
    Tk_Fill3DRectangle(textPtr->display, pixMap, textPtr->border, 0, 0,
	Tk_Width(tkwin), Tk_Height(tkwin), 0, TK_RELIEF_FLAT);

    x = -(textPtr->vPort.x);
    y = -(textPtr->vPort.y);

    if (deltaY >= 0) {
	y += textPtr->linePtrPtr[textPtr->first]->offset;
	curLine = textPtr->first;
	lastY = 0;
    } else {
	y += textPtr->linePtrPtr[textPtr->last]->offset;
	curLine = textPtr->last;
	lastY = Tk_Height(tkwin);
    }
    forceCopy = 0;

    /* Draw each line */
    for (i = textPtr->first; i <= textPtr->last; i++) {

	/* Initialize character position in text buffer to start */
	curPos = 0;
	/* Initialize X position */
	x = -(textPtr->vPort.x);

	linePtr = textPtr->linePtrPtr[curLine];
	baseline = y + linePtr->baseline;	/* Base line in window
						 * coordinates */

	for (entryPtr = Blt_FirstListEntry(&(linePtr->windowList));
	    entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	    childPtr = (Child *)Blt_GetListValue(entryPtr);
	    MoveChild(childPtr, linePtr->offset);
	    childPtr->flags |= VISIBLE;
	    numChars = (childPtr->precedingTextEnd - curPos);
	    if ((numChars > 0) && (linePtr->text != NULL)) {
		TkDisplayChars(textPtr->display, pixMap, textPtr->gc,
		    textPtr->fontPtr, linePtr->text + curPos,
		    numChars, x, baseline, 0);
		x += childPtr->precedingTextWidth;
	    }
	    curPos = childPtr->precedingTextEnd;
	    x += childPtr->cavityWidth;
	    forceCopy++;
	}

	/*
	 * This may be the text trailing the last child or the entire
	 * line if no children occur on it.
	 */
	numChars = (linePtr->numChars - curPos);
	if ((numChars > 0) && (linePtr->text != NULL)) {
#ifdef notdef
	    if (textPtr->selectFirst >= curLine &&
		textPtr->selectLast <= curLine) {
		Tk_Fill3DRectangle(textPtr->display, pixMap,
		    textPtr->selectBorder, y, 0,
		    numChars, Tk_Height(tkwin),
		    0, textPtr->selectBorderWidth);
	    }
#endif
	    TkDisplayChars(textPtr->display, pixMap, textPtr->gc,
		textPtr->fontPtr, linePtr->text + curPos,
		numChars, x, baseline, 0);
	}
	/* Go to the top of the next line */
	if (deltaY >= 0) {
	    y += textPtr->linePtrPtr[curLine++]->height;
	}
	if (forceCopy > 0 && !(textPtr->flags & VIEW_RESIZED)) {
	    if (deltaY >= 0) {
		XCopyArea(textPtr->display, pixMap, Tk_WindowId(tkwin),
		    textPtr->gc, 0, lastY, Tk_Width(tkwin), y - lastY,
		    0, lastY);
	    } else {
		XCopyArea(textPtr->display, pixMap, Tk_WindowId(tkwin),
		    textPtr->gc, 0, y, Tk_Width(tkwin), lastY - y,
		    0, y);
	    }
	    forceCopy = 0;	/* Reset drawing flag */
	    lastY = y;		/* Record last Y position */
	}
	if ((deltaY < 0) && (curLine > 0)) {
	    y -= textPtr->linePtrPtr[--curLine]->height;
	}
    }
    /*
     * If the viewport was resized, draw the page in one operation.
     * Otherwise draw any left-over block of text (either at the top
     * or bottom of the page)
     */
    if (textPtr->flags & VIEW_RESIZED) {
	XCopyArea(textPtr->display, pixMap, Tk_WindowId(tkwin),
	    textPtr->gc, 0, 0, Tk_Width(tkwin), Tk_Height(tkwin), 0, 0);
    } else if (lastY != y) {
	if (deltaY >= 0) {
	    XCopyArea(textPtr->display, pixMap, Tk_WindowId(tkwin),
		textPtr->gc, 0, lastY, Tk_Width(tkwin),
		Tk_Height(tkwin) - lastY, 0, lastY);
	} else {
	    XCopyArea(textPtr->display, pixMap, Tk_WindowId(tkwin),
		textPtr->gc, 0, 0, Tk_Width(tkwin), lastY, 0, 0);
	}
    }
    XFreePixmap(textPtr->display, pixMap);
}

/*
 * ----------------------------------------------------------------------
 *
 * DisplayText --
 *
 * 	This procedure is invoked to display a hypertext widget.
 *	Many of the operations which might ordinarily be performed
 *	elsewhere (e.g. in a configuration routine) are done here
 *	because of the somewhat unusual interactions occurring between
 *	the parent and child windows.
 *
 *      Recompute the layout of the text if necessary. This is
 *	necessary if the world coordinate system has changed.
 *	Specifically, the following may have occurred:
 *
 *	  -  a text attribute has changed (font, linespacing, etc.).
 *	  -  child option changed (anchor, width, height).
 *        -  actual child window was resized.
 *	  -  new text string or file.
 *
 *      This is deferred to the display routine since potentially
 *      many of these may occur (especially child window changes).
 *
 *	Set the vertical and horizontal scrollbars (if they are
 *	designated) by issuing a Tcl command.  Done here since
 *	the text window width and height are needed.
 *
 *	If the viewport position or contents have changed in the
 *	vertical direction,  the now out-of-view child windows
 *	must be moved off the viewport.  Since child windows will
 *	obscure the text window, it is imperative that the children
 *	are moved off before we try to redraw text in the same area.
 *      This is necessary only for vertical movements.  Horizontal
 *	child window movements are handled automatically in the
 *	page drawing routine.
 *
 *      Get the new first and last line numbers for the viewport.
 *      These line numbers may have changed because either a)
 *      the viewport changed size or position, or b) the text
 *	(child window sizes or text attributes) have changed.
 *
 *	If the viewport has changed vertically (i.e. the first or
 *      last line numbers have changed), move the now out-of-view
 *	child windows off the viewport.
 *
 *      Potentially many expose events may be generated when the
 *	the individual child windows are moved and/or resized.
 *	These events need to be ignored.  Since (I think) expose
 * 	events are guaranteed to happen in order, we can bracket
 *	them by sending phony events (via XSendEvent). The phony
 *      event turn on and off flags which indicate if the events
 *	should be ignored.
 *
 *	Finally, the page drawing routine is called.
 *
 * Results:
 *	None.
 *
 * Side effects:
 * 	Commands are output to X to display the hypertext in its
 *	current mode.
 *
 * ----------------------------------------------------------------------
 */
static void
DisplayText(clientData)
    ClientData clientData;	/* Information about widget. */
{
    Htext *textPtr = (Htext *)clientData;
    register Tk_Window tkwin;
    int oldFirst;		/* First line of old viewport */
    int oldLast;		/* Last line of old viewport */
    int deltaY;			/* Change in viewport in Y direction */

    textPtr->flags &= ~REDRAW_PENDING;

    tkwin = textPtr->tkwin;
    if ((tkwin == NULL) || (textPtr->numLines == 0)) {
	return;
    }
    if (textPtr->flags & REQUEST_LAYOUT) {
	/*
	 * Recompute the layout when children are created, deleted,
	 * moved, or resized.  Also when text attributes (such as
	 * font, linespacing) have changed.
	 */
	ComputeLayout(textPtr);
    }
    textPtr->width = (unsigned int)textPtr->reqWidth;
    textPtr->height = (unsigned int)textPtr->reqHeight;
    if (textPtr->width == 0) {
	textPtr->width = BLT_MIN(textPtr->vPort.width, textPtr->maxWidth);
    }
    if (textPtr->height == 0) {
	textPtr->height = BLT_MIN(textPtr->vPort.height, textPtr->maxHeight);
    }
    if ((textPtr->width != Tk_ReqWidth(tkwin)) ||
	(textPtr->height != Tk_ReqHeight(tkwin))) {
	Tk_GeometryRequest(tkwin, textPtr->width, textPtr->height);
	textPtr->flags |= REQUEST_LAYOUT;
	EventuallyRedraw(textPtr);
	return;
    }
    if (!Tk_IsMapped(tkwin)) {
	return;
    }
    /*
     * Turn off layout requests here, after the text window has been
     * mapped Otherwise, relative slave window height and width
     * requests wrt to the text window will be wrong.
     */
    textPtr->flags &= ~REQUEST_LAYOUT;

    /* Is there a pending gotoline request? */
    if (textPtr->flags & REQUEST_GOTO) {
	textPtr->pendingY = textPtr->linePtrPtr[textPtr->reqLineNum]->offset;
	textPtr->flags &= ~REQUEST_GOTO;
    }
    deltaY = textPtr->pendingY - textPtr->vPort.y;
    oldFirst = textPtr->first, oldLast = textPtr->last;

    /*
     * If the viewport has changed size or position, or the text
     * and/or child subwindows have changed, adjust the scrollbars to
     * new positions.
     */
    if (textPtr->flags & (VIEW_MOVED | VIEW_RESIZED | LAYOUT_CHANGED)) {
	/* Reset viewport origin and world extents */
	textPtr->vPort.x = textPtr->pendingX;
	textPtr->vPort.y = textPtr->pendingY;
	if (textPtr->xScrollCmd != NULL)
	    UpdateScrollbar(textPtr->interp, textPtr->xScrollCmd,
		textPtr->vPort.width, Tk_Width(tkwin), textPtr->vPort.x,
		textPtr->scrollX);
	if (textPtr->yScrollCmd != NULL)
	    UpdateScrollbar(textPtr->interp, textPtr->yScrollCmd,
		textPtr->vPort.height, Tk_Height(tkwin),
		textPtr->vPort.y, textPtr->scrollY);
	/*
	 * Given a new viewport or text height, find the first and
	 * last line numbers of the new viewport.
	 */
	GetVisibleLines(textPtr);
    }
    /*
     * (This is a kludge.) Send an expose event before and after
     * drawing the page of text.  Since moving and resizing of the
     * subwindows will cause redundant expose events in the parent
     * window, the phony events will bracket them indicating no action
     * should be taken.
     */
    SendBogusEvent(tkwin);

    /*
     * If either the position of the viewport has changed or the size
     * of width or height of the entire text have changed, move the
     * children from the previous viewport out of the current
     * viewport. Worry only about the vertical child window movements.
     * The horizontal moves are handled by the when drawing the page
     * of text.
     */
    if (textPtr->first != oldFirst || textPtr->last != oldLast) {
	register Line **linePtrPtr;
	register int i;
	int first, last;
	Blt_ListEntry *entryPtr;
	Child *childPtr;

	/* Figure out which lines are now out of the viewport */

	if ((textPtr->first > oldFirst) && (textPtr->first <= oldLast)) {
	    first = oldFirst, last = textPtr->first;
	} else if ((textPtr->last < oldLast) && (textPtr->last >= oldFirst)) {
	    first = textPtr->last, last = oldLast;
	} else {
	    first = oldFirst, last = oldLast;
	}

	linePtrPtr = &(textPtr->linePtrPtr[first]);
	for (i = first; i <= last; i++) {
	    for (entryPtr = Blt_FirstListEntry(&((*linePtrPtr)->windowList));
		entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
		childPtr = (Child *)Blt_GetListValue(entryPtr);
		MoveChild(childPtr, (*linePtrPtr)->offset);
		childPtr->flags &= ~VISIBLE;
	    }
	    linePtrPtr++;
	}
    }
    DrawPage(textPtr, deltaY);
    SendBogusEvent(tkwin);

    /* Reset flags */
    textPtr->flags &= ~(VIEW_RESIZED | VIEW_MOVED | LAYOUT_CHANGED);
}

/*
 * ----------------------------------------------------------------------
 *
 * GetVisibleLines --
 *
 * 	Calculates which lines are visible using the height
 *      of the viewport and y offset from the top of the text.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Only those line between first and last inclusive are
 * 	redrawn.
 *
 * ----------------------------------------------------------------------
 */
static int
GetVisibleLines(textPtr)
    Htext *textPtr;
{
    int topLine, bottomLine;
    int topPos, bottomPos;
    int lastLine;

    topPos = textPtr->pendingY;
    lastLine = textPtr->numLines - 1;
    /* First line */
    topLine = LineSearch(textPtr, topPos, 0, lastLine);
    if (topLine < 0) {
	/*
	 * This can't be. The newY offset must be corrupted.
	 */
	fprintf(stderr, "First position not found `%d'", topPos);
	return TCL_ERROR;
    }
    textPtr->first = topLine;

    /*
     * If there is less text than window space, the bottom line is the
     * last line of text.  Otherwise search for the line at the bottom
     * of the window.
     */
    bottomPos = topPos + Tk_Height(textPtr->tkwin) - 1;
    if (bottomPos < textPtr->vPort.height) {
	bottomLine = LineSearch(textPtr, bottomPos, topLine, lastLine);
    } else {
	bottomLine = lastLine;
    }
    if (bottomLine < 0) {
	/*
	 * This can't be. The newY offset must be corrupted.
	 */
	fprintf(stderr, "Last position not found `%d'\n", bottomPos);
#ifdef notdef
	fprintf(stderr, "vPort.height=%d,height=%d,top=%d,first=%d,last=%d\n",
	    textPtr->vPort.height, Tk_Height(textPtr->tkwin), topPos,
	    textPtr->linePtrPtr[topLine]->offset,
	    textPtr->linePtrPtr[lastLine]->offset);
#endif
	return TCL_ERROR;
    }
    textPtr->last = bottomLine;
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * LineSearch --
 *
 * 	Performs a binary search for the line located at some world
 *	Y coordinate. The search is limited to those lines between
 *	low and high inclusive.
 *
 * Results:
 *	Returns the line number at the given Y coordinate. If position
 *	does not correspond to any of the lines in the given the set,
 *	-1 is returned.
 *
 * ----------------------------------------------------------------------
 */
static int
LineSearch(textPtr, position, low, high)
    Htext *textPtr;
    int position;
    int low, high;
{
    register int midPoint;
    register Line *linePtr;

    while (low <= high) {
	midPoint = (low + high) >> 1;
	linePtr = textPtr->linePtrPtr[midPoint];
	if (position < linePtr->offset) {
	    high = midPoint - 1;
	} else if (position >= (linePtr->offset + linePtr->height)) {
	    low = midPoint + 1;
	} else {
	    return (midPoint);
	}
    }
    return -1;
}

/*
 * ----------------------------------------------------------------------
 *
 * UpdateScrollbar --
 *
 * 	Invoke a Tcl command to the scrollbar, defining the new
 *	position and length of the scroll. See the Tk documentation
 *	for further information on the scrollbar.  It is assumed the
 *	scrollbar command prefix is valid.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Scrollbar is commanded to change position and/or size.
 *
 * ----------------------------------------------------------------------
 */
static void
UpdateScrollbar(interp, command, total, window, first, units)
    Tcl_Interp *interp;
    char *command;		/* scrollbar command */
    int total;			/* Total distance */
    int window;			/* Window distance */
    int first;			/* Position of viewport */
    int units;			/* Unit distance */
{
    char scrollArgs[1000];
    int totalUnits, windowUnits;
    int firstUnit, lastUnit;

    totalUnits = (total / units) + 1;
    windowUnits = window / units;
    firstUnit = first / units;
    lastUnit = (firstUnit + windowUnits);

    /* Keep scrolling within range. */
    if (firstUnit >= totalUnits) {
	firstUnit = totalUnits;
    }
    if (lastUnit > totalUnits) {
	lastUnit = totalUnits;
    }
    sprintf(scrollArgs, " %d %d %d %d", totalUnits, windowUnits,
	firstUnit, lastUnit);
    if (Tcl_VarEval(interp, command, scrollArgs, (char *)NULL) != TCL_OK) {
	Tk_BackgroundError(interp);
    }
}


static void
FreeAllLines(textPtr)
    Htext *textPtr;
{
    register int i;

    for (i = 0; i < textPtr->numLines; i++) {
	DestroyLine(textPtr->linePtrPtr[i]);
    }
    if (textPtr->linePtrPtr != NULL) {
	free((char *)textPtr->linePtrPtr);
    }
    textPtr->linePtrPtr = NULL;
    textPtr->arraySize = textPtr->numLines = 0;
}


static void
AdjustLinesAllocated(textPtr)
    Htext *textPtr;
{
    if (textPtr->arraySize > 0) {
	ResizeArray((char **)&(textPtr->linePtrPtr), sizeof(Line *),
	    textPtr->numLines, textPtr->arraySize);
	textPtr->arraySize = textPtr->numLines;
	textPtr->first = 0;
	textPtr->last = textPtr->numLines - 1;
	textPtr->pendingX = textPtr->pendingY = 0;
	textPtr->vPort.width = textPtr->vPort.height = 0;
	textPtr->vPort.x = textPtr->vPort.y = 0;
    }
}


static Line *
GetLastLine(textPtr)
    Htext *textPtr;
{
    Line *linePtr;

    if (textPtr->numLines == 0) {
	linePtr = NewLine(textPtr);

	if (linePtr == NULL) {
	    textPtr->interp->result = "can't allocate line structure";
	    return NULL;
	}
    }
    linePtr = textPtr->linePtrPtr[textPtr->numLines - 1];
    return linePtr;
}

static void
SetTextInfo(linePtr, text, size)
    Line *linePtr;
    char *text;
    int size;
{
    linePtr->numChars = size;
    if (linePtr->text != NULL) {
	free((char *)linePtr->text);
    }
    linePtr->text = strdup(text);
}

static void
GetTextInfo(linePtr, buffer, sizePtr)
    Line *linePtr;
    char *buffer;
    int *sizePtr;
{
    *sizePtr = linePtr->numChars;
    if (linePtr->numChars > 0) {
	strcpy(buffer, linePtr->text);
    }
    buffer[linePtr->numChars] = 0;
}

/*
 * ----------------------------------------------------------------------
 *
 * ParseInput --
 *
 * 	Parse the input to the Htext structure into an array of lines.
 *
 * Results:
 *	Returns TCL_OK or error depending if the file was read correctly.
 *
 * ----------------------------------------------------------------------
 */
static int
ParseInput(interp, textPtr, inputPtr)
    Tcl_Interp *interp;
    Htext *textPtr;
    char *inputPtr;
{
    register Line *linePtr;
    int c;

#define HUGE_LINE_SIZE 1024
    char buffer[HUGE_LINE_SIZE];
#define HUGE_COMMAND_SIZE 10000
    char command[HUGE_COMMAND_SIZE];
    int count;
    register int state;

    linePtr = GetLastLine(textPtr);
    if (linePtr == NULL) {
	return TCL_ERROR;	/* Error allocating line */
    }
    GetTextInfo(linePtr, buffer, &count);
    state = 0;
    while ((c = *inputPtr++) != '\0') {
	if (c == textPtr->specChar) {
	    state++;
	} else if (c == '\n') {
	    state = -1;
	} else if ((state == 0) && (c == '\\')) {
	    state = 3;
	} else {
	    state = 0;
	}

	switch (state) {
	case 2:		/* Tcl Command block found */
	    count--;
	    inputPtr = CollectCommand(textPtr, inputPtr, command);
	    if (inputPtr == NULL) {
		return TCL_ERROR;
	    }
	    linePtr->numChars = count;
	    if (Tcl_Eval(interp, command) != TCL_OK) {
		return TCL_ERROR;
	    }
	    state = 0;
	    break;

	case 4:		/* Escaped block designator */
	    buffer[count - 1] = c;
	    state = 0;
	    break;

	case -1:		/* End of text line  */
	    buffer[count] = '\0';
	    SetTextInfo(linePtr, buffer, count);
	    linePtr = NewLine(textPtr);
	    if (linePtr == NULL) {
		return TCL_ERROR;
	    }
	    count = state = 0;
	    break;

	default:		/* Default action, add to text buffer */
	    buffer[count++] = c;
	    break;
	}
	if (count == HUGE_LINE_SIZE) {
	    interp->result = "text line is too long";
	    return TCL_ERROR;
	}
    }
    if (count > 0) {
	buffer[count] = '\0';
	SetTextInfo(linePtr, buffer, count);
    }
    return TCL_OK;
}


static char *
ReadFile(interp, fileName)
    Tcl_Interp *interp;
    char *fileName;
{
    FILE *fp;
    int arraySize;
    register int numBytes;
    char *buffer;
    register int count;
    char *result = NULL;

    fp = fopen(fileName, "r");
    if (fp == NULL) {
	Tcl_AppendResult(interp, "can't open \"", fileName,
	    "\" for reading: ", Tcl_PosixError(interp), (char *)NULL);
	return NULL;
    }
    count = 0;
    arraySize = BUFSIZ;		/* Initial array size */
    if (ResizeArray(&buffer, sizeof(char), arraySize, count) != TCL_OK) {
	interp->result = "can't allocate character array";
	return NULL;
    }
    for (;;) {
	/* Read in next block of text */
	numBytes = fread(&buffer[count], sizeof(char), BUFSIZ, fp);

	if (numBytes < 0) {
	    Tcl_AppendResult(interp, "error reading \"", fileName, "\": ",
		Tcl_PosixError(interp), (char *)NULL);
	    goto error;
	} else if (numBytes == 0) {
	    break;
	}
	count += numBytes;
	if (count >= arraySize) {
	    /* Reallocate with double the buffer size */
	    arraySize += arraySize;
	    if (ResizeArray(&buffer, sizeof(char),
		    arraySize, count) != TCL_OK) {
		interp->result = "can't allocate character array";
		goto error;
	    }
	}
    }
    buffer[count] = '\0';
    result = buffer;
  error:
    fclose(fp);
    return result;
}


static char *
CollectCommand(textPtr, inputPtr, buffer)
    Htext *textPtr;
    char *inputPtr;
    char *buffer;
{
    register int c;
    register int state;
    register int count;


    /* Simply collect the all the characters until %% into a buffer */

    state = count = 0;
    while ((c = *inputPtr++) != '\0') {
	if (c == textPtr->specChar) {
	    state++;
	} else if ((state == 0) && (c == '\\')) {
	    state = 3;
	} else {
	    state = 0;
	}

	switch (state) {
	case 2:		/* End of command block found */
	    buffer[count - 1] = '\0';
	    return (inputPtr);

	case 4:		/* Escaped block designator */
	    buffer[count] = c;
	    state = 0;
	    break;

	default:		/* Add to command buffer */
	    buffer[count++] = c;
	    break;
	}
	if (count == HUGE_COMMAND_SIZE) {
	    textPtr->interp->result = "command block is too long";
	    return NULL;
	}
    }
    textPtr->interp->result = "premature end of TCL command block";
    return NULL;
}

static int
IncludeText(interp, textPtr, fileName)
    Tcl_Interp *interp;
    Htext *textPtr;
    char *fileName;
{
    char *inputPtr;
    int result;

    if ((textPtr->text == NULL) && (fileName == NULL)) {
	return TCL_OK;		/* Empty text string */
    }
    inputPtr = textPtr->text;
    if (fileName != NULL) {
	inputPtr = ReadFile(interp, fileName);
	if (inputPtr == NULL) {
	    return TCL_ERROR;
	}
    }
    result = ParseInput(interp, textPtr, inputPtr);
    if (fileName != NULL) {
	free(inputPtr);
    }
    return (result);
}

/* ARGSUSED */
static char *
HtextVarProc(clientData, interp, name1, name2, flags)
    ClientData clientData;	/* Information about widget. */
    Tcl_Interp *interp;		/* Interpreter containing variable. */
    char *name1;		/* Name of variable. */
    char *name2;		/* Second part of variable name. */
    int flags;			/* Information about what happened. */
{
    Htext *textPtr = (Htext *)clientData;
    Htext *lastTextPtr;

    /* Check to see of this is the most recent trace */
    lastTextPtr = (Htext *)Tcl_VarTraceInfo2(interp, name1, name2, flags,
	HtextVarProc, (ClientData)NULL);
    if (lastTextPtr != textPtr) {
	return NULL;		/* Ignore all but most current trace */
    }
    if (flags & TCL_TRACE_READS) {
	char c;

	c = name2[0];
	if ((c == 'w') && (strcmp(name2, "widget") == 0)) {
	    Tcl_SetVar2(interp, name1, name2, Tk_PathName(textPtr->tkwin),
		flags);
	} else if ((c == 'l') && (strcmp(name2, "line") == 0)) {
	    char buf[80];

	    sprintf(buf, "%d", textPtr->numLines);
	    Tcl_SetVar2(interp, name1, name2, buf, flags);
	} else if ((c == 'f') && (strcmp(name2, "file") == 0)) {
	    register char *fileName;

	    fileName = textPtr->fileName;
	    if (fileName == NULL) {
		fileName = "";
	    }
	    Tcl_SetVar2(interp, name1, name2, fileName, flags);
	} else {
	    return ("Unknown variable");
	}
    }
    return NULL;
}

static void
CreateTraces(textPtr)
    Htext *textPtr;
{
    register Tcl_Interp *interp = textPtr->interp;
    int flags = (TCL_GLOBAL_ONLY | TCL_TRACE_READS);
    static char globalCmd[] = "global blt_htext";

    Tcl_TraceVar2(interp, HTEXT_CMDNAME, "widget", flags, HtextVarProc,
	(ClientData)textPtr);
    Tcl_TraceVar2(interp, HTEXT_CMDNAME, "line", flags, HtextVarProc,
	(ClientData)textPtr);
    Tcl_TraceVar2(interp, HTEXT_CMDNAME, "file", flags, HtextVarProc,
	(ClientData)textPtr);
    /*
     * Make the traced variables global to the widget
     */
    Tcl_Eval(interp, globalCmd);
}

static void
DeleteTraces(textPtr)
    Htext *textPtr;
{
    register Tcl_Interp *interp = textPtr->interp;
    int flags = (TCL_GLOBAL_ONLY | TCL_TRACE_READS);

    Tcl_UntraceVar2(interp, HTEXT_CMDNAME, "file", flags, HtextVarProc,
	(ClientData)textPtr);
    Tcl_UntraceVar2(interp, HTEXT_CMDNAME, "line", flags, HtextVarProc,
	(ClientData)textPtr);
    Tcl_UntraceVar2(interp, HTEXT_CMDNAME, "widget", flags, HtextVarProc,
	(ClientData)textPtr);

}

/*
 * ----------------------------------------------------------------------
 *
 * FindChild --
 *
 *	Searches for a child widget matching the path name given
 *	If found, the pointer to the child structure is returned,
 *	otherwise NULL.
 *
 * Results:
 *	The pointer to the child structure. If not found, NULL.
 *
 * ----------------------------------------------------------------------
 */
static Child *
FindChild(textPtr, name)
    Htext *textPtr;		/* Hypertext widget structure */
    char *name;			/* Path name of child window  */
{
    Tcl_HashEntry *entryPtr;
    Tk_Window tkwin;

    tkwin = Tk_NameToWindow(textPtr->interp, name, textPtr->tkwin);
    if (tkwin == NULL) {
	return NULL;
    }
    entryPtr = Tcl_FindHashEntry(&(textPtr->subwindows), (char *)tkwin);
    if (entryPtr != NULL) {
	return (Child *) Tcl_GetHashValue(entryPtr);
    }
    return NULL;
}

/*
 *--------------------------------------------------------------
 *
 * ChildGeometryProc --
 *
 *	This procedure is invoked by Tk_GeometryRequest for
 *	subwindows managed by the hypertext widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Arranges for tkwin, and all its managed siblings, to
 *	be repacked and drawn at the next idle point.
 *
 *--------------------------------------------------------------
 */
 /* ARGSUSED */
static void
ChildGeometryProc(clientData, tkwin)
    ClientData clientData;	/* Information about window that got new
			         * preferred geometry.  */
    Tk_Window tkwin;		/* Other Tk-related information about the
			         * window. */
{
    Child *childPtr = (Child *)clientData;

    if ((childPtr->windowWidth != GetReqWidth(childPtr)) ||
	(childPtr->windowHeight != GetReqHeight(childPtr))) {
	childPtr->textPtr->flags |= REQUEST_LAYOUT;
	EventuallyRedraw(childPtr->textPtr);
    }
}

static void
SendBogusEvent(tkwin)
    Tk_Window tkwin;
{
    enum {
	DontPropagate = 0
    };
    XEvent event;

    event.type = event.xexpose.type = Expose;
    event.xexpose.window = Tk_WindowId(tkwin);
    event.xexpose.display = Tk_Display(tkwin);
    event.xexpose.count = 0;
    event.xexpose.x = event.xexpose.y = 0;
    event.xexpose.width = Tk_Width(tkwin);
    event.xexpose.height = Tk_Height(tkwin);
    XSendEvent(Tk_Display(tkwin), Tk_WindowId(tkwin), DontPropagate,
	ExposureMask, &event);
}

/*
 * --------------------------------------------------------------------
 *
 * ResizeArray --
 *
 *	Reallocates memory to the new size given.  New memory
 *	is also cleared (zeros).
 *
 * Results:
 *	Returns a pointer to the new object or NULL if an error occurred.
 *
 * Side Effects:
 *	Memory is re/allocated.
 *
 * --------------------------------------------------------------------
 */
static int
ResizeArray(arrayPtr, elemSize, newLength, prevLength)
    char **arrayPtr;
    unsigned int elemSize;
    unsigned int newLength;
    unsigned int prevLength;
{
    char *newPtr;

    if (newLength == prevLength) {
	return TCL_OK;
    }
    if (newLength == 0) {	/* Free entire array */
	free(*arrayPtr);
	*arrayPtr = NULL;
	return TCL_OK;
    }
    newPtr = (char *)calloc(elemSize, newLength);
    if (newPtr == NULL) {
	return TCL_ERROR;
    }
    if ((prevLength > 0) && (*arrayPtr != NULL)) {
	unsigned int size;

	size = BLT_MIN(prevLength, newLength) * elemSize;
	if (size > 0) {
	    memcpy(newPtr, *arrayPtr, size);
	}
	free(*arrayPtr);
    }
    *arrayPtr = newPtr;
    return TCL_OK;
}

#ifdef notdef
static int
TextXYToIndex(TextPtr, x, y)
    Text *TextPtr;
    int x, y;

{
    Line *line = TextPtr->topLine;
    int result, dummy;

    /* locate the line */

    y -= TextPtr->borderWidth + 1;

    if (y > 0) {
	int count = y / TextPtr->pmHeight;

	while (line->length > 0 && count--)
	    line = line->next;
    }
    if (line->length == 0)
	result = TextPtr->numChars;
    else
	result = line->index +
	    TkMeasureChars(TextPtr->fontPtr,
	    line->text,
	    line->length,
	    TextPtr->offset,
	    x + TextPtr->leftIndex * TextPtr->avgWidth,
	    0, &dummy);

    return result;
}

/*
 *--------------------------------------------------------------
 *
 * GetTextIndex --
 *
 *	Parse an index into a Text widget and return either its value
 *	or an error.
 *
 * Results:
 *	A standard Tcl result.  If all went well, then *indexPtr is
 *	filled in with the index (into TextPtr) corresponding to
 *	string.  Otherwise an error message is left in interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
static int
GetTextIndex(interp, TextPtr, string, indexPtr)
    Tcl_Interp *interp;		/* For error messages. */
    Text *TextPtr;		/* Text for which the index is being
				 * specified. */
    char *string;		/* Numerical index into TextPtr's element
				 * list, or "end" to refer to last element. */
    int *indexPtr;		/* Where to store converted relief. */
{
    int length;
    char c;

    length = strlen(string);
    c = string[0];

    if ((c == 'e') && (strncmp(string, "end", length) == 0)) {
	*indexPtr = TextPtr->numChars;
    } else if ((c == 'c') && (strncmp(string, "cursor", length) == 0)) {
	*indexPtr = TextPtr->cursorPos;
    } else if ((c == 's') && (length > 4) &&
	(strncmp(string, "sel.first", length) == 0)) {

	if (TextPtr->selectFirst == -1) {
	    interp->result = "selection isn't in Text";
	    return TCL_ERROR;
	}
    } else if ((c == 's') && (length > 4) &&
	(strncmp(string, "sel.last", length) == 0)) {
	*indexPtr = TextPtr->selectLast;
    } else if ((c == 't') && (strncmp(string, "top", length) == 0)) {
	*indexPtr = TextPtr->topLine->index;
    } else if (c == '@') {
	char *comma;
	int x, y;

	string++;
	if ((comma = strchr(string, ',')) == NULL) {
	    goto badIndex;
	}
	*comma = '\0';
	if ((Tcl_GetInt(interp, string, &x) != TCL_OK) ||
	    (Tcl_GetInt(interp, comma + 1, &y) != TCL_OK)) {
	    goto badIndex;
	}
	if (TextPtr->numChars == 0) {
	    *indexPtr = 0;
	} else {
	    *indexPtr = TextXYToIndex(TextPtr, x, y);
	}
    } else {
	if (Tcl_GetInt(interp, string, indexPtr) != TCL_OK) {
	    goto badIndex;
	}
	if (*indexPtr < 0) {
	    *indexPtr = 0;
	} else if (*indexPtr > TextPtr->numChars) {
	    *indexPtr = TextPtr->numChars;
	}
    }
    return TCL_OK;
  badIndex:

    /*
     * Some of the paths here leave messages in interp->result, so we
     * have to clear it out before storing our own message.
     */
    Tcl_SetResult(interp, (char *)NULL, TCL_STATIC);
    Tcl_AppendResult(interp, "bad text index \"", string,
	"\"", (char *)NULL);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * TextSelectTo --
 *
 *	Modify the selection by moving its un-anchored end.  This could
 *	make the selection either larger or smaller.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The selection changes.
 *
 *----------------------------------------------------------------------
 */
static void
TextSelectTo(TextPtr, index)
    register Text *TextPtr;	/* Information about widget. */
    int index;			/* Index of element that is to
				 * become the "other" end of the
				 * selection. */
{
    int newFirst, newLast;

    /*
     * Grab the selection if we don't own it already.
     */

    if (TextPtr->selectFirst == -1) {
	Tk_OwnSelection(TextPtr->tkwin, TextLostSelection,
	    (ClientData)TextPtr);
    }
    if (index < 0) {
	index = 0;
    }
    if (index >= TextPtr->numChars) {
	index = TextPtr->numChars - 1;
    }
    if (TextPtr->selectAnchor > TextPtr->numChars) {
	TextPtr->selectAnchor = TextPtr->numChars;
    }
    if (TextPtr->selectAnchor <= index) {
	newFirst = TextPtr->selectAnchor;
	newLast = index;
    } else {
	newFirst = index;
	newLast = TextPtr->selectAnchor - 1;
	if (newLast < 0) {
	    newFirst = newLast = -1;
	}
    }
    if ((TextPtr->selectFirst == newFirst)
	&& (TextPtr->selectLast == newLast)) {
	return;
    }
    TextPtr->selectFirst = newFirst;
    TextPtr->selectLast = newLast;
    TextPtr->flags |= REFRESH_NEEDED;
    EventuallyRedraw(TextPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * TextFetchSelection --
 *
 *	This procedure is called back by Tk when the selection is
 *	requested by someone.  It returns part or all of the selection
 *	in a buffer provided by the caller.
 *
 * Results:
 *	The return value is the number of non-NULL bytes stored
 *	at buffer.  Buffer is filled (or partially filled) with a
 *	NULL-terminated string containing part or all of the selection,
 *	as given by offset and maxBytes.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static int
TextFetchSelection(clientData, offset, buffer, maxBytes)
    ClientData clientData;	/* Information about Text widget. */
    int offset;			/* Offset within selection of first
				 * character to be returned. */
    char *buffer;		/* Location in which to place
				 * selection. */
    int maxBytes;		/* Maximum number of bytes to place
				 * at buffer, not including terminating
				 * NULL character. */
{
    Text *TextPtr = (Text *) clientData;
    char *text;
    int count;

    if (TextPtr->selectFirst < 0) {
	return -1;
    }
    count = TextPtr->selectLast + 1 - TextPtr->selectFirst - offset;
    if (count > maxBytes) {
	count = maxBytes;
    }
    if (count <= 0) {
	return 0;
    }
    text = GetText(TextPtr, TextPtr->selectFirst + offset, count);

    if (text != NULL) {
	strcpy(buffer, text);
	free(text);
    } else
	count = 0;

    return count;
}

/*
 *----------------------------------------------------------------------
 *
 * TextLostSelection --
 *
 *	This procedure is called back by Tk when the selection is
 *	grabbed away from a Text widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The existing selection is unhighlighted, and the window is
 *	marked as not containing a selection.
 *
 *----------------------------------------------------------------------
 */
static void
TextLostSelection(clientData)
    ClientData clientData;	/* Information about Text widget. */
{
    Text *TextPtr = (Text *) clientData;

    TextPtr->selectFirst = -1;
    TextPtr->selectLast = -1;
    EventuallyRedraw(TextPtr);
}

#endif

/*
 *----------------------------------------------------------------------
 *
 * GotoLine --
 *
 *	Move the top line of the viewport to the new location based
 *	upon the given line number.  Force outlier requests to the
 *	top or bottom of text.
 *
 * Results:
 *	Stanard Tcl result. If TCL_OK, interp->result contains the
 *	current line number.
 *
 * Side effects:
 *	At the next idle point, the text viewport will be move to the
 *	new line.
 *
 *----------------------------------------------------------------------
 */
static int
GotoLine(textPtr, interp, argc, argv)
    Htext *textPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int line;

    if (argc > 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " gotoline ?line?\"", (char *)NULL);
	return TCL_ERROR;
    }
    line = textPtr->first;
    if (argc == 3) {
	if ((argv[2][0] == 'e') && (strcmp(argv[2], "end") == 0)) {
	    line = textPtr->numLines - 1;
	} else {
	    if (Tcl_GetInt(textPtr->interp, argv[2], &line) != TCL_OK) {
		return TCL_ERROR;
	    }
	    /* Keep the line request within the current range of lines */
	    if (line < 1) {
		line = 1;
	    } else if (line > textPtr->numLines) {
		line = textPtr->numLines;
	    }
	    line--;
	}
	textPtr->reqLineNum = line;
	textPtr->flags |= VIEW_MOVED;

	/*
	 * Make only a request for a change in the viewport.  Defer
	 * the actual scrolling until the text layout is adjusted at
	 * the next idle point.
	 */
	if (line != textPtr->first) {
	    textPtr->flags |= REQUEST_GOTO;
	    EventuallyRedraw(textPtr);
	}
    }
    sprintf(textPtr->interp->result, "%d", line + 1);
    return TCL_OK;
}

static int
ScrollX(textPtr, string)
    Htext *textPtr;
    char *string;
{
    int x;

    x = textPtr->vPort.x;
    if (string != NULL) {
	if (Tk_GetPixels(textPtr->interp,
		textPtr->tkwin, string, &x) != TCL_OK) {
	    return TCL_ERROR;
	}
	x *= textPtr->scrollX;	/* Convert to pixels */
	if (x > textPtr->vPort.width) {
	    x = textPtr->vPort.width - 1;
	} else if (x < 0) {
	    x = 0;
	}
	if (x != textPtr->vPort.x) {
	    textPtr->pendingX = x;
	    textPtr->flags |= VIEW_MOVED;
	    EventuallyRedraw(textPtr);
	}
    }
    sprintf(textPtr->interp->result, "%d", x / textPtr->scrollX);
    return TCL_OK;
}


static int
ScrollY(textPtr, string)
    Htext *textPtr;
    char *string;
{
    int y;

    y = textPtr->vPort.y;
    if (string != NULL) {
	if (Tk_GetPixels(textPtr->interp,
		textPtr->tkwin, string, &y) != TCL_OK) {
	    return TCL_ERROR;
	}
	y *= textPtr->scrollY;	/* Convert to pixels */
	if (y > textPtr->vPort.height) {
	    y = textPtr->vPort.height - 1;
	} else if (y < 0) {
	    y = 0;
	}
	if (y != textPtr->vPort.y) {
	    textPtr->pendingY = y;
	    textPtr->flags |= VIEW_MOVED;
	    EventuallyRedraw(textPtr);
	}
    }
    sprintf(textPtr->interp->result, "%d", y / textPtr->scrollY);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetChildWindows --
 *
 *	Returns a list of all the pathNames of child windows of the
 *	Htext widget.  If a pattern argument is given, only the names
 *	of windows matching it will be placed into the list.
 *
 * Results:
 *	Standard Tcl result.  If TCL_OK, interp->result will contain
 *	the list of the child window pathnames.  Otherwise it will
 *	contain an error message.
 *
 *----------------------------------------------------------------------
 */
static int
GetChildren(textPtr, interp, argc, argv)
    Htext *textPtr;		/* Hypertext widget record */
    Tcl_Interp *interp;		/* Interpreter associated with widget */
    int argc;
    char **argv;
{
    Child *childPtr;
    Tcl_HashEntry *entryPtr;
    Tcl_HashSearch searchId;
    char *name;

    if ((argc != 2) && (argc != 3)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " children ?pattern?\"", (char *)NULL);
	return TCL_ERROR;
    }
    for (entryPtr = Tcl_FirstHashEntry(&(textPtr->subwindows), &searchId);
	entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&searchId)) {
	childPtr = (Child *)Tcl_GetHashValue(entryPtr);
	if (childPtr->tkwin == NULL) {
	    fprintf(stderr, "window `%s' is null\n",
		Tk_PathName(Tcl_GetHashKey(&(textPtr->subwindows), entryPtr)));
	    continue;
	}
	name = Tk_PathName(childPtr->tkwin);
	if ((argc == 2) || (Tcl_StringMatch(name, argv[2]))) {
	    Tcl_AppendElement(interp, name);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureParent --
 *
 * 	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	a hypertext widget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 * 	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 * 	etc. get set for textPtr;  old resources get freed, if there were any.
 * 	The hypertext is redisplayed.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureParent(textPtr, interp, argc, argv)
    Htext *textPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (argc == 2) {
	return (Tk_ConfigureInfo(interp, textPtr->tkwin, configSpecs,
		(char *)textPtr, (char *)NULL, 0));
    } else if (argc == 3) {
	return (Tk_ConfigureInfo(interp, textPtr->tkwin, configSpecs,
		(char *)textPtr, argv[2], 0));
    } else {
	if (ConfigureHtext(interp, textPtr, argc - 2, argv + 2,
		TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	    return TCL_ERROR;
	}
	EventuallyRedraw(textPtr);
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureChild --
 *
 * 	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	a hypertext child.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 * 	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information get set for the child cavity; old
 *	resources get freed, if there were any. The child is marked
 *	for redisplay.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureChild(textPtr, interp, argc, argv)
    Htext *textPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Child *childPtr;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " childconfigure childName ?args...?\"", (char *)NULL);
	return TCL_ERROR;
    }
    childPtr = FindChild(textPtr, argv[2]);
    if (childPtr == NULL) {
	Tcl_AppendResult(interp, "can't find window \"", argv[2], "\" in \"",
	    Tk_PathName(textPtr->tkwin), "\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (argc == 3) {
	return (Tk_ConfigureInfo(interp, textPtr->tkwin, childConfigSpecs,
		(char *)childPtr, (char *)NULL, 0));
    } else if (argc == 4) {
	return (Tk_ConfigureInfo(interp, textPtr->tkwin, childConfigSpecs,
		(char *)childPtr, argv[3], 0));
    } else {
	if (Tk_ConfigureWidget(interp, textPtr->tkwin, childConfigSpecs,
		argc - 3, argv + 3, (char *)childPtr,
		TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	    return TCL_ERROR;
	}
	textPtr->flags |= REQUEST_LAYOUT;
	EventuallyRedraw(textPtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ScanText --
 *
 *	Implements the quick scan for hypertext widgets.
 *
 *----------------------------------------------------------------------
 */
static int
ScanText(textPtr, interp, argc, argv)
    Htext *textPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int markX, markY;
    char c;
    int length;

    if (argc != 5) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " scan mark|dragto x y\"", (char *)NULL);
	return TCL_ERROR;
    }
    if ((Tcl_GetInt(interp, argv[3], &markX) != TCL_OK) ||
	(Tcl_GetInt(interp, argv[4], &markY) != TCL_OK)) {
	return TCL_ERROR;
    }
    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 'm') && (strncmp(argv[2], "mark", length) == 0)) {
	textPtr->scanMark.x = markX, textPtr->scanMark.y = markY;
	textPtr->scanPt.x = textPtr->vPort.x;
	textPtr->scanPt.y = textPtr->vPort.y;

    } else if ((c == 'd') && (strncmp(argv[2], "dragto", length) == 0)) {
	int x, y;

	x = textPtr->scanPt.x - (10 * (markX - textPtr->scanMark.x));
	y = textPtr->scanPt.y - (10 * (markY - textPtr->scanMark.y));

	if (x < 0) {
	    x = textPtr->scanPt.x = 0;
	    textPtr->scanMark.x = markX;
	} else if (x >= textPtr->vPort.width) {
	    x = textPtr->scanPt.x = textPtr->vPort.width - textPtr->scrollX;
	    textPtr->scanMark.x = markX;
	}
	if (y < 0) {
	    y = textPtr->scanPt.y = 0;
	    textPtr->scanMark.y = markY;
	} else if (y >= textPtr->vPort.height) {
	    y = textPtr->scanPt.y = textPtr->vPort.height - textPtr->scrollY;
	    textPtr->scanMark.y = markY;
	}
	if ((y != textPtr->pendingY) || (x != textPtr->pendingX)) {
	    textPtr->pendingX = x, textPtr->pendingY = y;
	    textPtr->flags |= VIEW_MOVED;
	    EventuallyRedraw(textPtr);
	}
    } else {
	Tcl_AppendResult(interp, "bad scan option \"", argv[2],
	    "\":  must be mark or dragto", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SearchText --
 *
 *	Returns the linenumber of the next line matching the given
 *	pattern within the range of lines provided.  If the first
 *	line number is greater than the last, the search is done in
 *	reverse.
 *
 *----------------------------------------------------------------------
 */
static int
SearchText(textPtr, interp, argc, argv)
    Htext *textPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int first, last;
    register int i;
    register char *string;
    int lineNum;
    enum Direction {
	REVERSE = -1, FORWARD = 1
    } direction;		/* Indicates the direction of the search */

    if ((argc < 3) || (argc > 5)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " search pattern ?from? ?to?\"", (char *)NULL);
	return TCL_ERROR;
    }
    first = 0;
    last = textPtr->numLines - 1;

    if (argc > 3) {
	if (argv[3][0] == 'e' && strcmp(argv[3], "end") == 0) {
	    first = textPtr->numLines - 1;
	} else {
	    if (Tcl_GetInt(interp, argv[3], &first) != TCL_OK) {
		return TCL_ERROR;
	    }
	    first--;
	    if (first < 0) {
		first = 0;
	    } else if (first >= textPtr->numLines) {
		first = textPtr->numLines - 1;
	    }
	}
    }
    if (argc > 4) {
	if ((argv[4][0] == 'e') && (strcmp(argv[4], "end") == 0)) {
	    last = textPtr->numLines - 1;
	} else {
	    if (Tcl_GetInt(interp, argv[4], &last) != TCL_OK) {
		return TCL_ERROR;
	    }
	    last--;
	    if (last < 0) {
		last = 0;
	    } else if (last >= textPtr->numLines) {
		last = textPtr->numLines - 1;
	    }
	}
    }
    direction = (first > last) ? REVERSE : FORWARD;
    lineNum = -1;
    for (i = first; i != (last + direction); i += direction) {
	string = textPtr->linePtrPtr[i]->text;
	if ((string != NULL) && Tcl_StringMatch(string, argv[2])) {
	    lineNum = i + 1;
	    break;
	}
    }
    sprintf(textPtr->interp->result, "%d", lineNum);
    return TCL_OK;
}

/*
 * --------------------------------------------------------------
 *
 * TextWidgetCmd --
 *
 * 	This procedure is invoked to process the Tcl command that
 *	corresponds to a widget managed by this module. See the user
 * 	documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 * --------------------------------------------------------------
 */
static int
TextWidgetCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Information about hypertext widget. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    register Htext *textPtr = (Htext *)clientData;
    int result = TCL_OK;
    int length;
    char c;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " option ?arg arg ...?\"", (char *)NULL);
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)textPtr);
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'a') && (strncmp(argv[1], "append", length) == 0)) {
	result = AppendChild(textPtr, interp, argc, argv);
    } else if ((c == 'c') && (length > 5) &&
	(strncmp(argv[1], "childconfigure", length) == 0)) {
	result = ConfigureChild(textPtr, interp, argc, argv);
    } else if ((c == 'c') && (length > 5) &&
	(strncmp(argv[1], "children", length) == 0)) {
	result = GetChildren(textPtr, interp, argc, argv);
    } else if ((c == 'c') && (length > 1) &&
	(strncmp(argv[1], "configure", length) == 0)) {
	result = ConfigureParent(textPtr, interp, argc, argv);
    } else if ((c == 'g') && (strncmp(argv[1], "gotoline", length) == 0)) {
	result = GotoLine(textPtr, interp, argc, argv);
    } else if ((c == 'x') && (strncmp(argv[1], "xview", length) == 0)) {
	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" xview ?position?\"", (char *)NULL);
	    goto error;
	}
	result = ScrollX(textPtr, argv[2]);
    } else if ((c == 'y') && (strncmp(argv[1], "yview", length) == 0)) {
	if (argc > 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" yview ?position?\"", (char *)NULL);
	    goto error;
	}
	result = ScrollY(textPtr, argv[2]);
    } else if ((c == 's') && (length > 1)
	&& (strncmp(argv[1], "scan", length) == 0)) {
	result = ScanText(textPtr, interp, argc, argv);
    } else if ((c == 's') && (length > 2) &&
	strncmp(argv[1], "search", length) == 0) {
	result = SearchText(textPtr, interp, argc, argv);
#ifdef notdef
    } else if ((c == 's') && (length > 1) &&
	strncmp(argv[1], "select", length) == 0) {
	int index;

	if (argc < 3) {
	    Tcl_AppendResult(interp, "too few args: should be \"",
		argv[0], " select option ?index?\"", (char *)NULL);
	    goto error;
	}
	length = strlen(argv[2]);
	c = argv[2][0];
	if ((c == 'c') && (strncmp(argv[2], "clear", length) == 0)) {
	    if (argc != 3) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " select clear\"", (char *)NULL);
		goto error;
	    }
	    if (textPtr->selectFirst != -1) {
		textPtr->selectFirst = textPtr->selectLast = -1;
		goto redisplay;
	    }
	    goto error;
	}
	if (argc >= 4) {
	    if (GetTextIndex(interp, textPtr, argv[3], &index) != TCL_OK) {
		goto error;
	    }
	}
	if ((c == 'a') && (strncmp(argv[2], "adjust", length) == 0)) {
	    if (argc != 4) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " select adjust index\"",
		    (char *)NULL);
		goto error;
	    }
	    TextSelectTo(textPtr, index);
	} else if ((c == 'f') && (strncmp(argv[2], "from", length) == 0)) {
	    if (argc != 4) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " select from index\"",
		    (char *)NULL);
		goto error;
	    }
	    textPtr->selectAnchor = index;
	} else if ((c == 't') && (strncmp(argv[2], "to", length) == 0)) {
	    if (argc != 4) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " select to index\"", (char *)NULL);
		goto error;
	    }
	    TextSelectTo(textPtr, index);
	} else {
	    Tcl_AppendResult(interp, "bad select option \"", argv[2],
		"\": must be adjust, clear, from, or to", (char *)NULL);
	    goto error;
	}
#endif
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be ",
	    "append, configure, childconfigure, gotoline, scan, search, xview, or yview",
	    (char *)NULL);
    }
  error:
    Tk_Release((ClientData)textPtr);
    return result;
}

/*
 * --------------------------------------------------------------
 *
 * HtextCmd --
 *
 * 	This procedure is invoked to process the "htext" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 * --------------------------------------------------------------
 */

static int
HtextCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    register Htext *textPtr;
    Tk_Window mainWindow = (Tk_Window)clientData;
    Tk_Window tkwin;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " pathName ?options?\"", (char *)NULL);
	return TCL_ERROR;
    }
    textPtr = (Htext *)calloc(1, sizeof(Htext));
    if (textPtr == NULL) {
	interp->result = "can't allocate text structure";
	return TCL_ERROR;
    }
    tkwin = Tk_CreateWindowFromPath(interp, mainWindow, argv[1], (char *)NULL);
    if (tkwin == NULL) {
	free(textPtr);
	return TCL_ERROR;
    }
    /* Initialize the new hypertext widget */

    Tk_SetClass(tkwin, "Blt_htext");
    textPtr->tkwin = tkwin;
    textPtr->display = Tk_Display(tkwin);
    textPtr->interp = interp;
    textPtr->numLines = textPtr->arraySize = 0;
    textPtr->lineSpacing = 1;
    textPtr->scrollX = textPtr->scrollY = 10;
    Tcl_InitHashTable(&(textPtr->subwindows), TCL_ONE_WORD_KEYS);

    /*
     * -----------------------------------------------------------------
     *
     * Create the widget command before configuring the widget. This
     * is because the "-file" and "-text" options may have embedded
     * commands that self-reference the widget through the
     * "$blt_htext(widget)" variable.
     *
     * ------------------------------------------------------------------
     */

    Tcl_CreateCommand(interp, argv[1], TextWidgetCmd, (ClientData)textPtr,
	(Tcl_CmdDeleteProc *)NULL);
    Tk_CreateEventHandler(tkwin, ExposureMask | StructureNotifyMask,
	TextEventProc, (ClientData)textPtr);

    if (ConfigureHtext(interp, textPtr, argc - 2, argv + 2, 0) != TCL_OK) {
	Tk_DestroyWindow(textPtr->tkwin);
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, Tk_PathName(textPtr->tkwin), TCL_STATIC);

    return TCL_OK;
}

int
Blt_HtextInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, HTEXT_CMDNAME, (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_htext\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_htext\" requires Tk", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", HTEXT_CMDNAME, HTEXT_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, HTEXT_CMDNAME, HtextCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}
