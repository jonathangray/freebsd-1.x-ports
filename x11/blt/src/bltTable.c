
/*
 * bltTable.c --
 *
 *	This module implements a table geometry manager for the
 *	Tk toolkit.
 *
 * Copyright 1993-1994 by AT&T Bell Laboratories.
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
 * Table geometry manager created by George Howlett.
 */

/*
 * To do:
 *
 * 2) No way to detect if window is already a slave of another geometry
 *    manager.
 *
 * 3) No way to detect if window is already a master of another geometry
 *    manager.  This one is bad because is can cause a bad interaction
 *    with the window manager.
 *
 * 5) Relative sizing of partitions?
 *
 * 8) Change row/column allocation procedures?
 *
 */

#include "blt.h"
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#ifndef TABLE_VERSION
#define TABLE_VERSION "1.0"
#endif

#define TRUE 	1
#define FALSE 	0

#ifndef ABS
#define ABS(x)		(((x)<0)?(-(x)):(x))
#endif /* ABS */

#ifndef SHRT_MAX
#define SHRT_MAX	0x7FFF
#endif /* SHRT_MAX */

#ifndef USHRT_MAX
#define	USHRT_MAX	0xFFFF
#endif /* USHRT_MAX */

#define NUMENTRIES(t,type) \
    (((type) == ROW_PARTITION_TYPE) ? (t)->numRows : (t)->numCols)

/*
 * The following enumerated values are used as bit flags.
 */
typedef enum {
    FILL_NONE, FILL_X, FILL_Y, FILL_BOTH
} FillFlags;

typedef enum {
    RESIZE_NONE, RESIZE_EXPAND, RESIZE_SHRINK, RESIZE_BOTH
} ResizeFlags;

static char *fillStrings[] =
{
    "none", "x", "y", "both"
};
static char *resizeStrings[] =
{
    "none", "expand", "shrink", "both"
};

/*
 * Default bounds for both partitions and slave window requested sizes.
 */
#define DEF_MAX_LIMIT	SHRT_MAX
#define DEF_MIN_LIMIT	0

#define WITHOUT_PAD 	0
#define WITH_PAD	1

/*
 * Default values for slave window attributes.
 */
#define DEF_FILL	FILL_NONE
#define DEF_IPAD_X	0
#define DEF_IPAD_Y	0
#define DEF_PAD_X	0
#define DEF_PAD_Y	0
#define DEF_COLUMN_SPAN	1
#define DEF_ROW_SPAN	1
#define DEF_ANCHOR	TK_ANCHOR_CENTER

static int initialized = 0;
static Tcl_HashTable masterWindows, slaveWindows;

/*
 * Sun's bundled and unbundled C compilers choke when using function
 * typedefs that are declared static (but can handle "extern") such as
 *
 * 	static Tk_OptionParseProc parseProc;
 *  	static Tk_OptionPrintProc printProc;
 *
 * Workaround: provide the long forward declarations here.
*/
static int ParseLimits _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *value, char *widgRec, int offset));
static char *PrintLimits _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption LimitsOption =
{
    ParseLimits, PrintLimits, (ClientData)0
};

static int ParseFill _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *value, char *widgRec, int offset));
static char *PrintFill _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption FillOption =
{
    ParseFill, PrintFill, (ClientData)0
};

static int ParseResize _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *value, char *widgRec, int offset));
static char *PrintResize _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption ResizeOption =
{
    ParseResize, PrintResize, (ClientData)0
};

typedef struct {
    int max, min, nom;
} Limits;

typedef enum {
    ROW_PARTITION_TYPE, COLUMN_PARTITION_TYPE
} PartitionTypes;

static char *partitionNames[] =
{
    "row", "column"
};

/*
 * Partition --
 *
 * 	A partition creates a definable space (row or column) in the
 *	table. It may have requested minimum or maximum values which
 *	constrain the size of it.
 *
 */
typedef struct {
    int size;			/* Current size of the partition. This size
				 * is bounded by minSize and maxSize. */
    int nomSize;		/* The nominal size (neither expanded nor
				 * shrunk) of the partition based upon the
				 * requested sizes of the slave windows in
				 * this partition. */
    int minSize, maxSize;	/* Size constraints on the partition */
    int offset;			/* Offset of the partition (in pixels) from
				 * the origin of the master window */
    int span;			/* Minimum spanning window in partition */

    /* user-definable fields */

    ResizeFlags resize;		/* Indicates if the partition should shrink
				 * or expand from its nominal size. */
    int pad;			/* Pads the partition beyond its nominal
				 * size */
    Limits reqSize;		/* Requested bounds for the size of the
				 * partition. The partition will not expand
				 * or shrink beyond these limits, regardless
				 * of how it was specified (max slave size).
				 * This includes any extra padding which may
				 * be specified. */
} Partition;

#define DEF_TBL_RESIZE	"both"

static Tk_ConfigSpec rowConfigSpecs[] =
{
    {TK_CONFIG_CUSTOM, "-height", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Partition, reqSize), TK_CONFIG_NULL_OK,
	&LimitsOption},
    {TK_CONFIG_PIXELS, "-pady", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Partition, pad), 0},
    {TK_CONFIG_CUSTOM, "-resize", (char *)NULL, (char *)NULL,
	DEF_TBL_RESIZE, Tk_Offset(Partition, resize),
	TK_CONFIG_DONT_SET_DEFAULT, &ResizeOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

static Tk_ConfigSpec columnConfigSpecs[] =
{
    {TK_CONFIG_PIXELS, "-padx", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Partition, pad), 0},
    {TK_CONFIG_CUSTOM, "-resize", (char *)NULL, (char *)NULL,
	DEF_TBL_RESIZE, Tk_Offset(Partition, resize),
	TK_CONFIG_DONT_SET_DEFAULT, &ResizeOption},
    {TK_CONFIG_CUSTOM, "-width", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Partition, reqSize), TK_CONFIG_NULL_OK,
	&LimitsOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

static Tk_ConfigSpec *partConfigSpecs[2] =
{
    rowConfigSpecs, columnConfigSpecs
};

struct Table;

/*
 * Cubicle --
 *
 * 	A cubicle is the frame which contains a slave window and its
 *	padding. It may span multiple partitions and have requested
 *	limits which constrain the size of it.  Currently, only a
 *	single window can sit in a cubicle.
 */
typedef struct {
    Tk_Window tkwin;		/* Slave window */
    struct Table *tablePtr;	/* Table managing this window */
    int x, y;			/* Last known position of the slave window
				 * in its parent window.  Used to determine
				 * if the window has moved since last
				 * layout. */
    int extBW;			/* Last known border width of slave window */


    Limits reqWidth, reqHeight;	/* Configurable bounds for width and height
				 * requests made by the slave window */
    int rowSpan;		/* Number of rows spanned by slave */
    int rowIndex;		/* Starting row of span */
    int colSpan;		/* Number of columns spanned by slave */
    int colIndex;		/* Starting column of span */

    Tk_Anchor anchor;		/* Anchor type: indicates how the window is
				 * positioned if extra space is available in
				 * the cubicle */
    int padX, padY;		/* Extra padding around the slave window */
    int ipadX, ipadY;		/* Extra padding inside of the slave window
				 * (in addition to the requested size of
				 * the window) */
    FillFlags fill;		/* Fill style flag */
    Blt_ListEntry *rowEntryPtr;	/* Pointer to cubicle in table's row sorted
				 * list */
    Blt_ListEntry *colEntryPtr;	/* Pointer to cubicle in table's column
			         * sorted list */
} Cubicle;

#define DEF_TBL_FILL	"none"
#define DEF_TBL_ANCHOR	"center"
#define DEF_TBL_SPAN	"1"

static Tk_ConfigSpec cubicleConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", (char *)NULL, (char *)NULL,
	DEF_TBL_ANCHOR, Tk_Offset(Cubicle, anchor),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_INT, "-columnspan", "columnSpan", (char *)NULL,
	DEF_TBL_SPAN, Tk_Offset(Cubicle, colSpan),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_SYNONYM, "-cspan", "columnSpan", (char *)NULL,
	(char *)NULL, Tk_Offset(Cubicle, colSpan), 0},
    {TK_CONFIG_CUSTOM, "-fill", (char *)NULL, (char *)NULL,
	DEF_TBL_FILL, Tk_Offset(Cubicle, fill),
	TK_CONFIG_DONT_SET_DEFAULT, &FillOption},
    {TK_CONFIG_PIXELS, "-padx", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Cubicle, padX), 0},
    {TK_CONFIG_PIXELS, "-pady", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Cubicle, padY), 0},
    {TK_CONFIG_PIXELS, "-ipadx", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Cubicle, ipadX), 0},
    {TK_CONFIG_PIXELS, "-ipady", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Cubicle, ipadY), 0},
    {TK_CONFIG_CUSTOM, "-reqheight", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Cubicle, reqHeight), TK_CONFIG_NULL_OK,
	&LimitsOption},
    {TK_CONFIG_CUSTOM, "-reqwidth", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(Cubicle, reqWidth), TK_CONFIG_NULL_OK,
	&LimitsOption},
    {TK_CONFIG_INT, "-rowspan", "rowSpan", (char *)NULL,
	DEF_TBL_SPAN, Tk_Offset(Cubicle, rowSpan),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_SYNONYM, "-rspan", "rowSpan", (char *)NULL,
	(char *)NULL, Tk_Offset(Cubicle, rowSpan), 0},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/*
 * Cubicles are stored in two linked lists in the Table structure.
 * They are keyed by their starting row,column position.  The
 * following union makes a one-word key out of the row and column
 * components.  This, of course, assumes that a short int is half the
 * size of an int.  Which is NOT the case on a cray;
*/
typedef union {
    struct Position {
	unsigned short row;
	unsigned short column;
    } position;
    unsigned int index;
} SlaveKey;

/*
 * This is the default number of elements in the statically
 * pre-allocated column and row arrays.  This number should reflect a
 * useful number of row and columns, which fit most applications.
*/
#define DEF_ARRAY_SIZE	32

/*
 * Table structure
 */
typedef struct Table {
    int flags;			/* See the flags definitions below. */
    Tk_Window searchWin;	/* Main window of window hierarchy */
    Tk_Window tkwin;		/* The master window in which the slave
				 * windows are arranged. */
    Tcl_Interp *interp;		/* Interpreter associated with all windows */
    Blt_LinkedList *listPtr;	/* Points to either list of row or column
				 * sorted cubicles */
    Blt_LinkedList rowSorted;	/* List of cubicles sorted by increasing
				 * order of rows spanned and row,column
				 * indices */
    Blt_LinkedList colSorted;	/* List of cubicles sorted by increasing
				 * order of columns spanned and column,row
				 * indices */
    /*
     * Pre-allocated row and column partitions.
     */
    Partition colSpace[DEF_ARRAY_SIZE];
    Partition rowSpace[DEF_ARRAY_SIZE];

    Partition *colPtr;		/* Pointer to array of column information:
				 * Initially points to colSpace */
    Partition *rowPtr;		/* Pointer to array of row information:
				 * Initially points to rowSpace */
    int rowSize;		/* Number of rows allocated */
    int numRows;		/* Number of rows currently used */
    int colSize;		/* Number of columns allocated */
    int numCols;		/* Number of columns currently used */
    int width, height;		/* Dimensions of the master window */
    int reqWidth, reqHeight;	/* Normal width and height of table */
} Table;

/*
 * Table flags definitions
 */
#define ARRANGE_PENDING 1	/* A call to ArrangeTable is pending. This
				 * flag allows multiple layout changes to be
				 * requested before the table is actually
				 * reconfigured. */
#define REQUEST_LAYOUT 	2	/* Get the requested sizes of the slave
				 * windows before expanding/shrinking the
				 * size of the master window.  It's
				 * necessary to recompute the layout every
				 * time a partition or cubicle is added,
				 * reconfigured, or deleted, but not when
				 * the master window is resized. */
#define NON_PARENT	4	/* The table is managing slaves which are
				 * not children of the master window. This
				 * requires that they are moved when the
				 * master window is moved (a definite
				 * performance hit). */
/*
 * Forward declarations
 */
static void ArrangeTable _ANSI_ARGS_((ClientData clientData));
static void DestroyTable _ANSI_ARGS_((ClientData clientData));
static void DestroyCubicle _ANSI_ARGS_((Cubicle *cubiPtr));
static void TableEventProc _ANSI_ARGS_((ClientData clientData,
	XEvent *eventPtr));
static void InitPartitions _ANSI_ARGS_((Partition *partPtr, int length));
static void LinkRowEntry _ANSI_ARGS_((Cubicle *cubiPtr));
static void LinkColumnEntry _ANSI_ARGS_((Cubicle *cubiPtr));

extern int strcasecmp _ANSI_ARGS_((CONST char *s1, CONST char *s2));

/*
 *----------------------------------------------------------------------
 *
 * ParseLimits --
 *
 *	Converts the list of elements into zero or more pixel values
 *	which determine the range of pixel values possible.  An element
 *	can be in any form accepted by Tk_GetPixels. The list has a
 *	different meaning based upon the number of elements.
 *
 *	    # of elements:
 *		0 - the limits are reset to the defaults.
 *		1 - the minimum and maximum values are set to this
 *		    value, freezing the range at a single value.
 *		2 - first element is the minimum, the second is the
 *		    maximum.
 *		3 - first element is the minimum, the second is the
 *		    maximum, and the third is the nominal value.
 *
 * Results:
 *	The return value is a standard Tcl result.  The min and
 *	max fields of the range are set.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseLimits(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Window of table */
    char *value;		/* New width list */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of limits */
{
    Limits *limitsPtr = (Limits *)(widgRec + offset);
    int numElem;
    char **elemArr;
    int result = TCL_ERROR;
    int lim[3];
    register int i;
    int size;

    if (value == NULL) {
	limitsPtr->nom = limitsPtr->min = DEF_MIN_LIMIT;
	limitsPtr->max = DEF_MAX_LIMIT;
	return TCL_OK;
    }
    if (Tcl_SplitList(interp, value, &numElem, &elemArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numElem > 3) {
	Tcl_AppendResult(interp, "wrong # limits \"", value, "\"",
	    (char *)NULL);
	goto error;
    }
    for (i = 0; i < numElem; i++) {
	if (((elemArr[i][0] == 'i') || (elemArr[i][0] == 'I')) &&
	    (strcasecmp(elemArr[i], "inf") == 0)) {
	    size = DEF_MAX_LIMIT;
	} else if (Tk_GetPixels(interp, tkwin, elemArr[i], &size) != TCL_OK) {
	    goto error;
	}
	if ((size < DEF_MIN_LIMIT) || (size > DEF_MAX_LIMIT)) {
	    Tcl_AppendResult(interp, "invalid limit \"", value, "\"",
		(char *)NULL);
	    goto error;
	}
	lim[i] = size;
    }
    switch (numElem) {
    case 1:
	limitsPtr->max = limitsPtr->min = lim[0];
	limitsPtr->nom = DEF_MIN_LIMIT;
	break;
    case 2:
	if (lim[1] < lim[0]) {
	    Tcl_AppendResult(interp, "invalid limits \"", value, "\"",
		(char *)NULL);
	    goto error;
	}
	limitsPtr->min = lim[0];
	limitsPtr->max = lim[1];
	limitsPtr->nom = DEF_MIN_LIMIT;
	break;
    case 3:
	if (lim[1] < lim[0]) {
	    Tcl_AppendResult(interp, "invalid range \"", value, "\"",
		(char *)NULL);
	    goto error;
	}
	if ((lim[2] < lim[0]) || (lim[2] > lim[1])) {
	    Tcl_AppendResult(interp, "invalid nominal \"", value, "\"",
		(char *)NULL);
	    goto error;
	}
	limitsPtr->min = lim[0];
	limitsPtr->max = lim[1];
	limitsPtr->nom = lim[2];
	break;
    }
    result = TCL_OK;
  error:
    free(elemArr);
    return result;
}


static void
LimitsToString(min, max, nom, string)
    int min, max, nom;
    char string[];
{
    if (nom > DEF_MIN_LIMIT) {
	sprintf(string, "%d %d %d", min, max, nom);
    } else if (min == max) {
	sprintf(string, "%d", max);
    } else if ((min != DEF_MIN_LIMIT) || (max != DEF_MAX_LIMIT)) {
	sprintf(string, "%d %d", min, max);
    } else {
	string[0] = '\0';
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PrintLimits --
 *
 *	Convert the limits of the pixel values allowed into a list.
 *
 * Results:
 *	The string representation of the limits is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintLimits(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Row/column structure record */
    int offset;			/* Offset of window Partition record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation routine */
{
    Limits *limitsPtr = (Limits *)(widgRec + offset);
    char *result;
    char string[200];

    LimitsToString(limitsPtr->min, limitsPtr->max, limitsPtr->nom, string);
    result = strdup(string);
    *freeProcPtr = TCL_DYNAMIC;
    return (result);
}

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
 *		"none"	 Don't expand the window to fill the cubicle.
 * 		"x"	 Expand only the window's width.
 *		"y"	 Expand only the window's height.
 *		"both"   Expand both the window's height and width.
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
 * ParseResize --
 *
 *	Converts the resize mode into its numeric representation.
 *	Valid mode strings are "none", "expand", "shrink", or "both".
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseResize(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* Resize style string */
    char *widgRec;		/* Cubicle structure record */
    int offset;			/* Offset of style in record */
{
    ResizeFlags *resizePtr = (ResizeFlags *)(widgRec + offset);
    int length;
    char c;

    c = value[0];
    length = strlen(value);
    if ((c == 'n') && (strncmp(value, "none", length) == 0)) {
	*resizePtr = RESIZE_NONE;
    } else if ((c == 'b') && (strncmp(value, "both", length) == 0)) {
	*resizePtr = RESIZE_BOTH;
    } else if ((c == 'e') && (strncmp(value, "expand", length) == 0)) {
	*resizePtr = RESIZE_EXPAND;
    } else if ((c == 's') && (strncmp(value, "shrink", length) == 0)) {
	*resizePtr = RESIZE_SHRINK;
    } else {
	Tcl_AppendResult(interp, "bad resize argument \"", value,
	    "\": should be none, expand, shrink, or both", (char *)NULL);
	return TCL_ERROR;
    }
    return (TCL_OK);
}

/*
 *----------------------------------------------------------------------
 *
 * PrintResize --
 *
 *	Returns resize mode string based upon the resize flags.
 *
 * Results:
 *	The resize mode string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintResize(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Row/column structure record */
    int offset;			/* Offset of resize in Partition record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    ResizeFlags resize = *(ResizeFlags *)(widgRec + offset);

    return (resizeStrings[(int)resize]);
}

/*
 *--------------------------------------------------------------
 *
 * TableEventProc --
 *
 *	This procedure is invoked by the Tk event handler when
 *	the master window is reconfigured or destroyed.
 *
 *	The table will be rearranged at the next idle point if
 *	the master window has been resized or moved. There's a
 *	distinction made between parent and non-parent master
 *	window arrangements.  If the master window is moved and
 *	it's the parent of its slaves, the slaves are moved
 *	automatically.  If it's not the parent, the slaves need
 *	to be moved.  This can be a performance hit in rare cases
 *	where we're scrolling the master window (by moving it)
 *	and there are lots of slave windows.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Arranges for the table associated with tkwin to have its
 *	layout re-computed and drawn at the next idle point.
 *
 *--------------------------------------------------------------
 */
static void
TableEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window */
    XEvent *eventPtr;		/* Information about event */
{
    register Table *tablePtr = (Table *)clientData;

    if (eventPtr->type == ConfigureNotify) {
	if ((Blt_GetListLength(tablePtr->listPtr) > 0) &&
	    !(tablePtr->flags & ARRANGE_PENDING) &&
	    ((tablePtr->width != Tk_Width(tablePtr->tkwin)) ||
		(tablePtr->height != Tk_Height(tablePtr->tkwin))
		|| (tablePtr->flags & NON_PARENT))) {
	    tablePtr->flags |= ARRANGE_PENDING;
	    Tk_DoWhenIdle(ArrangeTable, (ClientData)tablePtr);
	}
    } else if (eventPtr->type == DestroyNotify) {
	if (tablePtr->flags & ARRANGE_PENDING) {
	    Tk_CancelIdleCall(ArrangeTable, (ClientData)tablePtr);
	}
	Tcl_DeleteHashEntry(Tcl_FindHashEntry(&masterWindows,
		(char *)tablePtr->tkwin));
	tablePtr->tkwin = NULL;
	Tk_EventuallyFree((ClientData)tablePtr, DestroyTable);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * SlaveEventProc --
 *
 *	This procedure is invoked by the Tk event handler when
 *	StructureNotify events occur for a slave window.  When a slave
 *	window is destroyed, it frees the corresponding cubicle structure
 *	and arranges for the table layout to be re-computed at the next
 *	idle point.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	If the slave window was deleted, the Cubicle structure gets cleaned
 *	up and the table is rearranged.
 *
 *----------------------------------------------------------------------
 */

static void
SlaveEventProc(clientData, eventPtr)
    ClientData clientData;	/* Pointer to Slave structure for window
				 * referred to by eventPtr. */
    XEvent *eventPtr;		/* Describes what just happened. */
{
    Cubicle *cubiPtr = (Cubicle *)clientData;

    if (eventPtr->type == ConfigureNotify) {
	int extBW;

	extBW = Tk_Changes(cubiPtr->tkwin)->border_width;
	cubiPtr->tablePtr->flags |= REQUEST_LAYOUT;
	if (!(cubiPtr->tablePtr->flags & ARRANGE_PENDING) &&
	    (cubiPtr->extBW != extBW)) {
	    cubiPtr->extBW = extBW;
	    cubiPtr->tablePtr->flags |= ARRANGE_PENDING;
	    Tk_DoWhenIdle(ArrangeTable, (ClientData)cubiPtr->tablePtr);
	}
    } else if (eventPtr->type == DestroyNotify) {
	cubiPtr->tablePtr->flags |= REQUEST_LAYOUT;
	if (!(cubiPtr->tablePtr->flags & ARRANGE_PENDING)) {
	    cubiPtr->tablePtr->flags |= ARRANGE_PENDING;
	    Tk_DoWhenIdle(ArrangeTable, (ClientData)cubiPtr->tablePtr);
	}
	DestroyCubicle(cubiPtr);
    }
}

/*
 *--------------------------------------------------------------
 *
 * SlaveReqProc --
 *
 *	This procedure is invoked by Tk_GeometryRequest for slave
 *	windows managed by the table geometry manager.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Arranges for the table associated with the slave window to
 *	have its layout re-computed and arranged at the next idle
 *	point.
 *
 *--------------------------------------------------------------
 */
/* ARGSUSED */
static void
SlaveReqProc(clientData, tkwin)
    ClientData clientData;	/* Information about window that got new
				 * preferred geometry.  */
    Tk_Window tkwin;		/* Other Tk-related information about the
			         * window. */
{
    Cubicle *cubiPtr = (Cubicle *)clientData;

    cubiPtr->tablePtr->flags |= REQUEST_LAYOUT;
    if (!(cubiPtr->tablePtr->flags & ARRANGE_PENDING)) {
	cubiPtr->tablePtr->flags |= ARRANGE_PENDING;
	Tk_DoWhenIdle(ArrangeTable, (ClientData)cubiPtr->tablePtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * FindCubicle --
 *
 *	Searches for the Cubicle structure corresponding to the given
 *	window.
 *
 * Results:
 *	If a structure associated with the window exists, a pointer to
 *	that structure is returned. Otherwise NULL is returned and if
 *	the TCL_LEAVE_ERR_MSG flag is set, an error message is left in
 *	interp->result.
 *
 *----------------------------------------------------------------------
 */

static Cubicle *
FindCubicle(interp, tkwin, flags)
    Tcl_Interp *interp;
    Tk_Window tkwin;		/* Slave window associated with table entry */
    int flags;
{
    Tcl_HashEntry *entryPtr;

    entryPtr = Tcl_FindHashEntry(&slaveWindows, (char *)tkwin);
    if (entryPtr == NULL) {
	if (flags & TCL_LEAVE_ERR_MSG) {
	    Tcl_AppendResult(interp, "\"", Tk_PathName(tkwin),
		"\" is not managed by any table", (char *)NULL);
	}
	return NULL;
    }
    return ((Cubicle *)Tcl_GetHashValue(entryPtr));
}

/*
 *----------------------------------------------------------------------
 *
 * CreateCubicle --
 *
 *	This procedure creates and initializes a new Cubicle structure
 *	to contain a slave window.  A valid slave window must have a
 *	parent window that is either a) the master window or b) a mutual
 *	ancestor of the master window.
 *
 * Results:
 *	Returns a pointer to the new structure describing the new table
 *	slave window entry.  If an error occurred, then the return
 *	value is NULL and an error message is left in interp->result.
 *
 * Side effects:
 *	Memory is allocated and initialized for the Cubicle structure.
 *
 *----------------------------------------------------------------------
 */
static Cubicle *
CreateCubicle(tablePtr, tkwin)
    Table *tablePtr;
    Tk_Window tkwin;
{
    register Cubicle *cubiPtr;
    int dummy;
    Tk_Window parent, ancestor;
    Tcl_HashEntry *entryPtr;
    int notParent = FALSE;	/* Indicates that the master window is not the
				 * parent of the new slave window. */

    /*
     * A valid slave window has a parent window that either
     *
     *    1) is the master window, or
     * 	  2) is a mutual ancestor of the master window.
     */
    ancestor = Tk_Parent(tkwin);
    for (parent = tablePtr->tkwin; (parent != ancestor) &&
	(!Tk_IsTopLevel(parent)); parent = Tk_Parent(parent)) {
	notParent = TRUE;
    }
    if (ancestor != parent) {
	Tcl_AppendResult(tablePtr->interp, "can't manage \"",
	    Tk_PathName(tkwin), "\" in table \"",
	    Tk_PathName(tablePtr->tkwin), "\"", (char *)NULL);
	return NULL;
    }
    cubiPtr = (Cubicle *)malloc(sizeof(Cubicle));
    if (cubiPtr == NULL) {
	tablePtr->interp->result = "can't allocate cubicle";
	return NULL;
    }
    if (notParent) {
	tablePtr->flags |= NON_PARENT;
    }
    /* Initialize the cubicle structure */

    cubiPtr->x = cubiPtr->y = 0;
    cubiPtr->tkwin = tkwin;
    cubiPtr->tablePtr = tablePtr;
    cubiPtr->extBW = Tk_Changes(tkwin)->border_width;
    cubiPtr->fill = DEF_FILL;
    cubiPtr->ipadX = DEF_IPAD_X;
    cubiPtr->ipadY = DEF_IPAD_Y;
    cubiPtr->padX = DEF_PAD_X;
    cubiPtr->padY = DEF_PAD_Y;
    cubiPtr->anchor = DEF_ANCHOR;
    cubiPtr->rowSpan = DEF_ROW_SPAN;
    cubiPtr->colSpan = DEF_COLUMN_SPAN;
    cubiPtr->reqWidth.min = cubiPtr->reqHeight.min = DEF_MIN_LIMIT;
    cubiPtr->reqWidth.max = cubiPtr->reqHeight.max = DEF_MAX_LIMIT;
    cubiPtr->reqWidth.nom = cubiPtr->reqHeight.nom = DEF_MIN_LIMIT;
    cubiPtr->rowEntryPtr = cubiPtr->colEntryPtr = NULL;
    entryPtr = Tcl_CreateHashEntry(&slaveWindows, (char *)tkwin, &dummy);
    Tcl_SetHashValue(entryPtr, (char *)cubiPtr);
    Tk_CreateEventHandler(tkwin, StructureNotifyMask, SlaveEventProc,
	(ClientData)cubiPtr);
    Tk_ManageGeometry(tkwin, SlaveReqProc, (ClientData)cubiPtr);
    return (cubiPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyCubicle --
 *
 *	Removes the Cubicle structure from the hash table and frees
 *	the memory allocated by it.  If the table is still in use
 *	(i.e. was not called from DestoryTable), remove its entries
 *	from the lists of row and column sorted partitions.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the cubicle is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyCubicle(cubiPtr)
    Cubicle *cubiPtr;
{
    Tcl_HashEntry *entryPtr;

    if (cubiPtr->rowEntryPtr != NULL) {
	Blt_DeleteListEntry(&(cubiPtr->tablePtr->rowSorted),
	    cubiPtr->rowEntryPtr);
    }
    if (cubiPtr->colEntryPtr != NULL) {
	Blt_DeleteListEntry(&(cubiPtr->tablePtr->colSorted),
	    cubiPtr->colEntryPtr);
    }
    Tk_DeleteEventHandler(cubiPtr->tkwin, StructureNotifyMask,
	SlaveEventProc, (ClientData)cubiPtr);
    Tk_ManageGeometry(cubiPtr->tkwin, (Tk_GeometryProc *) NULL,
	(ClientData)cubiPtr);
    entryPtr = Tcl_FindHashEntry(&slaveWindows, (char *)cubiPtr->tkwin);
    Tcl_DeleteHashEntry(entryPtr);
    free((char *)cubiPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureCubicle --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	one or more cubicles associated with a slave window which is
 *	managed by the table geometry manager.
 *
 * Note:
 *	Currently only the one slave window can be queried while many
 *	can be reconfigured at a time.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	The table layout is recomputed and rearranged at the next
 *	idle point.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureCubicle(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Tk_Window searchWin = (Tk_Window)clientData;
    Cubicle *cubiPtr;
    Tk_Window tkwin;
    Table *tablePtr;
    int numSlaves, numOptions;
    int oldRowSpan, oldColSpan;
    register int i;

    /* Find the number of slave windows to be configured */
    numSlaves = 0;
    for (i = 0; i < argc; i++) {
	if (argv[i][0] != '.') {
	    break;
	}
	numSlaves++;
    }
    /* And the number of options to be applied */
    numOptions = argc - numSlaves;

    for (i = 0; i < numSlaves; i++) {
	tkwin = Tk_NameToWindow(interp, argv[i], searchWin);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	cubiPtr = FindCubicle(interp, tkwin, TCL_LEAVE_ERR_MSG);
	if (cubiPtr == NULL) {
	    return TCL_ERROR;
	}
	if (numOptions == 0) {
	    return (Tk_ConfigureInfo(interp, tkwin, cubicleConfigSpecs,
		    (char *)cubiPtr, (char *)NULL, 0));
	} else if (numOptions == 1) {
	    return (Tk_ConfigureInfo(interp, tkwin, cubicleConfigSpecs,
		    (char *)cubiPtr, argv[numSlaves], 0));
	}
	oldRowSpan = cubiPtr->rowSpan;
	oldColSpan = cubiPtr->colSpan;
	if (Tk_ConfigureWidget(interp, tkwin, cubicleConfigSpecs,
		numOptions, argv + numSlaves, (char *)cubiPtr,
		TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((cubiPtr->colSpan < 1) || (cubiPtr->colSpan > USHRT_MAX)) {
	    Tcl_AppendResult(interp, "bad column span specified for \"",
		Tk_PathName(tkwin), "\"", (char *)NULL);
	    return TCL_ERROR;
	}
	if ((cubiPtr->rowSpan < 1) || (cubiPtr->rowSpan > USHRT_MAX)) {
	    Tcl_AppendResult(interp, "bad row span specified for \"",
		Tk_PathName(tkwin), "\"", (char *)NULL);
	    return TCL_ERROR;
	}
	tablePtr = cubiPtr->tablePtr;
	if (oldColSpan != cubiPtr->colSpan) {
	    Blt_UnlinkListEntry(&(tablePtr->colSorted), cubiPtr->colEntryPtr);
	    LinkColumnEntry(cubiPtr);
	}
	if (oldRowSpan != cubiPtr->rowSpan) {
	    Blt_UnlinkListEntry(&(tablePtr->rowSorted), cubiPtr->rowEntryPtr);
	    LinkRowEntry(cubiPtr);
	}
	tablePtr->flags |= REQUEST_LAYOUT;
	if (!(tablePtr->flags & ARRANGE_PENDING)) {
	    tablePtr->flags |= ARRANGE_PENDING;
	    Tk_DoWhenIdle(ArrangeTable, (ClientData)tablePtr);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * FindTable --
 *
 *	Searches for a table associated with the window given by its
 *	pathname.  This window represents the master window of the table.
 *	Errors may occur because 1) pathname does not represent a valid
 *	Tk window or 2) the window is not associated with any table
 *	as its master window.
 *
 * Results:
 *	If a table entry exists, a pointer to the Table structure is
 *	returned. Otherwise NULL is returned and if the TCL_LEAVE_ERR_MSG
 *	flag is set, an error message is left is interp->result.
 *
 *----------------------------------------------------------------------
 */
static Table *
FindTable(interp, pathName, searchWin, flags)
    Tcl_Interp *interp;		/* Interpreter to report errors back to */
    char *pathName;		/* Path name of the master window */
    Tk_Window searchWin;	/* Main window: used to search for window
				 * associated with pathname */
    int flags;			/* If non-zero, don't reset interp->result */
{
    Tcl_HashEntry *entryPtr;
    Tk_Window tkwin;

    tkwin = Tk_NameToWindow(interp, pathName, searchWin);
    if (tkwin == NULL) {
	if (!(flags & TCL_LEAVE_ERR_MSG)) {
	    Tcl_ResetResult(interp);
	}
	return NULL;
    }
    entryPtr = Tcl_FindHashEntry(&masterWindows, (char *)tkwin);
    if (entryPtr == NULL) {
	if (flags & TCL_LEAVE_ERR_MSG) {
	    Tcl_AppendResult(interp, "no table associated with window \"",
		pathName, "\"", (char *)NULL);
	}
	return NULL;
    }
    return ((Table *)Tcl_GetHashValue(entryPtr));
}

/*
 *----------------------------------------------------------------------
 *
 * CreateTable --
 *
 *	This procedure creates and initializes a new Table structure
 *	with tkwin as its master window. The internal structures
 *	associated with the table are initialized.

 * Results:
 *	Returns the pointer to the new Table structure describing the
 *	new table geometry manager.  If an error occurred, the return
 *	value will be NULL and an error message is left in interp->result.
 *
 * Side effects:
 *	Memory is allocated and initialized, an event handler is set up
 *	to watch tkwin, etc.
 *
 *----------------------------------------------------------------------
 */
static Table *
CreateTable(interp, pathName, searchWin)
    Tcl_Interp *interp;		/* Interpreter associated with table */
    char *pathName;		/* Path name of the master window to be
				 * associated with the new table */
    Tk_Window searchWin;	/* Main window */
{
    register Table *tablePtr;
    Tk_Window tkwin;

    tkwin = Tk_NameToWindow(interp, pathName, searchWin);
    if (tkwin == NULL) {
	return NULL;
    }
    tablePtr = (Table *)calloc(1, sizeof(Table));
    if (tablePtr != NULL) {
	int dummy;
	Tcl_HashEntry *entryPtr;

	tablePtr->tkwin = tkwin;
	tablePtr->searchWin = searchWin;
	tablePtr->interp = interp;
	tablePtr->listPtr = &(tablePtr->rowSorted);
	tablePtr->flags = 0;
	tablePtr->rowSize = tablePtr->colSize = DEF_ARRAY_SIZE;
	tablePtr->numRows = tablePtr->numCols = 0;
	tablePtr->rowPtr = tablePtr->rowSpace;
	Blt_InitLinkedList(&(tablePtr->rowSorted), TCL_ONE_WORD_KEYS);
	InitPartitions(tablePtr->rowPtr, DEF_ARRAY_SIZE);

	tablePtr->colPtr = tablePtr->colSpace;
	Blt_InitLinkedList(&(tablePtr->colSorted), TCL_ONE_WORD_KEYS);
	InitPartitions(tablePtr->colPtr, DEF_ARRAY_SIZE);

	Tk_CreateEventHandler(tablePtr->tkwin, StructureNotifyMask,
	    TableEventProc, (ClientData)tablePtr);
	entryPtr = Tcl_CreateHashEntry(&masterWindows, (char *)tkwin, &dummy);
	Tcl_SetHashValue(entryPtr, (ClientData)tablePtr);
    } else {
	Tcl_AppendResult(interp, "can't create table \"", pathName, "\"",
	    (char *)NULL);
    }
    return (tablePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyTable --
 *
 *	This procedure is invoked by Tk_EventuallyFree or Tk_Release
 *	to clean up the Table structure at a safe time (when no-one is
 *	using it anymore).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the table geometry manager is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyTable(clientData)
    ClientData clientData;	/* Table structure */
{
    register Table *tablePtr = (Table *)clientData;
    Blt_ListEntry *entryPtr;
    Cubicle *cubiPtr;

    for (entryPtr = Blt_FirstListEntry(tablePtr->listPtr);
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	cubiPtr = (Cubicle *)Blt_GetListValue(entryPtr);
	cubiPtr->rowEntryPtr = cubiPtr->colEntryPtr = NULL;
	DestroyCubicle(cubiPtr);
    }
    Blt_ClearList(&(tablePtr->rowSorted));
    Blt_ClearList(&(tablePtr->colSorted));

    if ((tablePtr->rowPtr != NULL) &&
	(tablePtr->rowPtr != tablePtr->rowSpace)) {
	free((char *)tablePtr->rowPtr);
    }
    if ((tablePtr->colPtr != NULL) &&
	(tablePtr->colPtr != tablePtr->colSpace)) {
	free((char *)tablePtr->colPtr);
    }
    free((char *)tablePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * InitPartitions --
 *
 *	Initializes the values of the newly created elements in
 *	the partition array.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The elements of the array of Partition structures is
 *	initialized.
 *
 *----------------------------------------------------------------------
 */
static void
InitPartitions(partPtr, length)
    register Partition *partPtr;/* Array of partitions to be initialized */
    int length;			/* Number of elements in array */
{
    register int i;

    for (i = 0; i < length; i++) {
	partPtr->resize = RESIZE_BOTH;
	partPtr->reqSize.nom = partPtr->reqSize.min =
	    partPtr->size = DEF_MIN_LIMIT;
	partPtr->reqSize.max = DEF_MAX_LIMIT;
	partPtr->nomSize = 0;
	partPtr->pad = 0;
	partPtr->span = 0;
	partPtr++;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ExtendArray --
 *
 *	Resizes the partition array to a larger size.
 *
 * Results:
 *	A pointer to the newly extended array is returned.
 *
 *----------------------------------------------------------------------
 */
static Partition *
ExtendArray(partArr, oldSize, newSize)
    Partition *partArr;		/*  */
    int oldSize, newSize;
{
    Partition *newArr;

    newArr = (Partition *)malloc(newSize * sizeof(Partition));
    if (newArr != NULL) {
	if (oldSize > 0) {
	    memcpy((char *)newArr, (char *)partArr,
		oldSize * sizeof(Partition));
	}
	InitPartitions(&(newArr[oldSize]), (int)(newSize - oldSize));
    }
    return (newArr);
}

/*
 *----------------------------------------------------------------------
 *
 * AssertColumn --
 *
 *	Checks the size of the column partitions and extends the
 *	size if a larger array is needed.
 *
 * Results:
 *	Returns 1 if the column exists.  Otherwise 0 is returned and
 *	interp->result contains an error message.
 *
 * Side effects:
 *	The size of the column partition array may be extended and
 *	initialized.
 *
 *----------------------------------------------------------------------
 */
static int
AssertColumn(tablePtr, column)
    Table *tablePtr;
    int column;
{
    if (column >= tablePtr->colSize) {
	int newSize;
	Partition *newArr;

	if (column >= USHRT_MAX) {
	    Tcl_AppendResult(tablePtr->interp, "too many columns in \"",
		Tk_PathName(tablePtr->tkwin), "\"", (char *)NULL);
	    return 0;
	}
	newSize = 2 * tablePtr->colSize;
	while (newSize <= column) {
	    newSize += newSize;
	}
	newArr = ExtendArray(tablePtr->colPtr, tablePtr->colSize, newSize);
	if (newArr == NULL) {
	    Tcl_AppendResult(tablePtr->interp, "can't extend columns in table",
		" \"", Tk_PathName(tablePtr->tkwin), "\": ",
		Tcl_PosixError(tablePtr->interp));
	    return 0;
	}
	if (tablePtr->colPtr != tablePtr->colSpace) {
	    free((char *)tablePtr->colPtr);
	}
	tablePtr->colPtr = newArr;
	tablePtr->colSize = newSize;
    }
    if (column >= tablePtr->numCols) {
	tablePtr->numCols = column + 1;
    }
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * AssertRow --
 *
 *	Checks the size of the row partitions and extends the
 *	size if a larger array is needed.
 *
 * Results:
 *	Returns 1 if the row exists.  Otherwise 0 is returned
 *	and interp->result contains an error message.
 *
 * Side effects:
 *	The size of the row partition array may be extended and
 *	initialized.
 *
 *----------------------------------------------------------------------
 */
static int
AssertRow(tablePtr, row)
    Table *tablePtr;
    int row;
{
    if (row >= tablePtr->rowSize) {
	register int newSize;
	Partition *newArr;

	if (row >= USHRT_MAX) {
	    Tcl_AppendResult(tablePtr->interp, "too many rows in \"",
		Tk_PathName(tablePtr->tkwin), "\"", (char *)NULL);
	    return 0;
	}
	newSize = 2 * tablePtr->rowSize;
	while (newSize <= row) {
	    newSize += newSize;
	}
	newArr = ExtendArray(tablePtr->rowPtr, tablePtr->rowSize, newSize);
	if (newArr == NULL) {
	    Tcl_AppendResult(tablePtr->interp, "can't extend rows in table \"",
		Tk_PathName(tablePtr->tkwin), "\": ",
		Tcl_PosixError(tablePtr->interp));
	    return 0;
	}
	if (tablePtr->rowPtr != tablePtr->rowSpace) {
	    free((char *)tablePtr->rowPtr);
	}
	tablePtr->rowPtr = newArr;
	tablePtr->rowSize = newSize;
    }
    if (row >= tablePtr->numRows) {
	tablePtr->numRows = row + 1;
    }
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * ParseIndex --
 *
 *	Parse the entry index string and return the row and column
 *	numbers in their respective parameters.  The format of a
 *	table entry index is <row>,<column> where <row> is the row
 *	number and <column> is the column number.  Rows and columns
 *	are numbered starting from zero.
 *
 * Results:
 *	Returns a standard Tcl result.  If TCL_OK is returned, the
 *	row and column numbers are returned via rowPtr and columnPtr
 *	respectively.
 *
 *----------------------------------------------------------------------
 */
static int
ParseIndex(interp, indexStr, rowPtr, columnPtr)
    Tcl_Interp *interp;
    char *indexStr;
    int *rowPtr;
    int *columnPtr;
{
    char *columnStr, *rowStr;
    long row, column;

    rowStr = indexStr;
    columnStr = strchr(indexStr, ',');
    if (columnStr == NULL) {
	Tcl_AppendResult(interp, "invalid index \"", indexStr,
	    "\": should be \"row,column\"", (char *)NULL);
	return TCL_ERROR;

    }
    *columnStr++ = '\0';
    if ((Tcl_ExprLong(interp, rowStr, &row) != TCL_OK) ||
	(Tcl_ExprLong(interp, columnStr, &column) != TCL_OK)) {
	return TCL_ERROR;
    }
    if ((row < 0) || (row > USHRT_MAX)) {
	Tcl_AppendResult(interp, "row index \"", rowStr,
	    "\" is out of range", (char *)NULL);
	return TCL_ERROR;
    }
    if ((column < 0) || (column > USHRT_MAX)) {
	Tcl_AppendResult(interp, "column index \"", columnStr,
	    "\" is out of range", (char *)NULL);
	return TCL_ERROR;
    }
    *rowPtr = (int)row;
    *columnPtr = (int)column;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MakeSlaveKey --
 *
 *	Creates a one word key out of the two 16 bit row and column
 *	indices.
 *
 *----------------------------------------------------------------------
 */
static unsigned int
MakeSlaveKey(row, column)
    int row;
    int column;
{
#ifndef cray
    SlaveKey key;

    key.position.row = (unsigned short)row;
    key.position.column = (unsigned short)column;
    return (key.index);
#else
    unsigned int index;

    index = (row & 0xffffffff);
    index |= ((column & 0xffffffff) << 32);
    return (index);
#endif /*cray*/
}

/*
 *----------------------------------------------------------------------
 *
 * LinkRowEntry --
 *
 *	Links new list entry into list of row-sorted cubicles.
 *	It's important to maintain this list, because the size of
 *	the row parititions is determined in order of this list.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Entry is linked into the list of row-sorted cubicles.
 *
 *----------------------------------------------------------------------
 */
static void
LinkRowEntry(newPtr)
    Cubicle *newPtr;
{
    register int delta;
    Cubicle *cubiPtr;
    Table *tablePtr;
    register Blt_ListEntry *entryPtr;

    tablePtr = newPtr->tablePtr;
    for (entryPtr = Blt_FirstListEntry(&(tablePtr->rowSorted));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	cubiPtr = (Cubicle *)Blt_GetListValue(entryPtr);
	delta = newPtr->rowSpan - cubiPtr->rowSpan;
	if (delta < 0) {
	    break;
	} else if (delta == 0) {
	    delta = newPtr->rowIndex - cubiPtr->rowIndex;
	    if (delta > 0) {
		break;
	    } else if (delta == 0) {
		delta = newPtr->colIndex - cubiPtr->colIndex;
		if (delta > 0) {
		    break;
		}
	    }
	}
    }
    if (entryPtr == NULL) {
	Blt_LinkListAfter(&(tablePtr->rowSorted), newPtr->rowEntryPtr,
	    entryPtr);
    } else {
	Blt_LinkListBefore(&(tablePtr->rowSorted), newPtr->rowEntryPtr,
	    entryPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * LinkColumnEntry --
 *
 *	Links new list entry into list of column-sorted cubicles.
 *	It's important to maintain this list, because the size of
 *	the column parititions is determined in order of this list.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Entry is linked into the list of column-sorted cubicles.
 *
 *----------------------------------------------------------------------
 */
static void
LinkColumnEntry(newPtr)
    Cubicle *newPtr;
{
    register int delta;
    Cubicle *cubiPtr;
    Table *tablePtr;
    register Blt_ListEntry *entryPtr;

    tablePtr = newPtr->tablePtr;
    for (entryPtr = Blt_FirstListEntry(&(tablePtr->colSorted));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	cubiPtr = (Cubicle *)Blt_GetListValue(entryPtr);
	delta = newPtr->colSpan - cubiPtr->colSpan;
	if (delta < 0) {
	    break;
	} else if (delta == 0) {
	    delta = newPtr->colIndex - cubiPtr->colIndex;
	    if (delta > 0) {
		break;
	    } else if (delta == 0) {
		delta = newPtr->rowIndex - cubiPtr->rowIndex;
		if (delta > 0) {
		    break;
		}
	    }
	}
    }
    if (entryPtr == NULL) {
	Blt_LinkListAfter(&(tablePtr->colSorted), newPtr->colEntryPtr,
	    entryPtr);
    } else {
	Blt_LinkListBefore(&(tablePtr->colSorted), newPtr->colEntryPtr,
	    entryPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * AddWindowToTable --
 *
 *	Adds the given window as a slave window into the table at
 *	a given row and column position.  The window may already
 *	exist as a slave window in the table. If tkwin is a slave
 *	of another table, it's an error.
 *
 *	The new window is inserted into both the slave window hash
 *	table (this is used to locate the information associated with
 *	the slave window without searching each table) and cubicle
 *	lists which are sorted in order of column and row spans.
 *
 * Results:
 *	Returns a standard Tcl result.  If an error occurred, TCL_ERROR
 *	is returned and an error message is left in interp->result.
 *
 * Side Effects:
 *	The table is re-computed and arranged at the next idle point.
 *
 *----------------------------------------------------------------------
 */
static int
AddWindowToTable(tablePtr, tkwin, row, column, argc, argv)
    Table *tablePtr;
    Tk_Window tkwin;
    int row, column;
    int argc;
    char **argv;
{
    register Cubicle *cubiPtr;
    Blt_ListEntry *entryPtr;
    int result = TCL_OK;
    unsigned int key;

    cubiPtr = FindCubicle(tablePtr->interp, tkwin, 0);
    if (cubiPtr != NULL) {
	/*
	 * Make sure the window is currently being managed by this table.
	 */
	if (cubiPtr->tablePtr != tablePtr) {
	    Tcl_AppendResult(tablePtr->interp, "\"", Tk_PathName(tkwin),
		"\" is already managed by \"", Tk_PathName(cubiPtr->tkwin),
		"\"", (char *)NULL);
	    return TCL_ERROR;
	}
	/*
	 * Remove the cubicle from both row and column lists.  It will
	 * be re-inserted into the table at the new position
	 */
	Blt_DeleteListEntry(&(tablePtr->rowSorted), cubiPtr->rowEntryPtr);
	Blt_DeleteListEntry(&(tablePtr->colSorted), cubiPtr->colEntryPtr);
    } else {
	cubiPtr = CreateCubicle(tablePtr, tkwin);
	if (cubiPtr == NULL) {
	    return TCL_ERROR;
	}
    }
    /*
     * If there's already a slave window at this position in the
     * table, unmap it and remove the cubicle.
     */
    key = MakeSlaveKey(row, column);
    entryPtr = Blt_FindListEntry(tablePtr->listPtr, (char *)key);
    if (entryPtr != NULL) {
	Cubicle *oldPtr;

	oldPtr = (Cubicle *)Blt_GetListValue(entryPtr);
	if (Tk_IsMapped(oldPtr->tkwin)) {
	    Tk_UnmapWindow(oldPtr->tkwin);
	}
	DestroyCubicle(oldPtr);
    }
    cubiPtr->colIndex = column;
    cubiPtr->rowIndex = row;
    if (!AssertRow(tablePtr, cubiPtr->rowIndex) ||
	!AssertColumn(tablePtr, cubiPtr->colIndex)) {
	return TCL_ERROR;
    }
    if (argc > 0) {
	result = Tk_ConfigureWidget(tablePtr->interp, cubiPtr->tkwin,
	    cubicleConfigSpecs, argc, argv, (char *)cubiPtr,
	    TK_CONFIG_ARGV_ONLY);
    }
    if ((cubiPtr->colSpan < 1) || (cubiPtr->rowSpan < 1)) {
	Tcl_AppendResult(tablePtr->interp, "invalid spans specified for \"",
	    Tk_PathName(tkwin), "\"", (char *)NULL);
	DestroyCubicle(cubiPtr);
	return TCL_ERROR;
    }
    /*
     * Insert the cubicle into both the row and column layout lists
     */
    cubiPtr->rowEntryPtr = Blt_CreateListEntry((char *)key);
    Blt_SetListValue(cubiPtr->rowEntryPtr, cubiPtr);
    LinkRowEntry(cubiPtr);

    cubiPtr->colEntryPtr = Blt_CreateListEntry((char *)key);
    Blt_SetListValue(cubiPtr->colEntryPtr, cubiPtr);
    LinkColumnEntry(cubiPtr);

    if (!AssertColumn(tablePtr, cubiPtr->colIndex + cubiPtr->colSpan - 1) ||
	!AssertRow(tablePtr, cubiPtr->rowIndex + cubiPtr->rowSpan - 1)) {
	return TCL_ERROR;
    }
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * ManageWindows --
 *
 *	Processes an argv/argc list of table entries to add and
 *	configure new slave windows into the table.  A table entry
 *	consists of the window path name, table index, and optional
 *	configuration options.  The first argument in the argv list
 *	is the name of the table.  If no table exists for the given
 *	window, a new one is created.
 *
 * Results:
 *	Returns a standard Tcl result.  If an error occurred, TCL_ERROR
 *	is returned and an error message is left in interp->result.
 *
 * Side Effects:
 *	Memory is allocated, a new master table is possibly created, etc.
 *	The table is re-computed and arranged at the next idle point.
 *
 *----------------------------------------------------------------------
 */
static int
ManageWindows(tablePtr, interp, argc, argv)
    Table *tablePtr;		/* Table to manage new slave windows */
    Tcl_Interp *interp;		/* Interpreter to report errors back to */
    int argc;			/*  */
    char **argv;		/* List of slave windows, indices, and
				 * options */
{
    char *savePtr;
    int row, column;
    register int i, count;
    Tk_Window tkwin;

    for (i = 0; i < argc; /*empty*/ ) {
	tkwin = Tk_NameToWindow(interp, argv[i], tablePtr->tkwin);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	if ((i + 1) == argc) {
	    Tcl_AppendResult(interp, "missing index argument for \"", argv[i],
		"\"", (char *)NULL);
	    return TCL_ERROR;
	}
	if (ParseIndex(interp, argv[i + 1], &row, &column) != TCL_OK) {
	    return TCL_ERROR;	/* Invalid row,column index */
	}
	/*
	 * Find the end this entry's option-value pairs, first
	 * skipping over the slave window's pathname and table index
	 * arguments.
	 */
	i += 2;
	for (count = i; count < argc; count += 2) {
	    if (argv[count][0] != '-') {
		break;
	    }
	}
	savePtr = argv[count];
	argv[count] = NULL;
	if (AddWindowToTable(tablePtr, tkwin, row, column, count - i,
		argv + i) != TCL_OK) {
	    return TCL_ERROR;
	}
	argv[count] = savePtr;
	i = count;
    }
    /* If all went well, arrange for the table layout to be performed. */
    tablePtr->flags |= REQUEST_LAYOUT;
    if (!(tablePtr->flags & ARRANGE_PENDING)) {
	tablePtr->flags |= ARRANGE_PENDING;
	Tk_DoWhenIdle(ArrangeTable, (ClientData)tablePtr);
    }
    interp->result = Tk_PathName(tablePtr->tkwin);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SlaveNames --
 *
 *	Returns a list of all the pathnames of the slaves window
 *	managed by a table geometry manager.  The table is given
 *	by the path name of a master window associated with the table.
 *	pathnames of all slave windows managed by this table.
 *
 * Results:
 *	Returns a standard Tcl result.  If no error occurred, TCL_OK
 *	is returned and a list of slave window path names is left in
 *	interp->result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
SlaveNames(tablePtr, interp, argc, argv)
    Table *tablePtr;
    Tcl_Interp *interp;		/* Interpreter to return list of names to */
    int argc;			/* Number of arguments */
    char **argv;		/* Contains 1-2 arguments: pathname of master
				 * window associated with the table and search
				 * pattern */
{
    Blt_ListEntry *entryPtr;
    Cubicle *cubiPtr;

    for (entryPtr = Blt_FirstListEntry(tablePtr->listPtr);
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	cubiPtr = (Cubicle *)Blt_GetListValue(entryPtr);
	if ((argc != 2) ||
	    (Tcl_StringMatch(Tk_PathName(cubiPtr->tkwin), argv[1]))) {
	    Tcl_AppendElement(interp, Tk_PathName(cubiPtr->tkwin));
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MasterNames --
 *
 *	Returns a list of all the pathnames of the master windows
 *	managed by a table geometry manager matching a given pattern.
 *	If no pattern is present (argc == 0), all pathnames are returned.
 *
 * Results:
 *	Returns a standard Tcl result.  If no error occurred, TCL_OK
 *	is returned and a list of slave window path names is left in
 *	interp->result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
MasterNames(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of the interpreter: Used to
				 * search for windows in the hierarchy */
    Tcl_Interp *interp;		/* Interpreter to return list of names to */
    int argc;
    char **argv;		/* Contains 0-1 arguments: search pattern */
{
    Tk_Window searchWin = (Tk_Window)clientData;
    Tcl_HashEntry *entryPtr;
    Tcl_HashSearch cursor;
    register Table *tablePtr;

    for (entryPtr = Tcl_FirstHashEntry(&masterWindows, &cursor);
	entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&cursor)) {
	tablePtr = (Table *)Tcl_GetHashValue(entryPtr);
	if ((tablePtr->searchWin == searchWin) && ((argc != 1) ||
		(Tcl_StringMatch(Tk_PathName(tablePtr->tkwin), argv[0])))) {
	    Tcl_AppendElement(interp, Tk_PathName(tablePtr->tkwin));
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PartitionInfo --
 *
 *
 *	Returns a list of row and column partition information.
 *	The information consists of the minimum size, current size,
 *	maximum size, and resize flag option.  The format should
 *	be suitable to pass back to the "configure" option.
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
PartitionInfo(tablePtr, interp, type, argc, argv)
    Table *tablePtr;
    Tcl_Interp *interp;
    PartitionTypes type;
    int argc;
    char **argv;
{
    long index;
    Partition *partPtr;
    char **indexArr;
    char buf[BUFSIZ];
    char string[200];
    char *format;
    int numPartitions, maxIndex;
    int queryAll;
    int result = TCL_ERROR;
    register int i;

    if (Tcl_SplitList(interp, argv[0], &numPartitions, &indexArr) != TCL_OK) {
	return TCL_ERROR;	/* Can't split list */
    }
    maxIndex = 0;		/* Inhibit compiler warnings */
    partPtr = NULL;
    format = NULL;
    if ((numPartitions == 1) &&
	(indexArr[0][0] == 'a') && (strcmp(indexArr[0], "all") == 0)) {
	numPartitions = NUMENTRIES(tablePtr, type);
	queryAll = TRUE;
    } else {
	maxIndex = NUMENTRIES(tablePtr, type);
	queryAll = FALSE;
    }
    for (i = 0; i < numPartitions; i++) {
	if (queryAll) {
	    index = i;
	} else {
	    if (Tcl_ExprLong(interp, indexArr[i], &index) != TCL_OK) {
		goto error;
	    }
	    if ((index < 0) || (index >= maxIndex)) {
		Tcl_AppendResult(interp, "index \"", indexArr[i],
		    "\" is out of range", (char *)NULL);
		goto error;
	    }
	}
	if (type == ROW_PARTITION_TYPE) {
	    partPtr = tablePtr->rowPtr + index;
	    format = "%d -resize %s -height {%s} -pady %d";
	} else if (type == COLUMN_PARTITION_TYPE) {
	    partPtr = tablePtr->colPtr + index;
	    format = "%d -resize %s -width {%s} -padx %d";
	}
	LimitsToString(partPtr->reqSize.min, partPtr->reqSize.max,
	    partPtr->reqSize.nom, string);
	sprintf(buf, format, index, resizeStrings[(int)partPtr->resize],
	    string, partPtr->pad);
	Tcl_AppendElement(tablePtr->interp, buf);
    }
    result = TCL_OK;
  error:
    free((char *)indexArr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * PartitionSizes --
 *
 *
 *	Returns the sizes of the named partitions (rows or columns)
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
PartitionSizes(tablePtr, interp, type, indexStr)
    Table *tablePtr;
    Tcl_Interp *interp;
    PartitionTypes type;
    char *indexStr;
{
    long index;
    Partition *partPtr;
    char **indexArr;
    char string[200];
    int numPartitions, maxIndex;
    int queryAll;
    int result = TCL_ERROR;
    register int i;

    if (Tcl_SplitList(interp, indexStr, &numPartitions, &indexArr) != TCL_OK) {
	return TCL_ERROR;	/* Can't split list */
    }
    maxIndex = 0;		/* Suppress compiler warning */
    if ((numPartitions == 1) &&
	(indexArr[0][0] == 'a') && (strcmp(indexArr[0], "all") == 0)) {
	numPartitions = NUMENTRIES(tablePtr, type);
	queryAll = TRUE;
    } else {
	maxIndex = NUMENTRIES(tablePtr, type);
	queryAll = FALSE;
    }
    partPtr = ((type == ROW_PARTITION_TYPE)
	? tablePtr->rowPtr : tablePtr->colPtr);
    for (i = 0; i < numPartitions; i++) {
	if (queryAll) {
	    index = i;
	} else {
	    if (Tcl_ExprLong(interp, indexArr[i], &index) != TCL_OK) {
		goto error;
	    }
	    if ((index < 0) || (index >= maxIndex)) {
		Tcl_AppendResult(interp, "index \"", indexArr[i],
		    "\" is out of range", (char *)NULL);
		goto error;
	    }
	}
	sprintf(string, "%d", partPtr[index].size);
	Tcl_AppendElement(tablePtr->interp, string);
    }
    result = TCL_OK;
  error:
    free((char *)indexArr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutTable --
 *
 *	Forces layout of the table geometry manager.  This is useful
 *	to get the geometry manager to calculate the normal width and
 *	height of each row and column.  Otherwise, one needs to
 *	withdraw the master window, run "update", and then query to
 *	geometry manager.
 *
 * Results:
 *	Returns a standard Tcl result.  If no error occurred, TCL_OK
 *	is returned. Otherwise, TCL_ERROR and a error message is left in
 *	interp->result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
LayoutTable(clientData, interp, name)
    ClientData clientData;	/* Main window of the interpreter: Used to
				 * search for windows in the hierarchy */
    Tcl_Interp *interp;		/* Interpreter to report errors to */
    char *name;			/* Path name of master window associated with
				 * the table */
{
    Tk_Window searchWin = (Tk_Window)clientData;
    Table *tablePtr;

    tablePtr = FindTable(interp, name, searchWin, TCL_LEAVE_ERR_MSG);
    if (tablePtr == NULL) {
	return TCL_ERROR;
    }
    tablePtr->flags |= REQUEST_LAYOUT;
    if (!(tablePtr->flags & ARRANGE_PENDING)) {
	tablePtr->flags |= ARRANGE_PENDING;
	ArrangeTable((ClientData)tablePtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SlaveInfo --
 *
 *	Returns the name, position and options of a slave in the table.
 *
 * Results:
 *	Returns a standard Tcl result.  A list of the slave window
 *	attributes is left in interp->result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
SlaveInfo(clientData, interp, name)
    ClientData clientData;
    Tcl_Interp *interp;
    char *name;
{
    Tk_Window searchWin = (Tk_Window)clientData;
    Tk_Window tkwin;
    Cubicle *cubiPtr;
    char string[200];

    tkwin = Tk_NameToWindow(interp, name, searchWin);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    cubiPtr = FindCubicle(interp, tkwin, TCL_LEAVE_ERR_MSG);
    if (cubiPtr == NULL) {
	return TCL_ERROR;
    }
    sprintf(string, " %s %d,%d", Tk_PathName(cubiPtr->tkwin),
	cubiPtr->rowIndex, cubiPtr->colIndex);
    Tcl_AppendResult(interp, string, (char *)NULL);
    sprintf(string, " -ipadx %d -ipady %d -padx %d -pady %d", cubiPtr->ipadX,
	cubiPtr->ipadY, cubiPtr->padX, cubiPtr->padY);
    Tcl_AppendResult(interp, string, (char *)NULL);
    sprintf(string, " -rowspan %d -columnspan %d", cubiPtr->rowSpan,
	cubiPtr->colSpan);
    Tcl_AppendResult(interp, string, (char *)NULL);
    Tcl_AppendResult(interp, " -anchor ", Tk_NameOfAnchor(cubiPtr->anchor),
	" -fill ", fillStrings[(int)cubiPtr->fill], (char *)NULL);
    LimitsToString(cubiPtr->reqWidth.min, cubiPtr->reqWidth.max,
	cubiPtr->reqWidth.nom, string);
    Tcl_AppendResult(interp, " -reqwidth {", string, "}", (char *)NULL);
    LimitsToString(cubiPtr->reqHeight.min, cubiPtr->reqHeight.max,
	cubiPtr->reqHeight.nom, string);
    Tcl_AppendResult(interp, " -reqheight {", string, "}", (char *)NULL);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigurePartition --
 *
 *	This procedure is called to process an argv/argc list in order
 *	to configure a row or column in the table geometry manager.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Partition configuration options (bounds, resize flags, etc)
 *	get set.  New partitions may be created as necessary. The table
 *	is recalculated and arranged at the next idle point.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigurePartition(tablePtr, interp, type, argc, argv)
    Table *tablePtr;
    Tcl_Interp *interp;
    PartitionTypes type;
    int argc;
    char **argv;
{
    int index;
    Partition *partPtr;
    Tk_ConfigSpec *configSpecsPtr;
    char **indexArr;
    int numPartitions;
    int configureAll;
    int result = TCL_ERROR;
    register int i;

    if (Tcl_SplitList(interp, argv[0], &numPartitions, &indexArr) != TCL_OK) {
	return TCL_ERROR;	/* Can't split list */
    }
    configSpecsPtr = partConfigSpecs[(int)type];
    configureAll = FALSE;

    partPtr = NULL;
    if ((numPartitions == 1) &&
	(indexArr[0][0] == 'a') && (strcmp(indexArr[0], "all") == 0)) {
	numPartitions = NUMENTRIES(tablePtr, type);
	configureAll = TRUE;
    }
    for (i = 0; i < numPartitions; i++) {
	if (configureAll) {
	    index = i;
	} else {
	    long int value;

	    if (Tcl_ExprLong(interp, indexArr[i], &value) != TCL_OK) {
		goto error;
	    }
	    index = (int)value;
	    if ((index < 0) || (index > USHRT_MAX)) {
		Tcl_AppendResult(interp, "index \"", indexArr[i],
		    "\" is out of range", (char *)NULL);
		goto error;
	    }
	}
	if (type == ROW_PARTITION_TYPE) {
	    if (!AssertRow(tablePtr, index)) {
		goto error;
	    }
	    partPtr = tablePtr->rowPtr + index;
	} else if (type == COLUMN_PARTITION_TYPE) {
	    if (!AssertColumn(tablePtr, index)) {
		goto error;
	    }
	    partPtr = tablePtr->colPtr + index;
	}
	if (argc == 1) {
	    free((char *)indexArr);
	    return (Tk_ConfigureInfo(interp, tablePtr->tkwin, configSpecsPtr,
		    (char *)partPtr, (char *)NULL, 0));
	} else if (argc == 2) {
	    free((char *)indexArr);
	    return (Tk_ConfigureInfo(interp, tablePtr->tkwin, configSpecsPtr,
		    (char *)partPtr, argv[1], 0));
	}
	if (Tk_ConfigureWidget(interp, tablePtr->tkwin, configSpecsPtr,
		argc - 1, argv + 1, (char *)partPtr,
		TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	    goto error;
	}
    }
    tablePtr->flags |= REQUEST_LAYOUT;
    if (!(tablePtr->flags & ARRANGE_PENDING)) {
	tablePtr->flags |= ARRANGE_PENDING;
	Tk_DoWhenIdle(ArrangeTable, (ClientData)tablePtr);
    }
    result = TCL_OK;
  error:
    free((char *)indexArr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ForgetWindow --
 *
 *	Processes an argv/argc list of slave window names and  purges
 *	their entries from their respective tables.  The windows are
 *	unmapped and the tables are rearranged at the next idle point.
 *	Note that all the named slave windows do not need to exist in
 *	the same table.
 *
 * Results:
 *	Returns a standard Tcl result.  If an error occurred, TCL_ERROR
 *	is returned and an error message is left in interp->result.
 *
 * Side Effects:
 *	Memory is deallocated (the cubicle is destroyed), etc.
 *	The affected tables are is re-computed and arranged at the next
 *	idle point.
 *
 *----------------------------------------------------------------------
 */

static int
ForgetWindow(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Cubicle *cubiPtr;
    register int i;
    Tk_Window tkwin;

    for (i = 0; i < argc; i++) {
	tkwin = Tk_NameToWindow(interp, argv[i], (Tk_Window)clientData);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	cubiPtr = FindCubicle(interp, tkwin, TCL_LEAVE_ERR_MSG);
	if (cubiPtr == NULL) {
	    return TCL_ERROR;
	}
	if (Tk_IsMapped(cubiPtr->tkwin)) {
	    Tk_UnmapWindow(cubiPtr->tkwin);
	}
	/*
	 * Arrange for the call back here because not all the named
	 * slave windows may belong to the same table.
	 */
	cubiPtr->tablePtr->flags |= REQUEST_LAYOUT;
	if (!(cubiPtr->tablePtr->flags & ARRANGE_PENDING)) {
	    cubiPtr->tablePtr->flags |= ARRANGE_PENDING;
	    Tk_DoWhenIdle(ArrangeTable, (ClientData)cubiPtr->tablePtr);
	}
	DestroyCubicle(cubiPtr);
    }
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
    XPoint newPt;

    newPt.x = newPt.y = 0;
    switch (anchor) {
    case TK_ANCHOR_NW:		/* Upper left corner */
	break;
    case TK_ANCHOR_W:		/* Left center */
	newPt.y = (deltaY / 2);
	break;
    case TK_ANCHOR_SW:		/* Lower left corner */
	newPt.y = deltaY;
	break;
    case TK_ANCHOR_N:		/* Top center */
	newPt.x = (deltaX / 2);
	break;
    case TK_ANCHOR_CENTER:	/* Centered */
	newPt.x = (deltaX / 2);
	newPt.y = (deltaY / 2);
	break;
    case TK_ANCHOR_S:		/* Bottom center */
	newPt.x = (deltaX / 2);
	newPt.y = deltaY;
	break;
    case TK_ANCHOR_NE:		/* Upper right corner */
	newPt.x = deltaX;
	break;
    case TK_ANCHOR_E:		/* Right center */
	newPt.x = deltaX;
	newPt.y = (deltaY / 2);
	break;
    case TK_ANCHOR_SE:		/* Lower right corner */
	newPt.x = deltaX;
	newPt.y = deltaY;
	break;
    }
    return (newPt);
}

/*
 *----------------------------------------------------------------------
 *
 * GetReqWidth --
 *
 *	Returns the width requested by the slave window starting in
 *	the given cubicle.  The requested space also includes any
 *	internal padding which has been designated for this window.
 *
 *	The requested width of the window is always bounded by
 *	the limits set in cubiPtr->reqWidth.
 *
 * Results:
 *	Returns the requested width of the slave window.
 *
 *----------------------------------------------------------------------
 */
static int
GetReqWidth(cubiPtr)
    Cubicle *cubiPtr;
{
    register int width;

    if (cubiPtr->reqWidth.nom > 0) {
	width = cubiPtr->reqWidth.nom;
    } else {
	width = Tk_ReqWidth(cubiPtr->tkwin) + (2 * cubiPtr->ipadX);
    }
    if (width < cubiPtr->reqWidth.min) {
	width = cubiPtr->reqWidth.min;
    } else if (width > cubiPtr->reqWidth.max) {
	width = cubiPtr->reqWidth.max;
    }
    return (width);
}

/*
 *----------------------------------------------------------------------
 *
 * GetReqHeight --
 *
 *	Returns the height requested by the slave window starting in
 *	the given cubicle.  The requested space also includes any
 *	internal padding which has been designated for this window.
 *
 *	The requested height of the window is always bounded by
 *	the limits set in cubiPtr->reqHeight.
 *
 * Results:
 *	Returns the requested height of the slave window.
 *
 *----------------------------------------------------------------------
 */
static int
GetReqHeight(cubiPtr)
    Cubicle *cubiPtr;
{
    register int height;

    if (cubiPtr->reqHeight.nom > 0) {
	height = cubiPtr->reqHeight.nom;
    } else {
	height = Tk_ReqHeight(cubiPtr->tkwin) + (2 * cubiPtr->ipadY);
    }
    if (height < cubiPtr->reqHeight.min) {
	height = cubiPtr->reqHeight.min;
    } else if (height > cubiPtr->reqHeight.max) {
	height = cubiPtr->reqHeight.max;
    }
    return (height);
}

/*
 *----------------------------------------------------------------------
 *
 * GetSpan --
 *
 *	Calculates the distance of the given span of partitions.
 *
 * Results:
 *	Returns the space currently used in the span of partitions.
 *
 *----------------------------------------------------------------------
 */
static int
GetSpan(partArr, length, withPad)
    Partition *partArr;		/* Array of partitions */
    int length;			/* Number of partitions spanned */
    int withPad;		/* If non-zero, include the extra padding at
				 * the end partitions of the span in the space
				 * used */
{
    register Partition *partPtr;
    Partition *startPtr, *endPtr;
    register int spaceUsed;

    startPtr = partArr;
    endPtr = partArr + (length - 1);

    spaceUsed = 0;
    for (partPtr = startPtr; partPtr <= endPtr; partPtr++) {
	spaceUsed += partPtr->size;
    }
    if (!withPad) {
	spaceUsed -= (startPtr->pad + endPtr->pad);
    }
    return (spaceUsed);
}

/*
 *----------------------------------------------------------------------
 *
 * GrowSpan --
 *
 *	Expand the span by the amount of the extra space needed.
 *      This procedure is used in LayoutPartitions to grow the
 *	partitions to their minimum nominal size, starting from
 *	a zero width and height space.
 *
 *	This looks more complicated than it really is.  The idea is
 *	to make the size of the partitions correspond to the smallest
 *	cubicle spans.  For example, if window A is in column 1 and
 *	window B spans both columns 0 and 1, any extra space needed
 *	to fit window B should come from column 0.
 *
 *	On the first pass we try to add space to partitions which have
 *	not been touched yet (i.e. have no nominal size).  Since the
 *	row and column lists are sorted in ascending order of the number
 *	of rows or columns spanned, the space is distributed amongst the
 *	smallest spans first.
 *
 *	The second pass handles the case of windows which have the same
 *	span.  For example, if A and B, which span the same number of
 *	partitions are the only windows to span column 1, column 1 would
 *	grow to contain the bigger of the two slices of space.
 *
 *	If there is still extra space after the first two passes, this
 *	means that there were no partitions of with no window spans or
 *	the same order span that could be expanded. The third pass
 *	will try to remedy this by parcelling out the left over space
 *	evenly among the rest of the partitions.
 *
 *	On each pass, we have to keep iterating over the span, evenly
 *	doling out slices of extra space, because we may hit partition
 *	limits as space is donated.  In addition, if there are left
 *	over pixels because of round-off, this will distribute them as
 *	evenly as possible.  For the worst case, it will take *length*
 *	passes to expand the span.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 * 	The partitions in the span may be expanded.
 *
 *----------------------------------------------------------------------
 */
static void
GrowSpan(array, length, extraSpace)
    Partition *array;		/* Array of (column/row) partitions  */
    int length;			/* Number of partitions in the span */
    int extraSpace;		/* The amount of extra space needed to
				 * grow the span. */
{
    register Partition *partPtr;
    Partition *startPtr, *endPtr;
    int availSpace, adjustSize;
    int numAvail;		/* Number of partitions with space available */

    startPtr = array;
    endPtr = array + length;

    /*
     *-------------------------------------------------------------------
     *
     * Pass 1: Add space first to partitions which were previously empty
     *
     *-------------------------------------------------------------------
     */

    /* Find out how many partitions have no size yet */
    numAvail = 0;
    for (partPtr = startPtr; partPtr < endPtr; partPtr++) {
	if ((partPtr->nomSize == 0) && (partPtr->maxSize > partPtr->size)) {
	    numAvail++;
	}
    }
    while ((numAvail > 0) && (extraSpace > 0)) {
	adjustSize = extraSpace / numAvail;
	if (adjustSize == 0) {
	    adjustSize = 1;
	}
	for (partPtr = startPtr; (partPtr < endPtr) && (extraSpace > 0);
	    partPtr++) {
	    availSpace = partPtr->maxSize - partPtr->size;
	    if ((partPtr->nomSize == 0) && (availSpace > 0)) {
		if (adjustSize < availSpace) {
		    extraSpace -= adjustSize;
		    partPtr->size += adjustSize;
		} else {
		    extraSpace -= availSpace;
		    partPtr->size += availSpace;
		    numAvail--;
		}
		partPtr->span = length;
	    }
	}
    }

    /*
     *-------------------------------------------------------------------
     *
     * Pass 2: Add space to partitions which have the same minimum span
     *
     *-------------------------------------------------------------------
     */

    numAvail = 0;
    for (partPtr = startPtr; partPtr < endPtr; partPtr++) {
	if ((partPtr->span == length) && (partPtr->maxSize > partPtr->size)) {
	    numAvail++;
	}
    }
    while ((numAvail > 0) && (extraSpace > 0)) {
	adjustSize = extraSpace / numAvail;
	if (adjustSize == 0) {
	    adjustSize = 1;
	}
	for (partPtr = startPtr; (partPtr < endPtr) && (extraSpace > 0);
	    partPtr++) {
	    availSpace = partPtr->maxSize - partPtr->size;
	    if ((partPtr->span == length) && (availSpace > 0)) {
		if (adjustSize < availSpace) {
		    extraSpace -= adjustSize;
		    partPtr->size += adjustSize;
		} else {
		    extraSpace -= availSpace;
		    partPtr->size += availSpace;
		    numAvail--;
		}
	    }
	}
    }

    /*
     *-------------------------------------------------------------------
     *
     * Pass 3: Try to expand all the windows with space still available
     *
     *-------------------------------------------------------------------
     */

    /* Find out how many partitions still have space available */
    numAvail = 0;
    for (partPtr = startPtr; partPtr < endPtr; partPtr++) {
	if (partPtr->maxSize > partPtr->size) {
	    numAvail++;
	}
	partPtr->nomSize = partPtr->size;
    }
    while ((numAvail > 0) && (extraSpace > 0)) {
	adjustSize = extraSpace / numAvail;
	if (adjustSize == 0) {
	    adjustSize = 1;
	}
	for (partPtr = startPtr; (partPtr < endPtr) && (extraSpace > 0);
	    partPtr++) {
	    availSpace = partPtr->maxSize - partPtr->size;
	    if (availSpace > 0) {
		if (adjustSize < availSpace) {
		    extraSpace -= adjustSize;
		    partPtr->nomSize = partPtr->size =
			(partPtr->size + adjustSize);
		} else {
		    extraSpace -= availSpace;
		    partPtr->nomSize = partPtr->size =
			(partPtr->size + availSpace);
		    numAvail--;
		}
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * AdjustPartitions --
 *
 *	Adjust the span by the amount of the extra space needed.
 *	If the amount (extraSpace) is negative, shrink the span,
 *	otherwise expand it.  Size constraints on the partitions may
 *	prevent any or all of the spacing adjustments.
 *
 *	This is very much like the GrowSpan procedure, but in this
 *	case we are shrinking or expanding all the (row or column)
 *	partitions. It uses a two pass approach, first giving
 *	space to partitions which not are smaller/larger than their
 *	nominal sizes. This is because constraints on the partitions
 *	may cause resizing to be non-linear.
 *
 *	If there is still extra space, this means that all partitions
 *	are at least to their nominal sizes.  The second pass will
 *	try to add/remove the left over space evenly among all the
 *	partitions which still have space available.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 * 	The size of the partitions in the span may be increased
 *	or decreased.
 *
 *----------------------------------------------------------------------
 */
static void
AdjustPartitions(array, length, extraSpace)
    Partition *array;		/* Array of (column/row) partitions  */
    int length;			/* Number of partitions */
    int extraSpace;		/* The amount of extra space to grow or shrink
				 * the span. If negative, it represents the
				 * amount of space to remove */
{
    register Partition *partPtr;
    Partition *startPtr, *endPtr;
    int availSpace, adjustSize;
    int numAvail;

    startPtr = array;
    endPtr = array + length;

    /*
     *-------------------------------------------------------------------
     *
     * Pass 1: Adjust partition's with space beyond its nominal size.
     *
     *-------------------------------------------------------------------
     */
    numAvail = 0;
    for (partPtr = startPtr; partPtr < endPtr; partPtr++) {
	if (extraSpace < 0) {
	    availSpace = partPtr->size - partPtr->nomSize;
	} else {
	    availSpace = partPtr->nomSize - partPtr->size;
	}
	if (availSpace > 0) {
	    numAvail++;
	}
    }
    while ((numAvail > 0) && (extraSpace != 0)) {
	adjustSize = extraSpace / numAvail;
	if (adjustSize == 0) {
	    adjustSize = (extraSpace > 0) ? 1 : -1;
	}
	for (partPtr = startPtr; (partPtr < endPtr) && (extraSpace != 0);
	    partPtr++) {
	    availSpace = partPtr->nomSize - partPtr->size;
	    if (((extraSpace > 0) && (availSpace > 0)) ||
		((extraSpace < 0) && (availSpace < 0))) {
		if (ABS(adjustSize) < ABS(availSpace)) {
		    extraSpace -= adjustSize;
		    partPtr->size += adjustSize;
		} else {
		    extraSpace -= availSpace;
		    partPtr->size += availSpace;
		    numAvail--;
		}
	    }
	}
    }

    /*
     *-------------------------------------------------------------------
     *
     * Pass 2: Adjust the partitions with space still available
     *
     *-------------------------------------------------------------------
     */

    numAvail = 0;
    for (partPtr = startPtr; partPtr < endPtr; partPtr++) {
	if (extraSpace > 0) {
	    availSpace = partPtr->maxSize - partPtr->size;
	} else {
	    availSpace = partPtr->size - partPtr->minSize;
	}
	if (availSpace > 0) {
	    numAvail++;
	}
    }
    while ((numAvail > 0) && (extraSpace != 0)) {
	adjustSize = extraSpace / numAvail;
	if (adjustSize == 0) {
	    adjustSize = (extraSpace > 0) ? 1 : -1;
	}
	for (partPtr = startPtr; (partPtr < endPtr) && (extraSpace != 0);
	    partPtr++) {
	    if (extraSpace > 0) {
		availSpace = partPtr->maxSize - partPtr->size;
	    } else {
		availSpace = partPtr->minSize - partPtr->size;
	    }
	    if (((extraSpace > 0) && (availSpace > 0)) ||
		((extraSpace < 0) && (availSpace < 0))) {
		if (ABS(adjustSize) < ABS(availSpace)) {
		    extraSpace -= adjustSize;
		    partPtr->size += adjustSize;
		} else {
		    extraSpace -= availSpace;
		    partPtr->size += availSpace;
		    numAvail--;
		}
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ResetPartitions --
 *
 *	Sets/resets the size of each row and column partition to the
 *	minimum limit of the partition (this is usually zero). This
 *	routine gets called when new slave windows are added, deleted,
 *	or resized.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 * 	The size of each partition is re-initialized to its minimum size.
 *
 *----------------------------------------------------------------------
 */
static void
ResetPartitions(partPtr, length)
    register Partition *partPtr;
    int length;
{
    register int i;
    int size;

    for (i = 0; i < length; i++) {
	if (partPtr->reqSize.nom > 0) {
	    /*
	     * This could be done more cleanly.  Want to ensure that
	     * the requested nominal size is not overridden when
	     * determining the normal sizes.  So temporarily fix min
	     * and max to the nominal size and reset them back later.
	     */
	    partPtr->minSize = partPtr->maxSize = partPtr->size =
		partPtr->nomSize = partPtr->reqSize.nom;

	} else {
	    partPtr->minSize = partPtr->reqSize.min;
	    partPtr->maxSize = partPtr->reqSize.max;
	    size = 2 * partPtr->pad;
	    if (size < partPtr->minSize) {
		size = partPtr->minSize;
	    } else if (size > partPtr->maxSize) {
		size = partPtr->maxSize;
	    }
	    partPtr->nomSize = 0;
	    partPtr->size = size;
	}
	partPtr->span = 0;
	partPtr++;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * SetNominalSizes
 *
 *	Sets the normal sizes for each partition in the array.
 *	The partition size is the requested window size plus an
 *	amount of padding.  In addition, adjust the min/max bounds
 *	of the partition depending upon the resize flags (whether
 *	the partition can be expanded or shrunk from its normal
 *	size).
 *
 * Results:
 *	Returns the total space needed for the all the partitions.
 *
 * Side Effects:
 *	The nominal size of each partition in the array is set.
 *	This is used later to determine how to shrink or grow the
 *	table if the master window cannot be resized to accommodate
 *	exactly the size requirements of all the partitions.
 *
 *----------------------------------------------------------------------
 */
static int
SetNominalSizes(partPtr, numEntries)
    register Partition *partPtr;/* Row or column partition array */
    int numEntries;		/* Number of partitions in the array */
{
    register int i, size;
    int total = 0;

    for (i = 0; i < numEntries; i++) {
	/*
	 * Restore the real bounds after setting nominal size.
	 */
	partPtr->minSize = partPtr->reqSize.min;
	partPtr->maxSize = partPtr->reqSize.max;

	size = partPtr->size;
	if (size > partPtr->maxSize) {
	    size = partPtr->maxSize;
	}
	partPtr->nomSize = partPtr->size = size;
	total += partPtr->nomSize;

	/*
	 * If a partition can't be resized (to either expand or
	 * shrink), hold its respective limit at its normal size.
	 */

	if (!(partPtr->resize & RESIZE_EXPAND)) {
	    partPtr->maxSize = partPtr->nomSize;
	}
	if (!(partPtr->resize & RESIZE_SHRINK)) {
	    partPtr->minSize = partPtr->nomSize;
	}
	partPtr++;
    }
    return (total);
}

/*
 *----------------------------------------------------------------------
 *
 * LayoutPartitions --
 *
 *	Calculates the normal space requirements for both the row
 *	and column partitions.  Each slave window is added in order of
 *	the number of rows or columns spanned, which defines the space
 *	needed among in the partitions spanned.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 * 	The sum of normal sizes set here will be used as the normal
 *	size for the master window.
 *
 *----------------------------------------------------------------------
 */
static void
LayoutPartitions(tablePtr)
    Table *tablePtr;
{
    register Blt_ListEntry *entryPtr;
    register Cubicle *cubiPtr;
    Partition *rowPtr, *colPtr;
    int neededSpace, usedSpace, totalSpace;
    int twiceBW;

    twiceBW = (2 * Tk_InternalBorderWidth(tablePtr->tkwin));
    ResetPartitions(tablePtr->colPtr, tablePtr->numCols);
    for (entryPtr = Blt_FirstListEntry(&(tablePtr->colSorted));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	cubiPtr = (Cubicle *)Blt_GetListValue(entryPtr);
	neededSpace = GetReqWidth(cubiPtr) +
	    2 * (cubiPtr->padX + cubiPtr->extBW);
	if (neededSpace > 0) {
	    colPtr = tablePtr->colPtr + cubiPtr->colIndex;
	    usedSpace = GetSpan(colPtr, cubiPtr->colSpan, WITHOUT_PAD);
	    if (neededSpace > usedSpace) {
		GrowSpan(colPtr, cubiPtr->colSpan, neededSpace - usedSpace);
	    }
	}
    }
    totalSpace = SetNominalSizes(tablePtr->colPtr, tablePtr->numCols);
    tablePtr->reqWidth = totalSpace + twiceBW;

    ResetPartitions(tablePtr->rowPtr, tablePtr->numRows);
    for (entryPtr = Blt_FirstListEntry(&(tablePtr->rowSorted));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	cubiPtr = (Cubicle *)Blt_GetListValue(entryPtr);
	neededSpace = GetReqHeight(cubiPtr) +
	    2 * (cubiPtr->padY + cubiPtr->extBW);
	if (neededSpace > 0) {
	    rowPtr = tablePtr->rowPtr + cubiPtr->rowIndex;
	    usedSpace = GetSpan(rowPtr, cubiPtr->rowSpan, WITHOUT_PAD);
	    if (neededSpace > usedSpace) {
		GrowSpan(rowPtr, cubiPtr->rowSpan, neededSpace - usedSpace);
	    }
	}
    }
    totalSpace = SetNominalSizes(tablePtr->rowPtr, tablePtr->numRows);
    tablePtr->reqHeight = totalSpace + twiceBW;
}

/*
 *----------------------------------------------------------------------
 *
 * ArrangeCubicles
 *
 *	Places each slave window at its proper location.  First
 *	determines the size and position of the each window.
 *	It then considers the following:
 *
 *	  1. translation of slave window position its parent window.
 *	  2. fill style
 *	  3. anchor
 *	  4. external and internal padding
 *	  5. window size must be greater than zero
 *
 * Results:
 *	None.
 *
 * Side Effects:
 * 	The size of each partition is re-initialized its minimum size.
 *
 *----------------------------------------------------------------------
 */
static void
ArrangeCubicles(tablePtr)
    Table *tablePtr;		/* Table widget structure */
{
    register Blt_ListEntry *entryPtr;
    register Cubicle *cubiPtr;
    register int width, height;
    Partition *colPtr, *rowPtr;
    register int x, y;
    int winWidth, winHeight;
    int deltaX, deltaY;
    Tk_Window parent, ancestor;
    int maxX, maxY;

    maxX = tablePtr->width - Tk_InternalBorderWidth(tablePtr->tkwin);
    maxY = tablePtr->height - Tk_InternalBorderWidth(tablePtr->tkwin);
    for (entryPtr = tablePtr->listPtr->tailPtr; entryPtr != NULL;
	entryPtr = entryPtr->prevPtr) {
	cubiPtr = (Cubicle *)Blt_GetListValue(entryPtr);

	colPtr = tablePtr->colPtr + cubiPtr->colIndex;
	rowPtr = tablePtr->rowPtr + cubiPtr->rowIndex;

	x = colPtr->offset + colPtr->pad + cubiPtr->padX + cubiPtr->extBW;
	y = rowPtr->offset + rowPtr->pad + cubiPtr->padY + cubiPtr->extBW;

	/*
	 * Unmap any slave windows which start outside of the master
	 * window
	 */
	if ((x >= maxX) || (y >= maxY)) {
	    if (Tk_IsMapped(cubiPtr->tkwin)) {
		Tk_UnmapWindow(cubiPtr->tkwin);
	    }
	    continue;
	}
	width = GetSpan(colPtr, cubiPtr->colSpan, WITHOUT_PAD) -
	    (2 * (cubiPtr->padX + cubiPtr->extBW));
	height = GetSpan(rowPtr, cubiPtr->rowSpan, WITHOUT_PAD) -
	    (2 * (cubiPtr->padY + cubiPtr->extBW));

	winWidth = GetReqWidth(cubiPtr);
	winHeight = GetReqHeight(cubiPtr);

	/*
	 *
	 * Compare the window's requested size to the size of the
	 * span.
	 *
	 * 1) If it's bigger or if the fill flag is set, make them
	 *    the same size. Check that the new size is within the
	 *    bounds set for the window.
	 *
	 * 2) Otherwise, position the window in the space according
	 *    to its anchor.
	 *
	 */

	if ((width <= winWidth) || (cubiPtr->fill & FILL_X)) {
	    winWidth = width;
	    if (winWidth > cubiPtr->reqWidth.max) {
		winWidth = cubiPtr->reqWidth.max;
	    }
	}
	if ((height <= winHeight) || (cubiPtr->fill & FILL_Y)) {
	    winHeight = height;
	    if (winHeight > cubiPtr->reqHeight.max) {
		winHeight = cubiPtr->reqHeight.max;
	    }
	}
	/*
	 * If the window is too small to be interesting (i.e. it has
	 * only an external border) then unmap it.
	 */
	if ((winWidth < 1) || (winHeight < 1)) {
	    if (Tk_IsMapped(cubiPtr->tkwin)) {
		Tk_UnmapWindow(cubiPtr->tkwin);
	    }
	    continue;
	}
	deltaX = deltaY = 0;
	if (width > winWidth) {
	    deltaX = (width - winWidth);
	}
	if (height > winHeight) {
	    deltaY = (height - winHeight);
	}
	if ((deltaX > 0) || (deltaY > 0)) {
	    XPoint newPt;

	    newPt = TranslateAnchor(deltaX, deltaY, cubiPtr->anchor);
	    x += newPt.x, y += newPt.y;
	}
	/*
	 * Clip the slave window at the bottom and/or right edge of
	 * the master
	 */
	if (winWidth > (maxX - x)) {
	    winWidth = (maxX - x);
	}
	if (winHeight > (maxY - y)) {
	    winHeight = (maxY - y);
	}
	/*
	 * If the slave's parent window is not the master window,
	 * translate the master window's x and y coordinates to the
	 * coordinate system of the slave's parent.
	 */

	parent = Tk_Parent(cubiPtr->tkwin);
	for (ancestor = tablePtr->tkwin; ancestor != parent;
	    ancestor = Tk_Parent(ancestor)) {
	    x += Tk_X(ancestor) + Tk_Changes(ancestor)->border_width;
	    y += Tk_Y(ancestor) + Tk_Changes(ancestor)->border_width;
	}

	/*
	 * Resize or move the window if necessary. Save the window's x
	 * and y coordinates for reference next time.
	 */

	if ((x != cubiPtr->x) || (y != cubiPtr->y) ||
	    (winWidth != Tk_Width(cubiPtr->tkwin)) ||
	    (winHeight != Tk_Height(cubiPtr->tkwin))) {
#ifdef notdef
	    fprintf(stderr, "%s at %d,%d is %dx%d\n",
		Tk_PathName(cubiPtr->tkwin), x, y, winWidth, winHeight);
#endif
	    Tk_MoveResizeWindow(cubiPtr->tkwin, x, y, (unsigned int)winWidth,
		(unsigned int)winHeight);
	    cubiPtr->x = x, cubiPtr->y = y;
	}
	if (!Tk_IsMapped(cubiPtr->tkwin)) {
	    Tk_MapWindow(cubiPtr->tkwin);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ArrangeTable --
 *
 *
 * Results:
 *	None.
 *
 * Side Effects:
 * 	The slave windows in the table are possibly resized and redrawn.
 *
 *----------------------------------------------------------------------
 */
static void
ArrangeTable(clientData)
    ClientData clientData;
{
    Table *tablePtr = (Table *)clientData;
    int width, height;
    register int i;
    unsigned int offset;
    int twiceBW;

#ifdef notdef
    fprintf(stderr, "ArrangeTable(%s)\n", Tk_PathName(tablePtr->tkwin));
#endif
    Tk_Preserve((ClientData)tablePtr);
    tablePtr->flags &= ~ARRANGE_PENDING;

    /*
     * If the table has no children anymore, then don't do anything at
     * all: just leave the master window's size as-is.
     */

    if ((Blt_GetListLength(tablePtr->listPtr) == 0) ||
	(tablePtr->tkwin == NULL)) {
	Tk_Release((ClientData)tablePtr);
	return;
    }
    if (tablePtr->flags & REQUEST_LAYOUT) {
	tablePtr->flags &= ~REQUEST_LAYOUT;
	LayoutPartitions(tablePtr);
    }
    /*
     * Initially, try to fit the partitions exactly into the master
     * window by resizing the window.  If the window's requested size
     * is different, send a request to the master window's geometry
     * manager to resize.
     */

    if ((tablePtr->reqWidth != Tk_ReqWidth(tablePtr->tkwin)) ||
	(tablePtr->reqHeight != Tk_ReqHeight(tablePtr->tkwin))) {
	Tk_GeometryRequest(tablePtr->tkwin, tablePtr->reqWidth,
	    tablePtr->reqHeight);
	tablePtr->flags |= ARRANGE_PENDING;
	Tk_DoWhenIdle(ArrangeTable, (ClientData)tablePtr);
	Tk_Release((ClientData)tablePtr);
	return;
    }
    /*
     * If the parent isn't mapped then don't do anything more: wait
     * until it gets mapped again.  Need to get at least to here to
     * reflect size needs up the window hierarchy, but there's no
     * point in actually mapping the children.
     */

    if (!Tk_IsMapped(tablePtr->tkwin)) {
	Tk_Release((ClientData)tablePtr);
	return;
    }
    /*
     * Save the width and height of the master window so we know when
     * it has changed during ConfigureNotify events.
     */

    tablePtr->width = Tk_Width(tablePtr->tkwin);
    tablePtr->height = Tk_Height(tablePtr->tkwin);
    twiceBW = (2 * Tk_InternalBorderWidth(tablePtr->tkwin));

    width = GetSpan(tablePtr->colPtr, tablePtr->numCols, WITH_PAD) + twiceBW;
    height = GetSpan(tablePtr->rowPtr, tablePtr->numRows, WITH_PAD) + twiceBW;

    /*
     * If the previous geometry request was not fulfilled (i.e. the
     * size of the master window is different from partitions' space
     * requirements), try to adjust size of the partitions to fit the
     * window.
     */

    if (tablePtr->width != width) {
	AdjustPartitions(tablePtr->colPtr, tablePtr->numCols,
	    tablePtr->width - width);
	width = GetSpan(tablePtr->colPtr, tablePtr->numCols, WITH_PAD) +
	    twiceBW;
    }
    if (tablePtr->height != height) {
	AdjustPartitions(tablePtr->rowPtr, tablePtr->numRows,
	    tablePtr->height - height);
	height = GetSpan(tablePtr->rowPtr, tablePtr->numRows, WITH_PAD) +
	    twiceBW;
    }
    /*
     * If after adjusting the size of the partitions the space
     * required does not equal the size of the window, do one of the
     * following:
     *
     * 1) If is smaller, center the table in the window.
     * 2) If it's still bigger, clip the partitions that extend beyond
     *    the edge of the master window.
     *
     * Set the row and column offsets (including the master's internal
     * border width). To be used later when positioning the slave
     * windows.
     */

    offset = Tk_InternalBorderWidth(tablePtr->tkwin);
    if (width < tablePtr->width) {
	offset += (tablePtr->width - width) / 2;
    }
    for (i = 0; i < tablePtr->numCols; i++) {
	tablePtr->colPtr[i].offset = offset;
	offset += tablePtr->colPtr[i].size;
    }

    offset = Tk_InternalBorderWidth(tablePtr->tkwin);
    if (height < tablePtr->height) {
	offset += (tablePtr->height - height) / 2;
    }
    for (i = 0; i < tablePtr->numRows; i++) {
	tablePtr->rowPtr[i].offset = offset;
	offset += tablePtr->rowPtr[i].size;
    }
    ArrangeCubicles(tablePtr);
    Tk_Release((ClientData)tablePtr);
}

/*
 *--------------------------------------------------------------
 *
 * PartitionCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to the table geometry manager. It handles
 *	only those commands related to the table's rows or columns.
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
PartitionCmd(tablePtr, interp, type, argc, argv)
    Table *tablePtr;
    Tcl_Interp *interp;
    PartitionTypes type;
    int argc;
    char **argv;
{
    char c;
    int length;
    int result = TCL_ERROR;
    char *partClass;

    partClass = partitionNames[(int)type];
    if (argc < 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " master ", partClass, " option number ?args?", (char *)NULL);
	return TCL_ERROR;
    }
    c = argv[3][0];
    length = strlen(argv[3]);
    if ((c == 'c') && (strncmp(argv[3], "configure", length) == 0)) {
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" master ", partClass, " configure index", (char *)NULL);
	    return TCL_ERROR;
	}
	result = ConfigurePartition(tablePtr, interp, type, argc - 4, argv + 4);
    } else if ((c == 'd') && (strncmp(argv[3], "dimension", length) == 0)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" master ", partClass, " dimension", (char *)NULL);
	    return TCL_ERROR;
	}
	sprintf(interp->result, "%d", NUMENTRIES(tablePtr, type));
	result = TCL_OK;
    } else if ((c == 'i') && (strncmp(argv[3], "info", length) == 0)) {
	if (argc < 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" master ", partClass, " info index", (char *)NULL);
	    return TCL_ERROR;
	}
	result = PartitionInfo(tablePtr, interp, type, argc - 4, argv + 4);
    } else if ((c == 's') && (strncmp(argv[3], "sizes", length) == 0)) {
	if (argc != 5) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" master ", partClass, " sizes index", (char *)NULL);
	    return TCL_ERROR;
	}
	result = PartitionSizes(tablePtr, interp, type, argv[4]);
    } else {
	Tcl_AppendResult(interp, "unknown ", partClass, " option \"", argv[3],
	    "\": should be configure, dimension, info, or sizes",
	    (char *)NULL);
    }
    return result;
}

/*
 *--------------------------------------------------------------
 *
 * TableCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to the table geometry manager.
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
TableCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    char c;
    int length;
    int result = TCL_ERROR;
    Tk_Window mainWindow = (Tk_Window)clientData;
    Table *tablePtr;

    if (argc < 2) {
	Tcl_AppendResult(interp,
	    "wrong # args: should be \"", argv[0], " options\"", (char *)NULL);
	return TCL_ERROR;
    }
    /* Initialize structures managing all table widgets once. */

    if (!initialized) {
	Tcl_InitHashTable(&masterWindows, TCL_ONE_WORD_KEYS);
	Tcl_InitHashTable(&slaveWindows, TCL_ONE_WORD_KEYS);
	initialized = TRUE;
    }
    tablePtr = NULL;
    c = argv[1][0];
    length = strlen(argv[1]);
    if (c == '.') {
	tablePtr = FindTable(interp, argv[1], mainWindow, 0);

	/* If the table doesn't exist, create one. */
	if (tablePtr == NULL) {
	    tablePtr = CreateTable(interp, argv[1], mainWindow);
	    if (tablePtr == NULL) {
		return TCL_ERROR;
	    }
	}
	return (ManageWindows(tablePtr, interp, argc - 2, argv + 2));
    } else if ((c == 'c') && (length > 2) &&
	(strncmp(argv[1], "configure", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" configure slave ?option-values ...?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	return (ConfigureCubicle(clientData, interp, argc - 2, argv + 2));
    } else if ((c == 'f') && (strncmp(argv[1], "forget", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" forget slave ?slave ...?\"", (char *)NULL);
	    return TCL_ERROR;
	}
	return (ForgetWindow(clientData, interp, argc - 2, argv + 2));
    } else if ((c == 'i') && (strncmp(argv[1], "info", length) == 0)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" info slave\"", (char *)NULL);
	    return TCL_ERROR;
	}
	return (SlaveInfo(clientData, interp, argv[2]));
    }
    /*
     * The rest of the options all take the pathname of the master
     * window as their second argument.  If the second argument starts
     * with a ".", try to find the table associated with it.
     */
    if ((argc > 2) && (argv[2][0] == '.')) {
	tablePtr = FindTable(interp, argv[2], mainWindow, TCL_LEAVE_ERR_MSG);
	if (tablePtr == NULL) {
	    return TCL_ERROR;
	}
    }
    if ((c == 'c') && (length > 2) &&
	(strncmp(argv[1], "column", length) == 0)) {
	result = PartitionCmd(tablePtr, interp, COLUMN_PARTITION_TYPE, argc,
	    argv);
    } else if ((c == 'r') && (strncmp(argv[1], "row", length) == 0)) {
	result = PartitionCmd(tablePtr, interp, ROW_PARTITION_TYPE, argc,
	    argv);
    } else if ((c == 'm') && (strncmp(argv[1], "masters", length) == 0)) {
	if ((argc != 2) && (argc != 3)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" masters ?pattern?", (char *)NULL);
	    return TCL_ERROR;
	}
	result = MasterNames(clientData, interp, argc - 2, argv + 2);
    } else if ((c == 's') && (strncmp(argv[1], "slaves", length) == 0)) {
	if ((argc != 3) && (argc != 4)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" slaves master ?pattern?", (char *)NULL);
	    return TCL_ERROR;
	}
	result = SlaveNames(tablePtr, interp, argc - 2, argv + 2);
    } else if ((c == 'a') && (strncmp(argv[1], "arrange", length) == 0)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" arrange master", (char *)NULL);
	    return TCL_ERROR;
	}
	result = LayoutTable(tablePtr, interp, argv[2]);
    } else {
	Tcl_AppendResult(interp, "unknown option \"", argv[1], "\": should be\
 configure, column, forget, info, masters, row, or slaves", (char *)NULL);
    }
    return result;
}

/*
 *--------------------------------------------------------------
 *
 * Blt_TableInit --
 *
 *	This procedure is invoked to initialized the Tcl command
 *	that corresponds to the table geometry manager.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Creates the new command and adds an entry into a global
 *	Tcl associative array.
 *
 *--------------------------------------------------------------
 */
int
Blt_TableInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, "blt_table", (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_table\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_table\" requires Tk", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", "blt_table", TABLE_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, "blt_table", TableCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}
