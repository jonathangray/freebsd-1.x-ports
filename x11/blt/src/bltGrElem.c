/*
 * bltGrElem.c --
 *
 *	This module implements a elements in the graph widget
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

static int ParseVector _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *value, char *widgRec, int offset));
static char *PrintVector _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));
static int ParseCoordPairs _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *value, char *widgRec,
	int offset));
static char *PrintCoordPairs _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

#include "bltGrElem.h"

#define DEF_VECTOR_SIZE	32	/* Starting size of a data vector. */

enum DataModes {
    CopyData, AppendData
};
enum DataTypes {
    SingleVector = 1, TwinVectors = 2
};

Tk_CustomOption bltXVectorOption =
{
    ParseVector, PrintVector, (ClientData)ANY_X_MASK
};
Tk_CustomOption bltYVectorOption =
{
    ParseVector, PrintVector, (ClientData)ANY_Y_MASK
};

Tk_CustomOption bltTwinOption =
{
    ParseCoordPairs, PrintCoordPairs, (ClientData)0
};

extern Element *Blt_BarElement();
extern Element *Blt_LineElement();


/* ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 *
 * ConvertExpressions --
 *
 *	Converts strings from an array of numeric expressions into
 *	an array of double precision values.
 *
 * Results:
 *     	Returns TCL_OK if sucessful. Otherwise TCL_ERROR will be
 *	returned and interp->result will be filled with an error message.
 *
 *----------------------------------------------------------------------
 */
static int
ConvertExpressions(interp, exprArr, numExpr, dataArr, start, step)
    Tcl_Interp *interp;		/* Interpreter to report error back to */
    char *exprArr[];		/* Array of expression strings */
    int numExpr;		/* Number of expressions in array */
    double dataArr[];
    int start, step;		/* Starting point and step */
{
    register int i, count;
    double x;

    /*
     * Parse and evaluate the list of numeric expressions into an
     * array of floating point numbers.
     */
    for (count = 0, i = start; i < numExpr; i += step, count++) {
	if (Tcl_ExprDouble(interp, exprArr[i], &x) != TCL_OK) {
	    return TCL_ERROR;
	}
	dataArr[count] = x;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetDataLimits --
 *
 *	Find the minimum, positive minumum, and maximum values in a
 *	given vector and store the results in the vector structure.
 *
 * Results:
 *     	None.
 *
 * Side Effects:
 *	Minimum, positive minimum, and maximum values are stored in
 *	the vector.
 *
 *----------------------------------------------------------------------
 */
static void
GetDataLimits(vecPtr)
    register Vector *vecPtr;
{
    register int i;

    /*
     * Find the minimum, positive minimum, and maximum values in the
     * array.
     */
    vecPtr->min = vecPtr->max = vecPtr->data[0];
    vecPtr->logMin = Blt_posInfinity;
    for (i = 0; i < vecPtr->length; i++) {
	if ((vecPtr->data[i] > 0.0) && (vecPtr->data[i] < vecPtr->logMin)) {
	    vecPtr->logMin = vecPtr->data[i];
	}
	if (vecPtr->data[i] < vecPtr->min) {
	    vecPtr->min = vecPtr->data[i];
	} else if (vecPtr->data[i] > vecPtr->max) {
	    vecPtr->max = vecPtr->data[i];
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * AppendVector --
 *
 *	Appends or overwrites a given vector with numeric expressions.
 *
 * Results:
 *     	A standard Tcl result.
 *
 * Side Effects:
 *	Memory is allocated for the new vector values.
 *
 *----------------------------------------------------------------------
 */
static int
AppendVector(interp, vecPtr, exprArr, numExpr, start, type, mode)
    Tcl_Interp *interp;		/* Interpreter to report error back to */
    Vector *vecPtr;		/* Vector to be filled */
    char *exprArr[];		/* Array of expression strings */
    int numExpr;		/* Number of expressions in array */
    int start;			/* Starting point */
    enum DataTypes type;	/* Type of data (pairs or single vector) */
    enum DataModes mode;	/* Append or overwrite current data */
{
    unsigned int arraySize;
    unsigned int needed;
    double *dataArr;
    unsigned int offset;
    int step;

    if (numExpr < 1) {
	if (vecPtr->data != NULL) {
	    free((char *)vecPtr->data);
	}
	vecPtr->data = NULL;
	vecPtr->size = 0;
	vecPtr->length = 0;
	return TCL_OK;
    }
    step = (int)type;
    offset = (mode == AppendData) ? vecPtr->length : 0;
    needed = (numExpr / step) + offset;

    /*
     * Keep doubling the size of the array until we fit the number
     * needed
     */
    arraySize = vecPtr->size;
    if (arraySize == 0) {
	arraySize = DEF_VECTOR_SIZE;
    }
    while (needed > arraySize) {
	arraySize += arraySize;
    }

    dataArr = (double *)calloc(arraySize, sizeof(double));
    if (dataArr == NULL) {
	interp->result = "can't allocate data vector";
	return TCL_ERROR;
    }
    if (offset > 0) {		/* Copy previous contents */
	memcpy((char *)dataArr, (char *)vecPtr->data, offset * sizeof(double));
    }
    if (ConvertExpressions(interp, exprArr, numExpr,
	    &(dataArr[offset]), start, step) != TCL_OK) {
	free((char *)dataArr);
	return TCL_ERROR;
    }
    if (vecPtr->data != NULL) {
	free((char *)vecPtr->data);
    }
    vecPtr->data = dataArr;
    vecPtr->length = needed;
    vecPtr->size = arraySize;
    GetDataLimits(vecPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ParseVector --
 *
 *	Given a Tcl list of numeric expression representing the element
 *	values, convert into an array of double precision values. In
 *	addition, the minimum and maximum values are saved.  Since
 *	elastic values are allow (values which translate to the
 *	min/max of the graph), we must try to get the non-elastic
 *	minimum and maximum.
 *
 * Results:
 *	The return value is a standard Tcl result.  The vector is passed
 *	back via the vecPtr.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseVector(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* Type of axis vector to fill */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* Tcl list of expressions */
    char *widgRec;		/* Element record */
    int offset;			/* Offset of vector in Element record */
{
    Element *elemPtr = (Element *)(widgRec + offset);
    register Vector *vecPtr;
    int flags = (int)clientData;
    int numExprs;
    char **exprArr = NULL;

    vecPtr = (flags & ANY_X_MASK) ? &(elemPtr->x) : &(elemPtr->y);

    /* Split the list of expressions and check the values */
    if (Tcl_SplitList(interp, value, &numExprs, &exprArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numExprs >= 65535) {
	interp->result = "vector is too large";	/* XDrawLines limit */
	goto error;
    }
    if (AppendVector(interp, vecPtr, exprArr, numExprs, 0, SingleVector,
	    CopyData) != TCL_OK) {
	goto error;
    }
    free((char *)exprArr);
    return TCL_OK;

  error:
    /* Clean up, release allocated storage */
    if (exprArr) {
	free((char *)exprArr);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintVector --
 *
 *	Convert the vector of floating point values into a Tcl list.
 *
 * Results:
 *	The string representation of the vector is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintVector(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Type of axis vector to print */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element record */
    int offset;			/* Offset of vector in Element record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Element *elemPtr = (Element *)(widgRec + offset);
    register Vector *vecPtr;
    int flags = (int)clientData;
    register int i;
    char *result;
    Tcl_DString buffer;
    char string[TCL_DOUBLE_SPACE + 1];

    vecPtr = (flags & ANY_X_MASK) ? &(elemPtr->x) : &(elemPtr->y);
    if (vecPtr->length == 0) {
	return "";
    }
    Tcl_DStringInit(&buffer);
    for (i = 0; i < vecPtr->length; i++) {
	Tcl_PrintDouble(elemPtr->interp, vecPtr->data[i], string);
	Tcl_DStringAppendElement(&buffer, string);
    }
    result = Tcl_DStringValue(&buffer);
    result = strdup(result);
    *freeProcPtr = TCL_DYNAMIC;
    Tcl_DStringFree(&buffer);
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * ParseCoordPairs --
 *
 *	This procedure is like ParseVector except that it
 *	interprets the list of numeric expressions as X Y coordinate
 *	pairs.  The minimum and maximum for both the X and Y vectors are
 *	determined.
 *
 * Results:
 *	The return value is a standard Tcl result.  The vectors are passed
 *	back via the widget record (elemPtr).
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ParseCoordPairs(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *value;		/* Tcl list of numeric expressions */
    char *widgRec;		/* Element record */
    int offset;			/* not used */
{
    Element *elemPtr = (Element *)widgRec;
    int numExprs;
    char **exprArr = NULL;

    /* Split the list of numbers and check the values */
    if (Tcl_SplitList(interp, value, &numExprs, &exprArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numExprs >= 131070) {
	interp->result = "vector is too large";	/* XDrawLines limit */
	goto error;
    } else if (numExprs & 1) {
	interp->result = "odd number of values in -xydata option";
	goto error;
    }
    if (AppendVector(interp, &(elemPtr->x), exprArr, numExprs, 0,
	    TwinVectors, CopyData) != TCL_OK) {
	goto error;
    }
    if (AppendVector(interp, &(elemPtr->y), exprArr, numExprs, 1,
	    TwinVectors, CopyData) != TCL_OK) {
	goto error;
    }
    free((char *)exprArr);
    return TCL_OK;
  error:
    /* Clean up, release allocated storage */
    if (exprArr) {
	free((char *)exprArr);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintCoordPairs --
 *
 *	Convert pairs of floating point values in the X and Y arrays
 *	into a Tcl list.
 *
 * Results:
 *	The return value is a string (Tcl list).
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PrintCoordPairs(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* not used */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    register Element *elemPtr = (Element *)widgRec;
    register int i;
    char *result;
    int length;
    char string[TCL_DOUBLE_SPACE + 1];
    Tcl_DString buffer;

    result = "";
    if ((elemPtr->x.length < 1) || (elemPtr->y.length < 1)) {
	return result;
    }
    Tcl_DStringInit(&buffer);
    length = BLT_MIN(elemPtr->x.length, elemPtr->y.length);
    for (i = 0; i < length; i++) {
	Tcl_PrintDouble(elemPtr->interp, elemPtr->x.data[i], string);
	Tcl_DStringAppendElement(&buffer, string);
	Tcl_PrintDouble(elemPtr->interp, elemPtr->y.data[i], string);
	Tcl_DStringAppendElement(&buffer, string);
    }
    result = Tcl_DStringValue(&buffer);
    result = strdup(result);
    *freeProcPtr = TCL_DYNAMIC;
    Tcl_DStringFree(&buffer);
    return (result);
}

/*
 * Generic element routines:
 */
/*
 *----------------------------------------------------------------------
 *
 * CreateElement --
 *
 *	Add a new element to the graph.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static int
CreateElement(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register Element *elemPtr;
    Tcl_HashEntry *entryPtr;
    Blt_ListEntry *listPtr;
    int dummy;

    if (argc < 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element create name ?options?\"", (char *)NULL);
	return TCL_ERROR;
    }
    entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[3]);
    if (entryPtr != NULL) {
	Tcl_AppendResult(interp, "element \"", argv[3],
	    "\" already exists in \"", argv[0], "\"", (char *)NULL);
	return TCL_ERROR;
    }
    elemPtr = NULL;
    if (graphPtr->type == BAR_ELEM_TYPE) {
	elemPtr = Blt_BarElement();
    } else if (graphPtr->type == LINE_ELEM_TYPE) {
	elemPtr = Blt_LineElement();
    }
    if (elemPtr == NULL) {
	Tcl_AppendResult(interp, "can't create element \"", argv[3], "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    elemPtr->interp = interp;
    elemPtr->id = Tk_GetUid(argv[3]);
    elemPtr->label = strdup(argv[3]);
    elemPtr->axisFlags = STD_AXES_MASK;
    elemPtr->x.size = elemPtr->y.size = 0;
    elemPtr->x.length = elemPtr->y.length = 0;
    elemPtr->activeArr = elemPtr->staticArr;
    elemPtr->numActivePoints = 0;

    if (Tk_ConfigureWidget(interp, graphPtr->tkwin, elemPtr->configSpecs,
	    argc - 4, argv + 4, (char *)elemPtr, 0) != TCL_OK) {
	(*elemPtr->destroyProc) (graphPtr, elemPtr);
	return TCL_ERROR;
    }
    entryPtr = Tcl_CreateHashEntry(&(graphPtr->elemTable),
	(char *)elemPtr->id, &dummy);
    Tcl_SetHashValue(entryPtr, (ClientData)elemPtr);
    (*elemPtr->configProc) (graphPtr, elemPtr);
    listPtr = Blt_CreateListEntry(elemPtr->id);
    Blt_SetListValue(listPtr, elemPtr);
    Blt_LinkListAfter(&(graphPtr->elemList), listPtr,
	(Blt_ListEntry *)NULL);
    elemPtr->mapped = 1;
    elemPtr->flags |= LAYOUT_NEEDED;
    graphPtr->flags |= DIRTY;
    Blt_ComputeAxes(graphPtr);
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureElement --
 *
 *	Sets the element specifications by the given the command line
 *	arguments and calls the element specification configuration
 *	routine. If zero or one command line options are given, only
 *	information about the option(s) is returned in interp->result.
 *      If the element configuration has changed and the element is
 *	currently displayed, the axis limits are updated and recomputed.
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
ConfigureElement(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    Tcl_HashEntry *entryPtr;
    Element *elemPtr;
    int flags = TK_CONFIG_ARGV_ONLY;
    int result;

    if (argc < 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element configure name ?options?\"", (char *)NULL);
	return TCL_ERROR;
    }
    entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[3]);
    if (entryPtr == NULL) {
	Tcl_AppendResult(interp, "can't find element \"", argv[3],
	    "\" in \"", argv[0], "\"", (char *)NULL);
	return TCL_ERROR;
    }
    elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
    if (argc == 4) {
	return (Tk_ConfigureInfo(interp, graphPtr->tkwin, elemPtr->configSpecs,
		(char *)elemPtr, (char *)NULL, flags));
    } else if (argc == 5) {
	return (Tk_ConfigureInfo(interp, graphPtr->tkwin, elemPtr->configSpecs,
		(char *)elemPtr, argv[4], flags));
    }
    result = Tk_ConfigureWidget(interp, graphPtr->tkwin, elemPtr->configSpecs,
	argc - 4, argv + 4, (char *)elemPtr, flags);
    if ((*elemPtr->configProc) (graphPtr, elemPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (result != TCL_OK) {
	return TCL_ERROR;
    }
    elemPtr->flags |= LAYOUT_NEEDED;
    graphPtr->flags |= LAYOUT_NEEDED;
    if (elemPtr->mapped) {
	graphPtr->flags |= (DIRTY | REFRESH);
    }
    /* Layout entire graph if specific element options change */
    if (Blt_OptionChanged(elemPtr->configSpecs, "-label", (char *)NULL)) {
	graphPtr->flags |= LAYOUT_ALL;
    }
    Blt_ComputeAxes(graphPtr);
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * AppendElement --
 *
 *	Given a Tcl list of numeric expression representing the element
 *	values, convert into an array of double precision values. In
 *	addition, the minimum and maximum values are saved.  Since
 *	elastic values are allow (values which translate to the
 *	min/max of the graph), we must try to get the non-elastic
 *	minimum and maximum.
 *
 * Results:
 *	The return value is a standard Tcl result.  The vector is passed
 *	back via the vecPtr.
 *
 *----------------------------------------------------------------------
 */
static int
AppendElement(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register Vector *vecPtr;
    int numExprs;
    char **exprArr = NULL;
    int result = TCL_ERROR;
    unsigned int arraySize;
    register Element *elemPtr;
    Tcl_HashEntry *entryPtr;

    if (argc != 5) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element append name coords\"", NULL);
	return TCL_ERROR;
    }
    entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[3]);
    if (entryPtr == NULL) {
	Tcl_AppendResult(interp, "can't find element \"", argv[3],
	    "\" in \"", argv[0], "\"", (char *)NULL);
	return TCL_ERROR;
    }
    elemPtr = (Element *)Tcl_GetHashValue(entryPtr);

    /* Split the list of expressions and check the values */

    if (Tcl_SplitList(interp, argv[4], &numExprs, &exprArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numExprs < 2) {
	interp->result = "too few numeric expressions in coordinate pair list";
	goto error;
    }
    if (numExprs & 1) {
	interp->result = "odd number of expressions in coordinate pair list";
	goto error;
    }
    /*
     * Check both x and y vectors for XDrawLines size limit (64K)
     */
    vecPtr = &(elemPtr->x);
    arraySize = (numExprs / 2) + vecPtr->length;
    if (arraySize >= 65535) {
	interp->result = "x-coordinate vector is too large";
	goto error;
    }
    if (AppendVector(interp, vecPtr, exprArr, numExprs, 0,
	    TwinVectors, AppendData) != TCL_OK) {
	goto error;
    }
    vecPtr = &(elemPtr->y);
    arraySize = (numExprs / 2) + vecPtr->length;
    if (arraySize >= 65535) {
	interp->result = "y-coordinate vector is too large";
	goto error;
    }
    if (AppendVector(interp, vecPtr, exprArr, numExprs, 1,
	    TwinVectors, AppendData) != TCL_OK) {
	goto error;
    }
    result = TCL_OK;
    elemPtr->flags |= LAYOUT_NEEDED;	/* Re-generate screen points */
    graphPtr->flags |= DIRTY;
    Blt_ComputeAxes(graphPtr);
    Blt_RedrawGraph(graphPtr);
  error:
    if (exprArr != NULL) {
	free((char *)exprArr);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteElements --
 *
 *	Delete the named elements from the graph.
 *
 * Results:
 *	TCL_ERROR is returned if any of the named elements can not be found.
 *	Otherwise TCL_OK is returned;
 *
 * Side Effects:
 *	If the element is currently mapped, the plotting area of the
 *	graph is redrawn. Memory and resources allocated by the elements
 *	are released.
 *
 *----------------------------------------------------------------------
 */
static int
DeleteElements(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;
    int argc;			/* Number of element names */
    char **argv;		/* List of element names */
{
    register Element *elemPtr;
    Tcl_HashEntry *entryPtr;
    Blt_ListEntry *listPtr;
    register int i;
    int count;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element delete name ?name...?\"", (char *)NULL);
	return TCL_ERROR;
    }
    count = 0;
    for (i = 3; i < argc; i++) {
	entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[i]);
	if (entryPtr == NULL) {
	    Tcl_AppendResult(interp, "can't find element \"", argv[i],
		"\" in \"", argv[0], (char *)NULL);
	    return TCL_ERROR;
	}
	elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
	Tcl_DeleteHashEntry(entryPtr);

	listPtr = Blt_FindListEntry(&(graphPtr->elemList), argv[i]);
	if (listPtr != NULL) {
	    Blt_DeleteListEntry(&(graphPtr->elemList), listPtr);
	    count++;
	}
	/*
	 * Call the element's own destructor to release the memory and
	 * resources allocated for it.
	 */
	(*elemPtr->destroyProc) (graphPtr, elemPtr);
    }
    if (count > 0) {
	Blt_ComputeAxes(graphPtr);
	Blt_RedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ElementNames --
 *
 *	Runs through the given list of element entries and builds a
 *	Tcl list of element names.  This procedure is used in the
 *	"names" and "show" commands.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *	interp->result contains the list of element names.
 *
 *----------------------------------------------------------------------
 */
static int
ElementNames(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register Element *elemPtr;
    register Tcl_HashEntry *entryPtr;
    Tcl_HashSearch cursor;

    if ((argc < 3) || (argc > 4)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element names ?pattern?", (char *)NULL);
	return TCL_ERROR;
    }
    for (entryPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&cursor)) {
	elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
	if ((argc == 3) || Tcl_StringMatch(elemPtr->id, argv[3])) {
	    Tcl_AppendElement(interp, elemPtr->id);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ResetDisplayList --
 *
 *	Given a Tcl list of element names, this procedure rebuilds the
 *	display list, ignoring invalid element names. This list describes
 *	not only only which elements to draw, but in what order.  This is
 *	only important for bar and pie charts.
 *
 * Results:
 *	The return value is a standard Tcl result.  Only if the Tcl list
 *	cannot be split, a TCL_ERROR is returned and interp->result contains
 *	an error message.
 *
 * Side effects:
 *	The graph is eventually redrawn using the new display list.
 *
 *----------------------------------------------------------------------
 */
static int
ResetDisplayList(graphPtr, newList)
    Graph *graphPtr;		/* Graph widget record */
    char *newList;		/* Tcl list of element names */
{
    int numNames;		/* Number of names found in Tcl name list */
    char **nameArr;		/* Broken out array of element names */
    Blt_ListEntry *listPtr;
    Tcl_HashEntry *hashPtr;
    Tcl_HashSearch cursor;
    register int count;
    Element *elemPtr;		/* Element information record */

    if (Tcl_SplitList(graphPtr->interp, newList, &numNames,
	    &nameArr) != TCL_OK) {
	Tcl_AppendResult(graphPtr->interp, "can't split name list \"", newList,
	    "\"", (char *)NULL);
	return TCL_ERROR;
    }
    /* Clear the display list and mark all elements unmapped. */
    Blt_ClearList(&(graphPtr->elemList));
    for (hashPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	hashPtr != NULL; hashPtr = Tcl_NextHashEntry(&cursor)) {
	elemPtr = (Element *)Tcl_GetHashValue(hashPtr);
	elemPtr->mapped = 0;
    }

    /*
     * Rebuild the display list, checking that each name it exists
     * (we're currently ignoring invalid element names).
     */
    for (count = 0; count < numNames; count++) {
	hashPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), nameArr[count]);
	if (hashPtr != NULL) {
	    elemPtr = (Element *)Tcl_GetHashValue(hashPtr);
	    elemPtr->mapped = 1;
	    listPtr = Blt_CreateListEntry(elemPtr->id);
	    if (listPtr == NULL) {
		free((char *)nameArr);
		return TCL_ERROR;
	    }
	    Blt_SetListValue(listPtr, elemPtr);
	    Blt_LinkListAfter(&(graphPtr->elemList), listPtr,
		(Blt_ListEntry *)NULL);
	}
    }
    free((char *)nameArr);
    graphPtr->flags |= (DIRTY | LAYOUT_ALL);
    Blt_ComputeAxes(graphPtr);
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ShowElements --
 *
 *	Displays or rebuilds the element display list.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *	interp->result contains the list of element names.
 *
 *----------------------------------------------------------------------
 */
static int
ShowElements(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register Element *elemPtr;
    register Blt_ListEntry *entryPtr;

    if ((argc < 3) || (argc > 4)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element show ?nameList?\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (argc == 4) {
	ResetDisplayList(graphPtr, argv[3]);
    }
    for (entryPtr = Blt_FirstListEntry(&(graphPtr->elemList));
	entryPtr != NULL; entryPtr = Blt_NextListEntry(entryPtr)) {
	elemPtr = (Element *)Blt_GetListValue(entryPtr);
	Tcl_AppendElement(interp, (char *)elemPtr->id);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ClosestElement --
 *
 *	Find the element closest to the window coordinates specified.
 *
 * Results:
 *	Returns TCL_OK if no errors occured. The result field of the
 *	interpreter may contain a list containing the element name,
 *	the index of the closest point, and the x and y graph coordinates
 *	of the point is stored.  If an error occurred, returns TCL_ERROR
 *	and an error message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */
static int
ClosestElement(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;
    int argc;			/* Number of element names */
    char **argv;		/* List of element names */
{
    register Element *elemPtr;
    int x, y;
    ClosestPoint min, elem;

    if (argc < 5) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element closest winX winY ?elements...?\"", (char *)NULL);
	return TCL_ERROR;
    }
    if ((Tk_GetPixels(interp, graphPtr->tkwin, argv[3], &x) != TCL_OK) ||
	(Tk_GetPixels(interp, graphPtr->tkwin, argv[4], &y) != TCL_OK)) {
	Tcl_AppendResult(interp, ": bad window coordinates", (char *)NULL);
	return TCL_ERROR;
    }
    min.dist = Blt_posInfinity;

    if (argc > 5) {
	Tcl_HashEntry *entryPtr;
	register int i;

	/*
	 * Search for closest point in set of elements listed. In case
	 * of two or more elements have the same closest point, pick
	 * the first element.
	 */
	for (i = 5; i < argc; i++) {	/* Element name list supplied */
	    entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[i]);
	    if (entryPtr == NULL) {
		Tcl_AppendResult(interp, "can't find element \"", argv[i],
		    "\" in \"", argv[0], "\"", (char *)NULL);
		return TCL_ERROR;
	    }
	    elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
	    if (!elemPtr->mapped) {
		Tcl_AppendResult(interp, "element \"", argv[i],
		    "\" isn't mapped", (char *)NULL);
		return TCL_ERROR;
	    }
	    if ((*elemPtr->closestProc) (graphPtr, elemPtr, x, y, &elem)) {
		if (elem.dist < min.dist) {
		    min = elem;
		}
	    }
	}
    } else {
	Blt_ListEntry *entryPtr;
	/*
	 * Find the closest point in set of displayed elements,
	 * searching the display list from back to front.  It's
	 * possible that two or more element's may include the same
	 * closest data point.  In these cases, we pick the one drawn
	 * last. BTW. It's ok if an element itself has more than one
	 * point which is the closest.  Active elements are drawn
	 * after the normal elements.
	 */
	for (entryPtr = Blt_LastListEntry(&(graphPtr->elemList));
	    entryPtr != NULL; entryPtr = Blt_PrevListEntry(entryPtr)) {
	    elemPtr = (Element *)Blt_GetListValue(entryPtr);
	    if ((*elemPtr->closestProc) (graphPtr, elemPtr, x, y, &elem)) {
		if (elem.dist < min.dist) {
		    min = elem;
		}
	    }
	}
    }

    if (min.dist != Blt_posInfinity) {
	char string[TCL_DOUBLE_SPACE + 1];

	Tcl_AppendElement(interp, min.elemPtr->id);
	sprintf(string, "%d", min.index);
	Tcl_AppendElement(interp, string);
	Tcl_PrintDouble(interp, min.x, string);
	Tcl_AppendElement(interp, string);
	Tcl_PrintDouble(interp, min.y, string);
	Tcl_AppendElement(interp, string);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ActivateElement --
 *
 *	Find the element closest to the window coordinates specified.
 *
 * Results:
 *	Returns TCL_OK if no errors occured. The result field of the
 *	interpreter may contain a list containing the element name,
 *	the index of the closest point, and the x and y graph coordinates
 *	of the point is stored.  If an error occurred, returns TCL_ERROR
 *	and an error message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */

static int
ActivateElement(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;
    int argc;			/* Number of element names */
    char **argv;		/* List of element names */
{
    Element *elemPtr;
    Tcl_HashEntry *entryPtr;
    int numActive;
    int *indexArr;

    if (argc < 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element ", argv[2], " name ?index...?\"", (char *)NULL);
	return TCL_ERROR;
    }
    entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[3]);
    if (entryPtr == NULL) {
	Tcl_AppendResult(interp, "can't find element \"", argv[3], "\" in \"",
	    argv[0], "\"", (char *)NULL);
	return TCL_ERROR;
    }
    elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
    elemPtr->flags |= ACTIVE;
    numActive = argc - 4;
    indexArr = elemPtr->staticArr;
    if (numActive > 0) {
	register int i;
	int index;

	if (argc > DEF_ACTIVE_SIZE) {
	    indexArr = (int *)malloc(sizeof(int) * numActive);
	}
	if (indexArr == NULL) {
	    interp->result = "can't allocate index array";
	    return TCL_ERROR;
	}
	argv += 4;
	for (i = 0; i < numActive; i++) {
	    if (Tcl_GetInt(interp, argv[i], &index) != TCL_OK) {
		free((char *)indexArr);
		return TCL_ERROR;
	    }
	    indexArr[i] = index;
	}
    }
    elemPtr->numActivePoints = numActive;
    if (elemPtr->activeArr != elemPtr->staticArr) {
	free((char *)elemPtr->activeArr);
    }
    elemPtr->activeArr = indexArr;
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DeactivateElement --
 *
 *	Clears the active bit for the named elements.
 *
 * Results:
 *	Returns TCL_OK if no errors occured.
 *
 *----------------------------------------------------------------------
 */

static int
DeactivateElement(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget */
    Tcl_Interp *interp;
    int argc;			/* Number of element names */
    char **argv;		/* List of element names */
{
    Element *elemPtr;
    Tcl_HashEntry *entryPtr;
    register int i;

    if (argc < 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " element deactivate name ?name...?\"", (char *)NULL);
	return TCL_ERROR;
    }
    for (i = 3; i < argc; i++) {
	entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), argv[3]);
	if (entryPtr == NULL) {
	    Tcl_AppendResult(interp, "can't find element \"", argv[3],
		"\" in \"", argv[0], "\"", (char *)NULL);
	    return TCL_ERROR;
	}
	elemPtr = (Element *)Tcl_GetHashValue(entryPtr);
	elemPtr->flags &= ~ACTIVE;
	elemPtr->numActivePoints = 0;
	if (elemPtr->activeArr != elemPtr->staticArr) {
	    free((char *)elemPtr->activeArr);
	    elemPtr->activeArr = elemPtr->staticArr;
	}
    }
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 * Global routines:
 */

/*
 *--------------------------------------------------------------
 *
 * Blt_ElementCmd --
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
/* ARGSUSED */
int
Blt_ElementCmd(graphPtr, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    int argc;			/* # arguments */
    char **argv;		/* Argument list */
{
    int result = TCL_ERROR;
    char c;
    int length;
    Tcl_Interp *interp = graphPtr->interp;

    if (argc < 3) {
	Tcl_AppendResult(graphPtr->interp, "wrong # args: should be \"",
	    argv[0], " element option ?args?\"", NULL);
	return TCL_ERROR;
    }
    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 'c') && (length > 1) &&
	(strncmp(argv[2], "create", length) == 0)) {
	result = CreateElement(graphPtr, interp, argc, argv);
    } else if ((c == 'c') && (length > 1) &&
	(strncmp(argv[2], "configure", length) == 0)) {
	result = ConfigureElement(graphPtr, interp, argc, argv);
    } else if ((c == 'c') && (length > 1) &&
	(strncmp(argv[2], "closest", length) == 0)) {
	result = ClosestElement(graphPtr, interp, argc, argv);
    } else if ((c == 'a') && (length > 1) &&
	(strncmp(argv[2], "append", length) == 0)) {
	result = AppendElement(graphPtr, interp, argc, argv);
    } else if ((c == 'a') && (length > 1) &&
	(strncmp(argv[2], "activate", length) == 0)) {
	result = ActivateElement(graphPtr, interp, argc, argv);
    } else if ((c == 'd') && (length > 2) &&
	(strncmp(argv[2], "deactivate", length) == 0)) {
	result = DeactivateElement(graphPtr, interp, argc, argv);
    } else if ((c == 'd') && (length > 2) &&
	(strncmp(argv[2], "delete", length) == 0)) {
	result = DeleteElements(graphPtr, interp, argc, argv);
    } else if ((c == 'n') && (strncmp(argv[2], "names", length) == 0)) {
	result = ElementNames(graphPtr, interp, argc, argv);
    } else if ((c == 's') && (strncmp(argv[2], "show", length) == 0)) {
	result = ShowElements(graphPtr, interp, argc, argv);
    } else {
	char *options;

	options = " activate, append, closest, configure, create, \
deactivate, delete, names, or show";
	Tcl_AppendResult(graphPtr->interp, "bad element option \"", argv[2],
	    "\": should be ", options, (char *)NULL);
	return TCL_ERROR;
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * UpdateVector --
 *
 *	Called by C interface routine Blt_GraphElement.
 *
 * Results:
 *	The return value is a standard Tcl result.  The vector is passed
 *	back via the vecPtr.
 *
 *----------------------------------------------------------------------
 */
static int
UpdateVector(graphPtr, vecPtr, start, numValues, valueArr)
    Graph *graphPtr;		/* Graph widget */
    Vector *vecPtr;		/* Element vector */
    int start;			/* Starting value */
    int numValues;		/* Number of elements in array */
    double *valueArr;		/* Array of floating point values */
{
    unsigned int arraySize;
    double *newArr;
    register int offset;
    register int i;
    int needed;

    offset = vecPtr->length;
    needed = (numValues / 2) + offset;

    /*
     * Keep doubling the size of the array until we fit the number
     * needed
     */
    arraySize = vecPtr->size;
    if (arraySize == 0) {
	arraySize = DEF_VECTOR_SIZE;
    }
    while (needed > arraySize) {
	arraySize += arraySize;
    }
    newArr = (double *)malloc(arraySize * sizeof(double));
    if (newArr == NULL) {
	graphPtr->interp->result = "can't allocate data vector";
	return TCL_ERROR;
    }
    if (offset > 0) {
	memcpy((char *)newArr, (char *)vecPtr->data, offset * sizeof(double));
    }
    /*
     * Pick out every other value and stuff it into the extended
     * array.
     */
    for (i = start; i < numValues; i += 2) {
	newArr[offset++] = valueArr[i];
    }
    if (vecPtr->data != NULL) {
	free((char *)vecPtr->data);
    }
    vecPtr->data = newArr;
    vecPtr->length = needed;
    vecPtr->size = arraySize;
    GetDataLimits(vecPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GraphElement --
 *
 *	User convenience routine to set data of an individual graph
 *	element.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *      Graph is redrawn with new data values. Axes are recalculated.
 *
 *----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
int
Blt_GraphElement(interp, pathName, elemName, numValues, valueArr)
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    char *pathName;		/* Path name of graph widget */
    char *elemName;		/* Name of element to set */
    int numValues;		/* Number of coordinates in array */
    double *valueArr;		/* Array of XY coordinate values */
{
    Tcl_HashEntry *entryPtr;
    Graph *graphPtr;
    Element *elemPtr;
    Tk_Window mainWin, tkwin;
    Tk_Uid classUid;
    ClientData clientData;

    mainWin = Tk_MainWindow(interp);
    if (mainWin == NULL) {
	return TCL_ERROR;
    }
    tkwin = Tk_NameToWindow(interp, pathName, mainWin);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    classUid = Tk_Class(tkwin);
    if (classUid != Tk_GetUid("Blt_graph")) {
	Tcl_AppendResult(interp, "window \"", pathName,
	    "\" is the wrong class \"", classUid, "\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (Blt_FindCmd(interp, pathName, &clientData) != TCL_OK) {
	Tcl_AppendResult(interp, "can't find command \"", pathName,
	    "\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (numValues <= 2) {
	Tcl_AppendResult(interp, "too few values in array", (char *)NULL);
	return TCL_ERROR;
    }
    if (numValues & 1) {
	Tcl_AppendResult(interp, "odd number of values in array", (char *)NULL);
	return TCL_ERROR;
    }
    graphPtr = (Graph *)clientData;
    entryPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), elemName);
    if (entryPtr == NULL) {
	Tcl_AppendResult(interp, "can't find element \"", elemName, "\" in \"",
	    pathName, "\"", (char *)NULL);
	return TCL_ERROR;
    }
    elemPtr = (Element *)Tcl_GetHashValue(entryPtr);

    /* Reset element data values */
    elemPtr->x.length = elemPtr->y.length = 0;
    UpdateVector(graphPtr, &(elemPtr->x), 0, numValues, valueArr);
    UpdateVector(graphPtr, &(elemPtr->y), 1, numValues, valueArr);

    /* Indicate element layout needs to be recalculated. */
    elemPtr->flags |= LAYOUT_NEEDED;
    graphPtr->flags |= DIRTY;
    Blt_ComputeAxes(graphPtr);
    Blt_RedrawGraph(graphPtr);
    return TCL_OK;
}
