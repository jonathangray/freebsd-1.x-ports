
/*
 * bltGrElem.h --
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

#ifndef _ELEMENT_H
#define _ELEMENT_H

#define DEF_ACTIVE_SIZE 8	/* Default size of the static active
				 * array.  Contains indices of the
				 * active data points within an element */


typedef enum {
    ELEM_NORMAL, ELEM_ACTIVE
} ElementActiveStates;

/*
 * An element can be a line or a bar.
 */
typedef enum {
    LINE_ELEM_TYPE, BAR_ELEM_TYPE
} ElementClassType;

/*
 * It can have one of the following types of symbol.
 */
typedef enum {
    LINE_SYMBOL, SQUARE_SYMBOL, CIRCLE_SYMBOL, DIAMOND_SYMBOL,
    PLUS_SYMBOL, CROSS_SYMBOL, SPLUS_SYMBOL, SCROSS_SYMBOL
} SymbolType;

/*
 * The data structure below contains information pertaining to a line
 * vector.  It consists of an array of floating point data values and
 * for convenience, the number and minimum/maximum values.
 */
typedef struct Vector {
    double *data;		/* Array of values */
    unsigned int length;	/* Number of entries in the array */
    unsigned int size;		/* Size of data array allocated */
    double min, max;		/* Smallest/largest values in the array */
    double logMin;		/* Smallest positive value */
} Vector;

/*
 * An element has one or more vectors plus several attributes,
 * such as line style, thickness, color, and symbol type.  It
 * has an identifier which distinguishes it among the list of all
 * elements.
 */
typedef struct Element Element;
typedef struct ClosestPoint ClosestPoint;

typedef void (ElemDisplayProc) _ANSI_ARGS_((Graph *graphPtr, Element *elemPtr,
	int active));
typedef void (ElemPrintProc) _ANSI_ARGS_((Graph *graphPtr, Element *elemPtr,
	int active));
typedef void (ElemDestroyProc) _ANSI_ARGS_((Graph *graphPtr,
	Element *elemPtr));
typedef int (ElemConfigProc) _ANSI_ARGS_((Graph *graphPtr, Element *elemPtr));
typedef void (ElemLayoutProc) _ANSI_ARGS_((Graph *graphPtr, Element *elemPtr));
typedef int (ElemLimitsProc) _ANSI_ARGS_((Graph *graphPtr, Element *elemPtr,
	GraphAxis *axisPtr, double *minPtr, double *maxPtr));
typedef int (ElemDistanceProc) _ANSI_ARGS_((Graph *graphPtr,
	Element *elemPtr, int x, int y, ClosestPoint * closePtr));
typedef void (ElemDrawSymbolsProc) _ANSI_ARGS_((Graph *graphPtr,
	Element *elemPtr, int symbolSize, XPoint *pointArr, int numPoints,
	int active));
typedef void (ElemPrintSymbolsProc) _ANSI_ARGS_((Graph *graphPtr,
	Element *elemPtr, int symbolSize, XPoint *pointArr, int numPoints,
	int active));

struct Element {
    Tcl_Interp *interp;		/* Interpreter of graph widget */
    ElementClassType type;	/* Type of element; either BAR_ELEMENT or
				 * LINE_ELEMENT */
    unsigned int flags;
    Tk_Uid id;			/* Identifier to refer the element. Used in
				 * the "insert", "delete", or "show",
				 * commands. */
    int mapped;			/* If true, element is currently visible. */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    char *label;		/* Label displayed in legend */
    SymbolType symbol;		/* Element symbol type */
    double symbolScale;		/* Size of symbol as a percentage of the
				 * drawing area. */
    unsigned int symbolSize;	/* Computed size of symbol in pixels. */

    Vector x, y;		/* Contains array of floating point graph
				 * coordinate values. Also holds min/max and
				 * the number of coordinates */
    unsigned int axisFlags;	/* Indicates which axes to map element's
				 * coordinates onto */
    int *activeArr;		/* Array of indices of active data
				 * points (malloc-ed). Initially points
				 * to static storage "staticArr". */
    int staticArr[DEF_ACTIVE_SIZE];
    int numActivePoints;	/* Number of active data points. If
				 * zero and active bit is set in
				 * "flags", then all data points are
				 * active. */

    ElemConfigProc *configProc;
    ElemDestroyProc *destroyProc;
    ElemDisplayProc *displayProc;
    ElemLimitsProc *limitsProc;
    ElemDistanceProc *closestProc;
    ElemLayoutProc *layoutProc;
    ElemPrintProc *printProc;
    ElemDrawSymbolsProc *drawSymbolsProc;
    ElemPrintSymbolsProc *printSymbolsProc;
};

#define	LAYOUT_NEEDED 	(1<<0)	/* Indicates that the element's
				 * configuration has changed such that
				 * its layout of the element (i.e. its
				 * position in the graph window) needs
				 * to be recalculated. */

#define	ACTIVE 		(1<<8)	/* Non-zero indicates that the element
				 * should be drawn in its active
				 * foreground and background
				 * colors. */

#define	LABEL_ACTIVE 	(1<<9)	/* Non-zero indicates that the
				 * element's entry in the legend
				 * should be drawn in its active
				 * foreground and background
				 * colors. */
struct ClosestPoint {
    Element *elemPtr;		/* Closest element */
    int index;			/* Index of closest data point */
    double x, y;		/* Graph coordinates of closest point */
    double dist;		/* Distance from the screen coordinates */
};

#endif /* _ELEMENT_H */
