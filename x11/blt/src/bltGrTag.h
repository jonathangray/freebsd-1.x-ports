
/*
 * bltGrTag.h --
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
 * Tag types:
 *
 * 	A tag can be either a text, bitmap, connected line, window,
 *	or polygon.
 */

typedef struct Tag Tag;

typedef enum {
    UNKNOWN_TAG_TYPE = (-1),
    TEXT_TAG_TYPE, LINE_TAG_TYPE, POLYGON_TAG_TYPE,
    BITMAP_TAG_TYPE, WINDOW_TAG_TYPE
} TagClassType;

/*
 * This structure corresponds with the specific types of tags.
 * Don't change this structure without changing the individual
 * tag structures of each type in Tag.c.
 */

typedef void (DisplayTagProc) _ANSI_ARGS_((Graph *, Tag *));
typedef void (DestroyTagProc) _ANSI_ARGS_((Graph *, Tag *));
typedef int (ConfigTagProc) _ANSI_ARGS_((Graph *, Tag *));
typedef void (LayoutTagProc) _ANSI_ARGS_((Graph *, Tag *));
typedef void (PrintTagProc) _ANSI_ARGS_((Graph *, Tag *));
typedef char *(TypeOfTagProc) _ANSI_ARGS_((Tag *));

struct Tag {
    TagClassType type;		/* Type of tag */
    unsigned int flags;
    Tk_Uid id;			/* Identifier for tag in list */
    double *coordArr;		/* Coordinate array to position tag */
    int numCoords;		/* Number of points in above array */
    Tk_ConfigSpec *configSpecs;	/* Tag configuration specifications */
    Tk_Uid elemId;		/* Element associated with tag */
    unsigned int axisFlags;	/* Indicates which axis to map element's
				 * coordinates onto */

    DisplayTagProc *displayProc;
    DestroyTagProc *destroyProc;
    ConfigTagProc *configProc;
    LayoutTagProc *layoutProc;
    PrintTagProc *printProc;
    TypeOfTagProc *typeProc;
};
