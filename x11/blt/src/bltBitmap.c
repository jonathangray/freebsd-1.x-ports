
/*
 * bltBitmap.c --
 *
 *	This module implements Tcl bitmaps for the Tk toolkit.
 *
 *	Much of the code is taken from XRdBitF.c and XWrBitF.c
 *	from the MIT X11R5 distribution.
 *
 * Copyright, 1987, Massachusetts Institute of Technology
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
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
 * "blt_bitmap" command created by George Howlett.
 */

#include "blt.h"
#include <ctype.h>
#include <X11/Xutil.h>

#ifndef BITMAP_VERSION
#define BITMAP_VERSION "1.1"
#endif

#define MAX_SIZE 255
enum Formats {
    UNKNOWN, V10, V11
};

typedef struct {
    double theta;		/* Rotation of text string */
    XFontStruct *fontPtr;	/* Font pointer */

} BitmapInfo;

#define DEF_BITMAP_FONT		"*-Helvetica-Bold-R-Normal-*-14-*"
#define DEF_BITMAP_ROTATE	"0.0"

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_FONT, "-font", (char *)NULL, (char *)NULL,
	DEF_BITMAP_FONT, Tk_Offset(BitmapInfo, fontPtr), 0},
    {TK_CONFIG_DOUBLE, "-rotate", (char *)NULL, (char *)NULL,
	DEF_BITMAP_ROTATE, Tk_Offset(BitmapInfo, theta),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/* Shared data for the image read/parse logic */
static short hexTable[256];	/* conversion value */
static int initialized = 0;	/* easier to fill in at run time */

#define blt_width 40
#define blt_height 40
static unsigned char blt_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0xff, 0xff, 0x03, 0x00, 0x04,
    0x00, 0x00, 0x02, 0x00, 0x04, 0x00, 0x00, 0x02, 0x00, 0xe4, 0x33, 0x3f,
    0x01, 0x00, 0x64, 0x36, 0x0c, 0x01, 0x00, 0x64, 0x36, 0x8c, 0x00, 0x00,
    0xe4, 0x33, 0x8c, 0x00, 0x00, 0x64, 0x36, 0x8c, 0x00, 0x00, 0x64, 0x36,
    0x0c, 0x01, 0x00, 0xe4, 0xf3, 0x0d, 0x01, 0x00, 0x04, 0x00, 0x00, 0x02,
    0x00, 0x04, 0x00, 0x00, 0x02, 0x00, 0xfc, 0xff, 0xff, 0x03, 0x00, 0x0c,
    0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x0c, 0xf8, 0xff,
    0x03, 0x80, 0xed, 0x07, 0x00, 0x04, 0xe0, 0x0c, 0x00, 0x20, 0x09, 0x10,
    0x0c, 0x00, 0x00, 0x12, 0x10, 0x0c, 0x00, 0x00, 0x10, 0x30, 0x00, 0x00,
    0x00, 0x19, 0xd0, 0x03, 0x00, 0x00, 0x14, 0xb0, 0xfe, 0xff, 0xff, 0x1b,
    0x50, 0x55, 0x55, 0x55, 0x0d, 0xe8, 0xaa, 0xaa, 0xaa, 0x16, 0xe4, 0xff,
    0xff, 0xff, 0x2f, 0xf4, 0xff, 0xff, 0xff, 0x27, 0xd8, 0xae, 0xaa, 0xbd,
    0x2d, 0x6c, 0x5f, 0xd5, 0x67, 0x1b, 0xbc, 0xf3, 0x7f, 0xd0, 0x36, 0xf8,
    0x01, 0x10, 0xcc, 0x1f, 0xe0, 0x45, 0x8e, 0x92, 0x0f, 0xb0, 0x32, 0x41,
    0x43, 0x0b, 0xd0, 0xcf, 0x3c, 0x7c, 0x0d, 0xb0, 0xaa, 0xc2, 0xab, 0x0a,
    0x60, 0x55, 0x55, 0x55, 0x05, 0xc0, 0xff, 0xab, 0xaa, 0x03, 0x00, 0x00,
    0xfe, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define bigblt_width 64
#define bigblt_height 64
static unsigned char bigblt_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0xff, 0xff, 0xff, 0x3f, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x10, 0x00,
    0x00, 0x00, 0xe2, 0x0f, 0xc7, 0xff, 0x10, 0x00, 0x00, 0x00, 0xe2, 0x1f,
    0xc7, 0xff, 0x10, 0x00, 0x00, 0x00, 0xe2, 0x38, 0x07, 0x1c, 0x08, 0x00,
    0x00, 0x00, 0xe2, 0x38, 0x07, 0x1c, 0x08, 0x00, 0x00, 0x00, 0xe2, 0x38,
    0x07, 0x1c, 0x08, 0x00, 0x00, 0x00, 0xe2, 0x1f, 0x07, 0x1c, 0x04, 0x00,
    0x00, 0x00, 0xe2, 0x1f, 0x07, 0x1c, 0x04, 0x00, 0x00, 0x00, 0xe2, 0x38,
    0x07, 0x1c, 0x08, 0x00, 0x00, 0x00, 0xe2, 0x38, 0x07, 0x1c, 0x08, 0x00,
    0x00, 0x00, 0xe2, 0x38, 0x07, 0x1c, 0x08, 0x00, 0x00, 0x00, 0xe2, 0x1f,
    0xff, 0x1c, 0x10, 0x00, 0x00, 0x00, 0xe2, 0x0f, 0xff, 0x1c, 0x10, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x20, 0x00,
    0x00, 0x00, 0xfe, 0xff, 0xff, 0xff, 0x3f, 0x00, 0x00, 0x00, 0x06, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0xc0, 0xff, 0xff, 0x07, 0x00,
    0x00, 0xe0, 0xf6, 0x3f, 0x00, 0x00, 0x38, 0x00, 0x00, 0x1c, 0x06, 0x00,
    0x00, 0x00, 0xc0, 0x00, 0x80, 0x03, 0x06, 0x00, 0x00, 0xc0, 0x08, 0x03,
    0x40, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x04, 0x40, 0x00, 0x06, 0x00,
    0x00, 0x00, 0x40, 0x04, 0x40, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x04,
    0x40, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x04, 0xc0, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x0c, 0x06, 0x40, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05,
    0xc0, 0xfe, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x06, 0x40, 0x55, 0xff, 0xff,
    0xff, 0xff, 0x7f, 0x05, 0x80, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0x06,
    0x80, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x03, 0x40, 0xab, 0xaa, 0xaa,
    0xaa, 0xaa, 0xaa, 0x01, 0x70, 0x57, 0x55, 0x55, 0x55, 0x55, 0xd5, 0x04,
    0x28, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x0b, 0xd8, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0x14, 0xd0, 0xf7, 0xff, 0xff, 0xff, 0xff, 0xff, 0x13,
    0xf0, 0xda, 0xbf, 0xaa, 0xba, 0xfd, 0xd6, 0x0b, 0x70, 0xed, 0x77, 0x55,
    0x57, 0xe5, 0xad, 0x07, 0xb8, 0xf7, 0xab, 0xaa, 0xaa, 0xd2, 0x5b, 0x0f,
    0xf8, 0xfb, 0x54, 0x55, 0x75, 0x94, 0xf7, 0x1e, 0xf0, 0x7b, 0xfa, 0xff,
    0x9f, 0xa9, 0xef, 0x1f, 0xc0, 0xbf, 0x00, 0x20, 0x40, 0x54, 0xfe, 0x0f,
    0x00, 0x1f, 0x92, 0x00, 0x04, 0xa9, 0xfc, 0x01, 0xc0, 0x5f, 0x41, 0xf9,
    0x04, 0x21, 0xfd, 0x00, 0xc0, 0x9b, 0x28, 0x04, 0xd8, 0x0a, 0x9a, 0x03,
    0x40, 0x5d, 0x08, 0x40, 0x44, 0x44, 0x62, 0x03, 0xc0, 0xaa, 0x67, 0xe2,
    0x03, 0x64, 0xba, 0x02, 0x40, 0x55, 0xd5, 0x55, 0xfd, 0xdb, 0x55, 0x03,
    0x80, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0x01, 0x00, 0x57, 0x55, 0x55,
    0x55, 0x55, 0xd5, 0x00, 0x00, 0xac, 0xaa, 0xaa, 0xaa, 0xaa, 0x2a, 0x00,
    0x00, 0xf0, 0xff, 0x57, 0x55, 0x55, 0x1d, 0x00, 0x00, 0x00, 0x00, 0xf8,
    0xff, 0xff, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

/* Forward declarations */
extern Pixmap Blt_CreateTextBitmap _ANSI_ARGS_((Display * display,
	Drawable draw, XFontStruct *fontPtr, char *textStr, double theta,
	unsigned int *bmWidthPtr, unsigned int *bmHeightPtr));

/*
 *--------------------------------------------------------------
 *
 * InitHexTable --
 *
 *	Table index for the hex values. Initialized once, first time.
 *	Used for translation value or delimiter significance lookup.
 *
 *	We build the table at run time for several reasons:
 *
 *     	  1.  portable to non-ASCII machines.
 *	  2.  still reentrant since we set the init flag after setting
 *            table.
 *        3.  easier to extend.
 *        4.  less prone to bugs.
 *
 * Results:
 *	None.
 *
 *--------------------------------------------------------------
 */
static void
InitHexTable()
{
    hexTable['0'] = 0;
    hexTable['1'] = 1;
    hexTable['2'] = 2;
    hexTable['3'] = 3;
    hexTable['4'] = 4;
    hexTable['5'] = 5;
    hexTable['6'] = 6;
    hexTable['7'] = 7;
    hexTable['8'] = 8;
    hexTable['9'] = 9;
    hexTable['A'] = 10;
    hexTable['B'] = 11;
    hexTable['C'] = 12;
    hexTable['D'] = 13;
    hexTable['E'] = 14;
    hexTable['F'] = 15;
    hexTable['a'] = 10;
    hexTable['b'] = 11;
    hexTable['c'] = 12;
    hexTable['d'] = 13;
    hexTable['e'] = 14;
    hexTable['f'] = 15;
}

/*
 *--------------------------------------------------------------
 *
 * GetHexValue --
 *
 *	Converts the hexadecimal string into an unsigned integer
 *	value.  The hexadecimal string need not have a leading "0x".
 *
 * Results:
 *	Returns a standard TCL result. If the conversion was
 *	successful, TCL_OK is returned, otherwise TCL_ERROR.
 *
 * Side Effects:
 * 	If the conversion fails, interp->result is filled with an
 *	error message.
 *
 *--------------------------------------------------------------
 */
static int
GetHexValue(interp, string, valuePtr)
    Tcl_Interp *interp;
    char *string;
    int *valuePtr;
{
    register int c;
    register char *s;
    register int value;

    s = string;
    if ((s[0] == '0') && ((s[1] == 'x') || (s[1] == 'X'))) {
	s += 2;
    }
    if (s[0] == '\0') {
	Tcl_AppendResult(interp, "expecting hex value: got \"", string, "\"",
	    (char *)NULL);
	return TCL_ERROR;	/* Only found "0x"  */
    }
    value = 0;
    for ( /*empty*/ ; *s != '\0'; s++) {
	/* trim high bits, check type and accumulate */
	c = *s & 0xff;
	if (!(isascii(c) && isxdigit(c))) {
	    Tcl_AppendResult(interp, "expecting hex value: got \"", string,
		"\"", (char *)NULL);
	    return TCL_ERROR;	/* Not a hexadecimal number */
	}
	value = (value << 4) + hexTable[c];
    }
    *valuePtr = value;
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * BitmapToSource --
 *
 *	Converts a bitmap into an array of source data.
 *
 * Results:
 *	Returns the number of bytes in an array of source data
 *	representing the bitmap.  Returns -1 if the array could not
 *	be allocated.
 *
 * Side Effects:
 *	Memory is allocated for source array. Caller must free
 *	array later.
 *
 *--------------------------------------------------------------
 */
static int
BitmapToSource(interp, tkwin, bitmap, width, height, dataPtrPtr)
    Tcl_Interp *interp;		/* Interpreter to report results to */
    Tk_Window tkwin;		/* Main window of interpreter */
    Pixmap bitmap;		/* Bitmap to be queried */
    unsigned int width, height;	/* Dimensions of the bitmap */
    unsigned char **dataPtrPtr;	/* Pointer to converted array of data */
{
    int value, bitMask;
    register int x, y;
    int count;
    int arraySize, bytes_per_line;
    XImage *imagePtr;
    unsigned char *dataPtr;

    /* then convert the bitmap to an image */
    imagePtr = XGetImage(Tk_Display(tkwin), bitmap, 0, 0, width, height, 1L,
	ZPixmap);

    /*
     * The slow but robust brute force method of converting the image:
     */
    bytes_per_line = (width + 7) / 8;
    arraySize = height * bytes_per_line;
    dataPtr = (unsigned char *)malloc(sizeof(unsigned char) * arraySize);
    if (dataPtr == NULL) {
	interp->result = "can't allocate array of source data";
	return -1;
    }
    count = 0;
    for (y = 0; y < height; y++) {
	value = 0, bitMask = 1;
	for (x = 0; x < width; /*empty*/ ) {
	    if (XGetPixel(imagePtr, x, y)) {
		value |= bitMask;
	    }
	    bitMask <<= 1;
	    x++;
	    if (!(x & 7)) {
		dataPtr[count++] = (unsigned char)value;
		value = 0, bitMask = 1;
	    }
	}
	if (x & 7) {
	    dataPtr[count++] = (unsigned char)value;
	}
    }
    XDestroyImage(imagePtr);

    *dataPtrPtr = dataPtr;
    return (count);
}

/*
 *--------------------------------------------------------------
 *
 * AsciiToSource --
 *
 *	Converts a Tcl list of ASCII values into a source array.
 *
 * Results:
 *	A standard TCL result.
 *
 * Side Effects:
 * 	If an error occurs while processing the data, interp->result
 *	is filled with a corresponding error message.
 *
 *--------------------------------------------------------------
 */
static int
AsciiToSource(interp, elemList, width, height, dataPtrPtr)
    Tcl_Interp *interp;		/* Interpreter to report results to */
    char *elemList;		/* List of of hex numbers representing
				 * bitmap source data */
    unsigned int width, height;	/* Height and width */
    unsigned char **dataPtrPtr;	/* source data array (output) */
{
    int arraySize;		/* Number of bytes of data */
    int value;			/* from an input line */
    int padding;		/* to handle alignment */
    int bytes_per_line;		/* per scanline of data */
    unsigned char *dataPtr;
    register int count;
    enum Formats format;
    register int i;		/*  */
    char **valueArr;
    int numValues;

    /* First time through initialize the ascii->hex translation table */
    if (!initialized) {
	InitHexTable();
	initialized = 1;
    }
    if (Tcl_SplitList(interp, elemList, &numValues, &valueArr) != TCL_OK) {
	return -1;
    }
    bytes_per_line = (width + 7) / 8;
    arraySize = bytes_per_line * height;
    if (numValues == arraySize) {
	format = V11;
    } else if (numValues == (arraySize / 2)) {
	format = V10;
    } else {
	Tcl_AppendResult(interp, "bitmap has wrong # of data values",
	    (char *)NULL);
	goto error;
    }
    padding = 0;
    if (format == V10) {
	padding = ((width % 16) && ((width % 16) < 9));
	if (padding) {
	    bytes_per_line = (width + 7) / 8 + padding;
	    arraySize = bytes_per_line * height;
	}
    }
    dataPtr = (unsigned char *)
	calloc(sizeof(unsigned char), (unsigned int)arraySize);
    if (dataPtr == NULL) {
	interp->result = "can't allocate memory for bitmap";
	goto error;
    }
    count = 0;
    for (i = 0; i < numValues; i++) {
	if (GetHexValue(interp, valueArr[i], &value) != TCL_OK) {
	    free((char *)dataPtr);
	    goto error;
	}
	dataPtr[count++] = (unsigned char)value;
	if (format == V10) {
	    if ((!padding) || (((i * 2) + 2) % bytes_per_line)) {
		dataPtr[count++] = value >> 8;
	    }
	}
    }
    free((char *)valueArr);
    *dataPtrPtr = dataPtr;
    return (count);
  error:
    free((char *)valueArr);
    return -1;
}

/*
 *--------------------------------------------------------------
 *
 * ShowBitmap --
 *
 *	Returns a list of hex values corresponding to the source
 *	bits of the bitmap given.
 *
 *	Converts the unsigned character value into a two character
 *	hexadecimal string.  A separator is also added, which may
 *	either a newline or space according the the number of bytes
 *	already output.
 *
 * Results:
 *	Returns TCL_ERROR is a source data array can't be generated
 *	from the bitmap (memory allocation failure), otherwise TCL_OK.
 *
 *--------------------------------------------------------------
 */
static int
ShowBitmap(interp, tkwin, bitmap)
    Tcl_Interp *interp;		/* Interpreter to report results to */
    Tk_Window tkwin;		/* Main window of interpreter */
    Pixmap bitmap;		/* Bitmap to be queried */
{
    unsigned char *dataPtr;
    char *separator;
    int arraySize;
    register int i;
    char string[200];
    unsigned int width, height;

    /* Get the dimensions of the bitmap */
    Tk_SizeOfBitmap(Tk_Display(tkwin), bitmap, &width, &height);
    arraySize = BitmapToSource(interp, tkwin, bitmap, width, height, &dataPtr);
    if (arraySize < 0) {
	return TCL_ERROR;
    }
#define BYTES_PER_OUTPUT_LINE 24
    for (i = 0; i < arraySize; i++) {
	separator = (i % BYTES_PER_OUTPUT_LINE) ? " " : "\n    ";
	sprintf(string, "%02x", dataPtr[i]);
	Tcl_AppendResult(interp, separator, string, 0);
    }
    free((char *)dataPtr);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ComposeBitmap --
 *
 *	Converts the text string into an internal bitmap.
 *
 *	There's a lot of extra (read unnecessary) work going on
 *	here, but I don't (right now) think that it matters much.
 *	The rotated bitmap (formerly an image) is converted back
 *	to an image just so we can convert it to source data for
 *	Tk_DefineBitmap.
 *
 * Results:
 *	A standard TCL result.
 *
 * Side Effects:
 * 	If an error occurs while processing the data, interp->result
 *	is filled with a corresponding error message.
 *
 *--------------------------------------------------------------
 */
static int
ComposeBitmap(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    unsigned int width, height;	/* dimensions of bitmap */
    Pixmap bitmap;		/* Text bitmap */
    unsigned char *dataPtr;	/* Copy of source array from bitmap */
    int arraySize;
    BitmapInfo info;		/* Text rotation and font information */

    if (argc < 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " compose name text ?flags?\"", (char *)NULL);
	return TCL_ERROR;
    }
    /* Initialize info and process flags */
    info.fontPtr = NULL;	/* Will be initialized by Tk_ConfigureWidget */
    info.theta = 0.0;		/* No rotation by default */
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc - 4, argv + 4,
	    (char *)&info, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    /* Need a window Id to create the bitmap.  Could use the root window. */
    Tk_MakeWindowExist(tkwin);
    bitmap = Blt_CreateTextBitmap(Tk_Display(tkwin), Tk_WindowId(tkwin),
	info.fontPtr, argv[3], info.theta, &width, &height);
    Tk_FreeOptions(configSpecs, (char *)&info, Tk_Display(tkwin), 0);

    /* Convert bitmap back to source data */
    arraySize = BitmapToSource(interp, tkwin, bitmap, width, height, &dataPtr);
    XFreePixmap(Tk_Display(tkwin), bitmap);
    if (arraySize < 0) {
	return TCL_ERROR;
    }
    /* Create the bitmap again, this time using Tk's bitmap facilities */
    if (Tk_DefineBitmap(interp, Tk_GetUid(argv[2]), (char *)dataPtr,
	    width, height) != TCL_OK) {
	free((char *)dataPtr);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * DefineBitmap --
 *
 *	Converts the dataList into an internal bitmap.
 *
 * Results:
 *	A standard TCL result.
 *
 * Side Effects:
 * 	If an error occurs while processing the data, interp->result
 *	is filled with a corresponding error message.
 *
 *--------------------------------------------------------------
 */
/* ARGSUSED */
static int
DefineBitmap(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    unsigned int width, height;	/* dimensions of bitmap */
    int w, h;			/* Dimensions from input */
    char **dimArr, **elemArr;
    int numDim, numElem;
    unsigned char *dataPtr;	/* working variable */
    register char *p;
    BitmapInfo info;		/* TBA: Text rotation and font information */

    if (argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " define name data\"", (char *)NULL);
	return TCL_ERROR;
    }
    /* Initialize info and process flags */

    dataPtr = NULL;
    dimArr = elemArr = NULL;
    if (Tcl_SplitList(interp, argv[3], &numElem, &elemArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numElem != 2) {
	Tcl_AppendResult(interp, "wrong # of bitmap data components: ",
	    "should be \"dimList bitsList\"", (char *)NULL);
	goto error;
    }
    if (Tcl_SplitList(interp, elemArr[0], &numDim, &dimArr) != TCL_OK) {
	goto error;
    }
    if (numDim != 2) {
	Tcl_AppendResult(interp, "wrong # of bitmap dimensions: ",
	    "should be \"width height\"", (char *)NULL);
	goto error;
    }
    if ((Tcl_GetInt(interp, dimArr[0], &w) != TCL_OK) ||
	(Tcl_GetInt(interp, dimArr[1], &h) != TCL_OK)) {
	goto error;
    }
    if ((w < 1) || (h < 1)) {
	Tcl_AppendResult(interp, "invalid bitmap dimensions \"", elemArr[0],
	    "\"", (char *)NULL);
	goto error;
    }
    width = (unsigned int)w;
    height = (unsigned int)h;
    free((char *)dimArr);
    dimArr = NULL;

    /* Convert commas to blank spaces */

    for (p = elemArr[1]; *p != '\0'; p++) {
	if (*p == ',') {
	    *p = ' ';
	}
    }
    if (AsciiToSource(interp, elemArr[1], width, height, &dataPtr) < 0) {
	goto error;
    }
    free((char *)elemArr);
    elemArr = NULL;

    /* If bitmap is to be rotated or scale, do it here */

    if (Tk_DefineBitmap(interp, Tk_GetUid(argv[2]), (char *)dataPtr,
	    width, height) != TCL_OK) {
	goto error;
    }
    return TCL_OK;

  error:
    if (dataPtr != NULL) {
	free((char *)dataPtr);
    }
    if (dimArr != NULL) {
	free((char *)dimArr);
    }
    if (elemArr != NULL) {
	free((char *)elemArr);
    }
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * BitmapExists --
 *
 *	Indicates if the named bitmap exists.
 *
 *--------------------------------------------------------------
 */
static int
BitmapExists(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    Pixmap bitmap;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " exists name\"", (char *)NULL);
	return TCL_ERROR;
    }
    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    Tcl_ResetResult(interp);
    if (bitmap == None) {
	interp->result = "0";
    } else {
	interp->result = "1";
	Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * BitmapHeight --
 *
 *	Returns the height of the named bitmap.
 *
 *--------------------------------------------------------------
 */
static int
BitmapHeight(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    unsigned int width, height;
    Pixmap bitmap;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " height name\"", (char *)NULL);
	return TCL_ERROR;
    }
    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    if (bitmap == None) {
	return TCL_ERROR;
    }
    Tk_SizeOfBitmap(Tk_Display(tkwin), bitmap, &width, &height);
    sprintf(interp->result, "%d", height);
    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * BitmapWidth --
 *
 *	Returns the width of the named bitmap.
 *
 *--------------------------------------------------------------
 */
static int
BitmapWidth(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    unsigned int width, height;
    Pixmap bitmap;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " width name\"", (char *)NULL);
	return TCL_ERROR;
    }
    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    if (bitmap == None) {
	return TCL_ERROR;
    }
    Tk_SizeOfBitmap(Tk_Display(tkwin), bitmap, &width, &height);
    sprintf(interp->result, "%d", width);
    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * BitmapBits --
 *
 *	Returns the source data (excluding width and height)
 *	of the named bitmap.
 *
 *--------------------------------------------------------------
 */
static int
BitmapBits(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    Pixmap bitmap;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " bits name\"", (char *)NULL);
	return TCL_ERROR;
    }
    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    if (bitmap == None) {
	return TCL_ERROR;
    }
    ShowBitmap(interp, tkwin, bitmap);
    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * BitmapData --
 *
 *	Returns the source data (including width and height)
 *	of the named bitmap.
 *
 *--------------------------------------------------------------
 */
static int
BitmapData(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    Pixmap bitmap;
    char string[200];
    unsigned int width, height;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " data name\"", (char *)NULL);
	return TCL_ERROR;
    }
    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    if (bitmap == None) {
	return TCL_ERROR;
    }
    Tk_SizeOfBitmap(Tk_Display(tkwin), bitmap, &width, &height);
    sprintf(string, "%d %d", width, height);
    Tcl_AppendResult(interp, "  {", string, "} {", (char *)NULL);
    ShowBitmap(interp, tkwin, bitmap);
    Tcl_AppendResult(interp, "\n  }", (char *)NULL);
    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * BitmapCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to bitmaps managed by this module.
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
BitmapCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;
    char **argv;
{
    Tk_Window tkwin = (Tk_Window)clientData;
    int length;
    char c;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " option name ?args?\"", (char *)NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'd') && (length > 1) &&
	(strncmp(argv[1], "define", length) == 0)) {
	return (DefineBitmap(tkwin, interp, argc, argv));
    } else if ((c == 'e') && (strncmp(argv[1], "exists", length) == 0)) {
	return (BitmapExists(tkwin, interp, argc, argv));
    } else if ((c == 'w') && (strncmp(argv[1], "width", length) == 0)) {
	return (BitmapWidth(tkwin, interp, argc, argv));
    } else if ((c == 'h') && (strncmp(argv[1], "height", length) == 0)) {
	return (BitmapHeight(tkwin, interp, argc, argv));
    } else if ((c == 'b') && (strncmp(argv[1], "bits", length) == 0)) {
	return (BitmapBits(tkwin, interp, argc, argv));
    } else if ((c == 'd') && (length > 1) &&
	(strncmp(argv[1], "data", length) == 0)) {
	return (BitmapData(tkwin, interp, argc, argv));
    } else if ((c == 'c') && (strncmp(argv[1], "compose", length) == 0)) {
	return (ComposeBitmap(tkwin, interp, argc, argv));
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be ",
	    " define, exists, width, height, data, bits, or compose",
	    (char *)NULL);
	return TCL_ERROR;
    }
}

/*
 *--------------------------------------------------------------
 *
 * Blt_BitmapInit --
 *
 *	This procedure is invoked to initialize the bitmap command.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Adds the command to the interpreter and sets an array variable
 *	which its version number.
 *
 *--------------------------------------------------------------
 */
int
Blt_BitmapInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, "blt_bitmap", (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_bitmap\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_bitmap\" requires Tk", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", "blt_bitmap", BITMAP_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, "blt_bitmap", BitmapCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);

    /* Define the BLT logo bitmaps */

    Tk_DefineBitmap(interp, Tk_GetUid("BLT"), (char *)blt_bits,
	blt_width, blt_height);
    Tk_DefineBitmap(interp, Tk_GetUid("bigBLT"), (char *)bigblt_bits,
	bigblt_width, bigblt_height);
    return TCL_OK;
}
