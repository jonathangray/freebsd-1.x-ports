/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 *
 *****************************************************************************/

/*  signs.c
 *
 *  This file contains all the routines for the creation and manipulation of 
 *  symbols used in xdbx.  There are 3 different signs:
 *    arrow  - a solid right arrow to indicate the current execution point.
 *    updown - an outlined right arrow to indicate position in stack trace.
 *    stop   - a stop hand symbol to indicate a breakpoint is set.
 *    bomb  - a bomb symbol to indicate the point of segmentation fault.
 *
 *  To display a sign on a given line in the source window, it is first
 *  created and mapped.  To undisplay it, the sign is unmapped.  It can
 *  be mapped again when the sign is needed.  Note that the sign is never
 *  moved, so that there can be as many signs created (but not mapped) as
 *  the number of lines in the source window.
 *  For arrow and updown, there can be at most one of each mapped at a time.
 *  For stop, there can be more than one mapped at the same time.
 */

#include "global.h"
#include "bitmaps.h"

#define MAXSTOPS        256             /* max number of stops */
#define MAXSIGNS        256             /* max number of signs */
#define OFFSET	        2             	/* offset for displaying signs */

typedef struct {
    Widget      w;
    Boolean     mapped;
} ArrowSign;

typedef struct {
    Widget      w;
    Boolean     mapped;
} UpdownSign;

typedef struct {
    Widget      w;
    Boolean     mapped;
} StopSign;

typedef struct {
    Widget      w;
    Boolean     mapped;
} BombSign;

static ArrowSign 	arrowsign[MAXSIGNS];
static UpdownSign 	updownsign[MAXSIGNS];
static StopSign		stopsign[MAXSIGNS];
static BombSign 	bombsign[MAXSIGNS];

Arrow 		arrow;
Updown 		updown;
Stops		stops[MAXSTOPS];	/* array of stops */
Bomb 		bomb;
Cardinal	nstops;			/* number of stops */

/* Initialize data structures */

void signs_init()
{
    int i;

    for (i=0; i<MAXSIGNS; i++) {
	arrowsign[i].w = NULL;
	arrowsign[i].mapped = FALSE;
    }
    for (i=0; i<MAXSIGNS; i++) {
	stopsign[i].w = NULL;
	stopsign[i].mapped = FALSE;
    }
    arrow.i = 0;
    arrow.line = 0;
    strcpy(arrow.file, "");
    updown.i = 0;
    updown.line = 0;
    strcpy(updown.file, "");
    nstops = 0;
    bomb.i = 0;
    bomb.line = 0;
    strcpy(bomb.file, "");
}


/*  Create an arrow symbol, updown symbol or stop symbol:
 *    calculate the position of the symbol based on i, the number of lines
 *    from the top line.
 *    create the pixmap of the symbol
 *    display the symbol as a bitmap in a label widget.
 */
static Widget CreateSign(parent, sign, i)
    Widget	parent;
    char	*sign;
    Cardinal 	i;
{
    TextWidget 	ctx = (TextWidget) sourceWindow;
    Arg 	args[15];
    Cardinal 	n;
    Dimension 	source_height, height, width; 
    char	*bits;
    Pixel       fg, bg;
    int 	horizDistance, vertDistance, height_per_line;
    int         screen;
    Dimension	vbar_width = 0;
    Dimension	border_width = 0;

    if (displayedFile == NULL) return NULL;

    /* Get height and background pixel values of parent window */
    n = 0;
    XtSetArg(args[n], XtNheight, &source_height);			n++;
    XtSetArg(args[n], XtNbackground, &bg);				n++;
    XtGetValues(parent, args, n);

    height_per_line = source_height/displayedFile->lines;
    vertDistance = OFFSET + (i * height_per_line); 

    screen = DefaultScreen(display);

    if (sign && !strcmp(sign, "arrow")) {
	bits = arrow_bits;
	width = arrow_width;
	height = arrow_height;
	horizDistance = 0;
	fg = app_resources.arrow_color;
    }
    else if (sign && !strcmp(sign, "updown")) {
	bits = updown_bits;
	width = updown_width;
	height = updown_height;
	horizDistance = 0;
	fg = app_resources.updown_color;
    }
    else if (sign && !strcmp(sign, "stop")) {
	bits = stop_bits;
	width = stop_width;
	height = stop_height;
	horizDistance = arrow_width;
	fg = app_resources.stop_color;
    }
    else if (sign && !strcmp(sign, "bomb")) {
	bits = bomb_bits;
	width = bomb_width;
	height = bomb_height;
	horizDistance = 0;
	fg = app_resources.bomb_color;
    };

    if( ctx->text.vbar != NULL )
    {
	    n = 0;
	    XtSetArg(args[n], XtNwidth, &vbar_width); 			n++;
	    XtSetArg(args[n], XtNborderWidth, &border_width);		n++;
	    XtGetValues(ctx->text.vbar, args, n);
	    vbar_width += (border_width * 2);
    }
    
    n = 0;
    XtSetArg(args[n], XtNborderWidth, 0);				n++;
    XtSetArg(args[n], XtNwidth, (XtArgVal) width);			n++;
    XtSetArg(args[n], XtNheight, (XtArgVal) height);			n++;
    XtSetArg(args[n], XtNresize, (XtArgVal) False);			n++;
    XtSetArg(args[n], XtNmappedWhenManaged, (XtArgVal) False);		n++;
    XtSetArg(args[n], XtNbitmap, XCreatePixmapFromBitmapData (
        display, DefaultRootWindow(display), bits, width, height,
        fg, bg, DefaultDepth(display, screen)));			n++;

    XtSetArg(args[n], XtNfromVert, (XtArgVal) NULL);			n++;
    XtSetArg(args[n], XtNfromHoriz, (XtArgVal) NULL);			n++;
    XtSetArg(args[n], XtNhorizDistance, (XtArgVal) horizDistance+vbar_width);
									n++;
    XtSetArg(args[n], XtNvertDistance, (XtArgVal) vertDistance);	n++;
    XtSetArg(args[n], XtNtop, (XtArgVal) XawChainTop);			n++;
    XtSetArg(args[n], XtNleft, (XtArgVal) XawChainLeft);		n++;
    XtSetArg(args[n], XtNbottom, (XtArgVal) XawChainTop);		n++;
    XtSetArg(args[n], XtNright, (XtArgVal) XawChainLeft);		n++;

    return XtCreateManagedWidget(sign, labelWidgetClass, parent, args, n);
}

/*
 *  Given a line number, displays a stop sign if that line is viewable.
 *  If the stop widget for that line does not exist, create one and map it.
 *  If the stop widget exists but not mapped, map it.
 */
void DisplayStop(file, line)
FileRec *file;
int	line;
{
    Cardinal i;

    if (line >= file->topline && line <= file->bottomline) {
	i = line - file->topline;
	if (stopsign[i].w == NULL) {	/* widget does not exist */
	    stopsign[i].w = CreateSign(sourceForm, "stop", i);
	    XtMapWidget(stopsign[i].w);
	    stopsign[i].mapped = 1;
	}
	else if (!stopsign[i].mapped) { /* widget not mapped */
	    XtMapWidget(stopsign[i].w);
	    stopsign[i].mapped = 1;
	}
    }
}

/*
 *  Unmap all stop signs and then display only those stops that are viewable.
 */
void UpdateStops(file)
FileRec *file;
{
    Cardinal i;
    int	 line;

    if (file == NULL) return;
    for (i=0; i<file->lines; i++)
	if (stopsign[i].w && stopsign[i].mapped) {
	    XtUnmapWidget(stopsign[i].w);
	    stopsign[i].mapped = 0;
	}

    for (i=1; i<=nstops; i++)
	if (stops[i].file && !strcmp(stops[i].file, file->pathname) &&
	    (line=stops[i].line) && line >= file->topline &&
	    line <= file->bottomline) {
	    DisplayStop(file, line);
	}
}

/*
 * Given a line number, unmap the stop sign associated with that line.
 */
void RemoveStop(line)
int line;
{
    Cardinal i;

    if (displayedFile && line >= displayedFile->topline && 
			 line <= displayedFile->bottomline) {
	i = line - displayedFile->topline;
	if (stopsign[i].w && stopsign[i].mapped) {
	    XtUnmapWidget(stopsign[i].w);
	    stopsign[i].mapped = 0;
	}
    }
}

void ClearStops()
{
    int i;

    for (i=1; i<=nstops; i++) {
	stops[i].file = NULL;
	stops[i].line = 0;
    }
}

/*  Unmap the current arrow sign.
 *  Display a new arrow sign if it is viewable.
 */
void UpdateArrow(file)
FileRec *file;
{
    Cardinal i;
    int	     line;

    if (file == NULL) return;
    i = arrow.i;
    if (i>=0 && i<file->lines)
	if (arrowsign[i].w && arrowsign[i].mapped) {
	    XtUnmapWidget(arrowsign[i].w);
	    arrowsign[i].mapped = 0;
	}
    line = arrow.line;
    if (arrow.file && !strcmp(arrow.file, file->pathname) &&
    	line >= file->topline && line <= file->bottomline) {
        i = line - file->topline;
	arrow.i = i;
	if (arrowsign[i].w == NULL) {
	    arrowsign[i].w = CreateSign(sourceForm, "arrow", i);
	    XtMapWidget(arrowsign[i].w);
	    arrowsign[i].mapped = TRUE;
	}
	else if (!arrowsign[i].mapped) {
	    XtMapWidget(arrowsign[i].w);
	    arrowsign[i].mapped = TRUE;
	}
    }
}


/*  If the new updown is on the same line as the arrow, remove the updown.
 *  Unmap current updown sign.
 *  Display the updown if it is viewable.
 */
void UpdateUpdown(file)
FileRec *file;
{
    Cardinal i;
    int	     line;

    if (file == NULL) return;
    
/* (PW)9JULY91 : add test line else it prevents up/down when a function calls itself */
    if (updown.file && !strcmp(updown.file, arrow.file) && 
	!strcmp(updown.func, arrow.func) && (updown.line == arrow.line)) {
	updown.line = 0;
	strcpy(updown.file, "");
    }

    i = updown.i;
    if (i>=0 && i<file->lines)
	if (updownsign[i].w && updownsign[i].mapped) {
	    XtUnmapWidget(updownsign[i].w);
	    updownsign[i].mapped = 0;
	}
    line = updown.line;
    if (updown.file && !strcmp(updown.file, file->pathname) &&
    	line >= file->topline && line <= file->bottomline) {
        i = line - file->topline;
	updown.i = i;
	if (updownsign[i].w == NULL) {
	    updownsign[i].w = CreateSign(sourceForm, "updown", i);
	    XtMapWidget(updownsign[i].w);
	    updownsign[i].mapped = TRUE;
	}
	else if (!updownsign[i].mapped) {
	    XtMapWidget(updownsign[i].w);
	    updownsign[i].mapped = TRUE;
	}
    }
}

/*  Unmap the current bomb sign, if any.
 *  Display a new bomb sign.
 */
void UpdateBomb(file)
FileRec *file;
{
    Cardinal i;
    int	     line;

    if (file == NULL) return;
    i = bomb.i;
    if (i>=0 && i<file->lines)
	if (bombsign[i].w && bombsign[i].mapped) {
	    XtUnmapWidget(bombsign[i].w);
	    bombsign[i].mapped = 0;
	}
    line = bomb.line;
    if (bomb.file && !strcmp(bomb.file, file->pathname) &&
    	line >= file->topline && line <= file->bottomline) {
        i = line - file->topline;
	bomb.i = i;
	if (bombsign[i].w == NULL) {
	    bombsign[i].w = CreateSign(sourceForm, "bomb", i);
	    XtMapWidget(bombsign[i].w);
	    bombsign[i].mapped = TRUE;
	}
	else if (!bombsign[i].mapped) {
	    XtMapWidget(bombsign[i].w);
	    bombsign[i].mapped = TRUE;
	}
    }
}
