/*
 * Copyright 1993 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

/************************************************************************
 *                                                                      *
 * WARNING: This object assumes knowledge of its superclass' internals  *
 *                                                                      *
 ************************************************************************/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <AxeiiSinkP.h>

#include "blockCaret.xbm"

#define Offset(field) XtOffsetOf(AxeiiSinkRec, axeii_sink.field)

static XtResource resources[] = {
    {XtNblockCursor, XtCBlockCursor, XtRBoolean, sizeof(Boolean),
         Offset(block_cursor), XtRImmediate, (XtPointer) False},
    {XtNcursorBitmap, XtCBitmap, XtRString, sizeof(String),
         Offset(cursor_bitmap), XtRString, (XtPointer) NULL},
};

#undef Offset

#define PRIVATE(w,field) (((AxeiiSinkObject) w)->axeii_sink.field)

static void Initialize();
static void InsertCursor(), GetCursorBounds();

#define SuperClass		(&asciiSinkClassRec)
AxeiiSinkClassRec axeiiSinkClassRec = {
  {
/* core_class fields */	
    /* superclass	  	*/	(WidgetClass) SuperClass,
    /* class_name	  	*/	"AxeiiSink",
    /* widget_size	  	*/	sizeof(AxeiiSinkRec),
    /* class_initialize   	*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* obj1		  	*/	NULL,
    /* obj2		  	*/	NULL,
    /* obj3		  	*/	0,
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* obj4		  	*/	FALSE,
    /* obj5		  	*/	FALSE,
    /* obj6			*/	FALSE,
    /* obj7		  	*/	FALSE,
    /* destroy		  	*/	NULL,
    /* obj8		  	*/	NULL,
    /* obj9		  	*/	NULL,
    /* set_values	  	*/	NULL,
    /* set_values_hook		*/	NULL,
    /* obj10			*/	NULL,
    /* get_values_hook		*/	NULL,
    /* obj11		 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* obj12		   	*/	NULL,
    /* obj13			*/	NULL,
    /* obj14			*/	NULL,
    /* extension		*/	NULL
  },
/* text_sink_class fields */
  {
    /* DisplayText              */      XtInheritDisplayText,
    /* InsertCursor             */      InsertCursor,
    /* ClearToBackground        */      XtInheritClearToBackground,
    /* FindPosition             */      XtInheritFindPosition,
    /* FindDistance             */      XtInheritFindDistance,
    /* Resolve                  */      XtInheritResolve,
    /* MaxLines                 */      XtInheritMaxLines,
    /* MaxHeight                */      XtInheritMaxHeight,
    /* SetTabs                  */      XtInheritSetTabs,
    /* GetCursorBounds          */      GetCursorBounds
  },
/* ascii_sink_class fields */
  {
    /* unused			*/	0
  },
/* axeii_sink_class fields */
  {
    /* unused			*/	0
  }
};

WidgetClass axeiiSinkObjectClass = (WidgetClass)&axeiiSinkClassRec;

/* ARGSUSED */
static void
Initialize(request, new)
Widget request, new;
{
    AxeiiSinkObject sink = (AxeiiSinkObject) new;
    Display *display = XtDisplayOfObject(new);
    Pixmap pix = None;
    Window root;
    int x, y;
    unsigned int borderWidth, depth;

    if (PRIVATE(new,cursor_bitmap))
    {
	int junk;

	pix = XmuLocateBitmapFile(XDefaultScreenOfDisplay(display),
				  PRIVATE(new,cursor_bitmap),
				  (char *) 0, 0,
				  &junk, &junk, &junk, &junk);
    }
    else if (PRIVATE(new,block_cursor))
    {
	pix = XCreateBitmapFromData (display, XDefaultRootWindow(display),
				     (char *) blockCaret_bits,
				     blockCaret_width, blockCaret_height);
    }

    if (pix != None)
    {
	XFreePixmap(display, sink->ascii_sink.insertCursorOn);
	sink->ascii_sink.insertCursorOn = pix;
    }
    
    XGetGeometry(display, sink->ascii_sink.insertCursorOn,
		 &root, &x, &y, &PRIVATE(new,cursor_width),
		 &PRIVATE(new,cursor_height), &borderWidth, &depth);
}

/* $XConsortium: AsciiSink.c,v 1.57 91/07/21 20:35:00 converse Exp $ */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * The following are little-modified copies of the equivalent AsciiSink
 * functions, hence the inclusion of the above copyright notice.
 */

static void
GetCursorBounds(w, rect)
Widget w;
XRectangle * rect;
{
    AsciiSinkObject sink = (AsciiSinkObject) w;

    rect->width = (unsigned short) PRIVATE(w,cursor_width);
    rect->height = (unsigned short) PRIVATE(w,cursor_height);
    rect->x = sink->ascii_sink.cursor_x - (short) (rect->width / 2);
    rect->y = sink->ascii_sink.cursor_y - (short) rect->height;
}

/*
 * Although this is unchanged it can't be inherited because it calls
 * GetCursorBounds, which would be AsciiSink's GetCursorBounds. The
 * GetCursorBounds which now gets called is our own. Maybe AsciiSink's
 * InsertCursor should call XawTextGetCursorBounds.
 */
static void
InsertCursor (w, x, y, state)
Widget w;
Position x, y;
XawTextInsertState state;
{
    AsciiSinkObject sink = (AsciiSinkObject) w;
    Widget text_widget = XtParent(w);
    XRectangle rect;

    sink->ascii_sink.cursor_x = x;
    sink->ascii_sink.cursor_y = y;

    GetCursorBounds(w, &rect);
    if (state != sink->ascii_sink.laststate && XtIsRealized(text_widget)) 
        XCopyPlane(XtDisplay(text_widget),
                   sink->ascii_sink.insertCursorOn,
                   XtWindow(text_widget), sink->ascii_sink.xorgc,
                   0, 0, (unsigned int) rect.width, (unsigned int) rect.height,
                   (int) rect.x, (int) rect.y, 1);
    sink->ascii_sink.laststate = state;
}
