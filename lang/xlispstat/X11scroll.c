/* X11buttons - buttons for X11 dialogs and windows                    */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
/***********************************************************************/
/**                                                                   **/
/**                    General Includes and Definitions               **/
/**                                                                   **/
/***********************************************************************/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include "xlisp.h"
#include "StGWWindow.h"

extern Display *StX11Display();
extern LVAL StX11ItemObject();

#define nil 0L

extern XContext EventContext, ObjectContext, ThumbContext, ScrollActionContext;
extern Cursor DoubleArrowCursor, RightArrowCursor, LeftArrowCursor;
extern Cursor UpDownArrowCursor, UpArrowCursor, DownArrowCursor;
extern Pixmap ScrollThumbPM;

/***********************************************************************/
/**                                                                   **/
/**                           Event Handler                           **/
/**                                                                   **/
/***********************************************************************/

static LVAL scroll_handler(report, modal)
     XEvent report;
     int modal;
{
  Display *dpy = StX11Display();
  Window s;
  LVAL object;
  int up, is_vertical, x, y, old_x, old_y, (*action)();

  s = report.xany.window;
  if (XFindContext(dpy, s, ObjectContext, &object) == 0 && objectp(object)) {
    is_vertical = is_vertical_scroll_bar(s);
    if (XFindContext(dpy, s, ScrollActionContext, &action) != 0)
      xlfail("can't find thumb context");
    switch (report.type) {
    case ButtonPress:
      switch (report.xbutton.button) {
      case Button1:
      case Button3:
	up = (report.xbutton.button == Button1) ? TRUE : FALSE;
	if (is_vertical)
	  XDefineCursor(dpy, s, (up) ? UpArrowCursor : DownArrowCursor);
	else 
	  XDefineCursor(dpy, s, (up) ? LeftArrowCursor : RightArrowCursor);
	if (action != nil)
	  (*action)(object, s, (up) ? 'L' : 'R',
		    report.xbutton.x, report.xbutton.y);
	while (! XCheckTypedEvent(dpy, ButtonRelease, &report));
	if (is_vertical) XDefineCursor(dpy, s, UpDownArrowCursor);
	else XDefineCursor(dpy, s, DoubleArrowCursor);
	break;
      case Button2:
	if (action != nil)
	  (*action)(object, s, 'M', report.xbutton.x, report.xbutton.y);
	XSync(dpy, FALSE);
#ifdef DODO
	XGrabPointer(dpy, s, TRUE, ButtonMotionMask,
		     GrabModeAsync, GrabModeAsync, s, None, CurrentTime);
#endif /* DODO */

	x = report.xbutton.x;
	y = report.xbutton.y;
	old_x = x;
	old_y = y;	
	while (! XCheckWindowEvent(dpy, s, ButtonReleaseMask, &report)) {
	  while (XCheckWindowEvent(dpy, s, ButtonMotionMask, &report)) {
	    x = report.xmotion.x;
	    y = report.xmotion.y;
	  }
	  if (x != old_x || y != old_y) {
	    if (action != nil)
	      (*action)(object, s, 'M', report.xbutton.x, report.xbutton.y);
	    XSync(dpy, FALSE);
	    old_x = x;
	    old_y = y;
	  }
	}
#ifdef DODO
	XUngrabPointer(dpy, CurrentTime);
#endif /* DODO */
	break;
      }
      break;
    }
  }
  return(NIL);
}

/***********************************************************************/
/**                                                                   **/
/**               Constructing and Removing Scrollbars                **/
/**                                                                   **/
/***********************************************************************/

InstallScrollBar(w, object, left, top, width, height, ps, action)
     Window w, *ps;
     LVAL object;
     int left, top, width, height, (*action)();
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Window s, thumb;

  s = XCreateSimpleWindow(dpy, w, left, top, width, height, 1,
			  BlackPixel(dpy, screen), WhitePixel(dpy, screen));
  XSelectInput(dpy, s, 
	       ExposureMask | ButtonMotionMask |
	       ButtonPressMask | ButtonReleaseMask);

  thumb = XCreateSimpleWindow(dpy, s, left, top, width, height, 0,
			      WhitePixel(dpy, screen), 
			      WhitePixel(dpy, screen));
  XSetWindowBackgroundPixmap(dpy, thumb, ScrollThumbPM);

  if (width > height) XDefineCursor(dpy, s, DoubleArrowCursor);
  else XDefineCursor(dpy, s, UpDownArrowCursor);

  if (XSaveContext(dpy, s, EventContext, (XContext) scroll_handler) != 0)
    xlfail("could not install event handler");
  if (XSaveContext(dpy, s, ObjectContext, (XContext) object) != 0)
    xlfail("could not install object in scroll bar");
  if (XSaveContext(dpy, s, ThumbContext, (XContext) thumb) != 0)
    xlfail("could not install thumb in scroll bar");
  if (XSaveContext(dpy, s, ScrollActionContext, (XContext) action) != 0)
    xlfail("could not install action in scroll bar");

  XMapSubwindows(dpy, s);
  *ps = s;
}

DeleteScrollBar(s)
     Window s;
{
  Display *dpy = StX11Display();

  if (XDeleteContext(dpy, s, EventContext) != 0)
    xlfail("could not delete event context");
  if (XDeleteContext(dpy, s, ObjectContext) != 0)
    xlfail("could not delete object context");
  if (XDeleteContext(dpy, s, ThumbContext) != 0)
    xlfail("could not delete thumb context");
  if (XDeleteContext(dpy, s, ScrollActionContext) != 0)
    xlfail("could not delete action context");
}

/***********************************************************************/
/**                                                                   **/
/**                   Hiding and Showing Scrollbars                   **/
/**                                                                   **/
/***********************************************************************/

ShowScrollBar(s, left, top, width, height)
     Window s;
     int left, top, width, height;
{
  Display *dpy = StX11Display();

  XMoveResizeWindow(dpy, s, left, top, width, height);
  XMapWindow(dpy, s);
}

HideScrollBar(s)
     Window s;
{
  Display *dpy = StX11Display();

  XUnmapWindow(dpy, s);
}

/***********************************************************************/
/**                                                                   **/
/**                       Adjusting Scrollbars                        **/
/**                                                                   **/
/***********************************************************************/

AdjustScrollBar(s, val, page, max)
     Window s;
{
  Display *dpy = StX11Display();
  Window root, thumb;
  int left, top;
  unsigned int width, height, b_width, depth;
  int tleft, ttop, twidth, theight;
  double val_frac, page_frac;

  if (max > 0) {
    XGetGeometry(dpy, s, &root, &left, &top, &width, &height, 
		 &b_width, &depth);

    if (XFindContext(dpy, s, ThumbContext, &thumb) != 0)
      xlfail("can't find thumb context");

    val_frac = ((double) val / (double) max);
    page_frac = ((double) page / (double) max);
    if (width > height) { /* horizontal scroll bar */
      ttop = 0;
      theight = height;
      tleft = width * val_frac;
      twidth = width * page_frac;
    }
    else { /* vertical scroll bar */
      tleft = 0;
      twidth = width;
      ttop = height * val_frac;
      theight = height * page_frac;
    }
    XMoveResizeWindow(dpy, thumb, tleft, ttop, twidth, theight);
  }
}

static is_vertical_scroll_bar(s)
     Window s;
{
  Display *dpy = StX11Display();
  Window root;
  int lx, ly;
  unsigned int width, height, b_width, depth;

  XGetGeometry(dpy, s, &root, &lx, &ly, &width, &height, &b_width, &depth);

  return((height > width) ? TRUE : FALSE);
}
