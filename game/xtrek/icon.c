/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 * Copyright 1986 Chris Gutherie
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>

#include <stdio.h>
#include <math.h>

#include "defs.h"
#include "data.h"
#include "icon.h"


GLOBAL void     drawIcon(p)
  register struct player *p;
{
  Display        *display;

  display = p->display;
  XSetForeground(display, p->bmgc, p->textColor);
  XCopyPlane(display, p->Fibm, p->iconWin, p->bmgc, 0, 0,
	     icon_width, icon_height, 0, 0, 1);
}


GLOBAL void     CreateIcon(p, rX, rY, RootWin, uspec)
  aPlayer        *p;
  int             rX, rY;
  Window          RootWin;
  int             uspec;
{
  XSizeHints      wininfo;
  XWMHints        winhints;
  Window          icon_window;
  int             rootX = 0, rootY = YOFF;
  unsigned int    dummy1, dummy2;
  char           *str;
  Display        *display = p->display;
  Window          base_window = p->baseWin;

  p->Fibm = XCreateBitmapFromData(display, base_window, icon_bits,
				 icon_width, icon_height);

  wininfo.x = rX;
  wininfo.y = rY;
  wininfo.width = WINSIDE * 2 + 1 * BORDER;
  wininfo.height = WINSIDE + 2 * BORDER + 2 * MSGSIZE;
  wininfo.min_width = WINSIDE * 2 + 1 * BORDER;
  wininfo.min_height = WINSIDE + 2 * BORDER + 2 * MSGSIZE;
  wininfo.max_width = WINSIDE * 2 + 1 * BORDER;
  wininfo.max_height = WINSIDE + 2 * BORDER + 2 * MSGSIZE;
  wininfo.flags = uspec & (XValue | YValue | XNegative | YNegative) ?
    USPosition | PSize | PMinSize | PMaxSize :
    PPosition | PSize | PMinSize | PMaxSize;
  XSetStandardProperties(display, base_window, PROGRAM_NAME,
			 PROGRAM_NAME, p->Fibm, (char **) NULL, 0, &wininfo);

  icon_window = XCreateSimpleWindow(display, RootWin, rootX, rootY,
	      icon_width, icon_height, BORDER, p->borderColor, p->backColor);

  uspec = 0;
  if (str = XGetDefault(display, PROGRAM_NAME, "icon.geometry")) {
    uspec = XParseGeometry(str, &rootX, &rootY, &dummy1, &dummy2);
    if (CHECKSPEC(uspec, (XValue | XNegative)))
      rootX = -rootX;
    if (CHECKSPEC(uspec, (YValue | YNegative)))
      rootY = -rootY;
  }
  winhints.icon_window = icon_window;
  winhints.input = True;
  winhints.flags = IconWindowHint|InputHint;
  XSetWMHints(display, base_window, &winhints);
  wininfo.x = rootX;
  wininfo.y = rootY;
  wininfo.flags = uspec & (XValue | YValue | XNegative | YNegative) ?
    USPosition : PPosition;
  XSetNormalHints(display, icon_window, &wininfo);

  p->iconWin = icon_window;
}
