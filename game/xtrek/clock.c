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
#include <signal.h>

#include "defs.h"
#include "data.h"

#define clock_width 48
#define clock_height 48
LOCAL char      clock_bits[] = {
  0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00, 0x00, 0x80, 0x87, 0xf0, 0x00, 0x00,
  0x00, 0x60, 0x80, 0x00, 0x03, 0x00, 0x00, 0x18, 0x80, 0x00, 0x0c, 0x00,
  0x00, 0x14, 0x00, 0x00, 0x10, 0x00, 0x00, 0x23, 0x00, 0x00, 0x68, 0x00,
  0x80, 0x00, 0x00, 0x00, 0x84, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x01,
  0x20, 0x00, 0x00, 0x00, 0x00, 0x02, 0x20, 0x00, 0x00, 0x00, 0x00, 0x02,
  0x10, 0x00, 0x00, 0x00, 0x00, 0x04, 0x18, 0x00, 0x00, 0x00, 0x00, 0x08,
  0x28, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x04, 0x00, 0x00, 0x00, 0x00, 0x12,
  0x04, 0x00, 0x00, 0x00, 0x00, 0x10, 0x02, 0x00, 0x00, 0x00, 0x00, 0x20,
  0x02, 0x00, 0x00, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0x00, 0x00, 0x20,
  0x02, 0x00, 0x00, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40,
  0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40,
  0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0x0f, 0x00, 0x80, 0x00, 0x00, 0x78,
  0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40,
  0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40,
  0x02, 0x00, 0x00, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0x00, 0x00, 0x20,
  0x02, 0x40, 0xdd, 0x5d, 0x01, 0x20, 0x02, 0x80, 0xc8, 0xcc, 0x00, 0x20,
  0x04, 0x40, 0x49, 0x5d, 0x01, 0x10, 0x04, 0x00, 0x00, 0x00, 0x00, 0x10,
  0x28, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x0c,
  0x10, 0x00, 0x00, 0x00, 0x00, 0x04, 0x20, 0x00, 0x00, 0x00, 0x00, 0x02,
  0x20, 0x00, 0x00, 0x00, 0x00, 0x02, 0x40, 0x00, 0x00, 0x00, 0x00, 0x01,
  0x80, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x23, 0x00, 0x00, 0x62, 0x00,
  0x00, 0x14, 0x00, 0x00, 0x14, 0x00, 0x00, 0x18, 0x80, 0x00, 0x0c, 0x00,
  0x00, 0x60, 0x80, 0x00, 0x03, 0x00, 0x00, 0x80, 0x87, 0xf0, 0x00, 0x00,
0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define CLOCK_WID  (BOXSIDE * 9 / 10)
#define CLOCK_HEI  (BOXSIDE * 2 / 3)
#define CLOCK_BDR  0
#define CLOCK_X    (BOXSIDE / 2 - CLOCK_WID / 2)
#define CLOCK_Y    (BOXSIDE / 2 - CLOCK_HEI / 2)

GLOBAL void     makeClock(p, w)
  register struct player *p;
  Window          w;
{
  p->startTime = time(0);

  p->once = 0;
  p->oldtime = -1;
  p->clockw = XCreateSimpleWindow(p->display, w, CLOCK_X, CLOCK_Y,
		       (unsigned) CLOCK_WID, (unsigned) CLOCK_HEI, CLOCK_BDR,
				  p->backColor, p->backColor);

  XMapWindow(p->display, p->clockw);
  p->tbm = XCreateBitmapFromData(p->display, p->clockw,
				 clock_bits, clock_width, clock_height);

  XClearWindow(p->display, p->clockw);
}

GLOBAL void     destroyClock(p)
  register struct player *p;
{
  XFreePixmap(p->display, p->tbm);
  p->tbm = (Pixmap) NULL;
  XDestroyWindow(p->display, p->clockw);
  p->clockw = (Window) NULL;
}

#define PI    3.141592654

GLOBAL void     showTimeLeft(p, time, max, flg)
  register struct player *p;
  int             time, max, flg;
{
  char            buf[BUFSIZ], *cp;
  int             cx, cy, ex, ey, tx, ty;

  if (!p->once || flg) {
    p->once = 1;
    XClearWindow(p->display, p->clockw);
    p->oldtime = -1;
    cx = CLOCK_WID / 2;			/* 45 */
    cy = (CLOCK_HEI - fontHeight(p->dfont)) / 2;	/* 26 */
    ex = cx - clock_width / 2;		/* 21 */
    ey = cy - clock_height / 2;		/* 2 */

    XSetForeground(p->display, p->bmgc, p->textColor);
    XCopyPlane(p->display, p->tbm, p->clockw, p->bmgc, 0, 0, clock_width, clock_height,
	       ex, ey, 1);

    cp = "Auto Quit";
    tx = cx - fontWidth(p->dfont) * strlen(cp) / 2;
    ty = CLOCK_HEI - p->dfont->descent;
    XDrawImageString(p->display, p->clockw, p->dfgc, tx, ty, cp, strlen(cp));
  }
  XSetFunction(p->display, p->dfgc, GXinvert);
  if (p->oldtime != -1) {
    cx = CLOCK_WID / 2;			/* 45 */
    cy = (CLOCK_HEI - fontHeight(p->dfont)) / 2;	/* 26 */
    ex = cx - clock_width * sin(2 * PI * p->oldtime / max) / 2;
    ey = cy - clock_height * cos(2 * PI * p->oldtime / max) / 2;
    XDrawLine(p->display, p->clockw, p->dfgc, cx, cy, ex, ey);
  }
  p->oldtime = time;

  cx = CLOCK_WID / 2;			/* 45 */
  cy = (CLOCK_HEI - fontHeight(p->dfont)) / 2;	/* 26 */
  ex = cx - clock_width * sin(2 * PI * time / max) / 2;
  ey = cy - clock_height * cos(2 * PI * time / max) / 2;
  XDrawLine(p->display, p->clockw, p->dfgc, cx, cy, ex, ey);
  XSetFunction(p->display, p->dfgc, GXcopy);

  sprintf(buf, "%2.2d", max - time);
  tx = cx - fontWidth(p->dfont) * strlen(buf) / 2;
  ty = cy - fontHeight(p->dfont) / 2;
  XDrawImageString(p->display, p->clockw, p->dfgc, tx, ty, buf, strlen(buf));
}
