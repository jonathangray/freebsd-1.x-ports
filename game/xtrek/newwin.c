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

#define WARBORDER 2
#define EXTRAWAR 2

/* fix this later */
static int      numwho[MAXEMPIRES] = {-1, -1, -1, -1, -1, -1, -1, -1};


GLOBAL void     redrawEmpire(p, empire, flg)
  register struct player *p;
  int             flg;
  int             empire;
{
  char            buf[BUFSIZ];

  if (numwho[empire] < 0 || flg || numwho[empire] != numShips(empire)) {
    Display        *display = p->display;
    Window          window = p->win[empire];

    XClearWindow(display, window);
    XDrawImageString(display, window, p->dfgc, p->dfont->ascent, p->dfont->ascent, empires[empire].name, empires[empire].namelen);
    (void) sprintf(buf, "%d", numwho[empire] = numShips(empire));
    XDrawString(display, window, p->bFgc, 5, p->bigFont->ascent + p->dfont->ascent + 15, buf, strlen(buf));
  }
}


GLOBAL void     redrawQuit(p, qwin)
  register struct player *p;
  Window          qwin;
{
  XDrawImageString(p->display, qwin, p->dfgc,
		   p->dfont->ascent, p->dfont->ascent, "Quit xtrek", 10);
}


/* wargo and warno are now hanging off the end of the waremp array */
/* wargo is waremp[numempires] and warno is waremp[numempires+1] */

LOCAL void      CreateWarWindows(p)
  struct player  *p;
{
  Display        *display;
  int             i;
  unsigned        WARHEIGHT = fontHeight(p->dfont) * 2;
  unsigned        WARWIDTH = fontWidth(p->dfont) * 20;

  display = p->display;
  p->war = XCreateSimpleWindow(display, p->baseWin, WINSIDE + 10,
			       -BORDER + 10, (unsigned) WARWIDTH,
			       (unsigned) WARHEIGHT * (numempires + EXTRAWAR),
			       WARBORDER, p->borderColor, p->backColor);

  for (i = 0; i < numempires + 2; i++) {
    p->waremp[i] = XCreateSimpleWindow(display, p->war, 0, i * WARHEIGHT,
	       WARWIDTH, WARHEIGHT, WARBORDER, p->borderColor, p->backColor);
    XMapWindow(display, p->waremp[i]);
  }

  getResources(p, PROGRAM_NAME);

  if (!p->mono) {
      u_long           color = p->aColor[(int) Green];

      XSetWindowBorder(display, p->baseWin, color);
      XSetWindowBorder(display, p->mapw, color);
      XSetWindowBorder(display, p->w, color);
      XSetWindowBorder(display, p->warnw, color);
      XSetWindowBorder(display, p->messagew, color);
      XSetWindowBorder(display, p->tstatw, color);
      XSetWindowBorder(display, p->iconWin, color);
  }
  else {
    Pixmap          tile = p->aTile[(int) Green];

    XSetWindowBorderPixmap(display, p->baseWin, tile);
    XSetWindowBorderPixmap(display, p->mapw, tile);
    XSetWindowBorderPixmap(display, p->iconWin, tile);
    XSetWindowBorderPixmap(display,p->messagew,tile);
    XSetWindowBorderPixmap(display,p->w,tile);
    XSetWindowBorderPixmap(display,p->warnw,tile);
    XSetWindowBorderPixmap(display,p->tstatw,tile);
  }
  p->p_status = POutfit;
}


/*
 * this is separate from newwin.  It should; be called *after* openmem.  If
 * not, expose events will be eaten by the forked process (daemon).
 */
GLOBAL void     mapAll(p)
  register struct player *p;
{
  Display        *display;

  initinput(p);
  display = p->display;
  XMapWindow(display, p->mapw);
  XMapWindow(display, p->tstatw);
  XMapWindow(display, p->warnw);
  XMapWindow(display, p->messagew);
  XMapWindow(display, p->w);
  XMapWindow(display, p->baseWin);
}


/* This routine throws up an entry window for the player. */
#define CANCHOOSE(side,p) (((p)->p_mask & FLAG(side)) && !p->mustexit)

GLOBAL void     entrywindow(p)
  register struct player *p;
{
  int             i;
  int             dx = (WINSIDE - BOXSIDE) / numempires;
  Display        *display;
  Window          window;

  display = p->display;
  for (i = 0; i < numempires; i++) {
    window = XCreateSimpleWindow(display, p->w, i * dx, 400, BOXSIDE, BOXSIDE,
				 1, p->shipCol[i], p->backColor);
    if (CANCHOOSE(i, p))
      XSelectInput(display, window,
	  KeyPressMask | ButtonPressMask | ButtonReleaseMask | ExposureMask);
    else {
      XSelectInput(display, window, ExposureMask);
      XSetWindowBackgroundPixmap(display, window, p->stippleTile);
    }
    XMapWindow(display, window);
    p->win[i] = window;
  }

  window = XCreateSimpleWindow(display, p->w, numempires * dx, 400,
			    BOXSIDE, BOXSIDE, 1, p->textColor, p->backColor);
  XSelectInput(display, window, KeyPressMask | ButtonPressMask | ExposureMask);
  XMapWindow(display, window);
  XClearWindow(display, window);
  makeClock(p, window);			/* make clock and start time */
  p->qwin = window;
}


GLOBAL void     del_entrywindow(p)
  register struct player *p;
{
  int             i;
  Display        *display = p->display;

  destroyClock(p);
  for (i = 0; i < numempires; i++) {
    XDestroyWindow(display, p->win[i]);
    p->win[i] = (Window) NULL;
  }
  XDestroyWindow(display, p->qwin);
  p->qwin = (Window) NULL;
}


GLOBAL char    *newwin(p)
  register struct player *p;
{
  char           *monitor;
  register int    i;
  Display        *display;
  register char  *str;
  register struct player *j;
  static char     rvbuf[80];
  Window          root_window;
  int             invert;
  Window          base_window;

  monitor = p->p_monitor;
  for (i = 0, j = &players[0]; i < MAXPLAYER; i++, j++)
    if (j->p_status != PFree && (j != p) && !ISROBOT(j) &&
	!strcmp(j->p_monitor, monitor)) {
      if (j->p_status == PAlive)
	sprintf(rvbuf, "%s already playing on %s.", j->p_login, monitor);
      else
	sprintf(rvbuf, "Someone already playing on %s.", monitor);
    }
  if (!(display = XOpenDisplay(monitor))) {
    perror(monitor);
    sprintf(rvbuf, "Problems with display %s.", monitor);
    return rvbuf;
  }
  p->display = display;
  p->screen = DefaultScreen(display);
  p->depth = DefaultDepth(display, p->screen);
  p->mono = XDisplayCells(display, p->screen) <= 2;
  p->xcn = XConnectionNumber(display);
  root_window = RootWindow(display, p->screen);

  {
    unsigned int    dummy;
    int             rootX = 0;
    int             rootY = YOFF;
    int             uspec = 0;

    if (str = XGetDefault(display, PROGRAM_NAME, "geometry")) {
      uspec = XParseGeometry(str, &rootX, &rootY, &dummy, &dummy);
      if (CHECKSPEC(uspec, (XValue | XNegative)))
	rootX = -rootX;
      if (CHECKSPEC(uspec, (YValue | YNegative)))
	rootY = -rootY;
    }
    base_window = XCreateSimpleWindow(display, root_window, rootX, rootY,
				      (unsigned) WINSIDE * 2 + 1 * BORDER,
			       (unsigned) WINSIDE + 2 * BORDER + 2 * MSGSIZE,
				      BORDER, p->borderColor, p->textColor);
    p->baseWin = base_window;

    CreateIcon(p, rootX, rootY, root_window, uspec);
  }

  invert = getColorDefs(p, PROGRAM_NAME);	/* true if white is 0 */

  {
    XGCValues       gcv;

    gcv.foreground = p->borderColor;
    gcv.background = p->backColor;
    p->gc = XCreateGC(display, base_window, GCForeground | GCBackground, &gcv);

    gcv.graphics_exposures = 0;
    gcv.background = p->backColor;
    p->bmgc = XCreateGC(display, base_window, GCBackground | GCGraphicsExposures, &gcv);

    gcv.function = GXcopy;
    gcv.foreground = p->backColor;
    p->cleargc = XCreateGC(display, base_window, GCFunction | GCForeground, &gcv);

    p->w = XCreateSimpleWindow(display, base_window, -BORDER, -BORDER, WINSIDE, WINSIDE, BORDER, p->borderColor, p->backColor);

    gcv.foreground = p->textColor;
    gcv.background = p->backColor;
    gcv.function = !p->mono ? GXcopy : invert ? GXand : GXor;
    p->monogc = XCreateGC(display, p->w, GCBackground | GCFunction | GCForeground, &gcv);
  }

  if (getFonts(p, PROGRAM_NAME)) {
    XCloseDisplay(display);
    sprintf(rvbuf, "Not all fonts available on %s.\n", monitor);
    return rvbuf;
  }
  p->mapw = XCreateSimpleWindow(display, base_window, WINSIDE, -BORDER, WINSIDE, WINSIDE, BORDER, p->borderColor, p->backColor);

  p->tstatw = XCreateSimpleWindow(display, base_window, -BORDER, WINSIDE, WINSIDE, STATSIZE, BORDER, p->borderColor, p->backColor);

  p->warnw = XCreateSimpleWindow(display, base_window, WINSIDE, WINSIDE, WINSIDE, MSGSIZE, BORDER, p->borderColor, p->backColor);

  p->messagew = XCreateSimpleWindow(display, base_window, WINSIDE, WINSIDE + BORDER + MSGSIZE, WINSIDE, MSGSIZE, BORDER, p->borderColor, p->backColor);

  p->planetw = XCreateSimpleWindow(display, p->w, 3, 3, 47 * fontWidth(p->dfont), (MAXPLANETS + 3) * fontHeight(p->dfont), 2, p->borderColor, p->backColor);

  p->playerw = XCreateSimpleWindow(display, p->w, 3, 3, 66 * fontWidth(p->dfont), (MAXPLAYER + 3) * fontHeight(p->dfont), 2, p->borderColor, p->backColor);

  CreateHelpWindow(p, root_window);

  XDefineCursor(display, base_window, (Cursor) XCreateFontCursor(display, XC_crosshair));
  XDefineCursor(display, p->iconWin, (Cursor) XCreateFontCursor(display, XC_crosshair));

  /* These windows will be used for setting one's warlike stats */
  CreateWarWindows(p);
  return 0;
}
