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

#include <signal.h>

#include "defs.h"
#include "data.h"
#include "polar.h"

/* Display information about the nearest objext to mouse */

/*
 * * When the player asks for info, this routine finds the object * nearest
 * the mouse, either player or planet, and pop up a window * with the desired
 * information in it. *
 *
 * We intentionally provide less information than is actually * available.
 * Keeps the fog of war up. *
 *
 * There is a different sized window for each type player/planet * and we take
 * care to keep it from extending beyond the main * window boundaries.
 */

#define DRAWSTRING(buf) \
  XDrawImageString(p->display, p->infow, p->dfgc, fontWidth(p->dfont), \
   p->dfont->ascent + fontHeight (p->dfont) * line, buf, strlen (buf));

GLOBAL void     inform(p, ww, x, y)
  register struct player *p;
  Window          ww;
  int             x, y;
{
  char            buf[BUFSIZ * 2];
  int             line = 0;
  register struct player *j;
  register struct planet *k;
  int             mx, my;
  Window          subw;
  struct obtype  *target;
  XWindowAttributes wa;

  p->infomapped = 1;
  target = gettarget(p, ww, x, y, TARG_PLAYER | TARG_PLANET | TARG_MYSELF);

  XGetWindowAttributes(p->display, ww, &wa);

  {
    int             rx, ry;
    unsigned int             maskr;

    XQueryPointer(p->display, ww, &subw, &subw, &rx, &ry, &mx, &my, &maskr);
  }

  if (target->o_type == PLAYERTYPE) {
    double          dist;

    /* Too close to the edge? */
    /* if (mx + 23 * fontWidth(p->dfont) + 2 > wa.width) mx = wa.width - 23 *
       fontWidth(p->dfont) - 2; if (my + 7 * fontHeight(p->dfont) + 2 >
       wa.height) my = wa.height - 7 * fontHeight(p->dfont) - 2; */
    p->infow = XCreateSimpleWindow(p->display, p->w, 10, 10,
				   (unsigned) 23 * fontWidth(p->dfont),
				   (unsigned) 9 * fontHeight(p->dfont),
				   2, p->borderColor, p->backColor);
    XClearWindow(p->display, p->infow);
    XMapWindow(p->display, p->infow);
    j = &players[target->o_num];
    dist = POLAR_DISTANCE(p->p_x - j->p_x, p->p_y - j->p_y);
    (void) sprintf(buf, "%s (%c%1x):", j->p_name,
		   empires[j->p_empire].code, j->p_no);
    DRAWSTRING(buf);
    line++;

    (void) sprintf(buf, "Login   %-s", j->p_login);
    DRAWSTRING(buf);
    line++;

    (void) sprintf(buf, "Display %-s", j->p_monitor);
    DRAWSTRING(buf);
    line++;
    (void) sprintf(buf, "Speed   %1.1f", (float) j->p_speed * 0.1);
    DRAWSTRING(buf);
    line++;

    if (((p->p_playerl == j->p_no) && ISPLOCK(j) ||
	 (p->p_empire == j->p_empire) || !ISSHIELD(j))) {
      DRAWSTRING(buf);
      line++;
      (void) sprintf(buf, "Damage  %-d", j->p_damage / DAMSCALEVAL);
      DRAWSTRING(buf);
      line++;
      (void) sprintf(buf, "Shields %-d", j->p_shield / DAMSCALEVAL);
      DRAWSTRING(buf);
      line++;
    }
    (void) sprintf(buf, "kills   %-4.2f", j->p_kills);
    DRAWSTRING(buf);
    line++;
    (void) sprintf(buf, "dist    %-d", (int) dist);
    DRAWSTRING(buf);
    line++;
    if (j->p_swar & p->p_mask) {
      DRAWSTRING("WAR");
      line++;
    }
    else if (j->p_hostile & p->p_mask) {
      DRAWSTRING("HOSTILE");
      line++;
    }
    else {
      DRAWSTRING("PEACEFUL");
      line++;
    }
  }
  else {				/* Planet */
    /* Too close to the edge? */
    if (mx + 20 * fontWidth(p->dfont) + 2 > wa.width)
      mx = wa.width - 20 * fontWidth(p->dfont) - 2;
    if (my + 3 * fontHeight(p->dfont) + 2 > wa.height)
      my = wa.height - 3 * fontHeight(p->dfont) - 2;

    p->infow = XCreateSimpleWindow(p->display, ww, mx, my,
		       20 * fontWidth(p->dfont), 3 * fontHeight(p->dfont), 2,
				   p->borderColor, p->backColor);

    XClearWindow(p->display, p->infow);
    XMapWindow(p->display, p->infow);
    k = &planets[target->o_num];
    if (k->pl_info & p->p_mask) {
      (void) sprintf(buf, "%s (%c)", k->pl_name, empires[k->pl_owner].code);
      DRAWSTRING(buf);
      line++;
      (void) sprintf(buf, "Armies %d", k->pl_armies);
      DRAWSTRING(buf);
      line++;
      (void) sprintf(buf, "%s %s ",
		     (k->pl_flags & PLREPAIR ? "REPAIR" : "      "),
		     (k->pl_flags & PLFUEL ? "FUEL" : "    "));
      {
	int             i;
	char           *s = buf + 12;

	for (i = 0; i < numempires; i++)
	  *s++ = (k->pl_info & FLAG(i)) ? empires[i].code : ' ';
	*s = 0;
      }
      DRAWSTRING(buf);
      line++;
    }
    else {
      (void) sprintf(buf, "%s", k->pl_name);
      DRAWSTRING(buf);
      line++;
      (void) sprintf(buf, "No other info");
      DRAWSTRING(buf);
      line++;
    }
    XFlush(p->display);
  }
}


GLOBAL void     destroyInfo(p)
  register struct player *p;
{
  XDestroyWindow(p->display, p->infow);
  p->infomapped = 0;
  p->redrawall = 1;
}
