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

#include <stdio.h>
#include <math.h>
#include <signal.h>

#include "defs.h"
#include "data.h"

/* Set up the war window and map it */
static char    *gos = "  Re-program";
static char    *exs = "  Exit - no change";
static char    *peaces = "Peace";
static char    *hostiles = "Hostile";
static char    *wars = "War";

LOCAL void      fillwin(p, emp, hostile, warbits)
  register struct player *p;
  int             hostile, warbits;
  int             emp;
{
  char            buf[80];
  Window          win = p->waremp[emp];
  u_long           empireflag = FLAG(emp);
  char           *string = empires[emp].abbrev;

  XFillRectangle(p->display, win, p->cleargc, 0, fontHeight(p->dfont) / 2, fontWidth(p->dfont) * 20,
		 fontHeight(p->dfont));
  /* XPixSet(win, 0, fontHeight(dfont) / 2, fontWidth(dfont) * 20,
     fontHeight(dfont), backColor); */

  if (empireflag & warbits) {
    (void) sprintf(buf, "%s -  %s", string, wars);
    XDrawImageString(p->display, win, p->dfgc, 0, fontHeight(p->dfont) / 2 + p->dfont->ascent, buf, strlen(buf));
  }
  else if (empireflag & hostile) {
    (void) sprintf(buf, "%s -  %s", string, hostiles);
    XDrawImageString(p->display, win, p->dfgc, 0, fontHeight(p->dfont) / 2 + p->dfont->ascent, buf, strlen(buf));
  }
  else {
    (void) sprintf(buf, "%s -  %s", string, peaces);
    XDrawImageString(p->display, win, p->dfgc, 0, fontHeight(p->dfont) / 2 + p->dfont->ascent, buf, strlen(buf));
  }
}

GLOBAL void     waraction(p, data)
  register struct player *p;
  XKeyEvent      *data;
{
  int             changes;
  int             i;
  char            buffer[100];

  for (i = 0; i < numempires; i++)
    if (data->window == p->waremp[i]) {
      if (p->p_swar & FLAG(i)) {
	sprintf(buffer, "War already declared vs the %s", empires[i].name);
	warning(p, buffer);
	if (p->do_bell)
	  XBell(p->display, 0);
      }
      else {
	p->newhostile ^= FLAG(i);
      }
      break;
    }
  warrefresh(p);

  if (data->window == p->waremp[numempires]) {
    changes = p->p_hostile ^ p->newhostile;
    for (i = 0; i < numempires; i++)
      if (changes & FLAG(i))
	sendwarn(p, empires[i].name, p->newhostile & FLAG(i), i);

    p->p_hostile = p->newhostile;
    if (p->reprogram) {
      p->delay = p->p_updates + 100;
      SETWAR(p);			/* stop copilots, mostly */
      warning(p, "Pausing ten seconds to re-program battle computers.");
    }
    XUnmapWindow(p->display, p->war);
    return;
  }
  if (data->window == p->waremp[numempires + 1]) {
    XUnmapWindow(p->display, p->war);
    return;
  }
}

GLOBAL void     sendwarn(p, string, atwar, empire)
  register struct player *p;
  char           *string;
  int             atwar;
  int             empire;
{
  char            buf[BUFSIZ];
  char            addrbuf[10];

  if (atwar) {
    (void) sprintf(buf, "%s (%c%x) declaring war on the %s",
		   p->p_name,
		   empires[p->p_empire].code,
		   p->p_no,
		   string);
    p->reprogram = 1;
  }
  else {
    (void) sprintf(buf, "%s (%c%x) declaring peace with the %s",
		   p->p_name,
		   empires[p->p_empire].code,
		   p->p_no,
		   string);
  }

  (void) sprintf(addrbuf, " %c%x->%-3s",
		 empires[p->p_empire].code,
		 p->p_no,
		 empires[empire].abbrev);
  pmessage(buf, empire, MEMPIRE, addrbuf);
}

GLOBAL void     warwindow(p)
  register struct player *p;
{
  XMapWindow(p->display, p->war);
  p->newhostile = p->p_hostile;
  p->reprogram = 0;
  warrefresh(p);
}

GLOBAL void     warrefresh(p)
  register struct player *p;
{
  int             i;

  for (i = 0; i < numempires; i++)
    fillwin(p, i, p->newhostile, p->p_swar);

  XDrawImageString(p->display, p->waremp[numempires], p->dfgc, 0,
	      fontHeight(p->dfont) / 2 + p->dfont->ascent, gos, strlen(gos));

  XDrawImageString(p->display, p->waremp[numempires + 1], p->dfgc, 0,
	      fontHeight(p->dfont) / 2 + p->dfont->ascent, exs, strlen(exs));
}
