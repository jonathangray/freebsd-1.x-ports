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

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#include "defs.h"
#include "data.h"


GLOBAL void     death(p)
  register struct player *p;
{
  p->p_status = PDead;
  p->p_explode = DEATHTIME;
  phasers[p->p_no].ph_status=PhFree;
  
  if (!(ISROBOT(p))) {
    XClearWindow(p->display, p->w);
    if (!p->mono) {
	XSetWindowBorder(p->display, p->baseWin, p->aColor[(int) Green]);
	XSetWindowBorder(p->display, p->mapw, p->aColor[(int) Green]);
	XSetWindowBorder(p->display, p->w, p->aColor[(int) Green]);
	XSetWindowBorder(p->display, p->warnw, p->aColor[(int) Green]);
	XSetWindowBorder(p->display, p->messagew, p->aColor[(int) Green]);
	XSetWindowBorder(p->display, p->tstatw, p->aColor[(int) Green]);
	XSetWindowBorder(p->display, p->iconWin, p->aColor[(int) Green]);
    }
    else {
	XSetWindowBorderPixmap(p->display, p->baseWin, p->aTile[(int) Green]);
	XSetWindowBorderPixmap(p->display, p->mapw, p->aTile[(int) Green]);
	XSetWindowBorderPixmap(p->display, p->iconWin, p->aTile[(int) Green]);
	XSetWindowBorderPixmap(p->display,p->messagew,p->aTile[(int) Green]);
	XSetWindowBorderPixmap(p->display,p->w,p->aTile[(int) Green]);
	XSetWindowBorderPixmap(p->display,p->warnw,p->aTile[(int) Green]);
	XSetWindowBorderPixmap(p->display,p->tstatw,p->aTile[(int) Green]);
    }
  }
  switch (p->p_whydead) {
      char            buf[80];

    case KQuit:
      warning(p, "Self destruct complete.");
      break;
    case KTorp:
      sprintf(buf, "You were killed by a torp from %s (%c%x).",
	      players[p->p_whodead].p_name,
	      empires[players[p->p_whodead].p_empire].code,
	      p->p_whodead);
      warning(p, buf);
      break;
    case KPhaser:
      sprintf(buf, "You were killed by a phaser shot from %s (%c%x).",
	      players[p->p_whodead].p_name,
	      empires[players[p->p_whodead].p_empire].code,
	      p->p_whodead);
      warning(p, buf);
      break;
    case KPlanet:
      sprintf(buf, "You were killed by planetary fire from %s.",
	      planets[p->p_whodead].pl_name);
      warning(p, buf);
      break;
    case KShip:
      sprintf(buf, "You were killed by an exploding ship (%c%x).",
	      empires[players[p->p_whodead].p_empire].code,
	      p->p_whodead);
      warning(p, buf);
      break;

#ifdef notdef
    case KDaemon:
      sprintf(buf, "You were killed by a dying daemon.");
      warning(p, buf);
      p->mustexit = 1;
      break;
#endif

    case KWinner:
      sprintf(buf, "The galaxy has been conquered by %s (%c%x).",
	      players[p->p_whodead].p_name,
	      empires[players[p->p_whodead].p_empire].code,
	      p->p_whodead);
      if (p->p_no != p->p_whodead)
	/* Allow them to repick any empire.  However, the winner can't be
	   allowed to repick any empire as we have to keep his empire
	   affiliation around in order to print his designation in other
	   players' death messages. */
	p->p_mask = ALLEMPIRE;
      warning(p, buf);
      break;
    default:
      sprintf(buf, "You were killed by something unknown to this game?");
      warning(p, buf);
      p->mustexit = 1;
      break;
  }
 if (ISROBOT(p)) {
    p->p_status = PFree;
    nplayers--;
  }
  else {
    /* Let's get rid of the status window since when dead you have no status.  rr2b */
    if(p->statwin) closeStats(p,p->statwin);
    if (ismapped(p, p->playerw))
      XUnmapWindow(p->display, p->playerw);
    if (ismapped(p, p->planetw))
      XUnmapWindow(p->display, p->planetw);
    if (p->infomapped)
      destroyInfo(p);
    if (ismapped(p, p->war))
      XUnmapWindow(p->display, p->war);
    p->p_status = POutfit;
  }
  p->p_flags = 0;
}
