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
#include <math.h>
#include <signal.h>

#include "defs.h"
#include "data.h"
#include "polar.h"

/*
 * * Provide the angular distance between two angles.
 */
GLOBAL int      angdist(x, y)
  unsigned char   x, y;
{
  register unsigned char res;

  res = ABS(x - y);
  if (res > 128)
    return (256 - (int) res);
  return ((int) res);
}


/*
 * Find the object nearest mouse.  Returns a pointer to an * obtype
 * structure.  This is used for info and locking on. *
 *
 * Because we are never interested in it, this function will * never return
 * your own ship as the target. *
 *
 * Finally, this only works on the two main windows
 */

static struct obtype _target;

GLOBAL struct obtype *gettarget(p, ww, x, y, targtype)
  struct player  *p;
  Window          ww;
  int             x, y;
  int             targtype;
{
  register int    i;
  register struct player *j;
  register struct planet *k;
  register int    g_x, g_y;
  double          dist, closedist;

  if (ww == p->mapw) {
    g_x = x * GWIDTH / WINSIDE;
    g_y = y * GWIDTH / WINSIDE;
  }
  else {
    g_x = p->p_x + ((x - WINSIDE / 2) * SCALE);
    g_y = p->p_y + ((y - WINSIDE / 2) * SCALE);
  }
  closedist = GWIDTH;

  if (targtype & TARG_PLANET)
    for (i = MAXPLANETS, k = &planets[MAXPLANETS - 1]; i--; k--) {
      dist = POLAR_DISTANCE(g_x - k->pl_x, g_y - k->pl_y);
      if (dist < closedist) {
	_target.o_type = PLANETTYPE;
	_target.o_num = i;
	closedist = dist;
      }
    }
  if (targtype & TARG_PLAYER)
    for (i = MAXPLAYER, j = &players[MAXPLAYER - 1]; i--; j--) {
      if (j->p_status != PAlive)
	continue;
      if (ISCLOAK(j) && (!(targtype & TARG_CLOAK)))
	continue;
      if (j == p && (targtype & TARG_MYSELF) == 0)
	continue;
      dist = POLAR_DISTANCE(g_x - j->p_x, g_y - j->p_y);
      if (dist < closedist) {
	_target.o_type = PLAYERTYPE;
	_target.o_num = i;
	closedist = dist;
      }
    }
  if (closedist == GWIDTH) {		/* Didn't get one.  bad news */
    _target.o_type = PLAYERTYPE;
    _target.o_num = p->p_no;		/* Return myself.  Oh well... */
  }
  return &_target;
}

/*
 * * Tell us if a window is currently visible on the screen
 */

GLOBAL int      ismapped(p, win)
  register struct player *p;
  Window          win;
{
  XWindowAttributes info;

  XGetWindowAttributes(p->display, win, &info);

  return info.map_state == IsViewable;
}

/*
 * These are routines that need to be done on interrupts but don't belong in
 * the redraw routine and particularly don't belong in the daemon.
 */

GLOBAL void     auto_features(p)
  register struct player *p;
{
  register int    speed;
  struct player  *pl;
  register struct planet *pln = &planets[p->p_planet];
  char            buf[128];

#ifdef notdef
  /* NOTE: this will be handled elsewhere */
  if (p->copilot && !ISCOPILOT(me)) {
    printf("Owning player has kicked you out\n");
    exit(0);
  }
#endif

  if (ISSELFDEST(p)) {
    if (p->p_updates >= p->selfdest) {
      CLRSELFDEST(p);
      p->p_explode = PEXPTIME * 2;
      p->p_whydead = KQuit;
      p->p_status = PExplode;
    }
    else {
      sprintf(buf, "Self destruct in %d seconds.",
	      (p->selfdest - p->p_updates) / UPS);
      warning(p, buf);
    }
  }
  /* give certain information about bombing or beaming */
  if (ISBOMB(p)) {
    if (pln->pl_armies <= MINARMIES) {
      sprintf(buf, "Cannot bomb %s while armies are less than %d.",
	      pln->pl_name, MINARMIES + 1);
      warning(p, buf);
      CLRBOMB(p);
    }
    else {
      sprintf(buf, "Bombing %s.  %d armies left",
	      pln->pl_name, pln->pl_armies);
      warning(p, buf);
    }
  }
  else if (ISBEAMUP(p)) {
    if (pln->pl_armies < 5) {
      sprintf(buf, "%s: Too few armies to beam up.", pln->pl_name);
      warning(p, buf);
      CLRBEAMUP(p);
    }
    else if (p->p_armies >= (int) (p->p_kills * 2) ||
	     p->p_armies >= p->ship.maxarmies) {
      sprintf(buf, "No more room on board for armies.");
      warning(p, buf);
      CLRBEAMUP(p);
    }
    else {
      sprintf(buf, "Beaming up.  (%d/%d)", p->p_armies,
	      ((p->p_kills * 2) > p->ship.maxarmies) ?
	      p->ship.maxarmies : (int) (p->p_kills * 2));
      warning(p, buf);
    }
  }
  else if (ISBEAMDOWN(p)) {
    if (p->p_armies == 0) {
      sprintf(buf, "No more armies to beam down to %s.",
	      pln->pl_name);
      warning(p, buf);
      CLRBEAMDOWN(p);
    }
    else {
      sprintf(buf, "Beaming down.  (%d/%d) %s has %d armies left",
	      p->p_armies,
	      ((p->p_kills * 2) > p->ship.maxarmies) ?
	      p->ship.maxarmies : (int) (p->p_kills * 2),
	      pln->pl_name, pln->pl_armies);
      warning(p, buf);
    }
  }
  if (ISPLOCK(p)) {
    pl = &players[p->p_playerl];
    if (pl->p_status != PAlive)
      CLRPLOCK(p);
    else if (ISFOLLOW(p) && !ISCLOAK(pl))
      set_course(p, newcourse(p, pl->p_x, pl->p_y));	/* set course to player
							   pl */
  }
  if (ISPLLOCK(p)) {			/* set course to planet x */
    int             dist;

    dist = POLAR_DISTANCE(p->p_x - pln->pl_x, p->p_y - pln->pl_y);
    speed = p->p_speed;
    dist -= ORBDIST;
    if (dist <= 0 && speed <= ORBSPEED)
      orbit(p);
    else {
      speed = WARP1(speed);
      speed *= speed;
      speed /= p->ship.decint / 16;
      set_course(p, newcourse(p, pln->pl_x, pln->pl_y));
      if (dist < speed)
	set_speed(p, ORBSPEED);
    }
  }
}

GLOBAL unsigned char newcourse(p, x, y)
  register struct player *p;
  register int    x, y;
{
  return POLAR_DIRECTION(x - p->p_x, y - p->p_y);
}

GLOBAL int      numShips(owner)
  register int    owner;
{
  int             num = 0;
  register struct player *player;

  for (player = &players[0]; player < &players[MAXPLAYER]; player++)
    if (player->p_empire == owner && player->p_status == PAlive)
      num++;
  return num;
}
