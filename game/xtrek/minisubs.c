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

#include "defs.h"
#include "data.h"
#include "polar.h"

#define TOOHOT(x) (x > TEMPSCALE(MAXTEMP))
#define WEAPON_LIMIT TEMPSCALE(80)
#define ENGINE_LIMIT TEMPSCALE(85)

LOCAL void      CheckOverheat(p)
  aPlayer        *p;
{
  register int    rnd;
  int             temp_w = p->p_wtemp, temp_e = p->p_etemp;

  if (!ISWEP(p) && TOOHOT(temp_w) && ((rnd = random()) % temp_w) > WEAPON_LIMIT) {
    SETWEP(p);
    p->p_wtime = (rnd % PWEAPLOCKVAR) + PWEAPLOCKMIN;
  }
  if (!ISENG(p) && TOOHOT(temp_e) && ((rnd = random()) % temp_e) > ENGINE_LIMIT) {
    SETENG(p);
    p->p_etime = (rnd % PENGLOCKVAR) + PENGLOCKMIN;
    p->p_desspeed = 0;

#ifdef TURBO_OPTION
    CLRTURBO(p);
    p->turboclicks = 0;
#endif

  }
}

GLOBAL void     CoolSystems(p)
  aPlayer        *p;
{
  register int    cool_w, cool_e, temp_e, temp_w;

  if (FAILED(p, Cooling)) {		/* deep trouble */
    CheckOverheat(p);
    return;
  }
  cool_w = p->ship.wcool;
  cool_e = p->ship.ecool;
  if (ISSHIELD(p) || ISCLOAK(p)) {	/* cool slower if shield */
    cool_w /= g_coolpenalty;
    cool_e /= g_coolpenalty;
  }
  temp_w = p->p_wtemp - cool_w;		/* do the cool */
  temp_e = p->p_etemp - cool_e;

  if (temp_w < 0) {
    temp_e += temp_w;			/* transfer unused cooling */
    temp_w = 0;				/* capacity */
  }
  if (temp_e < 0) {
    temp_w += temp_e;
    if (temp_w < 0)
      temp_w = 0;
    temp_e = 0;
  }
  if (ISWEP(p) && (--p->p_wtime <= 0))
    CLRWEP(p);

  if (ISENG(p) && (--p->p_etime <= 0))
    CLRENG(p);

  p->p_wtemp = temp_w;
  p->p_etemp = temp_e;
  CheckOverheat(p);
}

GLOBAL void     add_fuel(p)
  register struct player *p;
{
  register int    refuel = p->ship.recharge;

  if ((ISORBIT(p)) && !hostilePlanet(&planets[p->p_planet], p)) {
    if (!ISFUELP(&planets[p->p_planet]))
      refuel *= 2;
    else
      refuel *= 4;
  }
  p->p_fuel += refuel;

  if (p->p_fuel <= 0) {			/* out of fuel 		 */
    p->p_fuel = 0;
    p->p_desspeed = 0;			/* coast to a stop 	 */
    cloak_off(p);			/* drop cloak and shield */
    shield_down(p);
  }
  else if (p->p_fuel > p->ship.maxfuel)
    p->p_fuel = p->ship.maxfuel;
}

GLOBAL void     player_orbit(p)
  register struct player *p;
{
  int             dir;
  struct planet  *planet;

  if (!ISORBIT(p))
    return;
  dir = (unsigned char) (p->p_dir + 2);
  p->p_dir = dir;
  p->p_desdir = dir;
  planet = &planets[p->p_planet];
  p->p_x = POLAR_Y(ORBDIST, dir) + planet->pl_x;
  p->p_y = -POLAR_X(ORBDIST, dir) + planet->pl_y;
}


/* should be inline if using gcc, eh? */

LOCAL void      DoMove(p)
  aPlayer        *p;
{
  int             speed = WARP1(p->p_speed);

  p->p_x += POLAR_X(speed, p->p_dir);
  p->p_y += POLAR_Y(speed, p->p_dir);

  /* Bounce off the side of the galaxy */

  if (p->p_x < 0) {
    p->p_x = -p->p_x;
    p->p_dir = p->p_desdir = 256 - p->p_dir;
  }
  else if (p->p_x > GWIDTH) {
    p->p_x = 2 * GWIDTH - p->p_x;
    p->p_dir = p->p_desdir = 256 - p->p_dir;
  }
  if (p->p_y < 0) {
    p->p_y = -p->p_y;
    p->p_dir = p->p_desdir = 128 - p->p_dir;
  }
  else if (p->p_y > GWIDTH) {
    p->p_y = 2 * GWIDTH - p->p_y;
    p->p_dir = p->p_desdir = 128 - p->p_dir;
  }
}


GLOBAL void     space_move(p)
  register struct player *p;
{
  register int    desspeed, maxspeed, speed, ispeed;
  int             subspeed, subdiv;

  if (ISORBIT(p))
    return;

#ifdef TURBO_OPTION
  if (ISTURBO(p))
    speed = p->ship.turbospeed;
  else
#endif

    speed = p->p_speed;
  if (p->p_dir != p->p_desdir) {
    if (speed <= 1) {
      p->p_dir = p->p_desdir;
      p->p_subdir = 0;
    }
    else {
      p->p_subdir += (8 * (int) p->ship.turns) / (speed * speed);
      subdiv = p->p_subdir / 8;
      if (subdiv) {

	unsigned char   a = p->p_dir;
	unsigned char   b = p->p_desdir;
	unsigned char   diff = ABS(a - b);
	int             dirdiff = diff > 128 ? 256 - diff : diff;

	if (subdiv > dirdiff)
	  p->p_dir = p->p_desdir;
	else if ((unsigned char) (p->p_dir - p->p_desdir) > 127)
	  p->p_dir += subdiv;
	else
	  p->p_dir -= subdiv;
	p->p_subdir %= 8;
      }
    }
  }
  /* Alter and charge for speed */

  if (!ISENG(p)) {			/* engines alive */
    maxspeed = p->ship.maxspeed;
    if (p->p_damage) {			/* scale remainder */
      int             maxdam = p->ship.maxdamage;
      int             remain = (1024 * (maxdam - p->p_damage)) / maxdam;

      maxspeed = (maxspeed * remain) / 1024;
    }
    desspeed = p->p_desspeed;
    if (desspeed > maxspeed)
      desspeed = maxspeed;
  }
  else
    desspeed = maxspeed = 0;

  if (ISTURBO(p))
    goto skip;

  if (desspeed != speed) {
    subspeed = p->p_subspeed;
    if (desspeed > speed)
      subspeed += p->ship.accint;
    else if (desspeed < speed)
      subspeed -= p->ship.decint;

    subdiv = subspeed / 128;
    if (subdiv) {
      subspeed %= 128;
      ispeed = speed + subdiv;
      if (desspeed > speed) {
	if (ispeed > desspeed)
	  ispeed = desspeed;
      }
      else if (desspeed < speed) {
	if (ispeed < desspeed)
	  ispeed = desspeed;
      }
      speed = ispeed;
    }
    p->p_subspeed = subspeed;
    p->p_speed = speed;
  }
skip:

  if (speed == 0)			/* not moving so skip rest */
    return;

  p->p_fuel -= (speed * COST(p, Warp)) / SPEEDSCALE(1);
  p->p_etemp += (speed * HEAT(p, Warp)) / SPEEDSCALE(1);

  DoMove(p);				/* Move them... */
}

/* Fixed by Rob Ryan May 7,1990 */
GLOBAL void     adjust_alert(p)
  register struct player *p;
{
  int             center_x = p->p_x, center_y = p->p_y;
  int             red = 0, yellow = 0;
  int             mask = p->p_swar | p->p_hostile;
  register struct player *other;

  for (other = players; other < &players[MAXPLAYER]; other++) {
      int            delta_x, delta_y;
      unsigned int   distance_sqr;

      if (other->p_status != PAlive)
	  continue;
      if (!(other->p_mask & mask))
	  continue;
      delta_x = (other->p_x-center_x)>>2;
      delta_y = (other->p_y-center_y)>>2;

      /* This next section was totally bogus, now it works. rr2b */
      if (BETWEEN(-YRANGE, delta_x, YRANGE)&&BETWEEN(-YRANGE, delta_y, YRANGE)) {
	  distance_sqr = ((delta_x * delta_x) + (delta_y * delta_y));
	  /* we hardly need to keep going if we find one redalert */
	  if (distance_sqr < (RRANGE * RRANGE)) {
	      red = 1;
	      break;
	  } else 
	      if (distance_sqr < (YRANGE * YRANGE))
		  yellow = 1;
      }
  }
  p->p_alert =  red ? Red : (yellow ? Yellow : Green);
}
