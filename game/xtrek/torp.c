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
/*
#include <X11/Xlib.h>
#include <X11/Xos.h>
*/
#include <stdio.h>
#include <math.h>

#include "defs.h"
#include "data.h"
#include "polar.h"

/* Launch torp */

GLOBAL void     ntorp(p, course, status, imptime)
  register struct player *p;
  unsigned char   course;
  TorpState       status;
  int             imptime;
{
  int             torpcost;
  int             torpheat;
  short          *owned;
  int             reload;
  int             i;
  struct torp    *torp;

  if (ISWEP(p))
    RETWARN(p, "Weapons overheated.");

  if (ISCLOAK(p))
    RETWARN(p, "Can't fire while cloaked.");

  if (ISREPAIR(p))
    RETWARN(p, "Can't fire while repairing.");

  if (FAILED(p, Torp))
    RETWARN(p, "Torps inoperative.");

  owned = &torp_tubes[p->p_no];
  reload = p->ship.reload;

  if (status == TMine) {
    torpcost = COST(p, Mine);
    torpheat = HEAT(p, Mine);
    if (p->p_nmine >= MAXMINE) {
      char            buf[64];

      sprintf(buf, "Sorry, %d mines max.", MAXMINE);
      RETWARN(p, buf);
    }
  }
  else {
    torpcost = COST(p, Torp);
    torpheat = HEAT(p, Torp);
    if (p->p_ntorp >= MAXTORP - MAXMINE) {
      char            buf[64];

      sprintf(buf, "Sorry, %d torps max.", MAXTORP - MAXMINE);
      RETWARN(p, buf);
    }
  }
  if (*owned > (reload * p->ship.burst))
    RETWARN(p, "Tubes not reloaded.");

  if (p->p_fuel < torpcost)
    RETWARN(p, "Not enough fuel.");


  for (i = MAXTORP, torp = &torps[p->p_no * MAXTORP]; i--; torp++)
    if (torp->t_status == TFree)
      goto fire_it;
/*
  fprintf(stderr, "ntorp: sanity check failed - no torp to fire\n");
  */
  return;

fire_it:

  *owned += reload;
  p->p_fuel -= torpcost;
  p->p_wtemp += torpheat;
  torp->t_status = status;
  torp->t_owner = p->p_no;
  torp->t_mask = p->p_mask;
  torp->t_x = p->p_x;
  torp->t_y = p->p_y;
  if (status == TMine) {
    torp->t_type = MINETorp;

    torp->t_speed = 0;
    torp->t_fuse = 1;
    p->p_nmine++;
    torp->t_fuse = RAND(MFIREVAR) + MFIREMIN;
    torp->t_damage = p->ship.minedamage;
    torp->t_range = p->ship.minedrange;
  }
  else {
    p->p_ntorp++;
    torp->t_dir = course;
    torp->t_speed = p->ship.torpspeed;
    torp->t_type = NORMTorp;
    if (imptime >= 0)
      torp->t_fuse = imptime;
    else
      torp->t_fuse = RAND(TFIREVAR) + TFIREMIN;
    torp->t_damage = p->ship.torpdamage;
    torp->t_range = p->ship.torpdrange;
    p->p_stats.st_torps++;
    empires[p->p_empire].stats.torps++;
  }
  torp->t_dx=POLAR_X(WARP1(torp->t_speed), torp->t_dir);
  torp->t_dy=POLAR_Y(WARP1(torp->t_speed), torp->t_dir);
  
  torp->t_war = ISRHOSTILE(p) ? ALLEMPIRE : p->p_hostile | p->p_swar;
}

/* See if there is someone close enough to explode for */
/* keep track of how near an enemy has come as well */
/* currently it only returns whether something is near enough to detonate */

GLOBAL int      nearest(torp)
  register struct torp *torp;
{
  register int    i;
  register int    dx, dy;
  register struct player *j;
  int             d_range = torp->t_range, t_own = torp->t_owner;
  int             nd_range = -d_range, t_x = torp->t_x, t_y = torp->t_y;
  u_long           t_war = torp->t_war;

  for (i = MAXPLAYER, j = &players[MAXPLAYER - 1]; i--; j--) {
    if (j->p_status != PAlive || t_own == j->p_no)
      continue;
    if (t_war & j->p_mask) {
      dx = t_x - j->p_x;
      if (dx > d_range || dx < nd_range)
	continue;
      dy = t_y - j->p_y;
      if (dy > d_range || dy < nd_range)
	continue;
      return 1;
    }
  }
  return 0;
}

LOCAL void      explode(torp)
  register struct torp *torp;
{
  register int    tdam = torp->t_damage;

  do_explosion(torp->t_x, torp->t_y, KTorp,
	       torp->t_owner, torp->t_range, tdam);

  if (torp->t_status == TExplode)
    return;
  if (torp->t_status != TMine)
    players[torp->t_owner].p_ntorp--;
  else
    players[torp->t_owner].p_nmine--;
  torp->t_status = TExplode;
  torp->t_speed = 0;
  torp->t_fuse = tdam / DAMSCALEVAL / 8;
  torp->t_damage /= 4;
}

/* Do damage to all surrounding players */
GLOBAL void     do_explosion(center_x, center_y, why, who, range, expdam)
  int             center_x, center_y, why, who;
  register int    range, expdam;
{
  int             twice_range = range * 2;
  int             range_sqr = range * range;
  struct player  *player;

  for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
    int             delta_x;
    int             delta_y;
    int             distance_sqr;
    int             damage;

    if (player->p_status != PAlive)
      continue;
    delta_x = player->p_x - center_x;
    if (delta_x >= twice_range || delta_x <= -twice_range)
      continue;
    delta_y = player->p_y - center_y;
    if (delta_y >= twice_range || delta_y <= -twice_range)
      continue;
    distance_sqr = delta_x * delta_x + delta_y * delta_y;
    if (distance_sqr <= range_sqr)
      damage = expdam;
    else
      damage = (range_sqr * 4 - distance_sqr) * expdam / (range_sqr * 3);
    if (damage <= 0)
      continue;
    damage_ship(player, who, damage, why);
  }
}

GLOBAL void     udtorp_tubes()
{
  short          *tube;

  for (tube = &torp_tubes[0]; tube < &torp_tubes[MAXPLAYER]; tube++)
    if (*tube > 0)
      (*tube)--;
}

/* Detonate torps and mines */

#define DRANGE ((RRANGE * 3)/2)

GLOBAL void     detTorp(p)
  register struct player *p;
{
  register int    i;
  register struct torp *t;
  for (i = MAXTORP, t = &torps[(p->p_no * MAXTORP)]; i; t++, i--)
    if (t->t_status == TMove)
      t->t_status = TOff;
}

GLOBAL void     detMine(p)
  register struct player *p;
{
  register int    i;
  register struct torp *t;
  
  for (i = MAXTORP, t = &torps[(p->p_no * MAXTORP)]; i; t++, i--)
    if (t->t_status == TMine) {
      if (g_minedetonate)
	explode(t);
      else
	t->t_status = TFree;
    }
  p->p_nmine = 0;
}

/*
 * * Here we have another flaw.  Detonating other players torps can be a *
 * very quick way to die.  Why?  Because you always take some damage. *
 * Experienced players never detonate other players' torps.  Balance is *
 * really hard to obtain with this type of function.  Technically, a * player
 * could nearly continuously detonate torps (at least faster than * they
 * could be fired) and never be hurt, if I allowed less damage as * a
 * possible result.  So here it sits.
 */

GLOBAL void     detothers(p)
  register struct player *p;
{
  int             detcost;
  int             h;

  if (ISWEP(p))
    RETWARN(p, "Weapons overheated.");

  detcost = COST(p, Detonate);
  if (p->p_fuel < detcost)
    RETWARN(p, "Not enough fuel to detonate.");

  p->p_fuel -= detcost;
  p->p_wtemp += HEAT(p, Detonate);

  h = MAXPLAYER;
  while (--h >= 0) {
    int             i;
    struct torp    *torp;

    if ((players[h].p_status == PFree) || (h == p->p_no))
      continue;
    for (i = MAXTORP, torp = &torps[h * MAXTORP]; --i >= 0; torp++) {
      int             delta_x;
      int             delta_y;

      if (torp->t_status != TMove)
	continue;
      delta_x = torp->t_x - p->p_x;
      delta_y = torp->t_y - p->p_y;
      if (INRANGE(delta_x, delta_y, DETDIST))
	torp->t_status = TDet;
    }
  }
}

GLOBAL void     udtorps()
{
  register struct torp *torp;

  for (torp = torps; torp < &torps[MAXPLAYER * MAXTORP]; torp++) {
    int             do_explode = 0;

    switch (torp->t_status) {
      case TFree:
	continue;
      case TMine:
	torp->t_x += RAND(MINE_WOBBLE) - (MINE_WOBBLE / 2);	/* wobble */
	torp->t_y += RAND(MINE_WOBBLE) - (MINE_WOBBLE / 2);
	if (nearest(torp))
	  do_explode = 1;
	break;
      case TMove:
	torp->t_x += torp->t_dx;
	if (torp->t_x < 0) {
	  torp->t_x = 0;
	  do_explode = 1;
	  break;
	}
	else if (torp->t_x > GWIDTH) {
	  torp->t_x = GWIDTH;
	  do_explode = 1;
	  break;
	}
	torp->t_y += torp->t_dy;
	if (torp->t_y < 0) {
	  torp->t_y = 0;
	  do_explode = 1;
	  break;
	}
	else if (torp->t_y > GWIDTH) {
	  torp->t_y = GWIDTH;
	  do_explode = 1;
	  break;
	}
	if (nearest(torp) || (torp->t_fuse-- <= 0))
	  do_explode = 1;
	break;
      case TExplode:
	if (--(torp->t_fuse) > 0)
	  do_explode = 1;
	else {
	  torp->t_status = TFree;
	  continue;
	}
	break;
      case TOff:
      case TDet:
	torp->t_x += torp->t_dx;
	if (torp->t_x < 0)
	  torp->t_x = 0;
	else if (torp->t_x > GWIDTH)
	  torp->t_x = GWIDTH;
	else {
	  torp->t_y += torp->t_dy;
	  if (torp->t_y < 0)
	    torp->t_y = 0;
	  else if (torp->t_y > GWIDTH)
	    torp->t_y = GWIDTH;
	}
	do_explode = 1;
    }
    if (do_explode) {
      explode(torp);
      do_explode = 0;
    }
  }
}
