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
#include <signal.h>
#include <setjmp.h>
#include <sys/wait.h>
#include <sys/ioctl.h>

#include "defs.h"
#include "data.h"

#define PLANET_KILL_POINTS (0.25)
#define ARMY_KILL_POINTS (0.02)
#define KILL_CARRY_RATIO (2.0)

LOCAL int       pl_warning[MAXPLANETS];	/* To keep planets shut up for awhile */

extern int      tm_robots[];

GLOBAL void     create_random_planets()
{
  int             planet_number;

  bcopy(pdata, planets, sizeof pdata);
  for (planet_number = 0; planet_number < numplanets; planet_number++) {
    int             flags = planets[planet_number].pl_flags;

    if (flags & PLHOME)
      flags |= PLREPAIR | PLFUEL;
    else {
      int             rand = RAND(100);

      if (rand < 50)
	flags |= PLFUEL;
      if (rand < 20)
	flags |= PLREPAIR;
    }
    planets[planet_number].pl_flags = flags;
  }
}

GLOBAL void     udplanets()
{
  register int    i, n_arms;
  register struct planet *pl;

  for (i = numplanets, pl = &planets[numplanets - 1]; i--; pl--) {
    n_arms = pl->pl_armies;
    if (n_arms < 10)
      pl->pl_armies++;
    else if ((random() % 64) > n_arms) {
      if (n_arms > 32)
	pl->pl_armies++;
      else
	pl->pl_armies += (64 - n_arms) / 16;
    }
  }
}


GLOBAL void     PlanetFight()
{
  register int    h;
  register struct planet *l;

  for (h = numplanets, l = &planets[numplanets - 1]; h--; l--) {
    aPlayer        *player;

    if (l->pl_flags & PLCOUP) {
      l->pl_flags &= ~PLCOUP;
      l->pl_owner = l->pl_orig_owner;
    }
    l->pl_flags &= ~PLREDRAW;
    if (pl_warning[h] > 0)
      pl_warning[h]--;
    for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
      int             delta_x;
      int             delta_y;

      if (player->p_status != PAlive)
	continue;

      delta_x = player->p_x - l->pl_x;
      if (delta_x >= PFIREDIST * 3 || delta_x <= -PFIREDIST * 3)
	continue;
      delta_y = player->p_y - l->pl_y;
      if (delta_y >= PFIREDIST * 3 || delta_y <= -PFIREDIST * 3)
	continue;
      l->pl_flags |= PLREDRAW;

      if (!hostilePlanet(l, player))
	continue;
      if (OUTRANGE(delta_x, delta_y, PFIREDIST))
	continue;

      if (l->pl_armies > 0) {
	damage_ship(player, h, DAMSCALE(l->pl_armies / 2 + 2), KPlanet);
	if (player->p_status == PExplode) {
	  char            buf1[80];

	  sprintf(buf1, "%-3s->%-3s", l->pl_name, empires[l->pl_owner].abbrev);
	  pmessage("Enemy destroyed!", l->pl_owner, MEMPIRE, buf1);
	}
      }
      /* do bombing */
      if (!ISORBIT(player) || !ISBOMB(player))
	continue;
      if (player->p_planet != l->pl_no)
	continue;

      if (pl_warning[h] <= 0) {		/* Warn owning empire */
	char            buf1[80];

	pl_warning[h] = 50 / PLFIGHTFUSE;
	sprintf(buf1, "%-3s->%-3s", l->pl_name, empires[l->pl_owner].abbrev);
	pmessage("We are under attack!", l->pl_owner, MEMPIRE, buf1);
      }
      /* Send in a robot if there are no other defenders and 	the planet is
         in the empire's home space */

      if (tcount[l->pl_owner] == 0 && tm_robots[l->pl_owner] == 0) {
	startrobot(l->pl_owner, PFRSTICKY | PFRHARD, 0, 0);
	tm_robots[l->pl_owner] = (RAND(60) + 60) / EMPIREFUSE;
      } {
	aPlayer        *other;

	for (other = &players[0]; other < &players[MAXPLAYER]; other++) {
	  if (ISROBOT(other) && other->p_empire == l->pl_owner) {
	    SETASSIST(other);
	    other->p_desx = l->pl_x;
	    other->p_desy = l->pl_y;
	  }
	}
      }
      if (l->pl_armies <= MINARMIES)
	continue;
      if (RAND(128) >= 64) {
	l->pl_armies--;
	player->p_kills += ARMY_KILL_POINTS;
	empires[player->p_empire].stats.armsbomb++;
	player->p_stats.st_armsbomb++;
      }
    }
  }
}

#ifdef OLDBEAMINGCODE
GLOBAL void     beam(j)
  register aPlayer *j;
{
  register int    h;
  register struct planet *l;

  /* do beaming */

  for (h = numplanets, l = &planets[0]; h--; l++) {
    if (j->p_planet != l->pl_no)
      continue;
    printf("pplanet:%d lplanet:%d\n",j->p_planet,(((long)l)-((long)&planets[0]))/sizeof(planets[0]));
    if (ISBEAMUP(j)) {
      if (l->pl_armies < MINARMIES)
	continue;
      if (j->p_armies >= j->ship.maxarmies)
	continue;
      /* XXX */
      if (j->p_armies == (int) (j->p_kills * KILL_CARRY_RATIO))
	continue;
      if (j->p_empire != l->pl_owner)
	continue;
      j->p_armies++;
      l->pl_armies--;
    }
    else if (ISBEAMDOWN(j)) {
      if (j->p_armies == 0)
	continue;
      if (j->p_empire != l->pl_owner) {
	j->p_armies--;
	if (l->pl_armies > 0) {
	  l->pl_armies--;
	  j->p_kills += ARMY_KILL_POINTS;
	  j->p_stats.st_armsbomb++;
	}
	else {				/* planet taken over */
	  l->pl_armies++;
	  l->pl_owner = j->p_empire;
	  empires[j->p_empire].stats.planets++;
	  j->p_stats.st_planets++;
	  j->p_kills += PLANET_KILL_POINTS;
	  checkwin(j);
	}
      }
      else {
	j->p_armies--;
	l->pl_armies++;
      }
    }
  }
}
#else /* OLDBEAMINGCODE */
GLOBAL void     beam(j)
  register aPlayer *j;
{
    register struct planet *l;

    /* do beaming */

    l=(&planets[j->p_planet]);
    if (ISBEAMUP(j)) {
	if (l->pl_armies < MINARMIES)
	    return;
	if (j->p_armies >= j->ship.maxarmies)
	    return;
	/* XXX */
	if (j->p_armies == (int) (j->p_kills * KILL_CARRY_RATIO))
	    return;
	if (j->p_empire != l->pl_owner)
	    return;
	j->p_armies++;
	l->pl_armies--;
    }
    else if (ISBEAMDOWN(j)) {
	if (j->p_armies == 0)
	    return;
	if (j->p_empire != l->pl_owner) {
	    j->p_armies--;
	    if (l->pl_armies > 0) {
		l->pl_armies--;
		j->p_kills += ARMY_KILL_POINTS;
		j->p_stats.st_armsbomb++;
	    }
	    else {				/* planet taken over */
		l->pl_armies++;
		l->pl_owner = j->p_empire;
		empires[j->p_empire].stats.planets++;
		j->p_stats.st_planets++;
		j->p_kills += PLANET_KILL_POINTS;
		checkwin(j);
	    }
	}
	else {
	    j->p_armies--;
	    l->pl_armies++;
	}
    }
}
#endif /* OLDBEAMINGCODE */

/*
 * This function is called when a planet has been taken over. It checks all
 * the planets to see if the victory conditions are right.  If so, it blows
 * everyone out of the game and resets the galaxy
 */
GLOBAL void     checkwin(winner)
  aPlayer        *winner;
{
  int             empire;
  int             own[MAXEMPIRES];

  for (empire = 0; empire < numempires; empire++)
    own[empire] = 0;

  {
    int             i = numplanets;
    aPlanet        *planet = &planets[0];

    while (--i >= 0) {
      own[planet->pl_owner]++;
      planet++;
    }
  }

  for (empire = 0; empire < numempires; empire++)
    if (own[empire] >= VICTORY) {
      aPlayer        *player;

      /* We have a winning empire */
      for (player = players; player < &players[MAXPLAYER]; player++)
	if (player->p_status != PFree) {
	  player->p_status = PExplode;
	  player->p_explode = 0;
	  player->p_whydead = KWinner;
	  player->p_whodead = winner->p_no;
	}
      winner->p_stats.st_conqs++;
      create_random_planets();
      return;
    }
}
