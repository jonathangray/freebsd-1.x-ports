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

 
#include "defs.h"
#include "data.h"
#include "polar.h"

#define SKILL_DIV 8

GLOBAL void     repair_damage(p)
  register aPlayer *p;
{
  register int    repair_work;
  int             i, maxshield = p->ship.maxshields, oldrepair;

  if (p->p_damage || p->p_shield < maxshield)
    goto isdamage;			/* Yuck (I'm guilty, Mike) */

  for (i = 0; i < NSUBSYSTEMS; i++)
    if (p->p_sysdamage[i])
      goto isdamage;

  CLRREPAIR(p);				/* if no damage then punt out of here */
  return;

isdamage:
  repair_work = p->ship.repair;
  {
    int             skill_bonus = repair_work * ((p->p_stats.st_kills) / (p->p_stats.st_losses + 1));

    repair_work += skill_bonus / SKILL_DIV;
  }

  if (ISREPAIR(p) && p->p_speed == 0)
    repair_work *= 2;

  if ((ISORBIT(p)) && !hostilePlanet(&planets[p->p_planet], p)) {
    if (!(planets[p->p_planet].pl_flags & PLREPAIR))
      repair_work *= 2;
    else
      repair_work *= 4;
  }
  p->p_damage -= repair_work;		/* accumulate repair */
  if (p->p_damage <= 0)
    p->p_damage = 0;


  oldrepair = repair_work;		/* store for later use with shields */
  repair_work *= DAMSCALE(1);

  for (i = 0; (i < NSUBSYSTEMS) && repair_work; i++) {
    if (p->p_sysdamage[i]) {
      repair_work /= 4;
      if ((p->p_sysdamage[i] -= repair_work) < 0) p->p_sysdamage[i] = 0;
    }
  }

  if (FAILED(p, Shield))
    return;
  p->p_shield += ISSHIELD(p) ? oldrepair / 2 : oldrepair;
  if (p->p_shield >= maxshield)
    p->p_shield = maxshield;

}

GLOBAL void     damage_ship(p, attacker, damage, how)
  register struct player *p;
  register int    attacker, damage;
  DeathReason     how;
{
  struct player  *other = &players[attacker];

  if (how != KPlanet && other->p_hostile & p->p_mask)
    other->p_swar |= p->p_mask;
  /* Note that if a player is at peace with the victim, then the damage was
     caused either accidently, or because the victim was at war with, or
     hostile to, the player. In either case, we don't consider the damage to
     be an act of war. */

  if (ISSHIELD(p)) {
    int shield = p->p_shield - damage;

    if (shield < 0) {
      damage = -shield;
      p->p_shield = 0;
    }
    else {
      p->p_shield = shield;
      return;
    }
  }
  p->p_damage += damage;
  if (p->p_damage < p->ship.maxdamage) {
    p->p_newdamage += damage + 1;
    return;
  }
  p->p_status = PExplode;
  p->p_explode = PEXPTIME;
  if (how != KPlanet) {
    if (attacker != p->p_no) {
      float           killvalue = 1.0 + (p->p_armies + p->p_kills) * 0.1;

      other->p_kills += killvalue;
      other->p_stats.st_kills += killvalue;
      empires[other->p_empire].stats.kills += killvalue;
      if (other->p_stats.st_maxkills < other->p_kills)
	other->p_stats.st_maxkills = other->p_kills;
    }
    killmess(p, other);
  }
  empires[p->p_empire].stats.losses++;
  p->p_stats.st_losses++;
  p->p_whydead = how;
  p->p_whodead = attacker;
}

/* Maximum amount of damage a system will ever have (/SAMSCALEVAL) */
#define MAX_DAMAGE 110
/* Maximum sys_fail[] of any system (100 percent change of survival..) */
#define BEST_SYSTEM 100
/* Maximum amount of "random" damage one hit will do to a system */
#define MAX_NEW_HIT (32*DAMSCALE(1))

/* NEWDAMGE (mg2p)
   p->p_newdamge (1 -> ~200) * (DAMSCALEVAL)
   Systems fail in the range (20 -> 100) * DAMSCALEVAL
*/
GLOBAL void break_ship(p)
  register struct player *p;
{
  register int    newdamage = p->p_newdamage;
#ifdef DEBUG
  register int    loop = random() % ((newdamage / 16 + p->p_damage / 64) + 1);
  register int    pound = newdamage * 6 + 2;
#endif DEBUG

  register int i;
  p->p_newdamage = 0;

  newdamage /= DAMSCALEVAL;

#ifdef DEBUG
  printf("loop in break_ship:%d\n",loop);
  printf("pound in break_ship:%ld\n",pound);
  printf("newdamage:%d\n",newdamage);
  printf("damage:%d\n",p->p_damage);
  for(i=0;i<NSUBSYSTEMS;i++) printf("%d:%d ",i, p->p_sysdamage[i]);
  puts("");
#endif

  {
  register int rand_sys, rn;
  if (newdamage > 2*NSUBSYSTEMS) newdamage = 2*NSUBSYSTEMS;
  for(i = 0; i < newdamage; i++) {
    rand_sys = (rn=random()) % NSUBSYSTEMS;
    if (sysfail[rand_sys] <= ((rn /= NSUBSYSTEMS) % BEST_SYSTEM)) {
      p->p_sysdamage[rand_sys] += ((rn /= BEST_SYSTEM) % MAX_NEW_HIT) + (sysfail[rand_sys]*DAMSCALE(1))/8;
      if (p->p_sysdamage[rand_sys] > DAMSCALE(MAX_DAMAGE)) p->p_sysdamage[rand_sys] = DAMSCALE(MAX_DAMAGE);
    }
  }
  }

#ifdef DEBUG
  for(i=0;i<NSUBSYSTEMS;i++) printf("%d:%d ",i, p->p_sysdamage[i]);
  puts("");
  fflush(stdout);
#endif
  if (FAILED(p, Cloak))
    CLRCLOAK(p);
  if (FAILED(p, Shield))
    CLRSHIELD(p);
  if (ISENG(p))
    CLRTURBO(p);
  if (FAILED(p, Lock)) {
    CLRPLOCK(p);
    CLRPLLOCK(p);
  }
  if (FAILED(p, Trans)) {
    CLRBEAMDOWN(p);
    CLRBEAMUP(p);
  }
}
