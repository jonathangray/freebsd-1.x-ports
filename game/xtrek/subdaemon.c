
 /* Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
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
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <signal.h>
#ifdef CHAINSAW
#include <setjmp.h>
extern jmp_buf ioerrorbuf;
extern struct player *ioerrorplayer;
#endif /* CHAINSAW */

#include "defs.h"
#include "data.h"

int             tm_robots[MAXEMPIRES + 1];	/* To limit the number of
						   robots */
int             tm_coup[MAXEMPIRES + 1];/* To allow a coup */
extern void setRedrawFlag();
/*
 * subdaemon - allocate memory and initialize subdaemon
 */
GLOBAL void     subdaemon(filename)
  char           *filename;
{
  register int    i;

  for (i = MAXPLAYER; i--;)
    players[i].p_status = PFree;

  if (filename)
    XTrekParse(filename);
  else {
    standardships();			/* copy standard ships */
    fprintf(stderr, "No planet file; restarting galaxy.\n");
    numplanets = MAXPLANETS;
    numempires = DEFEMPIRES;
    create_random_planets();
  }

  ConfigGlobals();
  convertships();
  CreateShipBitMaps();
  status->active = 0;
  fprintf(stderr, "Daemon ready\n");
  fflush(stderr);
}

int             empirefuse, plfightfuse, beamfuse, planetfuse;

GLOBAL void     move()
{
  struct player  *player;

  signal(SIGALRM,SIG_IGN);
  for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
    PlayerStatus    status = player->p_status;

    if (status != PFree)
      udplayer(player);
    if (status == PAlive && !beamfuse && ISORBIT(player))
      beam(player);
  }

  /* Per tick things...one time for all players */
  robocount();
  robostart();
  udphaser();
  udtorp_tubes();
  udtorps();
  if (beamfuse)
    beamfuse--;
  else
    beamfuse = BEAMFUSE;
  if (empirefuse)
    empirefuse--;
  else {
    empirefuse = EMPIREFUSE;
    empiretimers();
  }
  if (planetfuse)
    planetfuse--;
  else {
    planetfuse = PLANETFUSE;
    udplanets();			/* do populations */
  }
  if (plfightfuse)
    plfightfuse--;
  else {
    plfightfuse = PLFIGHTFUSE;
    PlanetFight();			/* Planet fire */
  }

  /* Remaining player things... */
  for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
    PlayerStatus    status = player->p_status;

    if (!ISROBOT(player)) {
      switch (status) {
	case PAlive:
	case PExplode:
	  redraw(player);
	  break;
	default:;
      }
    }
    else {				/* hard robots recalculate more often */
      if (status == PAlive &&
	  (ISRHARD(player) ? (udcounter & 1) == (player->p_no & 1)
	   : udcounter % 10 == player->p_no % 10)) {
	  if(debug) printf("calling rmove with %ld\n",player);
	  rmove(player);
      }
    }
  }
  signal(SIGALRM, setRedrawFlag);
}

GLOBAL void     robocount()
{
  struct player  *p;
  int             i;

  for (i = 0; i < numempires; i++)
    rcount[i] = 0;
  for (i = MAXPLAYER, p = &players[MAXPLAYER - 1]; i--; p--)
    if (ISROBOT(p))
      rcount[p->p_empire]++;
}

GLOBAL void     robostart()
{
  int             i;

  if (!inrobots)
    return;
  for (i = 0; i < numempires; i++)
    if (rcount[i] == 0)
	startrobot(i, PFRHARD, 0, 0);
}

GLOBAL void     udplayer(j)
  register struct player *j;
{
  int             i;

#ifdef CHAINSAW
  ioerrorplayer = j;
  if (setjmp(ioerrorbuf)) {
      j->display = 0;
      j->p_flags = 0;
      j->p_status = PFree;
      playerchange = 1;
      nplayers--;
      return;
  }
#endif /* CHAINSAW */

  for (i = 0; i < MAXEMPIRES; i++)
    rcount[i] = 0;

  auto_features(j);
  switch (j->p_status) {
    case PDead:
      if (--j->p_explode <= 0 && j->p_ntorp <= 0) {
	j->p_status = PFree;
	nplayers--;
	j->p_sysdamage[(int) SRS] = 0;
	if(j->UsingX) XSync(j->display, 1);
	j->redrawall = 1;
      }
      break;

    case PExplode:
      j->p_updates++;
      /* damage everyone else around */
      if (j->p_explode == PEXPTIME)
	do_explosion(j->p_x, j->p_y, KShip, j->p_no,
		     j->ship.torpdrange, j->ship.torpdamage);
      if (--j->p_explode <= 0) {
	  if (j->p_stats.st_maxkills < j->p_kills)
	      j->p_stats.st_maxkills = j->p_kills;
	  if (empires[j->p_empire].stats.maxkills < j->p_kills)
	      empires[j->p_empire].stats.maxkills = j->p_kills;
	  empires[j->p_empire].stats.kills += j->p_kills;
	/*  j->p_stats.st_kills += j->p_kills; */
	  death(j);
      }
      break;
    case PAlive:

      tcount[j->p_empire]++;

      j->p_updates++;

      repair_damage(j);			/* repair damage */

      CoolSystems(j);			/* cool systems */

      if (ISCLOAK(j))			/* Charge for cloaking */
	j->p_fuel -= COST(j, Cloak);

      if (ISSHIELD(j))			/* Charge for shield */
	j->p_fuel -= COST(j, Shield);

#ifdef TURBO_OPTION
      if (ISTURBO(j)) {
	if (--(j->turboclicks) <= 0)
	  CLRTURBO(j);
      }
#endif

      player_orbit(j);			/* Move Player in orbit */

      space_move(j);			/* Move player through space */

      add_fuel(j);			/* Add fuel */

      adjust_alert(j);			/* Set player's alert status */

      if (j->p_newdamage > 0)		/* break things */
	break_ship(j);

      break;

    case POutfit:
    case PFree:
    case PSetup:
      break;

    default:
      fprintf(stderr, "udplayer: Invalid status\n");
      break;
  }					/* end switch */
}

GLOBAL void     empiretimers()
{
  register int    i;

  for (i = 0; i <= numempires; i++) {
    if (tm_robots[i] > 0)
      tm_robots[i]--;
    if (tm_coup[i] > 0)
      tm_coup[i]--;
  }
}


GLOBAL void     killmess(victim, killer)
  struct player  *victim, *killer;
{
  char            buf[80];

  sprintf(buf, "%s (%c%x) killed.",
	  victim->p_name, empires[victim->p_empire].code, victim->p_no);
  pmessage(buf, 0, MALL, "");
}

GLOBAL void     dumpmessages()
{
  struct message *message;

  for (message = &messages[0]; message < &messages[MAXMSG]; message++)
    if (message->m_flags & MVALID)
      fprintf(stderr, "%d, %s\n", message - &messages[0], message->m_data);
}
