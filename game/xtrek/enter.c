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
#include <errno.h>
#include <pwd.h>
#include <ctype.h>

#include "defs.h"
#include "data.h"

extern long     random();

/* find a planet and put him near it */

LOCAL void      PlacePlayer(p)
  aPlayer        *p;
{
  int             x = GWIDTH / 2;
  int             y = GWIDTH / 2;
  int             counter = numplanets;

  do {
    struct planet  *planet = &planets[RAND(numplanets)];

    if (planet->pl_owner == p->p_empire) {
      x = planet->pl_x;
      y = planet->pl_y;
      break;
    }
  } while (--counter >= 0);

  x += (random() & 0x1fff) - 0x1000;
  if (x < 0)
    x = 0;
  else if (x >= GWIDTH)
    x = GWIDTH - 1;
  p->p_x = x;
  y += (random() >> 13 & 0x1fff) - 0x1000;
  if (y < 0)
    y = 0;
  else if (y >= GWIDTH)
    y = GWIDTH - 1;
  p->p_y = y;
}


/* Enter the game */

GLOBAL void     enter(empire, disp, pno)
  int             empire;
  char           *disp;
  int             pno;
{
  struct player  *p = &players[pno];
  int             i;

  getship(&p->ship, empire);		/* load up ship stats */

  p->p_name[12] = '\0';
  p->p_login[12] = '\0';
  p->p_updates = 0;
  p->do_bell = 0;
  p->p_dir = 0;
  p->p_desdir = 0;
  p->p_speed = 0;
  p->fracwarp = 0;
  p->p_desspeed = 0;
  p->p_subspeed = 0;
  p->p_damage = 0;
  p->p_etemp = 0;
  p->p_etime = 0;
  p->p_wtemp = 0;
  p->p_wtime = 0;
  p->p_swar = 0;
  p->p_kills = 0.0;
  p->p_armies = 0;
  p->p_umsg.m_pending = 0;
  p->delay = 0;
  p->warncount = 0;
  p->infomapped = 0;
  p->mustexit = 0;
  p->p_newdamage = 0;
  p->showShields = 1;
  p->p_status = PAlive;
  p->oldalert = Green;
  p->mapmode = 1;
  p->namemode = 1;
  p->statmode = 1;
  p->redrawall = 1;
  p->warntimer = -1;

  p->UsingX=TRUE;

  /* reset sliders */
#define BOGUS -1
  p->p_oldshield = BOGUS;
  p->p_olddamage = BOGUS;
  p->p_oldfuel = BOGUS;
  p->p_oldspeed = BOGUS;
  p->p_oldwtemp = BOGUS;
  p->p_oldetemp = BOGUS;

  p->p_desx = -1;
  p->p_desy = -1;
  p->p_despl = -1;
  p->p_no = pno;
  p->p_empire = empire;
  p->p_mask = 1 << empire;
  p->p_hostile = ALLEMPIRE & ~p->p_mask;
  p->p_stats.st_entries++;
  p->p_shield = p->ship.maxshields;
  p->p_fuel = p->ship.maxfuel;
  p->lastm = mctl->mc_current;
  SETSHIELD(p);
  SETCOPILOT(p);
  CLRORBIT(p);
  CLRCLOAK(p);

  /* undamage */
  for (i = 0; i < NSUBSYSTEMS; i++)
    p->p_sysdamage[i] = 0;

  if (*disp)				/* if player */
    InitPixMaps(p);			/* init pixmaps */

  /* check for stat window */
  if (p->statwin)
    SETSHOWSTATS(p);
  else
    CLRSHOWSTATS(p);

  /* ship name */
  {
    char            buf[80];

    sprintf(buf, "%c%x", empires[p->p_empire].code, p->p_no);
    strncpy(p->p_mapchars, buf, 2);
  }

  PlacePlayer(p);

  /* count mines and torps */
  {
    int             num_mines = 0;
    int             num_torps = 0;
    int             i;
    struct torp    *torp;

    for (i = MAXTORP, torp = &torps[pno * MAXTORP]; i--; torp++)
      switch (torp->t_status) {
	case TFree:
	  continue;
	case TMine:
	  num_mines++;
	  break;
	default:
	  num_torps++;
	  break;
      }
    p->p_nmine = num_mines;
    p->p_ntorp = num_torps;
  }

  time(&p->start_time);
}
