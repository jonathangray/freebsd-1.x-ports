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
 * This file will include all the interfaces between the input routines and
 * the daemon.  They should be useful for writing robots and the like
 */

#include <X11/Xlib.h>

#include <stdio.h>
#include <math.h>
#include <signal.h>

#include "defs.h"
#include "data.h"


GLOBAL void     set_speed(p, speed)
  register struct player *p;
  int             speed;
{
  p->p_desspeed = speed;
  CLRREPAIR(p);
  CLRBOMB(p);
  CLRORBIT(p);
  CLRBEAMUP(p);
  CLRBEAMDOWN(p);
}

GLOBAL void     shield_up(p)
  register struct player *p;
{
  if (ISCLOAK(p) || FAILED(p, Shield))
    return;
  SETSHIELD(p);
  CLRREPAIR(p);
  CLRBOMB(p);
  CLRBEAMUP(p);
  CLRBEAMDOWN(p);
}

GLOBAL void     shield_down(p)
  register struct player *p;
{
  CLRSHIELD(p);
}

GLOBAL void     shield_tog(p)
  register struct player *p;
{
  if (ISSHIELD(p))
    shield_down(p);
  else
    shield_up(p);
}

GLOBAL void     follow_ship(p)
  register struct player *p;
{
  SETFOLLOW(p);
  CLRPLLOCK(p);
}

GLOBAL void     bomb_planet(p)
  register struct player *p;
{
  if (!(ISORBIT(p)))
    RETWARN(p, "Must be orbiting to bomb.");

  SETBOMB(p);
  CLRSHIELD(p);
  CLRREPAIR(p);
  CLRBEAMUP(p);
  CLRBEAMDOWN(p);
}

GLOBAL void     beam_up(p)
  register struct player *p;
{
  if (FAILED(p, Trans))
    RETWARN(p, "Transporters too badly damaged!");

  if (!(ISORBIT(p)))
    RETWARN(p, "Must be orbiting to beam up.");

  if (p->p_empire != planets[p->p_planet].pl_owner)
    RETWARN(p, "Those aren't our armies.");

  SETBEAMUP(p);
  CLRSHIELD(p);
  CLRREPAIR(p);
  CLRBOMB(p);
  CLRBEAMDOWN(p);
}

GLOBAL void     beam_down(p)
  register struct player *p;
{
  if (FAILED(p, Trans))
    RETWARN(p, "Transporters too damaged!");

  if (!(ISORBIT(p)))
    RETWARN(p, "Must be orbiting to beam down.");

  SETBEAMDOWN(p);
  CLRBEAMUP(p);
  CLRSHIELD(p);
  CLRREPAIR(p);
  CLRBOMB(p);
}

GLOBAL void     repair(p)
  register struct player *p;
{
  p->p_desspeed = 0;
  SETREPAIR(p);
  CLRBEAMUP(p);
  CLRSHIELD(p);
  CLRBEAMDOWN(p);
  CLRBOMB(p);
}

GLOBAL void     repair_off(p)
  register struct player *p;
{
  CLRREPAIR(p);
}

GLOBAL void     repeat_message(p)
  register struct player *p;
{
  if (++(p->lastm) == MAXMSG) p->lastm = 0;
}

GLOBAL int      cloak_on(p)
  register struct player *p;
{
  if (FAILED(p, Cloak)) {
    warning(p, "Sorry, Keptin, cloaking system inoperative");
    return 0;
  }
  shield_down(p);
  SETCLOAK(p);
  return 1;
}

GLOBAL void     cloak_off(p)
  register struct player *p;
{
  CLRCLOAK(p);
  shield_up(p);
}
