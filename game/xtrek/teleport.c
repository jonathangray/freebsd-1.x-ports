/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
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
*/

#include "defs.h"
#include "data.h"

#ifdef TELEPORT_OPTION
GLOBAL void     teleport(p)
  aPlayer        *p;
{
  int             dx, dy;
  int             cost;
  double          frac;			/* fraction of fuel available. */


  if (ISENG(p))
    RETWARN(p, "Engines overheated!");
  if (ISREPAIR(p))
    RETWARN(p, "Can't teleport while repairing!");
  if (ISCLOAK(p))
    RETWARN(p, "Can't teleport while cloaked!");

  if (p->p_fuel >= COST(p, Teleport)) {	/* enough fuel */
    frac = 1.0;
    cost = COST(p, Teleport);
  }
  else {				/* not enough fuel, use fraction */
    frac = (double) p->p_fuel / COST(p, Teleport);
    cost = p->p_fuel;
  }

  dx = (frac * 2 * (512 - RAND(1024)) * p->ship.telrange) / 1024;
  dy = (frac * 2 * (512 - RAND(1024)) * p->ship.telrange) / 1024;

  /* Move Y coord. */

  p->p_y += dy;
  if (p->p_y < 0)
    p->p_y = -p->p_y;
  if (p->p_y > GWIDTH)
    p->p_y = 2 * GWIDTH - p->p_y;

  /* Move X coord. */

  p->p_x += dx;
  if (p->p_x < 0)
    p->p_x = -p->p_x;
  if (p->p_x > GWIDTH)
    p->p_x = 2 * GWIDTH - p->p_x;

  p->p_fuel -= cost;
  p->p_etemp += HEAT(p, Teleport);
}

#endif					/* TELEPORT_OPTION */

#ifdef TURBO_OPTION
/* Go to turbo speed. */

GLOBAL void     turbo(p)
  aPlayer        *p;
{

  if (ISENG(p))
    RETWARN(p, "Tough luck! Your engines are too hot!");
  if (ISREPAIR(p))
    RETWARN(p, "Can't teleport while repairing!");
  if (ISCLOAK(p))
    RETWARN(p, "Can't teleport while cloaked!");

  if (ISTURBO(p)) {
    CLRTURBO(p);
    warning(p, "TURBO mode off!");
    p->turboclicks = 0;
  }
  else {
    SETTURBO(p);
    p->turboclicks = p->ship.turbotime;
    warning(p, "TURBO mode active!  HANG ON!");
  }

}

#endif					/* TURBO_OPTION */
