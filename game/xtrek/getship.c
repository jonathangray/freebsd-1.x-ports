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
#include <X11/Xos.h>
#include <X11/Xlib.h>
*/

#include <stdio.h>
#include "defs.h"
#include "data.h"

LOCAL aShip     ships[MAXEMPIRES];
LOCAL short     shipinit[MAXEMPIRES];

LOCAL aShip     defaultShip;

/* F      R       K     O */
LOCAL int       s_turns[DEFEMPIRES] = {6000, 5500, 7000, 9100};
LOCAL int       s_accint[DEFEMPIRES] = {220, 200, 250, 300};
LOCAL int       s_decint[DEFEMPIRES] = {180, 150, 190, 230};
LOCAL int       s_phaserdamage[DEFEMPIRES] = {30, 20, 70, 15};
LOCAL int       s_phasercost[DEFEMPIRES] = {480, 320, 1120, 240};

LOCAL int       s_ph_pulses[DEFEMPIRES] = {2, 4, 1, 8};
LOCAL int       s_phaserdist[DEFEMPIRES] = {6000, 5000, 7000, 4500};
LOCAL int       s_torpdamage[DEFEMPIRES] = {24, 32, 16, 16};
LOCAL int       s_torpcost[DEFEMPIRES] = {384, 512, 256, 256};
LOCAL int       s_minedamage[DEFEMPIRES] = {48, 64, 32, 32};
LOCAL int       s_minecost[DEFEMPIRES] = {768, 1024, 512, 512};
LOCAL int       s_torpdrange[DEFEMPIRES] = {600, 800, 500, 450};
LOCAL int       s_minedrange[DEFEMPIRES] = {1200, 1600, 1000, 900};
LOCAL int       s_torpspeed[DEFEMPIRES] = {140, 120, 170, 190};
LOCAL int       s_repair[DEFEMPIRES] = {21, 18, 20, 19};
LOCAL int       s_maxdamage[DEFEMPIRES] = {200, 190, 175, 150};
LOCAL int       s_maxshields[DEFEMPIRES] = {200, 220, 180, 160};
LOCAL int       s_reload[DEFEMPIRES] = {3, 4, 2, 1};
LOCAL int       s_burst[DEFEMPIRES] = {2, 0, 1, 3};
LOCAL int       s_detcost[DEFEMPIRES] = {75, 125, 100, 100};

LOCAL int       s_recharge[DEFEMPIRES] = {45, 30, 35, 40};
LOCAL int       s_wcool[DEFEMPIRES] = {3, 5, 4, 3};
LOCAL int       s_ecool[DEFEMPIRES] = {15, 7, 6, 5};

LOCAL int       s_maxspeed[DEFEMPIRES] = {90, 80, 95, 120};
LOCAL int       s_maxarmies[DEFEMPIRES] = {10, 8, 8, 6};
LOCAL int       s_maxfuel[DEFEMPIRES] = {10000, 9000, 9000, 8000};
LOCAL int       s_cruise[DEFEMPIRES] = {50, 51, 58, 63};
LOCAL int       s_hscruise[DEFEMPIRES] = {60, 55, 60, 80};
LOCAL int       s_battle[DEFEMPIRES] = {45, 40, 50, 55};
LOCAL int       s_bturn[DEFEMPIRES] = {40, 30, 45, 50};
LOCAL int       s_flee[DEFEMPIRES] = {75, 90, 80, 99};
LOCAL int       s_engage[DEFEMPIRES] = {11000, 8000, 9000, 12000};
LOCAL int       s_disengage[DEFEMPIRES] = {20000, 17000, 20000, 20000};
LOCAL int       s_shotdamage[DEFEMPIRES] = {72, 90, 48, 48};
LOCAL int       s_circledist[DEFEMPIRES] = {7000, 5000, 7000, 9000};
LOCAL int       s_cloaked[DEFEMPIRES] = {50, 35, 55, 60};
LOCAL int       s_refresh[DEFEMPIRES] = {35, 30, 40, 45};

LOCAL int       s_telrange[DEFEMPIRES] = {8000, 8000, 8000, 8000};
LOCAL int       s_telcost[DEFEMPIRES] = {2000, 2000, 2000, 2000};
LOCAL int       s_cloakcost[DEFEMPIRES] = {60, 20, 30, 45};
LOCAL int       s_warpcost[DEFEMPIRES] = {100, 50, 50, 25};
LOCAL int       s_shieldcost[DEFEMPIRES] = {0, 0, 0, 0};

LOCAL int       s_srsensorfail[DEFEMPIRES] = {90, 90, 90, 90};
LOCAL int       s_torpfail[DEFEMPIRES] = {80, 80, 80, 80};
LOCAL int       s_coolingfail[DEFEMPIRES] = {65, 65, 65, 65};
LOCAL int       s_phaserfail[DEFEMPIRES] = {75, 75, 75, 75};
LOCAL int       s_shieldfail[DEFEMPIRES] = {70, 70, 70, 70};
LOCAL int       s_lockfail[DEFEMPIRES] = {55, 55, 55, 55};
LOCAL int       s_lrsensorfail[DEFEMPIRES] = {40, 40, 40, 40};
LOCAL int       s_cloakfail[DEFEMPIRES] = {85, 85, 85, 85};
LOCAL int       s_transfail[DEFEMPIRES] = {35, 35, 35, 35};


LOCAL void      copyship(ship, i)
  aShip          *ship;
  int             i;
{
  ship->turns = s_turns[i];
  ship->accint = s_accint[i];
  ship->decint = s_decint[i];
  ship->phaserdamage = s_phaserdamage[i];
  ship->cost[(int) Phaser] = s_phasercost[i];
  ship->ph_pulses = s_ph_pulses[i];
  ship->phaserdist = s_phaserdist[i];
  ship->torpdamage = s_torpdamage[i];
  ship->cost[(int) Teleport] = s_torpcost[i];
  ship->minedamage = s_minedamage[i];
  ship->cost[(int) Mine] = s_minecost[i];
  ship->torpdrange = s_torpdrange[i];
  ship->minedrange = s_minedrange[i];
  ship->torpspeed = s_torpspeed[i];
  ship->repair = s_repair[i];
  ship->maxdamage = s_maxdamage[i];
  ship->maxshields = s_maxshields[i];
  ship->reload = s_reload[i];
  ship->burst = s_burst[i];
  ship->cost[(int) Detonate] = s_detcost[i];
  ship->recharge = s_recharge[i];
  ship->cost[(int) Cloak] = s_cloakcost[i];
  ship->cost[(int) Warp] = s_warpcost[i];
  ship->wcool = s_wcool[i];
  ship->ecool = s_ecool[i];
  ship->maxspeed = s_maxspeed[i];
  ship->maxarmies = s_maxarmies[i];
  ship->maxfuel = s_maxfuel[i];
  ship->cruise = s_cruise[i];
  ship->hscruise = s_hscruise[i];
  ship->battle = s_battle[i];
  ship->bturn = s_bturn[i];
  ship->flee = s_flee[i];
  ship->engage = s_engage[i];
  ship->disengage = s_disengage[i];
  ship->shotdamage = s_shotdamage[i];
  ship->circledist = s_circledist[i];
  ship->cloaked = s_cloaked[i];
  ship->refresh = s_refresh[i];
  ship->telrange = s_telrange[i];
  ship->cost[(int) Teleport] = s_telcost[i];
  ship->cost[(int) Shield] = s_shieldcost[i];

  {					/* assume cannot fail  */
    int             j;			/* unless told otherwise */

    for (j = 0; j < NSUBSYSTEMS; j++)
      ship->fail[j] = 100;
  }

  ship->fail[(int) SRS] = s_srsensorfail[i];
  ship->fail[(int) Torp] = s_torpfail[i];
  ship->fail[(int) Cooling] = s_coolingfail[i];
  ship->fail[(int) Phaser] = s_phaserfail[i];
  ship->fail[(int) Shield] = s_shieldfail[i];
  ship->fail[(int) Lock] = s_lockfail[i];
  ship->fail[(int) LRS] = s_lrsensorfail[i];
  ship->fail[(int) Cloak] = s_cloakfail[i];
  ship->fail[(int) Trans] = s_transfail[i];


}

/* initialize the first four empires with the standard values */

GLOBAL void     standardships()
{
  int             i;

  for (i = 0; i < 4; i++)
    copyship(&ships[i], i);
}

GLOBAL void     initdefaultship()
{
  copyship(&defaultShip, 0);		/* default to Federation */
}


GLOBAL void     getship(shipp, empire)
  aShip          *shipp;
  int             empire;
{
  if (empire < 0) {
    fprintf(stderr, "getship: bad empire number: %d\n", empire);
    exit(1);
  }
  if (!shipinit[empire])		/* undefined so return default */
    bcopy(&defaultShip, shipp, sizeof (aShip));
  else
    bcopy(&ships[empire], shipp, sizeof (aShip));
}

GLOBAL aShip   *findship(name)
  char           *name;
{
  if (name == NULL)
    return (&defaultShip);
  else {
    int             empire = findempire(name);

    if (empire == -1)
      return NULL;
    if (!shipinit[empire]) {		/* first time so copy defaults */
      bcopy(&defaultShip, &ships[empire], sizeof (aShip));
      shipinit[empire] = 1;
    }
    return &ships[empire];
  }
}


#define MAXSLIDERTEMP (TEMPSCALE(MAXTEMP + MAXTEMP/4))


LOCAL void      initsliders(empire)
  int             empire;
{
  struct slider  *sl;

  sl = &sliders[empire][0];
  sl->label = "Shield";
  sl->label_length = 6;
  sl->min = 0;
  sl->max = sl->diff = s_maxshields[empire];
  sl->scale = DAMSCALEVAL;
  sl->low_red = sl->max / 4;
  sl->high_red = sl->max;

  sl = &sliders[empire][1];
  sl->label = "Damage";
  sl->label_length = 6;
  sl->min = 0;
  sl->max = sl->diff = s_maxdamage[empire];
  sl->scale=DAMSCALEVAL;
  sl->low_red = 0;
  sl->high_red = sl->max / 2;

  sl = &sliders[empire][2];
  sl->label = "Fuel";
  sl->label_length = 4;
  sl->min = 0;
  sl->max = sl->diff = s_maxfuel[empire];
  sl->scale=1;
  sl->low_red = sl->max / 5;
  sl->high_red = sl->max;

  sl = &sliders[empire][3];
  sl->label = "Warp";
  sl->label_length = 4;
  sl->min = 0;
  sl->max = sl->diff = s_maxspeed[empire];
  sl->scale=1;
  sl->low_red = 0;
  sl->high_red = sl->max / 2;

  sl = &sliders[empire][4];
  sl->label = "W Temp";
  sl->label_length = 6;
  sl->min = 0;
  sl->max = sl->diff = MAXSLIDERTEMP;
  sl->low_red = 0;
  sl->scale=1;
  sl->high_red = sl->max * 3 / 4;

  sl = &sliders[empire][5];
  sl->label = "E Temp";
  sl->label_length = 6;
  sl->min = 0;
  sl->max = sl->diff = MAXSLIDERTEMP;
  sl->scale=1;
  sl->low_red = 0;
  sl->high_red = sl->max * 3 / 4;
}

LOCAL void      convertship(ship)
  aShip          *ship;
{
  int             i;

  ship->maxdamage *= DAMSCALEVAL;
  ship->maxshields *= DAMSCALEVAL;

  ship->phaserdamage *= DAMSCALEVAL;
  ship->torpdamage *= DAMSCALEVAL;
  ship->minedamage *= DAMSCALEVAL;


  ship->wcool = TEMPSCALE(ship->wcool) / 100;
  ship->ecool = TEMPSCALE(ship->ecool) / 100;

  /* heat specified in 100ths of degrees to 16ths */


  for (i = 0; i < NSUBSYSTEMS; i++) {
    ship->heat[i] = TEMPSCALE(ship->heat[i]) / 100;
    ship->fail[i] *=DAMSCALEVAL/8;
    ship->fail[i] += ship->maxdamage/100;
#ifdef DEBUG
    printf("maxdamage:%d\n",ship->maxdamage);
    printf("fail:%d\n",ship->fail[i]);
#endif  
  }
}

GLOBAL void     convertships()
{
  int             empire;

  for (empire = 0; empire < numempires; empire++) {
    convertship(&ships[empire]);
    initsliders(empire);
  }
  convertship(&defaultShip);
}
