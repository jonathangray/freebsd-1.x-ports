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

int             dbg = 1;
int             xtrek_socket;
struct player   players[MAXPLAYER];
struct torp     torps[MAXPLAYER * MAXTORP];
short           torp_tubes[MAXPLAYER];
struct status   status[1];
struct planet   planets[MAXPLANETS];
struct phaser   phasers[MAXPLAYER];
struct message  messages[MAXMSG];
struct mctl     mctl[MAXMSG];

int             numempires = 0;
int             numplanets = 0;
int             playerchange = 0;
int             nplayers = 0;
int             nrobots = 0;
int             inrobots = 0;
int             udcounter;
int             debug = 0;
short           tcount[MAXEMPIRES + 1];
short           rcount[MAXEMPIRES + 1];

/* global config variables */

float           g_coolpenalty = 1.25;
int             g_fastdestruct = 0;
int             g_minedetonate = 0;
int 		g_selfdestruct=1;
int 		g_turbo=0;
int 		g_teleport=1;
int 		g_mine=1;
int		g_cloak=1;

int             DEATHTIME = 4;		/* Player is dead for 6 seconds */
int             PFIRETIME = 1;		/* Phaser fires for 1 second */
int             TFIREMIN = 7;		/* Torp lives at least 5 sec */
int             TFIREVAR = 2;		/* Torp may live up to 2 more sec */
int             MFIREMIN = 60;		/* Mine lives at least 1 min */
int             MFIREVAR = 120;		/* Mine may live up to 2 more min */
int             PEXPTIME = 1;		/* Player explodes for 1 sec */
int             PWEAPLOCKMIN = 10;	/* Weapons lock for at least 10 sec */
int             PWEAPLOCKVAR = 15;	/* Weapons may lock for 15 sec more */
int             PENGLOCKMIN = 10;	/* Engines lock for at least 10 sec */
int             PENGLOCKVAR = 15;	/* Engines may lock for 15 sec more */
int             PSELFDESTTIME = 5;	/* Self destruct in 5 sec */
int             RGIVEUPTIME = 60;	/* Robot gives up in 60 sec */
int             ORBSPEED = 20;		/* Fastest a ship can go into orbit */

/* Space */
int             DETDIST = 2000;		/* At this range a player can detonate
					   a torp */
int             ORBDIST = 900;		/* At this range a player can orbit a
					   planet */
int             PFIREDIST = 1500;	/* At this range a planet will shoot
					   at a player */
int             PHITANG = 5;		/* Number of degrees that a phaser
					   will hit */
int             PHITANGL = 10;		/* Number of degrees that a locked
					   phaser will hit */

int             AUTOQUIT = 180;		/* auto logout in 180 secs */

int             MINE_WOBBLE = 128;

/* other */

struct empire   empires[MAXEMPIRES] =
{
  {"Federation", "FED", "M5", 'F', 10, GWIDTH / 4, GWIDTH * 3 / 4},
  {"Romulan", "ROM", "Colossus", 'R', 7, GWIDTH / 4, GWIDTH / 4},
  {"Klingon", "KLI", "Guardian", 'K', 7, GWIDTH * 3 / 4, GWIDTH / 4,},
  {"Orion", "ORI", "HAL", 'O', 5, GWIDTH * 3 / 4, GWIDTH * 3 / 4}
};

/*
 * Phaser, Torp, Mine, Trans, Shield, Cloak, SRS, LRS, Lock, Cooling,
 * Detonate, Teleport, Warp, Sentinel}
 */

char            syslet[NSUBSYSTEMS] =
{'P', 'T', 'M', 't', 'S', 'c', 's', 'l', 'L', 'C', 'D', 'J', 'W'};

short           sysfail[NSUBSYSTEMS] = {
  75,					/* phaser */
  80,					/* torp */
  70,					/* mine */
  35,					/* trans */
  70,					/* shield */
  85,					/* cloak */
  90,					/* src */
  40,					/* lrs */
  55,					/* lock */
  65,					/* cool   */
  70,					/* det */
  80,					/* teleport */
  100					/* warp   */
};

short           sysfix[NSUBSYSTEMS] = {
  70,					/* phaser */
  80,					/* torp */
  70,					/* mine */
  40,					/* trans */
  60,					/* shield */
  90,					/* cloak */
  100,					/* srs */
  45,					/* lrs */
  50,					/* lock */
  55,					/* cool   */
  70,					/* det */
  80,					/* teleport */
  100					/* warp   */
};

char            colorlet[3] = {'G', 'Y', 'R'};

/* PFUNC           MapFunc[2] = {XMapWindow, XUnmapWindow}; */

aSlider         sliders[MAXEMPIRES][NUM_SLIDERS];

#define RICH (PLHOME|PLFUEL|PLREPAIR)

struct planet   planets[MAXPLANETS];

struct planet   pdata[MAXPLANETS] = {
  {0, RICH, FED, FED, 20000, 80000, "Earth", 5, 30, FLAG(FED), 0, 0},
  {1, 0, FED, FED, 10000, 60000, "Sasus", 5, 30, FLAG(FED), 0, 0},
  {2, 0, FED, FED, 25000, 60000, "Candeleron", 10, 30, FLAG(FED), 0, 0},
  {3, 0, FED, FED, 44000, 81000, "Beta III", 8, 30, FLAG(FED), 0, 0},
  {4, 0, FED, FED, 33000, 55000, "Janus", 5, 30, FLAG(FED), 0, 0},
  {5, 0, FED, FED, 30000, 90000, "Deneb VI", 8, 30, FLAG(FED), 0, 0},
  {6, 0, FED, FED, 45000, 66000, "Ceti IV", 7, 30, FLAG(FED), 0, 0},
  {7, 0, FED, FED, 11000, 75000, "Altar", 5, 30, FLAG(FED), 0, 0},
  {8, 0, FED, FED, 8000, 93000, "Dekar", 5, 30, FLAG(FED), 0, 0},
  {9, 0, FED, FED, 32000, 74000, "Daltus", 6, 30, FLAG(FED), 0, 0},
  {10, RICH, ROM, ROM, 20000, 20000, "Romulus", 7, 30, FLAG(ROM), 0, 0},
  {11, 0, ROM, ROM, 45000, 7000, "Ethen", 5, 30, FLAG(ROM), 0, 0},
  {12, 0, ROM, ROM, 4000, 12000, "Amur", 4, 30, FLAG(ROM), 0, 0},
  {13, 0, ROM, ROM, 42000, 44000, "Remus", 5, 30, FLAG(ROM), 0, 0},
  {14, 0, ROM, ROM, 13000, 45000, "Bal", 3, 30, FLAG(ROM), 0, 0},
  {15, 0, ROM, ROM, 28000, 8000, "Tahndar", 7, 30, FLAG(ROM), 0, 0},
  {16, 0, ROM, ROM, 28000, 23000, "Dact", 4, 30, FLAG(ROM), 0, 0},
  {17, 0, ROM, ROM, 40000, 25000, "Sirius II", 9, 30, FLAG(ROM), 0, 0},
  {18, 0, ROM, ROM, 25000, 44000, "Rakhir", 6, 30, FLAG(ROM), 0, 0},
  {19, 0, ROM, ROM, 8000, 29000, "Rus", 3, 30, FLAG(ROM), 0, 0},
  {20, RICH, KLI, KLI, 80000, 20000, "Klin", 7, 30, FLAG(KLI), 0, 0},
  {21, 0, KLI, KLI, 70000, 40000, "Malatrakir", 10, 30, FLAG(KLI), 0, 0},
  {22, 0, KLI, KLI, 60000, 10000, "Amakron", 7, 30, FLAG(KLI), 0, 0},
  {23, 0, KLI, KLI, 54000, 40000, "Laltir", 6, 30, FLAG(KLI), 0, 0},
  {24, 0, KLI, KLI, 93000, 8000, "Khartair", 8, 30, FLAG(KLI), 0, 0},
  {25, 0, KLI, KLI, 90000, 37000, "Monpur III", 10, 30, FLAG(KLI), 0, 0},
  {26, 0, KLI, KLI, 69000, 31000, "Sectus", 6, 30, FLAG(KLI), 0, 0},
  {27, 0, KLI, KLI, 83000, 48000, "Makus", 5, 30, FLAG(KLI), 0, 0},
  {28, 0, KLI, KLI, 54000, 21000, "Jakar", 5, 30, FLAG(KLI), 0, 0},
  {29, 0, KLI, KLI, 73000, 5000, "Gendus", 6, 30, FLAG(KLI), 0, 0},
  {30, RICH, ORI, ORI, 80000, 80000, "Orion", 5, 30, FLAG(ORI), 0, 0},
  {31, 0, ORI, ORI, 92000, 59000, "Amterion", 8, 30, FLAG(ORI), 0, 0},
  {32, 0, ORI, ORI, 65000, 55000, "Lumecis", 7, 30, FLAG(ORI), 0, 0},
  {33, 0, ORI, ORI, 52000, 60000, "Bitar V", 7, 30, FLAG(ORI), 0, 0},
  {34, 0, ORI, ORI, 72000, 69000, "Prastan", 7, 30, FLAG(ORI), 0, 0},
  {35, 0, ORI, ORI, 64000, 80000, "Sorlen", 6, 30, FLAG(ORI), 0, 0},
  {36, 0, ORI, ORI, 56000, 89000, "Zetus", 5, 30, FLAG(ORI), 0, 0},
  {37, 0, ORI, ORI, 91000, 94000, "Jih", 3, 30, FLAG(ORI), 0, 0},
  {38, 0, ORI, ORI, 70000, 93000, "Markus II", 9, 30, FLAG(ORI), 0, 0},
  {39, 0, ORI, ORI, 85000, 70000, "Oren", 4, 30, FLAG(ORI), 0, 0}
};
