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

extern int      xtrek_socket;
extern int      dbg;
extern struct planet planets[MAXPLANETS];
extern struct planet pdata[MAXPLANETS];

extern struct empire empires[MAXEMPIRES];
extern struct player players[MAXPLAYER];
extern struct torp torps[MAXPLAYER * MAXTORP];
extern short    torp_tubes[MAXPLAYER];
extern struct status status[1];
extern struct planet planets[MAXPLANETS];
extern struct phaser phasers[MAXPLAYER];
extern struct message messages[MAXMSG];
extern struct mctl mctl[MAXMSG];
extern int      numempires;
extern int      numplanets;

extern aSlider  sliders[MAXEMPIRES][NUM_SLIDERS];

extern short    sysfail[NSUBSYSTEMS];
extern short    sysfix[NSUBSYSTEMS];
extern char     syslet[NSUBSYSTEMS];
extern char     colorlet[3];

extern int      udcounter;
extern int      inrobots;
extern short    tcount[MAXEMPIRES + 1];
extern short    rcount[MAXEMPIRES + 1];

extern double   Sine[320];

extern int      playerchange;
extern int      nplayers;
extern int      nrobots;
extern int      debug;

/* global config variables */

extern float    g_coolpenalty;
extern int      g_fastdestruct;
extern int      g_minedetonate;
extern int g_selfdestruct;
extern int g_turbo;
extern int g_teleport;
extern int g_mine;
extern int g_cloak;

extern int      DEATHTIME;
extern int      PFIRETIME;
extern int      TFIREMIN;
extern int      TFIREVAR;
extern int      MFIREMIN;
extern int      MFIREVAR;
extern int      PEXPTIME;
extern int      PWEAPLOCKMIN;
extern int      PWEAPLOCKVAR;
extern int      PENGLOCKMIN;
extern int      PENGLOCKVAR;
extern int      PSELFDESTTIME;
extern int      RGIVEUPTIME;
extern int      ORBSPEED;

extern int      DETDIST;
extern int      ORBDIST;
extern int      PFIREDIST;
extern int      PHITANG;
extern int      PHITANGL;
extern int      PHITDIST;
extern int      AUTOQUIT;
extern int      FUEL_PER_TEMP;
extern int      MINE_WOBBLE;

extern PFUNC    MapFunc[2];
