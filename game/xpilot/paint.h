/* $Id: paint.h,v 1.1 1994/02/23 14:40:06 jkh Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-93 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef PAINT_H
#define PAINT_H

#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include "types.h"
#include "dbuff.h"
#include "keys.h"
#include "client.h"

void Add_message(char *message);
int Handle_start(long server_loops);
int Handle_end(long server_loops);
int Handle_self(int x, int y, int vx, int vy, int dir,
    float power, float turnspeed, float turnresistance,
    int lock_id, int lock_dist, int lock_dir,
    int nextCheckPoint, int numCloaks, int numSensors, int numMines,
    int numRockets, int numEcms, int numTransporters, int numFrontShots,
    int numBackShots, int numAfterburners, int numLasers,
    int num_tanks, int currentTank,
    int fuel_sum, int fuel_max, int packet_size);
int Handle_damaged(int damaged);
int Handle_destruct(int count);
int Handle_shutdown(int count, int delay);
int Handle_refuel(int x0, int y0, int x1, int y1);
int Handle_connector(int x0, int y0, int x1, int y1);
int Handle_laser(int color, int x, int y, int len, int dir);
int Handle_smart(int x, int y, int dir);
int Handle_ball(int x, int y, int id);
int Handle_ship(int x, int y, int id, int dir, int shield, int cloak);
int Handle_mine(int x, int y);
int Handle_item(int x, int y, int type);
int Handle_shot(int x, int y, int color);
int Handle_debris(int type, u_byte *p, int n);
int Handle_ecm(int x, int y, int size);
int Handle_trans(int x1, int y1, int x2, int y2);
int Handle_paused(int x, int y, int count);
int Handle_radar(int x, int y);
int Handle_vcannon(int x, int y, int type);
int Handle_vfuel(int x, int y, long fuel);
int Handle_vbase(int x, int y, int xi, int yi, int type);
int Handle_message(char *msg);
int Handle_eyes(int id);
void Paint_item(u_byte type, Drawable d, GC gc, int x, int y);
void Paint_sliding_radar(void);
void Paint_world_radar(void);
void Paint_score_entry(int entry_num, other_t* other, bool best);
void Paint_score_start(void);
int Handle_time_left(long sec);
void Game_over_action(u_byte stat);

#define MAX_COLORS		16	/* Max. color switched colors ever */

/*
 * Global objects.
 */

/* The fonts used in the game */
extern XFontStruct* gameFont;
extern XFontStruct* messageFont;
extern XFontStruct* scoreListFont;
extern XFontStruct* buttonFont;
extern XFontStruct* textFont;
extern XFontStruct* talkFont;

/* The name of the fonts used in the game */
extern char gameFontName[FONT_LEN];
extern char messageFontName[FONT_LEN];
extern char scoreListFontName[FONT_LEN];
extern char buttonFontName[FONT_LEN];
extern char textFontName[FONT_LEN];
extern char talkFontName[FONT_LEN];

extern Display	*dpy;			/* Display of player (pointer) */
extern short	about_page;		/* Which page is the player on? */
extern u_short	team;			/* What team is the player on? */
extern bool	players_exposed;	/* Is score window exposed? */
extern bool	radar_exposed;		/* Is radar window exposed? */

#define MAX_COLOR_LEN		32

extern GC	gc, messageGC, radarGC, buttonGC, scoreListGC, textGC, talkGC;
extern Window	top, draw, radar, players;
extern Pixmap	p_draw, p_radar, s_radar;
extern Pixmap	itemBitmaps[];
extern long	dpl_1[2], dpl_2[2];	/* Used by radar hack */
extern Window	about_w, about_close_b, about_next_b, about_prev_b,
		keys_w, keys_close_b, talk_w;
extern XColor	colors[MAX_COLORS];		/* Colors */
extern Colormap	colormap;		/* Private colormap */
extern int	maxColors;		/* Max. number of colors to use */
extern bool	gotFocus;
extern bool	talk_mapped;
extern short	view_width, view_height;	/* Visible area from server */
extern u_byte	debris_colors;		/* Number of debris intensities */
extern u_byte	spark_rand;		/* Sparkling effect */
extern float	charsPerTick;		/* Output speed of messages */
extern bool	markingLights;		/* Marking lights on ships */

extern int		maxKeyDefs;
extern keydefs_t	*keyDefs;
extern other_t*		self;		/* Player info */
extern dbuff_state_t*	dbuf_state;	/* Holds current dbuff state */

#endif
