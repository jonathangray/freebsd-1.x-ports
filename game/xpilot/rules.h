/* $Id: rules.h,v 1.1 1994/02/23 14:40:07 jkh Exp $
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

#ifndef RULES_H
#define RULES_H

#define CRASH_WITH_PLAYER   (1<<0)
#define PLAYER_KILLINGS	    (1<<1)
#define LIMITED_LIVES	    (1<<2)
#define TIMING		    (1<<3)
#define ONE_PLAYER_ONLY	    (1<<4)
#define PLAYER_SHIELDING    (1<<5)
#define LIMITED_VISIBILITY  (1<<6)
#define TEAM_PLAY	    (1<<7)
#define WRAP_PLAY	    (1<<8)
#define ALLOW_NUKES	    (1<<9)

/*
 * Possible player status bits
 */
#define PLAYING			(1L<<0)		/* Not returning to base */
#define PAUSE			(1L<<1)
#define GAME_OVER		(1L<<2)         /* 0-1-2 must stay below 8 */
#define KILLED			(1L<<3)		/* is unimportant for client */
#define THRUSTING		(1L<<4)
#define SELF_DESTRUCT		(1L<<5)

#define GRAVITY			(1L<<8)
#define WARPING			(1L<<9)
#define WARPED			(1L<<10)
#define CONFUSED		(1L<<11)

/* #define WAITING_SHOTS	(1L<<32) */
/* #define SHOT_GRAVITY		(1L<<32) */
/* #define LOOSE_MASS		(1L<<32) */
/* #define INACTIVE		(1L<<32) */
/* #define FUEL_GAUGE		(1L<<32) */
/* #define VELOCITY_GAUGE	(1L<<32) */
/* #define POWER_GAUGE		(1L<<32) */

typedef struct {
    int lives;
    long mode;
} rules_t;

#endif
