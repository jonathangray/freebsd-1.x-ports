/* $Id: keys.h,v 1.1 1994/02/23 14:40:05 jkh Exp $
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

#ifndef KEYS_H
#define KEYS_H

#ifndef SERVER
#include <X11/Intrinsic.h>

#define CONTROL_DELAY	100
#endif

/*
 * The following enum type defines the possible actions as a result of
 * a keypress or keyrelease.
 */
typedef enum {
    KEY_DUMMY,
    KEY_LOCK_NEXT,
    KEY_LOCK_PREV,
    KEY_LOCK_CLOSE,
    KEY_CHANGE_HOME,
    KEY_SHIELD,
    KEY_FIRE_SHOT,
    KEY_FIRE_MISSILE,
    KEY_FIRE_TORPEDO,
    KEY_FIRE_NUKE,
    KEY_FIRE_HEAT,
    KEY_DROP_MINE,
    KEY_DETACH_MINE,
    KEY_TURN_LEFT,
    KEY_TURN_RIGHT,
    KEY_SELF_DESTRUCT,
    KEY_ID_MODE,
    KEY_PAUSE,
    KEY_TANK_DETACH,
    KEY_TANK_NEXT,
    KEY_TANK_PREV,
    KEY_REPAIR,
    KEY_TOGGLE_COMPASS,
    KEY_SWAP_SETTINGS,
    KEY_REFUEL,
    KEY_CONNECTOR,
    KEY_INCREASE_POWER,
    KEY_DECREASE_POWER,
    KEY_INCREASE_TURNSPEED,
    KEY_DECREASE_TURNSPEED,
    KEY_THRUST,
    KEY_CLOAK,
    KEY_ECM,
    KEY_DROP_BALL,
    KEY_TRANSPORTER,
    KEY_TALK,
    KEY_FIRE_LASER,
    NUM_KEYS		/* The number of different keys_t */
} keys_t;


#ifndef SERVER
typedef struct {
    KeySym	keysym;			/* Keysym-to-action array */
    keys_t	key;
} keydefs_t;
#endif

#endif
