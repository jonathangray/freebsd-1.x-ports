/* $Id: map.h,v 1.1 1994/02/23 14:40:05 jkh Exp $
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

#ifndef	MAP_H
#define	MAP_H

#include "types.h"
#include "rules.h"
#include "const.h"
#include "item.h"

#define SPACE			0
#define BASE			1
#define FILLED			2
#define FILLED_NO_DRAW		3
#define REC_LU			4
#define REC_LD			5
#define REC_RU			6
#define REC_RD			7
#define FUEL			8
#define CANNON			9
#define CHECK			10
#define SPECIAL			11
#define POS_GRAV		20
#define NEG_GRAV		21
#define CWISE_GRAV		22
#define ACWISE_GRAV		23
#define WORMHOLE		24
#define TREASURE		25
#define	TARGET			26

#define DIR_RIGHT		0
#define DIR_UP			(RES/4)
#define DIR_LEFT		(RES/2)
#define DIR_DOWN		(3*RES/4)

/* Do NOT change these! */
#define MAX_CHECKS		26
#define MAX_TEAMS		10

typedef struct {
    position	pos;
    long	fuel;
    unsigned	conn_mask;
    long	last_change;
} fuel_t; 

typedef struct {
    ipos	pos;
    float	force;
} grav_t;

typedef struct {
    ipos	pos;
    int		dir;
    u_short	team;
} base_t;

typedef struct {
    ipos	pos;
    int		dir;
    int		dead_time;
    bool	active;
    unsigned	conn_mask;
    long	last_change;
} cannon_t;

typedef struct {
    int		max;		/* Max on world at a given time */
    int		num;		/* Number active right now */
    int		chance;		/* Chance for the item to appear */
} item_t;

typedef enum { WORM_NORMAL, WORM_IN, WORM_OUT } wormType;

typedef struct {
    ipos	pos;
    int		lastdest, countdown, lastplayer;
    wormType	type;
} wormhole_t;

typedef struct {
    ipos	pos;
    bool	have;
    u_short	team;
    int 	count;
} treasure_t;

typedef struct {
    ipos       pos;
    u_short    team;
    int        dead_time;
    int        damage;
    unsigned	conn_mask;
    long	last_change;
} target_t;

typedef struct {
    int		NumMembers;
    int		NumBases;
} team_t;
    

typedef struct {
    int		x, y;		/* Size of world in blocks */
    int		diagonal;	/* Diagonal length in blocks */
    int		width, height;	/* Size of world in pixels (optimization) */
    int		hypotenuse;	/* Diagonal length in pixels (optimization) */
    rules_t	*rules;
    char	name[MAX_CHARS];
    char	author[MAX_CHARS];

    u_byte	**block;
    vector	**gravity;

    item_t	items[NUM_ITEMS];

    team_t	teams[MAX_TEAMS];

    int		NumBases;
    base_t	*base;
    int		NumFuels;
    fuel_t	*fuel;
    int		NumGravs;
    grav_t	*grav;
    int		NumCannons;
    cannon_t	*cannon;
    int		NumChecks;
    ipos	check[MAX_CHECKS];
    int		NumWormholes;
    wormhole_t	*wormHoles;
    int		NumTreasures;
    treasure_t	*treasures;
    int         NumTargets;
    target_t    *targets;
} World_map;

#endif
